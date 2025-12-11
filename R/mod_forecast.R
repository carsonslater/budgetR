# Module UI
forecastUI <- function(id) {
  ns <- NS(id)
  tagList(
    navset_card_tab(
      full_screen = TRUE,
      nav_panel("Portfolio Forecast",
        sidebarLayout(
          sidebarPanel(
            h4("Settings"),
            numericInput(ns("initial_val"), "Current Portfolio Value ($)", value = 100000),
            numericInput(ns("monthly_add"), "Monthly Contribution ($)", value = 1000),
            sliderInput(ns("years"), "Years to Forecast", 1, 40, 20),
            hr(),
            h5("Simple Model"),
            sliderInput(ns("avg_return"), "Expected Annual Return (%)", 0, 20, 8),
            hr(),
            h5("Stochastic Model (Monte Carlo)"),
            sliderInput(ns("volatility"), "Annual Volatility (Std Dev %)", 5, 40, 15),
            numericInput(ns("num_sims"), "Number of Simulations", 100, min = 10, max = 1000),
            actionButton(ns("run_sim"), "Run Simulation", class = "btn-primary")
          ),
          mainPanel(
            plotOutput(ns("forecast_plot")),
            verbatimTextOutput(ns("stats_summary"))
          )
        )
      ),
      nav_panel("Stock Analysis",
        sidebarLayout(
          sidebarPanel(
            selectizeInput(ns("ticker"), "Stock Ticker", choices = NULL, multiple = FALSE),
            dateRangeInput(ns("date_range"), "Date Range", start = Sys.Date() - 365*2, end = Sys.Date()),
            actionButton(ns("analyze_stock"), "Analyze", icon = icon("chart-line"))
          ),
          mainPanel(
            plotOutput(ns("stock_chart")),
            tableOutput(ns("stock_performance"))
          )
        )
      )
    )
  )
}

# Module Server
forecastServer <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # Portfolio Simulation Logic
    # --------------------------------------------------------------------------
    
    # Update Ticker Choices from DB
    observe({
      tickers <- tbl(con, "portfolio_snapshots") |> 
        distinct(symbol) |> 
        collect() |> 
        pull(symbol) |> 
        unique() |>
        na.omit()
      
      updateSelectizeInput(session, "ticker", choices = tickers, server = TRUE)
    })
    
    sim_data <- eventReactive(input$run_sim, {
      # Parameters
      P0 <- input$initial_val
      PMT <- input$monthly_add
      mu <- input$avg_return / 100
      sigma <- input$volatility / 100
      years <- input$years
      n_months <- years * 12
      n_sims <- input$num_sims
      
      # Vectorized Simulation
      # We simulate monthly returns
      # Monthly mu approx = mu/12 (geometric is more precise but arithmetic is standard for simple tools)
      # Monthly sigma = sigma / sqrt(12)
      
      mu_m <- mu / 12
      sigma_m <- sigma / sqrt(12)
      
      # Matrix of returns: (n_months) rows x (n_sims) cols
      random_returns <- matrix(rnorm(n_months * n_sims, mean = mu_m, sd = sigma_m), 
                               nrow = n_months, ncol = n_sims)
      
      # Accumulate value
      # This loop is readable; for speed in R, could use C++ or cumprod carefully, 
      # but for <1000 sims/40 years, loop is fine.
      
      # Initialize matrix with P0
      # But we have monthly contributions.
      # V_t = V_{t-1} * (1 + r_t) + PMT
      
      sim_paths <- matrix(0, nrow = n_months + 1, ncol = n_sims)
      sim_paths[1, ] <- P0
      
      for (t in 2:(n_months + 1)) {
        # Previous value * Growth + Contribution
        sim_paths[t, ] <- sim_paths[t-1, ] * (1 + random_returns[t-1, ]) + PMT
      }
      
      # Convert to Tidy Frame for Plotting
      # We just want percentiles for the fan chart to keep it light
      
      months <- 0:n_months
      
      # Calculate quantiles per month
      stats <- apply(sim_paths, 1, quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
      
      tibble(
        month = months,
        year = months / 12,
        p05 = stats["5%", ],
        p25 = stats["25%", ],
        median = stats["50%", ],
        p75 = stats["75%", ],
        p95 = stats["95%", ]
      )
    })
    
    output$forecast_plot <- renderPlot({
      req(sim_data())
      
      sim_data() |>
        ggplot(aes(x = year)) +
        # Fan Area
        geom_ribbon(aes(ymin = p05, ymax = p95, fill = "90% Confidence"), alpha = 0.2) +
        geom_ribbon(aes(ymin = p25, ymax = p75, fill = "50% Confidence"), alpha = 0.3) +
        geom_line(aes(y = median, color = "Median"), size = 1) +
        scale_y_continuous(labels = scales::dollar_format()) +
        scale_fill_tq() +
        scale_color_tq() +
        theme_tq() +
        labs(title = paste("Portfolio Monte Carlo Simulation:", input$years, "Years"),
             subtitle = paste("Assumptions: ", input$avg_return, "% Return,", input$volatility, "% Volatility, $", input$monthly_add, "/mo contrib"),
             y = "Portfolio Value", x = "Years")
    })
    
    output$stats_summary <- renderPrint({
      df <- sim_data()
      final_row <- tail(df, 1)
      list(
        "Expected Median" = scales::dollar(final_row$median),
        "Pessimistic (5th %)" = scales::dollar(final_row$p05),
        "Optimistic (95th %)" = scales::dollar(final_row$p95)
      )
    })
    
    # --------------------------------------------------------------------------
    # Stock Logic
    # --------------------------------------------------------------------------
    
    stock_data <- eventReactive(input$analyze_stock, {
      req(input$ticker)
      tq_get(input$ticker, get = "stock.prices", 
             from = input$date_range[1], to = input$date_range[2])
    })
    
    output$stock_chart <- renderPlot({
      req(stock_data())
      stock_data() |>
        ggplot(aes(x = date, y = close)) +
        geom_line() +
        geom_ma(ma_fun = SMA, n = 50, color = "blue", linetype = 2) +
        geom_ma(ma_fun = SMA, n = 200, color = "red", linetype = 2) +
        theme_tq() +
        labs(title = paste(input$ticker, "Price History"), 
             subtitle = "Blue = 50 Day MA, Red = 200 Day MA", y = "Close Price")
    })
    
    output$stock_performance <- renderTable({
      req(stock_data())
      
      # Calculate simple returns
      stock_data() |>
        tq_transmute(select = adjusted, 
                     mutate_fun = periodReturn, 
                     period = "yearly") |>
        mutate(
          yearly.returns = scales::percent(yearly.returns, accuracy = 0.01),
          date = as.character(date) # Format date as string to remove decimal
        ) |>
        rename(Date = date, `Yearly Return` = yearly.returns)
    })
    
  })
}
