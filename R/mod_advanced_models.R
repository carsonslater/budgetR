# Module UI
advancedModelsUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Advanced Statistical Models"),
      layout_sidebar(
        sidebar = sidebar(
          h4("Model Configuration"),
          selectInput(ns("ticker"), "Stock Ticker", choices = c("SPY", "QQQ", "IWM"), selected = "SPY", multiple=FALSE),
          # In real app, ticker list would come from DB or API
          
          selectInput(ns("model_type"), "Model Type", 
                      choices = c("GARCH (Volatility)" = "GARCH",
                                  "LSTM (Deep Learning)" = "LSTM",
                                  "RNN (Deep Learning)" = "RNN",
                                  "Ensemble (Average)" = "Ensemble")),
          
          numericInput(ns("horizon"), "Forecast Horizon (Days)", value=30, min=1, max=100),
          
          # Dynamic Inputs based on Model Type
          uiOutput(ns("dynamic_params")),
          
          hr(),
          actionButton(ns("train_btn"), "Train & Forecast", class="btn-primary w-100"),
          br(), br(),
          div(style="font-size: 0.8em; color: gray;", 
              "Note: Deep Learning models may take several seconds to train on CPU.")
        ),
        
        div(
          h4("Forecast Results"),
          plotOutput(ns("forecast_plot"), height = "500px"),
          hr(),
          verbatimTextOutput(ns("model_metrics"))
        )
      )
    )
  )
}

# Module Server
advancedModelsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --------------------------------------------------------------------------
    # Dynamic UI
    # --------------------------------------------------------------------------
    output$dynamic_params <- renderUI({
      req(input$model_type)
      
      if (input$model_type == "GARCH") {
        tagList(
          selectInput(ns("garch_order"), "GARCH Order (p, q)", 
                      choices = c("1,1", "1,0", "2,1"), selected = "1,1"),
          selectInput(ns("dist_model"), "Distribution", 
                      choices = c("norm", "std", "ged"), selected = "norm")
        )
      } else if (input$model_type %in% c("LSTM", "RNN", "Ensemble")) {
        tagList(
          sliderInput(ns("epochs"), "Training Epochs", min=5, max=100, value=10),
          sliderInput(ns("lookback"), "Lookback Window", min=5, max=60, value=20),
          sliderInput(ns("hidden_size"), "Hidden Layer Size", min=8, max=64, value=16)
        )
      }
    })
    
    # --------------------------------------------------------------------------
    # Data Fetching
    # --------------------------------------------------------------------------
    stock_data <- eventReactive(input$train_btn, {
      req(input$ticker)
      showNotification("Fetching Data...", id="train_notif")
      
      tryCatch({
        df <- tq_get(input$ticker, from = Sys.Date() - 365*2, to = Sys.Date())
        
        # Validation
        if (nrow(df) == 0) stop("No data returned for ticker.")
        
        df <- df |>
          select(date, close) |>
          arrange(date) |>
          mutate(returns = log(close / lag(close))) |>
          drop_na()
        
        if (nrow(df) < 50) stop("Not enough historical data points (need > 50).")
        df
      }, error = function(e) {
        showNotification(paste("Error fetching data:", e$message), type = "error", id="train_notif")
        NULL
      })
    })

    # ... Torch models (skipped for brevity in replace block, assuming unchanged) ...
    # Wait, I need to keep the Torch definitions. I will target the model_results block specifically.
    
    # --------------------------------------------------------------------------
    # Model Logic
    # --------------------------------------------------------------------------
    
    # Define Torch Modules (Simple)
    LSTMModel <- nn_module(
      "LSTMModel",
      initialize = function(input_size, hidden_size, output_size = 1) {
        self$lstm <- nn_lstm(input_size, hidden_size, batch_first = TRUE)
        self$linear <- nn_linear(hidden_size, output_size)
      },
      forward = function(x) {
        out <- self$lstm(x)
        # out[[1]] is (batch, seq, hidden)
        # Get last time step:
        # Note: In R, dim indexing is tricky.
        # We assume batch_first=TRUE, so dim 2 is seq_len.
        last_out <- out[[1]][, dim(out[[1]])[2], ]
        self$linear(last_out)
      }
    )
    
    RNNModel <- nn_module(
      "RNNModel",
      initialize = function(input_size, hidden_size, output_size = 1) {
        self$rnn <- nn_rnn(input_size, hidden_size, batch_first = TRUE)
        self$linear <- nn_linear(hidden_size, output_size)
      },
      forward = function(x) {
        out <- self$rnn(x)
        last_out <- out[[1]][, dim(out[[1]])[2], ]
        self$linear(last_out)
      }
    )
    
    # Helper to train Torch Model
    train_torch_model <- function(df, type, epochs, lookback, hidden_size) {
      # Prepare Data: Sliding Window
      counts <- df$close
      mean_val <- mean(counts)
      sd_val <- sd(counts)
      scaled <- (counts - mean_val) / sd_val
      
      n <- length(scaled)
      x_list <- list()
      y_list <- list()
      
      for (i in 1:(n - lookback)) {
        x_list[[i]] <- scaled[i:(i + lookback - 1)]
        y_list[[i]] <- scaled[i + lookback]
      }
      
      # Convert to matrix first for speed/safety
      x_mat <- do.call(rbind, x_list)
      y_vec <- unlist(y_list)
      
      x_tensor <- torch_tensor(x_mat)$unsqueeze(3) # (Batch, Seq, Feat=1)
      y_tensor <- torch_tensor(y_vec)$unsqueeze(2) # (Batch, 1)
      
      # Model Init
      model <- if (type == "LSTM") {
        LSTMModel(1, hidden_size)
      } else {
        RNNModel(1, hidden_size)
      }
      
      criterion <- nn_mse_loss()
      optimizer <- optim_adam(model$parameters, lr = 0.01)
      
      # Train Loop
      model$train()
      for (epoch in 1:epochs) {
        optimizer$zero_grad()
        output <- model(x_tensor)
        loss <- criterion(output, y_tensor)
        loss$backward()
        optimizer$step()
      }
      
      list(model = model, mean = mean_val, sd = sd_val, last_seq = tail(scaled, lookback))
    }
    
    # Forecast with Torch
    forecast_torch <- function(model_obj, horizon) {
      model <- model_obj$model
      mean_val <- model_obj$mean
      sd_val <- model_obj$sd
      curr_seq <- torch_tensor(model_obj$last_seq)$unsqueeze(1)$unsqueeze(3)
      
      preds <- numeric(horizon)
      model$eval()
      
      for (i in 1:horizon) {
        pred <- model(curr_seq)
        preds[i] <- as.numeric(pred)
        
        # Update sequence
        old_vec <- as.numeric(curr_seq[1, , 1])
        new_vec <- c(old_vec[-1], as.numeric(pred))
        curr_seq <- torch_tensor(new_vec)$unsqueeze(1)$unsqueeze(3)
      }
      
      # Unscale and Unname
      unname(preds * sd_val + mean_val)
    }
    
    model_results <- eventReactive(input$train_btn, {
      data <- stock_data()
      req(data) # Stop if data fetch failed
      
      type <- input$model_type
      horizon <- input$horizon
      
      showNotification(paste("Training", type, "Model..."), id="train_notif", duration = NULL)
      
      tryCatch({
        res <- list()
        
        # 1. GARCH
        if (type == "GARCH" || type == "Ensemble") {
          spec <- ugarchspec(
            variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
            mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
            distribution.model = "norm"
          )
          fit <- ugarchfit(spec = spec, data = data$returns)
          forc <- ugarchforecast(fit, n.ahead = horizon)
          
          last_price <- tail(data$close, 1)
          f_returns <- fitted(forc)
          f_sigma <- sigma(forc)
          
          cum_ret <- cumsum(as.numeric(f_returns))
          garch_prices <- last_price * exp(cum_ret)
          cum_sigma <- sqrt(cumsum(as.numeric(f_sigma)^2)) 
          
          res$GARCH <- list(
            mean = unname(garch_prices), 
            lower = unname(last_price * exp(cum_ret - 2 * cum_sigma)), 
            upper = unname(last_price * exp(cum_ret + 2 * cum_sigma))
          )
        }
        
        # 2. LSTM
        if (type == "LSTM" || type == "Ensemble") {
          mod <- train_torch_model(data, "LSTM", input$epochs, input$lookback, input$hidden_size)
          preds <- forecast_torch(mod, horizon)
          res$LSTM <- list(mean = preds)
        }
        
        # 3. RNN
        if (type == "RNN" || type == "Ensemble") {
          mod <- train_torch_model(data, "RNN", input$epochs, input$lookback, input$hidden_size)
          preds <- forecast_torch(mod, horizon)
          res$RNN <- list(mean = preds)
        }
        
        # Ensemble Logic
        if (type == "Ensemble") {
          p1 <- res$GARCH$mean
          p2 <- res$LSTM$mean
          p3 <- res$RNN$mean
          
          avg_price <- (p1 + p2 + p3) / 3
          e_min <- pmin(p1, p2, p3)
          e_max <- pmax(p1, p2, p3)
          
          res$Ensemble <- list(mean = avg_price, lower = e_min, upper = e_max)
        }
        
        showNotification("Forecasting Complete!", id="train_notif")
        
        if (type == "Ensemble") res$Ensemble else res[[type]]
        
      }, error = function(e) {
        showNotification(paste("Training Failed:", e$message), type = "error", id="train_notif")
        NULL
      })
    })
    
    # --------------------------------------------------------------------------
    # Plotting
    # --------------------------------------------------------------------------
    output$forecast_plot <- renderPlot({
      res <- model_results()
      hist_data <- stock_data()
      
      # Combine History + Forecast
      # Last Date
      last_date <- tail(hist_data$date, 1)
      future_dates <- seq(last_date + 1, by = "days", length.out = length(res$mean))
      
      forecast_df <- tibble(
        date = future_dates,
        close = res$mean,
        type = "Forecast"
      )
      
      # Add bounds if they exist
      if (!is.null(res$lower)) {
        forecast_df$lower <- res$lower
        forecast_df$upper <- res$upper
      }
      
      hist_df <- tail(hist_data, 90) |> mutate(type = "History") # Show last 90 days context
      
      p <- ggplot() +
        geom_line(data = hist_df, aes(x = date, y = close, color = "History"), size = 1) +
        geom_line(data = forecast_df, aes(x = date, y = close, color = "Forecast"), size = 1, linetype = "dashed") +
        scale_color_tq() +
        theme_tq() +
        labs(title = paste("Price Forecast:", input$model_type), y = "Price", x = "")
      
      if (!is.null(res$lower)) {
        p <- p + geom_ribbon(data = forecast_df, aes(x = date, ymin = lower, ymax = upper), fill = "blue", alpha = 0.2)
      }
      
      p
    })
    
    output$model_metrics <- renderPrint({
      res <- model_results()
      cat("Final Forecasted Price (", input$horizon, " days): ", dollar(tail(res$mean, 1)), "\n", sep="")
      if (!is.null(res$lower)) {
        cat("95% Interval: [", dollar(tail(res$lower, 1)), ", ", dollar(tail(res$upper, 1)), "]", sep="")
      }
    })
    
  })
}
