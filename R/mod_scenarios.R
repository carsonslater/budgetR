# Module UI
scenariosUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Financial Projects & Goals"),
      layout_sidebar(
        sidebar = sidebar(
          h5("Add New Project"),
          textInput(ns("proj_name"), "Project Name", placeholder = "e.g., Tesla Model 3"),
          numericInput(ns("target_amount"), "Target Amount ($)", value = 50000, min = 0),
          dateInput(ns("target_date"), "Target Date", value = Sys.Date() + 365*5),
          numericInput(ns("current_saved"), "Current Savings ($)", value = 0, min = 0),
          sliderInput(ns("assumed_rate"), "Assumed Annual Return (%)", min = 0, max = 15, value = 7, step = 0.5),
          actionButton(ns("add_project"), "Save Project", class = "btn-success")
        ),
        
        div(
          h4("Manage Projects"),
          layout_columns(
            col_widths = c(8, 4),
            uiOutput(ns("project_select_ui")),
            actionButton(ns("delete_proj"), "Delete Selected", class = "btn-outline-danger w-100", icon = icon("trash"), style = "margin-top: 25px;")
          ),
          tableOutput(ns("projects_table")),
          hr(),
          h4("Analysis"),
          plotOutput(ns("projects_plot"))
        )
      )
    )
  )
}

# Module Server
scenariosServer <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive trigger for updates
    projects_trigger <- reactiveVal(0)
    
    # Add Project Logic
    observeEvent(input$add_project, {
      req(input$proj_name, input$target_amount)
      
      # Basic ID generation (max + 1)
      current_max <- dbGetQuery(con, "SELECT MAX(id) as max_id FROM projects")$max_id
      new_id <- ifelse(is.na(current_max), 1, current_max + 1)
      
      # Insert into DuckDB
      dbExecute(con, "INSERT INTO projects VALUES (?, ?, ?, ?, ?, ?, ?)",
                list(new_id, input$proj_name, input$target_amount, input$target_date, 
                     input$current_saved, input$assumed_rate / 100, Sys.time()))
      
      showNotification(paste("Project", input$proj_name, "saved!"), type = "message")
      projects_trigger(projects_trigger() + 1)
    })
    
    # Fetch Data
    projects_data <- reactive({
      projects_trigger()
      tbl(con, "projects") |> collect()
    })
    
    # Calculate Monthly Contributions
    analyzed_projects <- reactive({
      df <- projects_data()
      req(nrow(df) > 0)
      
      df |>
        mutate(
          months_remaining = as.numeric(difftime(target_date, Sys.Date(), units = "weeks")) / 4.345,
          months_remaining = ifelse(months_remaining < 1, 1, months_remaining), # Avoid division by zero
          
          # FV = PV * (1+r)^n + PMT * [((1+r)^n - 1) / r]
          # We need to solve for PMT
          # PMT = (FV - PV*(1+r)^n) / [((1+r)^n - 1) / r]
          
          monthly_rate = assumed_rate / 12,
          # Handle 0% interest case
          monthly_contrib = case_when(
            monthly_rate == 0 ~ (target_amount - current_saved) / months_remaining,
            TRUE ~ (target_amount - current_saved * (1 + monthly_rate)^months_remaining) / 
                   (( (1 + monthly_rate)^months_remaining - 1 ) / monthly_rate)
          )
        )
    })
    
    # Dynamic Selector
    output$project_select_ui <- renderUI({
      df <- projects_data()
      req(nrow(df) > 0)
      # Default: Select All
      selectInput(ns("visible_projects"), "Select Projects to View/Delete", 
                  choices = df$name, selected = df$name, multiple = TRUE)
    })
    
    # Delete Logic
    observeEvent(input$delete_proj, {
      req(input$visible_projects)
      
      # Safer to use parameterized queries, but for list of names it's tricky in simple DBI
      # We'll loop or use specific logic.
      # duckdb doesn't support 'IN ?' easily with vector binding in R DBI yet usually.
      # Let's simple loop for safety or paste string (careful of injection, but low risk here).
      
      selected <- input$visible_projects
      
      # remove from DB
      # iterate to be safe with DBI
      for (p_name in selected) {
        dbExecute(con, "DELETE FROM projects WHERE name = ?", list(p_name))
      }
      
      showNotification(paste("Deleted:", paste(selected, collapse=", ")), type = "warning")
      projects_trigger(projects_trigger() + 1)
    })
    
    output$projects_table <- renderTable({
      df <- analyzed_projects()
      # Filter if selection exists
      if (!is.null(input$visible_projects)) {
        df <- df |> filter(name %in% input$visible_projects)
      }
      
      df |>
        select(Project = name, `Target ($)` = target_amount, `By` = target_date, 
               `Saved ($)` = current_saved, `Return (%)` = assumed_rate, 
               `Months Left` = months_remaining, `Required Monthly Contrib ($)` = monthly_contrib) |>
        mutate(
          `Target ($)` = scales::dollar(`Target ($)`),
          `Saved ($)` = scales::dollar(`Saved ($)`),
          `Return (%)` = scales::percent(`Return (%)`),
          `Required Monthly Contrib ($)` = scales::dollar(`Required Monthly Contrib ($)`),
          `Months Left` = round(`Months Left`, 1)
        )
    })
    
    output$projects_plot <- renderPlot({
      df <- analyzed_projects()
      req(nrow(df) > 0, input$visible_projects)
      
      # Filter by selection
      df <- df |> filter(name %in% input$visible_projects)
      req(nrow(df) > 0)
      
      # Generate Growth Paths
      growth_data <- df |>
        rowwise() |>
        do({
          row <- .
          n_months <- ceiling(row$months_remaining)
          if (n_months <= 0) n_months <- 1
          
          # Sequence of dates
          dates <- seq(Sys.Date(), by = "month", length.out = n_months + 1)
          
          # Calculate balances
          # Balance_t = Previous * (1+r) + PMT
          # Instead of loop, use FV of annuity formula per t:
          # V_t = PV*(1+r)^t + PMT * [((1+r)^t - 1)/r]
          
          t <- 0:n_months
          pmt <- row$monthly_contrib
          r <- row$monthly_rate
          pv <- row$current_saved
          
          if (r == 0) {
            vals <- pv + pmt * t
          } else {
            vals <- pv * (1 + r)^t + pmt * ((1 + r)^t - 1) / r
          }
          
          tibble(
            Project = row$name,
            Date = dates,
            Value = vals,
            Target = row$target_amount
          )
        }) |>
        ungroup()

      growth_data |>
        ggplot(aes(x = Date, y = Value, color = Project)) +
        geom_line(size = 1.2) +
        geom_hline(aes(yintercept = Target, color = Project), linetype = "dashed", alpha = 0.5) +
        scale_y_continuous(labels = scales::dollar_format()) +
        scale_color_tq() +
        theme_tq() +
        labs(title = "Projected Goal Progress",
             subtitle = "Solid Line = Projected Balance | Dashed Line = Target",
             y = "Project Value", x = "")
    })
    
  })
}
