# Module UI
dataUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Data Management"),
      layout_columns(
        col_widths = c(4, 8),
        card_body(
          actionButton(ns("sync_db"), "Sync from Google Sheets", icon = icon("sync"), class = "btn-warning w-100"),
          br(), br(),
          actionButton(ns("seed_demo"), "Load Demo Data (Testing)", icon = icon("vial"), class = "btn-secondary w-100"),
          hr(),
          helpText("This pulls the latest data from Tiller into your local database.")
        ),
        card_body(
          h5("Database Status"),
          textOutput(ns("db_status")),
          tableOutput(ns("sample_data"))
        )
      )
    )
  )
}

# Module Server
dataServer <- function(id, con, sheet_url) {
  moduleServer(id, function(input, output, session) {
    
    # Trigger for updates
    data_trigger <- reactiveVal(0)
    
    # Sync Logic
    observeEvent(input$sync_db, {
      req(sheet_url)
      showNotification("Syncing with Tiller Sheet...", type = "message", id = "sync_n")
      
      tryCatch({
        # Read from Google Sheets
        # Assuming Tiller structure. Adjust cols as necessary.
        raw_df <- read_sheet(sheet_url, sheet = "Balances") |>
          select(Date, Account, Amount, any_of(c("Asset Class", "Symbol", "Description")))
        
        # Basic cleaning
        clean_df <- raw_df |>
          mutate(
            Date = as.Date(Date),
            # Normalize column names for DB
            account = Account,
            value = Amount,
            asset_class = if("Asset Class" %in% names(raw_df)) `Asset Class` else "Unknown",
            symbol = if("Symbol" %in% names(raw_df)) Symbol else NA,
            price = NA, # Tiller Balances often don't have price/qty, just total value. 
            quantity = NA
          ) |>
          select(snapshot_date = Date, account, symbol, quantity, price, value, asset_class)
        
        # Write to DuckDB (Overwrite)
        # Using a transaction is safer but for simple usage:
        dbExecute(con, "DELETE FROM portfolio_snapshots")
        dbWriteTable(con, "portfolio_snapshots", clean_df, append = TRUE)
        
        showNotification("Sync Complete!", type = "message", id = "sync_n")
        data_trigger(data_trigger() + 1)
        
      }, error = function(e) {
        showNotification(paste("Sync Failed:", e$message), type = "error", id = "sync_n")
      })
    })

    # Demo Data Logic
    observeEvent(input$seed_demo, {
      showNotification("Generating synthetic data...", id = "demo_n")
      tryCatch({
        source("R/demo_data.R") # Load generator function
        fake_df <- generate_demo_data()
        
        dbExecute(con, "DELETE FROM portfolio_snapshots")
        dbWriteTable(con, "portfolio_snapshots", fake_df, append = TRUE)
        
        showNotification("Demo Data Loaded!", type = "message", id = "demo_n")
        data_trigger(data_trigger() + 1)
      }, error = function(e) {
        showNotification(paste("Demo Load Failed:", e$message), type = "error", id = "demo_n")
      })
    })
    
    # Status
    output$db_status <- renderText({
      data_trigger()
      count <- dbGetQuery(con, "SELECT COUNT(*) as n FROM portfolio_snapshots")$n
      paste("Total Records:", count, "| Latest Sync:", Sys.time())
    })
    
    # Sample
    output$sample_data <- renderTable({
      data_trigger()
      tbl(con, "portfolio_snapshots") |> 
        head(5) |> 
        collect() |>
        mutate(
          snapshot_date = as.character(as.Date(snapshot_date, origin = "1970-01-01")),
          price = scales::dollar(price),
          value = scales::dollar(value),
          quantity = scales::comma(quantity, accuracy = 0.01)
        ) |>
        rename(
          Date = snapshot_date,
          Account = account,
          Ticker = symbol,
          Qty = quantity,
          Price = price,
          Value = value,
          Class = asset_class
        )
    })
    
    # Return the lazy table for other modules to use
    return(reactive({
      data_trigger()
      tbl(con, "portfolio_snapshots")
    }))
  })
}
