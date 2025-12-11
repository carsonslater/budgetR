# Module UI
reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Generate Reports"),
      layout_sidebar(
        sidebar = sidebar(
          dateRangeInput(ns("report_range"), "Report Period", 
                         start = floor_date(Sys.Date() - months(1), "month"), 
                         end = ceiling_date(Sys.Date() - months(1), "month") - days(1)),
          downloadButton(ns("dl_report"), "Download PDF Report", class = "btn-info")
        ),
        p("Generates a performance report based on the selected date range.")
      )
    )
  )
}

# Module Server
reportServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    output$dl_report <- downloadHandler(
      filename = function() {
        paste0("Portfolio_Report_", format(Sys.Date(), "%Y%m%d"), ".pdf")
      },
      content = function(file) {
        showNotification("Generating Report...", type = "message", id = "gen_rep")
        
        # Get data
        df <- data_reactive() |>
          filter(snapshot_date >= !!input$report_range[1], 
                 snapshot_date <= !!input$report_range[2]) |>
          collect()
        
        # Temp paths
        tempReport <- file.path(tempdir(), "monthly_template.Rmd")
        file.copy("reports/monthly_template.Rmd", tempReport, overwrite = TRUE)
        
        # Render
        rmarkdown::render(
          tempReport, 
          output_file = file,
          params = list(
            data = df,
            start_date = input$report_range[1],
            end_date = input$report_range[2]
          ),
          envir = new.env(parent = globalenv())
        )
        
        showNotification("Report Downloaded", type = "message", id = "gen_rep")
      }
    )
    
  })
}
