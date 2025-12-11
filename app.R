# Sourcing Global (Environment Setup)
# In a typical Shiny deployment, this runs automatically, but explicit sourcing helps local dev.
source("global.R")

# Load Modules
# source("R/mod_data_sync.R")
source("R/mod_data.R")
# source("R/mod_portfolio.R") # File not created yet
source("R/mod_advanced_models.R")
source("R/mod_forecast.R")
source("R/mod_scenarios.R")
source("R/mod_report.R")

# ==============================================================================
# UI
# ==============================================================================

ui <- page_navbar(
  title = "BudgetR",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  nav_panel("Projects & Goals", scenariosUI("scenarios")),
  nav_panel("Forecasting", forecastUI("forecast")),
  nav_panel("Advanced Analytics", advancedModelsUI("advanced")),
  nav_panel("Reports", reportUI("reports")),
  nav_panel("Data Management", dataUI("data")),
  
  nav_spacer(),
  nav_item(
    a(href = "https://github.com/carson", target = "_blank", icon("github"))
  )
)

# ==============================================================================
# Server
# ==============================================================================

server <- function(input, output, session) {
  
  # Connect to Data Module to get the Reactive Data Handle
  # We pass 'con' (from global.R) and the Configured Sheet URL
  portfolio_tbl <- dataServer("data", con, SHEET_URL)
  
  # Projects Module
  scenariosServer("scenarios", con)
  
  # Forecast Module
  forecastServer("forecast", con)

  # Advanced Models Module
  advancedModelsServer("advanced")
  
  # Reports Module
  reportServer("reports", portfolio_tbl)
  
}

# Run
shinyApp(ui, server)
