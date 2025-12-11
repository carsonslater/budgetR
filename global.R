library(shiny)
library(bslib)
library(tidyverse)
library(googlesheets4)
library(duckdb)
library(duckplyr)
library(tidyquant)
library(dbplyr)
library(rmarkdown)
library(torch)
library(rugarch)
library(timetk)

# ==============================================================================
# Database Connection
# ==============================================================================

# Create/Connect to a local persistent DuckDB database
# We use a file-based DB so 'Projects' are saved between sessions.
db_path <- "budgetr_db.duckdb"
con <- dbConnect(duckdb::duckdb(), db_path)

# Ensure tables exist
if (!dbExistsTable(con, "projects")) {
  dbExecute(con, "CREATE TABLE projects (
    id INTEGER PRIMARY KEY,
    name VARCHAR,
    target_amount DOUBLE,
    target_date DATE,
    current_saved DOUBLE,
    assumed_rate DOUBLE,
    created_at TIMESTAMP
  )")
}

if (!dbExistsTable(con, "portfolio_snapshots")) {
  # We will sync Tiller data here
  dbExecute(con, "CREATE TABLE portfolio_snapshots (
    snapshot_date DATE,
    account VARCHAR,
    symbol VARCHAR,
    quantity DOUBLE,
    price DOUBLE,
    value DOUBLE,
    asset_class VARCHAR
  )")
}

# Use duckplyr for efficient data manipulation
# Note: For strict duckplyr usage, we would use tbl_duckdb(con, "table_name")

# ==============================================================================
# Configuration
# ==============================================================================

# Options
options(gargle_oauth_email = TRUE)

# Placeholder for User's Sheet URL - User must update this!
# In a real app, this might be an input or config file.
SHEET_URL <- "https://docs.google.com/spreadsheets/d/YOUR_SHEET_ID_HERE"

# On stop, close connection
onStop(function() {
  dbDisconnect(con, shutdown = TRUE)
})
