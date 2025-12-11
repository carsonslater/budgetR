generate_demo_data <- function(years = 2) {
  # Generate synthetic portfolio history
  dates <- seq(Sys.Date() - years*365, Sys.Date(), by="month")
  
  # Assets
  tickers <- c("SPY", "TLT", "GLD", "MSFT", "TSLA")
  base_prices <- c(400, 100, 180, 300, 200)
  volatility <- c(0.15, 0.10, 0.12, 0.25, 0.40) # Annual vol
  
  # Generate paths
  snapshots <- map_dfr(seq_along(tickers), function(i) {
    tick <- tickers[i]
    p0 <- base_prices[i]
    vol <- volatility[i]
    mu <- 0.08 # Drift
    
    # 1. Generate Price Path
    n <- length(dates)
    dt <- 1/12
    returns <- rnorm(n, mean = (mu - 0.5*vol^2)*dt, sd = vol*sqrt(dt))
    prices <- p0 * exp(cumsum(returns))
    
    # 2. Generate Quantity (accumulation)
    # Start with 10 units, add 1 every month
    qty <- seq(10, 10 + n - 1, length.out = n)
    
    tibble(
      snapshot_date = dates,
      account = if(i <= 3) "Fidelity Brokerage" else "Robinhood Spec",
      symbol = tick,
      quantity = qty,
      price = prices,
      value = qty * prices,
      asset_class = if(tick == "TLT") "Fixed Income" else "Equity"
    )
  })
  
  snapshots
}
