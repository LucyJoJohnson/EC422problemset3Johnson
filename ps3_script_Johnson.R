
# Read the CSV file + Inspect the Date Column
library(tidyverse)
retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)
retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

# 2.3 Reshape your data to long format
retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")
head(retail_raw_long)

# 2.4 Build your tsibble
library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)
head(retail_ts)
has_gaps(retail_ts)

# 2.5 Canadian Gas Production
head(canadian_gas)
# canadian_gas has less columns than retail_ts, and only has one series
key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)
has_gaps(can_gas)
head(can_gas)

# 2.6 Working with Dates in a tsibble
can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year    = year(Month),
         month   = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)
# We sum instead of take the mean of the monthly values of gas production because we want the total values per quarter of gas produced instead of the average. The quarterly series appear to still be seasonal

# 2.7 Plotting with Autoplot
retail_ts |> autoplot(sales)

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
# The seasonal adjustment is smoothing out the line so there is less spikes and dips. The repeating pattern is that there is a spike and then a dip each year. The SA series has this pattern but the spikes and dips are less extreme. The two series follow the same path over the long run
library(ggplot2)
can_gas |>
  autoplot() +
  labs(
    title = "Monthly Natural Gas Production in Canada",
    subtitle = "Measured in Billions of Cubic Meters",
    x = "Year", y = "Natural Gas Production", color = NULL
  ) +
  theme_minimal()

can_gas |>
  autoplot() +
  geom_smooth()
  labs(
    title = "Monthly Natural Gas Production in Canada",
    subtitle = "Measured in Billions of Cubic Meters",
    x = "Year", y = "Natural Gas Production", color = NULL
  ) +
  theme_minimal()
# I chose geom_smooth because I wanted to see clearly how the trend went up because there are so many spikes and dips I wanted the trend to be easy to track.

# 3.1 Log Transformation
# The swings get larger as the series grows, but only in the middle of the graph. Then, the swings get smaller and stabilize around 1992.
  can_gas <- can_gas |>
    mutate(log_volume = log(Volume))
head(can_gas)  

can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Log of Monthly Natural Gas Production in Canada",
    x = "Year",
    y = "Log of Natural Gas Production"
  ) +
  theme_minimal()
# The plot is still trending upward, but at an overall decreasing rate instead of increasing rate. The size of the seasonal swings in the middle do not change thatmuch, leading me to believe that a log transformation is not the right transformation for the data. This is because the data does not become linear with the log transoformation, it instead just changes shape.

retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA")
retail_nsa <- retail_nsa |>
  mutate(log_sales = log(sales))

retail_nsa |>
  autoplot(sales) +
  labs(
    title = "NSA Retail Sales",
    x = "Year",
    y = "Retail Sales"
  )

retail_nsa |>
  autoplot(log_sales) +
  labs(
    title = "Log of NSA Retail Sales",
    x = "Year",
    y = "Log Retail Sales"
  )
# I think that a log transformation for retail sales is better than for can_gas because with the log transformation, the graph looks more linear. 

# 3.2 Growth Rates
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * (Volume - lag(Volume)) / lag(Volume),
    growth_log    = 100 * (log(Volume) - lag(log(Volume)))
  )
can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Canadian gas production: month-over-month growth",
    x = "Month",
    y = "Growth rate (%)"
  )

can_gas <- can_gas |>
  mutate(growth_gap = abs(growth_direct - growth_logdiff))
can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Gap between direct and log-difference growth rates",
    x = "Month",
    y = "Absolute gap (%)"
  )
# The log approximation is very good because the farthest difference is just above 3 percent, and a lot of the differences are at zero.

# 3.3 SA vs NSA Growth Rates
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()
# The non-seasonally adjusted data has a lot more extreme spikes and dips than the seasonally adjusted data, just like the graph in 2.7b. The SA data still has spikes and dips but they are much less extreme. 
# 4.1 A Season Plot
retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

# The non-seasonally adjusted graph has a rise around February and November, while the seasonally adjusted graph is mostly horizontal with a few yearly exceptions, not showing a seasonal pattern

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Plot: Log of Canadian Gas Production",
    x = "Month",
    y = "Log(Volume)"
  )
# I see a relatively small seasonal pattern, but there generally is a dip from March to November, which gets more extreme as the years get older. The highest months of gas production are December-January. This makes sense because December-January are on average very cold months
# 4.2 A Subseries Plot

can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

# 4.3 Seasonally Adjusting a Series with X-13ARIMA-SEATS

retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()
# December is when the seasonal factor is highest. In December it is 1.2 units above sales in that month. The seasonal swings do change overtime, becoming smaller. 

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()
# My adjustment closely matches the Census Bureau adjustment. However, the two adjustments differ the most a little after Janurary 2020, when the differences are above 1 %, and become more negative. Some of this ispossible due to COVID. 

# 4.4. A Simple Model – Linear Trend + Seasonal Dummies
can_gas_fit <- can_gas |>
  model(TSLM(log_volume ~ trend() + season()))

report(can_gas_fit)

can_gas_fit |>
  tidy() |>
  select(term, estimate, std.error, statistic, p.value) |>
  knitr::kable(digits = 3)
# The estimated average monthly growth rate is .4%. The month with the largest seasonal effect is month 6 which is June with an aboslute T stat of 4.777

can_gas_fit |>
  augment() |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = log_volume, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Fitted")) +
  labs(title = "Linear Trend + Seasonal Dummies Fit: Canadian Gas Production",
       y = "log(Volume)", color = NULL) +
  theme_minimal()
# The fitted line does not track the actual series very fell. The fitted line is linear, while the actual series is not linear. The fitted does well with the values from January 1990 to January 2000, but does not capture the trend of the data very well from anywhere else. 
can_gas_fit |> gg_tsresiduals()
can_gas_fit |>
  augment() |>
  features(.innov, ljung_box, lag = 24)
# The residuals in the graph do not look like white noise; they have a clear trend first going up and then gradually going down. The LB test rejects the null at the 5 percent level with a p-value of zero

# 5.1 An Expanding Window TSCV
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  stretch_tsibble(.init = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)
gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))
# The date of the first forecast is January 1980, and the date of the last forecast is February 2005. 
gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE)) |>
  select(.model, MAE, RMSE) |>
  knitr::kable(digits = 1)
# The snaive model forecasts more accurately because the MAE and RMSE are both lower than the MAE and RMSE of the tslm model. The metrics do not agree

# 5.2 Is the Difference Significant?
errs <- gas_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(can_gas) |> select(Month, actual = Volume),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()

forecast::dm.test(errs$snaive, errs$tslm, h = 1, power = 1)
# DM stat: -31.278, p-value: 2.2e-16. We reject the null that the two model's forecasts are equally accurate.

# 5.3 A Rolling Window
gas_cv_fc <- can_gas |>
  select(Month, Volume) |>
  slide_tsibble(.size = 240, .step = 1) |>
  model(snaive = SNAIVE(Volume),
        tslm   = TSLM(log(Volume) ~ trend() + season())) |>
  forecast(h = 1)
gas_cv_fc |>
  accuracy(can_gas, measures = list(MAE = MAE, RMSE = RMSE))
# The tslm model accuracy changed more after the switch to a rolling window. This is because the rolling window only includes the most recent 240 months, which is the most recent trend. 

