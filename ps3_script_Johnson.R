
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
