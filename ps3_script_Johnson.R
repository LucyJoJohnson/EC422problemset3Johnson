
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
