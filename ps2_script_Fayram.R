library(tidyquant)
library(tidyverse)

unemp_raw <- tq_get(
  c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
  get  = "economic.data",
  from = "1976-01-01"
)

library(tidyquant)

test <- tq_get(
  "UNRATE",
  get = "economic.data",
  from = "1976-01-01"
)

head(test)

head(unemp_raw)

dir.create("data", showWarnings = FALSE)

write_rds(unemp_raw, "data/unemp_raw.rds")
