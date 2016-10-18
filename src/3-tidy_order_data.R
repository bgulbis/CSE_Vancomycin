# tidy order data

library(tidyverse)
library(edwr)

ords <- read_data("data/raw", "orders") %>%
    as.order_by()

details <- read_data("data/raw", "details") %>%
    as.order_info()

