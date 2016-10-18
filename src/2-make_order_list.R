# make list of vancomycin orders

library(tidyverse)
library(edwr)

ords <- read_data("data/raw", "orders") %>%
    as.order_by()

concat_encounters(unique(ords$order.id))
