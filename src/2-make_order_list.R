# make list of vancomycin orders

library(tidyverse)
library(edwr)

ords <- read_data("data/raw", "orders") %>%
    as.order_by() %>%
    rename(order.id = `Source Order ID`,
           order.status = `Order Department Status - Generic`)

concat_encounters(unique(ords$order.id))
