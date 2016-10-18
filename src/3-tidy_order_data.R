# tidy order data

library(tidyverse)
library(stringr)
library(lubridate)
library(edwr)

details <- read_data("data/raw", "details") %>%
    as.order_info()

draw_times <- details %>%
    filter(detail.descr == "Requested Start Date/Time") %>%
    distinct(pie.id, order.id, detail.datetime)

ords <- read_data("data/raw", "orders") %>%
    as.order_by() %>%
    left_join(draw_times, by = c("pie.id", "order.id"))

order_times <- ords %>%
    select(pie.id, order.id, order, detail.datetime, order.status, action.datetime) %>%
    distinct(pie.id, order.id, order, detail.datetime, order.status, .keep_all = TRUE) %>%
    spread(order.status, action.datetime)

orders_valid <- order_times %>%
    filter(is.na(Canceled),
           is.na(Discontinued)) %>%
    select(-Canceled, -Discontinued) %>%
    mutate(request = str_detect(order, "Request"),
           collect_diff = difftime(Collected, detail.datetime, units = "mins"))

saveRDS(orders_valid, "data/tidy/orders_valid.Rds")
