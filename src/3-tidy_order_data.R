# tidy order data

library(tidyverse)
library(stringr)
library(lubridate)
library(edwr)

dirr::get_rds("data/tidy")

order_actions <- read_data("data/raw", "action") %>%
    as.order_action()

actions <- order_actions %>%
    select(pie.id, order.id, order.status, action.datetime) %>%
    arrange(pie.id, order.id, action.datetime) %>%
    distinct(pie.id, order.id, order.status, .keep_all = TRUE) %>%
    spread(order.status, action.datetime)

timing <- order_timing %>%
    arrange(pie.id, order.id, order.datetime, review.datetime) %>%
    distinct(pie.id, order.id, .keep_all = TRUE)

details <- read_data("data/raw", "details") %>%
    as.order_info() %>%
    filter(detail.descr == "Requested Start Date/Time") %>%
    distinct(pie.id, order.id, detail.datetime)

orders <- left_join(timing, actions, by = c("pie.id", "order.id")) %>%
    left_join(details, by = c("pie.id", "order.id")) %>%
    mutate(order.action.datetime = coalesce(Ordered, Scheduled),
           cancel.action.datetime = coalesce(Canceled, Discontinued),
           collect_request_diff = as.numeric(difftime(Collected, request.datetime, units = "mins")),
           collect_detail_diff = as.numeric(difftime(Collected, detail.datetime, units = "mins")),
           request_diff = request.datetime == detail.datetime,
           request = str_detect(order, "Request"),
           timely90 = abs(collect_detail_diff) <= 90,
           timely60 = abs(collect_detail_diff) <= 60,
           timely30 = abs(collect_detail_diff) <= 30,
           sched_diff = as.numeric(difftime(detail.datetime, order.action.datetime, units = "hours")),
           shift = if_else(hour(detail.datetime) >= 7 & hour(detail.datetime) < 19, "day", "night"))

orders_valid <- orders %>%
    filter(is.na(cancel.action.datetime))

saveRDS(orders_valid, "data/tidy/orders_valid.Rds")
