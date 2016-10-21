# tidy order data

library(tidyverse)
library(stringr)
library(lubridate)
library(edwr)

dirr::get_rds("data/tidy")

order_actions <- read_data("data/raw", "action") %>%
    as.order_action()

system_requests <- order_actions %>%
    filter(action.type == "Order",
           action.provider == "SYSTEM") %>%
    distinct(pie.id, order.id)

actions <- order_actions %>%
    anti_join(system_requests, by = "order.id") %>%
    select(pie.id, order.id, order.status, action.datetime) %>%
    arrange(pie.id, order.id, action.datetime) %>%
    distinct(pie.id, order.id, order.status, .keep_all = TRUE) %>%
    spread(order.status, action.datetime)

timing <- order_timing %>%
    anti_join(system_requests, by = "order.id") %>%
    arrange(pie.id, order.id, order.datetime, review.datetime) %>%
    distinct(pie.id, order.id, .keep_all = TRUE)

details <- read_data("data/raw", "details") %>%
    as.order_info() %>%
    anti_join(system_requests, by = "order.id")

request_times <- details %>%
    filter(detail.descr == "Requested Start Date/Time") %>%
    distinct(pie.id, order.id, detail.datetime)

priority <- details %>%
    filter(detail.descr %in% c("Collection Priority", "Frequency")) %>%
    spread(detail.descr, detail) %>%
    select(-detail.datetime) %>%
    rename(priority = `Collection Priority`,
           freq = Frequency)

# if lab ordered as Early AM, then change request date/time to next day at 0300
make_early <- function(x) {
    dt <- x + days(1)
    hour(dt) <- 3
    minute(dt) <- 0
    dt
}

req_times <- left_join(request_times, priority, by = c("pie.id", "order.id")) %>%
    mutate(new_time = make_early(detail.datetime),
           detail.datetime = if_else(freq == "Early AM", new_time, detail.datetime, new_time))

orders <- left_join(timing, actions, by = c("pie.id", "order.id")) %>%
    left_join(req_times, by = c("pie.id", "order.id")) %>%
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
           shift = if_else(hour(detail.datetime) >= 7 & hour(detail.datetime) < 19, "day", "night")) %>%
    group_by(pie.id) %>%
    arrange(pie.id, detail.datetime, order.action.datetime) %>%
    mutate(mult_levels = is.na(Collected) &
               lead(Collected) <= detail.datetime + hours(6) &
               lag(Collected >= detail.datetime - hours(6)))

requests <- orders %>%
    filter(str_detect(order, "Request"),
           is.na(Canceled),
           is.na(Discontinued))

reqs_completed <- requests %>%
    select(pie.id, req_id = order.id, order_req = order, req_time = detail.datetime) %>%
    left_join(orders, by = "pie.id") %>%
    filter(req_id != order.id,
           Collected >= req_time - hours(2),
           Collected <= req_time + hours(2)) %>%
    mutate(req_completed = TRUE) %>%
    select(pie.id, order.id = req_id, req_completed)

orders_requests <- left_join(requests, reqs_completed, by = c("pie.id", "order.id")) %>%
    mutate(req_completed = coalesce(req_completed, FALSE))

orders_valid <- orders %>%
    filter(is.na(cancel.action.datetime),
           mult_levels != TRUE)

saveRDS(orders_valid, "data/tidy/orders_valid.Rds")
saveRDS(orders_requests, "data/tidy/orders_requests.Rds")
