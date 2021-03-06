# tidy order data

library(tidyverse)
library(stringr)
library(lubridate)
library(edwr)
# library(assertr)

dirr::get_rds("data/tidy")

timing <- read_data("data/raw", "timing") %>%
    as.order_timing()

details <- read_data("data/raw", "details") %>%
    as.order_info()

order_actions <- read_data("data/raw", "action") %>%
    as.order_action()

early <- details %>%
    filter(detail.descr == "Frequency",
           detail == "Early AM")

early_am <- timing %>%
    semi_join(early, by = c("pie.id", "order.id"))

requests <- timing %>%
    filter(str_detect(order, "Request"))

cancelled_orders <- timing %>%
    filter(!is.na(cancel.datetime))

# dc_orders <- timing %>%
#     filter(!is.na(discontinue.datetime))
#
# dc_details <- details %>%
#     semi_join(dc_orders, by = c("pie.id", "order.id"))
#
# dc_actions <- order_actions %>%
#     semi_join(dc_orders, by = c("pie.id", "order.id")) %>%
#     select(pie.id, order.id, order.status, action.datetime) %>%
#     arrange(pie.id, order.id, action.datetime) %>%
#     distinct(pie.id, order.id, order.status, .keep_all = TRUE) %>%
#     spread(order.status, action.datetime) %>%
#     left_join(order_by, by = "order.id")
#
# dc_requests <- dc_actions %>%
#     semi_join(requests, by = c("pie.id", "order.id"))
#
# dc_early <- dc_orders %>%
#     semi_join(early_am, by = c("pie.id", "order.id"))
#
# dc_invest <- dc_orders %>%
#     anti_join(dc_requests, by = c("pie.id", "order.id")) %>%
#     anti_join(dc_early, by = c("pie.id", "order.id"))
#
# dc_cancel <- dc_invest %>%
#     semi_join(cancelled_orders, by = c("pie.id", "order.id"))

valid_timing <- timing %>%
    anti_join(early_am, c("pie.id", "order.id")) %>%
    anti_join(requests, c("pie.id", "order.id")) %>%
    anti_join(cancelled_orders, c("pie.id", "order.id"))

# dc_valid <- orders %>%
#     semi_join(dc_invest, by = c("pie.id", "order.id"))

valid_details <- details %>%
    semi_join(valid_timing, c("pie.id", "order.id"))

valid_actions <- order_actions %>%
    semi_join(valid_timing, c("pie.id", "order.id"))

request_times <- valid_details %>%
    filter(detail.descr == "Requested Start Date/Time") %>%
    arrange(pie.id, desc(detail.datetime)) %>%
    select(pie.id, order.id, detail.datetime) %>%
    distinct(pie.id, order.id, .keep_all = TRUE)

order_by <- valid_actions %>%
    filter(action.type == "Order",
           action.provider != "SYSTEM") %>%
    select(order.id, action.provider.role, action.comm)

request_actions <- order_actions %>%
    semi_join(requests, c("pie.id", "order.id"))

request_order_by <- request_actions %>%
    filter(action.type == "Order",
           action.provider != "SYSTEM") %>%
    select(order.id, action.provider.role, action.comm)

requests <- requests %>%
    left_join(request_order_by, by = "order.id")

actions <- valid_actions %>%
    select(pie.id, order.id, order.status, action.datetime) %>%
    arrange(pie.id, order.id, action.datetime) %>%
    distinct(pie.id, order.id, order.status, .keep_all = TRUE) %>%
    spread(order.status, action.datetime) %>%
    left_join(order_by, by = "order.id")

priority <- valid_details %>%
    filter(detail.descr %in% c("Collection Priority", "Frequency")) %>%
    spread(detail.descr, detail) %>%
    select(-detail.datetime) %>%
    rename(priority = `Collection Priority`,
           freq = Frequency)
    # filter(is.na(freq) | freq != "Early AM")

nurse_collect <- valid_actions %>%
    filter(order.status == "Collected") %>%
    select(pie.id, order.id, rn_collect = action.provider)

# system_requests <- order_actions %>%
#     filter(action.type == "Order",
#            action.provider == "SYSTEM") %>%
#     distinct(pie.id, order.id)
#
# order_by <- order_actions %>%
#     filter(action.type == "Order",
#            action.provider != "SYSTEM") %>%
#     select(order.id, action.provider.role)
#
# actions <- order_actions %>%
#     anti_join(system_requests, by = "order.id") %>%
#     select(pie.id, order.id, order.status, action.datetime) %>%
#     arrange(pie.id, order.id, action.datetime) %>%
#     distinct(pie.id, order.id, order.status, .keep_all = TRUE) %>%
#     spread(order.status, action.datetime) %>%
#     left_join(order_by, by = "order.id")
#
# order_comm <- order_actions %>%
#     anti_join(system_requests, by = "order.id") %>%
#     select(pie.id, order.id, action.comm) %>%
#     filter(!is.na(action.comm)) %>%
#     distinct()
#
# timing <- read_data("data/raw", "timing") %>%
#     as.order_timing() %>%
#     anti_join(system_requests, by = "order.id") %>%
#     arrange(pie.id, order.id, order.datetime) %>%
#     distinct(pie.id, order.id, .keep_all = TRUE)
#
# details <- read_data("data/raw", "details") %>%
#     as.order_info() %>%
#     anti_join(system_requests, by = "order.id")
#
# request_times <- details %>%
#     filter(detail.descr == "Requested Start Date/Time") %>%
#     arrange(pie.id, desc(detail.datetime)) %>%
#     select(pie.id, order.id, detail.datetime) %>%
#     distinct(pie.id, order.id, .keep_all = TRUE)
#
# priority <- details %>%
#     filter(detail.descr %in% c("Collection Priority", "Frequency")) %>%
#     spread(detail.descr, detail) %>%
#     select(-detail.datetime) %>%
#     rename(priority = `Collection Priority`,
#            freq = Frequency) %>%
#     filter(is.na(freq) | freq != "Early AM")

# if lab ordered as Early AM, then change request date/time to next day at 0300
# make_early <- function(x) {
#     dt <- x + days(1)
#     hour(dt) <- 3
#     minute(dt) <- 0
#     dt
# }
#
# req_times <- left_join(request_times, priority, by = c("pie.id", "order.id")) %>%
#     mutate(new_time = make_early(detail.datetime),
#            detail.datetime = if_else(freq == "Early AM", new_time, detail.datetime, new_time))

# miss <- full_join(timing, actions, by = c("pie.id", "order.id")) %>%
#     filter(is.na(order.unit))

# id <- concat_encounters(miss$order.id)

# check_detail_time <- function(x) {
#
# }

orders <- full_join(valid_timing, actions, by = c("pie.id", "order.id")) %>%
    left_join(request_times, by = c("pie.id", "order.id")) %>%
    left_join(priority, by = c("pie.id", "order.id")) %>%
    left_join(nurse_collect, by = c("pie.id", "order.id")) %>%
    # corrected data for rows with detail.datetime that had incorrect years
    mutate(detail.datetime = if_else(detail.datetime < order.datetime - days(1), detail.datetime + years(1), detail.datetime)) %>%
    # verify(detail.datetime >= order.datetime - days(1)) %>%
    # left_join(order_comm, by = c("pie.id", "order.id")) %>%
    # filter(is.na(discontinue.datetime),
    #        is.na(cancel.datetime),
    #        is.na(Canceled),
    #        is.na(Discontinued)) %>%
    mutate(collect_detail_diff = as.numeric(difftime(Collected, detail.datetime, units = "mins")),
           order_detail_diff = as.numeric(difftime(detail.datetime, order.datetime, units = "hours")),
           order_dispatch_diff = as.numeric(difftime(Dispatched, order.datetime, units = "hours")),
           dispatch_detail_diff = as.numeric(difftime(detail.datetime, Dispatched, units = "hours")),
           early_am = priority == "Routine" & is.na(freq),
           timely120 = abs(collect_detail_diff) <= 120,
           timely60 = abs(collect_detail_diff) <= 60,
           appropriate = timely60 | (early_am & timely120),
           early = collect_detail_diff <= -120,
           late = collect_detail_diff >= 240,
           sched_diff = as.numeric(difftime(detail.datetime, Scheduled, units = "hours")),
           shift = if_else(hour(detail.datetime) >= 7 & hour(detail.datetime) < 19, "day", "night")) %>%
    group_by(pie.id) %>%
    arrange(pie.id, detail.datetime) %>%
    mutate(mult_levels = is.na(Collected) &
               lead(Collected) <= detail.datetime + hours(3) &
               lag(Collected) >= detail.datetime - hours(3))

# requests <- orders %>%
#     # select(pie.id:order.datetime, action.comm, request, Canceled:detail.datetime) %>%
#     filter(request == TRUE) %>%
#     arrange(pie.id, order.datetime)
#
# not_completed <- orders %>%
#     filter(request == FALSE,
#            is.na(Collected))
#     # filter(request == TRUE,
#     #        is.na(Canceled),
#     #        is.na(Discontinued))
#
# reqs_completed <- requests %>%
#     select(pie.id, req_id = order.id, order_req = order, req_time = detail.datetime) %>%
#     left_join(orders, by = "pie.id") %>%
#     filter(req_id != order.id,
#            Collected >= req_time - hours(2),
#            Collected <= req_time + hours(2)) %>%
#     mutate(req_completed = TRUE) %>%
#     select(pie.id, order.id = req_id, req_completed)
#
# orders_requests <- requests %>%
    # left_join(requests, reqs_completed, by = c("pie.id", "order.id")) %>%
    # mutate(req_completed = coalesce(req_completed, FALSE))

# orders_valid <- orders %>%
    # filter(is.na(cancel.action.datetime),
           # mult_levels != TRUE)
    # filter(mult_levels != TRUE)

# df <- full_join(levels, orders_valid, by = c("pie.id", "order.id"))
#
# missing_orders <- df %>%
#     filter(is.na(order.unit))
#
# id <- concat_encounters(missing_orders$order.id)

saveRDS(orders, "data/tidy/orders_valid.Rds")
saveRDS(requests, "data/tidy/orders_requests.Rds")
saveRDS(early_am, "data/tidy/orders_early_am.Rds")
# saveRDS(levels, "data/tidy/levels.Rds")
