# make figures for graduation presentation

library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
# library(broom)
# library(qicharts)
library(themebg)
library(ReporteRs)

x <- dirr::get_rds("data/tidy")

end_date <- "2017-04-16"
baseline <- "2016-12-31"

hvi <- c("HH CVICU", "HH CVIMU", "HH HFIC", "HH HFIM", "HH 5HVI", "HH CCU", "HVI CIMU")
# hvi <- "HH CVICU"

orders_levels <- full_join(vanc_levels, orders_valid, by = c("pie.id", "order.id")) %>%
    filter(event.unit %in% hvi | (is.na(event.unit) & order.unit %in% hvi)) %>%
    mutate(not_collected = is.na(Collected) & is.na(lab.result) & (!mult_levels | is.na(mult_levels)),
           location = coalesce(event.unit, order.unit),
           month = floor_date(detail.datetime, unit = "month"),
           week = floor_date(detail.datetime, unit = "week"),
           day = wday(detail.datetime, label = TRUE, abbr = TRUE),
           hour = hour(detail.datetime),
           day_order = wday(order.datetime, label = TRUE, abbr = TRUE),
           hour_order = hour(order.datetime),
           exceed_2hr = abs(collect_detail_diff) > 120,
           pilot = detail.datetime >= mdy("1/30/2017", tz = "US/Central"),
           future_order = order_detail_diff > 1) %>%
    dmap_at(c("timely120", "timely60", "appropriate"), ~ coalesce(.x, FALSE)) %>%
    dmap_at("future_order", ~if_else(.x, "Future", "Now"))

data_orders <- orders_levels %>%
    filter(priority %in% c("Routine", "Stat", "Timed Study"),
           detail.datetime >= order.datetime - hours(1)) %>%
    mutate(priority = if_else(early_am, "Early AM", priority))

collected <- data_orders %>%
    filter(!is.na(Collected),
           order.datetime <= ymd(baseline)) %>%
    group_by(event.unit) %>%
    summarize(collected = n(),
              timely = sum(appropriate, na.rm = TRUE),
              early = sum(early, na.rm = TRUE),
              late = sum(late, na.rm = TRUE),
              late_2hr = sum(exceed_2hr, na.rm = TRUE))

not_done <- data_orders %>%
    filter(is.na(Collected),
           is.na(lab.result),
           order.datetime <= ymd(baseline)) %>%
    group_by(order.unit) %>%
    count %>%
    rename(not_done = n)

requests <- orders_requests %>%
    filter(order.datetime <= ymd(baseline)) %>%
    group_by(order.unit) %>%
    count %>%
    rename(requests = n) %>%
    filter(order.unit %in% hvi)

early_am <- orders_early_am %>%
    filter(order.datetime <= ymd(baseline)) %>%
    group_by(order.unit) %>%
    count() %>%
    rename(early_am = n) %>%
    filter(order.unit %in% hvi)

priority <- data_orders %>%
    filter(!is.na(Collected),
           order.datetime <= ymd(baseline),
           priority %in% c("Routine", "Stat", "Timed Study")) %>%
    count(event.unit, priority) %>%
    group_by(event.unit) %>%
    spread(priority, n)

# percent_timely = timely / collected,

totals_units <- left_join(collected, not_done, by = c("event.unit" = "order.unit")) %>%
    left_join(requests, by = c("event.unit" = "order.unit")) %>%
    left_join(early_am, by = c("event.unit" = "order.unit")) %>%
    mutate(total = collected + not_done,
           defects = total - timely,
           percent_not_done = not_done / total,
           percent_appropriate = timely / total,
           percent_untimely = 1 - percent_appropriate,
           percent_exceed_2hr = late_2hr / total,
           percent_requests = requests / total,
           percent_early_am = early_am / total)

totals_hvi <- totals_units %>%
    ungroup() %>%
    summarize_at(vars(collected, timely, early, late, late_2hr, not_done, requests, early_am, total, defects), funs(sum), na.rm = TRUE) %>%
    mutate(event.unit = "HVI",
           percent_not_done = not_done / total,
           percent_appropriate = timely / total,
           percent_untimely = 1 - percent_appropriate,
           percent_exceed_2hr = late_2hr / total,
           percent_requests = requests / total,
           percent_early_am = early_am / total)

totals <- bind_rows(totals_units, totals_hvi) %>%
    select(event.unit, num_levels = total, everything()) %>%
    filter(!is.na(event.unit))

cvicu_reqs <- orders_requests %>%
    filter(order.unit == "HH CVICU",
           order.datetime <= ymd(baseline)) %>%
    mutate(priority = "Request") %>%
    select(pie.id, priority, action.provider.role)

cvicu_orders <- data_orders %>%
    filter(event.unit == "HH CVICU",
           detail.datetime >= order.datetime - hours(1)) %>%
    full_join(cvicu_reqs, by = c("pie.id", "priority", "action.provider.role"))

fig_order_type <- cvicu_orders %>%
    dmap_at("priority", fct_infreq) %>%
    dmap_at("priority", fct_rev) %>%
    ggplot(aes(x = priority)) +
    geom_bar() +
    xlab("Order Type") +
    ylab("Number of Orders") +
    theme_bg(xticks = FALSE) +
    coord_flip()

fig_provider_type <- cvicu_orders %>%
    filter(!is.na(action.provider.role)) %>%
    dmap_at("action.provider.role", fct_infreq) %>%
    dmap_at("action.provider.role", fct_lump, n = 4) %>%
    dmap_at("action.provider.role", fct_rev) %>%
    ggplot(aes(x = action.provider.role)) +
    geom_bar() +
    xlab("Provider Type") +
    ylab("Number of Orders") +
    theme_bg(yticks = FALSE) +
    coord_flip()

cvicu <- orders_levels %>%
    filter(location == "HH CVICU",
           month != "2016-03-01")

weekly_requests <- orders_requests %>%
    filter(order.unit == "HH CVICU",
           is.na(cancel.datetime),
           !is.na(action.provider.role)) %>%
    mutate(week = floor_date(request.datetime, unit = "week")) %>%
    group_by(week) %>%
    count() %>%
    rename(requests = n) %>%
    filter(week != "2016-03-27", week != end_date)

weekly_levels <- cvicu %>%
    filter(week != "2016-03-27",
           week != end_date,
           !is.na(Dispatched)) %>%
    group_by(week) %>%
    summarize(levels = n(),
              early_am = sum(early_am),
              appropriate = sum(appropriate),
              uncollected = sum(not_collected)) %>%
    full_join(weekly_requests, by = "week") %>%
    dmap_at(c("requests", "uncollected"), ~ coalesce(.x, 0L))

fig_pilot <- weekly_levels %>%
    mutate_at(c("early_am", "appropriate", "uncollected", "requests"), funs(. / levels)) %>%
    mutate(pilot = ymd(week, tz = "US/Central") >= mdy("1/29/2017", tz = "US/Central")) %>%
    group_by(pilot) %>%
    summarize_if(is.numeric, mean) %>%
    gather(key, value, early_am:requests) %>%
    select(-levels) %>%
    spread(pilot, value) %>%
    rename(baseline = `FALSE`, pilot = `TRUE`) %>%
    mutate(change = round((pilot / baseline * 100) - 100, 0)) %>%
    dmap_at("key", factor, levels = c("early_am", "requests", "appropriate", "uncollected")) %>%
    ggplot(aes(x = key, y = change)) +
    geom_bar(stat = "identity", width = 0.75) +
    geom_hline(yintercept = 0, color = "light gray") +
    scale_x_discrete("", labels = c("Early AM", "Requests", "Timely", "Uncollected")) +
    scale_y_continuous("Change (%)", limits = c(-100, 100), breaks = seq(-100, 100, 25)) +
    theme_bg(xticks = FALSE)

cvicu_dispatch <- data_orders %>%
    filter(event.unit == "HH CVICU",
           !is.na(Dispatched)) %>%
    mutate(dispatch_min = round_date(Dispatched, unit = "15 mins"),
           dispatch_min = minute(dispatch_min),
           dispatch_hour = round_date(Dispatched, unit = "hour"),
           dispatch_hour = hour(dispatch_hour))

fig_dispatch_histogram <- ggplot(cvicu_dispatch, aes(x = dispatch_detail_diff * 60, fill = future_order)) +
    geom_vline(xintercept = 0, color = "light grey") +
    geom_hline(yintercept = 0, color = "light grey") +
    geom_histogram(binwidth = 15, color = "white") +
    facet_wrap(~ priority, scales = "free_y") +
    scale_x_continuous("Task Dispatch to Scheduled Collection (minutes)", breaks = seq(-12 * 60, 12 * 60, 2 * 60)) +
    ylab("Number Dispatched") +
    scale_fill_brewer("", palette = "Set1") +
    coord_cartesian(xlim = c(-4 * 60, 4 * 60)) +
    theme_bg()

fig_dispatch_batch <- cvicu_dispatch %>%
    filter(priority %in% c("Routine", "Timed Study")) %>%
    count(priority, dispatch_hour) %>%
    ggplot(aes(x = dispatch_hour, y = n)) +
    geom_bar(stat = "identity") +
    scale_x_continuous("Dispatch Hour of Day", breaks = seq(0, 24, 6)) +
    ylab("Number of Orders") +
    scale_fill_brewer("Order Priority") +
    theme_bg()

# PowerPoint -------------------------------------------

doc <- pptx() %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = fig_order_type,
            offx = 1,
            offy = 1,
            width = 3,
            height = 2.25,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = fig_provider_type,
            offx = 1,
            offy = 1,
            width = 3,
            height = 2.25,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = fig_pilot,
            offx = 1,
            offy = 1,
            width = 4,
            height = 3,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = fig_dispatch_histogram,
            offx = 1,
            offy = 1,
            width = 6,
            height = 4.5,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = fig_dispatch_batch,
            offx = 1,
            offy = 1,
            width = 6,
            height = 4.5,
            vector.graphic = TRUE,
            fontname_sans = "Calibri")

writeDoc(doc, file = "doc/graduation_figures.pptx")

