# make figures for graduation presentation

library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
# library(broom)
# library(qicharts)
library(themebg)
library(ReporteRs)
library(SixSigma)

x <- dirr::get_rds("data/tidy")

end_date <- "2017-05-07"
pilot_start <- "2017-01-30"
pilot_end <- "2017-03-05"
pilot2_start <- "2017-04-16"
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
           group = if_else(detail.datetime >= ymd(pilot_start, tz = "US/Central") & detail.datetime < ymd(pilot2_start, tz = "US/Central"), "pilot1",
                           if_else(detail.datetime >= ymd(pilot2_start, tz = "US/Central"), "pilot2", "baseline")),
           future_order = order_detail_diff > 1,
           dispatch_late = Dispatched > detail.datetime + minutes(5)) %>%
    dmap_at(c("timely120", "timely60", "appropriate"), ~ coalesce(.x, FALSE)) %>%
    dmap_at("future_order", ~if_else(.x, "Future", "Now"))

data_orders <- orders_levels %>%
    filter(priority %in% c("Routine", "Stat", "Timed Study"),
           detail.datetime >= order.datetime - hours(1)) %>%
    mutate(priority = if_else(early_am, "Early AM", priority))

collected <- data_orders %>%
    filter(!is.na(Collected),
           order.datetime <= ymd(end_date)) %>%
    group_by(event.unit, group) %>%
    summarize(collected = n(),
              timely = sum(appropriate, na.rm = TRUE),
              early = sum(early, na.rm = TRUE),
              late = sum(late, na.rm = TRUE),
              late_2hr = sum(exceed_2hr, na.rm = TRUE))

not_done <- data_orders %>%
    filter(is.na(Collected),
           is.na(lab.result),
           order.datetime <= ymd(end_date)) %>%
    group_by(order.unit, group) %>%
    count %>%
    rename(not_done = n)

requests <- orders_requests %>%
    mutate(group = if_else(order.datetime >= ymd(pilot_start, tz = "US/Central") & order.datetime < ymd(pilot2_start, tz = "US/Central"), "pilot1",
                           if_else(order.datetime >= ymd(pilot2_start, tz = "US/Central"), "pilot2", "baseline"))) %>%
    filter(order.datetime <= ymd(end_date)) %>%
    group_by(order.unit, group) %>%
    count %>%
    rename(requests = n) %>%
    filter(order.unit %in% hvi)

early_am <- orders_early_am %>%
    mutate(group = if_else(order.datetime >= ymd(pilot_start, tz = "US/Central") & order.datetime < ymd(pilot2_start, tz = "US/Central"), "pilot1",
                           if_else(order.datetime >= ymd(pilot2_start, tz = "US/Central"), "pilot2", "baseline"))) %>%
    filter(order.datetime <= ymd(end_date)) %>%
    group_by(order.unit, group) %>%
    count() %>%
    rename(early_am = n) %>%
    filter(order.unit %in% hvi)

priority <- data_orders %>%
    filter(!is.na(Collected),
           order.datetime <= ymd(end_date),
           priority %in% c("Routine", "Stat", "Timed Study")) %>%
    count(event.unit, group, priority) %>%
    group_by(event.unit, group) %>%
    spread(priority, n)

# percent_timely = timely / collected,

totals_units <- left_join(collected, not_done, by = c("event.unit" = "order.unit", "group")) %>%
    left_join(requests, by = c("event.unit" = "order.unit", "group")) %>%
    left_join(early_am, by = c("event.unit" = "order.unit", "group")) %>%
    mutate(total = collected + not_done,
           defects = total - timely,
           percent_not_done = not_done / total * 100,
           percent_appropriate = timely / total * 100,
           percent_untimely = (1 - timely / total) * 100,
           percent_exceed_2hr = late_2hr / total * 100,
           percent_requests = requests / total * 100,
           percent_early_am = early_am / total * 100)

totals_hvi <- totals_units %>%
    group_by(group) %>%
    summarize_at(vars(collected, timely, early, late, late_2hr, not_done, requests, early_am, total, defects), funs(sum), na.rm = TRUE) %>%
    mutate(event.unit = "HVI",
           percent_not_done = not_done / total * 100,
           percent_appropriate = timely / total * 100,
           percent_untimely = (1 - timely / total) * 100,
           percent_exceed_2hr = late_2hr / total * 100,
           percent_requests = requests / total * 100,
           percent_early_am = early_am / total * 100)

totals <- bind_rows(totals_units, totals_hvi) %>%
    select(event.unit, group, num_levels = total, everything()) %>%
    filter(!is.na(event.unit))

cvicu_reqs <- orders_requests %>%
    mutate(group = if_else(order.datetime >= ymd(pilot_start, tz = "US/Central") & order.datetime < ymd(pilot2_start, tz = "US/Central"), "pilot1",
                           if_else(order.datetime >= ymd(pilot2_start, tz = "US/Central"), "pilot2", "baseline"))) %>%
    filter(order.unit == "HH CVICU",
           order.datetime <= ymd(end_date)) %>%
    mutate(priority = "Request") %>%
    select(pie.id, group, priority, action.provider.role)

cvicu_orders <- data_orders %>%
    filter(event.unit == "HH CVICU",
           detail.datetime >= order.datetime - hours(1)) %>%
    full_join(cvicu_reqs, by = c("pie.id", "group", "priority", "action.provider.role"))

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

cvicu_dispatch <- data_orders %>%
    filter(event.unit == "HH CVICU",
           !is.na(Dispatched)) %>%
    mutate(dispatch_min = round_date(Dispatched, unit = "15 mins"),
           dispatch_min = minute(dispatch_min),
           dispatch_hour = round_date(Dispatched, unit = "hour"),
           dispatch_hour = hour(dispatch_hour))

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
    dmap_at("action.provider.role", str_replace_all, pattern = ".*Pharm.*", replacement = "Pharmacist") %>%
    dmap_at("action.provider.role", str_replace_all, pattern = ".*MD.*|.*ED.*", replacement = "Physician") %>%
    dmap_at("action.provider.role", str_replace_all, pattern = ".*Practitioner.*", replacement = "NP/PA") %>%
    dmap_at("action.provider.role", str_replace_all, pattern = ".*RN.*", replacement = "Nurse") %>%
    dmap_at("action.provider.role", fct_infreq) %>%
    dmap_at("action.provider.role", fct_lump, n = 4) %>%
    dmap_at("action.provider.role", fct_rev) %>%
    ggplot(aes(x = action.provider.role)) +
    geom_bar() +
    xlab("Provider Type") +
    ylab("Number of Orders") +
    theme_bg(yticks = FALSE) +
    coord_flip()

fig_pilot <- weekly_levels %>%
    mutate_at(c("early_am", "appropriate", "uncollected", "requests"), funs(. / levels)) %>%
    mutate(pilot = ymd(week, tz = "US/Central") >= ymd(pilot_start, tz = "US/Central") & ymd(week, tz = "US/Central") <= ymd(pilot2_start, tz = "US/Central")) %>%
    group_by(pilot) %>%
    summarize_if(is.numeric, mean) %>%
    gather(key, value, early_am:requests) %>%
    select(-levels) %>%
    spread(pilot, value) %>%
    rename(baseline = `FALSE`, pilot = `TRUE`) %>%
    mutate(change = round((pilot / baseline * 100) - 100, 0)) %>%
    dmap_at("key", factor, levels = c("early_am", "requests", "appropriate", "uncollected")) %>%
    ggplot(aes(x = key, y = change)) +
    geom_bar(stat = "identity", width = 0.75, fill = "#377eb8") +
    geom_hline(yintercept = 0, color = "light gray") +
    scale_x_discrete("", labels = c("Early AM", "Requests", "Timely", "Uncollected")) +
    scale_y_continuous("Change Relative to Baseline (%)", limits = c(-80, 20), breaks = seq(-100, 100, 20)) +
    theme_bg(xticks = FALSE) +
    theme(axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 12))

fig_pilot2_vs_pilot1 <- weekly_levels %>%
    filter(ymd(week, tz = "US/Central") >= ymd(pilot_start, tz = "US/Central")) %>%
    mutate_at(c("early_am", "appropriate", "uncollected", "requests"), funs(. / levels)) %>%
    mutate(pilot = ymd(week, tz = "US/Central") >= ymd(pilot2_start, tz = "US/Central") & ymd(week, tz = "US/Central") <= ymd(end_date, tz = "US/Central")) %>%
    group_by(pilot) %>%
    summarize_if(is.numeric, mean) %>%
    gather(key, value, early_am:requests) %>%
    select(-levels) %>%
    spread(pilot, value) %>%
    rename(baseline = `FALSE`, pilot = `TRUE`) %>%
    mutate(change = round((pilot / baseline * 100) - 100, 0)) %>%
    dmap_at("key", factor, levels = c("early_am", "requests", "appropriate", "uncollected")) %>%
    ggplot(aes(x = key, y = change)) +
    geom_bar(stat = "identity", width = 0.75, fill = "#377eb8") +
    geom_hline(yintercept = 0, color = "light gray") +
    scale_x_discrete("", labels = c("Early AM", "Requests", "Timely", "Uncollected")) +
    scale_y_continuous("Change Relative to Pilot 1 (%)", limits = c(-80, 20), breaks = seq(-100, 100, 20)) +
    theme_bg(xticks = FALSE) +
    theme(axis.text.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 12))

fig_dispatch_histogram <- ggplot(cvicu_dispatch, aes(x = dispatch_detail_diff * 60, fill = dispatch_late)) +
    geom_vline(xintercept = 0, color = "black") +
    # geom_hline(yintercept = 0, color = "light grey") +
    geom_histogram(binwidth = 15, color = "white") +
    scale_x_continuous("Time from Task Dispatch to Scheduled Collection (minutes)", breaks = seq(-12 * 60, 12 * 60, 60)) +
    ylab("Number Dispatched") +
    scale_fill_brewer("", palette = "Set1", direction = -1, labels = c("Before Level Due", "After Level Due")) +
    coord_cartesian(xlim = c(-2 * 60, 4 * 60)) +
    theme_bg() +
    theme(axis.title = element_text(size = 14),
          axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.position = "bottom")

fig_dispatch_histogram_by_priority <- ggplot(cvicu_dispatch, aes(x = dispatch_detail_diff * 60, fill = dispatch_late)) +
    geom_vline(xintercept = 0, color = "black") +
    # geom_hline(yintercept = 0, color = "light grey") +
    geom_histogram(binwidth = 15, color = "white") +
    facet_wrap(~ priority, scales = "free_y") +
    scale_x_continuous("Time from Task Dispatch to Scheduled Collection (minutes)", breaks = seq(-12 * 60, 12 * 60, 2 * 60)) +
    ylab("Number Dispatched") +
    scale_fill_brewer("", palette = "Set1", direction = -1, labels = c("Before Level Due", "After Level Due")) +
    coord_cartesian(xlim = c(-2 * 60, 4 * 60)) +
    theme_bg() +
    theme(axis.title = element_text(size = 14),
          axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.position = "bottom")

fig_dispatch_batch <- cvicu_dispatch %>%
    filter(priority %in% c("Routine", "Timed Study")) %>%
    count(priority, dispatch_hour) %>%
    ggplot(aes(x = dispatch_hour, y = n)) +
    geom_bar(stat = "identity", fill = "#377eb8") +
    scale_x_continuous("Hour of Day", breaks = seq(0, 24, 6)) +
    ylab("Number of Task Dispatches") +
    scale_fill_brewer("Order Priority") +
    theme_bg() +
    theme(axis.title = element_text(size = 14),
          axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 14),
          legend.text = element_text(size = 14))

# Cause and Effect -------------------------------------

ss_effect <- "Untimely Levels"
ss_causes_gr <- c("Nursing", "Provider", "System")
ss_causes <- c(list(c("Handoff", "Work flow", "Other patient")),
               list(c("Order method", "Requests", "Early AM")),
               list(c("Task view", "Task dispatch", "Downtime")))
ss_sub <- ""

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
            height = 4,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = fig_pilot2_vs_pilot1,
            offx = 1,
            offy = 1,
            width = 4,
            height = 4,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = fig_dispatch_histogram,
            offx = 1,
            offy = 1,
            width = 7,
            height = 4.5,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = fig_dispatch_histogram_by_priority,
            offx = 1,
            offy = 1,
            width = 7,
            height = 4.5,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = print,
            x = fig_dispatch_batch,
            offx = 1,
            offy = 1,
            width = 7,
            height = 4.5,
            vector.graphic = TRUE,
            fontname_sans = "Calibri") %>%
    addSlide(slide.layout = "Blank") %>%
    addPlot(fun = function() ss.ceDiag(ss_effect, ss_causes_gr, ss_causes, main = "", sub = ""),
            offx = 1,
            offy = 1,
            width = 7,
            height = 4.5,
            pointsize = 24,
            vector.graphic = TRUE,
            fontname_sans = "Calibri")

writeDoc(doc, file = "doc/graduation_figures.pptx")

