# make list of vancomycin orders

library(tidyverse)
library(edwr)

# hvi <- c("HH CVICU", "HH CVIMU", "HH HFIC", "HH HFIM", "HH 5HVI", "HH CCU", "HVI CIMU")

timing <- read_data("data/raw", "timing") %>%
    as.order_timing()
    # filter(order.unit %in% hvi)

vanc_levels <- read_data("data/raw", "vanc_level") %>%
    as.labs() %>%
    rename(order.id = `Clinical Event Order ID`,
           event.unit = `Nurse Unit of Clinical Event`) %>%
    filter(!is.na(event.unit))

orders <- bind_rows(timing["order.id"], vanc_levels["order.id"]) %>%
    distinct()

id <- concat_encounters(orders$order.id, 950)

# run EDW queries:
#   * Orders - Actions - Source Order ID Prompt
#   * Orders - Details - Source Order ID Prompt

saveRDS(timing, "data/tidy/order_timing.Rds")
saveRDS(vanc_levels, "data/tidy/vanc_levels.Rds")
