# make list of vancomycin orders

library(tidyverse)
library(edwr)

# hvi <- c("HH CVICU", "HH CVIMU", "HH HFIC", "HH HFIM", "HH 5HVI", "HH CCU", "HVI CIMU")

timing <- read_data("data/raw", "timing") %>%
    as.order_timing()
    # filter(order.unit %in% hvi)

id <- concat_encounters(unique(timing$order.id), 950)

# run EDW queries:
#   * Orders - Actions - Source Order ID Prompt
#   * Orders - Details - Source Order ID Prompt

saveRDS(timing, "data/tidy/order_timing.Rds")
