# make list of vancomycin orders

library(tidyverse)
library(edwr)

timing <- read_data("data/raw", "timing") %>%
    as.order_timing() %>%
    filter(order.unit == "HH CVICU")

print(concat_encounters(unique(timing$order.id)))

# run EDW queries:
#   * Orders - Actions - Source Order ID Prompt
#   * Orders - Details - Source Order ID Prompt

saveRDS(timing, "data/tidy/order_timing.Rds")
