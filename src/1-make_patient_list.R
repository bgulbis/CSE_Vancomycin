# make patient list

# run EDW query: Patients - by Order Date and Unit - Order Mnemonic Prompt

library(tidyverse)
library(edwr)

pts <- read_data("data/raw", "patients") %>%
    as.patients() %>%
    arrange(pie.id)

id <- concat_encounters(pts$pie.id)

# use results to run EDW queries:
#   * Orders - Timing - Prompt without Review
#       - Order Catalog Mnemonic: Vancomycin Level; Vancomycin Level Peak; Vancomycin Level Peak Request; Vancomycin Level Request; Vancomycin Level Trough; Vancomycin Level Trough Request
#   * Clinical Events - Prompt
#       - Clinical Event: 	Vanco Lvl; Vanco Pk; Vanco Tr
