# make patient list

# run EDW query: Patients - by Order Date and Unit - Order Mnemonic Prompt

library(tidyverse)
library(edwr)

pts <- read_data("data/raw", "patients") %>%
    as.patients() %>%
    arrange(pie.id)

print(concat_encounters(pts$pie.id))

# use results to run EDW queries:
#   * Orders - Timing - Prompt
#   * Clinical Events - Prompt
#       - Clinical Event: 	Vanco Lvl; Vanco Pk; Vanco Tr; Vancomycin Level; Vancomycin Level Trough; Vancomycin Level Peak; Vancomycin Trough Level
