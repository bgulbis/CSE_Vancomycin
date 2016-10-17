# make patient list

library(tidyverse)
library(edwr)

pts <- read_data("data/raw", "patients") %>%
    as.patients() %>%
    arrange(pie.id)

concat_encounters(pts$pie.id)
