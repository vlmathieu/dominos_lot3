library("dplyr")
library("tidyverse")

path <- "/Users/valentinmathieu/Desktop/wd/dominos_lot3/resources/inhouse/results-survey857139.csv" # nolint
data <- read.csv(file = path, header = TRUE, sep = ";")
data <- as_tibble(data)

colnames(data)
summary(data)
data[1, ]
data %>%
  select("ConSurface") %>%
  unique()

likert <- data %>%
  select(contains("ATTENV")) %>%
  select(contains("P")) %>%
  mutate(across(
    everything(),
    gsub,
    pattern = "Tout à fait d'accord",
    replacement = "Strongly agree"
  ))
