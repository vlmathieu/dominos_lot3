# Libraries
suppressMessages(library(tidyverse))

# Snakemake log
log_file <- file(snakemake@log[[1]], open = "wt")
sink(log_file, append = TRUE, type = "message")
sink(log_file, append = TRUE)

# Load data
dat <- as_tibble(read.csv(file = snakemake@input[[1]],
                          header = TRUE,
                          sep = ";"))

# Extract list of code <> question
code_quest <- colnames(dat)

# Extract codes
code <- code_quest %>%
  lapply(function(x) unlist(strsplit(x, "..", fixed = TRUE))[1]) %>%
  unlist()

# Extract questions and correct spelling
quest <- code_quest %>%
  lapply(function(x) unlist(strsplit(x, "..", fixed = TRUE))[-1]) %>%
  lapply(function(x) paste0(x, collapse = ".")) %>%
  lapply(function(x) gsub("\\.\\.$|\\.$|^\\.", "", x)) %>%
  lapply(function(x) gsub("..", " - ", x, fixed = TRUE)) %>%
  lapply(function(x) gsub(".", " ", x, fixed = TRUE)) %>%
  lapply(function(x) gsub(" s a", " s'a", x, fixed = TRUE)) %>%
  lapply(function(x) gsub(" d ", " d'", x, fixed = TRUE)) %>%
  lapply(function(x) gsub("j a", "j'a", x, fixed = TRUE)) %>%
  lapply(function(x) gsub("J a", "J'a", x, fixed = TRUE)) %>%
  lapply(function(x) gsub("qu un", "qu'un", x, fixed = TRUE)) %>%
  lapply(function(x) gsub("est il", "est-il", x, fixed = TRUE)) %>%
  lapply(function(x) gsub("é e ", "é(e)", x, fixed = TRUE)) %>%
  lapply(function(x) gsub(" s ", "(s) ", x, fixed = TRUE)) %>%
  lapply(function(x) gsub("ez v", "ez-v", x, fixed = TRUE)) %>%
  lapply(function(x) gsub("hez-v", "hez v", x, fixed = TRUE)) %>%
  unlist()

# Create corresponding table between codes and questions
corr_code_quest <- as_tibble(data.frame(code = code, question = quest))
print(corr_code_quest)

# Save corresponding table
corr_code_quest %>%
  write.csv2(., file = snakemake@output[[1]], row.names = FALSE)
