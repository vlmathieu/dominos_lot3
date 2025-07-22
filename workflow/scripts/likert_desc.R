library(ggplot2)
library(hrbrthemes)
suppressMessages(library(likert))
suppressMessages(library(tidyverse))

# Snakemake log
log_file <- file(snakemake@log[[1]], open = "wt")
sink(log_file, append = TRUE, type = "message")
sink(log_file, append = TRUE)

# Functions
dat_filter_cleaning <- function(dat, corr_code_quest, att_code, att) {

  # Levels for ordering plot results
  levs <- c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree") # nolint

  lik_att <- dat %>%
    # Keep columns relative to the assessed attitude of code att_code
    dplyr::select(dplyr::starts_with(att_code)) %>%
    # Filter to keep complete responses
    dplyr::filter_all(dplyr::all_vars(. != "")) %>% # nolint
    # Translate French to English
    mutate(across(everything(), ~replace(., . == "Tout à fait d'accord", "Strongly agree"))) %>% # nolint
    mutate(across(everything(), ~replace(., . == "Plutôt d'accord", "Agree"))) %>% # nolint
    mutate(across(everything(), ~replace(., . == "Ni d'accord ni pas d'accord", "Neutral"))) %>% # nolint
    mutate(across(everything(), ~replace(., . == "Plutôt pas d'accord", "Disagree"))) %>% # nolint
    mutate(across(everything(), ~replace(., . == "Pas du tout d'accord", "Strongly disagree"))) %>% # nolint
    # Set levels
    mutate(across(everything(), ~factor(., levels = levs))) %>%
    dplyr::rename_all(~ gsub(".{1}$", "", .)) %>% # remove "." at end of column to match question # nolint
    as.data.frame()

  # Print df characteristics
  cat("Filtered data for attitude ", att, " :\n")
  message(str(lik_att))

  return(lik_att)
}

dat_likert_process <- function(lik_att, att) {

  # Extract correspondance code <> question for attitudes
  code_quest <- corr_code_quest %>%
    filter(code %in% colnames(lik_att)) %>% # nolint
    dplyr::mutate(question = gsub("^.*?- ", "", question)) # nolint

  # Print resulting correspondance table
  cat(paste0("Questions corresponding to attitude ", att, " :\n"))
  print(code_quest, n = 100)

  # Update colnames with full questions for plotting
  lik_att <- lik_att %>%
    dplyr::rename_with(~code_quest$question, code_quest$code)

  # Produce likert results
  likert_results <- likert(lik_att)

  return(likert_results)
}

plot_res <- function(dat, corr_code_quest, att_code, att) {

  # Process data for plotting
  lik_att <- dat_filter_cleaning(dat, corr_code_quest, att_code, att)
  likert_results <- dat_likert_process(lik_att, att)

  # Get number of observations for subtitle
  nb_obs <- nrow(lik_att)

  # Produce plot
  lik_plot <- likert.bar.plot(likert_results) +
    theme_ipsum() +
    labs(title = att,
         subtitle = paste0("n = ", nb_obs)) +
    theme(legend.position = "bottom")

  # Save plot to all file extensions
  for (ext in snakemake@params$ext) { # nolint

    ggsave(
      paste(
            paste(sub(pattern = "(.*)\\_.*$",
                      replacement = "\\1",
                      basename(snakemake@output[[1]])), # nolint
                  att_code,
                  sep = "_"),
            ext,
            sep = "."),
      plot = lik_plot,
      device = ext,
      path = paste(dirname(snakemake@output[[1]]),
                   sep = "/"),
      create.dir = TRUE,
      scale = 2,
      width = 1800,
      height = 2100,
      units = c("px"),
      dpi = 300,
      limitsize = TRUE,
      bg = "white"
    )
  }
}

# Load data
dat <- as_tibble(read.csv(file = snakemake@input[[1]],
                          header = TRUE,
                          sep = ";"))
corr_code_quest <- as_tibble(read.csv(file = snakemake@input[[2]],
                                      header = TRUE,
                                      sep = ";"))

# List of assessed attitudes
att_lst <- dat %>%
  select(contains(snakemake@params$att)) %>%
  colnames() %>%
  lapply(function(x) unlist(strsplit(x, ".", fixed = TRUE))[1]) %>%
  unlist() %>%
  unique()

# Correspondance between attitude codes and descriptions
corr_att <- tibble(code = att_lst) %>%
  mutate(att = case_when(grepl("ENV", code) ~ "Attitudes towards the environment",  # nolint
                         grepl("FO", code) ~ "Attitudes towards forests",
                         grepl("BE", code) ~ "Attitudes towards wood for energy",  # nolint
                         grepl("BC", code) ~ "Attitudes towards wood for construction")) # nolint
message("List of assessed attitudes:\n",
        paste(corr_att$code, corr_att$att, sep = "\t| ", collapse = "\n"))

# Gather parameters
params <- list(
  replicate(nrow(corr_att), dat, simplify = FALSE),
  replicate(nrow(corr_att), corr_code_quest, simplify = FALSE),
  corr_att$code,
  corr_att$att
)

# Likert descriptive bar plots for every attitude
purrr::pmap(params, plot_res)
