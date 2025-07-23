suppressMessages(library(likert))
suppressMessages(library(tidyverse))
suppressMessages(library(psych))
suppressMessages(library(magick))
suppressMessages(library(kableExtra))
suppressMessages(library(knitr))

# Snakemake log
log_file <- file(snakemake@log[[1]], open = "wt")
sink(log_file, append = TRUE, type = "message")
sink(log_file, append = TRUE)

# Load data
dat <- as_tibble(read.csv(file = snakemake@input[[1]],
                          header = TRUE,
                          sep = ";"))
corr_code_quest <- as_tibble(read.csv(file = snakemake@input[[2]],
                                      header = TRUE,
                                      sep = ";"))
# dat <- as_tibble(read.csv(file = "/Users/valentinmathieu/Desktop/wd/dominos_lot3/resources/inhouse/results_survey857139_code.csv", # nolint
#                           header = TRUE,
#                           sep = ";"))
# corr_code_quest <- as_tibble(read.csv(file = "/Users/valentinmathieu/Desktop/wd/dominos_lot3/results/output/code_quest.csv", # nolint
#                                       header = TRUE,
#                                       sep = ";"))

# Encode data for cronbach computation
lik <- dat |>
  # Filter data to keep likert columns on attitudes
  select(contains("ATT")) |>
  filter_all(all_vars(. != "")) |>
  dplyr::rename_all(~ gsub(".{1}$", "", .)) |> # remove "." at end of column to match codes and question # nolint
  # Encode likert scale data
  mutate_all(~replace(., . == "Tout à fait d'accord", 5)) |> # nolint
  mutate_all(~replace(., . == "Plutôt d'accord", 4)) |> # nolint
  mutate_all(~replace(., . == "Ni d'accord ni pas d'accord", 3)) |> # nolint
  mutate_all(~replace(., . == "Plutôt pas d'accord", 2)) |> # nolint
  mutate_all(~replace(., . == "Pas du tout d'accord", 1)) |> # nolint
  mutate_if(is.character, as.numeric)

# Correspondance between attitude codes and descriptions
corr_att <- tibble(code = snakemake@params$att) |>
  mutate(att = case_when(grepl("ENV", code) ~ "Attitudes towards the environment",  # nolint
                         grepl("FO", code) ~ "Attitudes towards forests",
                         grepl("BE", code) ~ "Attitudes towards wood for energy",  # nolint
                         grepl("BC", code) ~ "Attitudes towards wood for construction")) # nolint
message("List of assessed attitudes:\n",
        paste(corr_att$code, corr_att$att, sep = "\t| ", collapse = "\n"))

# Cronbach alpha
# Attitudes toward the environment
att_env_codes <- c(
  "ATTENV.P1", "ATTENV.P2", "ATTENV.P3", "ATTENV.P6", "ATTENV.P8", "ATTENV.P11", "ATTENV.P12", # nolint
  "ATTENV.P1R", "ATTENV.P2R", "ATTENV.P3R", "ATTENV.P6R", "ATTENV.P8R", "ATTENV.P11R", "ATTENV.P12R", # nolint
  "ATTENV.U4", "ATTENV.U5", "ATTENV.U7", "ATTENV.U9", "ATTENV.U10",
  "ATTENV.U4R", "ATTENV.U5R", "ATTENV.U7R", "ATTENV.U9R", "ATTENV.U10R"
)
att_env_keys <- c(rep(1, 7), rep(-1, 7), rep(-1, 5), rep(1, 5))

cronbach_att_env <- lik |>
  select(all_of(att_env_codes)) |>
  as.data.frame() |>
  psych::alpha(keys = att_env_keys)

message("\n\n--- CRONBACH ALPHA | ATTITUDES TOWARD THE ENVIRONMENT ---\n")
print(cronbach_att_env)

# Attitudes toward the forests
att_fo_codes <- c(
  "ATTFO.P1", "ATTFO.P2", "ATTFO.P3", "ATTFO.P6", "ATTFO.P8",
  "ATTFO.P1R", "ATTFO.P2R", "ATTFO.P3R", "ATTFO.P6R", "ATTFO.P8R",
  "ATTFO.U4", "ATTFO.U5", "ATTFO.U7", "ATTFO.U9", "ATTFO.U10",
  "ATTFO.U4R", "ATTFO.U5R", "ATTFO.U7R", "ATTFO.U9R", "ATTFO.U10R"
)
att_fo_keys <- c(rep(1, 5), rep(-1, 5), rep(-1, 5), rep(1, 5))

cronbach_att_fo <- lik |>
  select(all_of(att_fo_codes)) |>
  as.data.frame() |>
  psych::alpha(keys = att_fo_keys)

message("\n\n---CRONBACH ALPHA | ATTITUDES TOWARD FORESTS ---\n")
print(cronbach_att_fo)

# Attitudes toward wood for energy
# N.B.: Nature et Tech sont mal encodées/posées
att_be_codes <- c(
  "ATTBE.Ecomen", "ATTBE.Ecoloc", "ATTBE.BienEtre", "ATTBE.NatureR", "ATTBE.Durable", "ATTBE.Health", "ATTBE.Tech", # nolint
  "ATTBE.EcomenR", "ATTBE.EcolocR", "ATTBE.BienEtreR", "ATTBE.Nature", "ATTBE.DurableR", "ATTBE.HealthR", "ATTBE.TechR" # nolint
)
att_be_keys <- c(rep(1, 6), rep(-1, 8))

cronbach_att_be <- lik |>
  select(all_of(att_be_codes)) |>
  as.data.frame() |>
  psych::alpha(keys = att_be_keys)

message("\n\n---CRONBACH ALPHA | ATTITUDES TOWARD WOOD FOR ENERGY ---\n")
print(cronbach_att_be)

# Attitudes toward wood for construction
att_bc_codes <- c(
  "ATTBC.Ecomen", "ATTBC.Ecoloc", "ATTBC.BienEtre", "ATTBC.Nat", "ATTBC.Durable", "ATTBC.Onehealth", "ATTBC.Tech", # nolint
  "ATTBC.EcomenR", "ATTBC.EcolocR", "ATTBC.BienEtreR", "ATTBC.NatR", "ATTBC.DurableR", "ATTBC.OnehealthR", "ATTBC.TechR" # nolint
)
att_bc_keys <- c(rep(1, 7), rep(-1, 7))

cronbach_att_bc <- lik |>
  select(all_of(att_bc_codes)) |>
  as.data.frame() |>
  psych::alpha(keys = att_bc_keys)

message("\n\n--- CRONBACH ALPHA | ATTITUDES TOWARD WOOD FOR CONSTRUCTION ---\n")
print(cronbach_att_bc)

# Plot results
# Results for all attitudes
res <- bind_rows(c(att = "ATTENV", cronbach_att_env$total),
                 c(att = "ATTFO", cronbach_att_fo$total),
                 c(att = "ATTBE", cronbach_att_be$total),
                 c(att = "ATTBC", cronbach_att_bc$total))

# As .tex file
res |>
  knitr::kable(format = "latex",
               booktabs = TRUE,
               caption = "Cronbach alpha statistics for each assessed attitudes") |> # nolint
  save_kable(file = snakemake@output[[1]])
