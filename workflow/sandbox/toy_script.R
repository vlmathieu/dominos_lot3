library("dplyr")
library("tidyverse")
library("ggplot2")
library("hrbrthemes")
library("shadowtext")
library("forcats")
library("stringr")

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

table(colnames(likert))


###### Knowledge
quest <- data %>%
  select(contains("Con")) %>%
  select(!contains("consentement"))

col_quest <- colnames(quest)

full_quest <- c(
  "Environ quelle proportion du territoire français métropolitain est\ncouverte par des forêts ?", # nolint
  "Depuis 150 ans, la surface forestière : ",
  "Gérer les forêts permet de limiter certains risques associés aux\nincendies, aux insectes, aux tempêtes...", # nolint
  "En France, la forêt appartient principalement :",
  "Les arbres français sont majoritairement des :",
  "En France, on récolte :",
  "Comment évaluez-vous vos connaissances sur la forêt ?"
)

know_quest <- data.frame(code = col_quest, quest = full_quest)

for (i in seq_len(nrow(know_quest))){

  plot_data <- quest %>%
    select(know_quest$code[i]) %>%
    filter((!!as.name(know_quest$code[i])) != "") %>%
    gather(!!as.name(know_quest$code[i])) %>%
    count(!!as.name(know_quest$code[i])) %>%
    rename_with(~c("count"), c(n)) %>%
    mutate(wrapc = str_wrap(!!as.name(know_quest$code[i]), width = 20))

  plot <- plot_data %>%
    ggplot(aes(x = count,
               y = fct_reorder(wrapc, count))) +
    geom_col(width = 0.4) +
    geom_text(aes(label = paste(round(count / sum(count) * 100, 2),
                                "%")),
              hjust = -0.1,
              fontface = "bold") +
    scale_x_continuous(
      limits = c(0, 7000)
    ) +
    labs(
      title = "Connaissances générales sur la forêt",
      subtitle = know_quest$quest[i],
      y = "Réponse",
      x = "Nombre de réponses",
      caption = sprintf("(n = %s)", sum(plot_data$count))
    ) +
    theme_ipsum_rc(grid = "X") # +

  ggplot2::ggsave(
    filename = paste0(know_quest$code[i], ".png"),
    path = "/Users/valentinmathieu/Desktop",
    plot = plot,
    bg = "white"
  )

}

##########

path <- "/Users/valentinmathieu/Desktop/wd/dominos_lot3/results/output/code_quest.csv" # nolint
dat <- read.csv(file = path, header = TRUE, sep = ";")
dat <- as_tibble(dat)
