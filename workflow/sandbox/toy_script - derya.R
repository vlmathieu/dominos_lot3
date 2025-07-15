rm(list=ls())
library("dplyr")
library("tidyverse")
library("tidyr") # For tidying the data
library("ggplot2") # For plotting sophisticated graphs
library("rio") # For plotting sophisticated graphs
library("ggstats") # For plotting sophisticated graphs
library("labelled")

path <- "~/recherche/DOMINOS/dominos_lot3/resources/inhouse/results-survey857139.csv" # nolint
data <- read.csv(file = path, header = TRUE, sep = ";", na.strings=c("","NA"))

## ------------- ##
#    clean data   #
## ------------- ##
data_complete <- data[!is.na(data$submitdate), ] #regarder un peu plus ou est ce qu'ils se sont arrétés

## recodage CSP
ocsp<-unique(data_complete$SocioCSP.other.)
csp<-unique(data_complete$SocioCSP)
print(ocsp)
print(csp)

data_complete$SocioCSPclean<-NA

data_complete <- data_complete %>%
  mutate(
    SocioCSPclean = case_when(
      str_detect(tolower(SocioCSP.other.), "ch[ôo]m|sans emploi|recherche|invalid|handicap|incapacit[ée]|arr[eê]t de travail|maman|m[eè]re|femme|maladie|inapte|foyer|burn") ~ "Inactif",
      str_detect(tolower(SocioCSP.other.), "retrait") ~ "Retraité",
      str_detect(tolower(SocioCSP.other.), "[ée]tudiant|apprent|alternant|formation|civique") ~ "Étudiant",
      str_detect(tolower(SocioCSP.other.), "professeur|enseignant|cadre|lib[eé]ral|presse|sportif|finance|intermittent") ~ "Profession libérale / Cadre",
      str_detect(tolower(SocioCSP.other.), "ma[çc]on") ~ "Ouvrier",
      str_detect(tolower(SocioCSP.other.), "publi|fonctionnaire") ~ "Profession intermédiaire",
      str_detect(tolower(SocioCSP.other.), "salari[ée]|caiss|nounou|aidant|int[ée]rim") ~ "Employé",
      str_detect(tolower(SocioCSP.other.), "mon compte|pendant|auto|ind[ée]pendant|") ~ "Agriculteur / Artisan / Commerçant / Chefs d’entreprise",
      is.na(SocioCSP.other.) ~ NA_character_,
      TRUE ~ "Autre"
    )
  )

data_complete$SocioCSPclean<-ifelse(is.na(data_complete$SocioCSPclean), data_complete$SocioCSP, data_complete$SocioCSPclean)
data_complete$SocioCSP_code<-as.factor(data_complete$SocioCSPclean)

## variables socioeco importantes:

data_complete$SocioAge <- factor(data_complete$SocioAge, 
                               levels = c("18 à 24 ans" ,"25 à 34 ans" ,"35 à 49 ans" , "50 à 64 ans",  "65 ans et plus" ),
                               ordered = TRUE)

data_complete$SocioGenre <-as.factor(data_complete$SocioGenre)

data_complete$SocioEduc <- factor(data_complete$SocioEduc, 
                                 levels = c("Aucun diplôme, certificat d’études primaires","Brevet de collèges (BEPC)",
                                            "CAP, BEP ou équivalent" ,"Baccalauréat, brevet professionnel ou équivalent" ,
                                            "Bac +2 à Bac +5", "Supérieur à Bac +5" ),
                                 ordered = TRUE)

data_complete$SocioRevenu <- factor(data_complete$SocioEduc, 
                                  levels = c("Aucun diplôme, certificat d’études primaires","Brevet de collèges (BEPC)",
                                             "CAP, BEP ou équivalent" ,"Baccalauréat, brevet professionnel ou équivalent" ,
                                             "Bac +2 à Bac +5", "Supérieur à Bac +5" ),
                                  ordered = TRUE)

## knowledge variables: ConSurface
data_complete$ConEssence_num<-ifelse(data_complete$ConEssence=="Feuillus",1,0)
data_complete$ConSurface_num<-ifelse(data_complete$ConSurface=="Un tiers",1,0)
data_complete$ConSurface2_num<-ifelse(data_complete$ConSurface2=="A fortement progressé",1,0)
data_complete$ConGestion_num<-ifelse(data_complete$ConGestion=="Vrai",1,0)
data_complete$ConProp_num<-ifelse(data_complete$ConProp=="À des individus privés",1,0)
data_complete$ConRecolte_num<-ifelse(data_complete$ConRecolte=="Moins de bois que ce que la forêt produit",1,0)
data_complete$Con_num<-rowSums(data_complete[,c('ConEssence_num','ConSurface_num','ConSurface2_num',"ConGestion_num","ConProp_num","ConRecolte_num")], na.rm = TRUE)
table(data_complete$Con_num, data_complete$ConEval)

## threat variables: 

#codage facteur des échelles de likert
likert_levels <- c(
  "Pas du tout d'accord",
  "Plutôt pas d'accord",
  "Ni d'accord ni pas d'accord",
  "Plutôt d'accord",
  "Tout à fait d'accord"
)


data_complete <- data_complete %>%
  mutate(across(starts_with("ATTMENACE"), ~ factor(.x, levels = likert_levels, ordered = TRUE)))

data_complete <- data_complete %>%
  mutate(across(starts_with("ATTMENACE"), ~ as.numeric(.x), .names = "{.col}_num"))

#reverse coding? -> on les a tourné négativement, mais le codage reste le même, je pense que l'on s'est trmpé
#ou alors, ce sont les neg qui faut reverse coder

## proximity variables
unique(data_complete$ProxChauf)
unique(data_complete$ProxChauf.other.)


data_complete <- data_complete %>%
  mutate(
    ProxChaufclean = case_when(
      is.na(ProxChauf.other.) ~ NA_character_,
      str_detect(tolower(ProxChauf.other.), "pelle|granul[ée]|bois|pelet|pel[ée]|chauffage au pellet|biomasse|granuy") ~ "Bois",
      str_detect(tolower(ProxChauf.other.), "[ée]lec|clim|solaire") ~ "Électricité",
      str_detect(tolower(ProxChauf.other.), "gaz") ~ "Gaz",
      str_detect(tolower(ProxChauf.other.), "p[ée]trole") ~ "Fioul",
      str_detect(tolower(ProxChauf.other.), "géothermie") ~ "Géothermie",
      str_detect(tolower(ProxChauf.other.), "cpcu|collectif|colllectif|colectif|urbain|central|réseau|chaleur|'|un|télé|plaid|bougie|ne chauffe pas|aucun chauffage|je n’utilise pas de chauffage|eau") ~ "Je ne sais pas",
      str_detect(tolower(ProxChauf.other.), "ordure|ordures|d[ée]chet") ~ "Autre",
      TRUE ~ "Autre"
    )
  )

test<-data_complete[,c("ProxChauf","ProxChauf.other.","ProxChaufclean")]

data_complete$ProxChaufclean = ifelse(data_complete$ProxChauf.other. == "Chauffage collectif incinérateur d'ordures ", "Autre",data_complete$ProxChaufclean)
data_complete$ProxChaufclean = ifelse(data_complete$ProxChauf.other. == "Usine d'incinération des ordures menageres", "Autre",data_complete$ProxChaufclean)
data_complete$ProxChaufclean = ifelse(data_complete$ProxChauf.other. == "Roulage de déchets municipaux", "Autre",data_complete$ProxChaufclean)

data_complete$ProxChaufclean<-ifelse(is.na(data_complete$ProxChaufclean), data_complete$ProxChauf, data_complete$ProxChaufclean)
data_complete$ProxChaufclean_code<-as.factor(data_complete$ProxChaufclean)

## ------------------------- ##
#      measurement model      #
## ------------------------- ##
# a remplace rpar nos variables
# et mettre les va

model_cfa <- '
  # First-order factors
  Proximity =~ prox1 + prox2 + prox3
  Threat =~ threat1 + threat2 + threat3 
  Knowledge =~ know1 + know2 + know3
  Env_Preserv =~ env_pres1 + env_pres2 + env_pres3 
  Env_Util =~ env_util1 + env_util2 + env_util3
  
  # Second-order factor
  AttEnv =~ Env_Preserv + Env_Util
  AttForest =~ forest1 + forest2 + forest3
  AttWood =~ wood1 + wood2 + wood3
  
'

fit_cfa <- cfa(model_cfa, data = mydata)

summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)




library(lavaan)

# --- Example SEM model ---

model <- '
  ##############################
  # 1) MEASUREMENT MODEL
  ##############################

  # Exogenous latent: Proximity
  # First-order factors
  Proximity =~ prox1 + prox2 + prox3
  Threat =~ threat1 + threat2 + threat3 
  Knowledge =~ know1 + know2 + know3
  Env_Preserv =~ env_pres1 + env_pres2 + env_pres3 
  Env_Util =~ env_util1 + env_util2 + env_util3
  
  # Second-order factor
  AttEnv =~ Env_Preserv + Env_Util
  AttForest =~ forest1 + forest2 + forest3
  AttWood =~ wood1 + wood2 + wood3
  ##############################
  # 2) STRUCTURAL MODEL
  ##############################
  # Socioeconomic observed predictors
  Threat ~ Socio1 + Socio2 + Proximity
  Knowledge ~ Socio1 + Socio2 + Proximity
  AttEnv ~ Threat + Knowledge + Socio1 + Socio2 + Proximity
  AttForest ~ Threat + Knowledge + AttEnv + Socio1 + Socio2 + Proximity
  AttWood ~ Threat + Knowledge + AttForest + Socio1 + Socio2 + Proximity
  ##############################
  # (Optional) Covariances
  ##############################
  # If needed: allow exogenous variables to correlate
  Socio1 ~~ Socio2
  Proximity ~~ Socio1 + Socio2
'
# --- Fit the model ---
fit <- sem(model, data = mydata)
# --- Get summary with fit indices ---
summary(fit, fit.measures = TRUE, standardized = TRUE)

# --- Optional: modification indices to inspect improvements ---
modindices(fit, sort = TRUE, minimum.value = 5)
