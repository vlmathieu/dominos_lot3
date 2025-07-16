rm(list=ls())
library("dplyr")
library("tidyverse")
library("tidyr") # For tidying the data
library("ggplot2") # For plotting sophisticated graphs
library("rio") # For plotting sophisticated graphs
library("ggstats") # For plotting sophisticated graphs
library("labelled")

path <- "~/recherche/DOMINOS/dominos_github/resources/inhouse/results_survey857139_code.csv" # nolint
data <- read.csv(file = path, header = TRUE, sep = ";", na.strings=c("","NA"))

## ------------- ##
#    clean data   #
## ------------- ##

data_complete <- data[!is.na(data$submitdate), ] #regarder un peu plus ou est ce qu'ils se sont arrétés
data_complete <- data_complete[data_complete$SocioGenre!="Autre", ] #regarder un peu plus ou est ce qu'ils se sont arrétés

### recodage variables socioeconomiques ###

### CSP

# print réponses:
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

# CSP clean:
data_complete$SocioCSPclean<-ifelse(is.na(data_complete$SocioCSPclean), data_complete$SocioCSP, data_complete$SocioCSPclean) 
data_complete$SocioCSP_code<-as.factor(data_complete$SocioCSPclean)
levels(data_complete$SocioCSP_code)

data_complete <- data_complete %>% #lavaan ne supporte pas les variable catégorielles non ordonnées comme variables exogene, il faut créer des dummies
  mutate(SocioCSP_code = factor(SocioCSP_code)) %>%
  mutate(
    CSP_Employe = ifelse(SocioCSP_code == "Employé", 1, 0),
    CSP_Etudiant = ifelse(SocioCSP_code == "Étudiant", 1, 0),
    CSP_Inactif = ifelse(SocioCSP_code == "Inactif", 1, 0),
    CSP_Ouvrier = ifelse(SocioCSP_code == "Ouvrier", 1, 0),
    CSP_ProfInter = ifelse(SocioCSP_code == "Profession intermédiaire", 1, 0),
    CSP_ProfLibre = ifelse(SocioCSP_code == "Profession libérale / Cadre", 1, 0),
    CSP_Retraite = ifelse(SocioCSP_code == "Retraité", 1, 0),
    CSP_Artisan = ifelse(SocioCSP_code == "Agriculteur / Artisan / Commerçant / Chefs d’entreprise", 1, 0),
    CSP_Autre = ifelse(SocioCSP_code == "Autre", 1, 0)
  )


### Autres (pertinentes pour notre analyse)
#age
data_complete$SocioAge <- factor(data_complete$SocioAge, 
                               levels = c("18 à 24 ans" ,"25 à 34 ans" ,"35 à 49 ans" , "50 à 64 ans",  "65 ans et plus" ),
                               ordered = TRUE)

data_complete <- data_complete %>%
  filter(SocioGenre != "Je préfère ne pas répondre")

#genre
data_complete$SocioGenre <-as.factor(data_complete$SocioGenre)

#education
data_complete$SocioEduc <- factor(data_complete$SocioEduc, 
                                  levels = c("Aucun diplôme, certificat d’études primaires","Brevet de collèges (BEPC)",
                                             "CAP, BEP ou équivalent" ,"Baccalauréat, brevet professionnel ou équivalent" ,
                                             "Bac +2 à Bac +5", "Supérieur à Bac +5" ),
                                  ordered = TRUE)
levels(data_complete$SocioEduc)

#type de commune
data_complete$SocioCom <- as.factor(data_complete$SocioCom)

data_complete <- data_complete %>% #lavaan ne supporte pas les variable catégorielles non ordonnées comme variables exogene, il faut créer des dummies
  mutate(SocioCom = factor(SocioCom)) %>%
  mutate(
    Ville_Rural = ifelse(SocioCom == "Une commune rurale", 1, 0),
    Ville_Moyenne = ifelse(SocioCom ==  "Une ville de 20 000 à 99 999 habitants" | SocioCom =="Une ville de 2000 à 19 999 habitants", 1, 0),
    Ville_Grande = ifelse(SocioCom == "Une ville de plus de 100 000 habitants", 1, 0),
  )


#nombre de pers. dans le ménage
data_complete$SocioMenage <- as.numeric(as.character(data_complete$SocioMenage ))

#revenu
data_complete$SocioRevenu<-ifelse(is.na(data_complete$SocioRevenu), "Non déclaré", data_complete$SocioRevenu)
data_complete$SocioRevenu <-factor(data_complete$SocioRevenu, 
       levels = c( "Non déclaré","Moins de 1500 €" ,"Entre 1500 et 2000 € inclus","Entre 2001 et 2500 € inclus",
                  "Entre 2501 et 3000 € inclus" ,"Entre 3001 et 3500 € inclus" ,
                  "Entre 3501 et 4000 € inclus", "Entre 4001 et 4500 € inclus","Entre 5001 et 5500 € inclus",
                  "Entre 5501 et 6000 € inclus","Supérieur à 6000 €"),
       ordered = TRUE)
any(is.na(data_complete$SocioRevenu))


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
likert <- data %>%
  select(contains("ATTENV")) %>%
  select(contains("P")) %>%
  mutate(across(
    everything(),
    gsub,
    pattern = "Tout à fait d'accord",
    replacement = "Strongly agree"
  ))


#reverse coding? -> il faut reverse coder les neg car les R vont dans le même sens que les non R
#ou alors, ce sont les neg qui faut reverse coder

### proximity variables ###
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
levels(data_complete$ProxChaufclean_code)



### garder une version clean de la base de donnée

library(lavaan)

# --- Example SEM model ---

model <- '
  ##############################
  # 1) MEASUREMENT MODEL
  ##############################

  # Exogenous latent: Proximity
  # First-order factors
  Proximity =~ ProxProp + ProxRes + ProxTravail + ProxInfo+ ProxChauf + ProxLog +ProxChauf2 + ProxPnr + ProxProm + ProxParc
  Threat =~ ATTMENACE.SantePos. + ATTMENACE.SantePosR. + ATTMENACE.CCNeg. + ATTMENACE.CCNegR. + ATTMENACE.GestionPos. + ATTMENACE.GestionPosR. +  ATTMENACE.GestioNegR. + ATTMENACE.GestionNeg. + ATTMENACE.InqNegR. + ATTMENACE.InqNeg. + ATTMENACE.InqPosR.+ATTMENACE.InqPos.
  Knowledge =~ ConSurface + ConSurface2 + ConGestion  + ConEssence +ConRecolte +ConEval
  Env_Preserv =~ ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R. + ATTENV.P6R. + ATTENV.P8R. +ATTENV.P8. +ATTENV.P12.+ATTENV.P12R.
  Env_Util =~ ATTENV.U4. +ATTENV.U4R. + ATTENV.U5R. + ATTENV.U5. + ATTENV.U7R. + ATTENV.U7. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10.+ ATTENV.U10R.
  Fo_Preserv =~ ATTFO.P1.+ATTFO.P1R.+ATTFO.P2.+ATTFO.P2R.+ATTFO.P3.+ATTFO.P3R.+ATTFO.P6.+ATTFO.P6R.+ATTFO.P8.+ATTFO.P8R.
  Fo_Util =~ ATTFO.U4. + ATTFO.U4R. + ATTFO.U5R. + ATTFO.U5. + ATTFO.U7R. + ATTFO.U7. + ATTFO.U9. + ATTFO.U9R. + ATTFO.U10R. + ATTFO.U10.
  EW_Econ =~ ATTBE.EcomenR. + ATTBE.Ecomen. + ATTBE.EcolocR. + ATTBE.Ecoloc.
  EW_Ecol =~ ATTBE.DurableR. + ATTBE.Durable. +  ATTBE.HealthR. + ATTBE.Health.
  EW_Conf =~ ATTBE.NatureR. + ATTBE.Nature. + ATTBE.TechR. + ATTBE.Tech. + ATTBE.BienEtreR. + ATTBE.BienEtre.
  CW_Econ =~ ATTBC.EcomenR. + ATTBC.Ecomen. + ATTBC.EcolocR. + ATTBC.Ecoloc.
  CW_Ecol =~ ATTBC.DurableR. + ATTBC.Durable. +  ATTBC.OnehealthR. + ATTBC.Onehealth.
  CW_Conf =~ ATTBC.NatR. + ATTBC.Nat. + ATTBC.TechR. + ATTBC.Tech. + ATTBC.BienEtreR. + ATTBC.BienEtre.

  
  # Second-order factor
  AttEnv =~ Env_Preserv + Env_Util
  AttFo =~ Fo_Preserv + Fo_Util 
  AttEW =~ EW_Ecol + EW_Econ + EW_Conf
  AttCW =~ CW_Ecol + CW_Econ + CW_Conf
  
  ##############################
  # 2) STRUCTURAL MODEL
  ##############################
  
  # Socioeconomic observed predictors
  Threat ~ SocioAge + SocioGenre + Ville_Moyenne + Ville_Grande + Ville_Rural +CSP_Employe + CSP_Etudiant+CSP_Ouvrier+CSP_Inactif+CSP_ProfInter+CSP_ProfLibre +CSP_Retraite+CSP_Artisan+CSP_Autre + SocioRegion  + SocioEduc + SocioMenage + SocioRevenu + Proximity
  Knowledge ~ SocioAge + SocioGenre + Ville_Moyenne + Ville_Grande + Ville_Rural +CSP_Employe + CSP_Etudiant+CSP_Ouvrier+CSP_Inactif+CSP_ProfInter+CSP_ProfLibre +CSP_Retraite+CSP_Artisan+CSP_Autre + SocioRegion  + SocioEduc + SocioMenage + SocioRevenu + Proximity
  AttEnv ~ Threat + Knowledge +  SocioAge + SocioGenre + Ville_Moyenne + Ville_Grande + Ville_Rural + CSP_Employe + CSP_Etudiant+CSP_Ouvrier+CSP_Inactif+CSP_ProfInter+CSP_ProfLibre +CSP_Retraite+CSP_Artisan+CSP_Autre + SocioRegion  + SocioEduc + SocioMenage + SocioRevenu + Proximity
  AttFo ~ Threat + Knowledge +  SocioAge + SocioGenre + Ville_Moyenne + Ville_Grande + Ville_Rural +CSP_Employe + CSP_Etudiant+CSP_Ouvrier+CSP_Inactif+CSP_ProfInter+CSP_ProfLibre +CSP_Retraite+CSP_Artisan+CSP_Autre + SocioRegion  + SocioEduc + SocioMenage + SocioRevenu + Proximity
  AttCW ~ Threat + Knowledge +  SocioAge + SocioGenre + Ville_Moyenne + Ville_Grande + Ville_Rural +CSP_Employe + CSP_Etudiant+CSP_Ouvrier+CSP_Inactif+CSP_ProfInter+CSP_ProfLibre +CSP_Retraite+CSP_Artisan+CSP_Autre + SocioRegion  + SocioEduc + SocioMenage + SocioRevenu + Proximity
  AttEW ~ Threat + Knowledge +  SocioAge + SocioGenre + Ville_Moyenne + Ville_Grande + Ville_Rural +CSP_Employe + CSP_Etudiant+CSP_Ouvrier+CSP_Inactif+CSP_ProfInter+CSP_ProfLibre +CSP_Retraite+CSP_Artisan+CSP_Autre + SocioRegion  + SocioEduc + SocioMenage + SocioRevenu + Proximity

  ##############################
  # (Optional) Covariances
  ##############################

'
# --- Fit the model ---
fit <- sem(model, data = data_complete, missing = "fiml")
# --- Get summary with fit indices ---
summary(fit, fit.measures = TRUE, standardized = TRUE)

# --- Optional: modification indices to inspect improvements ---
modindices(fit, sort = TRUE, minimum.value = 5)

library(semPlot)


# Pour tracer le path diagram :
semPaths(fit, what = "std", layout = "tree", edge.label.cex = 0.8)

