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

## ------------------------------------------------------------------------- ##
#                                clean data                                   #
## ------------------------------------------------------------------------- ##

data_complete <- data[!is.na(data$submitdate), ] #regarder un peu plus ou est ce qu'ils se sont arrétés
data_complete <- data_complete[data_complete$SocioGenre!="Autre", ] #concerne seulement 3 répondants

### tables attitudes

table<-as.data.frame(grep("^ATT",colnames(data_complete),value=T))

### recodage variables socioeconomiques ###

## CSP

print(unique(data_complete$SocioCSP))
print(unique(data_complete$SocioCSP.other.))

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
    CSP_Inter = ifelse(SocioCSP_code == "Employé"| SocioCSP_code == "Profession intermédiaire" |SocioCSP_code == "Ouvrier" | SocioCSP_code == "Autre", 1, 0),
    CSP_Etudiant = ifelse(SocioCSP_code == "Étudiant", 1, 0),
    CSP_Cadre = ifelse(SocioCSP_code == "Profession libérale / Cadre", 1, 0),
    CSP_Inactif = ifelse(SocioCSP_code == "Retraité" | SocioCSP_code == "Inactif" , 1, 0),
    CSP_Artisan = ifelse(SocioCSP_code == "Agriculteur / Artisan / Commerçant / Chefs d’entreprise", 1, 0),
  )
table(data_complete$CSP_Artisan, useNA = "always")

## Age

data_complete$SocioAge <- factor(data_complete$SocioAge, 
                               levels = c("18 à 24 ans" ,"25 à 34 ans" ,"35 à 49 ans" , "50 à 64 ans",  "65 ans et plus" ))
table(data_complete$SocioAge)

data_complete <- data_complete %>% 
  mutate(
    SocioAge_Jeune = ifelse(SocioAge == "18 à 24 ans"| SocioCSP_code == "25 à 34 ans", 1, 0),
    SocioAge_Inter = ifelse(SocioAge == "35 à 49 ans", 1, 0),
    SocioAge_Vieux = ifelse(SocioAge == "50 à 64 ans"| SocioCSP_code == "65 ans et plus", 1, 0) 
  )

data_complete <- data_complete %>%
  filter(SocioGenre != "Je préfère ne pas répondre")
table(data_complete$SocioAge_Vieux, useNA = "always")

## Genre
data_complete$SocioGenre <-as.factor(data_complete$SocioGenre)
table(data_complete$SocioGenre)

data_complete$SocioGenre_Femme = ifelse(data_complete$SocioGenre == "Femme", 1, 0)
table(data_complete$SocioGenre_Femme, useNA = "always")

## Education
data_complete$SocioEduc <- factor(data_complete$SocioEduc, 
                                  levels = c("Aucun diplôme, certificat d’études primaires","Brevet de collèges (BEPC)",
                                             "CAP, BEP ou équivalent" ,"Baccalauréat, brevet professionnel ou équivalent" ,
                                             "Bac +2 à Bac +5", "Supérieur à Bac +5" ))
table(data_complete$SocioEduc)

data_complete <- data_complete %>% 
  mutate(
    SocioEduc_inf = ifelse(SocioEduc == "Aucun diplôme, certificat d’études primaires"| SocioCSP_code == "Brevet de collèges (BEPC)", 1, 0),
    SocioEduc_moyen = ifelse(SocioEduc == "CAP, BEP ou équivalent"| SocioCSP_code == "Baccalauréat, brevet professionnel ou équivalent", 1, 0),
    SocioEduc_sup = ifelse(SocioEduc ==  "Bac +2 à Bac +5"| SocioCSP_code == "Supérieur à Bac +5", 1, 0),
  )

## Type de commune
data_complete$SocioCom <- as.factor(data_complete$SocioCom)

data_complete <- data_complete %>% 
  mutate(
    SocioCom_Rural = ifelse(SocioCom == "Une commune rurale", 1, 0),
    SocioCom_Moyenne = ifelse(SocioCom ==  "Une ville de 20 000 à 99 999 habitants" | SocioCom =="Une ville de 2000 à 19 999 habitants", 1, 0),
    SocioCom_Grande = ifelse(SocioCom == "Une ville de plus de 100 000 habitants", 1, 0),
  )

table(data_complete$SocioCom_Rural, useNA = "always")

## Nombre de personne dans le ménage 
data_complete$SocioMenage <- as.numeric(as.character(data_complete$SocioMenage ))

## Revenu #transformer en variable numérique
data_complete$SocioRevenu <-factor(data_complete$SocioRevenu, 
       levels = c("Moins de 1500 €" ,"Entre 1500 et 2000 € inclus","Entre 2001 et 2500 € inclus",
                  "Entre 2501 et 3000 € inclus" ,"Entre 3001 et 3500 € inclus" ,
                  "Entre 3501 et 4000 € inclus", "Entre 4001 et 4500 € inclus","Entre 4501 € et 5000 € inclus",
                  "Entre 5001 et 5500 € inclus","Entre 5501 et 6000 € inclus","Supérieur à 6000 €"))

data_complete$SocioRevenu_num <- recode(data_complete$SocioRevenu,
                                        "Moins de 1500 €" = 1250,
                                        "Entre 1500 et 2000 € inclus" = 1750,
                                        "Entre 2001 et 2500 € inclus" = 2250,
                                        "Entre 2501 et 3000 € inclus" = 2750,
                                        "Entre 3001 et 3500 € inclus" = 3250,
                                        "Entre 3501 et 4000 € inclus" = 3750,
                                        "Entre 4001 et 4500 € inclus" = 4250,
                                        "Entre 4501 € et 5000 € inclus" = 4750,
                                        "Entre 5001 et 5500 € inclus" = 5250,
                                        "Entre 5501 et 6000 € inclus" = 5750,
                                        "Supérieur à 6000 €" = 6500
)

table(data_complete$SocioRevenu_num, useNA = "always")
str(data_complete$SocioRevenu_num)
###  knowledge variables ###

data_complete$ConEssence_num<-ifelse(data_complete$ConEssence=="Feuillus",1,0)
data_complete$ConSurface_num<-ifelse(data_complete$ConSurface=="Un tiers",1,0)
data_complete$ConSurface2_num<-ifelse(data_complete$ConSurface2=="A fortement progressé",1,0)
data_complete$ConGestion_num<-ifelse(data_complete$ConGestion=="Vrai",1,0)
data_complete$ConProp_num<-ifelse(data_complete$ConProp=="À des individus privés",1,0)
data_complete$ConRecolte_num<-ifelse(data_complete$ConRecolte=="Moins de bois que ce que la forêt produit",1,0)
data_complete$Con_num<-rowSums(data_complete[,c('ConEssence_num','ConSurface_num','ConSurface2_num',"ConGestion_num","ConProp_num","ConRecolte_num")], na.rm = TRUE)

data_complete$ConEval_num <- recode(data_complete$ConEval,
                                        "Très faibles" = 1,
                                        "Faibles" = 2,
                                        "Moyennes" = 3,
                                        "Bonnes" = 4,
                                        "Très bonnes" = 5)

table(data_complete$ConEval_num, useNA = "always")


### perception of threat variables ###

## tout recoder likert de 1 à 5 (1: je ne ressens pas de menace, 5: je ressens une menace), recoder les "neg" après 

data_complete <- data_complete %>%
  mutate(across(starts_with("ATTMENACE"),
                ~ factor(recode(.,
                                "Pas du tout d'accord" = 1,
                                "Plutôt pas d'accord" = 2,
                                "Ni d'accord ni pas d'accord" = 3,
                                "Plutôt d'accord" = 4,
                                "Tout à fait d'accord" = 5),
                         ordered = TRUE,levels = 1:5)
  ))
# pour "SantePos": je pense que les forêts française sont en bonne santé.
# la reverse est en faite: "CCNegR" je ne pense pas que les forêts françaises soient en forme.
# si on va dans le sens, on ressent de + en + de menace de 1 à 5 SantePos est en fait la reverse de CCNegR.
# on renomme ce couple: SanteR pour SantePos et Sante pour CCNegR.


data_complete <- data_complete %>%
  mutate(ATTMENACE.SantePos. = 6 - as.numeric(ATTMENACE.SantePos.))


names(data_complete)[names(data_complete)=="ATTMENACE.SantePos."]<-"ATTMENACE.SanteR." #je pense que les forêts française sont en bonne santé.
names(data_complete)[names(data_complete)=="ATTMENACE.CCNegR."]<-"ATTMENACE.Sante." # je ne pense pas que les forêts françaises soient en forme.

# pour SantePosR: je ne pense pas que le CC ait de lourde csq sur les fo : CCR. Il faut reverse celle-ci.
# pour CCNeg: le changement climatique fait peser de graves risques... CC

data_complete <- data_complete %>%
  mutate(ATTMENACE.SantePosR. = 6 - as.numeric(ATTMENACE.SantePosR.))
names(data_complete)[names(data_complete)=="ATTMENACE.SantePosR."]<-"ATTMENACE.CCR." 
names(data_complete)[names(data_complete)=="ATTMENACE.CCNeg."]<-"ATTMENACE.CC." 

# pour GestionPos; je pense que les forêts sont gérées durablement (si on garde la même logique c'est une R) -> GestionR.
# correspond à GestionNeg : l'exploitation des forêts menace leur intégrité -> Gestion
data_complete <- data_complete %>%
  mutate(ATTMENACE.GestionPos. = 6 - as.numeric(ATTMENACE.GestionPos.))
names(data_complete)[names(data_complete)=="ATTMENACE.GestionPos."]<-"ATTMENACE.GestionR." 
names(data_complete)[names(data_complete)=="ATTMENACE.GestionNeg."]<-"ATTMENACE.Gestion." 

# pour GestionPosR ; la déforestation n'est pas un probleme (si on garde la même logique c'est une R) -> DefoR.
# correspond à GestionNegR : la déforestation n'épargne pas les forêt -> Defo
data_complete <- data_complete %>%
  mutate(ATTMENACE.GestionPosR. = 6 - as.numeric(ATTMENACE.GestionPosR.))
names(data_complete)[names(data_complete)=="ATTMENACE.GestionPosR."]<-"ATTMENACE.DefoR." 
names(data_complete)[names(data_complete)=="ATTMENACE.GestioNegR."]<-"ATTMENACE.Defo." 

# InqNegR: je ne pense pas que l'exploitation endommagera la planete (si on garde la meme logique, c'est un R) -> InqR
# correspond a InqPos: je pense que l'exploitation impactera notre BE (Inq)
data_complete <- data_complete %>%
  mutate(ATTMENACE.InqNegR. = 6 - as.numeric(ATTMENACE.InqNegR.))
names(data_complete)[names(data_complete)=="ATTMENACE.InqNegR."]<-"ATTMENACE.InqR." 
names(data_complete)[names(data_complete)=="ATTMENACE.InqPos."]<-"ATTMENACE.Inq." 

# InqNeg: je suis plutot optimiste: c'est une R -> NoptR
# correspond à InqPosR: je ne suis pas optimiste -> Nopt
data_complete <- data_complete %>%
  mutate(ATTMENACE.InqNeg. = 6 - as.numeric(ATTMENACE.InqNeg.))
names(data_complete)[names(data_complete)=="ATTMENACE.InqNeg."]<-"ATTMENACE.NoptR." 
names(data_complete)[names(data_complete)=="ATTMENACE.InqPosR."]<-"ATTMENACE.Nopt." 


### proximity variables ###

# moyen de chauffage principal
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
data_complete$ProxChaufclean<-as.factor(data_complete$ProxChaufclean)

table(data_complete$ProxChaufclean)

data_complete <- data_complete %>% #lavaan ne supporte pas les variable catégorielles non ordonnées comme variables exogene, il faut créer des dummies
  mutate(ProxChaufclean = factor(ProxChaufclean)) %>%
  mutate(
    ProxChauf_bois = ifelse(ProxChaufclean == "Bois", 1, 0),
    ProxChauf_nonfossil = ifelse(ProxChaufclean ==  "Électricité" | ProxChaufclean =="Géothermie" | ProxChaufclean =="Pompe à chaleur" , 1, 0),
    ProxChauf_fossil = ifelse(ProxChaufclean == "Fioul" | ProxChaufclean == "Gaz", 1, 0),
    ProxChauf_autre = ifelse(ProxChaufclean == "Autre" | ProxChaufclean == "Je ne sais pas", 1, 0),
  )

# moyen de chauffage secondaire

unique(data_complete$ProxChauf2)
unique(data_complete$ProxChauf2.other.)

data_complete <- data_complete %>%
  mutate(
    ProxChauf2clean = case_when(
      is.na(ProxChauf2.other.) ~ NA_character_,
      str_detect(tolower(ProxChauf2.other.), "pelle|granul[ée]|bois|pelet|pelket|pel[ée]|chauffage au pellet|biomasse|granuy|chemin[ée]e") ~ "Bois",
      str_detect(tolower(ProxChauf2.other.), "[ée]lec|clim|solaire|solaire") ~ "Électricité",
      str_detect(tolower(ProxChauf2.other.), "gaz") ~ "Gaz",
      str_detect(tolower(ProxChauf2.other.), "p[ée]trole|fioul") ~ "Fioul",
      str_detect(tolower(ProxChauf2.other.), "géothermie") ~ "Géothermie",
      str_detect(tolower(ProxChauf2.other.), "cpcu|collectif|colllectif|colectif|urbain|central|réseau|chaleur|'|un|télé|plaid|bougie|ne chauffe pas|aucun chauffage|je n’utilise pas de chauffage|eau|radiateur") ~ "Je ne sais pas",
      str_detect(tolower(ProxChauf2.other.), "ordure|ordures|d[ée]chet|thanol|usb|graminé|bouillote|tien|baie vitrée") ~ "Autre",
      str_detect(tolower(ProxChauf2.other.), "pompe") ~ "pompe a chaleur",
      TRUE ~ "Autre"
    )
  )

test<-data_complete[,c("ProxChauf2","ProxChauf2clean","ProxChauf2.other.")]

data_complete <- data_complete %>% 
  mutate(ProxChauf2clean = factor(ProxChauf2clean)) %>%
  mutate(
    ProxChauf2_bois = ifelse(ProxChauf2clean == "Bois", 1, 0)
  )

data_complete$ProxChauf_bois1<-ifelse((data_complete$ProxChauf2_bois==1 | data_complete$ProxChauf_bois==1), 1, 0)
data_complete$ProxChauf_bois1<-ifelse(is.na(data_complete$ProxChauf_bois1) ,0 ,  data_complete$ProxChauf_bois1)

table(data_complete$ProxChauf_bois1, useNA = "always")


# propriété

table(data_complete$ProxProp)
data_complete$ProxProp <- ifelse(data_complete$ProxProp == "Oui", 1, 0)

# réseau
table(data_complete$ProxRes)
data_complete$ProxRes <- ifelse(data_complete$ProxRes == "Oui", 1, 0)

# réseau2
table(data_complete$ProxRes2)
data_complete$ProxRes2 <- ifelse(data_complete$ProxRes2 == "Oui", 1, 0)

# travail
table(data_complete$ProxTravail)
data_complete$ProxTravail <- ifelse(data_complete$ProxTravail == "Oui", 1, 0)

# Info #un peu redondant par rapport à la question de connaissance, je propose de l'enlever. Idem pour infoou
table(data_complete$ProxInfo)

# logement
table(data_complete$ProxLog)

data_complete <- data_complete %>% 
  mutate(ProxLog = factor(ProxLog)) %>%
  mutate(
    ProxLog_Bois = ifelse(ProxLog == "Oui, entièrement (ex. ossature bois)" | ProxLog == "Oui, partiellement (ex. charpente seulement)", 1, 0),
    ProxLog_Autre = ifelse(ProxLog == "Non", 1, 0) )

table(data_complete$ProxLog_Bois, useNA = "always")

# PNR 

table(data_complete$ProxPnr)
data_complete$ProxPnr <- ifelse(data_complete$ProxPnr == "Oui", 1, 0)


# Promenade en forêt
table(data_complete$ProxProm)
data_complete$ProxProm <-factor(data_complete$ProxProm, 
                               levels = c("Jamais","Moins d’une fois par mois","Au moins une fois par mois","Au moins une fois par semaine","Tous les jours"))

data_complete <- data_complete %>% 
  mutate(
    ProxProm_Jamais = ifelse(ProxProm == "Jamais", 1, 0),
    ProxProm_Peu = ifelse(ProxProm == "Moins d’une fois par mois" | ProxProm=="Au moins une fois par mois", 1, 0),
    ProxProm_Souvent = ifelse(ProxProm == "Au moins une fois par semaine"| ProxProm=="Tous les jours", 1, 0) )


### environmental attitudes ###
data_complete <- data_complete %>%
  mutate(across(starts_with("ATTENV"),
                ~ factor(recode(.,
                                "Pas du tout d'accord" = 1,
                                "Plutôt pas d'accord" = 2,
                                "Ni d'accord ni pas d'accord" = 3,
                                "Plutôt d'accord" = 4,
                                "Tout à fait d'accord" = 5),
                         ordered = TRUE,levels = 1:5)
  ))

data_complete <- data_complete %>%
  mutate(across(starts_with("ATTENV") & ends_with("R."),
                ~ 6 - as.numeric(.)
  ))


### forest attitudes ###
data_complete <- data_complete %>%
  mutate(across(starts_with("ATTFO"),
                ~ factor(recode(.,
                                "Pas du tout d'accord" = 1,
                                "Plutôt pas d'accord" = 2,
                                "Ni d'accord ni pas d'accord" = 3,
                                "Plutôt d'accord" = 4,
                                "Tout à fait d'accord" = 5),
                         ordered = TRUE,levels = 1:5)
  ))


data_complete <- data_complete %>%
  mutate(across(starts_with("ATTFO") & ends_with("R."),
                ~ 6 - as.numeric(.)
  ))
### wood construction attitude ###
data_complete <- data_complete %>%
  mutate(across(starts_with("ATTBC"),
                ~ factor(recode(.,
                                "Pas du tout d'accord" = 1,
                                "Plutôt pas d'accord" = 2,
                                "Ni d'accord ni pas d'accord" = 3,
                                "Plutôt d'accord" = 4,
                                "Tout à fait d'accord" = 5),
                         ordered = TRUE,levels = 1:5)
  ))

data_complete <- data_complete %>%
  mutate(across(starts_with("ATTBC") & ends_with("R."),
                ~ 6 - as.numeric(.)
  ))

### wood energy attitude ###
colnames(data_complete)
names(data_complete)[names(data_complete)=="ATTBE.Tech."]<-"ATTBE.TechR1."
names(data_complete)[names(data_complete)=="ATTBE.TechR."]<-"ATTBE.Tech.R2."

data_complete <- data_complete %>%
  mutate(across(starts_with("ATTBE"),
    ~ factor(recode(.,
             "Pas du tout d'accord" = 1,
             "Plutôt pas d'accord" = 2,
             "Ni d'accord ni pas d'accord" = 3,
             "Plutôt d'accord" = 4,
             "Tout à fait d'accord" = 5),
      ordered = TRUE,levels = 1:5)
    ))

data_complete <- data_complete %>%
  mutate(across(starts_with("ATTBE") & ends_with("R."),
                ~ 6 - as.numeric(.)
  ))
data_complete <- data_complete %>%
  mutate(across(starts_with("ATTBE") & ends_with("R1."),
                ~ 6 - as.numeric(.)
  ))
data_complete <- data_complete %>%
  mutate(across(starts_with("ATTBE") & ends_with("R2."),
                ~ 6 - as.numeric(.)
  ))

### new tables attitudes

newtable<-as.data.frame(grep("^ATT",colnames(data_complete),value=T))
corr_table<-cbind(table, newtable)
colnames(corr_table) <- c("code1", "code2")

### add attitudes names
code_quest<-read.csv("~/recherche/DOMINOS/dominos_github/results/output/code_quest.csv", sep=";")
code_quest <- code_quest[grepl("ATT", code_quest$code), ]
corr_table<-cbind(corr_table, code_quest)
corr_table$code<-NULL
write.csv(corr_table, "~/recherche/DOMINOS/dominos_github/results/output/recode_quest.csv")

## ------------------------------------------------------------------------- ##
#                test the SEM modelprogressively                              #
## ------------------------------------------------------------------------- ##
library(lavaan)

model <- '
  ##############################
  # 1) MEASUREMENT MODEL
  ##############################

  # First-order factors
  Proximity =~ ProxChauf_bois1  + ProxTravail   + ProxLog_Bois  + ProxPnr  + ProxProm_Souvent + ProxRes + ProxRes2

'
fit <- sem(model, data = data_complete)
summary(fit, fit.measures = TRUE, standardized = TRUE)
modindices(fit, sort = TRUE, minimum.value = 5)

library(semPlot)

## ------------------------------------------------------------------------- ##
#                         test the SEM model                                  #
## ------------------------------------------------------------------------- ##
ordered_vars <- grep("^ATT", names(data_complete), value = TRUE)

table<-data_complete[,c("id","ProxChauf_bois1","ProxTravail","ProxLog_Bois","ProxPnr","ProxProm_Souvent","ConEssence_num","ConSurface_num","ConSurface2_num","ConGestion_num","ConRecolte_num","ConEval_num","ConProp_num","SocioAge_Jeune","SocioAge_Vieux","SocioGenre_Femme","SocioCom_Grande", "SocioCom_Rural","SocioEduc_sup","SocioEduc_inf","SocioRevenu_num" )]
str(table)

str(data_complete[, ordered_vars])
data_complete[, ordered_vars] <- lapply(
  data_complete[, ordered_vars],
  function(x) {
    if (!is.ordered(x)) {
      factor(x, ordered = TRUE)
    } else {
      x
    }
  }
)


model <- '
  ##############################
  # 1) MEASUREMENT MODEL
  ##############################

  # First-order factors
  Proximity =~ ProxChauf_bois1 + ProxTravail   + ProxLog_Bois  + ProxPnr  + ProxProm_Souvent 
  Threat =~ ATTMENACE.Sante. + ATTMENACE.SanteR. + ATTMENACE.CC. + ATTMENACE.CCR. + ATTMENACE.Gestion. + ATTMENACE.GestionR. + ATTMENACE.Defo. + ATTMENACE.DefoR. + ATTMENACE.InqR. + ATTMENACE.Inq. 
  Knowledge =~ ConEssence_num + ConSurface_num + ConSurface2_num + ConGestion_num + ConRecolte_num + ConEval_num + ConProp_num 
  Env_Preserv =~ ATTENV.P1R. +ATTENV.P1. + ATTENV.P2. + ATTENV.P2R. + ATTENV.P6R. + ATTENV.P6. + ATTENV.P8R. +ATTENV.P8. +ATTENV.P12.+ATTENV.P12R.
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
  Threat ~ SocioAge_Jeune + SocioAge_Vieux + SocioGenre_Femme   +   SocioCom_Grande+ SocioCom_Rural    + SocioEduc_sup + SocioEduc_inf  + SocioRevenu_num  + Proximity
  Knowledge ~ SocioAge_Jeune + SocioAge_Vieux + SocioGenre_Femme   + SocioCom_Rural   + SocioEduc_sup + SocioEduc_inf  + SocioRevenu_num  + Proximity
  AttEnv ~ Threat + Knowledge + SocioAge_Jeune + SocioAge_Vieux + SocioGenre_Femme  + SocioCom_Grande + SocioCom_Rural    + SocioEduc_sup + SocioEduc_inf  + SocioRevenu_num + Proximity
  AttFo ~ Threat + Knowledge +  SocioAge_Jeune + SocioAge_Vieux + SocioGenre_Femme  + SocioCom_Grande + SocioCom_Rural    + SocioEduc_sup + SocioEduc_inf  + SocioRevenu_num  + Proximity
  AttCW ~ Threat + Knowledge + SocioAge_Jeune + SocioAge_Vieux + SocioGenre_Femme  + SocioCom_Grande + SocioCom_Rural    + SocioEduc_sup + SocioEduc_inf + SocioRevenu_num + Proximity
  AttEW ~ Threat + Knowledge   + SocioAge_Jeune + SocioAge_Vieux + SocioGenre_Femme  + SocioCom_Grande + SocioCom_Rural  + SocioEduc_sup + SocioEduc_inf  + SocioRevenu_num  + Proximity

  ##############################
  # (Optional) Covariances
  ##############################

'
# --- Fit the model ---
fit <- sem(model, data = data_complete, ordered = ordered_vars)
# --- Get summary with fit indices ---
summary(fit, fit.measures = TRUE, standardized = TRUE)

# --- Optional: modification indices to inspect improvements ---
modindices(fit, sort = TRUE, minimum.value = 5)

library(semPlot)


# Pour tracer le path diagram :
semPaths(fit, what = "std", layout = "tree", edge.label.cex = 0.8)

