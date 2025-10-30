rm(list=ls())
library("dplyr")
library("tidyverse")
library("tidyr") # For tidying the data
library("ggplot2") # For plotting sophisticated graphs
library("rio") # For plotting sophisticated graphs
library("ggstats") # For plotting sophisticated graphs
library("labelled")
library("lavaan")
library("lavaanPlot")
library("DiagrammeRsvg")
library("rsvg")
library("corrplot")
path <- "~/recherche/DOMINOS/dominos_github/resources/inhouse/results_survey857139_code.csv" # nolint
data <- read.csv(file = path, header = TRUE, sep = ";", na.strings=c("","NA"))

## ------------------------------------------------------------------------- ##
#                                clean data                                   #
## ------------------------------------------------------------------------- ##

data_complete <- data[!is.na(data$submitdate), ] #regarder un peu plus ou est ce qu'ils se sont arrétés
data_complete <- data_complete[data_complete$SocioGenre!="Autre", ] #concerne seulement 3 répondants

### time to answer the questionnaire 





### tables attitudes

table<-as.data.frame(grep("^ATT",colnames(data_complete),value=T))

#### recodage variables socioeconomiques ####

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

####  knowledge variables ####

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


#### perception of threat variables ####

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


#### proximity variables ####

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


#### environmental attitudes ####
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
table(data_complete$ATTENV.P1.)
table(data_complete$ATTENV.P1R.)

data_complete <- data_complete %>%
  mutate(across(starts_with("ATTENV") & ends_with("R."),
                ~ 6 - as.numeric(.)
  ))



#### forest attitudes ####
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

#### wood construction attitude ####
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

#### wood energy attitude ####
colnames(data_complete)

names(data_complete)[names(data_complete)=="ATTBE.Nature."]<-"ATTBE.NatureR1."
names(data_complete)[names(data_complete)=="ATTBE.NatureR."]<-"ATTBE.Nature."

names(data_complete)[names(data_complete)=="ATTBE.Tech."]<-"ATTBE.TechR1."
names(data_complete)[names(data_complete)=="ATTBE.TechR."]<-"ATTBE.TechR2."
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
table(data_complete$ATTBE.TechR2.)

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


#### final cleaning ####
ordered_vars <- grep("^ATT", names(data_complete), value = TRUE)

table<-data_complete[,c("id","ProxChauf_bois1","ProxTravail","ProxLog_Bois","ProxPnr","ProxProm_Souvent","ConEssence_num","ConSurface_num","ConSurface2_num","ConGestion_num","ConRecolte_num","ConEval_num","ConProp_num","SocioAge_Jeune","SocioAge_Vieux","SocioGenre_Femme","SocioCom_Grande", "SocioCom_Rural","SocioEduc_sup","SocioEduc_inf","SocioRevenu_num" )]


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
### tout remettre en numéric

data_complete <- data_complete %>%
  mutate(across(starts_with("ATT"), as.numeric))

### remove not valid answer

# validity question (Si vous lisez cette affirmation, cochez "tout à fait d'accord")
table(data_complete$ATTFO.test.) # remove resp. answering 1,2,3,4

data_complete<-data_complete[data_complete$ATTFO.test.==5,] # 4702 obs.



#############################################################################
# -------------------------------- CFA MODEL -----------------------------  #
#############################################################################
# test des corrélations env
env_vars <- grep("^ATTENV", names(data_complete), value = TRUE)
env_data <- data_complete[, env_vars]
env_data_num <- data.frame(lapply(env_data, function(x) as.numeric(as.character(x))))
cor_matrix <- cor(env_data_num, use = "pairwise.complete.obs")

corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)
high_corrs <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
cor_matrix[high_corrs]

fo_vars <- grep("^ATTFO", names(data_complete), value = TRUE)
fo_data <- data_complete[, fo_vars]
fo_data_num <- data.frame(lapply(fo_data, function(x) as.numeric(as.character(x))))
cor_matrix <- cor(fo_data_num, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)
high_corrs <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
cor_matrix[high_corrs]


be_vars <- grep("^ATTBE", names(data_complete), value = TRUE)
be_data <- data_complete[, be_vars]
be_data_num <- data.frame(lapply(be_data, function(x) as.numeric(as.character(x))))
cor_matrix <- cor(be_data_num, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)
high_corrs <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
cor_matrix[high_corrs]

bc_vars <- grep("^ATTBC", names(data_complete), value = TRUE)
bc_data <- data_complete[, bc_vars]
bc_data_num <- data.frame(lapply(bc_data, function(x) as.numeric(as.character(x))))
cor_matrix <- cor(bc_data_num, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)
high_corrs <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
cor_matrix[high_corrs]

#### --------------------------------------------------------------------####
####               confirmatory factor analysis                          ####
#### --------------------------------------------------------------------####

#### ENVIRONMENTAL ATTITUDES ####



# modèle à 2 facteurs
cfa_attenv <- '
  ATTENVP =~ ATTENV.P1. + ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R.+ ATTENV.P3.+ATTENV.P3R. + ATTENV.P6. + ATTENV.P6R. + ATTENV.P8. + ATTENV.P8R. + ATTENV.P12. + ATTENV.P12R. + ATTENV.P11.+ATTENV.P11R.
  ATTENVU =~ ATTENV.U4. + ATTENV.U4R. + ATTENV.U5. + ATTENV.U5R. + ATTENV.U7. + ATTENV.U7R. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10. + ATTENV.U10R.
  ATTENVP ~~ ATTENVU
'
fit_cfa_attenv <- cfa(cfa_attenv, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv, fit.measures=TRUE, standardized=TRUE)
fscores <- lavPredict(fit_cfa_attenv)        # scores latents
apply(fscores, 2, mean)               # Moyennes des facteurs
apply(fscores, 2, sd) 

plot_cfa_attenv<-lavaanPlot(  model = fit_cfa_attenv,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                             #labels = node_names,   # <- use labels instead of node_names
                             graph_options = list(rankdir = "TB", layout = "dot"),
                             edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attenv, "~/recherche/DOMINOS/results/plot_cfa_attenv.png", width = 1500, height=300)

# modèle à 1 facteur (pour version 12 items)
cfa_attenv_2 <- '
  ATTENV =~ ATTENV.P1. + ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R.+ ATTENV.P3.+ATTENV.P3R. + ATTENV.P6. + ATTENV.P6R. + ATTENV.P8. + ATTENV.P8R. + ATTENV.P12. + ATTENV.P12R. + ATTENV.P11.+ATTENV.P11R.+ ATTENV.U4. + ATTENV.U4R. + ATTENV.U5. + ATTENV.U5R. + ATTENV.U7. + ATTENV.U7R. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10. + ATTENV.U10R.
'
fit_cfa_attenv_2 <- cfa(cfa_attenv_2, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv_2, fit.measures=TRUE, standardized=TRUE)
fscores <- lavPredict(fit_cfa_attenv_2)        # scores latents
apply(fscores, 2, mean)               # Moyennes des facteurs
apply(fscores, 2, sd) 
plot_cfa_attenv_2<-lavaanPlot(  model = fit_cfa_attenv_2,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                              #labels = node_names,   # <- use labels instead of node_names
                              graph_options = list(rankdir = "TB", layout = "dot"),
                              edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attenv_2, "~/recherche/DOMINOS/results/plot_cfa_attenv_2.png", width = 1500, height=300)

# modèle à 2 facteurs avec corrélations erreurs
cfa_attenv_3 <- '
  ATTENVP =~ ATTENV.P1. + ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R.+ ATTENV.P3.+ATTENV.P3R. + ATTENV.P6. + ATTENV.P6R. + ATTENV.P8. + ATTENV.P8R. + ATTENV.P12. + ATTENV.P12R. + ATTENV.P11.+ATTENV.P11R.
  ATTENVU =~ ATTENV.U4. + ATTENV.U4R. + ATTENV.U5. + ATTENV.U5R. + ATTENV.U7. + ATTENV.U7R. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10. + ATTENV.U10R.
  ATTENVP ~~ ATTENVU
    ATTENV.P1. ~~ ATTENV.P1R.
  ATTENV.P2. ~~ ATTENV.P2R.
  ATTENV.P3. ~~ ATTENV.P3R.
  ATTENV.P6. ~~ ATTENV.P6R.
  ATTENV.P8. ~~ ATTENV.P8R.
  ATTENV.P11. ~~ ATTENV.P11R.
   ATTENV.P12. ~~ ATTENV.P12R.
  ATTENV.U4. ~~ ATTENV.U4R. 
  ATTENV.U5.  ~~ ATTENV.U5R. 
  ATTENV.U7.  ~~ ATTENV.U7R. 
  ATTENV.U9. ~~ ATTENV.U9R. 
  ATTENV.U10.  ~~ ATTENV.U10R.
  
'
fit_cfa_attenv_3 <- cfa(cfa_attenv_3, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv_3, fit.measures=TRUE, standardized=TRUE)
plot_cfa_attenv_3<-lavaanPlot(  model = fit_cfa_attenv_3,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                                #labels = node_names,   # <- use labels instead of node_names
                                graph_options = list(rankdir = "TB", layout = "dot"),
                                edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attenv_3, "~/recherche/DOMINOS/results/plot_cfa_attenv_3.png", width = 1500, height=300)

# modèle à 2 facteurs avec corrélations erreurs # enlever p12
cfa_attenv_4 <- '
  ATTENVP =~ ATTENV.P1. + ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R.+ ATTENV.P3.+ATTENV.P3R. + ATTENV.P6. + ATTENV.P6R. + ATTENV.P8. + ATTENV.P8R.  + ATTENV.P11.+ATTENV.P11R.
  ATTENVU =~ ATTENV.U4. + ATTENV.U4R. + ATTENV.U5. + ATTENV.U5R. + ATTENV.U7. + ATTENV.U7R. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10. + ATTENV.U10R.
  ATTENVP ~~ ATTENVU
    ATTENV.P1. ~~ ATTENV.P1R.
  ATTENV.P2. ~~ ATTENV.P2R.
  ATTENV.P3. ~~ ATTENV.P3R.
  ATTENV.P6. ~~ ATTENV.P6R.
  ATTENV.P8. ~~ ATTENV.P8R.
  ATTENV.P11. ~~ ATTENV.P11R.
  ATTENV.U4. ~~ ATTENV.U4R. 
  ATTENV.U5.  ~~ ATTENV.U5R. 
  ATTENV.U7.  ~~ ATTENV.U7R. 
  ATTENV.U9. ~~ ATTENV.U9R. 
  ATTENV.U10.  ~~ ATTENV.U10R.
  
'
fit_cfa_attenv_4 <- cfa(cfa_attenv_4, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv_4, fit.measures=TRUE, standardized=TRUE)
plot_cfa_attenv_4<-lavaanPlot(  model = fit_cfa_attenv_4,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                                #labels = node_names,   # <- use labels instead of node_names
                                graph_options = list(rankdir = "TB", layout = "dot"),
                                edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attenv_4, "~/recherche/DOMINOS/results/plot_cfa_attenv_4.png", width = 1500, height=300)


# modèle à 2 facteurs avec corrélations erreurs # enlever P3
cfa_attenv_5 <- '
  ATTENVP =~ ATTENV.P1. + ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R. + ATTENV.P6. + ATTENV.P6R. + ATTENV.P8. + ATTENV.P8R.  + ATTENV.P11.+ATTENV.P11R.
  ATTENVU =~ ATTENV.U4. + ATTENV.U4R. + ATTENV.U5. + ATTENV.U5R. + ATTENV.U7. + ATTENV.U7R. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10. + ATTENV.U10R.
  ATTENVP ~~ ATTENVU
    ATTENV.P1. ~~ ATTENV.P1R.
  ATTENV.P2. ~~ ATTENV.P2R.
  ATTENV.P6. ~~ ATTENV.P6R.
  ATTENV.P8. ~~ ATTENV.P8R.
  ATTENV.P11. ~~ ATTENV.P11R.
  ATTENV.U4. ~~ ATTENV.U4R. 
  ATTENV.U5. ~~ ATTENV.U5R. 
  ATTENV.U7.  ~~ ATTENV.U7R. 
  ATTENV.U9. ~~ ATTENV.U9R. 
  ATTENV.U10.  ~~ ATTENV.U10R.
  
'
fit_cfa_attenv_5<- cfa(cfa_attenv_5, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv_5, fit.measures=TRUE, standardized=TRUE)
plot_cfa_attenv_5<-lavaanPlot(  model = fit_cfa_attenv_5,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                                #labels = node_names,   # <- use labels instead of node_names
                                graph_options = list(rankdir = "TB", layout = "dot"),
                                edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attenv_5, "~/recherche/DOMINOS/results/plot_cfa_attenv_5.png", width = 1500, height=300)

# only one factor

cfa_attenvtest <- '
  ATTENV =~ ATTENV.P1. + ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R. + ATTENV.P6. + ATTENV.P6R. + ATTENV.P8. + ATTENV.P8R.  + ATTENV.P11.+ATTENV.P11R. + ATTENV.U4. + ATTENV.U4R. + ATTENV.U5. + ATTENV.U5R. + ATTENV.U7. + ATTENV.U7R. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10. + ATTENV.U10R.
   ATTENV.P1. ~~ ATTENV.P1R.
  ATTENV.P2. ~~ ATTENV.P2R.
  ATTENV.P3. ~~ ATTENV.P3R.
  ATTENV.P6. ~~ ATTENV.P6R.
  ATTENV.P8. ~~ ATTENV.P8R.
  ATTENV.P11. ~~ ATTENV.P11R.
   ATTENV.P12. ~~ ATTENV.P12R.
  ATTENV.U4. ~~ ATTENV.U4R. 
  ATTENV.U5.  ~~ ATTENV.U5R. 
  ATTENV.U7.  ~~ ATTENV.U7R. 
  ATTENV.U9. ~~ ATTENV.U9R. 
  ATTENV.U10.  ~~ ATTENV.U10R.
  
'
fit_cfa_attenvtest <- cfa(cfa_attenvtest, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenvtest, standardized=TRUE, fit.measures=TRUE)


comp_fit_env <- rbind(
  Model = fitMeasures(fit_cfa_attenv, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr")),
  Model_2 = fitMeasures(fit_cfa_attenv_2, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr")),
  Model_3 = fitMeasures(fit_cfa_attenv_3, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr")),
  Model_4 = fitMeasures(fit_cfa_attenv_4, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr")),
  Model_5 = fitMeasures(fit_cfa_attenv_5, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr")),
  Model_6 = fitMeasures(fit_cfa_attenvtest, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr"))
  
)




 #### FOREST ATTITUDES ####
 

 cfa_attfo <- '
  ATTFOP =~ ATTFO.P1. + ATTFO.P1R. + ATTFO.P2. + ATTFO.P2R. + ATTFO.P6. + ATTFO.P6R. + ATTFO.P8. + ATTFO.P8R. + ATTFO.P3. + ATTFO.P3R.
  ATTFOU =~ ATTFO.U4. + ATTFO.U4R. + ATTFO.U5. + ATTFO.U5R. + ATTFO.U7. + ATTFO.U7R. + ATTFO.U9. + ATTFO.U9R. + ATTFO.U10. + ATTFO.U10R.
  ATTFOP ~~ ATTFOU
'
 fit_cfa_attfo <- cfa(cfa_attfo, data=data_complete, std.lv=TRUE)
 summary(fit_cfa_attfo, standardized=TRUE, fit.measures=TRUE)
 

 plot_cfa_ATTFO<-lavaanPlot(  model = fit_cfa_attfo,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
   #labels = node_names,   # <- use labels instead of node_names
   graph_options = list(rankdir = "TB", layout = "dot"),
   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
 )
 save_png(plot_cfa_ATTFO, "~/recherche/DOMINOS/results/plot_cfa_ATTFO.png", width = 1500, height=300)
 
 #### WOOD ATTITUDES ####

 ### Construction

 # model1
 cfa_ATTBC_model1 <- '
  ATTBC_ECO=~  ATTBC.EcomenR. + ATTBC.Ecomen. + ATTBC.Ecoloc. + ATTBC.EcolocR. 
  ATTBC_CONF=~  ATTBC.BienEtreR. + ATTBC.BienEtre. + ATTBC.TechR. + ATTBC.Tech. + ATTBC.NatR. + ATTBC.Nat.
  ATTBC_BIO =~ ATTBC.OnehealthR. + ATTBC.Onehealth. + ATTBC.DurableR. + ATTBC.Durable. 
  ATTBC_ECO~~ATTBC_CONF
  ATTBC_CONF~~ ATTBC_BIO
  ATTBC_BIO ~~ATTBC_ECO
'
 fit_cfa_ATTBC_model1 <- cfa(cfa_ATTBC_model1, data = data_complete, std.lv = TRUE)
 lavInspect(fit_cfa_ATTBC_model1, "cov.lv") #

 summary(fit_cfa_ATTBC_model1, standardized=TRUE, fit.measures=TRUE)
 
 plot_cfa_ATTBC_model1<-lavaanPlot(model = fit_cfa_ATTBC_model1, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
   graph_options = list(rankdir = "TB", layout = "dot"),
   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model1, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model1.png", width = 1500, height=400)
 
 # model 2
 cfa_ATTBC_model2 <- '
  ATTBC_TECH =~  ATTBC.EcomenR. + ATTBC.Ecomen. + ATTBC.Ecoloc. + ATTBC.EcolocR.+ ATTBC.TechR. + ATTBC.Tech.
  ATTBC_CONF =~ ATTBC.BienEtreR. + ATTBC.BienEtre. + ATTBC.NatR. + ATTBC.Nat.
  ATTBC_BIO =~ ATTBC.OnehealthR. + ATTBC.Onehealth. + ATTBC.DurableR. + ATTBC.Durable. 
  ATTBC_TECH~~ATTBC_CONF
  ATTBC_CONF~~ ATTBC_BIO
  ATTBC_BIO ~~ATTBC_TECH
'
 fit_cfa_ATTBC_model2 <- cfa(cfa_ATTBC_model2, data = data_complete, std.lv = TRUE)
 lavInspect(cfa_ATTBC_model2, "cov.lv") 
 summary(fit_cfa_ATTBC_model2, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBC_model2<-lavaanPlot(model = fit_cfa_ATTBC_model2, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model2, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model2.png", width = 1500, height=400)
 

 # analyse epxloratoire
 
 library(psych)
 vars_ATTBC <- data_complete[, c("ATTBC.EcomenR." , "ATTBC.Ecomen." , "ATTBC.BienEtreR." , "ATTBC.BienEtre." , "ATTBC.TechR." , "ATTBC.Tech." , "ATTBC.Ecoloc." , "ATTBC.EcolocR.",
                                 "ATTBC.OnehealthR." , "ATTBC.Onehealth." , "ATTBC.DurableR." , "ATTBC.Durable." , "ATTBC.NatR." , "ATTBC.Nat.")]
 fa.parallel(vars_ATTBC, fm = "ml", fa = "fa")
 efa_ATTBC <- fa(vars_ATTBC, nfactors = 4, rotate = "oblimin", fm = "ml")
 print(efa_ATTBC, cut = 0.3, sort = TRUE)
 
 
 
 vars_ATTBC <- data_complete[, c("ATTBC.EcomenR." , "ATTBC.Ecomen." , "ATTBC.BienEtreR." , "ATTBC.BienEtre." , "ATTBC.TechR." , "ATTBC.Tech." , "ATTBC.Ecoloc." , "ATTBC.EcolocR.", "ATTBC.Onehealth." , "ATTBC.DurableR." , "ATTBC.Durable."  , "ATTBC.Nat.")]
 fa.parallel(vars_ATTBC, fm = "ml", fa = "fa")
 efa_ATTBC <- fa(vars_ATTBC, nfactors = 3, rotate = "oblimin", fm = "ml")
 print(efa_ATTBC, cut = 0.3, sort = TRUE)
 
 # model 3
 cfa_ATTBC_model3 <- '
  ATTBC_POS =~  ATTBC.Ecomen. + ATTBC.Ecoloc. + ATTBC.Tech.+ ATTBC.BienEtre.  + ATTBC.Durable.+ ATTBC.Nat.+ ATTBC.Onehealth.
  ATTBC_NEG =~ ATTBC.BienEtreR. + ATTBC.NatR.+ ATTBC.OnehealthR.  + ATTBC.DurableR.   + ATTBC.EcolocR.
  ATTBC_POS ~~ATTBC_NEG
'
 fit_cfa_ATTBC_model3 <- cfa(cfa_ATTBC_model3, data = data_complete, std.lv = TRUE)
 summary(fit_cfa_ATTBC_model3, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBC_model3<-lavaanPlot(model = fit_cfa_ATTBC_model3, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model3, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model3.png", width = 1500, height=400)
 
 
 # model 4
 cfa_ATTBC_model4 <- '
  ATTBC_U=~    ATTBC.Ecomen. + ATTBC.Ecoloc.  + ATTBC.BienEtre.   + ATTBC.Tech.
  ATTBC_P =~  + ATTBC.Nat. + ATTBC.Durable.+ ATTBC.Onehealth.   

'
 fit_cfa_ATTBC_model4 <- cfa(cfa_ATTBC_model4, data = data_complete, std.lv = TRUE)
 summary(fit_cfa_ATTBC_model4, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBC_model4<-lavaanPlot(model = fit_cfa_ATTBC_model4, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model4, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model4.png", width = 1500, height=400)
 
 vars_ATTBC <- data_complete[, c("ATTBC.Ecomen." , "ATTBC.Ecoloc." , "ATTBC.BienEtre." , "ATTBC.Tech." , "ATTBC.Nat." , "ATTBC.Durable." , "ATTBC.Onehealth." )]
 fa.parallel(vars_ATTBC, fm = "ml", fa = "fa")
 efa_ATTBC <- fa(vars_ATTBC, nfactors = 2, rotate = "oblimin", fm = "ml")
 print(efa_ATTBC, cut = 0.3, sort = TRUE)
 
 # model 5
 cfa_ATTBC_model5 <- '
  ATTBC_HUM=~    ATTBC.Ecomen. + ATTBC.Ecoloc.   + ATTBC.Tech. +ATTBC.Durable.+ ATTBC.Onehealth.   
  ATTBC_EGO =~  + ATTBC.Nat. +  + ATTBC.BienEtre. 

'
 fit_cfa_ATTBC_model5 <- cfa(cfa_ATTBC_model5, data = data_complete, std.lv = TRUE)
 summary(fit_cfa_ATTBC_model5, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBC_model5<-lavaanPlot(model = fit_cfa_ATTBC_model5, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model5, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model5.png", width = 1500, height=400)
 

 ### energy
 # model 3
 cfa_ATTBE_model3 <- '
  ATTBE_NEG =~   ATTBE.BienEtreR. + ATTBE.TechR2.+ ATTBE.NatureR1.  + ATTBE.DurableR.+ ATTBE.HealthR.+ATTBE.EcolocR. + ATTBE.EcomenR.
  ATTBE_POS =~   ATTBE.Health. + ATTBE.Durable. + ATTBE.Nature.+ ATTBE.Ecoloc. + ATTBE.Ecomen.  + ATTBE.BienEtre. 
  ATTBE_POS ~~ATTBE_NEG
'
 fit_cfa_ATTBE_model3 <- cfa(cfa_ATTBE_model3, data=data_complete, std.lv=TRUE)
 lavInspect(fit_cfa_ATTBE_model3, "cov.lv") 
 summary(fit_cfa_ATTBE_model3, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBE_model3<-lavaanPlot(model = fit_cfa_ATTBE_model3, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBE_model3, "~/recherche/DOMINOS/results/plot_cfa_ATTBE_model3.png", width = 1500, height=400)
 
 #EFA
 library(psych)
 vars_ATTBE <- data_complete[,c("ATTBE.EcomenR.","ATTBE.BienEtreR." , "ATTBE.BienEtre."  , "ATTBE.TechR1." , "ATTBE.TechR2." , "ATTBE.Nature." 
                              , "ATTBE.HealthR."  , "ATTBE.Health." , "ATTBE.Durable." ,"ATTBE.DurableR."  , "ATTBE.Ecoloc." , "ATTBE.EcolocR." , "ATTBE.Ecomen.") ]
 fa.parallel(vars_ATTBE, fm = "ml", fa = "fa")
 efa_ATTBE <- fa(vars_ATTBE, nfactors = 2, rotate = "oblimin", fm = "ml")
 print(efa_ATTBE, cut = 0.3, sort = TRUE)
 
 vars_ATTBE <- data_complete[,c( "ATTBE.BienEtre."   , "ATTBE.Nature."   , "ATTBE.Health." , "ATTBE.Durable."   , "ATTBE.Ecoloc."  , "ATTBE.Ecomen.") ]
 fa.parallel(vars_ATTBE, fm = "ml", fa = "fa")
 efa_ATTBE <- fa(vars_ATTBE, nfactors = 2, rotate = "oblimin", fm = "ml")
 print(efa_ATTBE, cut = 0.3, sort = TRUE)
 
 # model4 
 cfa_ATTBE_model4 <- '
  ATTBE_EGO =~   ATTBE.EcomenR.+ATTBE.BienEtreR. + ATTBE.BienEtre.  + ATTBE.TechR1. + ATTBE.TechR2.+ ATTBE.NatureR1. + ATTBE.Nature. 
  ATTBE_HUM =~  + ATTBE.HealthR.  + ATTBE.Health. + ATTBE.Durable. +ATTBE.DurableR.  + ATTBE.Ecoloc. + ATTBE.EcolocR. + ATTBE.Ecomen. 
  '
 
 fit_cfa_ATTBE_model4 <- cfa(cfa_ATTBE_model4, data=data_complete, std.lv=TRUE)
 lavInspect(fit_cfa_ATTBE_model4, "cov.lv") 
 summary(fit_cfa_ATTBE_model4, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBE_model4<-lavaanPlot(model = fit_cfa_ATTBE_model4, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBE_model4, "~/recherche/DOMINOS/results/plot_cfa_ATTBE_model4.png", width = 1500, height=400)
 
 
 # model5 
 cfa_ATTBE_model5 <- '
  ATTBE_EGO =~   ATTBE.BienEtreR. + ATTBE.BienEtre.   + ATTBE.Nature. 
  ATTBE_HUM =~  + ATTBE.HealthR.  + ATTBE.Health. + ATTBE.Durable. +ATTBE.DurableR.  + ATTBE.Ecoloc.   
  '
 
 fit_cfa_ATTBE_model5 <- cfa(cfa_ATTBE_model5, data=data_complete, std.lv=TRUE)
 lavInspect(fit_cfa_ATTBE_model5, "cov.lv") 
 summary(fit_cfa_ATTBE_model5, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBE_model5<-lavaanPlot(model = fit_cfa_ATTBE_model5, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBE_model5, "~/recherche/DOMINOS/results/plot_cfa_ATTBE_model5.png", width = 1500, height=400)
 
 # model6
 cfa_ATTBE_model6 <- '
  ATTBE_EGO =~   ATTBE.BienEtre.   + ATTBE.Nature. 
  ATTBE_HUM =~   ATTBE.Health. + ATTBE.Durable.  + ATTBE.Ecoloc.   + ATTBE.Ecomen.
  '
 
 fit_cfa_ATTBE_model6 <- cfa(cfa_ATTBE_model6, data=data_complete, std.lv=TRUE)
 lavInspect(fit_cfa_ATTBE_model6, "cov.lv") 
 summary(fit_cfa_ATTBE_model6, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBE_model6<-lavaanPlot(model = fit_cfa_ATTBE_model6, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBE_model6, "~/recherche/DOMINOS/results/plot_cfa_ATTBE_model6.png", width = 1500, height=400)
 
 
 ## bois energie + bois construction
 library(psych)
 vars_ATTB <- data_complete[, grep("^ATTB", names(data_complete), value = TRUE)]
 fa.parallel(vars_ATTB, fm = "ml", fa = "fa")
 efa_ATTB <- fa(vars_ATTB, nfactors = 4, rotate = "oblimin", fm = "ml")
 print(efa_ATTB, cut = 0.3, sort = TRUE)
 
 # model1
 cfa_ATTB_model1 <- '
  ATTB_EGO =~   ATTBE.BienEtre.   + ATTBE.Nature.  + ATTBC.Nat. +  ATTBC.BienEtre. 
  ATTB_HUM =~   ATTBE.Health. + ATTBE.Durable.  + ATTBE.Ecoloc.   + ATTBE.Ecomen. + ATTBC.Ecomen. + ATTBC.Ecoloc.   + ATTBC.Tech. +ATTBC.Durable.+ ATTBC.Onehealth.  
  '
 
 
 fit_cfa_ATTB_model1 <- cfa(cfa_ATTB_model1, data=data_complete, std.lv=TRUE)
 lavInspect(fit_cfa_ATTB_model1, "cov.lv") 
 summary(fit_cfa_ATTB_model1, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTB_model1<-lavaanPlot(model = fit_cfa_ATTB_model1, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTB_model1, "~/recherche/DOMINOS/results/plot_cfa_ATTB_model1.png", width = 1500, height=400)
 
 # model2
 cfa_ATTB_model2 <- '
  ATTB_CONF =~   ATTBE.BienEtre.   + ATTBE.Nature.  + ATTBC.Nat. +  ATTBC.BienEtre. 
  ATTB_ENV =~   ATTBE.Health. + ATTBE.Durable.   +ATTBC.Durable.+ ATTBC.Onehealth.  + ATTBE.Ecoloc.    + ATTBC.Ecoloc.
  ATTB_ECO =~  ATTBE.Ecomen. + ATTBC.Ecomen.+ ATTBC.Tech. 
 ' 
 fit_cfa_ATTB_model2 <- cfa(cfa_ATTB_model2, data=data_complete, std.lv=TRUE)
 lavInspect(fit_cfa_ATTB_model2, "cov.lv") 
 summary(fit_cfa_ATTB_model2, standardized=TRUE, fit.measures=TRUE)
 
 
 #### perception of threat ####
 
 # correlation
 men_vars <- grep("^ATTMEN", names(data_complete), value = TRUE)
 men_data <- data_complete[, men_vars]
 men_data_num <- data.frame(lapply(men_data, function(x) as.numeric(as.character(x))))
 cor_matrix <- cor(men_data_num, use = "pairwise.complete.obs")
 corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

 # model 1
 cfa_attmen <- '
 ATTMEN_INQ =~  +  ATTMENACE.NoptR. +   ATTMENACE.Nopt.    + ATTMENACE.Inq. + ATTMENACE.InqR.
 ATTMEN_EX =~ ATTMENACE.Gestion. + ATTMENACE.GestionR. + ATTMENACE.Defo. + ATTMENACE.DefoR.
 ATTMEN_SA =~ ATTMENACE.CC.    +   ATTMENACE.CCR. + ATTMENACE.Sante. + ATTMENACE.SanteR.
 ATTMEN_INQ~~ATTMEN_EX
 ATTMEN_EX ~~ATTMEN_SA
 ATTMEN_SA~~ATTMEN_INQ
 '
 fit_cfa_attmen<- cfa(cfa_attmen, data=data_complete, std.lv=TRUE) 
 lavInspect(fit_cfa_attmen, "cov.lv")
 summary(fit_cfa_attmen, standardized=TRUE, fit.measures=TRUE)

 plot_cfa_attmen<-lavaanPlot( model = fit_cfa_attmen,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
   graph_options = list(rankdir = "TB", layout = "dot"),
   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_attmen, "~/recherche/DOMINOS/results/plot_cfa_attmen.png", width = 1500, height=400)
 
 
 # analyse epxloratoire
 men_vars <- grep("^ATTMEN", names(data_complete), value = TRUE)
 
 library(psych)
 vars_ATTMEN <- data_complete[, grep("^ATTMEN", names(data_complete))]
 fa.parallel(vars_ATTMEN, fm = "ml", fa = "fa")
 efa_ATTMEN <- fa(vars_ATTMEN, nfactors = 4, rotate = "oblimin", fm = "ml")
 print(efa_ATTMEN, cut = 0.3, sort = TRUE)
 
 
 
 # model 2
 
 cfa_attmen_model2 <- '
 ATTMEN_OPTA =~ ATTMENACE.GestionR. + ATTMENACE.NoptR.  +  + ATTMENACE.SanteR. 
 ATTMEN_OPTF=~  ATTMENACE.CCR.  +   ATTMENACE.DefoR. + ATTMENACE.InqR. 
 ATTMEN_PESA=~ ATTMENACE.Nopt. + ATTMENACE.Gestion. + ATTMENACE.Sante.
 ATTMEN_PESF=~    ATTMENACE.Inq. + ATTMENACE.Defo. + ATTMENACE.CC.   
 '
 fit_cfa_attmen_model2<- cfa(cfa_attmen_model2, data=data_complete, std.lv=TRUE) 
 lavInspect(fit_cfa_attmen_model2, "cov.lv")
 summary(fit_cfa_attmen_model2, standardized=TRUE, fit.measures=TRUE)
 
 plot_cfa_attmen_model2<-lavaanPlot( model = fit_cfa_attmen_model2,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
                              graph_options = list(rankdir = "TB", layout = "dot"),
                              edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_attmen_model2, "~/recherche/DOMINOS/results/plot_cfa_attmen_model2.png", width = 1000, height=500)
 
 
 # model 3
 
 cfa_attmen_model3 <- '
 ATTMEN_OPTA =~ ATTMENACE.GestionR. + ATTMENACE.NoptR.  +  + ATTMENACE.SanteR. 
 ATTMEN_OPTF=~  ATTMENACE.CCR.  +   ATTMENACE.DefoR. + ATTMENACE.InqR. 
 ATTMEN_PESA=~ ATTMENACE.Nopt. + ATTMENACE.Gestion. + ATTMENACE.Sante.+ ATTMENACE.Inq. + ATTMENACE.Defo. + ATTMENACE.CC.  
 '
 fit_cfa_attmen_model3<- cfa(cfa_attmen_model3, data=data_complete, std.lv=TRUE) 
 lavInspect(fit_cfa_attmen_model3, "cov.lv")
 summary(fit_cfa_attmen_model3, standardized=TRUE, fit.measures=TRUE)
 
 plot_cfa_attmen_model3<-lavaanPlot( model = fit_cfa_attmen_model3,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
                                     graph_options = list(rankdir = "TB", layout = "dot"),
                                     edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_attmen_model3, "~/recherche/DOMINOS/results/plot_cfa_attmen_model3.png", width = 1000, height=500)
 
 # analyse epxloratoire
 vars_ATTMEN <- data_complete[, c("ATTMENACE.Gestion." , "ATTMENACE.Inq." ,"ATTMENACE.CC." , "ATTMENACE.Defo." , "ATTMENACE.Nopt." , "ATTMENACE.Sante." )]
 library(psych)
 fa.parallel(vars_ATTMEN, fm = "ml", fa = "fa")
 efa_ATTMEN <- fa(vars_ATTMEN, nfactors = 3, rotate = "oblimin", fm = "ml")
 print(efa_ATTMEN, cut = 0.3, sort = TRUE)
 
 
 #### -----------------------------####
 ####               SEM            ####
 #### -----------------------------####
 
  ### bois construction uniquement
 
sem_bc_test <- '
    # measurement model
  ATTENVP =~ ATTENV.P1. + ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R.+ ATTENV.P3.+ATTENV.P3R. + ATTENV.P6. + ATTENV.P6R. + ATTENV.P8. + ATTENV.P8R. + ATTENV.P12. + ATTENV.P12R. + ATTENV.P11.+ATTENV.P11R.
  ATTENVU =~ ATTENV.U4. + ATTENV.U4R. + ATTENV.U5. + ATTENV.U5R. + ATTENV.U7. + ATTENV.U7R. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10. + ATTENV.U10R.
  ATTFOP =~ ATTFO.P1. + ATTFO.P1R. + ATTFO.P2. + ATTFO.P2R. + ATTFO.P6. + ATTFO.P6R. + ATTFO.P8. + ATTFO.P8R. + ATTFO.P3. + ATTFO.P3R.
  ATTFOU =~ ATTFO.U4. + ATTFO.U4R. + ATTFO.U5. + ATTFO.U5R. + ATTFO.U7. + ATTFO.U7R. + ATTFO.U9. + ATTFO.U9R. + ATTFO.U10. + ATTFO.U10R.
  ATTBC_POS =~  ATTBC.Ecomen. + ATTBC.Ecoloc. + ATTBC.Tech.+ ATTBC.BienEtre.  + ATTBC.Durable.+ ATTBC.Nat.+ ATTBC.Onehealth.
  ATTBC_NEG =~ ATTBC.BienEtreR. + ATTBC.NatR.+ ATTBC.OnehealthR.  + ATTBC.DurableR.  + ATTBC.EcomenR. + + ATTBC.EcolocR.
  ATTMEN_OPTA =~ ATTMENACE.GestionR. + ATTMENACE.NoptR.  +  + ATTMENACE.SanteR. 
  ATTMEN_OPTF=~  ATTMENACE.CCR.  +   ATTMENACE.DefoR. + ATTMENACE.InqR. 
  ATTMEN_PESA=~ ATTMENACE.Nopt. + ATTMENACE.Gestion. + ATTMENACE.Sante.+ ATTMENACE.Inq. + ATTMENACE.Defo. + ATTMENACE.CC.  
  
    # regression 
  ATTBC_POS ~ ATTMEN_OPTA + ATTMEN_OPTF + ATTMEN_PESA + ATTFOU  + ATTFOP+ ATTENVU  + ATTENVP
  ATTBC_NEG ~ ATTMEN_OPTA + ATTMEN_OPTF + ATTMEN_PESA + ATTFOU  + ATTFOP+ ATTENVU  + ATTENVP
 '
 fit_sem_bc_test<- cfa(sem_bc_test, data=data_complete, std.lv=TRUE) 
 lavInspect(fit_sem_bc_test, "cov.lv")
 
 summary(fit_sem_bc_test, standardized=TRUE, fit.measures=TRUE)
 