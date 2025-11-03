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
table(data_complete$ATTFO.P1.)
table(data_complete$ATTFO.P1R.)

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

#data_complete <- data_complete %>%
 # mutate(across(starts_with("ATT"), as.numeric))

### remove not valid answer

# validity question (Si vous lisez cette affirmation, cochez "tout à fait d'accord")
table(data_complete$ATTFO.test.) # remove resp. answering 1,2,3,4

data_complete<-data_complete[data_complete$ATTFO.test.==5,] # 4702 obs.



#############################################################################
# ------------------------ descriptive stats -----------------------------  #
#############################################################################

library(tidyr)
library(dplyr)
library(ggplot2)

#### test de normalité
env_vars <- grep("^ATTENV", names(data_complete), value = TRUE)
data_long <- data_complete %>%
  pivot_longer(cols = all_of(env_vars), 
               names_to = "variable", 
               values_to = "value")
ggplot(data_long, aes(x = value)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_wrap(~variable, scales = "free_x") + 
  ylab("Proportion") +
  theme_minimal() 

fo_vars <- grep("^ATTFO", names(data_complete), value = TRUE)
data_long <- data_complete %>%
  pivot_longer(cols = all_of(fo_vars), 
               names_to = "variable", 
               values_to = "value")
ggplot(data_long, aes(x = value)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_wrap(~variable, scales = "free_x") + 
  ylab("Proportion") +
  theme_minimal() 

bc_vars <- grep("^ATTBC", names(data_complete), value = TRUE)
data_long <- data_complete %>%
  pivot_longer(cols = all_of(bc_vars), 
               names_to = "variable", 
               values_to = "value")
ggplot(data_long, aes(x = value)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_wrap(~variable, scales = "free_x") + 
  ylab("Proportion") +
  theme_minimal() 

be_vars <- grep("^ATTBE", names(data_complete), value = TRUE)
data_long <- data_complete %>%
  pivot_longer(cols = all_of(be_vars), 
               names_to = "variable", 
               values_to = "value")
ggplot(data_long, aes(x = value)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  facet_wrap(~variable, scales = "free_x") + 
  ylab("Proportion") +
  theme_minimal() 


library(psych)
describe(data_complete$ATTENV.P1.) # Mean = 4.32, Median = 4 → slight positive skew.
#Skew = -1.47 → actually negative skew (longer tail to the left).
#Kurtosis = 2.51 → slightly platykurtic (flatter than normal).

#### ALPHA

env_vars <- grep("^ATTENV", names(data_complete), value = TRUE)
env_data <- data_complete[, env_vars]
env_data <- data.frame(lapply(env_data, function(x) as.numeric(as.character(x))))
psych::alpha(env_data) # TR7S FAIBLE
envu_vars <- grep("^ATTENV.U", names(data_complete), value = TRUE)
envu_data <- data_complete[, envu_vars]
envu_data <- data.frame(lapply(envu_data, function(x) as.numeric(as.character(x))))
psych::alpha(envu_data) # ENTRE 0.5 ET 0.7: acceptable
envp_vars <- grep("^ATTENV.P", names(data_complete), value = TRUE)
envp_data <- data_complete[, envp_vars]
envp_data <- data.frame(lapply(envp_data, function(x) as.numeric(as.character(x))))
psych::alpha(envp_data) # ENTRE 0.5 ET 0.7: acceptable

fo_vars <- grep("^ATTFO", names(data_complete), value = TRUE)
fo_data <- data_complete[, fo_vars]
fo_data <- data.frame(lapply(fo_data, function(x) as.numeric(as.character(x))))
psych::alpha(fo_data) # TR7S FAIBLE
fou_vars <- grep("^ATTFO.U", names(data_complete), value = TRUE)
fou_data <- data_complete[, fou_vars]
fou_data <- data.frame(lapply(fou_data, function(x) as.numeric(as.character(x))))
psych::alpha(fou_data) # ENTRE 0.5 ET 0.7: acceptable
fop_vars <- grep("^ATTFO.P", names(data_complete), value = TRUE)
fop_data <- data_complete[, fop_vars]
fop_data <- data.frame(lapply(fop_data, function(x) as.numeric(as.character(x))))
psych::alpha(fop_data) # ENTRE 0.5 ET 0.7: acceptable

be_vars <- grep("^ATTBE", names(data_complete), value = TRUE)
be_data <- data_complete[, be_vars]
be_data <- data.frame(lapply(be_data, function(x) as.numeric(as.character(x))))
psych::alpha(be_data) # ENTRE 0.5 ET 0.7: acceptable

bc_vars <- grep("^ATTBC", names(data_complete), value = TRUE)
bc_data <- data_complete[, bc_vars]
bc_data <- data.frame(lapply(bc_data, function(x) as.numeric(as.character(x))))
psych::alpha(bc_data, check.keys=TRUE) # ENTRE 0.5 ET 0.7: acceptable

## test des corrélations 
cor_matrix <- cor(env_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

cor_matrix <- cor(fo_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

cor_matrix <- cor(be_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

cor_matrix <- cor(bc_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

### cohérence entre reversed and positively worded attitudes
# 

pos_cols <- grep("^ATTENV\\.P.*[^R]\\.$", names(env_data), value = TRUE)
rev_cols <- grep("^ATTENV\\.P.*R\\.$", names(env_data), value = TRUE)
wilcox_pair <- function(pos, rev) {
  test <- wilcox.test(env_data[[pos]], env_data[[rev]], paired = TRUE, exact = FALSE, correct = TRUE)
  tibble(
    item_pos = pos,
    item_rev = rev,
    V = test$statistic,
    p_value = test$p.value
  )
}
results <- map2_df(pos_cols, rev_cols, wilcox_pair)

results

pos_cols <- grep("^ATTFO\\.P.*[^R]\\.$", names(fo_data), value = TRUE)
rev_cols <- grep("^ATTFO\\.P.*R\\.$", names(fo_data), value = TRUE)
wilcox_pair <- function(pos, rev) {
  test <- wilcox.test(fo_data[[pos]], fo_data[[rev]], paired = TRUE, exact = FALSE, correct = TRUE)
  tibble(
    item_pos = pos,
    item_rev = rev,
    V = test$statistic,
    p_value = test$p.value
  )
}
results <- map2_df(pos_cols, rev_cols, wilcox_pair)
results

pos_cols <- grep("^ATTBC\\..*[^R]\\.$", names(data_complete), value = TRUE)
rev_cols <- grep("^ATTBC\\..*R\\.$", names(data_complete), value = TRUE)

wilcox_pair <- function(pos, rev) {
  test <- wilcox.test(bc_data[[pos]], bc_data[[rev]], paired = TRUE, exact = FALSE, correct = TRUE)
  tibble(
    item_pos = pos,
    item_rev = rev,
    V = test$statistic,
    p_value = test$p.value
  )
}
results <- map2_df(pos_cols, rev_cols, wilcox_pair)
results



#### --------------------------------------------------------------------####
####               confirmatory factor analysis                          ####
#### --------------------------------------------------------------------####

#### ENVIRONMENTAL ATTITUDES ####

# modèle à 2 facteurs
cfa_attenv_1 <- '
  ATTENVP =~ ATTENV.P1. + ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R.+ ATTENV.P3.+ATTENV.P3R. + ATTENV.P6. + ATTENV.P6R. + ATTENV.P8. + ATTENV.P8R. + ATTENV.P12. + ATTENV.P12R. + ATTENV.P11.+ATTENV.P11R.
  ATTENVU =~ ATTENV.U4. + ATTENV.U4R. + ATTENV.U5. + ATTENV.U5R. + ATTENV.U7. + ATTENV.U7R. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10. + ATTENV.U10R.
  ATTENVP ~~ ATTENVU
'
fit_cfa_attenv_1 <- cfa(cfa_attenv_1, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv_1, fit.measures=TRUE, standardized=TRUE)


plot_cfa_attenv_1<-lavaanPlot(  model = fit_cfa_attenv_1,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                             #labels = node_names,   # <- use labels instead of node_names
                             graph_options = list(rankdir = "TB", layout = "dot"),
                             edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attenv_1, "~/recherche/DOMINOS/results/plot_cfa_attenv_1.png", width = 1500, height=300)

# modèle à 1 facteur (pour version 12 items)
cfa_attenv_2 <- '
  ATTENV =~ ATTENV.P1. + ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R.+ ATTENV.P3.+ATTENV.P3R. + ATTENV.P6. + ATTENV.P6R. + ATTENV.P8. + ATTENV.P8R. + ATTENV.P12. + ATTENV.P12R. + ATTENV.P11.+ATTENV.P11R.+ ATTENV.U4. + ATTENV.U4R. + ATTENV.U5. + ATTENV.U5R. + ATTENV.U7. + ATTENV.U7R. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10. + ATTENV.U10R.
'
fit_cfa_attenv_2 <- cfa(cfa_attenv_2, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv_2, fit.measures=TRUE, standardized=TRUE)
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


# modèle à 2 facteurs avec corrélations erreurs # enlever u5 AU LIEUDE P3
cfa_attenv_6 <- '
  ATTENVP =~ ATTENV.P1. + ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R. + ATTENV.P6. + ATTENV.P6R. + ATTENV.P8. + ATTENV.P8R.  + ATTENV.P11.+ATTENV.P11R.+ ATTENV.P3.+ATTENV.P3R.
  ATTENVU =~ ATTENV.U4. + ATTENV.U4R. + ATTENV.U7. + ATTENV.U7R. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10. + ATTENV.U10R.
  ATTENVP ~~ ATTENVU
    ATTENV.P1. ~~ ATTENV.P1R.
  ATTENV.P2. ~~ ATTENV.P2R.
  ATTENV.P6. ~~ ATTENV.P6R.
    ATTENV.P3. ~~ ATTENV.P3R.
  ATTENV.P8. ~~ ATTENV.P8R.
  ATTENV.P11. ~~ ATTENV.P11R.
  ATTENV.U4. ~~ ATTENV.U4R. 
  ATTENV.U7.  ~~ ATTENV.U7R. 
  ATTENV.U9. ~~ ATTENV.U9R. 
  ATTENV.U10.  ~~ ATTENV.U10R.
  
'
fit_cfa_attenv_6<- cfa(cfa_attenv_6, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv_6, fit.measures=TRUE, standardized=TRUE)
plot_cfa_attenv_6<-lavaanPlot(  model = fit_cfa_attenv_6,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                                #labels = node_names,   # <- use labels instead of node_names
                                graph_options = list(rankdir = "TB", layout = "dot"),
                                edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attenv_6, "~/recherche/DOMINOS/results/plot_cfa_attenv_6.png", width = 1500, height=300)


# modèle à 2 facteurs avec corrélations erreurs # enlever u5 et p3
cfa_attenv_7 <- '
  ATTENVP =~ ATTENV.P1. + ATTENV.P1R. + ATTENV.P2. + ATTENV.P2R. + ATTENV.P6. + ATTENV.P6R. + ATTENV.P8. + ATTENV.P8R.  + ATTENV.P11.+ATTENV.P11R.
  ATTENVU =~ ATTENV.U4. + ATTENV.U4R. + ATTENV.U7. + ATTENV.U7R. + ATTENV.U9. + ATTENV.U9R. + ATTENV.U10. + ATTENV.U10R.
  ATTENVP ~~ ATTENVU
    ATTENV.P1. ~~ ATTENV.P1R.
  ATTENV.P2. ~~ ATTENV.P2R.
  ATTENV.P6. ~~ ATTENV.P6R.
    ATTENV.P3. ~~ ATTENV.P3R.
  ATTENV.P8. ~~ ATTENV.P8R.
  ATTENV.P11. ~~ ATTENV.P11R.
  ATTENV.U4. ~~ ATTENV.U4R. 
  ATTENV.U7.  ~~ ATTENV.U7R. 
  ATTENV.U9. ~~ ATTENV.U9R. 
  ATTENV.U10.  ~~ ATTENV.U10R.
  
'
fit_cfa_attenv_7<- cfa(cfa_attenv_7, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv_7, fit.measures=TRUE, standardized=TRUE)
plot_cfa_attenv_7<-lavaanPlot(  model = fit_cfa_attenv_7,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                                #labels = node_names,   # <- use labels instead of node_names
                                graph_options = list(rankdir = "TB", layout = "dot"),
                                edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attenv_7, "~/recherche/DOMINOS/results/plot_cfa_attenv_7.png", width = 1500, height=300)


### critiques liés aux reversed items

cfa_attenv_8 <- '
  ATTENVP =~ ATTENV.P1.  + ATTENV.P2.  + ATTENV.P6.  + ATTENV.P8.   + ATTENV.P11.
  ATTENVU =~ ATTENV.U4.  + ATTENV.U5.  + ATTENV.U7.  + ATTENV.U9.  + ATTENV.U10. 
  ATTENVP ~~ ATTENVU

'
fit_cfa_attenv_8<- cfa(cfa_attenv_8, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv_8, fit.measures=TRUE, standardized=TRUE)
plot_cfa_attenv_8<-lavaanPlot(  model = fit_cfa_attenv_8,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                                #labels = node_names,   # <- use labels instead of node_names
                                graph_options = list(rankdir = "TB", layout = "dot"),
                                edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attenv_8, "~/recherche/DOMINOS/results/plot_cfa_attenv_8.png", width = 1500, height=300)

### critiques liés aux reversed items

cfa_attenv_9 <- '
  ATTENVP =~ ATTENV.P1.  + ATTENV.P2. +ATTENV.P3.  + ATTENV.P6.  + ATTENV.P8.   + ATTENV.P11. + ATTENV.P12.
  ATTENVU =~ ATTENV.U4.  + ATTENV.U5.  + ATTENV.U7.  + ATTENV.U9.  + ATTENV.U10. 
  ATTENVP ~~ ATTENVU

'
fit_cfa_attenv_9<- cfa(cfa_attenv_9, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv_9, fit.measures=TRUE, standardized=TRUE)
plot_cfa_attenv_9<-lavaanPlot(  model = fit_cfa_attenv_9,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                                #labels = node_names,   # <- use labels instead of node_names
                                graph_options = list(rankdir = "TB", layout = "dot"),
                                edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attenv_9, "~/recherche/DOMINOS/results/plot_cfa_attenv_9.png", width = 1500, height=300)


### critiques liés aux reversed items

cfa_attenv_10 <- '
  ATTENVP =~ ATTENV.P1.  + ATTENV.P2. +ATTENV.P3.  + ATTENV.P6.  + ATTENV.P8.   + ATTENV.P11.
  ATTENVU =~ ATTENV.U4.  + ATTENV.U5.  + ATTENV.U7.  + ATTENV.U9.  + ATTENV.U10. 
  ATTENVP ~~ ATTENVU

'
fit_cfa_attenv_10<- cfa(cfa_attenv_10, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attenv_10, fit.measures=TRUE, standardized=TRUE)
plot_cfa_attenv_10<-lavaanPlot(  model = fit_cfa_attenv_10,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                                #labels = node_names,   # <- use labels instead of node_names
                                graph_options = list(rankdir = "TB", layout = "dot"),
                                edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attenv_10, "~/recherche/DOMINOS/results/plot_cfa_attenv_10.png", width = 1500, height=300)

comp_fit_env <- rbind(
  Model_1 = fitMeasures(fit_cfa_attenv_1, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_2 = fitMeasures(fit_cfa_attenv_2, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_3 = fitMeasures(fit_cfa_attenv_3, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_4 = fitMeasures(fit_cfa_attenv_4, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_5 = fitMeasures(fit_cfa_attenv_5, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_6 = fitMeasures(fit_cfa_attenv_6, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_7 = fitMeasures(fit_cfa_attenv_7, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_8 = fitMeasures(fit_cfa_attenv_8, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_9 = fitMeasures(fit_cfa_attenv_9, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_10 = fitMeasures(fit_cfa_attenv_10, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr"))
  
)   


 #### FOREST ATTITUDES ####
 
# modèle à 2 facteurs
cfa_attfo_1 <- '
  ATTFOP =~ ATTFO.P1. + ATTFO.P1R. + ATTFO.P2. + ATTFO.P2R.+ ATTFO.P3.+ATTFO.P3R. + ATTFO.P6. + ATTFO.P6R. + ATTFO.P8. + ATTFO.P8R. 
  ATTFOU =~ ATTFO.U4. + ATTFO.U4R. + ATTFO.U5. + ATTFO.U5R. + ATTFO.U7. + ATTFO.U7R. + ATTFO.U9. + ATTFO.U9R. + ATTFO.U10. + ATTFO.U10R.
  ATTFOP ~~ ATTFOU
'
fit_cfa_attfo_1 <- cfa(cfa_attfo_1, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_1, fit.measures=TRUE, standardized=TRUE)
plot_cfa_attfo_1<-lavaanPlot( model = fit_cfa_attfo_1,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                              #labels = node_names,   # <- use labels instead of node_names
                              graph_options = list(rankdir = "TB", layout = "dot"),
                              edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attfo_1,"~/recherche/DOMINOS/results/plot_cfa_attfo_1.png", width = 1500, height=300)

# modèle à 1 facteurs
cfa_attfo_2 <- '
  ATTFO_2 =~ ATTFO.P1. + ATTFO.P1R. + ATTFO.P2. + ATTFO.P2R.+ ATTFO.P3.+ATTFO.P3R. + ATTFO.P6. + ATTFO.P6R. + ATTFO.P8. + ATTFO.P8R. + ATTFO.U4. + ATTFO.U4R. + ATTFO.U5. + ATTFO.U5R. + ATTFO.U7. + ATTFO.U7R. + ATTFO.U9. + ATTFO.U9R. + ATTFO.U10. + ATTFO.U10R.
'
fit_cfa_attfo_2 <- cfa(cfa_attfo_2, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_2, fit.measures=TRUE, standardized=TRUE)

plot_cfa_attfo_2<-lavaanPlot(  model = fit_cfa_attfo_2,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                             #labels = node_names,   # <- use labels instead of node_names
                             graph_options = list(rankdir = "TB", layout = "dot"),
                             edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attfo_2, "~/recherche/DOMINOS/results/plot_cfa_attfo_2.png", width = 1500, height=300)
 

# deux facteurs, correlation errur
cfa_attfo_3 <- '
  ATTFOP =~ ATTFO.P1. + ATTFO.P1R. + ATTFO.P2. + ATTFO.P2R.+ ATTFO.P3.+ATTFO.P3R. + ATTFO.P6. + ATTFO.P6R. + ATTFO.P8. + ATTFO.P8R. 
  ATTFOU =~ ATTFO.U4. + ATTFO.U4R.+ ATTFO.U5. + ATTFO.U5R. + ATTFO.U7. + ATTFO.U7R. + ATTFO.U9. + ATTFO.U9R. + ATTFO.U10. + ATTFO.U10R.
  ATTFOP ~~ ATTFOU
  ATTFO.P1. ~~ ATTFO.P1R.
  ATTFO.P2. ~~ ATTFO.P2R.
  ATTFO.P3.~~ ATTFO.P3R. 
  ATTFO.P6. ~~ ATTFO.P6R. 
  ATTFO.P8. ~~ ATTFO.P8R.
  ATTFO.U4. ~~ ATTFO.U4R.
  ATTFO.U5. ~~ ATTFO.U5R. 
  ATTFO.U7. ~~ ATTFO.U7R. 
  ATTFO.U9. ~~ ATTFO.U9R. 
  ATTFO.U10. ~~ ATTFO.U10R.
'
fit_cfa_attfo_3 <- cfa(cfa_attfo_3, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_3, fit.measures=TRUE, standardized=TRUE)


plot_cfa_attfo_3<-lavaanPlot(  model = fit_cfa_attfo_3,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                             #labels = node_names,   # <- use labels instead of node_names
                             graph_options = list(rankdir = "TB", layout = "dot"),
                             edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attfo_3, "~/recherche/DOMINOS/results/plot_cfa_attfo_3.png", width = 1500, height=300)

# deux facteurs, correlation erreur, enelver U5
cfa_attfo_4 <- '
  ATTFOP =~ ATTFO.P1. + ATTFO.P1R. + ATTFO.P2. + ATTFO.P2R.+ ATTFO.P3.+ATTFO.P3R. + ATTFO.P6. + ATTFO.P6R. + ATTFO.P8. + ATTFO.P8R. 
  ATTFOU =~ ATTFO.U4. + ATTFO.U4R. + ATTFO.U7. + ATTFO.U7R. + ATTFO.U9. + ATTFO.U9R. + ATTFO.U10. + ATTFO.U10R.
  ATTFOP ~~ ATTFOU
  ATTFO.P1. ~~ ATTFO.P1R.
  ATTFO.P2. ~~ ATTFO.P2R.
  ATTFO.P3.~~ ATTFO.P3R. 
  ATTFO.P6. ~~ ATTFO.P6R. 
  ATTFO.P8. ~~ ATTFO.P8R.
  ATTFO.U4. ~~ ATTFO.U4R.
  ATTFO.U7. ~~ ATTFO.U7R. 
  ATTFO.U9. ~~ ATTFO.U9R. 
  ATTFO.U10. ~~ ATTFO.U10R.
'
fit_cfa_attfo_4 <- cfa(cfa_attfo_4, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_4, fit.measures=TRUE, standardized=TRUE)


plot_cfa_attfo_4<-lavaanPlot(  model = fit_cfa_attfo_4,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                               #labels = node_names,   # <- use labels instead of node_names
                               graph_options = list(rankdir = "TB", layout = "dot"),
                               edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attfo_4, "~/recherche/DOMINOS/results/plot_cfa_attfo_4.png", width = 1500, height=300)

# deux facteurs, correlation erreur, enelver P3 et U5
cfa_attfo_5 <- '
  ATTFOP =~ ATTFO.P1. + ATTFO.P1R. + ATTFO.P2. + ATTFO.P2R.+ ATTFO.P6. + ATTFO.P6R. + ATTFO.P8. + ATTFO.P8R. 
  ATTFOU =~ ATTFO.U4. + ATTFO.U4R. + ATTFO.U7. + ATTFO.U7R. + ATTFO.U9. + ATTFO.U9R. + ATTFO.U10. + ATTFO.U10R.
  ATTFOP ~~ ATTFOU
  ATTFO.P1. ~~ ATTFO.P1R.
  ATTFO.P2. ~~ ATTFO.P2R.
  ATTFO.P6. ~~ ATTFO.P6R. 
  ATTFO.P8. ~~ ATTFO.P8R.
  ATTFO.U4. ~~ ATTFO.U4R.
  ATTFO.U7. ~~ ATTFO.U7R. 
  ATTFO.U9. ~~ ATTFO.U9R. 
  ATTFO.U10. ~~ ATTFO.U10R.
'
fit_cfa_attfo_5 <- cfa(cfa_attfo_5, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_5, fit.measures=TRUE, standardized=TRUE)


plot_cfa_attfo_5<-lavaanPlot(  model = fit_cfa_attfo_5,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                               #labels = node_names,   # <- use labels instead of node_names
                               graph_options = list(rankdir = "TB", layout = "dot"),
                               edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attfo_5, "~/recherche/DOMINOS/results/plot_cfa_attfo_5.png", width = 1500, height=300)

# deux facteurs, correlation erreur, enelver P3 mais pas U5 
cfa_attfo_6 <- '
  ATTFOP =~ ATTFO.P1. + ATTFO.P1R. + ATTFO.P2. + ATTFO.P2R.+ ATTFO.P6. + ATTFO.P6R. + ATTFO.P8. + ATTFO.P8R. 
  ATTFOU =~ ATTFO.U4. + ATTFO.U4R.+ATTFO.U5. + ATTFO.U5R. + ATTFO.U7. + ATTFO.U7R. + ATTFO.U9. + ATTFO.U9R. + ATTFO.U10. + ATTFO.U10R.
  ATTFOP ~~ ATTFOU
  ATTFO.P1. ~~ ATTFO.P1R.
  ATTFO.P2. ~~ ATTFO.P2R.
  ATTFO.P6. ~~ ATTFO.P6R. 
  ATTFO.P8. ~~ ATTFO.P8R.
   ATTFO.U5. ~~ ATTFO.U5R.
  ATTFO.U4. ~~ ATTFO.U4R.
  ATTFO.U7. ~~ ATTFO.U7R. 
  ATTFO.U9. ~~ ATTFO.U9R. 
  ATTFO.U10. ~~ ATTFO.U10R.
'
fit_cfa_attfo_6 <- cfa(cfa_attfo_6, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_6, fit.measures=TRUE, standardized=TRUE)


plot_cfa_attfo_6<-lavaanPlot(  model = fit_cfa_attfo_6,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                               #labels = node_names,   # <- use labels instead of node_names
                               graph_options = list(rankdir = "TB", layout = "dot"),
                               edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attfo_6, "~/recherche/DOMINOS/results/plot_cfa_attfo_6.png", width = 1500, height=300)


### model 6 sans items reversed
cfa_attfo_7 <- '
  ATTFOP =~ ATTFO.P1.  + ATTFO.P2. + ATTFO.P6.  + ATTFO.P8.  
  ATTFOU =~ ATTFO.U4. +ATTFO.U5.  + ATTFO.U7.  + ATTFO.U9.  + ATTFO.U10. 
  ATTFOP ~~ ATTFOU

'
fit_cfa_attfo_7 <- cfa(cfa_attfo_7, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_7, fit.measures=TRUE, standardized=TRUE)


plot_cfa_attfo_7<-lavaanPlot(  model = fit_cfa_attfo_7,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                               #labels = node_names,   # <- use labels instead of node_names
                               graph_options = list(rankdir = "TB", layout = "dot"),
                               edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attfo_7, "~/recherche/DOMINOS/results/plot_cfa_attfo_7.png", width = 1500, height=300)

### model 5 sans items reversed

cfa_attfo_8 <- '
  ATTFOP =~ ATTFO.P1.  + ATTFO.P2. + ATTFO.P6.  + ATTFO.P8.  
  ATTFOU =~ ATTFO.U4.  + ATTFO.U7.  + ATTFO.U9.  + ATTFO.U10. 
  ATTFOP ~~ ATTFOU

'
fit_cfa_attfo_8 <- cfa(cfa_attfo_8, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_8, fit.measures=TRUE, standardized=TRUE)


plot_cfa_attfo_8<-lavaanPlot(  model = fit_cfa_attfo_8,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                               #labels = node_names,   # <- use labels instead of node_names
                               graph_options = list(rankdir = "TB", layout = "dot"),
                               edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attfo_8, "~/recherche/DOMINOS/results/plot_cfa_attfo_8.png", width = 1500, height=300)

# model 4 sans item reversed
cfa_attfo_9 <- '
  ATTFOP =~ ATTFO.P1.  + ATTFO.P2. + ATTFO.P3. + ATTFO.P6.  + ATTFO.P8.  
  ATTFOU =~ ATTFO.U4.  + ATTFO.U7.  + ATTFO.U9.  + ATTFO.U10. 
  ATTFOP ~~ ATTFOU

'
fit_cfa_attfo_9 <- cfa(cfa_attfo_9, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_9, fit.measures=TRUE, standardized=TRUE)


plot_cfa_attfo_9<-lavaanPlot(  model = fit_cfa_attfo_9,  coefs = TRUE, sig = 0.05, covs = TRUE,stars = c("latent","covs"),
                               #labels = node_names,   # <- use labels instead of node_names
                               graph_options = list(rankdir = "TB", layout = "dot"),
                               edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
)
save_png(plot_cfa_attfo_9, "~/recherche/DOMINOS/results/plot_cfa_attfo_9.png", width = 1500, height=300)

# même que model 1 sans item reversed
cfa_attfo_10 <- '
  ATTFOP =~ ATTFO.P1.  + ATTFO.P2. + ATTFO.P3. + ATTFO.P6.  + ATTFO.P8.  
  ATTFOU =~ ATTFO.U4.  + ATTFO.U5.  + ATTFO.U7.  + ATTFO.U9.  + ATTFO.U10. 
  ATTFOP ~~ ATTFOU
'
fit_cfa_attfo_10 <- cfa(cfa_attfo_10, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_10, fit.measures=TRUE, standardized=TRUE)

comp_fit_fo <- rbind(
  Model_1 = fitMeasures(fit_cfa_attfo_1, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_2 = fitMeasures(fit_cfa_attfo_2, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_3 = fitMeasures(fit_cfa_attfo_3, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_4 = fitMeasures(fit_cfa_attfo_4, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_5 = fitMeasures(fit_cfa_attfo_5, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_6 = fitMeasures(fit_cfa_attfo_6, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_7 = fitMeasures(fit_cfa_attfo_7, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_8 = fitMeasures(fit_cfa_attfo_8, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_9 = fitMeasures(fit_cfa_attfo_9, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
  Model_10 = fitMeasures(fit_cfa_attfo_10, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr"))
  
) 




modif<-modificationindices(fit_cfa_attfo_6)

# pas mal de U load en P -> ce qui concerne les services ES mais pas l'exploitation -> non
# test d'un autre modèle où l'on sépare exploitation bois/exploitation autre SE/préservation sous cloche

cfa_attfo_5 <- '
  ATTFOP =~   ATTFO.P2. + ATTFO.P2R.+ ATTFO.P6. + ATTFO.P6R. + ATTFO.P8. + ATTFO.P8R. 
  ATTFOU =~ ATTFO.U7. + ATTFO.U7R. + ATTFO.U9. + ATTFO.U9R. + ATTFO.U10. + ATTFO.U10R.
  ATTFOO =~ ATTFO.U4. + ATTFO.U4R.+ ATTFO.P1. + ATTFO.P1R.
  ATTFOP ~~ ATTFOU
  ATTFOP ~~ ATTFOO
  ATTFOU ~~ ATTFOO
  ATTFO.P1. ~~ ATTFO.P1R.
  ATTFO.P2. ~~ ATTFO.P2R.
  ATTFO.P6. ~~ ATTFO.P6R. 
  ATTFO.P8. ~~ ATTFO.P8R.
  ATTFO.U4. ~~ ATTFO.U4R.
  ATTFO.U7. ~~ ATTFO.U7R. 
  ATTFO.U9. ~~ ATTFO.U9R. 
  ATTFO.U10. ~~ ATTFO.U10R.
'
fit_cfa_attfo_5 <- cfa(cfa_attfo_5, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_5, fit.measures=TRUE, standardized=TRUE)

# mettre P1 en dimension U -> non

cfa_attfo_6 <- '
  ATTFOP =~   ATTFO.P2. + ATTFO.P2R.+ ATTFO.P6. + ATTFO.P6R. + ATTFO.P8. + ATTFO.P8R. 
  ATTFOU =~ ATTFO.U7. + ATTFO.U7R. + ATTFO.U9. + ATTFO.U9R. + ATTFO.U10. + ATTFO.U10R.+ ATTFO.P1. + ATTFO.P1R.+ ATTFO.U4. + ATTFO.U4R.
  ATTFOP ~~ ATTFOU
  ATTFO.P1. ~~ ATTFO.P1R.
  ATTFO.P2. ~~ ATTFO.P2R.
  ATTFO.P6. ~~ ATTFO.P6R. 
  ATTFO.P8. ~~ ATTFO.P8R.
  ATTFO.U4. ~~ ATTFO.U4R.
  ATTFO.U7. ~~ ATTFO.U7R. 
  ATTFO.U9. ~~ ATTFO.U9R. 
  ATTFO.U10. ~~ ATTFO.U10R.
'
fit_cfa_attfo_6 <- cfa(cfa_attfo_6, data=data_complete, std.lv=TRUE)
summary(fit_cfa_attfo_6, fit.measures=TRUE, standardized=TRUE)

comp_fit_fo <- rbind(
  Model = fitMeasures(fit_cfa_attfo, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr")),
  Model_2 = fitMeasures(fit_cfa_attfo_1, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr")),
  Model_3 = fitMeasures(fit_cfa_attfo_2, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr")),
  Model_4 = fitMeasures(fit_cfa_attfo_3, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr")),
  Model_5 = fitMeasures(fit_cfa_attfo_4, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr")),
  Model_6 = fitMeasures(fit_cfa_attfo_5, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr")),
  Model_7 = fitMeasures(fit_cfa_attfo_6, c("chisq","pvalue","cfi","tli","rmsea","aic","bic","srmr"))
) # le modèle 5 est le meilleur





 #### WOOD ATTITUDES ####

 ### Construction

 # model1
 cfa_ATTBC_model1 <- '
  ATTBC_ECO=~  ATTBC.EcomenR. + ATTBC.Ecomen. + ATTBC.Ecoloc. + ATTBC.EcolocR. 
  ATTBC_CONF=~  ATTBC.BienEtreR. + ATTBC.BienEtre. + ATTBC.TechR. + ATTBC.Tech. + ATTBC.NatR. + ATTBC.Nat.
  ATTBC_BIO =~ ATTBC.OnehealthR. + ATTBC.Onehealth. + ATTBC.DurableR. + ATTBC.Durable. 
  ATTBC_ECO ~~ ATTBC_CONF
  ATTBC_CONF ~~ ATTBC_BIO
  ATTBC_BIO ~~ ATTBC_ECO
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
 
 # model 2 mais sans techR
 cfa_ATTBC_model3 <- '
  ATTBC_TECH =~  ATTBC.EcomenR. + ATTBC.Ecomen. + ATTBC.Ecoloc. + ATTBC.EcolocR. + ATTBC.Tech.
  ATTBC_CONF =~ ATTBC.BienEtreR. + ATTBC.BienEtre. + ATTBC.NatR. + ATTBC.Nat.
  ATTBC_BIO =~ ATTBC.OnehealthR. + ATTBC.Onehealth. + ATTBC.DurableR. + ATTBC.Durable. 
  ATTBC_TECH~~ATTBC_CONF
  ATTBC_CONF~~ ATTBC_BIO
  ATTBC_BIO ~~ATTBC_TECH
'
 fit_cfa_ATTBC_model3 <- cfa(cfa_ATTBC_model3, data = data_complete, std.lv = TRUE)
 lavInspect(cfa_ATTBC_model3, "cov.lv") 
 summary(fit_cfa_ATTBC_model3, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBC_model3<-lavaanPlot(model = fit_cfa_ATTBC_model3, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model3, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model3.png", width = 1500, height=400)
 
 
 # model 4
 cfa_ATTBC_model4 <- '
  ATTBC_U=~    ATTBC.Ecomen. + ATTBC.Ecoloc.  + ATTBC.BienEtre.   + ATTBC.Tech.
  ATTBC_P =~  + ATTBC.Nat. + ATTBC.Durable.+ ATTBC.Onehealth.   
ATTBC_U~~ATTBC_P
'
 fit_cfa_ATTBC_model4 <- cfa(cfa_ATTBC_model4, data = data_complete, std.lv = TRUE)
 summary(fit_cfa_ATTBC_model4, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBC_model4<-lavaanPlot(model = fit_cfa_ATTBC_model4, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model4, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model4.png", width = 1500, height=400)
 
 
 # model 5 
 cfa_ATTBC_model5 <- '
  ATTBC=~    ATTBC.Ecomen. + ATTBC.Ecoloc.  + ATTBC.BienEtre.   + ATTBC.Tech. + ATTBC.Nat. + ATTBC.Durable.+ ATTBC.Onehealth.   
'
 fit_cfa_ATTBC_model5 <- cfa(cfa_ATTBC_model5, data = data_complete, std.lv = TRUE)
 summary(fit_cfa_ATTBC_model5, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBC_model5<-lavaanPlot(model = fit_cfa_ATTBC_model5, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model5, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model4.png", width = 1500, height=400)
 
 # model 6 # techR, onehealthR et EcomenR ne loadent pas
 cfa_ATTBC_model6 <- '
  ATTBC=~    ATTBC.Ecomen. +ATTBC.EcomenR. + ATTBC.Ecoloc. + ATTBC.EcolocR.  + ATTBC.BienEtre.  + ATTBC.BienEtreR.
  + ATTBC.Tech. + ATTBC.TechR. + ATTBC.Nat.+ ATTBC.NatR. + ATTBC.Durable.  + ATTBC.DurableR. + ATTBC.Onehealth.  + ATTBC.OnehealthR.  
'
 fit_cfa_ATTBC_model6 <- cfa(cfa_ATTBC_model6, data = data_complete, std.lv = TRUE)
 summary(fit_cfa_ATTBC_model6, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBC_model6<-lavaanPlot(model = fit_cfa_ATTBC_model6, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model6, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model4.png", width = 1500, height=400)
 
 # model 7 # techR, onehealthR et EcomenR ne loadent pas
 cfa_ATTBC_model7 <- '
  ATTBC=~    ATTBC.Ecomen.  + ATTBC.Ecoloc. + ATTBC.EcolocR.  + ATTBC.BienEtre.  + ATTBC.BienEtreR.
  + ATTBC.Tech.  + ATTBC.Nat.+ ATTBC.NatR. + ATTBC.Durable.  + ATTBC.DurableR. + ATTBC.Onehealth.    
'
 fit_cfa_ATTBC_model7 <- cfa(cfa_ATTBC_model7, data = data_complete, std.lv = TRUE)
 summary(fit_cfa_ATTBC_model7, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBC_model7<-lavaanPlot(model = fit_cfa_ATTBC_model7, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model7, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model4.png", width = 1500, height=400)
 
 # model 8
 cfa_ATTBC_model8 <- '
  ATTBC_HUM=~    ATTBC.Ecomen. + ATTBC.Ecoloc.   + ATTBC.Tech. +ATTBC.Durable.+ ATTBC.Onehealth.   
  ATTBC_EGO =~  ATTBC.Nat.  + ATTBC.BienEtre. 

'
 fit_cfa_ATTBC_model8 <- cfa(cfa_ATTBC_model8, data = data_complete, std.lv = TRUE)
 summary(fit_cfa_ATTBC_model8, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBC_model8<-lavaanPlot(model = fit_cfa_ATTBC_model8, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model8, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model8.png", width = 1500, height=400)
 
 # model 9
 cfa_ATTBC_model9 <- '
  ATTBC_HUM=~    ATTBC.Ecoloc. + ATTBC.Durable.+ ATTBC.Onehealth.   +ATTBC.Ecomen. + ATTBC.EcolocR.
  ATTBC_EGO =~  ATTBC.Nat.  + ATTBC.BienEtre. +ATTBC.BienEtreR. 

'
 fit_cfa_ATTBC_model9 <- cfa(cfa_ATTBC_model9, data = data_complete, std.lv = TRUE)
 summary(fit_cfa_ATTBC_model9, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBC_model9<-lavaanPlot(model = fit_cfa_ATTBC_model9, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBC_model9, "~/recherche/DOMINOS/results/plot_cfa_ATTBC_model9.png", width = 1500, height=400)
 
 comp_fit_bc <- rbind(
   Model_1 = fitMeasures(fit_cfa_ATTBC_model1, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_2 = fitMeasures(fit_cfa_ATTBC_model2, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_3 = fitMeasures(fit_cfa_ATTBC_model3, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_4 = fitMeasures(fit_cfa_ATTBC_model4, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_5 = fitMeasures(fit_cfa_ATTBC_model5, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_6 = fitMeasures(fit_cfa_ATTBC_model6, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_7 = fitMeasures(fit_cfa_ATTBC_model7, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_8 = fitMeasures(fit_cfa_ATTBC_model8, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_9 = fitMeasures(fit_cfa_ATTBC_model9, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr"))
 ) 
 
 ### energy
 
 # model1 : on s'inspire de model8 attBC
 cfa_ATTBE_model1 <- '
  ATTBE_EGO =~   ATTBE.BienEtre.   + ATTBE.Nature. 
  ATTBE_HUM =~   ATTBE.Health. + ATTBE.Durable.  + ATTBE.Ecoloc.   + ATTBE.Ecomen.
  ATTBE_EGO~~ATTBE_HUM
  '
 
 fit_cfa_ATTBE_model1 <- cfa(cfa_ATTBE_model1, data=data_complete, std.lv=TRUE)
 summary(fit_cfa_ATTBE_model1, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBE_model1<-lavaanPlot(model = fit_cfa_ATTBE_model6, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBE_model1, "~/recherche/DOMINOS/results/plot_cfa_ATTBE_model1.png", width = 1500, height=400)
 
 
 
 # ONE FACTOR MODEL
 cfa_ATTBE_model2 <- '
  ATTBE =~   ATTBE.BienEtre.   + ATTBE.Nature. +  ATTBE.Health. + ATTBE.Durable.  + ATTBE.Ecoloc.   + ATTBE.Ecomen.
  '
 
 fit_cfa_ATTBE_model2 <- cfa(cfa_ATTBE_model2, data=data_complete, std.lv=TRUE)
 summary(fit_cfa_ATTBE_model2, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTBE_model2<-lavaanPlot(model = fit_cfa_ATTBE_model6, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                   graph_options = list(rankdir = "TB", layout = "dot"),
                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTBE_model2, "~/recherche/DOMINOS/results/plot_cfa_ATTBE_model2.png", width = 1500, height=400)
 
 comp_fit_be <- rbind(
   Model_1 = fitMeasures(fit_cfa_ATTBE_model1, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_2 = fitMeasures(fit_cfa_ATTBE_model2, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr"))
 ) 
 
 
### bois energie + bois construction
 
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
  ATTB =~   ATTBE.BienEtre.   + ATTBE.Nature.  + ATTBC.Nat. +  ATTBC.BienEtre. + ATTBE.Health. + ATTBE.Durable.  + ATTBE.Ecoloc.   + ATTBE.Ecomen. + ATTBC.Ecomen. + ATTBC.Ecoloc.   + ATTBC.Tech. +ATTBC.Durable.+ ATTBC.Onehealth.  
  '
 
 
 fit_cfa_ATTB_model2 <- cfa(cfa_ATTB_model2, data=data_complete, std.lv=TRUE)
 summary(fit_cfa_ATTB_model2, standardized=TRUE, fit.measures=TRUE)
 plot_cfa_ATTB_model2<-lavaanPlot(model = fit_cfa_ATTB_model2, coefs = TRUE,sig = 0.05,covs = TRUE,stars = c("latent","covs"),
                                  graph_options = list(rankdir = "TB", layout = "dot"),
                                  edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_ATTB_model2, "~/recherche/DOMINOS/results/plot_cfa_ATTB_model2.png", width = 1500, height=400)
 
 
 #### perception of threat ####
 
 # correlation
 men_vars <- grep("^ATTMEN", names(data_complete), value = TRUE)
 men_data <- data_complete[, men_vars]
 men_data_num <- data.frame(lapply(men_data, function(x) as.numeric(as.character(x))))
 cor_matrix <- cor(men_data_num, use = "pairwise.complete.obs")
 corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.7)

 # model 1
 cfa_attmen_model1 <- '
 ATTMEN_INQ =~  +  ATTMENACE.NoptR. +   ATTMENACE.Nopt.    + ATTMENACE.Inq. + ATTMENACE.InqR.
 ATTMEN_EX =~ ATTMENACE.Gestion. + ATTMENACE.GestionR. + ATTMENACE.Defo. + ATTMENACE.DefoR.
 ATTMEN_SA =~ ATTMENACE.CC.    +   ATTMENACE.CCR. + ATTMENACE.Sante. + ATTMENACE.SanteR.
 ATTMEN_INQ~~ATTMEN_EX
 ATTMEN_EX ~~ATTMEN_SA
 ATTMEN_SA~~ATTMEN_INQ
 '
 fit_cfa_attmen_model1<- cfa(cfa_attmen_model1, data=data_complete, std.lv=TRUE) 
 lavInspect(fit_cfa_attmen_model1, "cov.lv")
 summary(fit_cfa_attmen_model1, standardized=TRUE, fit.measures=TRUE)

 plot_cfa_attmen_model1<-lavaanPlot( model = fit_cfa_attmen_model1,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
   graph_options = list(rankdir = "TB", layout = "dot"),
   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_attmen_model1, "~/recherche/DOMINOS/results/plot_cfa_attmen_model1.png", width = 1500, height=400)
 
 
# model 2 valentin
 cfa_attmen_model2 <- '
 ATTMEN_SAN =~ ATTMENACE.Sante. + ATTMENACE.SanteR. +  ATTMENACE.NoptR. +   ATTMENACE.Nopt.  +  ATTMENACE.CC.    +   ATTMENACE.CCR. 
 ATTMEN_EXP =~ ATTMENACE.Inq. + ATTMENACE.InqR. +ATTMENACE.Gestion. + ATTMENACE.GestionR. + ATTMENACE.Defo. + ATTMENACE.DefoR.
 ATTMEN_SAN~~ATTMEN_EXP
 '
 fit_cfa_attmen_model2<- cfa(cfa_attmen_model2, data=data_complete, std.lv=TRUE) 
 summary(fit_cfa_attmen_model2, standardized=TRUE, fit.measures=TRUE)
 
 plot_cfa_attmen_model2<-lavaanPlot( model = fit_cfa_attmen_model2,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
                                     graph_options = list(rankdir = "TB", layout = "dot"),
                                     edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_attmen_model2, "~/recherche/DOMINOS/results/plot_cfa_attmen_model2.png", width = 1500, height=400)
 
 # model 2 valentin avec correlation
 cfa_attmen_model3 <- '
 ATTMEN_SAN =~  ATTMENACE.Sante. + ATTMENACE.SanteR. +  ATTMENACE.NoptR. +   ATTMENACE.Nopt.  +  ATTMENACE.CC.    +   ATTMENACE.CCR. 
 ATTMEN_EXP =~ ATTMENACE.Inq. + ATTMENACE.InqR. +ATTMENACE.Gestion. + ATTMENACE.GestionR. + ATTMENACE.Defo. + ATTMENACE.DefoR.
 ATTMEN_SAN~~ATTMEN_EXP
  ATTMENACE.Sante. ~~ ATTMENACE.SanteR. 
  ATTMENACE.NoptR. ~~   ATTMENACE.Nopt.
  ATTMENACE.CC.    ~~    ATTMENACE.CCR. 
 ATTMENACE.Inq. ~~   ATTMENACE.InqR.
 ATTMENACE.Gestion.~~   ATTMENACE.GestionR.
 ATTMENACE.Defo. ~~   ATTMENACE.DefoR.
 
 '
 fit_cfa_attmen_model3<- cfa(cfa_attmen_model3, data=data_complete, std.lv=TRUE) 
 summary(fit_cfa_attmen_model3, standardized=TRUE, fit.measures=TRUE)
 
 plot_cfa_attmen_model3<-lavaanPlot( model = fit_cfa_attmen_model3,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
                                     graph_options = list(rankdir = "TB", layout = "dot"),
                                     edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_attmen_model3, "~/recherche/DOMINOS/results/plot_cfa_attmen_model3.png", width = 1500, height=400)
 
 # model 4 esther avec correlation
 cfa_attmen_model4 <- '
 ATTMEN_SAN =~  ATTMENACE.Sante. + ATTMENACE.SanteR. +  ATTMENACE.Gestion. + ATTMENACE.GestionR.  + ATTMENACE.Defo. + ATTMENACE.DefoR.
 ATTMEN_FUT =~  ATTMENACE.NoptR. +   ATTMENACE.Nopt.  +  ATTMENACE.CC.    +   ATTMENACE.CCR. 
 ATTMEN_INQ =~ ATTMENACE.Inq. + ATTMENACE.InqR.
 ATTMEN_SAN ~~ ATTMEN_FUT
  ATTMEN_SAN ~~ ATTMEN_INQ
  ATTMEN_FUT ~~ ATTMEN_INQ
  ATTMENACE.Sante. ~~ ATTMENACE.SanteR. 
  ATTMENACE.NoptR. ~~   ATTMENACE.Nopt.
  ATTMENACE.CC.    ~~    ATTMENACE.CCR. 
 ATTMENACE.Inq. ~~   ATTMENACE.InqR.
 ATTMENACE.Gestion.~~   ATTMENACE.GestionR.
 ATTMENACE.Defo. ~~   ATTMENACE.DefoR.
 
 '
 fit_cfa_attmen_model4<- cfa(cfa_attmen_model4, data=data_complete, std.lv=TRUE) 
 lavInspect(fit_cfa_attmen_model4, "cov.lv")
 summary(fit_cfa_attmen_model4, standardized=TRUE, fit.measures=TRUE)
 
# plot_cfa_attmen_model4<-lavaanPlot( model = fit_cfa_attmen_model4,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
 #                                     graph_options = list(rankdir = "TB", layout = "dot"),
 #                                   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 #save_png(plot_cfa_attmen_model4, "~/recherche/DOMINOS/results/plot_cfa_attmen_model4.png", width = 1500, height=400)
 
 # one factor model
 cfa_attmen_model5 <- '
 ATTMEN_SAN =~  ATTMENACE.Sante. + ATTMENACE.SanteR. +  ATTMENACE.Gestion. + ATTMENACE.GestionR.  + ATTMENACE.Defo. + ATTMENACE.DefoR.+ ATTMENACE.NoptR. +   ATTMENACE.Nopt.  +  ATTMENACE.CC.    +   ATTMENACE.CCR. + ATTMENACE.Inq. + ATTMENACE.InqR.
  ATTMENACE.Sante. ~~ ATTMENACE.SanteR. 
  ATTMENACE.NoptR. ~~   ATTMENACE.Nopt.
  ATTMENACE.CC.    ~~    ATTMENACE.CCR. 
 ATTMENACE.Inq. ~~   ATTMENACE.InqR.
 ATTMENACE.Gestion.~~   ATTMENACE.GestionR.
 ATTMENACE.Defo. ~~   ATTMENACE.DefoR.
 '
 fit_cfa_attmen_model5<- cfa(cfa_attmen_model5, data=data_complete, std.lv=TRUE) 
 summary(fit_cfa_attmen_model5, standardized=TRUE, fit.measures=TRUE)
 
 plot_cfa_attmen_model5<-lavaanPlot( model = fit_cfa_attmen_model5,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
                                     graph_options = list(rankdir = "TB", layout = "dot"),
                                     edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_attmen_model5, "~/recherche/DOMINOS/results/plot_cfa_attmen_model5.png", width = 1500, height=400)
 
 # model 6 en retirant les reversed
 
 cfa_attmen_model6 <- '
 ATTMEN_san =~  ATTMENACE.Sante.  +  ATTMENACE.Gestion.   + ATTMENACE.Defo.  
 ATTMEN_fut =~ ATTMENACE.Nopt.  +  ATTMENACE.CC.   + ATTMENACE.Inq. 
ATTMEN_san ~~ATTMEN_fut
 '
 fit_cfa_attmen_model6<- cfa(cfa_attmen_model6, data=data_complete, std.lv=TRUE) 
 summary(fit_cfa_attmen_model6, standardized=TRUE, fit.measures=TRUE)
 
 plot_cfa_attmen_model6<-lavaanPlot( model = fit_cfa_attmen_model6,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
                                     graph_options = list(rankdir = "TB", layout = "dot"),
                                     edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_attmen_model6, "~/recherche/DOMINOS/results/plot_cfa_attmen_model6.png", width = 1500, height=400)
 
 # model 7 en retirant les reversed
 
 cfa_attmen_model7 <- '
 ATTMEN =~  ATTMENACE.Sante.  +  ATTMENACE.Gestion.   + ATTMENACE.Defo. + ATTMENACE.Nopt.  +  ATTMENACE.CC.   + ATTMENACE.Inq. 
 '
 fit_cfa_attmen_model7<- cfa(cfa_attmen_model7, data=data_complete, std.lv=TRUE) 
 summary(fit_cfa_attmen_model7, standardized=TRUE, fit.measures=TRUE)
 
 plot_cfa_attmen_model7<-lavaanPlot( model = fit_cfa_attmen_model7,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
                                     graph_options = list(rankdir = "TB", layout = "dot"),
                                     edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 save_png(plot_cfa_attmen_model7, "~/recherche/DOMINOS/results/plot_cfa_attmen_model7.png", width = 1500, height=400)
 
 comp_fit_attmen <- rbind(
   Model_1 = fitMeasures(fit_cfa_attmen_model1, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_2 = fitMeasures(fit_cfa_attmen_model2, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_3 = fitMeasures(fit_cfa_attmen_model3, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_4 = fitMeasures(fit_cfa_attmen_model4, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_5 = fitMeasures(fit_cfa_attmen_model5, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_6 = fitMeasures(fit_cfa_attmen_model6, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")),
   Model_7 = fitMeasures(fit_cfa_attmen_model7, c("chisq.scaled","cfi.robust","tli.robust","rmsea.robust","srmr")) ) 
 
 
 
 #### OVERALL MEASUREMENT MODEL ####
 
 cfa_1_bc <- '
  ATTENVP =~ ATTENV.P1.  + ATTENV.P2.  + ATTENV.P6.  + ATTENV.P8.   + ATTENV.P11.
  ATTENVU =~ ATTENV.U4.  + ATTENV.U5.  + ATTENV.U7.  + ATTENV.U9.  + ATTENV.U10. 
  ATTENVP ~~ ATTENVU
  
  ATTFOP =~ ATTFO.P1.  + ATTFO.P2. + ATTFO.P6.  + ATTFO.P8.  
  ATTFOU =~ ATTFO.U4.  + ATTFO.U7.  + ATTFO.U9.  + ATTFO.U10. 
  ATTFOP ~~ ATTFOU
  
  ATTBC_HUM=~    ATTBC.Ecomen. + ATTBC.Ecoloc.   + ATTBC.Tech. +ATTBC.Durable.+ ATTBC.Onehealth.   
  ATTBC_EGO =~  ATTBC.Nat.  + ATTBC.BienEtre. 
  
  
  ATTMEN =~  ATTMENACE.Sante.  +  ATTMENACE.Gestion.   + ATTMENACE.Defo. + ATTMENACE.Nopt.  +  ATTMENACE.CC.   + ATTMENACE.Inq. 

'

 fit_cfa_1_bc <- cfa(cfa_1_bc, data=data_complete, std.lv=TRUE) # ATTFO ET ATTENV tres corrélés
 summary(fit_cfa_1_bc, fit.measures=TRUE, standardized=TRUE)

 plot_cfa_1_bc<-lavaanPlot( model = fit_cfa_1_bc,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
                                     graph_options = list(rankdir = "LR", layout = "dot"),
                     
                            
                                    edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 
 plot_cfa_1_bc <- lavaanPlot(
   model = fit_cfa_1_bc,
   coefs = TRUE,
   sig = 0.05,
   covs = TRUE,
   stars = c("latent"),
   graph_options = list(rankdir = "LR", layout = "dot"),
   node_options = list(shape = "box", fontsize = 12, color = "lightblue", style = "filled"),
   #latent_options = list(shape = "ellipse", color = "lightgreen", fontsize = 12, style = "filled"),
   edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10)
 )
 
 
 lavInspect(fit_cfa_1_bc, "cov.lv") 
 
 
 ### with forest attitudes only
 
 cfa_1_bc <- '
  
  ATTFOP =~ ATTFO.P1.  + ATTFO.P2. + ATTFO.P6.  + ATTFO.P8.  
  ATTFOU =~ ATTFO.U4.  + ATTFO.U7.  + ATTFO.U9.  + ATTFO.U10. 
  ATTFOP ~~ ATTFOU
  
  ATTBC_HUM=~    ATTBC.Ecomen. + ATTBC.Ecoloc.   + ATTBC.Tech. +ATTBC.Durable.+ ATTBC.Onehealth.   
  ATTBC_EGO =~  ATTBC.Nat.  + ATTBC.BienEtre. 
  
  
  ATTMEN =~  ATTMENACE.Sante.  +  ATTMENACE.Gestion.   + ATTMENACE.Defo. + ATTMENACE.Nopt.  +  ATTMENACE.CC.   + ATTMENACE.Inq. 

'
 
 
 fit_cfa_1_bc <- cfa(cfa_1_bc, data=data_complete, std.lv=TRUE) # ATTFO ET ATTENV tres corrélés
 summary(fit_cfa_1_bc, fit.measures=TRUE, standardized=TRUE)
 
 plot_cfa_1_bc<-lavaanPlot( model = fit_cfa_1_bc,   coefs = TRUE,  sig = 0.05,  covs = TRUE,   stars = c("latent","covs"),
                            graph_options = list(rankdir = "LR", layout = "dot"),
                            
                            
                            edge_options = list(color = "grey30", penwidth = 1.5, fontsize = 10))
 


 

 
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
 