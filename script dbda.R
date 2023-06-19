                  ## Analyse de données Loi de fin de vie 


# Nettoyage

rm(list=ls())

# Installation packages 

library(AER)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(dslabs)
library(stringdist)
library(stringr)
library(vcd)
library(stats)

# Importation des données

data <- read_excel("C:/Users/epcmic/OneDrive/Bureau/GitHub/Analyse_directives_anticipees/data.xlsx")
View(data)



                # Modification variables pour analyse #



# Suppression de variables non comprises dans les régressions faite à la main, 
# Simplification du nom des colonnes faite à la main. 

print(colnames(data))

-------------------------------------------------------------------------------

# Stat sur l'âge

class(data$Age)

convert_age <- function(data) {
data$Age <- as.numeric(gsub("[^0-9.]+", "", data$Age))
return(data)
}
data <- convert_age(data)

# Catégorie des ages 

databis <- data.frame(data$Age)
intervals <- c(18, 30, 40, 50, 60, 70, Inf)
labels <- c("18-30", "31-40", "41-50", "51-60", "61-70", "70 et plus")
databis$age_category <- cut(data$Age, breaks = intervals, labels = labels, 
                            include.lowest = TRUE)
data$Age <- databis$age_category


# Tri des NA en âge (trop jeunes, erreur, non mentionné)

datanona <- data.frame(data)
datanona <- data[!is.na(data$Age), ]
data <- datanona

# 5 observations en moins

rm(datanona)
rm(databis)
rm(intervals)
rm(labels)
rm(convert_age)

# Statistique âge 

table(data$Age)

# 288 entre 18 et 30 ans, 217 entre 31 et 40, 197 entre 41 et 50, 206 entre 51 et 60
# 102 entre 61 et 70, 50 à 70 ou plus 


-------------------------------------------------------------------------------

# Gender en dummy

binarygender <- c(data$Gender)
binarygender <- ifelse(binarygender == "Femme", 1, 0)
data$Gender <- binarygender
rm(binarygender)

table(data$Gender)
# 790 femmes et 270 hommes


-------------------------------------------------------------------------------

# Les villes

table(data$City)
# Il y a 504 personnes de villages, 308 de petite villes, 248 de grande ville

-------------------------------------------------------------------------------

# Stat sur la diversité des CSP + Graph

table(data$CSP)


csp_counts <- table(data$CSP)
csp_counts_df <- as.data.frame(csp_counts)
colnames(csp_counts_df) <- c("CSP", "Count")

ggplot(csp_counts_df, aes(x = "", y = Count, fill = CSP)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  xlab("") +
  ylab("") +
  ggtitle("Répartition des CSP") +
  scale_fill_brewer(palette = "Set3")


-------------------------------------------------------------------------------

# Stat milieu médical ou non 

table(data$Medical)
# 700 hors milieu médical et 360 en milieu médical


-------------------------------------------------------------------------------

# Stat niveau médical 

table(data$Niveaumed)


niveau_counts <- table(data$Niveaumed)

niveau_counts_df <- as.data.frame(niveau_counts)
colnames(niveau_counts_df) <- c("Niveaumed", "Count")

ggplot(niveau_counts_df, aes(x = "", y = Count, fill = Niveaumed)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  xlab("") +
  ylab("") +
  ggtitle("Répartition des niveaux en médecine") +
  scale_fill_brewer(palette = "Pastel1")

-------------------------------------------------------------------------------

# Maladies chroniques 

table(data$Malchro)
# 808 personnes sans maladie chronique, 252 avec 


-------------------------------------------------------------------------------

  
# Types de maladie chronique 

table(data$Typemalchro)

categorie_counts <- table(data$Typemalchro)
categorie_counts_df <- as.data.frame(categorie_counts)

colnames(categorie_counts_df) <- c("Type de maladie chronique", "Count")

ggplot(categorie_counts_df, aes(x = `Type de maladie chronique`, y = Count, 
                                fill = `Type de maladie chronique`)) +
  geom_bar(stat = "identity") +
  xlab("Type de maladie chronique") +
  ylab("Nombre de personnes") +
  ggtitle("Répartition des types de maladie chronique")

rm(categorie_counts_df)
rm(categorie_counts)
rm(csp_counts_df)
rm(niveau_counts_df)
rm(niveau_counts)
rm(csp_counts)


-------------------------------------------------------------------------------

# Entendu parler de la loi de fin de vie 

table(data$Loifin)
# 438 jamais entendu et 622 déjà entendu


-------------------------------------------------------------------------------


# Juste / Faux sur la loi de fin de vie 

table(data$Nomloi)

# beaucoup de catégories différentes alors tri, on accepte seulement si il y a 
# les deux mots "Claeys" et "Leonetti" 


dataset <- data.frame(data$Nomloi)
  
mots_reference <- c("Claeys", "Leonetti")

check_correspondance <- function(texte, mots_reference) {
  mots_texte <- strsplit(texte, " ")[[1]]
  correspondance <- sum(sapply(mots_reference, function(mot) 
    any(stringdist::stringdist(mots_texte, mot) <= 2)))
  return(correspondance == length(mots_reference))
}

data$Nomloi <- ifelse(sapply(dataset$data.Nomloi, check_correspondance, 
                                   mots_ref = mots_reference), "Sait", "Ne sait pas")

rm(dataset)
rm(mots_reference)
rm(check_correspondance)

table(data$Nomloi)
# 476 ne savent pas, 36 savent, en comptant les fautes d'othographe


--------------------------------------------------------------------------------


# Qui connait les directives anticipées 

table(data$Diranti)
# 458 ne connaissent pas et 602 connaissent


-------------------------------------------------------------------------------


# Où ont-ils connu les directives anticipées ?

table(data$Dirantibis)
reponses_sep <- strsplit(data$Dirantibis, ",")
comptes <- table(unlist(reponses_sep))
print(comptes)

rm(comptes)
rm(reponses_sep)

# Hopital = 194, Ecole = 86, Un ami = 45, Medecin traitant = 31, 
# Autre médecin = 91, Solo = 104, Média = 268



-------------------------------------------------------------------------------

# Stat sur ce que représentent les directives anticipées 


donnees <- data

donnees <- donnees %>%
  separate_rows(Rpzda, sep = ",\\s*") %>%
  filter(!is.na(Rpzda))

donnees$Rpzda <- trimws(donnees$Rpzda)

resultats <- donnees %>%
  group_by(Age, Rpzda) %>%
  summarise(Count = n()) %>%
  group_by(Age) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  arrange(Age, desc(Count))

top_reponses <- resultats %>%
  group_by(Age) %>%
  slice(1:3) %>%
  ungroup()

print(top_reponses)


# Les mots qui reviennent le plus, pour chaque catégorie sont en premier "un choix",
# Puis "Le respect". Ensuite, en troisième, chez les 18-30 et 31-40 et 41-50 c'est 
# une anticipation,à partir de 61ans et jusqu'aux plus âgés, le troisième mot qui
# Revient le plus est la liberté. 
# Le mot choix est retrouvé dans 20% des réponses chez les jeunes, 24,5% chez les 
# 61-70. Une anticipation est autour des 18% chez les 18-40 puis passe autour de 
# 16% et 14% chez les 41-60. La liberté est retrouvée dans 17,2% et 15,3% des réponses 
# respectivement chez les 61-70 et les +70. 


donnees <- donnees %>%
  separate_rows(Rpzda, sep = ",\\s*") %>%
  filter(!is.na(Rpzda))  

donnees$Rpzda <- trimws(donnees$Rpzda)

resultats <- donnees %>%
  group_by(Gender, Rpzda) %>%
  summarise(Count = n()) %>%
  group_by(Gender) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  arrange(desc(Count))

top_reponses <- resultats %>%
  group_by(Gender) %>%
  slice(1:5) %>%
  ungroup()

top_reponses$Gender <- ifelse(top_reponses$Gender == 1, "Femme", "Homme")

print(top_reponses)


# Chez les hommes et les femmes, séparément, les quatre premiers mots sont un choix
# Le respect, une anticipation, la liberté. Ensuite, pour les femmes, le 5ème mot 
# est un soulagement (retrouvé dans 11,7% des réponses) et chez les hommes c'est 
# Une responsabilité (dans 11,1% des réponses). 


table(donnees$Rpzda)

# En tout, 457 ont dit un choix, Une anticipation 354, 422 le respect, 325 la liberté, 
# 110 la sécurité, 241 un soulagement, 183 une responsabilité, 46 une angoisse

rm(donnees)

-------------------------------------------------------------------------------

  
# Qui a rédigé DA 

table(data$Vousda)
# 77 ont rédigé leur DA. 


reponses <- data %>%
  filter(!is.na(Vousda), Vousda == "Oui")

comptage_genre <- reponses %>%
  group_by(Gender) %>%
  summarise(Count = n())

comptage_genre$Gender <- ifelse(comptage_genre$Gender == 1, "Femme", "Homme")

print(comptage_genre)

# 13 sont des hommes et 64 sont des femmes 


reponses <- data %>%
  filter(!is.na(Vousda), Vousda == "Oui")

comptage_age <- reponses %>%
  group_by(Age) %>%
  summarise(Count = n())

print(comptage_age)

# Parmi ceux qui ont rédigé leurs DA, 16 sont 18-30, 17 sont 31-40, 7 sont 41-50,
# 19 sont 51-60, 9 sont 61-70 et 9 sont 70+



-------------------------------------------------------------------------------

# Si pas rédigé, pq ? 

table(data$Vousdanon)
reponses_sep <- strsplit(data$Vousdanon, ",")
comptes <- table(unlist(reponses_sep))
print(comptes)

# C'est difficile de se projeter dans l'avenir revient 231 fois, car je suis autonome 
# revient 44 fois, car en bonne santé revient 95 fois, car trop jeune revient 101 fois,
# Peur que les volontés ne soient pas respectées revient 30 fois, confiance en les proches
# revient 138 fois, manque d'info revient 143 fois, pas concerné 241 fois, ne savent pas 
# comment faire 54 fois. 





###############################################################################
###############################################################################
###############################################################################
###############################################################################





# Passage des variables catégorielles et binaire en facteurs 

data$Age <- as.factor(data$Age)
data$Gender <- as.factor(data$Gender)
data$City <- as.factor(data$City)
data$CSP <- as.factor(data$CSP)
data$Medical <- as.factor(data$Medical)
data$Niveaumed <- as.factor(data$Niveaumed)
data$Malchro <- as.factor(data$Malchro)
data$Loifin <- as.factor(data$Loifin)
data$Nomloi <- as.factor(data$Nomloi)
data$Diranti <- as.factor(data$Diranti)

# Transformation des variables en Oui/Non et en Sait/Ne sait pas en dummies

binaryloifin <- c(data$Loifin)
binaryloifin <- ifelse(binaryloifin == "Oui", 1, 0)
data$Loifin <- binaryloifin
rm(binaryloifin)

binarynom <- c(data$Nomloi)
binarynom <- ifelse(binarynom == "Sait", 1, 0)
data$Nomloi <- binarynom
rm(binarynom)

binaryda <- c(data$Diranti)
binaryda <- ifelse(binaryda == "Oui", 1, 0)
data$Diranti <- binaryda
rm(binaryda)

binarymalchro <- c(data$Malchro)
binarymalchro <- ifelse(binarymalchro == "Oui", 1, 0)
data$Malchro <- binarymalchro
rm(binarymalchro)

-------------------------------------------------------------------------------
  
  
# Qui connait loifin selon age 


reponses <- data %>%
  filter(!is.na(Loifin), Loifin == "1")

pourcentage <- reponses %>%
  group_by(Age) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# Parmi ceux qui connaissent la loi de fin de vie, 25,7% sont 18-30, 18,5% sont 
# 31-40, 9% sont 41-50, 17,8% sont 51-60, 20,9% sont 61-70 et 7% sont 70+


# Calcul du coefficient de corrélation de Cramer entre "Loifin" et "Age"
cramer_coeff <- assocstats(table(data$Loifin, data$Age))$cramer
print(cramer_coeff)
# Coefficient à 0,14 alors faible corrélation entre l'âge et la connaissance

data$Age <- relevel(data$CSP, ref = "18-30")
Reg <- glm(Loifin ~ Age, data = data, family = binomial)
summary(Reg)
# La catégorie d'âge '70 et plus' présente un coefficient de régression estimé 
# à 1.59215, avec une signification statistique élevée (<0.001). Cela suggère que 
# les individus âgés de 70 ans et plus ont une probabilité significativement plus 
# élevée de connaitre la loi par rapport à la catégorie 18-30, toutes choses étant 
# égales par ailleurs. 


-------------------------------------------------------------------------------

  
# Qui connait loifin selon genre

reponses <- data %>%
  filter(!is.na(Loifin), Loifin == "1")

pourcentage <- reponses %>%
  group_by(Gender) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

pourcentage$Gender <- ifelse(pourcentage$Gender == 1, "Femme", "Homme")

print(pourcentage)

# Ceux qui connaissent, 20,9% sont des hommes, 79,1% sont des femmes. 
  

# Calcul du coefficient de corrélation de Cramer entre "Loifin" et "Gender"
cramer_coeff <- assocstats(table(data$Loifin, data$Gender))$cramer
print(cramer_coeff)
# 0,12 alors faible correlation 


Reg <- glm(Loifin ~ Gender, data = data, family = binomial)
summary(Reg)

# Etre une femme est positivement associé à la prob de répondre oui car p-value 
# significative et coefficient supérieur à 0,57. 


-------------------------------------------------------------------------------

  
# Qui connait loifin selon ville 


reponses <- data %>%
  filter(!is.na(Loifin), Loifin == "1")

pourcentage <- reponses %>%
  group_by(City) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

  
# 48,4% viennent d'un village, 27,3 d'une petite ville, 24,3 d'une grande ville.

cramer_coeff <- assocstats(table(data$Loifin, data$City))$cramer
print(cramer_coeff)  
# 0,04 alors pas du tout correlé. 

data$City <- relevel(data$City, ref = "Un village")
Reg <- glm(Loifin ~ City, data = data, family = binomial)
summary(Reg)

# Aucun résultat significatif. 

-------------------------------------------------------------------------------

# Qui connait loifin selon CSP


reponses <- data %>%
  filter(!is.na(Loifin), Loifin == "1")

pourcentage <- reponses %>%
  group_by(CSP) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# 3% sont artisans, 25,1% sont cadre sup, 28,3% sont employé, 3,5% sont ouvriers, 
# 33% sont en profession intermédiaire, 7,1% sont sans activité. 

cramer_coeff <- assocstats(table(data$Loifin, data$CSP))$cramer
print(cramer_coeff) 
# 0,23, Faible corrélation mais la régression peut être intéressante 

Reg <- glm(Loifin ~ CSP, data = data, family = binomial)
summary(Reg)
# Pas la bonne CSP en référence alors changement de référence pour ouvrier, 
# population qui semble le moins connaitre la loi. 

class(data$CSP)
data$CSP <- relevel(data$CSP, ref = "Ouvrier")
Reg <- glm(Loifin ~ CSP, family = binomial, data = data)
summary(Reg)

# Résultats intéressants ! ce qui est très significatif : Cadre sup est associé
# positivement au fait de connaitre, employé aussi et prof inter aussi, par rapport 
# à la catégorie de référence, ouvrier.


-------------------------------------------------------------------------------

# Qui connait loifin selon maladie chronique 

reponses <- data %>%
  filter(!is.na(Loifin), Loifin == "1")

pourcentage <- reponses %>%
  group_by(Malchro) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

  
# 76,4% n'ont pas de maladie chronique, 23,6% ont une maladie chronique 


reponses <- data %>%
  filter(!is.na(Loifin), Loifin == "1", !is.na(Malchro), Malchro == "1")

pourcentage <- reponses %>%
  group_by(Typemalchro) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# Parmi ceux qui connaissent la Loifin et qui ont une maladie chronique, 1,3% 
# ont une maladie auto immune, 23,8% ont une maladie de type Autre, 15% ont un
# cancer, 20,4% ont une maladie cardiovasc, 13,6% ont le diabète, 4,1% ont une 
# maladie neurologique. Le reste n'a pas précisé la maladie. 


cramer_coeff <- assocstats(table(data$Loifin, data$Malchro))$cramer
print(cramer_coeff)
# Il ne semble pas y avoir de corrélation entre le fait d'avoir une maladie chronique 
# et le fait de connaitre la loi de fin de vie. Si ce résultat semble pourtant contre-
# intuitif, cela peut être du à la taille de l'échantillon et à la sur-représentation
# de personnel médical dans les individus. Le coefficient est de 0,003...

Reg <- glm(Loifin ~ Malchro, data = data, family = "binomial")
summary(Reg)

# Pas de résultat intéressant sur la maladie chronique ici. Aucun résultat
# significatif. 


-------------------------------------------------------------------------------
  
# Qui connait loifin selon milieu et niveau médical

oui_loifin <- subset(data, Loifin == "Oui")
percentage <- mean(oui_loifin$Medical == 1) * 100
percentage

# 46,7% des gens qui connaissent la loi de fin de vie dans cet échantillon sont 
# dans le milieu médical !

reponses <- data %>%
  filter(Medical == "1")

pourcentage <- reponses %>%
  group_by(Loifin) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# Parmi ceux dans le milieu médical, 80,8% connaissent la loi, 19,2% n'en ont pas 
# entendu parler. 

cramer_coeff <- assocstats(table(data$Loifin, data$Medical))$cramer
print(cramer_coeff)

# Le coefficient est le plus élevé qu'on a pu trouver jusqu'ici, de 0,32. il semble 
# que cette nouvelle variable pour savoir si l'individu est dans le milieu médical
# ou non est très pertinente. 

Reg <- glm(Loifin ~ Medical, data = data, family = "binomial")
summary(Reg)

# Le coefficient est de 1,51 et la p-value est faible alors le résultat est très
# significatif. Le fait d'être dans le milieu médical augmente grandement les 
# chances de connaitre la loi dans cet échantillon. Regardons maintenant selon 
# le niveau d'expertise médicale. 

class(data$Niveaumed)
data$Niveaumed <- relevel(data$Niveaumed, ref = "ASH")
Reg <- glm(Loifin ~ Niveaumed, family = binomial, data = data)
summary(Reg)

# Après pluiseurs essais, la catégorie de référence la plus intéressante est 
# ASH. Les résultats sont très significatifs. Toutes choses égales par ailleurs, 
# Le fait d'être étudiant, AS, infirmière ou médecin augmente de manière 
# statistiquement significative les chances de connaitre la loi de fin de vie,
# par rapport à un agent de service hospitalier. Les p-value sont très faibles et 
# les coefficients très élevés. Le coef le plus élevé est celui pour les médecins.
# Il semblent que ce soient ceux qui ont eu le plus de bonnes réponses, puis les 
# infirmières, puis les étudiants. 


###############################################################################
###############################################################################


# Qui connait le nom de la loifin selon age 

reponses <- data %>%
  filter(!is.na(Nomloi), Nomloi == "1")

pourcentage <- reponses %>%
  group_by(Age) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# 36,1% ont 18-30, 16,7% ont 31-40, 22,2% ont 41-50, 13,9% ont 51-60, 5,56% ont 
# 61-70 et 5,54% ont 70+

cramer_coeff <- assocstats(table(data$Nomloi, data$Age))$cramer
print(cramer_coeff)
# 0,08 alors pas de corrélation statistique. 

Reg <- glm(Nomloi ~ Age, data = data, family = binomial)
summary(Reg)
# Aucun résultat n'est significatif. Dans cet échantillon l'âge n'est pas une
# variable explicative de la connaissance de la loi de fin de vie ou des DA. 

-------------------------------------------------------------------------------

# Qui connait le nom de la loifin selon genre

reponses <- data %>%
  filter(Nomloi == "1")

pourcentage <- reponses %>%
  group_by(Gender) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# 8,3% sont des hommes, 91,7% sont des femmes 

cramer_coeff <- assocstats(table(data$Nomloi, data$Gender))$cramer
print(cramer_coeff)
# 0,08 alors pas de corrélation statistique 

Reg <- glm(Nomloi ~ Gender, data = data, family = binomial)
summary(Reg)
# Pas de résultat très significatif mais il semble qu'être une femme a un impact 
# positif sur la probabilité de connaitre la loi. 

-------------------------------------------------------------------------------


# Qui connait le nom de la loifin selon ville 

reponses <- data %>%
  filter(Nomloi == "1")

pourcentage <- reponses %>%
  group_by(City) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# 38,8% de village, 30,6 d'une petite ville et 30,6% d'une grande ville. 

cramer_coeff <- assocstats(table(data$Nomloi, data$City))$cramer
print(cramer_coeff)
# 0,05 alors pas de corrélation

Reg <- glm(Nomloi ~ City, data = data, family = binomial)
summary(Reg)
# Aucun résultat significatif. 

-------------------------------------------------------------------------------
  
  
# Qui connait le nom de la loifin selon CSP

reponses <- data %>%
  filter(Nomloi == "1")

pourcentage <- reponses %>%
  group_by(CSP) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)


# 38,9%  prof inter, 36,1% Cadre, 16,7% employé, 2,74% ouvriers, 5,56% sans acti


cramer_coeff <- assocstats(table(data$Nomloi, data$CSP))$cramer
print(cramer_coeff)
# Nan

Reg <- glm(Nomloi ~ CSP, data = data, family = binomial)
summary(Reg)
# Pas de résultat significatif pour la connaissance du nom de la loi.  

-------------------------------------------------------------------------------

# Qui connait le nom de la loifin selon maladie chronique 

reponses <- data %>%
  filter(Nomloi == "1")

pourcentage <- reponses %>%
  group_by(Malchro) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# 16,7% de ceux qui connaissent la loi ont une maladie chronique. 

cramer_coeff <- assocstats(table(data$Nomloi, data$Malchro))$cramer
print(cramer_coeff)
# 0,04 donc pas de corrélation. 

Reg <- glm(Nomloi ~ Malchro, data = data, family = binomial)
summary(Reg)
# Aucun résultat significatif. 



# Qui connait le nom de la loifin selon type maladie chronique

reponses <- data %>%
  filter(!is.na(Nomloi), Nomloi == "1", !is.na(Malchro), Malchro == "1")

pourcentage <- reponses %>%
  group_by(Typemalchro) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# Parmi ceux qui connaissent le nom de la loi et qui ont une maladie chronique, 
# 16,7% ont un cancer et 16,7% ont une maladie cardiovasc, 50% sont dans "Autre".
# Ainsi, les plus représentés ici sont cancer et cardiovasc. 

cramer_coeff <- assocstats(table(data$Nomloi, data$Typemalchro))$cramer
print(cramer_coeff)
# 0,16 mais peu intéressant ici vu que l'échantillon est de très petite taille. 

Reg <- glm(Nomloi ~ Typemalchro, data = data, family = binomial)
summary(Reg)
# Aucun résultat significatif ici, on distingue toutefois que le fait d'avoir un
# cancer ou une maladie cardiovasculaire a un effet positif sur le fait de connaitre
# le nom de la loi. Ce résultat n'est pas statistiquement significatif. 


-------------------------------------------------------------------------------

# Qui connait le nom de la loi fin selon milieu médical et niveau d'expertise
  
reponses <- data %>%
  filter(Nomloi == "1")

pourcentage <- reponses %>%
  group_by(Medical) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# Parmi ceux qui connaissent le nom de la loi, 86,1% sont dans le milieu médical. 

reponses <- data %>%
  filter(Medical == "1")

pourcentage <- reponses %>%
  group_by(Nomloi) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)
  
# Toutefois, seulement 8,5% de ceux dans le milieu médical connaissaient le nom
# de la loi. 

cramer_coeff <- assocstats(table(data$Nomloi, data$Medical))$cramer
print(cramer_coeff)
# 0,19 alors il semble y avoir une légère corrélation. 

Reg <- glm(Nomloi ~ Medical, data = data, family = binomial)
summary(Reg)
# On voit ici que le fait d'être dans le milieu médical a un impact positif sur 
# la probabilité de connaitre le nom de la loi. Le coefficient est élevé et 
# statistiquement significatif. (p-value ***)

Reg <- glm(Nomloi ~ Niveaumed , data = data, family = binomial)
summary(Reg)
# Ici, pour le nom de la loi, l'échantillon est vraiment petit et est difficilement
# un bon repère pour les stats, aucun résultat n'est significatif. Toutefois, 
# on peut voir qu'être médecin semble avoir un impact positif. 

###############################################################################


# Qui connait DA selon age 




# Qui connait DA selon genre




# Qui connait DA selon ville 



# Qui connait DA selon CSP




# Qui connait DA selon maladie chronique 



# Qui connait DA selon milieu et niveau d'expertise médical
