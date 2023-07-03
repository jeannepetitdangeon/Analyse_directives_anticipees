##                  ANALYSE DE DONNEES LOI DE FIN DE VIE


# Nettoyage du workspace

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

data <- read_excel("C:/Users/epcmic/OneDrive/Bureau/GitHub/Analyse_directives_anticipees/db.xlsx")
View(data)



#                  MODIFICATION DES VARIABLES POUR ANALYSE


# Suppression de variables non comprises dans les régressions

print(colnames(data))
data <- data[, -c(1, 3, 7, 12, 18, 19, 20, 21, 27, 28, 35, 36, 51)]



# Simplification du nom des colonnes

print(colnames(data))
colnames(data) <- c("Indiv", "Age", "Gender", "City", "CSP", "Medical", "Niveaumed",
                  "Malchro", "Typemalchro", "Loifin", "Nomloi", "Diranti", 
                  "Dirantibis", "Contextmedecin", "Rpzda", "Infoda", "Concernda",
                  "Redacda", "Conservda", "Valida", "Modifda", "Avantageda",
                  "Tapplicda", "Urgencepasda", "Applic1", "Vousda", "Vousdanon",
                  "Plusinfoda", "Plusexplida", "Mediuminfo", "Proinfo", "Infomedecin",
                  "Rolemedecin", "Tinfoda", "Helpda", "Medecinaborde", "Medecinattend",
                  "Expectconsult")

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


# Tri des NA en âge (trop jeune, erreur, non mentionné)

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

790/(790+270)
270/(790+270)
# 75% de femmes et 25% d'hommes. Sur-représentation des femmes
-------------------------------------------------------------------------------

# Les villes

table(data$City)
# Il y a 504 personnes de villages, 308 de petite villes, 248 de grande ville

-------------------------------------------------------------------------------

# Stat sur la diversité des CSP + Graph

table(data$CSP)

# La catégorie Employé est la plus importante, suivie de Profession intermédiaire.

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
# 714 hors milieu médical et 346 en milieu médical


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
  ggtitle("Répartition des niveaux dans le milieu de la santé") +
  scale_fill_brewer(palette = "Pastel1")

-------------------------------------------------------------------------------

# Maladies chroniques 

table(data$Malchro)
# 803 personnes sans maladie chronique, 257 avec


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
# 476 ne savent pas, 36 savent, en comptant les fautes d'othographe, parmis ceux
# qui ont déjà entendu parler de la loi de fin de vie et qui ont répondu a la 
# question. 


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
# Sachant que les individus répondent plusieurs endroits chacun, un au moins et
# ces question ne sont que pour ceux qui ont répondu qu'ils connaissent la loi.

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

donnees <- donnees %>%
  separate_rows(Rpzda, sep = ",\\s*") %>%
  filter(!is.na(Rpzda))  

donnees$Rpzda <- trimws(donnees$Rpzda)

resultats <- donnees %>%
  group_by(Medical, Rpzda) %>%
  summarise(Count = n()) %>%
  group_by(Medical) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  arrange(desc(Count))

top_reponses <- resultats %>%
  group_by(Medical) %>%
  slice(1:5) %>%
  ungroup()

top_reponses$Medical <- ifelse(top_reponses$Medical == 1, "Oui", "Non")

print(top_reponses)

# Mêmes réponses pour ceux qui sont dans le milieu médical ou non. 
# Un choix, le respect et là on a la liberté et une anticipation qui s'inversent 
# chez l'un et chez l'autre puis un soulagement. Rien de vraiment analysable. 
 

table(donnees$Rpzda)

# En tout, 457 ont dit un choix, Une anticipation 354, 422 le respect, 325 la liberté, 
# 110 la sécurité, 241 un soulagement, 183 une responsabilité, 46 une angoisse

rm(donnees)
rm(comptage_mal)
rm(comptage_med)
rm(resultats)
rm(top_reponses)
rm(reponses)


-------------------------------------------------------------------------------

  
# Qui a rédigé DA 

table(data$Vousda)
# 77 ont rédigé leur DA sur le sous échantillon. A comparer aux stats nationales.
# Regarder si ceux qui ont rédigé leurs DA ont eu juste aux questions. 


reponses <- data %>%
  filter(!is.na(Vousda), Vousda == "Oui")

comptage_genre <- reponses %>%
  group_by(Gender) %>%
  summarise(Count = n())

comptage_genre$Gender <- ifelse(comptage_genre$Gender == 1, "Femme", "Homme")

print(comptage_genre)

# 13 sont des hommes et 64 sont des femmes. A regarder selon sur représentation
# des femmes dans l'échantillon et acceptation de répondre au questionnaire.


reponses <- data %>%
  filter(!is.na(Vousda), Vousda == "Oui")

comptage_age <- reponses %>%
  group_by(Age) %>%
  summarise(Count = n())

print(comptage_age)

# Parmi ceux qui ont rédigé leurs DA, 16 sont 18-30, 17 sont 31-40, 7 sont 41-50,
# 19 sont 51-60, 9 sont 61-70 et 9 sont 70+

reponses <- data %>%
  filter(!is.na(Vousda), Vousda == "Oui")

comptage_med <- reponses %>%
  group_by(Medical) %>%
  summarise(Count = n())

print(comptage_med)

# 40 sont dans le médical, 37 ne le sont pas. 

reponses <- data %>%
  filter(!is.na(Vousda), Vousda == "Oui")

comptage_mal <- reponses %>%
  group_by(Malchro) %>%
  summarise(Count = n())

print(comptage_mal)

# 26 ont une maladie chronique et 51 n'en ont pas. Plus enclin à le faire quand 
# ça ne nous toucher pas d'aussi près et aussi lié au milieu qui favorise la 
# connaissance des DA. 

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

#                       STATS, CORRELATIONS ET REGRESSIONS




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
data$Vousda <- as.factor(data$Vousda)

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

binaryredac <- c(data$Vousda)
binaryredac <- ifelse(binaryredac == "Oui", 1, 0)
data$Vousda <- binaryredac
rm(binaryredac)
-------------------------------------------------------------------------------

  ##### AJOUTER COMPARAISON AVEC LES STATS DE LA BASE INITITALE 
  
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

Call:
  glm(formula = Loifin ~ Age, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.9830  -1.2735   0.9595   1.0842   1.1269  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)    0.22314    0.11859   1.882 0.059875 .  
Age31-40      -0.10318    0.18045  -0.572 0.567446    
Age41-50       0.03204    0.18628   0.172 0.863439    
Age51-60       0.31366    0.18685   1.679 0.093217 .  
Age61-70       0.25643    0.23575   1.088 0.276713    
Age70 et plus  1.59215    0.42447   3.751 0.000176 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1437.4  on 1059  degrees of freedom
Residual deviance: 1413.1  on 1054  degrees of freedom
AIC: 1425.1

Number of Fisher Scoring iterations: 4

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

Call:
  glm(formula = Loifin ~ Gender, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.3964  -1.3964   0.9732   0.9732   1.2090  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.07411    0.12180  -0.608    0.543    
Gender1      0.57549    0.14221   4.047 5.19e-05 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1437.4  on 1059  degrees of freedom
Residual deviance: 1421.0  on 1058  degrees of freedom
AIC: 1425

Number of Fisher Scoring iterations: 4

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

# 4% sont artisans, 16% sont cadre sup, 36% sont employé, 8% sont ouvriers, 
# 26% sont en profession intermédiaire, 7% sont sans activité. 

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

Call:
  glm(formula = Loifin ~ CSP, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.5508  -1.2208   0.8454   0.8765   1.5399  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -0.8210     0.2558  -3.209 0.001332 ** 
  CSPAgriculteur -11.7451   324.7438  -0.036 0.971149    
CSPArtisan       0.7697     0.4100   1.877 0.060468 .  
CSPCadre sup     1.6661     0.2946   5.656 1.55e-08 ***
  CSPEmployé       0.9226     0.2783   3.316 0.000915 ***
  CSPProf inter    1.5796     0.2842   5.559 2.71e-08 ***
  CSPSans acti     0.7985     0.3323   2.403 0.016253 *  
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1437.4  on 1059  degrees of freedom
Residual deviance: 1379.1  on 1053  degrees of freedom
AIC: 1393.1

Number of Fisher Scoring iterations: 11

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

  
# 76% n'ont pas de maladie chronique, 24% ont une maladie chronique 


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
  
# Qui connait loifin selon milieu et niveau en santé

oui_loifin <- subset(data, Loifin == "1")
percentage <- mean(oui_loifin$Medical == 1) * 100
percentage

# 33% des gens qui connaissent la loi de fin de vie dans cet échantillon sont 
# dans le milieu de la santé !

reponses <- data %>%
  filter(Medical == "1")

pourcentage <- reponses %>%
  group_by(Loifin) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# Parmi ceux dans le milieu médical, 60% connaissent la loi, 40% n'en ont pas 
# entendu parler. 

cramer_coeff <- assocstats(table(data$Loifin, data$Medical))$cramer
print(cramer_coeff)

# Le coefficient est le plus élevé qu'on a pu trouver jusqu'ici, de 0,32. il semble 
# que cette nouvelle variable pour savoir si l'individu est dans le milieu médical
# ou non est très pertinente. 

Reg <- glm(Loifin ~ Medical, data = data, family = "binomial")
summary(Reg)

Call:
  glm(formula = Loifin ~ Medical, family = "binomial", data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.8177  -1.1316   0.6523   1.2239   1.2239  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.1087     0.0757  -1.436    0.151    
Medical1      1.5479     0.1538  10.063   <2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1437.4  on 1059  degrees of freedom
Residual deviance: 1320.2  on 1058  degrees of freedom
AIC: 1324.2

Number of Fisher Scoring iterations: 4

# Le coefficient est de 1,51 et la p-value est faible alors le résultat est très
# significatif. Le fait d'être dans le milieu médical augmente grandement les 
# chances de connaitre la loi dans cet échantillon. Regardons maintenant selon 
# le niveau d'expertise médicale. 

class(data$Niveaumed)
data$Niveaumed <- relevel(data$Niveaumed, ref = "ASH")
Reg <- glm(Loifin ~ Niveaumed, family = binomial, data = data)
summary(Reg)

Call:
  glm(formula = Loifin ~ Niveaumed, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.7341   0.2195   0.3203   0.7268   1.4006  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)        -0.5108     0.5164  -0.989 0.322561    
NiveaumedAS         1.7071     0.6057   2.818 0.004826 ** 
  NiveaumedAutre      1.5325     0.5850   2.620 0.008803 ** 
  NiveaumedEtudiant   3.1499     0.8958   3.516 0.000437 ***
  NiveaumedIDE        3.4553     0.6908   5.002 5.68e-07 ***
  NiveaumedMedecin    4.2244     1.1362   3.718 0.000201 ***
  NiveaumedPharma     0.7520     0.6550   1.148 0.250927    
NiveaumedPsy        1.2040     1.3292   0.906 0.365034    
NiveaumedSecr       0.9163     0.6892   1.329 0.183685    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 351.81  on 359  degrees of freedom
Residual deviance: 289.34  on 351  degrees of freedom
(700 observations effacées parce que manquantes)
AIC: 307.34

Number of Fisher Scoring iterations: 6

# Après pluiseurs essais, la catégorie de référence la plus intéressante est 
# ASH. Les résultats sont très significatifs. Toutes choses égales par ailleurs, 
# Le fait d'être étudiant, AS, infirmière ou médecin augmente de manière 
# statistiquement significative les chances de connaitre la loi de fin de vie,
# par rapport à un agent de service hospitalier. Les p-value sont très faibles et 
# les coefficients très élevés. Le coef le plus élevé est celui pour les médecins.
# Il semble que ce soient ceux qui ont eu le plus de bonnes réponses, puis les 
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
# positif sur la probabilité de connaitre le nom de la loi. 

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

Call:
  glm(formula = Nomloi ~ Medical, family = binomial, data = data)

Deviance Residuals: 
  Min      1Q  Median      3Q     Max  
-0.507  -0.507  -0.199  -0.199   2.804  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -3.9120     0.4516  -8.662  < 2e-16 ***
  Medical1      1.9255     0.4906   3.925 8.67e-05 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 260.55  on 511  degrees of freedom
Residual deviance: 238.46  on 510  degrees of freedom
(548 observations effacées parce que manquantes)
AIC: 242.46

Number of Fisher Scoring iterations: 6

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

reponses <- data %>%
  filter(Diranti == "1")

pourcentage <- reponses %>%
  group_by(Age) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# Voici les pourcentages 

Age        Percentage
<fct>           <dbl>
1 18-30           27.2 
2 31-40           21.4 
3 41-50           17.6 
4 51-60           18.9 
5 61-70            8.64
6 70 et plus       6.15


cramer_coeff <- assocstats(table(data$Diranti, data$Age))$cramer
print(cramer_coeff)

# Pas de co

Reg <- glm(Diranti ~ Age , data = data, family = binomial)
summary(Reg)

Call:
  glm(formula = Diranti ~ Age, family = binomial, data = data)

Deviance Residuals: 
  Min      1Q  Median      3Q     Max  
-1.641  -1.270   1.020   1.088   1.161  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)  
(Intercept)    0.27958    0.11900   2.349   0.0188 *
  Age31-40       0.10289    0.18242   0.564   0.5727  
Age41-50      -0.12701    0.18597  -0.683   0.4946  
Age51-60      -0.06517    0.18386  -0.354   0.7230  
Age61-70      -0.24036    0.23107  -1.040   0.2982  
Age70 et plus  0.76638    0.34367   2.230   0.0257 *
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1449.8  on 1059  degrees of freedom
Residual deviance: 1440.6  on 1054  degrees of freedom
AIC: 1452.6

Number of Fisher Scoring iterations: 4
-------------------------------------------------------------------------------

# Qui connait DA selon genre

reponses <- data %>%
  filter(Diranti == "1")

pourcentage <- reponses %>%
  group_by(Gender) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

Gender Percentage
<fct>       <dbl>
  1 0            18.3
2 1            81.7


cramer_coeff <- assocstats(table(data$Diranti, data$Gender))$cramer
print(cramer_coeff)

# 0,18 alors peut être intéressant

Reg <- glm(Diranti ~ Gender , data = data, family = binomial)
summary(Reg)

Call:
  glm(formula = Diranti ~ Gender, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.3964  -1.3964   0.9732   0.9732   1.3401  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.3747     0.1239  -3.025  0.00248 ** 
  Gender1       0.8761     0.1440   6.085 1.17e-09 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1449.8  on 1059  degrees of freedom
Residual deviance: 1412.0  on 1058  degrees of freedom
AIC: 1416

Number of Fisher Scoring iterations: 4

-------------------------------------------------------------------------------

# Qui connait DA selon ville 
  
reponses <- data %>%
  filter(Diranti == "1")

pourcentage <- reponses %>%
  group_by(City) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

City             Percentage
<fct>                 <dbl>
  1 Un village             48.7
2 Une grande ville       23.3
3 Une petite ville       28.1

cramer_coeff <- assocstats(table(data$Diranti, data$City))$cramer
print(cramer_coeff)
# Pas de co car 0,02


Reg <- glm(Diranti ~ City  , data = data, family = binomial)
summary(Reg)

# Rien d'intéressant

-------------------------------------------------------------------------------

# Qui connait DA selon CSP

reponses <- data %>%
  filter(Diranti == "1")

pourcentage <- reponses %>%
  group_by(CSP) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

CSP        Percentage
<fct>           <dbl>
  1 Ouvrier          3.32
2 Artisan          2.33
3 Cadre sup       23.4 
4 Employé         28.6 
5 Prof inter      35.0 
6 Sans acti        7.31

cramer_coeff <- assocstats(table(data$Diranti, data$CSP))$cramer
print(cramer_coeff)
# 0,24 alors maybe 

Reg <- glm(Diranti ~ CSP  , data = data, family = binomial)
summary(Reg)

Call:
  glm(formula = Diranti ~ CSP, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.5539  -1.2003   0.8429   0.9575   1.6006  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)     -0.9555     0.2631  -3.632 0.000282 ***
  CSPAgriculteur -11.6106   324.7438  -0.036 0.971479    
CSPArtisan       0.3757     0.4250   0.884 0.376751    
CSPCadre sup     1.4976     0.2975   5.033 4.82e-07 ***
  CSPEmployé       1.0093     0.2849   3.542 0.000397 ***
  CSPProf inter    1.8076     0.2917   6.197 5.76e-10 ***
  CSPSans acti     0.9330     0.3379   2.761 0.005758 ** 
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1449.8  on 1059  degrees of freedom
Residual deviance: 1384.1  on 1053  degrees of freedom
AIC: 1398.1

Number of Fisher Scoring iterations: 11

-------------------------------------------------------------------------------

# Qui connait DA selon maladie chronique 

reponses <- data %>%
  filter(Diranti == "1")

pourcentage <- reponses %>%
  group_by(Malchro) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)
  
Malchro Percentage
<dbl>      <dbl>
  1       0       77.1
2       1       22.9 
  
  
reponses <- data %>%
  filter(!is.na(Diranti), Diranti == "1", !is.na(Malchro), Malchro == "1")

pourcentage <- reponses %>%
  group_by(Typemalchro) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

Typemalchro Percentage
<chr>            <dbl>
  1 Auto              1.45
2 Autre            26.1 
3 Cancer           14.5 
4 Cardiovasc       18.8 
5 Diabete          11.6 
6 Neuro             4.35
7 NA               23.2

cramer_coeff <- assocstats(table(data$Diranti, data$Malchro))$cramer
print(cramer_coeff)
# 0,02

Reg <- glm(Diranti ~ Malchro  , data = data, family = binomial)
summary(Reg)

# Rien d'intéressant ou signi

-------------------------------------------------------------------------------

# Qui connait DA selon milieu et niveau d'expertise médical

reponses <- data %>%
  filter(Diranti == "1")

pourcentage <- reponses %>%
  group_by(Medical) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

# 50/50

cramer_coeff <- assocstats(table(data$Diranti, data$Medical))$cramer
print(cramer_coeff)
# 0,39

Reg <- glm(Diranti ~ Medical  , data = data, family = binomial)
summary(Reg)

Call:
  glm(formula = Diranti ~ Medical, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.9019  -1.0603   0.5983   1.2992   1.2992  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept) -0.28185    0.07634  -3.692 0.000223 ***
  Medical1     1.91142    0.16155  11.831  < 2e-16 ***
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 1449.8  on 1059  degrees of freedom
Residual deviance: 1277.8  on 1058  degrees of freedom
AIC: 1281.8

Number of Fisher Scoring iterations: 4

# Selon le niveau d'expertise

Reg <- glm(Diranti ~ Niveaumed  , data = data, family = binomial)
summary(Reg)

Call:
  glm(formula = Diranti ~ Niveaumed, family = binomial, data = data)

Deviance Residuals: 
  Min        1Q    Median        3Q       Max  
-2.29741   0.00008   0.38499   0.66576   1.28139  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)   
(Intercept)       -3.791e-15  5.000e-01   0.000  1.00000   
NiveaumedAS        1.409e+00  6.026e-01   2.338  0.01940 * 
  NiveaumedAutre     1.350e+00  5.830e-01   2.315  0.02060 * 
  NiveaumedEtudiant  2.197e+00  7.876e-01   2.790  0.00528 **
  NiveaumedIDE       1.957e+01  1.075e+03   0.018  0.98548   
NiveaumedMedecin   2.565e+00  7.804e-01   3.287  0.00101 **
  NiveaumedPharma   -2.412e-01  6.421e-01  -0.376  0.70724   
NiveaumedPsy       6.931e-01  1.323e+00   0.524  0.60030   
NiveaumedSecr      1.099e+00  7.188e-01   1.528  0.12641   
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 321.17  on 359  degrees of freedom
Residual deviance: 248.54  on 351  degrees of freedom
(700 observations effacées parce que manquantes)
AIC: 266.54

Number of Fisher Scoring iterations: 18

###############################################################################


# Qui a rédigé DA selon age 

reponses <- data %>%
  filter(Vousda == "1")

pourcentage <- reponses %>%
  group_by(Age) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

Age        Percentage
<fct>           <dbl>
  1 18-30           20.8 
2 31-40           22.1 
3 41-50            9.09
4 51-60           24.7 
5 61-70           11.7 
6 70 et plus      11.7


cramer_coeff <- assocstats(table(data$Vousda, data$Age))$cramer
print(cramer_coeff)

# 0,14

Reg <- glm(Vousda ~ Age  , data = data, family = binomial)
summary(Reg)

Call:
  glm(formula = Vousda ~ Age, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-0.7710  -0.6128  -0.4531  -0.3715   2.3272  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)    -2.2246     0.2632  -8.453   <2e-16 ***
  Age31-40        0.3483     0.3703   0.941   0.3468    
Age41-50       -0.4144     0.4715  -0.879   0.3794    
Age51-60        0.6473     0.3644   1.776   0.0757 .  
Age61-70        0.6606     0.4512   1.464   0.1432    
Age70 et plus   1.1638     0.4678   2.488   0.0129 *  
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 458.47  on 594  degrees of freedom
Residual deviance: 446.01  on 589  degrees of freedom
(465 observations effacées parce que manquantes)
AIC: 458.01

Number of Fisher Scoring iterations: 5

-------------------------------------------------------------------------------

# Qui a rédigé DA selon genre

reponses <- data %>%
  filter(Vousda == "1")

pourcentage <- reponses %>%
  group_by(Gender) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

Gender Percentage
<fct>       <dbl>
1 0            16.9
2 1            83.1

cramer_coeff <- assocstats(table(data$Vousda, data$Gender))$cramer
print(cramer_coeff)
# 0,01

Reg <- glm(Vousda ~ Gender  , data = data, family = binomial)
summary(Reg)

Call:
  glm(formula = Vousda ~ Gender, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-0.5320  -0.5320  -0.5320  -0.5015   2.0667  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -2.0098     0.2953  -6.805 1.01e-11 ***
  Gender1       0.1260     0.3244   0.388    0.698    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 458.47  on 594  degrees of freedom
Residual deviance: 458.31  on 593  degrees of freedom
(465 observations effacées parce que manquantes)
AIC: 462.31

Number of Fisher Scoring iterations: 4

# Pas intéressant car même si plus de femmes, plus de femmes dans l'échantillon


-------------------------------------------------------------------------------

# Qui a rédigé DA selon ville 

reponses <- data %>%
  filter(Vousda == "1")

pourcentage <- reponses %>%
  group_by(City) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

City             Percentage
<fct>                 <dbl>
  1 Un village             46.8
2 Une grande ville       27.3
3 Une petite ville       26.0

cramer_coeff <- assocstats(table(data$Vousda, data$City))$cramer
print(cramer_coeff)
# 0,03

Reg <- glm(Vousda ~ City , data = data, family = binomial)
summary(Reg)

Call:
  glm(formula = Vousda ~ City, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-0.5723  -0.5158  -0.5158  -0.5051   2.0602  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)          -1.94987    0.17813 -10.946   <2e-16 ***
  CityUne grande ville  0.22371    0.29635   0.755     0.45    
CityUne petite ville -0.04483    0.29754  -0.151     0.88    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 458.47  on 594  degrees of freedom
Residual deviance: 457.71  on 592  degrees of freedom
(465 observations effacées parce que manquantes)
AIC: 463.71

Number of Fisher Scoring iterations: 4

# Pas intéressant 

-------------------------------------------------------------------------------

# Qui a rédigé DA selon CSP

reponses <- data %>%
  filter(Vousda == "1")

pourcentage <- reponses %>%
  group_by(CSP) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

CSP        Percentage
<fct>           <dbl>
  1 Cadre sup       23.4 
2 Employé         33.8 
3 Prof inter      33.8 
4 Sans acti        9.09

cramer_coeff <- assocstats(table(data$Vousda, data$CSP))$cramer
print(cramer_coeff)


Reg <- glm(Vousda ~ CSP , data = data, family = binomial)
summary(Reg)

# Pas assez de réponses pour avoir des résultats intéressants et un ordre des CSP

-------------------------------------------------------------------------------

# Qui a rédigé DA selon maladie chronique 

reponses <- data %>%
  filter(Vousda == "1")

pourcentage <- reponses %>%
  group_by(Malchro) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

Malchro Percentage
<dbl>      <dbl>
1       0       66.2
2       1       33.8
  
reponses <- data %>%
  filter(!is.na(Diranti), Vousda == "1", !is.na(Malchro), Malchro == "1")

pourcentage <- reponses %>%
  group_by(Typemalchro) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

Typemalchro Percentage
<chr>            <dbl>
  1 Autre            34.6 
2 Cancer           19.2 
3 Cardiovasc       19.2 
4 Diabete          11.5 
5 Neuro             3.85
6 NA               11.5 

cramer_coeff <- assocstats(table(data$Vousda, data$Malchro))$cramer
print(cramer_coeff)
# 0,09

Reg <- glm(Vousda ~ Malchro , data = data, family = binomial)
summary(Reg)

Call:
  glm(formula = Vousda ~ Malchro, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-0.6488  -0.4859  -0.4859  -0.4859   2.0953  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -2.0770     0.1485 -13.983   <2e-16 ***
  Malchro       0.6256     0.2637   2.372   0.0177 *  
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 458.47  on 594  degrees of freedom
Residual deviance: 453.13  on 593  degrees of freedom
(465 observations effacées parce que manquantes)
AIC: 457.13

Number of Fisher Scoring iterations: 4


-------------------------------------------------------------------------------


# Qui a rédigé DA selon milieu et niveau d'expertise médical

reponses <- data %>%
  filter(Vousda == "1")

pourcentage <- reponses %>%
  group_by(Medical) %>%
  summarise(Percentage = n() / nrow(reponses) * 100)

print(pourcentage)

Medical Percentage
<fct>        <dbl>
  1 0             48.1
2 1             51.9

cramer_coeff <- assocstats(table(data$Vousda, data$Medical))$cramer
print(cramer_coeff)
# 0,01

Reg <- glm(Vousda ~ Medical , data = data, family = binomial)
summary(Reg)

Call:
  glm(formula = Vousda ~ Medical, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-0.5369  -0.5369  -0.5159  -0.5159   2.0410  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept) -1.94976    0.17571 -11.097   <2e-16 ***
  Medical1     0.08568    0.24444   0.351    0.726    
---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 458.47  on 594  degrees of freedom
Residual deviance: 458.34  on 593  degrees of freedom
(465 observations effacées parce que manquantes)
AIC: 462.34

Number of Fisher Scoring iterations: 4

Reg <- glm(Vousda ~ Niveaumed , data = data, family = binomial)
summary(Reg)

Call:
  glm(formula = Vousda ~ Niveaumed, family = binomial, data = data)

Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-1.1774  -0.5663  -0.5306  -0.3923   2.2815  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)
(Intercept)       -1.757e+01  1.399e+03  -0.013    0.990
NiveaumedAS        1.631e+01  1.399e+03   0.012    0.991
NiveaumedAutre     1.582e+01  1.399e+03   0.011    0.991
NiveaumedEtudiant  1.504e+01  1.399e+03   0.011    0.991
NiveaumedIDE       1.568e+01  1.399e+03   0.011    0.991
NiveaumedMedecin   1.565e+01  1.399e+03   0.011    0.991
NiveaumedPharma    1.100e-09  1.838e+03   0.000    1.000
NiveaumedPsy       1.757e+01  1.399e+03   0.013    0.990
NiveaumedSecr      1.508e+01  1.399e+03   0.011    0.991

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 235.03  on 297  degrees of freedom
Residual deviance: 223.93  on 289  degrees of freedom
(762 observations effacées parce que manquantes)
AIC: 241.93

Number of Fisher Scoring iterations: 16


###############################################################################
###############################################################################
###############################################################################

#                       Stat sur ceux qui ont juste aux 
#                           questions sur les DA.


# Fonction utilisées dans les questions suivantes 

reponses <- strsplit(data$Infoda, ",")
reponses_distinctes <- unique(unlist(reponses))
print(reponses_distinctes)

databis <- data %>%
  separate_rows(Infoda, sep = ",")

-------------------------------------------------------------------------------
  
# Question 10

# Les volontés des patients exprimées par écrit ***
# Les volontés des patients exprimées à l'oral 
# Les options possibles en fin de vie incluant la sédation
# Les volontés des patients sur les conditions d'hydratation
# Les volontés concernant les méthodes de réanimation ***
# Les volontés concernant la poursuite des traitements ***
# Les volontés des patients concernant le refus, la limitation et l'arrêt des traitements ***


reponses_correctes <- c("Les volontés des patients exprimées par écrit sur les conditions de leur fin de vie", 
                        "Les volontés des patients exprimées à l'oral sur les conditions de leur fin de vie, en présence de 2 témoins dont la personne de confiance", 
                        "Les options possibles en fin de vie incluant la sédation",
                        "Les volontés des patients sur les conditions d'hydratation et de nutrition artificielles",
                        "Les volontés concernant les méthodes de réanimation (massage cardiaque, intubation...)",
                        "Les volontés concernant la poursuite des traitements et actes médicaux",
                        "Les volontés des patients concernant le refus, la limitation et l'arrêt des traitements et actes médicaux")

reponses_sep <- data %>%
  mutate(reponses = regmatches(Infoda, gregexpr(paste0("\\b(", paste(reponses_correctes, collapse = "|"), ")\\b"), Infoda)))

question10 <- reponses_sep %>%
  filter(lengths(reponses) == 7)

# Personne n'a trouvé les 7 bonnes réponses. 


reponses_fonda <- c("Les volontés des patients exprimées par écrit sur les conditions de leur fin de vie",
                    "Les volontés concernant les méthodes de réanimation",
                    "Les volontés concernant la poursuite des traitements et actes médicaux",
                    "Les volontés des patients concernant le refus")

reponses_sep <- data %>%
  mutate(reponses = regmatches(Infoda, gregexpr(paste0("\\b(", paste(reponses_fonda, collapse = "|"), ")\\b"), Infoda)))

question10 <- reponses_sep %>%
  filter(lengths(reponses) >= 4)

table(question10$Infoda)

# 252 personnes ont eu les 4 réponses fondamentales parmi leurs réponses.  


-------------------------------------------------------------------------------

# Question 11

# Les personnes en fin de vie
# Les personnes ayant une maladie grave et incurable
# Toutes les personnes majeures ***
# Les personnes sous tutelle 
# Les personnes ayant des troubles cognitifs
# Les personnes âgées de plus de 60 ans. 
# ERREUR FONDAMENTALE : Les majeurs et les mineurs 

filtre11 <- !grepl("Les majeurs et les mineurs", data$Concernda) & !is.na(data$Concernda)
question11 <- na.omit(data.frame(data$Concernda[filtre11]))
table(question11)

# 556 n'ont pas fait l'erreur fondamentale

question11 <- data.frame(question11$data.Concernda.filtre11.[grepl("Toutes les personnes majeures", 
                                                        question11$data.Concernda.filtre11.)])


# 413 n'ont pas fait l'erreur fondamentale et ont eu la réponse à ne pas oublier. 

-------------------------------------------------------------------------------

# Question 12

# Modèle type
# Sur papier libre 

question12 <- data %>%
  filter(Redacda == "En utilisant un modèle type proposé, Sur papier libre, daté et signé, authentifié")   

# 168 ont eu juste à cette question
table(data$Redacda)

-------------------------------------------------------------------------------
  
# Question 14

# Dans le dossier médical
# Dans le dossier chez votre médecin traitant 
# les donner à votre personne de confiance
# les garder chez vous
# ERREUR FONDAMENTALE : registre national
# ERREUR FONDA : notaire
# ERREUR FONDA : sur un fichier à partir de ma carte vitale


filtre14 <- !grepl("Les majeurs et les mineurs", data$Concernda) & !is.na(data$Concernda)
question14 <- na.omit(data.frame(data$Concernda[filtre11]))
table(question11)

-------------------------------------------------------------------------------

# Question 15 

# Absence de durée de validité ***
  
question15 <- data %>%
  filter(Valida == "Absence de durée de validité")

# 432 personnes ont eu juste à la question 15

-------------------------------------------------------------------------------

# Question 16

# Oui ***

question16 <- data %>%
  filter(Modifda == "Oui")

# 561 personnes ont eu juste à cette question

-------------------------------------------------------------------------------

# Question 18 

# Le jour où je ne pourrai plus communiquer après un accident grave 
# Le jour où je ne pourrai plus communiquer à l'occasion d'une maladie grave
# Uniquement dans les situations de fin de vie ***
# ERREUR : n'importe quel moment 

  

-------------------------------------------------------------------------------

# Question 19 

# Oui ***

question19 <- data %>%
  filter(Urgencepasda == "Oui")

# 370 ont eu juste à cette question


-------------------------------------------------------------------------------

# Question 20 

# Oui 

question20 <- data %>%
  filter(Pasda == "Oui")

# 199 personnes ont juste à cette question

-------------------------------------------------------------------------------

# Question 21 

# Des directives anticipées ***
# ERREUR : l'avis des proches

table(data$Applica1)

question21 <- data %>%
  filter(Applica1 == "Des directives anticipées")

# 427 personnes ont eu juste à cette question



###############################################################################
###############################################################################
###############################################################################

#                        PARTIE SUR LE MEDECIN TRAITANT


# Retour sur l'échantillon total

# Infos en plus ? 

table(data$Plusinfoda)
# 150 n'auraient pas aimé être plus informés, 871 oui. 

-------------------------------------------------------------------------------

# Explications en plus sur les termes médicaux ? 

table(data$Plusexplida)
# 231 ne veulent pas d'explications sur les termes médicaux en plus, 804 oui.


-------------------------------------------------------------------------------

# Par quels moyens ? 
  
reponses_sep <- strsplit(data$Mediuminfo, ",\\s*")
comptes <- table(unlist(reponses_sep))
print(comptes)

# 628 veulent une affiche chez le médecin traitant, 561 pour les brochures et dépliants,
# 235 dans des magazines, 590 avec un professionel de santé.426 dans reportages et doc.
# 225 dans des spots publicitaires. 

-------------------------------------------------------------------------------
  
# Par quel professionel de santé ? 
  
table(data$Proinfo)


library(stringdist)


donnees <- data.frame(data$Proinfo)

donnees$Proinfo <- tolower(donnees$data.Proinfo)
donnees$Proinfo <- gsub("[[:punct:]]", "", donnees$Proinfo)
donnees$Proinfo <- gsub("\\s+", " ", donnees$Proinfo)


compteur <- 0

for (i in 1:nrow(donnees)) {
  if (!is.na(donnees$Proinfo[i])) {
    mots <- strsplit(donnees$Proinfo[i], " ")[[1]]
    
    for (mot in mots) {
      if (!is.na(mot)) {
        distance <- stringdist::stringdist("médecin", mot, method = "lv")
        if (distance <= 2) {  
          compteur <- compteur + 1
          break 
        }
      }
    }
  }
}

cat("Nombre de lignes contenant le mot 'médecin':", compteur, "\n")

# 816 ont répondu médecin

compteur <- 0

for (i in 1:nrow(donnees)) {
  if (!is.na(donnees$Proinfo[i])) {
    mots <- strsplit(donnees$Proinfo[i], " ")[[1]]
    
    for (mot in mots) {
      if (!is.na(mot)) {
        distance <- stringdist::stringdist("infirmière", mot, method = "lv")
        if (distance <= 2) {  
          compteur <- compteur + 1
          break 
        }
      }
    }
  }
}

cat("Nombre de lignes contenant le mot 'infirmière':", compteur, "\n")

# 21 ont répondu infirmière

compteur <- 0

for (i in 1:nrow(donnees)) {
  if (!is.na(donnees$Proinfo[i])) {
    mots <- strsplit(donnees$Proinfo[i], " ")[[1]]
    
    for (mot in mots) {
      if (!is.na(mot)) {
        distance <- stringdist::stringdist("psy", mot, method = "lv")
        if (distance <= 3) {  
          compteur <- compteur + 1
          break 
        }
      }
    }
  }
}

cat("Nombre de lignes contenant le mot 'psy':", compteur, "\n")

# 260 à peu près ont dit psy 

------------------------------------------------------------------------------

# Médecin traitant doit être cet interlocuteur ? 
  

table(data$infomedecin)  
# 42 pensent que non, 997 pensent que oui. 


-------------------------------------------------------------------------------
  
# Rôle du médecin traitant ?
  
reponses_sep <- strsplit(data$Rolemedecin, ",\\s*")
comptes <- table(unlist(reponses_sep))
print(comptes)

# 949 ont dit informateur, 17 ont dit qu'il n'a aucun rôle à ce sujet, 259 ont 
# dit qu'il doit être porte parole des dernières volontés, 585 ont dit consultant,
# 429 ont dit conservateur de vos directives anticipées. 

-------------------------------------------------------------------------------

# Moment pour aborder le sujet ?

reponses_sep <- strsplit(data$Tinfoda, ",\\s*")
comptes <- table(unlist(reponses_sep))
print(comptes)
  
# 470 ont dit à l'annonce d'une maladie grave, 363 ont dit de manière systématique,
# 256 ont dit lors d'une consult de suivi relative à une maladie chronique, 122 lors
# d'une consult pour un tout autre motif, 513 quand je me sens prêt psychiquement. 

-------------------------------------------------------------------------------
  
# Aide pour rédiger du médecin traitant ?
  
table(data$Helpda)

# 394 ont dit non, 636 ont dit oui. 

-------------------------------------------------------------------------------
  
# Médecin aborde le sujet ?
  
table(data$Medecinaborde)

# 263 ont dit non, 756 ont dit oui. 

-------------------------------------------------------------------------------
  
# Médecin attend que le patient en parle ? 
  
table(data$Medecinattend)

# 678 ont dit non, 343 ont dit oui. 

-------------------------------------------------------------------------------

# qu'attendez vous d'une telle consultation avec le médecin traitant ? 
  
reponses_sep <- strsplit(data$Expectconsult, ",\\s*")
comptes <- table(unlist(reponses_sep))
print(comptes)

# 445 ont dit d'autres infos utiles, 623 sur les modalités d'écriture, 597 sur 
# les modalités de conservation, 516 infos sur les termes médicaux, 13 ne souhaitent 
# pas en parler avec le médecin traitant, 589 un rappel du cadre légal, 555 un temps
# d'écoute, 768 une définition claire et précise. 
  