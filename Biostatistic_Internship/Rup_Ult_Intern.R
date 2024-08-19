save(list = ls(), file= ('teststage_environnement7.Rdata'))
getwd()
setwd("C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine")
rm(list=ls())

load('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/teststage_environnement5.Rdata')

load('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/teststage_environnement8.Rdata')



library(prettyR)
library(RColorBrewer)
library(gtsummary)
library(ggplot2)

install.packages('mice')
library(mice)

library(MatchIt)
install.packages('MatchIt')

install.packages("MatchThem")
install.packages('MatchThem')
library(MatchThem)

library(cobalt)

install.packages("modelsummary")
library(modelsummary)

library(survival)
library(survminer)
library(tidyverse)

install.packages("patchwork")
library(patchwork)


### COULEURS ###
col.qual<-brewer.pal(n = 8, name = 'Set2')
col.seq<-brewer.pal(n = 9, name = 'YlGn')
col.div<-brewer.pal(n = 11, name = 'RdYlBu')#'PiYG')



##############################importation du fichier au format long et transformation en fichier de format large##
# Installer et charger la library readxl si elle n'est pas déjà installée
install.packages("readxl")
library(readxl)

# Utiliser la fonction `read_excel()` pour importer le fichier Excel
df_long <- read_excel("chemin/vers/votre/fichier.xlsx")

########## Transformation############

library(reshape2)

# Utiliser la fonction `dcast()` pour transformer le dataframe en format large
df_Rul_wide2 <- dcast(df_Rul_long, UniqueKey + ANNEE + ETABLISSEMENT + Nometprnom + GROUPE ~ Variable, value.var = "Valeur")

# Afficher le dataframe résultant
print(df_Rul_wide)





#####vérification des modalités de  chaques variables####

 for(i in facvar_Rul ) {
   tab.facvar <- table(df_Rul[ ,print(i)], useNA = "always", deparse.level = 1 ) 
   print(tab.facvar) 
 }









prettyR::describe(df_Rup.Ulcsv, num.desc= c("mean", "min", "max", "pctmiss", "nmiss", "q1", "q3",
                                            "valid.n", "median", "var"))






df_Rul <- read.csv2("/Users/TCHEUMENI/Documents/STAGE DE MASTER/df_RupUl.csv", stringsAsFactors = T, na.strings = "")



str(df_rhc)





## Création des variables uniques###

Create_uniq_var <- function(df_Rul, new_column, colnames) {
  df_Rul[[new_column]] <- NA
  
  for (col in colnames) {
    df_Rul[[new_column]] <- ifelse(df_Rul[[col]] == "OUI", col, df_Rul[[new_column]])
  }
  
  return(df_Rul[[new_column]])
  
}

names(df_Rul)
## fonction d'appel
df_Rul$var <- Create_uniq_var(df_Rul, "var", c("colonne1", "colonne2", ..., "colonnen"))


df_Rul$Profession <-  Create_uniq_var(df_Rul, "Profession", c("Menagere", "Elvetudiante", "Commerante", "Fonctionnaire", "Autres"))
df_Rul$Age_cat <- Create_uniq_var(df_Rul, "Age_cat", c("N25", "N2530", "N3035", "N3540", "N40"))
df_Rul$Stat_matri <- Create_uniq_var(df_Rul, "Stat_matri", c("marie", "clibataire" ,"veuve", "divorce"))
df_Rul$Religion <- Create_uniq_var(df_Rul, "Religion", c("Catholique", "Protestante", "pentectiste" , "musulman", "autres1" ))
df_Rul$Niv_scol <- Create_uniq_var(df_Rul, "Niv_scol", c("Primaire" , "Secondaire", "Suprieur"))
df_Rul$Age_gest <- Create_uniq_var(df_Rul, "Age_gest", c( "N37semaines", "N3742semaines", "N42semaines"))
df_Rul$Gravité <- Create_uniq_var(df_Rul, "Gravité", c("N1", "N24", "N5" ))
df_Rul$Parité <- Create_uniq_var(df_Rul, "Parité", c("Primipare",  "Paucipare",  "Multipare", "Grandemultipare" ))
df_Rul$Nbre_cpn <- Create_uniq_var(df_Rul, "Nbre_cpn", c("N4", "N41" , "N42" ))
df_Rul$Mod_trav <- Create_uniq_var(df_Rul, "Mod_trav", c("Travailinduit", "Travailspontane"  ))
df_Rul$HU <- Create_uniq_var(df_Rul, "HU", c("HU37cm","HU37cm1" ))
df_Rul$Deces <- Create_uniq_var(df_Rul, "Deces", c( "NNEVIVANT","MEREVIVANTE" ))
df_Rul$Pres_cpn <- Create_uniq_var(df_Rul, "Pres_cpn", c("Sagefemme", "rsidentengyncologie", "Internes" ,  "gyncologueobsttricien", "Mdecingnraliste"  ))
df_Rul$Lieu_Ccpn <- Create_uniq_var(df_Rul, "Lieu_Ccpn", c("hpital", "centredesante", "prive", "Aucun"))
df_Rul$Tps_tra <- Create_uniq_var(df_Rul, "Tps_tra", c( "N12h",  "N1248h","N48h"))
df_Rul$Lieu_tra <- Create_uniq_var(df_Rul, "Lieu_tra", c( "hpital1", "centredesante1", "cliniqueprive", "Domicile"))
df_Rul$Lieu_pro <- Create_uniq_var(df_Rul, "Lieu_pro", c( "hpital2", "centredesante2" , "cliniqueprive1", "Domicile1"))
df_Rul$Mod_adm <- Create_uniq_var(df_Rul, "Mod_adm", c("Evacue", "Rfre" ,"Venuedellemme" ))
df_Rul$Bishop <- Create_uniq_var(df_Rul, "Bishop", c("N7", "N71" ))
df_Rul$Pres_sal_accou <- Create_uniq_var(df_Rul, "Pres_sal_accou", c("sagefemme1" , "gyncologueobsttricien1", "Interne" ,"rsident", "Autre"  ))
df_Rul$Poids_foet <- Create_uniq_var(df_Rul, "Poids_foet", c("N3500g", "N35004000g", "N4000g" ))
df_Rul$Nbre_foet <- Create_uniq_var(df_Rul, "Nbre_foet", c("N11", "N2", "N21" ))
df_Rul$Espa_intge <- Create_uniq_var(df_Rul, "Espa_intge", c("N2ans", "N2ans1"))



##vecteur des variables nouvellement crées###

var_con_cré <- paste(colnames(df_Rul[, 107:129]), collapse = ",")
var_vec_cré <- strsplit(var_con_cré, ",")[[1]]  

## transformation des chaines de caractères en en facteurs
df_Rul[, var_vec_cré] <- lapply(df_Rul[, var_vec_cré], as.factor)
str(df_Rul[, var_vec_cré])


df_Rul$Age_cat <- factor(df_Rul$Age_cat, levels=c("N25", "N2530", "N3035", "N3540", "N40"), labels = c("<25", "[25-30[", "[30-35[", "[35-40[ ", "≥40 " ))
df_Rul$Age_gest <- factor(df_Rul$Age_gest, levels=c("N37semaines", "N3742semaines", "N42semaines"), labels = c( "< 37sem" ,  "[37-42]sem", ">42sem" ))
df_Rul$Gravité <- factor(df_Rul$Gravité, levels=c("N1", "N24", "N5"), labels = c(  "1",  "[2-4]" , "≥5" ))
df_Rul$Nbre_cpn <- factor(df_Rul$Nbre_cpn, levels=c("N4", "N41" , "N42"), labels = c( "<4 ", "4 " ,  ">4"  ))
df_Rul$Espa_intge <- factor(df_Rul$Espa_intge, levels=c("N2ans", "N2ans1"), labels = c( "<2ans",  "≥2ans" ))
df_Rul$Tps_tra <- factor(df_Rul$Tps_tra, levels=c( "N12h","N1248h", "N48h"), labels = c("<12h", "[12-48 h[" , "≥48h"  ))
df_Rul$Bishop <- factor(df_Rul$Bishop, levels=c("N7", "N71"), labels = c( "<7" , "≥7" ))
df_Rul$Poids_foet <- factor(df_Rul$Poids_foet, levels= c("N3500g", "N35004000g", "N4000g"), labels = c("<3500g" , "[3500-4000g[",  ">4000g"))
df_Rul$Nbre_foet <- factor(df_Rul$Nbre_foet, levels= c("N11", "N2", "N21"), labels = c( "1",  "2" , ">2"))



## vérication des données convertir

for(i in var_vec_cré ) {
  tab2.facvar <- table(df_Rul[ ,print(i)], useNA = "always", deparse.level = 1 ) 
  print(tab2.facvar) 
}

var_M1col

# variables à modalité repartir sur une colonne

var_M1col <- paste(colnames(df_Rul[, c(1:5)]), collapse = ",")
var_M1col <- strsplit(var_M1col, ",")[[1]] 

var_M2col <- paste(colnames(df_Rul[, c(58:60, 77:80, 92:106)]), collapse = ",")
var_M2col <- strsplit(var_M2col, ",")[[1]]  

## nouveau jeux de données
df_Rul2 <- df_Rul[ ,c(var_M1col, var_vec_cré, var_M2col)]
df_Rul2$ANNEE <-  as.Date(as.character(df_Rul2[,"ANNEE"]), format = "%Y")
df_Rul2 <- subset(df_Rul2, select = -(Deces))
df_Rul2$NNEVIVANT <- df_Rul$NNEVIVANT
df_Rul2$MEREVIVANTE <- df_Rul$MEREVIVANTE



### groupes de variables

var.ql<-names(Filter(is.factor,subset(df_Rul2, select = -c(1:4))))


## Description des variables###

tabl1 <- df_Rul2[, var.ql] %>%
  tbl_summary( by= GROUPE, 
                 missing_text = "(Missing)") %>%
  add_n() %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  bold_p()
tabl1
#############################

########################



##########################



## Bivariate pour representer le forestplot
library(broom)



ql_summary <- function(by_var) {
  summary <- var.ql[var.ql!=by_var] %>%      
    str_c(paste0(by_var,' ~ '), .) %>%  
    map(.f = ~glm(formula = as.formula(.x),
                  family = "binomial",
                  data = df_Rul2[,var.ql])) %>%        
    map(.f = ~tidy(.x,exponentiate = TRUE,conf.int = TRUE)) %>%
    bind_rows() %>% 
    mutate(across(where(is.numeric), round, digits = 2)) %>%
    select(term, estimate, conf.low, conf.high) %>%
    filter(term != '(Intercept)')
  summary
}

sum_ql<- ql_summary('GROUPE')%>% 
  mutate(group="GROUPE")





png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/forestER_ql.png',
    width=700,height=900)
ggplot(data=sum_ql,
       aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, col=group)) +
  geom_pointrange(aes(col=group), position=position_dodge(width =0.6)) +
  geom_hline(yintercept = 1, linetype=2)+
  geom_vline(xintercept =(1:length(sum_ql$term))+0.5, linetype=2)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high,col=group),width=0.5,cex=1, 
                position=position_dodge(width = 0.6))+ 
  scale_fill_brewer(palette = 'Set2') +
  scale_y_log10() +
  #facet_wrap(~term,strip.position="left",ncol=1,scales = "free_y") +
  labs(x = NULL, y = NULL) +
  guides(color = guide_legend(reverse=TRUE)) +
  theme(legend.title = element_blank(),
        axis.ticks.y=element_blank())+
  coord_flip()

dev.off()

var_M1col


##### Imputation  & Analysis #####

#################################
df_toimput<-subset(df_Rul, select=-c(UniqueKey, ANNEE,  ETABLISSEMENT, Nometprnom, Hydramnios))

df_imp<-mice(df_toimput, seed=29,method='pmm',m=10,maxit=15)#,print=F)
df.imp_check<-complete(df_imp)


for(i in var_vec_cré ) {
  tab2.facvar <- table(df_Rul2[ ,print(i)], useNA = "always", deparse.level = 1 ) 
  print(tab2.facvar) 
}
########################################################
####vrai imputation multiple ################

df_toimput2<-subset(df_Rul2, select=-c(UniqueKey, ANNEE,  ETABLISSEMENT, Nometprnom, Hydramnios))

df_imp2<-mice(df_toimput2, seed=29,method='pmm',m=10,maxit=15)#,print=F)
df.imp_check2<-complete(df_imp2)
str(df.imp_check2)
summary(df_imp2)

### verification et remplacement des valeurs manquantes ###
df.imp_check2[!complete.cases(df.imp_check2), ]

table(df.imp_check2$Asthnie, useNA = "always")

df.imp_check2$Asthnie[126] <- "OUI"

names(df_Rul2)

for(i in var_vec_cré ) {
  print(i)
  tab2.facvar <- table(df.imp_check2[ ,print(i)], useNA = "always", deparse.level = 1 ) 
  print(tab2.facvar) 
}

summary(df_imp2)

# Vérification imputation
png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/imputation-stripplot-.png')
stripplot(df_imp2,Age_cat+Religion+Parité+Nbre_cpn+TRAITEMENT+Espa_intge+ Lieu_tra+Bishop+Mod_adm~.imp)
dev.off()
png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/imputation-bwplot-.png')
bwplot(df_imp2, Age_cat+Religion+Parité+Nbre_cpn+TRAITEMENT+Espa_intge+ Lieu_tra+Bishop+Mod_adm~.imp)
dev.off()

png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/imputation-densityplot-.png')
densityplot(~Age_cat+Religion+Parité+Nbre_cpn+Asthnie+Espa_intge+ Lieu_tra+Bishop+Mod_adm, data=df.imp_check2)
dev.off()

png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/imputation-densityplot2-.png')
densityplot(df_imp2)
dev.off()

###########################
# imputation avec le grand jeu de donnée sans creation de variables unique.

df_Rul3 <- read.csv2("/Users/TCHEUMENI/Documents/STAGE DE MASTER/df_RupUl.csv", stringsAsFactors = T, na.strings = "")


df_toimput3<-subset(df_Rul3, select=-c(UniqueKey, ANNEE,  ETABLISSEMENT, Nometprnom, Hydramnios))

df_imp3<-mice(df_toimput3, seed=29,method='pmm',m=10,maxit=15)#,print=F)
df.imp_check3<-complete(df_imp3)



#################
####descriptions des variables après imputations####

var.qll<-names(Filter(is.factor,df.imp_check2))

tabl1 <- df.imp_check2[, var.qll] %>%
  tbl_summary( by= GROUPE, 
               missing_text = "(Missing)") %>%
  add_n() %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  bold_p()
tabl1

str(df.imp_check2)
df.imp_check2$de
df_Rul2$Deces

############################################
##AVEC LE GRAND JEU DE DONNEES
var.qll3<-names(Filter(is.factor,df.imp_check3))

tabl3 <- df.imp_check[, var.qll2] %>%
  tbl_summary( by= GROUPE, 
               missing_text = "(Missing)") %>%
  add_n() %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  bold_p()
tabl3


###########################################################################
# Tracer le graphique d'évolution des nombres de cas par annéé.

# Installer le package "ggplot2" s'il n'est pas déjà installé
if (!require("ggplot2")) {
  install.packages("ggplot2")
}

# Charger le package
library(ggplot2)

# Filtrer uniquement les cas dans le data frame
cas_df <- df_Rul2[df_Rul2$GROUPE == "CAS", ]

# Calculer le nombre de cas par année
cas_par_annee <- aggregate(cas_df$GROUPE, by = list(ANNEE = cas_df$ANNEE), FUN = length)

# Tracer le graphique d'évolution des nombres de cas par année avec des styles de ligne personnalisés
png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/Evolution des Cas.png')
ggplot(cas_par_annee, aes(x = ANNEE, y = x)) +
  geom_line(color = "blue") +
  labs(x = "Année", y = "Nombre de cas", title = "Évolution des cas à l'Hôpital Général et Laquintinie par année") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "blue", face = "bold"),
    axis.title.x = element_text(color = "black", face = "bold" ),
    axis.title.y = element_text(color = "black", face = "bold"),
    axis.line = element_line(color = "black", size = 0.5, linetype = "solid")
  )
dev.off()


#########################################

# tracer le le nombre de cas et de temoin par établissement

table(df_Rul$GROUPE, df_Rul$ETABLISSEMENT, deparse.level = 2)

# Filtrage des lignes où ETABLISSEMENT n'est pas NA
df_filtered <- subset(df_Rul2, !is.na(ETABLISSEMENT))

# Tracé du graphique
png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/Cas_Temoin_par_etabli.png')
ggplot(df_filtered, aes(x = ETABLISSEMENT, fill = GROUPE)) +
  geom_bar(position = "dodge") +
  labs(x = "Etablissement", y = "Nombre d'observations", fill = "Groupe") +
  ggtitle("Répartition des CAS et TEMOINS par établissement")
dev.off()

################################3
png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/poids~groupe.png')
boxplot(df_Rul2$Poids_foet~df_Rul2$GROUPE, col="yellow", main="Poids Foetal en fonction des Groupes", xlab = "GROUPE", ylab = "Densité")
dev.off()


png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/Age_distribution.png')
barplot(table(df.imp_check2$Age_cat), col = c("pink", "blue", "green", "yellow", "purple"), 
        main = "Distribution des âges", xlab = "Age_cat", ylab = "Fréquence")
abline(h = 0, lwd = 1)
dev.off()
######################################################################




###calcul du OR et son IC AVEC AJOUT DE LA pVALUE#####

#########################################

###vrai code avec p value pour varible global####

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# Boucle sur chaque variable
for (variable in variables) {
  # Ajuster le modèle de régression logistique en utilisant la variable comme prédicteur
  model <- glm(formula = paste("GROUPE ~", variable), data = df.imp_check2, family = "binomial")
  
  # Obtenir le résumé du modèle
  model_summary <- summary(model)
  
  # Extraire les coefficients et les erreurs standard
  coefficient <- exp(coef(model_summary)[2])  # Coefficient de la variable explicative (transformé en odds ratio)
  std_error <- coef(model_summary)[2, 2]  # Erreur standard du coefficient
  
  # Calculer les intervalles de confiance à 95%
  z_value <- qnorm(0.975)  # Valeur critique pour le calcul de l'intervalle de confiance
  lower_ci <- exp(log(coefficient) - z_value * std_error)
  upper_ci <- exp(log(coefficient) + z_value * std_error)
  
  # Calculer la valeur de p
  p_value <- 2 * (1 - pnorm(abs(coef(model_summary)[2]), 0, std_error))
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = coefficient, 
                                       LowerCI = lower_ci, 
                                       UpperCI = upper_ci,
                                       p_value = p_value,
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)
#################################################################################





#######################################
###calcul par modalités#########

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# Boucle sur chaque variable
for (variable in variables) {
  # Ajuster le modèle de régression logistique en utilisant la variable comme prédicteur
  model <- glm(formula = paste("GROUPE ~", variable), data = df.imp_check2, family = "binomial")
  
  # Extraire les coefficients et les erreurs standard
  coefficients <- coef(model)
  std_errors <- sqrt(diag(vcov(model)))
  
  # Calculer les odds ratios
  odds_ratios <- exp(coefficients)
  
  # Calculer les intervalles de confiance à 95%
  z_value <- qnorm(0.975)
  lower_ci <- exp(coefficients - z_value * std_errors)
  upper_ci <- exp(coefficients + z_value * std_errors)
  
  # Calculer les valeurs de p
  p_values <- 2 * (1 - pnorm(abs(coefficients), 0, std_errors))
  
  # Ajouter les résultats au dataframe en formatant sans la forme avec exposant et avec 2 chiffres après la virgule
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = format(odds_ratios, scientific = FALSE, digits = 2), 
                                       LowerCI = format(lower_ci, scientific = FALSE, digits = 2), 
                                       UpperCI = format(upper_ci, scientific = FALSE, digits = 2), 
                                       p_value = format(p_values, scientific = FALSE, digits = 4),
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)
####################################################


for(i in variables ) {
  print(i)
  tab3.facvar <- table(df.imp_check2[ ,print(i)], useNA = "always", deparse.level = 1 ) 
  print(tab3.facvar) 
}



# En utilisant le grand jeux de données

# Liste des variables du dataframe (à l'exception de GROUPE)
(variables <- names(df.imp_check)[-which(names(df.imp_check) == "GROUPE")])

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# Boucle sur chaque variable
for (variable in variables) {
  # Ajuster le modèle de régression logistique en utilisant la variable comme prédicteur
  model <- glm(formula = paste("GROUPE ~", variable), data = df.imp_check, family = "binomial")
  
  # Extraire les coefficients et les erreurs standard
  coefficients <- coef(model)
  std_errors <- sqrt(diag(vcov(model)))
  
  # Calculer les odds ratios
  odds_ratios <- exp(coefficients)
  
  # Calculer les intervalles de confiance à 95%
  z_value <- qnorm(0.975)
  lower_ci <- exp(coefficients - z_value * std_errors)
  upper_ci <- exp(coefficients + z_value * std_errors)
  
  # Calculer les valeurs de p
  p_values <- 2 * (1 - pnorm(abs(coefficients), 0, std_errors))
  
  # Ajouter les résultats au dataframe en formatant sans la forme avec exposant et avec 2 chiffres après la virgule
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = format(odds_ratios, scientific = FALSE, digits = 2), 
                                       LowerCI = format(lower_ci, scientific = FALSE, digits = 2), 
                                       UpperCI = format(upper_ci, scientific = FALSE, digits = 2), 
                                       p_value = format(p_values, scientific = FALSE, digits = 4),
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)









# VRAI BONBON
#######################################################


######regression lineaire simple##

###calcul par modalités#########

# Liste des variables du dataframe (à l'exception de GROUPE)


variables <- names(df.imp_check3)[-which(names(df.imp_check3) %in% c("GROUPE", "Autres", "divorce", "Aucun", "N21"))]


# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# Boucle sur chaque variable
for (variable in variables) {
  # Ajuster le modèle de régression logistique en utilisant la variable comme prédicteur
  model <- glm(formula = paste("GROUPE ~", variable), data = df.imp_check3, family = "binomial")
  
  # Extraire les coefficients et les erreurs standard
  coefficients <- coef(model)
  std_errors <- sqrt(diag(vcov(model)))
  
  # Calculer les odds ratios
  odds_ratios <- exp(coefficients)
  
  # Calculer les intervalles de confiance à 95%
  z_value <- qnorm(0.975)
  lower_ci <- exp(coefficients - z_value * std_errors)
  upper_ci <- exp(coefficients + z_value * std_errors)
  
  # Calculer les valeurs de p
  p_values <- 2 * (1 - pnorm(abs(coefficients), 0, std_errors))
  
  # Ajouter les résultats au dataframe en formatant sans la forme avec exposant et avec 2 chiffres après la virgule
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = format(odds_ratios, scientific = FALSE, digits = 2), 
                                       LowerCI = format(lower_ci, scientific = FALSE, digits = 2), 
                                       UpperCI = format(upper_ci, scientific = FALSE, digits = 2), 
                                       p_value = format(p_values, scientific = FALSE, digits = 4),
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)

####################################################



##model de regression logistique##


signvar <- c("Elvetudiante", "Suprieur", "Primipare", "N42", "Travailspontane", "hpital",
             "gyncologueobsttricien", "N2ans1", "N12h", "hpital1", "Domicile1",
             "Venuedellemme", "sagefemme1", "gyncologueobsttricien1", "N3500g", "GROUPE", 
             "N3742semaines", "N25")

new_df <- df.imp_check3[, signvar]

##########################################################

# Ajuster le modèle de régression logistique
model <- glm(formula = GROUPE ~ ., data = new_df, family = "binomial")

# Obtenir le résumé du modèle
summary_model <- summary(model)

# Obtenir les coefficients, les intervalles de confiance et les p-values pour chaque variable
coefficients <- summary_model$coefficients
conf_intervals <- confint(model)
p_values <- summary_model$coefficients[, "Pr(>|z|)"]

# Calculer l'odds ratio et ses intervalles de confiance
odds_ratio <- exp(coefficients[, "Estimate"])
odds_ci <- exp(conf_intervals)

# Arrondir les résultats à 3 chiffres après la virgule
results <- data.frame(
  Variable = rownames(coefficients),
  Coefficient = round(coefficients[, "Estimate"], 3),
  Odds_Ratio = round(odds_ratio, 3),
  CI_Lower = round(odds_ci[, 1], 3),
  CI_Upper = round(odds_ci[, 2], 3),
  p_value = round(p_values, 3)
)
print(results)


#######vérification des conditions de normalité du modèle#

hist(resid(model), col= "green")
line(density(model))

#################################################################


















table(df.imp_check2$Profession)
names(df.imp_check2)

###appriement sur les facteurs de confusions##

var_notps<-c('UniqueKey', 'GROUPE', 'ANNEE', 'Asthnie', 'ETABLISSEMENT', 'Nometprnom', 'Hydramnios')
var_ps<-names(df.imp_check2)[!(names(df.imp_check2) %in% var_notps)]
mod_formul<-paste('GROUPE~',paste(var_ps,collapse='+'),sep='')
mod_formul


cal.val<-0.1
matched_df<-matchthem(as.formula(mod_formul), ratio=4, df_imp2, approach='within',
                      method='nearest',caliper=cal.val)


 


##############################################################

str(df.imp_check2)

# Définir la proportion d'appariement souhaitée (1/4)
proportion_appariement <- 0.25

# Calculer le nombre d'observations à inclure dans l'échantillon d'appariement
nombre_appariement <- round(nrow(df.imp_check2) * proportion_appariement)

# Sélectionner un échantillon aléatoire d'observations pour l'appariement
echantillon_appariement <- sample(nrow(df.imp_check2), size = nombre_appariement, replace = FALSE)

# Créer un nouveau data frame contenant uniquement les observations appariées
df_appariement <- df.imp_check2[echantillon_appariement, ]

# Afficher la taille du data frame d'appariement
nrow(df_appariement)

#########################################################

# Définir la proportion d'appariement souhaitée (1/4)
proportion_appariement <- 0.25

# Calculer le nombre d'observations à inclure dans l'échantillon d'appariement
nombre_appariement <- round(nrow(df.imp_check2) * proportion_appariement)

# Sélectionner un échantillon aléatoire d'observations pour l'appariement
indices_appariement <- sample(nrow(df.imp_check2), size = nombre_appariement, replace = FALSE)

# Créer un nouveau data frame contenant uniquement les observations appariées
df_appariement <- df.imp_check2[indices_appariement, ]

# Extraire les facteurs de confusion pour l'appariement
facteurs_confusion <- c("Profession", "Age_cat", "Stat_matri", "Religion")

# Effectuer l'appariement en utilisant la fonction match()
indices_appariement_match <- match(df_appariement[, facteurs_confusion], df.imp_check2[, facteurs_confusion])

# Créer un nouveau data frame avec les observations appariées
df_apparie <- df.imp_check2[indices_appariement_match, ]

# Vérifier l'équilibre des facteurs de confusion entre les groupes appariés
table(df_apparie$GROUPE, df_apparie[, facteurs_confusion])


###########################

