install.packages("readxl")

library(readxl)

df_Rul <- read.csv2("/Users/TCHEUMENI/Documents/STAGE DE MASTER/df_RupUl.csv", stringsAsFactors = T, na.strings = "")

str(df_Rul)
facvar_Rul <- names((Filter(is.factor, df_Rul)))


#####vérification des modalités de  chaques variables####

for(i in facvar_Rul ) {
  tab.facvar <- table(df_Rul[ ,print(i)], useNA = "always", deparse.level = 1 ) 
  print(tab.facvar) 
}









prettyR::describe(df_Rup.Ulcsv, num.desc= c("mean", "min", "max", "pctmiss", "nmiss", "q1", "q3",
                                            "valid.n", "median", "var"))

facvar_Rul <- names((Filter(is.factor, df_Rul)))

rm(list=ls())





levels(df_Rup.Ulcsv$var_rupul)

labels(df_Rup.Ulcsv$var_rupul)


levels(df_Rup.Ulcsv$Domicile)<- ifelse(levels(df_Rup.Ulcsv$Domicile)[1]=="", NA, df_Rup.Ulcsv$Domicile)
table(df_Rup.Ulcsv$Domicile, useNA = "always")
labels(df_Rup.Ulcsv$Domicile)

levels(df_Rup.Ulcsv$Domicile)




table(df_Rup.Ulcsv$Domicile, useNA = "always")
levels(df_Rup.Ulcsv$Domicile)[1] <-NA

barplot(table(df_Rup.Ulcsv$Domicile), col = "pink")

function(var_rupul){
  if ()
}

for(i in var_rupul ){
  if(levels(df_Rup.Ulcsv$i)=="")
    levels(df_Rup.Ulcsv$i) <- "NA"
  return(levels(df_Rup.Ulcsv$i))
}













function(var_rupul ){
  print(i)
  if(levels(df_Rup.Ulcsv$i)[1]=="") {levels(df_Rup.Ulcsv$i)[1] <-"NA" }
  print(levels(df_Rup.Ulcsv$i))
} 

colnames(df_Rup.Ulcsv)

table(df_Rup.Ulcsv$Hydramnios, useNA = 'always')

levels(df_Rup.Ulcsv$Hydramnios)[1] <- 'NA'


summary(df_Rup.Ulcsv)

table(df_Rup.Ulcsv$Transfusion )

which(df_Rup.Ulcsv$Transfusion=="")

df_Rup.Ulcsv[126,"Transfusion"]


for(i in var_rupul){
  print(i)
}

var_rupul



df_var_rupul <-df_Rup.Ulcsv[, var_rupul]


str(df_var_rupul)

for(i in length(var_rupul) ){
  
  print(i)
  
}

levels(df_Rup.Ulcsv$ETABLISSEMENT)








df_Rul <- read.csv2("/Users/TCHEUMENI/Documents/STAGE DE MASTER/df_RupUl.csv", stringsAsFactors = T, na.strings = "")



str(df_Rul)





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

### groupes de variables

var.ql<-names(Filter(is.factor,subset(df_Rul2, select = -c(1:4))))

(var.qlln <- var.qll[1:5])


## Description des variables###

tabl1 <- df_Rul2[, var.ql] %>%
  tbl_summary( by= GROUPE, 
               missing_text = "(Missing)") %>%
  add_n() %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  bold_p()
tabl1





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

df_toimput<-subset(df_Rul, select=-c(UniqueKey, ANNEE,  ETABLISSEMENT, Nometprnom, Hydramnios))

df_imp<-mice(df_toimput, seed=29,method='pmm',m=10,maxit=15)#,print=F)
df.imp_check<-complete(df_imp)


for(i in var_vec_cré ) {
  tab2.facvar <- table(df_Rul2[ ,print(i)], useNA = "always", deparse.level = 1 ) 
  print(tab2.facvar) 
}


df_toimput2<-subset(df_Rul2, select=-c(UniqueKey, ANNEE,  ETABLISSEMENT, Nometprnom, Hydramnios))

df_imp2<-mice(df_toimput2, seed=29,method='pmm',m=10,maxit=15)#,print=F)
df.imp_check2<-complete(df_imp2)
str(df.imp_check2)



for(i in var_vec_cré ) {
  tab2.facvar <- table(df.imp_check2[ ,print(i)], useNA = "always", deparse.level = 1 ) 
  print(tab2.facvar) 
}




# Vérification imputation
png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/imputation-stripplot-.png')
stripplot(df_imp2,Age_cat+Religion+Parité+Nbre_cpn+Deces+Espa_intge+ Lieu_tra+Bishop+Mod_adm~.imp)
dev.off()
png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/imputation-bwplot-.png')
bwplot(df_imp2, Age_cat+Religion+Parité+Nbre_cpn+Deces+Espa_intge+ Lieu_tra+Bishop+Mod_adm~.imp)
dev.off()

png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/imputation-densityplot-.png')
densityplot(~Age_cat+Religion+Parité+Nbre_cpn+Deces+Espa_intge+ Lieu_tra+Bishop+Mod_adm, data=df.imp_check2)
dev.off()

png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/imputation-densityplot2-.png')
densityplot(df_imp2)
dev.off()

save(list = ls(), file= ('teststage_environnement.Rdata'))
getwd()
setwd("C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine")
save.image("ENVI AA2.Rdata")


# Sauvegarde du graphique en tant qu'image PNG
ggsave("C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/imputation-densityplot2-.png")

###appriement sur les facteurs de confusions##

var_notps<-c('UniqueKey', 'GROUPE', 'ANNEE', 'Asthnie', 'ETABLISSEMENT', 'Nometprnom', 'Hydramnios')
var_ps<-names(df_toimput2)[!(names(df_toimput2) %in% var_notps)]
mod_formul<-paste('GROUPE~',paste(var_ps,collapse='+'),sep='')
mod_formul


cal.val<-0.10
matched_df<-matchthem(as.formula(mod_formul), ratio=4, df_imp2, approach='within',
                      method='nearest',caliper=cal.val)




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




# Installer le package "epitools" s'il n'est pas déjà installé
if (!require("epitools")) {
  install.packages("epitools")
}

# Charger le package
library(epitools)

# Effectuer une statistique descriptive bivariée pour le groupe en fonction de toutes les autres variables
bivariate_summary <- summary(df.imp_check$GROUPE ~ ., data = df.imp_check, method = "case.control")

# Afficher les résultats (odds ratios et p-values)
print(bivariate_summary)




# Installer le package "vcd" s'il n'est pas déjà installé
if (!require("vcd")) {
  install.packages("vcd")
}

# Charger le package
library(vcd)

# Effectuer une statistique descriptive bivariée de toutes les autres variables en fonction de la variable GROUPE
bivariate_summary <- lapply(df.imp_check2[, -which(names(df.imp_check2) == "GROUPE")], function(x) {
  assocstats(table(df.imp_check2$GROUPE, x))
})

# Afficher les résultats (odds ratios et p-values)
for (i in 1:length(bivariate_summary)) {
  var_name <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")][i]
  print(paste("Variable:", var_name))
  print(bivariate_summary[[i]])
}




# Installer les packages nécessaires s'ils ne sont pas déjà installés
if (!require("dplyr")) {
  install.packages("dplyr")
}
if (!require("broom")) {
  install.packages("broom")
}
if (!require("epitools")) {
  install.packages("epitools")
}

# Charger les packages
library(dplyr)
library(broom)
library(epitools)

# Créer une fonction pour calculer les proportions, les odds ratios, les intervalles de confiance et les p-values
calculate_stats <- function(var) {
  table <- table(df.imp_check2$GROUPE, df.imp_check2[[var]])
  result <- prop.test(table)
  result_df <- data.frame(
    Variable = var,
    OR = result$estimate[2] / result$estimate[1],
    OR_CI = confint(result, method = "exact")[2, ],
    p_value = result$p.value
  )
  result_df
}

# Appliquer la fonction calculate_stats à toutes les autres variables
results <- lapply(names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")], calculate_stats)

# Regrouper les résultats en un seul data frame
result_df <- bind_rows(results)

# Afficher le tableau des résultats
print(result_df)

str(df.imp_check2)
1+1

bivariate_analysis <- function(var_name) {
  table <- table(df.imp_check2$GROUPE, df.imp_check2[[var_name]])
  proportions <- prop.table(table, margin = 1)
  result <- data.frame(table, proportions)
  
  # Calculate odds ratios and p-values
  or <- round(oddsratio(table), 2)
  p_value <- round(chisq.test(table)$p.value, 3)
  
  # Add odds ratios and p-values to the result data frame
  result$OR <- or[1, ]
  result$OR_CI <- paste("(", or[2, ], "-", or[3, ], ")")
  result$P_Value <- p_value
  
  return(result)
}

# Appliquer la fonction bivariate_analysis à toutes les autres variables
results <- lapply(names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")], bivariate_analysis)




# Chargement du package "epitools" pour calculer les odds ratios et les intervalles de confiance
install.packages("epitools")
library(epitools)

# Variables explicatives
variables_explicatives <- colnames(df.imp_check2)[!colnames(df.imp_check2) %in% "GROUPE"]

# Création d'un tableau pour stocker les résultats
resultats <- matrix(NA, nrow = length(variables_explicatives), ncol = 5)
colnames(resultats) <- c("Variable", "Modalité", "Proportion", "OR", "P-value")

# Boucle pour calculer les statistiques pour chaque variable explicative
for (i in 1:length(variables_explicatives)) {
  var <- variables_explicatives[i]
  table_cross <- table(df.imp_check2$GROUPE, df.imp_check2[[var]])
  
  # Calcul des proportions
  proportions <- prop.table(table_cross, margin = 1)
  
  # Calcul des odds ratios et des intervalles de confiance
  or <- oddsratio(table_cross, method = "wald")
  
  # Calcul des p-values
  p_value <- prop.test(table_cross)[["p.value"]]
  
  # Stockage des résultats dans le tableau
  resultats[i, 1] <- var
  resultats[i, 2] <- rownames(table_cross)[1]
  resultats[i, 3] <- proportions[1, 1]
  resultats[i, 4] <- or$measure[1]
  resultats[i, 5] <- p_value
  
  resultats[i + length(variables_explicatives), 1] <- var
  resultats[i + length(variables_explicatives), 2] <- rownames(table_cross)[2]
  resultats[i + length(variables_explicatives), 3] <- proportions[2, 1]
  resultats[i + length(variables_explicatives), 4] <- or$measure[2]
  resultats[i + length(variables_explicatives), 5] <- p_value
}

# Affichage des résultats
print(resultats)

str(df.imp_check2)
1+1



# Chargement du package "epitools" pour calculer les odds ratios et les intervalles de confiance
install.packages("epitools")
library(epitools)

# Variables explicatives
variables_explicatives <- colnames(df.imp_check2)[!colnames(df.imp_check2) %in% "GROUPE"]

# Création d'un tableau pour stocker les résultats
resultats <- matrix(NA, nrow = length(variables_explicatives), ncol = 4)
colnames(resultats) <- c("Variable", "OR", "IC", "P-value")

# Boucle pour calculer les statistiques pour chaque variable explicative
for (i in 1:length(variables_explicatives)) {
  var <- variables_explicatives[i]
  table_cross <- table(df.imp_check2$GROUPE, df.imp_check2[[var]])
  
  # Calcul des odds ratios et des intervalles de confiance
  or <- oddsratio(table_cross, method = "wald")
  ci <- confint(or, method = "wald")
  
  # Calcul des p-values
  p_value <- prop.test(table_cross)[["p.value"]]
  
  # Stockage des résultats dans le tableau
  resultats[i, 1] <- var
  resultats[i, 2] <- or$measure[1]
  resultats[i, 3] <- paste0("[", ci[1, 1], ", ", ci[1, 2], "]")
  resultats[i, 4] <- p_value
}

# Conversion des résultats en data frame
resultats <- as.data.frame(resultats)
colnames(resultats) <- c("Variable", "OR", "IC", "P-value")

# Affichage des résultats
print(resultats)

1+1
