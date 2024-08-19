df_Rul8 <- read.csv2("C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/df_Rul8.csv", stringsAsFactors = T, na.strings = "")

### groupes de variables

var.ql8<-names(Filter(is.factor,subset(df_Rul8, select = -c(1:4))))


## Description des variables###

tabl8 <- df_Rul8[, var.ql8] %>%
  tbl_summary( by= GROUPE, 
               missing_text = "(Missing)") %>%
  add_n() %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  bold_p()
tabl8



##########################



## Bivariate pour representer le forestplot
library(broom)



ql_summary <- function(by_var8) {
  summary <- var.ql[var.ql8!=by_var8] %>%      
    str_c(paste0(by_var8,' ~ '), .) %>%  
    map(.f = ~glm(formula = as.formula(.x),
                  family = "binomial",
                  data = df_Rul8[,var.ql8])) %>%        
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


##### Imputation  & Analysis #####

#################################

#BON

df_toimput8<-subset(df_Rul8, select=-c(UniqueKey, ANNEE,  ETABLISSEMENT, Nometprnom, Hydramnios))

df_imp8<-mice(df_toimput8, seed=29,method='pmm',m=10,maxit=15)#,print=F)
df.imp_check8<-complete(df_imp8)


for(i in var_vec_cré ) {
  tab8.facvar <- table(df_Rul8[ ,print(i)], useNA = "always", deparse.level = 1 ) 
  print(tab8.facvar) 

  str(df.imp_check8)
  
save( list=c("df.imp_check8", "df_toimput8", "df_imp8", "df_Rul8"), file="teststage_environnement8.Rdata")


##############################
  ####descriptions des variables après imputations####
 
#BON

  var.qll8<-names(Filter(is.factor,df.imp_check8))
  
  tabl_8 <- df.imp_check8[, var.qll8] %>%
    tbl_summary( by= GROUPE, 
                 missing_text = "(Missing)") %>%
    add_n() %>%
    add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
    bold_p()
  tabl_8
  
  str(df.imp_check8)
 
  summary(df.imp_check8)

  
  ### verification et remplacement des valeurs manquantes ###
  df.imp_check8[!complete.cases(df.imp_check8), ]
  
  table(df.imp_check2$Asthnie, useNA = "always")
df.imp_check8$N2[126]<- "OUI"
df.imp_check8$N21[126]<-"NON"  
df.imp_check8$Asthnie[126]<- "OUI"
df.imp_check8$N2ans[95]<- "OUI"
df.imp_check8$N2ans[231]<- "OUI"

df.imp_check8[c(126, 126, 126, 95, 231), c("N2", "N21", "Asthnie", "N2ans", "N2ans")] <- c("OUI", "NON", "OUI", "OUI", "OUI")

str(df.imp_check8)

##################################################
#Appriement


match_data8 <- matchit(GROUPE ~ ., data = df.imp_check8, method = "nearest", ratio = 1 / 4, replace = FALSE)

match_data8 <- matchit(GROUPE ~ ., data = df.imp_check8, method = "nearest", ratio = 4, replace = FALSE)

1+1
# Vérifier la classe et les niveaux des variables
infos_variables <- sapply(df.imp_check8, function(x) list(classe = class(x), niveaux = nlevels(x)))
variables_problematic = names(infos_variables[infos_variables$niveaux < 2 | infos_variables$classe != "factor"])
variables_problematic

## N2530,N3540, N40, Menagere, Fonctionnaire, marie, clibataire, veuve, divorce, Catholique, musulman, Primaire, N42semaines,
N24, Paucipare, Multipare, Grandemultipare, rsidentengyncologie, ATCDmyomectomie, N48h, Domicile,
Rfre, Utilisationocytocine, Manuvresobsttricales, rsident, N4000g, N11, N2, N21,TRAITEMENT,
Siegedelarupture, Typederupture, Lsionsvsicales

df.imp_check8b <- subset(df.imp_check8, select= -c(Autres, divorce, Aucun, N21,
                                                   N2530,N3540, N40, Menagere, Fonctionnaire, marie, clibataire, veuve, divorce, Catholique, musulman, Primaire, N42semaines,
                                                   N24, Paucipare, Multipare, Grandemultipare, rsidentengyncologie, ATCDmyomectomie, N48h, Domicile,
                                                   Rfre, Utilisationocytocine, Manuvresobsttricales, rsident, N4000g, N11, N2, N21,TRAITEMENT,
                                                   Siegedelarupture, Typederupture, Lsionsvsicales))

df.imp_check8b$GROUPE <- relevel(df.imp_check8b$GROUPE, ref = "TEMOINS")

match_data8b <- matchit(GROUPE ~ ., data = df.imp_check8b, method = "nearest", ratio =1, replace = FALSE)
matched_data8b <- match.data(match_data8b)

summary(matched_data8b)
summary(match_data8b)
table(matched_data8b$GROUPE)
table(df.imp_check8b$GROUPE)
levels(df.imp_check8b$GROUPE)
str(df.imp_check8b)
str(df.imp_check8)

plot(match_data8b, type='hist')
plot(match_data8b, type='hist')

infos_variables8b <- sapply(df.imp_check8b, function(x) list(classe = class(x), niveaux = nlevels(x)))


str(df.imp_check8b)

# test avec le model linéaire généralisé
sp <- glm(GROUPE ~ ., data = df.imp_check8b, family="binomial", control = glm.control(maxit = 1000))
boxplot(sp)
hist(resid(sp))

exp(coef(sp))
exp(confint(sp))


summary(sp)
########################################################
# Appariement avec le jeu de donnée des variables uniques crées######

df.imp_check2
df.imp_check2$GROUPE <- relevel(df.imp_check2$GROUPE, ref = "TEMOINS")

match_data2 <- matchit(GROUPE ~ ., data = df.imp_check2, method = "nearest", ratio =1, replace = FALSE, calipter= 0.001)

matched_data2 <- match.data(match_data2)

summary(matched_data2)
summary(match_data2)
str(df.imp_check2)
plot(match_data2, type='hist')
plot(match_data8b, type='hist')
#################

df.imp_check2b <- subset(df.imp_check2, select = -c(Religion, HU, ATCDcuretage, Manuvresobsttricales, TRAITEMENT, Etatgnralaltrconjonctivespales
                                                   ,Asthnie, palpationduftussouslapeau, Hmatomeduligamentlarge , NNEVIVANT, MEREVIVANTE  ))

match_data2b <- matchit(GROUPE ~ ., data = df.imp_check2b, method = "subclass", ratio =1, replace = FALSE)

matched_data2b <- match.data(match_data2b)



summary(matched_data2b)
summary(match_data2b)
str(df.imp_check2b)
plot(match_data2b, type='hist')

exp()

str(df.imp_check2b)

sp2b <- glm(GROUPE ~ ., data = matched_data2b, family="binomial")
hist(resid(sp2b), col="green")
boxplot(sp2b)
qqnorm(resid(sp2b))
qqline(resid(sp2b))





#######################

## l'appariement est valables avec les variables binaires discretes ou continues ###


# Sélectionnez uniquement les variables catégorielles

cat_vars <- sapply(df.imp_check8b, is.factor)
 
  factor_names <- names(df.imp_check8b)[sapply(df.imp_check8b, is.factor)]

# Convertissez les variables catégorielles en variables binaires
df.imp_check8b1 <- cbind(df.imp_check8b, model.matrix(~.-1, data = df.imp_check8b[, cat_vars]))

df.imp_check8b2 <- df.imp_check8b1[, setdiff(names(df.imp_check8b1), factor_names)]


cal.val<-0.1
match_data8b <- matchit(GROUPECAS ~ ., data = df.imp_check8b2, method = "nearest", ratio = 1, replace = FALSE)
matched_data8b <- match.data(match_data8b)
?matchit

str(df.imp_check8b )
summary(match_data8b)

plot(match_data8b, type='hist')
plot(match_data8b, type='hist')

################# verification de l'appariément####

library(Matching)
library(ggplot2)

# Données d'origine
ggplot(matched_data8b, aes(x = GROUPECAS, fill = GROUPECAS)) +
  geom_density(alpha = 0.5) +
  geom_density(data = df.imp_check8b[match_data8b$match.matrix, ], aes(x = GROUPECAS, fill = GROUPECAS),
               alpha = 0.5) +
  labs(title = "Courbes de densité avant et après l'appariement",
       x = "Groupe", y = "Densité") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Traités", "Contrôles"))

#######################################

library(ggplot2)

df.imp_check8b$GROUPECAS <- as.numeric(df.imp_check8b$GROUPECAS)

ggplot(df.imp_check8b, aes(x = GROUPECAS, fill = factor(GROUPECAS))) +
  geom_density(alpha = 0.5) +
  geom_density(data = df.imp_check8b[match_data8b$match.matrix, ], aes(x = GROUPECAS, fill = factor(GROUPECAS)),
               alpha = 0.5) +
  labs(title = "Courbes de densité avant et après l'appariement",
       x = "Groupe", y = "Densité") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Traités", "Contrôles"))


#########################

library(ggplot2)

df.imp_check8b$GROUPECAS <- as.numeric(df.imp_check8b$GROUPECAS)

ggplot(df.imp_check8b, aes(x = GROUPECAS, fill = factor(GROUPECAS))) +
  geom_density(alpha = 0.5) +
  geom_density(data = df.imp_check8b[match_data8b$match.matrix, ], aes(x = GROUPECAS, fill = factor(GROUPECAS)),
               alpha = 0.5) +
  labs(title = "Courbes de densité avant et après l'appariement",
       x = "Groupe", y = "Densité") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Traités", "Contrôles"))
1+1
#########################

library(Matching)
library(ggplot2)

# Données d'origine
ggplot(df.imp_check8b, aes(x = GROUPECAS, fill = GROUPECAS)) +
  geom_density(alpha = 0.5) +
  geom_density(data = df.imp_check8b[match_data8b$match.matrix, ], aes(x = GROUPECAS, fill = GROUPECAS),
               alpha = 0.5) +
  labs(title = "Courbes de densité avant et après l'appariement",
       x = "Groupe", y = "Densité") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Traités", "Contrôles"))

######################

library(ggplot2)

df.imp_check8b$GROUPECAS <- as.numeric(df.imp_check8b$GROUPECAS)

ggplot(df.imp_check8b, aes(x = GROUPECAS, fill = factor(GROUPECAS))) +
  geom_density(alpha = 0.5) +
  geom_density(data = df.imp_check8b[match_data8b$match.matrix, ], aes(x = GROUPECAS, fill = factor(GROUPECAS)),
               alpha = 0.5) +
  labs(title = "Courbes de densité avant et après l'appariement",
       x = "Groupe", y = "Densité") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Traités", "Contrôles"))
#####################

library(ggplot2)

df.imp_check8b$GROUPECAS <- as.factor(df.imp_check8b$GROUPECAS)

ggplot(df.imp_check8b, aes(x = GROUPECAS, fill = GROUPECAS)) +
  geom_density(alpha = 0.5) +
  geom_density(data = df.imp_check8b[match_data8b$match.matrix, ], aes(x = GROUPECAS, fill = GROUPECAS),
               alpha = 0.5) +
  labs(title = "Courbes de densité avant et après l'appariement",
       x = "Groupe", y = "Densité") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Traités", "Contrôles"))



sp <- glm(GROUPECAS ~ ., data = df.imp_check8b, family="binomial")
hist(resid(sp))
exp(coef(sp))

boxplot(df.imp_check8b$GROUPECAS)

str(df.imp_check8b)
summary(df.imp_check8b)

#variables significatives
signvar8 <- c("ElvetudianteOUI", "PrimipareOUI", "N42OUI", "TravailspontaneOUI", "hpitalOUI",
             "gyncologueobsttricienOUI", "N2ans1OUI", "N12hOUI", "hpital1OUI", "Domicile1OUI",
             "VenuedellemmeOUI", "sagefemme1OUI", "gyncologueobsttricien1OUI", "N3500gOUI", "GROUPECAS")


df.imp_check8b3 <- df.imp_check8b2[, signvar8]

model_Rupl <- glm(GROUPECAS ~ ., data = df.imp_check8b3, family="binomial")
hist(resid(model_Rupl), col="green", main= "histogramme des residus du model")
boxplot(model_Rupl)
qqnorm(resid(model_Rupl))
qqline(resid(model_Rupl))
str(df.imp_check8b2)

cal.val <-0.1
match_data8b3 <- matchit(GROUPECAS ~ ., data = df.imp_check8b3, method = "nearest", ratio =4, replace = FALSE)
matched_data8b3 <- match.data(match_data8b3)
str(matched_data8b3 )

summary(match_data8b3)

a <- glm(GROUPECAS ~ ., data = matched_data8b3, family="binomial")

hist(resid(a))
exp(coef(a))
exp(confint(a))

plot(matched_data8b3, type='hist')
plot(match_data8b3, type='hist')


match_data8b3 <- matchit(GROUPECAS ~ ., data = df.imp_check8b3, method = "subclass", ratio =1, replace = FALSE)
matched_data8b3 <- match.data(match_data8b3)
summary(match_data8b3)
plot(match_data8b3, type='hist')

exp


########################################

#########réalisation du forest plot

var.ql[!complete.cases(df.imp_check2),]
var.ql<-names(Filter(is.factor,df.imp_check2for))
df.imp_check2for <- subset(df.imp_check2, select = c(GROUPE, Age_cat,  Religion, Age_gest, Mod_trav, 
                                                     Nbre_foet, Espa_intge, Nbre_cpn, Lieu_Ccpn, Poids_foet, Bishop , Mod_adm, Parité, Niv_scol ))

str(df.imp_check2)


df.imp_check8[!complete.cases(df.imp_check8),] 

str(df.imp_check8)

## Bivariate pour representer le forestplot
library(broom)



ql_summary <- function(by_var) {
  summary <- var.ql[var.ql!=by_var] %>%      
    str_c(paste0(by_var,' ~ '), .) %>%  
    map(.f = ~glm(formula = as.formula(.x),
                  family = "binomial",
                  data = df.imp_check2for[,var.ql])) %>%        
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
    width = 700, height = 900)
ggplot(data = sum_ql,
       aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, col = group)) +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_vline(xintercept = (1:length(sum_ql$term)) + 0.5, linetype = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.5, cex = 1,
                position = position_dodge(width = 0.6)) +
  scale_color_manual(values = c("GROUPE" = "red")) +
  scale_y_log10() +
  labs(x = NULL, y = NULL) +
  guides(color = guide_legend(reverse = TRUE)) +
  theme(legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(color = "black", face = "bold")) +  # Modifier la couleur et le style de l'écriture sur l'axe des ordonnées
  coord_flip()

dev.off()





############################

##forestplot avec le jeu de donnée imputé#####
####GROUPE,N2530,  N3035, N3540, Menagere,Commerante, Fonctionnaire, N5
df.imp_check8plot <- subset(df.imp_check8, select= c(GROUPE,N2530,  N3035, N3540, Menagere,Commerante, Fonctionnaire, 
                                                      N5, Manuvresobsttricales, Utilisationocytocine, N71, N7, centredesante ,
                                                     Mdecingnraliste, Travailinduit, Niv_scol ))

var.ql<-names(Filter(is.factor,df.imp_check8plot))
uu
var.ql<-names(Filter(is.factor,df.imp_check8plot))
var.ql
str(df.imp_check8)
str(df.imp_check8plot)

var.ql <- var.ql[!var.ql %in% c("Elvetudiante", "Catholique", "Protestante", "musulman", "pentectiste",
                                "N3742semaines", "N1", "N24", "N5", "Primipare", "Internes", "Travailspontane",
                                "rsidentengyncologie", "hpital", "Domicile", "Rfre" , "Venuedellemme" ,
                                "gyncologueobsttricien1", "Interne"  , "rsident", "sagefemme1", "N2", "Transfusion",
                               "Siegedelarupture", "Typederupture" )]

## Bivariate pour representer le forestplot
library(broom)



ql_summary <- function(by_var) {
  summary <- var.ql[var.ql!=by_var] %>%      
    str_c(paste0(by_var,' ~ '), .) %>%  
    map(.f = ~glm(formula = as.formula(.x),
                  family = "binomial",
                  data = df.imp_check8plot[,var.ql])) %>%        
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



library(readxl)

 forpl <- data.frame()
 
 forpl <- read_excel("C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/OR SIGNIFICATIFS .xlsx")

 results
 
 str(forpl) 
 df_Rul3
 
 # Charger la bibliothèque dplyr pour manipuler les données
 library(dplyr)
 
 # Transformer les odds ratios en log-odds
 forpl <- forpl %>%
   mutate(LogOR = log(OR))
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 # Chargement de la bibliothèque ggplot2 (si elle n'est pas déjà chargée)
 library(ggplot2)
 
 # Création du forest plot avec ggplot2
 forest_plot2 <- ggplot(forpl, aes(x = OR, xmin = Lower_OR, xmax = Upper_OR, y = Variables)) +
   geom_point() +
   geom_errorbarh(height = 0.1) +
   labs(title = "Forest Plot des Variables et de leurs OR",
        x = "Rapport de Cotes (OR)",
        y = "Variables") +
   theme_minimal()
 
 # Affichage du forest plot
 print(forest_plot2)
 
 
 
 
############################33
 
 
 # Chargement de la bibliothèque ggplot2 (si elle n'est pas déjà chargée)
 library(ggplot2)
 
 # Création du forest plot avec ggplot2
 forest_plot <- ggplot(forpl, aes(x = OR, xmin = Lower_OR, xmax = Upper_OR, y = Variables)) +
   geom_point() +
   geom_errorbarh(height = 0.1) +
   labs(title = "Forest Plot des Variables et de leurs OR",
        x = "Rapport de Cotes (OR)",
        y = "Variables") +
   scale_x_log10(breaks = c(0.01, 0.1, 1, 10), labels = c("0.01", "0.1", "1", "10")) +  # Échelle logarithmique personnalisée
   theme_minimal()
 
 # Affichage du forest plot
 print(forest_plot)

 ##################################################
 
 
 # Chargement de la bibliothèque ggplot2 (si elle n'est pas déjà chargée)
 library(ggplot2)
 
 # Création du forest plot avec ggplot2
 forest_plot <- ggplot(forpl, aes(x = OR, xmin = Lower_OR, xmax = Upper_OR, y = Variables)) +
   geom_point(color = "blue", size = 3) +              # Mettre les points en bleu
   geom_errorbarh(height = 0.1, color = "blue") +      # Mettre les barres d'erreur en bleu
   geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1.2) +  # Ajouter un trait vertical en pointillé à partir de x = 1
   labs(title = "Forest Plot des Variables et de leurs OR",
        x = "Rapport de Cotes (OR)",
        y = "Variables") +
   scale_x_log10(breaks = c(0.01, 0.1, 1, 10), labels = c("0.01", "0.1", "1", "10")) +  # Échelle logarithmique personnalisée
   theme_minimal() +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +   # Supprimer les lignes de grille
   theme(axis.text.y = element_text(size = 12)) +        # Ajuster la taille du texte de l'axe y
   theme(axis.text.x = element_text(size = 12)) +        # Ajuster la taille du texte de l'axe x
   theme(axis.title = element_text(size = 14, face = "bold"))  # Ajuster la taille et le style du titre des axes
 
 # Affichage du forest plot
 print(forest_plot)
 
 #####################################
 
 # Chargement de la bibliothèque ggplot2 (si elle n'est pas déjà chargée)
 library(ggplot2)
 
 # Création du forest plot avec ggplot2
 forest_plot <- ggplot(forpl, aes(x = OR, xmin = Lower_OR, xmax = Upper_OR, y = Variables)) +
   geom_point(color = "blue", size = 3) +              # Mettre les points en bleu
   geom_errorbarh(height = 0.1, color = "blue") +      # Mettre les barres d'erreur en bleu
   geom_vline(xintercept = 1, linetype = "dashed", color = "black", size = 1.2) +  # Ajouter un trait vertical en pointillé à partir de x = 1
   labs(title = "Forest Plot des Variables et de leurs OR",
        x = "Rapport de Cotes (OR)",
        y = "Variables") +
   scale_x_log10(breaks = c(0.01, 0.1, 1, 10), labels = c("0.01", "0.1", "1", "10")) +  # Échelle logarithmique personnalisée
   theme_minimal() +
   theme(panel.grid.major = element_line(color = "gray", linetype = "dotted")) +   # Ajouter le quadrillage en pointillé gris
   theme(axis.text.y = element_text(size = 12)) +        # Ajuster la taille du texte de l'axe y
   theme(axis.text.x = element_text(size = 12)) +        # Ajuster la taille du texte de l'axe x
   theme(axis.title = element_text(size = 14, face = "bold"))  # Ajuster la taille et le style du titre des axes
 
 # Affichage du forest plot
 print(forest_plot)
 
 #######################################""""
 