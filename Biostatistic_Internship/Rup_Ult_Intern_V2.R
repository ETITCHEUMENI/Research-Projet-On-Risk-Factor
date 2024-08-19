# Installer les packages "rentrez" et "dplyr" s'ils ne sont pas déjà installés
if (!require("rentrez")) {
  install.packages("rentrez")
}
if (!require("dplyr")) {
  install.packages("dplyr")
}

# Charger les packages
library(rentrez)
library(dplyr)

# Paramètres de recherche
search_term <- "facteurs de risques des ruptures utérines"
db <- "pubmed"
retmax <- 10  # Nombre maximum d'articles à récupérer

# Effectuer une recherche spécifique à l'Hôpital Laquintinie de Douala
search_results_laquintinie <- entrez_search(db=db, term=paste0(search_term, " AND Laquintinie[AD]"), retmax=retmax)
id_list_laquintinie <- search_results_laquintinie$ids

# Effectuer une recherche spécifique à l'Hôpital Général de Douala
search_results_general <- entrez_search(db=db, term=paste0(search_term, " AND General[AD]"), retmax=retmax)
id_list_general <- search_results_general$ids

# Fusionner les listes d'identifiants d'articles des deux hôpitaux
id_list <- union(id_list_laquintinie, id_list_general)

# Récupérer les informations détaillées des articles
articles <- entrez_fetch(db=db, id=id_list, rettype="abstract", retmode="text")

# Afficher les articles
print(articles)

# Effectuer une recherche générale pour les autres hôpitaux
search_results_general <- entrez_search(db=db, term=search_term, retmax=retmax)
id_list_general <- search_results_general$ids

# Récupérer les informations détaillées des articles des autres hôpitaux
articles_general <- entrez_fetch(db=db, id=id_list_general, rettype="abstract", retmode="text")

# Afficher les articles des autres hôpitaux
print(articles_general)



# Installer les packages "rentrez" et "rjson" s'ils ne sont pas déjà installés
if (!require("rentrez")) {
  install.packages("rentrez")
}
if (!require("rjson")) {
  install.packages("rjson")
}

# Charger les packages
library(rentrez)
library(rjson)

# Paramètres de recherche
search_term <- "facteurs de risques des ruptures utérines"
retmax <- 100  # Nombre maximum d'articles à récupérer

# Fonction pour effectuer une recherche dans une base de données spécifiée
search_articles <- function(database) {
  search_results <- entrez_search(db=database, term=search_term, retmax=retmax)
  id_list <- search_results$ids
  if (length(id_list) > 0) {
    articles <- entrez_fetch(db=database, id=id_list, rettype="abstract", retmode="text")
    return(articles)
  } else {
    return(NULL)
  }
}

# Effectuer une recherche spécifique à l'Hôpital Laquintinie de Douala
articles_laquintinie <- search_articles("pubmed")

# Effectuer une recherche spécifique à l'Hôpital Général de Douala
articles_general <- search_articles("pubmed")

# Afficher les articles de l'Hôpital Laquintinie de Douala
if (!is.null(articles_laquintinie)) {
  print(articles_laquintinie)
} else {
  cat("Aucun résultat trouvé pour l'Hôpital Laquintinie de Douala.\n")
}

# Afficher les articles de l'Hôpital Général de Douala
if (!is.null(articles_general)) {
  print(articles_general)
} else {
  cat("Aucun résultat trouvé pour l'Hôpital Général de Douala.\n")
}

# Effectuer une recherche générale pour les autres hôpitaux dans PubMed
articles_pubmed <- search_articles("pubmed")

# Afficher les articles des autres hôpitaux dans PubMed
if (!is.null(articles_pubmed)) {
  print(articles_pubmed)
} else {
  cat("Aucun résultat trouvé pour les autres hôpitaux dans PubMed.\n")
}

# Effectuer une recherche dans la base de données Embase
articles_embase <- search_articles("embase")

# Afficher les articles des autres hôpitaux dans Embase
if (!is.null(articles_embase)) {
  print(articles_embase)
} else {
  cat("Aucun résultat trouvé pour les autres hôpitaux dans Embase.\n")
}

str(df_Rul2)
var.ql<-names(Filter(is.factor,subset(df_Rul2, select = -c(1:4))))
tabl1 <- df_Rul2[, var.ql] %>%
  tbl_summary( by= GROUPE, 
               missing_text = "(Missing)") %>%
  add_n() %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_odds_ratio(by = GROUPE, exponentiate = TRUE) %>%
  bold_p()
tabl1


#############

library(gtsummary)
library(Epi)
install.packages("epiR")

var.ql <- names(Filter(is.factor, subset(df_Rul2, select = -c(1:4))))

tabl1 <- df_Rul2[, var.ql] %>%
  tbl_summary(by = GROUPE, missing_text = "(Missing)") %>%
  add_n() %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2))

# Calculate odds ratio and confidence interval
odds_ratio_table <- lapply(var.ql, function(var) {
  tbl <- df_Rul2[, c(var, "GROUPE")]
  odds <- odds.test(tbl, method = "midp")
  ci <- confint(odds)
  tbl_summary(odds, ci, label = var)
})

# Combine odds ratio tables
tabl1 <- modify_table(tabl1, modify_fun = add_nevent_column)
tabl1 <- add_table_attributes(tabl1, data = lapply(odds_ratio_table, "[[", 1), label = "Odds Ratio")
tabl1 <- add_table_attributes(tabl1, data = lapply(odds_ratio_table, "[[", 2), label = "95% CI")

tabl1
##################

library(gtsummary)
library(survey)

var.ql <- names(Filter(is.factor, subset(df_Rul2, select = -c(1:4))))

tabl1 <- df_Rul2[, var.ql] %>%
  tbl_summary(by = GROUPE, missing_text = "(Missing)") %>%
  add_n() %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2))

# Create survey design object
design <- svydesign(ids = ~1, data = df_Rul2)

# Calculate odds ratio and confidence interval
odds_ratio_table <- lapply(var.ql, function(var) {
  tbl <- df_Rul2[, c(var, "GROUPE")]
  tbl_design <- svytable(~GROUPE + .data[[var]], design = design)
  odds <- svyoddsratio(tbl_design, method = "logit")
  ci <- confint(odds)
  tbl_summary(odds, ci, label = var)
})

# Combine odds ratio tables
tabl1 <- modify_table(tabl1, modify_fun = add_nevent_column)
tabl1 <- add_table_attributes(tabl1, data = lapply(odds_ratio_table, "[[", 1), label = "Odds Ratio")
tabl1 <- add_table_attributes(tabl1, data = lapply(odds_ratio_table, "[[", 2), label = "95% CI")

tabl1
####################

library(gtsummary)
library(survey)

# Create survey design object
design <- svydesign(ids = ~1, data = df_Rul2)

# Calculate odds ratio and confidence interval
odds_ratio_table <- lapply(var.ql, function(var) {
  tbl_design <- svytable(as.formula(paste0("~ GROUPE + ", var)), design = design)
  odds <- svyoddsratio(tbl_design, method = "logit")
  ci <- confint(odds)
  tbl_summary(odds, ci, label = var)
})

# Create summary table
tabl1 <- df_Rul2[, var.ql] %>%
  tbl_summary(by = GROUPE, missing_text = "(Missing)") %>%
  add_n() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  bold_p()

# Add odds ratio and confidence interval to the summary table
for (i in seq_along(odds_ratio_table)) {
  var <- var.ql[i]
  odds <- odds_ratio_table[[i]]
  tbl_add_columns(tabl1,
                  statistic = odds,
                  ci = confint(odds),
                  label = var,
                  include = TRUE)
}

# View the resulting table
View(tabl1)

###############################

library(gtsummary)
library(survey)

# Create survey design object
design <- svydesign(ids = ~1, data = df_Rul2, weights = ~YOUR_WEIGHT_VARIABLE_NAME)

# Calculate odds ratio and confidence interval
odds_ratio_table <- lapply(var.ql, function(var) {
  tbl_design <- svytable(as.formula(paste0("~ GROUPE + ", var)), design = design)
  odds <- svyoddsratio(tbl_design, method = "logit")
  ci <- confint(odds)
  tbl_summary(odds, ci, label = var)
})

# Create summary table
tabl1 <- df_Rul2[, var.ql] %>%
  tbl_summary(by = GROUPE, missing_text = "(Missing)") %>%
  add_n() %>%
  add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>%
  bold_p()

# Add odds ratio and confidence interval to the summary table
for (i in seq_along(odds_ratio_table)) {
  var <- var.ql[i]
  odds <- odds_ratio_table[[i]]
  tbl_add_columns(tabl1,
                  statistic = odds,
                  ci = confint(odds),
                  label = var,
                  include = TRUE)
}

# View the resulting table
View(tabl1)


Glm1 <- glm(data=df.imp_check2, GROUPE~ Profession+ Age_cat+ Religion+ Mod_trav, family="binomial")
summary(Glm1)
drop1(Glm1, .~., test="Chisq")
exp(coef(Glm1))

str(df.imp_check2)

#############################

# Fit the logistic regression model
model <- glm(data = df.imp_check2, GROUPE ~ Profession + Age_cat + Religion + Mod_trav, family = "binomial")

# Obtain the odds ratio and confidence interval
summary_table <- summary(model)$coefficients

# Extract odds ratios and their confidence intervals
odds_ratio <- exp(summary_table[, "Estimate"])
conf_interval <- exp(confint(model))

# Print the odds ratios and their confidence intervals
result <- data.frame(odds_ratio, conf_interval)
print(result)




# Charger la bibliothèque de régression logistique
library("glm")

# Diviser les variables en 9 parties
nb_partitions <- 9
parties <- lapply(df.imp_check2, function(x) cut(x, breaks = nb_partitions, labels = FALSE))

# Fonction pour calculer les odds ratios et les intervalles de confiance pour chaque partie
calculer_odds_ratios <- function(data) {
  modele_logit <- glm(GROUPE ~ ., data = data, family = binomial)
  
  # Calcul des odds ratios
  odds_ratios <- exp(coef(modele_logit))
  
  # Calcul des intervalles de confiance à 95%
  intervalles_confiance <- exp(confint.default(modele_logit))
  
  # Retourner les résultats
  return(list(odds_ratios = odds_ratios, intervalles_confiance = intervalles_confiance))
}

# Créer une liste pour stocker les résultats
resultats <- list()

# Boucle pour traiter chaque partie des variables
for (i in 1:nb_partitions) {
  # Sélectionner les données correspondantes à la partie i
  data_partie <- df.imp_check2[parties == i]
  
  # Exclure les colonnes où toutes les valeurs sont identiques dans la partie i
  colonnes_exclues <- sapply(data_partie, function(x) length(unique(x)) == 1)
  data_partie <- data_partie[, !colonne_exclues]
  
  # Appliquer le calcul des odds ratios et des intervalles de confiance
  resultats[[paste("Partie", i)]] <- calculer_odds_ratios(data_partie)
}

# Afficher les résultats
for (i in 1:length(resultats)) {
  partie <- names(resultats[i])
  odds_ratios <- resultats[[i]]$odds_ratios
  intervalles_confiance <- resultats[[i]]$intervalles_confiance
  
  cat("Partie:", partie, "\n")
  cat("Odds ratios:", odds_ratios, "\n")
  cat("Intervalle de confiance:", intervalles_confiance, "\n\n")
}

##########################################
Bon

# Install and load the "car" package
install.packages("car")
library(car)

# Fit the logistic regression model
model1 <- glm(data = df.imp_check2, GROUPE ~ Age_cat+ Stat_matri + Religion + Niv_scol+ Age_gest + Gravité+ Parité+ Nbre_cpn + Mod_trav, family = "binomial" )  

names(df.imp_check2)

# Obtain the odds ratio and confidence interval for binary variables
summary_table <- summary(model)$coefficients
odds_ratio <- exp(summary_table[, "Estimate"])
conf_interval <- exp(confint(model))

# Print the odds ratios and their confidence intervals for binary variables
result_binary <- data.frame(odds_ratio, conf_interval)

# Obtain the overall p-values for categorical variables with more than two modalities
anova_table <- Anova(model, type = "III")
p_values <- anova_table$"Pr(>Chi)"

# Print the overall p-values for categorical variables with more than two modalities
result_categorical <- data.frame(p_values)

# Print the results
print("Binary Variables:")
print(result_binary)

print("Categorical Variables:")
print(result_categorical)

###############################

# Define the number of groups
num_groups <- 5

# Create an empty list to store the grouped variables
grouped_variables <- list()

# Loop through the variables in df.imp_check2
for (variable in names(df.imp_check2)) {
  # Check if the variable is categorical or numeric
  if (is.factor(df.imp_check2[[variable]])) {
    # If the variable is categorical, add it to the grouped variables list as is
    grouped_variables[[variable]] <- df.imp_check2[[variable]]
  } else {
    # If the variable is numeric, divide it into equal-width groups
    grouped_variables[[variable]] <- cut(df.imp_check2[[variable]], breaks = num_groups)
  }
}

# Perform glm for each group of variables
results <- list()

for (group in grouped_variables) {
  # Combine the grouped variables with GROUPE
  data <- data.frame(GROUPE = df.imp_check2$GROUPE, group)
  
  # Fit the logistic regression model
  model <- glm(GROUPE ~ ., data = data, family = "binomial")
  
  # Calculate odds ratios and confidence intervals
  odds_ratios <- exp(coef(model))
  confidence_intervals <- confint(model)
  
  # Store the results
  results[[paste(names(group), collapse = "+")]] <- list(odds_ratios, confidence_intervals)
}

# Print the results
print(results)

############################

# Fit the logistic regression model
model1 <- glm(data = df.imp_check2, GROUPE ~ Age_cat ,  family = "binomial")

# Calculate odds ratios and confidence intervals
odds_ratios <- exp(coef(model1))
confidence_intervals <- confint(model1)

# Print the results
print(odds_ratios)
print(confidence_intervals)

########################################
bon
model1 <- glm(data = df.imp_check2, GROUPE ~  Age_cat, family = "binomial" )  

# Obtenir les coefficients et les erreurs standard
coefficients <- coef(model1)
std_errors <- sqrt(diag(vcov(model1)))

# Calculer les odds ratios
odds_ratios <- exp(coefficients)

# Calculer les intervalles de confiance à 95%
z_value <- qnorm(0.975)
lower_ci <- exp(coefficients - z_value * std_errors)
upper_ci <- exp(coefficients + z_value * std_errors)

# Créer un tableau avec les résultats
results <- data.frame(OddsRatio = odds_ratios, LowerCI = lower_ci, UpperCI = upper_ci)

# Afficher les résultats
print(results)

#####################################

model1 <- glm(data = df.imp_check2, GROUPE ~ Age_cat+ Stat_matri + Religion + Niv_scol+ Age_gest + Gravité+ Parité+ Nbre_cpn + Mod_trav, family = "binomial" )  

# Obtenir les coefficients et les erreurs standard
coefficients <- coef(model1)
std_errors <- sqrt(diag(vcov(model1)))

# Calculer les odds ratios
odds_ratios <- exp(coefficients)

# Calculer les intervalles de confiance à 95%
z_value <- qnorm(0.975)
lower_ci <- exp(coefficients - z_value * std_errors)
upper_ci <- exp(coefficients + z_value * std_errors)

# Créer un tableau avec les résultats
results <- data.frame(OddsRatio = odds_ratios, LowerCI = lower_ci, UpperCI = upper_ci)

# Afficher les résultats
print(results)


# Obtenir les coefficients et les erreurs standard
coefficients <- coef(model1)
std_errors <- sqrt(diag(vcov(model1)))

# Calculer les odds ratios
odds_ratios <- exp(coefficients)

# Calculer les intervalles de confiance à 95%
z_value <- qnorm(0.975)
lower_ci <- exp(coefficients - z_value * std_errors)
upper_ci <- exp(coefficients + z_value * std_errors)

# Créer un tableau avec les résultats
results <- data.frame(OddsRatio = odds_ratios, LowerCI = lower_ci, UpperCI = upper_ci)

# Afficher les résultats
print(results)

#######################################

# Référence : première modalité de Age_cat
reference_category <- levels(df.imp_check2$Age_cat)[1]

# Créer un vecteur de variables indicatrices pour Age_cat
indicators <- as.matrix(model.matrix(~ Age_cat - 1, data = df.imp_check2))

# Sélectionner uniquement les indicateurs correspondant aux modalités de Age_cat (à partir de la 2e colonne)
indicators <- indicators[, -1]

# Ajuster un modèle de régression logistique en utilisant les indicateurs
model_age <- glm(data = df.imp_check2, GROUPE ~ indicators, family = "binomial")

# Obtenir les coefficients et les erreurs standard
coefficients <- coef(model_age)
std_errors <- sqrt(diag(vcov(model_age)))

# Indice de la modalité de référence dans les résultats
reference_index <- which(levels(df.imp_check2$Age_cat) == reference_category)

# Retirer les coefficients et les erreurs standard de la modalité de référence
coefficients <- coefficients[-reference_index]
std_errors <- std_errors[-reference_index]

# Calculer les odds ratios
odds_ratios <- exp(coefficients)

# Calculer les intervalles de confiance à 95%
z_value <- qnorm(0.975)
lower_ci <- exp(coefficients - z_value * std_errors)
upper_ci <- exp(coefficients + z_value * std_errors)

# Créer un tableau avec les résultats
results <- data.frame(OddsRatio = odds_ratios, LowerCI = lower_ci, UpperCI = upper_ci)

# Afficher les résultats
print(results)

#########################

# Créer un vecteur de variables indicatrices pour Age_cat
indicators <- as.matrix(model.matrix(~ Age_cat, data = df.imp_check2))

# Ajuster un modèle de régression logistique en utilisant les indicateurs
model_age <- glm(data = df.imp_check2, GROUPE ~ indicators, family = "binomial")

# Obtenir les coefficients et les erreurs standard
coefficients <- coef(model_age)
std_errors <- sqrt(diag(vcov(model_age)))

# Calculer l'odd ratio global
odds_ratio_global <- exp(coefficients)

# Calculer les intervalles de confiance à 95%
z_value <- qnorm(0.975)
lower_ci <- exp(coefficients - z_value * std_errors)
upper_ci <- exp(coefficients + z_value * std_errors)

# Créer un tableau avec le résultat global
results <- data.frame(OddsRatio = odds_ratio_global, LowerCI = lower_ci, UpperCI = upper_ci)

# Afficher le résultat global
print(results)
###################################################




# Ajuster un modèle de régression logistique en utilisant les indicateurs
model_age <- glm(data = df.imp_check2, GROUPE ~ Age_cat, family = "binomial")
exp(coef(model_age))
drop1(model_age, .~. ,test= "Chisq")

####################################

# Ajuster le modèle de régression logistique avec la variable Age_cat
model_age <- glm(data = df.imp_check2, GROUPE ~ Age_cat, family = "binomial")

# Calculer l'odd ratio pour chaque modalité de Age_cat
odd_ratios <- exp(coef(model_age))

# Effectuer l'analyse de type "drop1" et obtenir le résultat global
drop1_result <- drop1(model_age, . ~ ., test = "Chisq")

# Extraire l'odd ratio global et son intervalle de confiance associé
global_odd_ratio <- exp(drop1_result$Deviance[2])
lower_ci <- exp(drop1_result$Deviance[2] - qnorm(0.975) * sqrt(drop1_result$Deviance[3]))
upper_ci <- exp(drop1_result$Deviance[2] + qnorm(0.975) * sqrt(drop1_result$Deviance[3]))

# Créer un tableau avec les résultats
results <- data.frame(GlobalOddsRatio = global_odd_ratio, LowerCI = lower_ci, UpperCI = upper_ci)

# Afficher les résultats
print(results)


###################################

# Liste des variables dans le dataframe
variables <- names(df.imp_check2)

# Créer un vecteur pour stocker les résultats
results <- data.frame(Variable = character(),
                      OddsRatio = numeric(),
                      LowerCI = numeric(),
                      UpperCI = numeric(),
                      stringsAsFactors = FALSE)

# Boucle sur chaque variable
for (var in variables) {
  # Ajuster un modèle de régression logistique simple
  model <- glm(formula = paste("GROUPE ~", var), data = df.imp_check2, family = "binomial")
  
  # Obtenir les coefficients et les erreurs standard
  coefficients <- coef(model)
  std_errors <- sqrt(diag(vcov(model)))
  
  # Calculer les odds ratios
  odds_ratios <- exp(coefficients)
  
  # Calculer les intervalles de confiance à 95%
  z_value <- qnorm(0.975)
  lower_ci <- exp(coefficients - z_value * std_errors)
  upper_ci <- exp(coefficients + z_value * std_errors)
  
  # Ajouter les résultats à la dataframe results
  result <- data.frame(Variable = var,
                       OddsRatio = odds_ratios,
                       LowerCI = lower_ci,
                       UpperCI = upper_ci,
                       stringsAsFactors = FALSE)
  results <- rbind(results, result)
}

# Afficher les résultats
print(results)

###############################################
Bon

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, OddsRatio = odds_ratios, LowerCI = lower_ci, UpperCI = upper_ci))
}

# Afficher les résultats
print(results)

############################################

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, OddsRatio = odds_ratios, LowerCI = lower_ci, UpperCI = upper_ci))
}

# Afficher les résultats
print(results)

###############################################

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, OddsRatio = odds_ratios, LowerCI = lower_ci, UpperCI = upper_ci))
}

# Afficher les résultats
print(results)
results


##################################################

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
  
  # Ajouter les résultats au dataframe en formatant avec deux chiffres après la virgule
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = round(odds_ratios, 2), 
                                       LowerCI = round(lower_ci, 2), 
                                       UpperCI = round(upper_ci, 2), 
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)
results


##########################################

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Ajouter les résultats au dataframe en formatant sans la forme avec exposant et avec 2 chiffres après la virgule
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = format(odds_ratios, scientific = FALSE, digits = 2), 
                                       LowerCI = format(lower_ci, scientific = FALSE, digits = 2), 
                                       UpperCI = format(upper_ci, scientific = FALSE, digits = 2), 
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)
results


# Installer le package knitr s'il n'est pas déjà installé
install.packages("knitr")

# Charger le package knitr
library(knitr)

# ...

# Afficher les résultats
kable(results, format = "html")


library(knitr)
kable(results, format = "html")


##################################

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

# Boucle sur chaque variable
for (variable in variables) {
  # Ajuster le modèle de régression logistique en utilisant la variable comme prédicteur
  model <- glm(formula = paste("GROUPE ~", variable), data = df.imp_check2, family = "binomial")
  
  # Extraire le coefficient, l'erreur standard et le p-value
  coefficient <- coef(model)[2]  # Coefficient de la variable explicative
  std_error <- sqrt(vcov(model)[2, 2])  # Erreur standard du coefficient
  z_value <- qnorm(0.975)  # Valeur critique pour le calcul de l'intervalle de confiance
  
  # Calculer l'odds ratio et les limites de l'intervalle de confiance à 95%
  odds_ratio <- exp(coefficient)
  lower_ci <- exp(coefficient - z_value * std_error)
  upper_ci <- exp(coefficient + z_value * std_error)
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = odds_ratio, 
                                       LowerCI = lower_ci, 
                                       UpperCI = upper_ci,
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)


###########################################

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = coefficient, 
                                       LowerCI = lower_ci, 
                                       UpperCI = upper_ci,
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)

##############################################
# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Arrondir les valeurs à deux chiffres après la virgule
  coefficient <- round(coefficient, digits = 2)
  lower_ci <- round(lower_ci, digits = 2)
  upper_ci <- round(upper_ci, digits = 2)
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = coefficient, 
                                       LowerCI = lower_ci, 
                                       UpperCI = upper_ci,
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)

#############################

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = coefficient, 
                                       LowerCI = lower_ci, 
                                       UpperCI = upper_ci,
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)
#######################################

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Arrondir les valeurs à deux chiffres après la virgule
  coefficient <- round(coefficient, digits = 2)
  lower_ci <- round(lower_ci, digits = 2)
  upper_ci <- round(upper_ci, digits = 2)
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = coefficient, 
                                       LowerCI = lower_ci, 
                                       UpperCI = upper_ci,
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)
##########################################

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = coefficient, 
                                       LowerCI = lower_ci, 
                                       UpperCI = upper_ci,
                                       stringsAsFactors = FALSE))
}

# Formater les résultats sans exposant
results$OddsRatio <- formatC(results$OddsRatio, format = "f", digits = 2)
results$LowerCI <- formatC(results$LowerCI, format = "f", digits = 2)
results$UpperCI <- formatC(results$UpperCI, format = "f", digits = 2)

# Afficher les résultats
print(results)



str(df_Rul)

install.packages("reshape")
library(reshape)

install.packages("reshape2")
library(reshape2)

df_Rul_long <- melt(df_Rul, id.vars = c("UniqueKey", "ANNEE", "ETABLISSEMENT", "Nometprnom", "GROUPE"))


library(tidyverse)

df_Rul_long <- df_Rul %>%
  pivot_longer(cols = -c(UniqueKey, ANNEE, ETABLISSEMENT, Nometprnom, GROUPE),
               names_to = "Variable",
               values_to = "Valeur")

df_Rul_wide <- df_Rul_long %>%
  pivot_wider(names_from = Variable, values_from = Valeur)

str(df_Rul_long)



library(reshape2)

# Utiliser la fonction `dcast()` pour transformer le dataframe en format large
df_Rul_wide2 <- dcast(df_Rul_long, UniqueKey + ANNEE + ETABLISSEMENT + Nometprnom + GROUPE ~ Variable, value.var = "Valeur")

# Afficher le dataframe résultant
print(df_Rul_wide)

str(df_Rul2)


library(ggplot2)

# Parcourir toutes les variables du dataframe
for (col in names(df_Rul2)) {
  # Vérifier le type de variable
  if (is.factor(df_Rul2[[col]])) {
    # Créer un diagramme à barres pour les variables catégorielles
    ggplot(df_Rul2, aes(x = as.factor(df_Rul2[[col]]))) +
      geom_bar() +
      labs(title = col, x = col, y = "Count")
  } else if (is.numeric(df_Rul2[[col]])) {
    # Créer un histogramme pour les variables numériques
    ggplot(df_Rul2, aes(x = df_Rul2[[col]])) +
      geom_histogram() +
      labs(title = col, x = col, y = "Count")
  }
  
  # Afficher le graphique
  print(plot)
}


#############################

library(ggplot2)

# Parcourir toutes les variables du dataframe
for (col in names(df_Rul2)) {
  # Vérifier le type de variable
  if (is.factor(df_Rul2[[col]])) {
    # Créer un diagramme à barres pour les variables catégorielles
    plot <- ggplot(df_Rul2, aes(x = as.factor(df_Rul2[[col]]))) +
      geom_bar() +
      labs(title = col, x = col, y = "Count")
  } else if (is.numeric(df_Rul2[[col]])) {
    # Créer un histogramme pour les variables numériques
    plot <- ggplot(df_Rul2, aes(x = df_Rul2[[col]])) +
      geom_histogram() +
      labs(title = col, x = col, y = "Count")
  }
  
  # Afficher le graphique
  print(plot)
}

#########################################

library(ggplot2)

# Parcourir toutes les variables du dataframe
for (col in names(df_Rul2)) {
  # Vérifier le type de variable
  if (is.factor(df_Rul2[[col]])) {
    # Créer un diagramme à barres pour les variables catégorielles
    plot <- ggplot(df_Rul2, aes(x = as.factor(df_Rul2[[col]]), fill = as.factor(df_Rul2[[col]]))) +
      geom_bar() +
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  } else if (is.numeric(df_Rul2[[col]])) {
    # Créer un histogramme pour les variables numériques
    plot <- ggplot(df_Rul2, aes(x = df_Rul2[[col]], fill = as.factor(df_Rul2[[col]]))) +
      geom_histogram(binwidth = 1) +
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  }
}
#################################

library(ggplot2)

# Parcourir toutes les variables du dataframe
for (col in names(df_Rul2)) {
  # Vérifier le type de variable
  if (is.factor(df_Rul2[[col]])) {
    # Créer un diagramme à barres pour les variables catégorielles
    plot <- ggplot(na.omit(df_Rul2), aes(x = as.factor(df_Rul2[[col]]), fill = as.factor(df_Rul2[[col]]))) +
      geom_bar() +
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  } else if (is.numeric(df_Rul2[[col]])) {
    # Créer un histogramme pour les variables numériques
    plot <- ggplot(na.omit(df_Rul2), aes(x = df_Rul2[[col]], fill = as.factor(df_Rul2[[col]]))) +
      geom_histogram(binwidth = 1) +
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  }
}


###########################################

library(ggplot2)

# Parcourir toutes les variables du dataframe
for (col in names(df.imp_check2)) {
  # Vérifier le type de variable
  if (is.factor(df.imp_check2[[col]])) {
    # Créer un diagramme à barres pour les variables catégorielles
    plot <- ggplot(na.omit(df.imp_check2), aes(x = as.factor(df.imp_check2[[col]]), fill = as.factor(df.imp_check2[[col]]))) +
      geom_bar() +
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  } else if (is.numeric(df.imp_check2[[col]])) {
    # Créer un histogramme pour les variables numériques
    plot <- ggplot(na.omit(df.imp_check2), aes(x = df.imp_check2[[col]], fill = as.factor(df.imp_check2[[col]]))) +
      geom_histogram(binwidth = 1) +
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  }
}

#################################

library(ggplot2)

# Créer un nouveau dataframe en excluant les valeurs manquantes
df_clean <- na.omit(df.imp_check2)

# Parcourir toutes les variables du dataframe
for (col in names(df_clean)) {
  # Vérifier le type de variable
  if (is.factor(df_clean[[col]])) {
    # Créer un diagramme à barres pour les variables catégorielles
    plot <- ggplot(df_clean, aes(x = as.factor(df_clean[[col]]), fill = as.factor(df_clean[[col]]))) +
      geom_bar() +
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  } else if (is.numeric(df_clean[[col]])) {
    # Créer un histogramme pour les variables numériques
    plot <- ggplot(df_clean, aes(x = df_clean[[col]], fill = as.factor(df_clean[[col]]))) +
      geom_histogram(binwidth = 1) +
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  }
}


##############################
 très bien
library(ggplot2)

# Créer un nouveau dataframe en excluant les valeurs manquantes
df_clean <- na.omit(df.imp_check2)

# Parcourir toutes les variables du dataframe
for (col in names(df_clean)) {
  # Vérifier le type de variable
  if (is.factor(df_clean[[col]])) {
    # Créer un diagramme à barres pour les variables catégorielles
    plot <- ggplot(df_clean, aes(x = as.factor(df_clean[[col]]), fill = as.factor(df_clean[[col]]))) +
      geom_bar(width = 0.5) +  # Ajuster la largeur des barres
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  } else if (is.numeric(df_clean[[col]])) {
    # Créer un histogramme pour les variables numériques
    plot <- ggplot(df_clean, aes(x = df_clean[[col]], fill = as.factor(df_clean[[col]]))) +
      geom_histogram(binwidth = 1, position = "identity", width = 0.5) +  # Ajuster la largeur des barres
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  }
}
######################################
#bon
library(ggplot2)

str(df_Rul2)

# Créer un nouveau dataframe en excluant les valeurs manquantes
df_clean <- na.omit(df.imp_check2)

# Parcourir toutes les variables du dataframe
for (col in names(df_clean)) {
  # Vérifier le type de variable
  if (is.factor(df_clean[[col]])) {
    # Créer un diagramme à barres pour les variables catégorielles
    plot <- ggplot(df_clean, aes(x = as.factor(df_clean[[col]]), fill = as.factor(df_clean[[col]]))) +
      geom_bar(position = "fill", width = 0.8) +  # Ajuster la largeur des barres et l'espacement entre les modalités
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "blue"))  # Modifier la couleur de fond en bleu
    
    print(plot)
  } else if (is.numeric(df_clean[[col]])) {
    # Créer un histogramme pour les variables numériques
    plot <- ggplot(df_clean, aes(x = df_clean[[col]], fill = as.factor(df_clean[[col]]))) +
      geom_histogram(binwidth = 1, position = "identity", width = 0.8) +  # Ajuster la largeur des barres et l'espacement entre les modalités
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "blue"))  # Modifier la couleur de fond en bleu
    
    print(plot)
  }
}

##################################

library(ggplot2)

# Créer un nouveau dataframe en excluant les valeurs manquantes
df_clean <- na.omit(df.imp_check2)

# Parcourir toutes les variables du dataframe
for (col in names(df_clean)) {
  # Vérifier le type de variable
  if (is.factor(df_clean[[col]])) {
    # Créer un diagramme à barres pour les variables catégorielles
    plot <- ggplot(df_clean, aes(x = as.factor(df_clean[[col]]), fill = as.factor(df_clean[[col]]))) +
      geom_bar(width = 0.8, position = "dodge") +  # Ajuster la largeur des barres et réduire l'espacement entre les modalités
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none", 
            panel.background = element_rect(fill = "blue"))  # Modifier la couleur de fond en bleu
    
    print(plot)
  } else if (is.numeric(df_clean[[col]])) {
    # Créer un histogramme pour les variables numériques
    plot <- ggplot(df_clean, aes(x = df_clean[[col]], fill = as.factor(df_clean[[col]]))) +
      geom_histogram(binwidth = 1, position = "identity", width = 0.8) +  # Ajuster la largeur des barres
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none", 
            panel.background = element_rect(fill = "blue"))  # Modifier la couleur de fond en bleu
    
    print(plot)
  }
}

###################################

# Charger le package ggplot2 s'il n'est pas déjà installé
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Créer un nouveau dataframe en excluant les valeurs manquantes
df_clean <- na.omit(df.imp_check2)

# Parcourir toutes les variables du dataframe
for (col in names(df_clean)) {
  # Vérifier le type de variable
  if (is.factor(df_clean[[col]])) {
    # Créer un diagramme à barres pour les variables catégorielles
    plot <- ggplot(df_clean, aes(x = as.factor(.data[[col]]), fill = as.factor(.data[[col]]))) +
      geom_bar(width = 0.25) +  # Ajuster la largeur des barres
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  } else if (is.numeric(df_clean[[col]])) {
    # Créer un histogramme pour les variables numériques
    plot <- ggplot(df_clean, aes(x = .data[[col]], fill = as.factor(.data[[col]]))) +
      geom_histogram(binwidth = 1, position = "identity", width = 0.5) +  # Ajuster la largeur des barres
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  }
}

################################"
# Charger le package ggplot2 s'il n'est pas déjà installé
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Créer un nouveau dataframe en excluant les valeurs manquantes
df_clean <- na.omit(df.imp_check2)

# Parcourir toutes les variables du dataframe
for (col in names(df_clean)) {
  # Vérifier le type de variable
  if (is.factor(df_clean[[col]])) {
    # Créer un diagramme à barres pour les variables catégorielles
    plot <- ggplot(df_clean, aes(x = as.factor(.data[[col]]), fill = as.factor(.data[[col]]))) +
      geom_bar(width = 0.5/2) +  # Ajuster la largeur des barres
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  } else if (is.numeric(df_clean[[col]])) {
    # Créer un histogramme pour les variables numériques
    plot <- ggplot(df_clean, aes(x = .data[[col]], fill = as.factor(.data[[col]]))) +
      geom_histogram(binwidth = 1, position = "identity", width = 0.5) +  # Ajuster la largeur des barres
      labs(title = col, x = col, y = "Count") +
      theme(legend.position = "none")  # Supprimer la légende
    
    print(plot)
  }
}

plot(df_Rul2$GROUPE,df_Rul2$ETABLISSEMENT)



##############################

# Tracé du graphique
ggplot(df_Rul2, aes(x = ETABLISSEMENT, fill = GROUPE)) +
  geom_bar(position = "dodge") +
  labs(x = "Etablissement", y = "Nombre d'observations", fill = "Groupe") +
  ggtitle(" CAS et TEMOINS par hospital ")


###############

# Filtrage des lignes où ETABLISSEMENT n'est pas NA
df_filtered <- subset(df_Rul2, !is.na(ETABLISSEMENT))

# Tracé du graphique
ggplot(df_filtered, aes(x = ETABLISSEMENT, fill = GROUPE)) +
  geom_bar(position = "dodge") +
  labs(x = "Etablissement", y = "Nombre d'observations", fill = "Groupe") +
  ggtitle("Répartition des CAS et TEMOINS par établissement (en excluant les valeurs manquantes)")

# Filtrage des valeurs manquantes
df_filtered <- na.omit(df_Rul2)

# Tracé du graphique
ggplot(df_filtered, aes(x = ETABLISSEMENT, fill = GROUPE)) +
  geom_bar(position = "dodge") +
  labs(x = "Etablissement", y = "Nombre d'observations", fill = "Groupe") +
  ggtitle("Répartition des CAS et TEMOINS par établissement (sans les valeurs manquantes)")


table(df_Rul$ETABLISSEMENT, useNA = "always")
table(df_Rul$GROUPE, useNA = "always")

str(df_Rul2)


##################

# Filtrage des lignes où ETABLISSEMENT n'est pas NA
df_filtered <- subset(df_Rul2, !is.na(ETABLISSEMENT))

# Tracé du graphique
png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/Cas_Temoin_par_etabli.png')
ggplot(df_filtered, aes(x = ETABLISSEMENT, fill = GROUPE)) +
  geom_bar(position = "dodge") +
  labs(x = "Etablissement", y = "Nombre d'observations", fill = "Groupe") +
  ggtitle("Répartition des CAS et TEMOINS par établissement")
dev.off()


table(df_Rul$GROUPE, df_Rul$ETABLISSEMENT, deparse.level = 2)


plot(table(df.imp_check2$Age_cat), col= "pink" )

plot(df.imp_check2$Age_cat, col= "pink" )
lines(density(df.imp_check2$Age_cat))
plot(df.imp_check2$Age_cat, col = "pink", pch = 50)
############################################################

# Tracé du diagramme en points
plot(df.imp_check2$Age_cat, col = "pink", pch = 16)

# Ajout d'une ligne continue sur l'axe des X
abline(h = 0, lwd = 1)

# Calcul de la densité des valeurs de la variable Age_cat
density_values <- density(df.imp_check2$Age_cat)

# Tracé de la courbe de densité
lines(density_values, col = "blue", lwd = 2)

#########################################################


png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/Age_distribution.png')
barplot(table(df.imp_check2$Age_cat), col = c("pink", "blue", "green", "yellow", "purple"), 
        main = "Distribution des âges", xlab = "Age_cat", ylab = "Fréquence")
abline(h = 0, lwd = 1)
dev.off()

boxplot(df.imp_check2$Pres_sal_accou~df.imp_check2$GROUPE, col= "pink")
str(df_Rul)

table(df_Rul$NNEVIVANT, useNA = "always")
table(df_Rul$MEREVIVANTE, useNA = "always")

df_Rul3 <- subset(df_Rul2, select = -(Deces))
df_Rul3$NNEVIVANT <- df_Rul$NNEVIVANT

df_Rul3$MEREVIVANTE <- df_Rul$MEREVIVANTE

plot(df_Rul3$ANNEE, df_Rul3[df_Rul$MEREVIVANTE=="", ] )
str(df_Rul3 )

# Charger la bibliothèque ggplot2
library(ggplot2)

# Filtrer les lignes avec la valeur "Non" pour la variable "MEREVIVANTE"
df_filtered <- df_Rul3[df_Rul3$MEREVIVANTE == "NON", ]

# Tracer le graphe en utilisant ggplot2
ggplot(df_filtered, aes(x = ANNEE)) +
  geom_bar() +
  labs(x = "Année", y = "Nombre de mères vivantes (Non)", fill = "Mères vivantes") +
  theme_minimal()

###############################

# Charger la bibliothèque ggplot2
library(ggplot2)

# Filtrer les lignes avec la valeur "Non" pour la variable "MEREVIVANTE"
df_filtered <- df_Rul3[df_Rul3$MEREVIVANTE == "NON", ]

# Calculer le nombre de mères vivantes (Non) par année
df_summary <- aggregate(MEREVIVANTE ~ ANNEE, data = df_filtered, FUN = length)

# Tracer le graphe en utilisant ggplot2
ggplot(df_summary, aes(x = ANNEE, y = MEREVIVANTE)) +
  geom_bar(stat = "identity") +
  labs(x = "Année", y = "Nombre de mères vivantes (Non)") +
  theme_minimal()

boxplot(df_Rul2$GROUPE~df_Rul2$Religion)

boxplot(df_Rul2$Religion~df_Rul2$GROUPE)

boxplot(df_Rul2$Stat_matri~df_Rul2$GROUPE)
boxplot(df_Rul2$GROUPE~df_Rul2$Stat_matri)

pie(table(df_Rul2$Profession), main = "Profession des patientes")

boxplot(df_Rul2$Siegedelarupture~df_Rul2$ANNEE)

boxplot(df_Rul2$GROUPE~df_Rul2$Stat_matri)
barplot(table(df_Rul2$Niv_scol))

png('C:/Users/TCHEUMENI/Desktop/LOGICIEL R/Epidémio/code stage de master_ rupture ulterine/poids~groupe.png')
boxplot(df_Rul2$Poids_foet~df_Rul2$GROUPE, col="yellow", main="Poids Foetal en fonction des Groupes", xlab = "GROUPE", ylab = "Densité")
dev.off()


# Création du graphe boxplot avec la variable Poids_foet
boxplot(df_Rul2$Poids_foe_num ~ df_Rul2$GROUPE, main = "Boxplot du Poids_foet par Groupe", xlab = "Groupe", ylab = "Poids_foet")


plot(df_Rul2$Siegedelarupture, df_Rul2$ANNEE)

plot(df_Rul2$Siegedelarupture~ df_Rul2$ANNEE)


table(df_Rul2$Siegedelarupture, df_Rul2$ANNEE)

hist(table(df_Rul2$Siegedelarupture))

barplot(table(df_Rul2$Siegedelarupture), col = "pink")
abline(lw=1)







#####################################################################################

#bon 

####### calcul du odd ratio et son interval de confiance########

#### fournir le resultats des odd ratio par modalités##


# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Ajouter les résultats au dataframe en formatant sans la forme avec exposant et avec 2 chiffres après la virgule
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = format(odds_ratios, scientific = FALSE, digits = 2), 
                                       LowerCI = format(lower_ci, scientific = FALSE, digits = 2), 
                                       UpperCI = format(upper_ci, scientific = FALSE, digits = 2), 
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)
results

#############################################################################################
## donne la valeurs du odd ratio pour chaque variables#####

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = coefficient, 
                                       LowerCI = lower_ci, 
                                       UpperCI = upper_ci,
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)

str(df.imp_check2)


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

################################################################
#bon mais sans p value


##########calcul du odd ration et de son interval de confiance###########

#### fournir le resultats des odd ratio par modalités##


# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Ajouter les résultats au dataframe en formatant sans la forme avec exposant et avec 2 chiffres après la virgule
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = format(odds_ratios, scientific = FALSE, digits = 2), 
                                       LowerCI = format(lower_ci, scientific = FALSE, digits = 2), 
                                       UpperCI = format(upper_ci, scientific = FALSE, digits = 2), 
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)
results

#######################################################
## donne la valeurs du odd ratio pour chaque variables#####

# Liste des variables du dataframe (à l'exception de GROUPE)
variables <- names(df.imp_check2)[-which(names(df.imp_check2) == "GROUPE")]

# Créer un dataframe pour stocker les résultats
results <- data.frame(Variable = character(), OddsRatio = numeric(), LowerCI = numeric(), UpperCI = numeric(), stringsAsFactors = FALSE)

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
  
  # Ajouter les résultats au dataframe
  results <- rbind(results, data.frame(Variable = variable, 
                                       OddsRatio = coefficient, 
                                       LowerCI = lower_ci, 
                                       UpperCI = upper_ci,
                                       stringsAsFactors = FALSE))
}

# Afficher les résultats
print(results)

##################################################################


mod1 <- glm(df_Rul$GROUPE~df_Rul$Protestante, family= "binomial")				
summary(mod1)              
exp(coef(mod1))

hist(resid(mod1))
qqnorm(df_toimput$NNEVIVANT)




df_Rul3 <- read.csv2("/Users/TCHEUMENI/Documents/STAGE DE MASTER/df_RupUl.csv", stringsAsFactors = T, na.strings = "")


df_toimput3<-subset(df_Rul3, select=-c(UniqueKey, ANNEE,  ETABLISSEMENT, Nometprnom, Hydramnios))

df_imp3<-mice(df_toimput3, seed=29,method='pmm',m=10,maxit=15)#,print=F)
df.imp_check3<-complete(df_imp3)



######################################################################
# Spécifier les variables catégorielles à extraire les modalités
variables <- c("Profession", "Age_gest", "Parité",  "Nbre_cpn", "Mod_travTravail",
               "Lieu_Ccpn", "Lieu_tra", "Lieu_pro", "Mod_adm", "Pres_sal_accou", "Espa_intge",
               "Siegedelarupture", "Typederupture")

#######################################################################

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

# variables significatives
signvar <- c("Elvetudiante", "Primipare", "N42", "Travailspontane", "hpital",
    "gyncologueobsttricien", "N2ans1", "N12h", "hpital1", "Domicile1",
   "Venuedellemme", "sagefemme1", "gyncologueobsttricien1", "N3500g", "GROUPE")
new_df <- df.imp_check3[, signvar]


##########################################################

# Ajuster le modèle de régression logistique
model <- glm(formula = GROUPE ~ ., data = new_df, family = "binomial")
hist(resid(model), col= "green")

lines(density(resid(model)))
qqnorm(resid(model))
qqline(resid(model))

# Obtenir le résumé du modèle
summary_model <- summary(model)

# Obtenir les coefficients, les intervalles de confiance et les p-values pour chaque variable
coefficients <- summary_model$coefficients
conf_intervals <- confint(model)
p_values <- summary_model$coefficients[, "Pr(>|z|)"]

# Calculer l'odds ratio et ses intervalles de confiance
odds_ratio <- exp(coefficients[, "Estimate"])
odds_ci <- exp(conf_intervals)

# Afficher les résultats
results <- data.frame(
  Variable = rownames(coefficients),
  Coefficient = coefficients[, "Estimate"],
  Odds_Ratio = odds_ratio,
  CI_Lower = odds_ci[, 1],
  CI_Upper = odds_ci[, 2],
  p_value = p_values
)
print(results)


str(df.imp_check3)


for(i in variables ) {
  print(i)
  tab3.facvar <- table(df.imp_check3[ ,print(i)], useNA = "always", deparse.level = 1 ) 
  print(tab3.facvar) 
}


str(df.imp_check2)



##############################
 
App2 <- df.imp_check2

# Installer et charger le package MatchIt
install.packages("MatchIt")
library(MatchIt)

# Étape 1 : Calculer le score de propension
model <- glm(GROUPE ~ ., data = App2, family = binomial, maxit = 100)
scores <- predict(model, type = "response")

# Étape 2 : Trier les observations en fonction du score de propension
App2$ScorePropension <- scores
App2 <- App2[order(scores), ]

# Étape 3 : Appariement 1:4 en utilisant MatchIt
match_data1 <- matchit(GROUPE ~ ScorePropension, data = App2, method = "nearest", ratio = 1, replace = FALSE, distance = "logit")



# Obtenir les données appariées
matched_data11 <- match.data(match_data1)

summary(match_data1)

# Vérifier les résultats
table(matched_data11$GROUPE)



str(df_Rul)

#########################################################
# verification de l'appariement


# Distribution des scores de propension avant et après appariement
plot(density(scores), main = "Distribution des scores de propension avant et après appariement")
lines(density(matched_data11$ScorePropension), col = "red")
legend("topright", c("Avant appariement", "Après appariement"), col = c("black", "red"), lty = 1)

#########################################

# Histogramme des scores de propension avant l'appariement
hist(scores, main = "Histogramme des scores de propension avant appariement", xlab = "Score de propension")

# Histogramme des scores de propension après l'appariement
hist(matched_data$ScorePropension, main = "Histogramme des scores de propension après appariement", xlab = "Score de propension")

#################################################

# Vérifier les fréquences des groupes traités et de contrôle
table(matched_data11$GROUPE)

##################################################################

# Graphique de la distribution des variables de contrôle avant et après appariement
par(mfrow = c(2, 2))
for (var in names(df.imp_check2)) {
  if (var != "GROUPE" & var != "ScorePropension") {
    hist(df.imp_check2[, var], main = paste("Distribution de", var, "avant appariement"))
    hist(matched_data[, var], main = paste("Distribution de", var, "après appariement"))
  }
}
#######################################################

# Graphique de la distribution des variables de contrôle avant et après appariement
par(mfrow = c(2, 2))
for (var in names(df.imp_check2)) {
  if (var != "GROUPE" & var != "ScorePropension") {
    if (is.factor(df.imp_check2[[var]]) & is.factor(matched_data[[var]])) {
      barplot(table(df.imp_check2[[var]]), main = paste("Distribution de", var, "avant appariement"))
      barplot(table(matched_data[[var]]), main = paste("Distribution de", var, "après appariement"))
    } else {
      cat("Variable", var, "is not a factor.\n")
    }
  }
}

#################################
#avec check 3

App3 <- df.imp_check3

# Installer et charger le package MatchIt
install.packages("MatchIt")
library(MatchIt)

# Étape 1 : Calculer le score de propension
model <- matchit(glm(GROUPE ~ ., data = App3, family = binomial ))
scores <- predict(model, type = "response")

# Étape 2 : Trier les observations en fonction du score de propension
App3$ScorePropension <- scores
App3 <- App3[order(scores), ]

# Étape 3 : Appariement 1:4 en utilisant MatchIt
match_data <- matchit(GROUPE ~ ScorePropension, data = App3, method = "nearest", ratio = 1, replace = FALSE, distance = "logit")

match_data <- matchit(GROUPE ~ ScorePropension, data = App3, method = "nearest", ratio = 4, distance = "logit", weight.matrix = c(0, 4))


# Obtenir les données appariées
matched_data <- match.data(match_data)

# Vérifier les résultats
table(matched_data$GROUPE)

summary(match_data)
###################################################

# verification de l'appariement


# Distribution des scores de propension avant et après appariement
plot(density(scores), main = "Distribution des scores de propension avant et après appariement")
lines(density(matched_data$ScorePropension), col = "red")
legend("topright", c("Avant appariement", "Après appariement"), col = c("black", "red"), lty = 1)





str(App3)


# Charger les packages nécessaires
library(MatchIt)

# Créer un nouveau data frame avec les variables pertinentes pour l'appariement
df <- App3[, c("GROUPE", "N25", "N2530", "N3035", ...)]  # Remplacez "N25", "N2530", "N3035", ... par les noms des variables pertinentes

# Effectuer une analyse de régression logistique pour estimer les scores de propension
model <- glm(GROUPE ~ N25 + N2530 + N3035 + ..., data = df, family = "binomial")
propensity_scores <- predict(model, type = "response")

# Ajouter les scores de propension au data frame
df$PropensityScore <- propensity_scores

# Effectuer l'appariement en utilisant la méthode du score de propension
matched_data <- matchit(GROUPE ~ PropensityScore, data = df, method = "nearest", ratio = 4)

# Obtenir le data frame apparié
matched_df <- match.data(matched_data)

# Afficher le résultat de l'appariement
print(matched_df)


############################

App3 <- df_imp2

# Charger les packages nécessaires
library(MatchThem)

# Variables à exclure du score de propension
var_notps <- c("GROUPE", "ROWNAMES", "imp", "m")  # Ajoutez d'autres variables que vous souhaitez exclure du score de propension

# Variables pour le score de propension
var_ps <- names(App3)[!(names(App3) %in% var_notps)]

# Formule pour le modèle de régression logistique
mod_formul <- paste("GROUPE ~", paste(var_ps, collapse = "+"), sep = "")
mod_formul
# Caliper
cal.val <- 0.10

# Appariement par score de propension avec méthode "nearest neighbor"
matched_df <- matchthem(as.formula(mod_formul), data = App3, approach = "within",
                        method = "nearest", caliper = cal.val, ratio = 4)

App2 <- df.imp_check2
sp <- glm(GROUPE ~ ., data = App2, family = binomial )
boxplot(sp)
