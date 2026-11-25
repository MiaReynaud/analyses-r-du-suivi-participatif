############################################################
# ANALYSE DES COMPORTEMENTS DES PECHEUSES MBISSA
# Application : Paramétrisation d'un système multi-agents

# Autrice : Mia Reynaud
# Date : 09/11/2026
############################################################

# ----------------------------------------------------------
# 1.1. NETTOYAGE DE L'ENVIRONNEMENT
# ----------------------------------------------------------

# Supprimer tous les objets présents dans l'environnement R
rm(list = ls())

# ----------------------------------------------------------
# 2.1. INSTALLATION DES PACKAGES NECESSAIRES
# ----------------------------------------------------------

packages <- c(
  "readxl",       # Importation des fichiers Excel
  "writexl",      # Exportation des résultats Excel
  "tidyverse",    # Manipulation et visualisation des données
  "janitor",      # Nettoyage des noms de variables
  "lubridate",    # Gestion des dates
  "stringi",      # Nettoyage des chaînes de caractères
  "cluster",      # Classification des individus
  "factoextra",   # Visualisation des classifications
  "vegan",        # Distances écologiques (Jaccard)
  "lme4",         # Modèles statistiques à effets mixtes
  "broom.mixed",  # Extraction des résultats des modèles
  "ggeffects",    # Prédictions statistiques
  "performance"   # Diagnostic des modèles
)

# Installer uniquement les packages absents
packages_manquants <- packages[
  !packages %in% rownames(installed.packages())
]

if(length(packages_manquants) > 0) {
  install.packages(packages_manquants)
}

# ----------------------------------------------------------
# 2.2. CHARGEMENT DES PACKAGES
# ----------------------------------------------------------

invisible(lapply(packages, library, character.only = TRUE))

# Vérification
sessionInfo()

# ----------------------------------------------------------
# 3.1. DEFINITION DU REPERTOIRE
# ----------------------------------------------------------

repertoire <- "D:/Mes Donnees/Analyses/R/Codes/Pann analyses suivi participatif"

# Fichier source
fichier_source <- file.path(
  repertoire,
  "bdd_fiches_mbissa_20251120_MR_minuscule.xlsx"
)

# Dossier de résultats
dossier_resultats <- file.path(
  repertoire,
  "Resultats_analyse_pecheuses"
)

# Créer le dossier s'il n'existe pas
if(!dir.exists(dossier_resultats)) {
  dir.create(dossier_resultats)
}

# Vérifier que le fichier existe
if(!file.exists(fichier_source)) {
  stop("ERREUR : le fichier Excel est introuvable.")
}

# Définir le répertoire de travail
setwd(repertoire)

# Vérification
print(fichier_source)
print(dossier_resultats)


# ----------------------------------------------------------
# 4.1. IMPORTATION DU FICHIER EXCEL
# ----------------------------------------------------------

# Lire la feuille "review_mia" du fichier Excel
df_brut <- read_excel(
  fichier_source,
  sheet = "Sheet1"
)

# Conserver une copie des données originales
df <- df_brut










































































# ----------------------------------------------------------
# 4.2. PREMIERE EXPLORATION
# ----------------------------------------------------------

# Nombre de lignes et de colonnes
dim(df)

# Noms des variables
names(df)

# Structure de la base
glimpse(df)

# Résumé statistique
summary(df)

# Nombre de valeurs manquantes par variable
colSums(is.na(df))

# ----------------------------------------------------------
# 5.1. NETTOYAGE DES NOMS DE VARIABLES
# ----------------------------------------------------------

df <- df %>%
  clean_names()

# ----------------------------------------------------------
# 5.2. CONVERSION DES VARIABLES
# ----------------------------------------------------------

# Fonction pour convertir les dates
convertir_date <- function(x) {
  
  if(inherits(x, "Date")) {
    return(as.Date(x))
  }
  
  if(inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  if(is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  
  suppressWarnings(
    as.Date(parse_date_time(
      as.character(x),
      orders = c("dmy", "ymd", "dmy HMS", "ymd HMS")
    ))
  )
}

# Conversion des variables
df <- df %>%
  mutate(
    date_peche = convertir_date(date_peche),
    date_debut_mbissa = convertir_date(date_debut_mbissa),
    
    rendement_pecheuse = as.numeric(rendement_pecheuse),
    
    zone = as.factor(zone),
    
    nom_pecheuse = as.character(nom_pecheuse),
    surnom_pecheuse = as.character(surnom_pecheuse)
  )









































# ----------------------------------------------------------
### nombre total de pêcheuses et répartition par village
# ----------------------------------------------------------

# Préparer une base avec une seule ligne par pêcheuse
pecheuses_uniques <- df %>%
  filter(!is.na(id_pecheuse)) %>%
  mutate(
    village = stringr::str_to_title(trimws(village)),
    village = if_else(
      is.na(village) | village == "",
      "Non renseigné",
      village
    )
  ) %>%
  group_by(id_pecheuse) %>%
  summarise(
    nb_villages = n_distinct(village[village != "Non renseigné"]),
    village = case_when(
      nb_villages == 0 ~ "Non renseigné",
      nb_villages == 1 ~ first(village[village != "Non renseigné"]),
      TRUE ~ "Plusieurs villages"
    ),
    .groups = "drop"
  )

# Vérifier qu'une pêcheuse n'est pas associée à plusieurs villages
if (any(pecheuses_uniques$village == "Plusieurs villages")) {
  stop("Attention : certains id_pecheuse sont associés à plusieurs villages.")
}

# Nombre total de pêcheuses distinctes
nombre_total_pecheuses <- n_distinct(pecheuses_uniques$id_pecheuse)

# Nombre de pêcheuses par village
nombre_pecheuses_village <- pecheuses_uniques %>%
  count(village, name = "nombre_pecheuses") %>%
  mutate(
    pourcentage = 100 * nombre_pecheuses / nombre_total_pecheuses
  ) %>%
  arrange(village)

# Afficher les résultats
cat("Nombre total de pêcheuses :", nombre_total_pecheuses, "\n\n")

print(nombre_pecheuses_village)

# ----------------------------------------------------------
### FIGURE : EFFECTIFS DES PECHEUSES PAR VILLAGE
# ----------------------------------------------------------

library(ggplot2)

figure_pecheuses <- ggplot(
  nombre_pecheuses_village,
  aes(
    x = reorder(village, nombre_pecheuses),
    y = nombre_pecheuses
  )
) +
  geom_col(
    fill = "grey45",
    width = 0.65
  ) +
  geom_text(
    aes(label = nombre_pecheuses),
    vjust = -0.5,
    size = 5
  ) +
  labs(
    title = "Répartition des pêcheuses suivies par village",
    subtitle = paste(
      "Effectif total :", nombre_total_pecheuses,
      "pêcheuses identifiées"
    ),
    x = "Village",
    y = "Nombre de pêcheuses"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15))
  ) +
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      size = 12,
      color = "grey35"
    ),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(size = 12)
  )

print(figure_pecheuses)











































# ----------------------------------------------------------
### nombre total de sélections de chaque zone, toutes pêcheuses confondues
# ----------------------------------------------------------

tableau_choix <- df %>%
  filter(
    !is.na(id_pecheuse),
    !is.na(date_peche)
  ) %>%
  mutate(
    zone = tolower(trimws(as.character(zone))),
    choix = case_when(
      type_peche == "pas_peche" ~ "Village",
      type_peche %in% c("pied", "pirogue") & zone == "rouge" ~ "Rouge",
      type_peche %in% c("pied", "pirogue") & zone == "vert" ~ "Vert",
      type_peche %in% c("pied", "pirogue") & zone == "jaune" ~ "Jaune",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(choix)) %>%
  count(choix, name = "nombre_occurrences") %>%
  complete(
    choix = c("Rouge", "Vert", "Jaune", "Village"),
    fill = list(nombre_occurrences = 0)
  ) %>%
  mutate(
    probabilite_selection =
      100 * nombre_occurrences / sum(nombre_occurrences)
  ) %>%
  arrange(factor(
    choix,
    levels = c("Rouge", "Vert", "Jaune", "Village")
  ))

print(tableau_choix)


# ----------------------------------------------------------
### PROBABILITES DE SELECTION PAR VILLAGE
# ----------------------------------------------------------

tableau_choix_village <- df %>%
  filter(
    !is.na(id_pecheuse),
    !is.na(date_peche),
    village %in% c("falia", "niodior")
  ) %>%
  mutate(
    zone = tolower(trimws(as.character(zone))),
    choix = case_when(
      zone == "village" ~ "Village",
      zone == "rouge" ~ "Rouge",
      zone == "vert" ~ "Vert",
      zone == "jaune" ~ "Jaune",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(choix)) %>%
  count(village, choix, name = "nombre_occurrences") %>%
  complete(
    village = c("falia", "niodior"),
    choix = c("Rouge", "Vert", "Jaune", "Village"),
    fill = list(nombre_occurrences = 0)
  ) %>%
  group_by(village) %>%
  mutate(
    probabilite_selection =
      100 * nombre_occurrences / sum(nombre_occurrences)
  ) %>%
  ungroup() %>%
  arrange(
    factor(village, levels = c("Falia", "Niodior")),
    factor(choix, levels = c("Rouge", "Vert", "Jaune", "Village"))
  )

print(tableau_choix_village, n = Inf)






















































# ----------------------------------------------------------
# DISTRIBUTION DES RENDEMENTS PAR VILLAGE
# ----------------------------------------------------------

graph_rendements <- ggplot(
  df %>%
    filter(
      !is.na(rendement_pecheuse),
      village %in% c("falia", "niodior")
    ),
  aes(
    x = rendement_pecheuse,
    fill = village
  )
) +
  geom_histogram(
    bins = 30,
    color = "white",
    alpha = 0.7,
    position = "identity"
  ) +
  scale_fill_manual(
    values = c(
      "falia" = "#6A51A3",
      "niodior" = "#E76FAD"
    )
  ) +
  labs(
    title = "Distribution des rendements individuels par village",
    x = "Rendement (bassines/heure/pêcheuse)",
    y = "Nombre d'observations",
    fill = "Village"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

print(graph_rendements)
















































# ============================================================
# ANALYSE DES PROFILS COMPORTEMENTAUX DES PECHEUSES
# Classification hiérarchique à partir de la variable zone
# ============================================================

# Ce script doit être exécuté après le nettoyage initial de df.

# ============================================================
# 0. PACKAGES
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(ggplot2)
library(cluster)
library(factoextra)
library(writexl)

# ============================================================
# 1. PREPARATION DES DONNEES
# ============================================================

# ------------------------------------------------------------
# 1.1 Vérifier les modalités de la variable zone
# ------------------------------------------------------------

print(table(df$zone, useNA = "ifany"))

# ------------------------------------------------------------
# 1.2 Nettoyer les noms des zones
# ------------------------------------------------------------

df <- df %>%
  mutate(
    zone_clean = as.character(zone),
    
    zone_clean = str_to_lower(zone_clean),
    
    zone_clean = stri_trans_general(
      zone_clean,
      "Latin-ASCII"
    ),
    
    zone_clean = str_trim(zone_clean)
  )

# ------------------------------------------------------------
# 1.3 Recoder les zones
# ------------------------------------------------------------

df <- df %>%
  mutate(
    zone_profil = case_when(
      
      zone_clean == "village" ~ "village",
      
      zone_clean == "rouge" ~ "rouge",
      
      zone_clean == "jaune" ~ "jaune",
      
      zone_clean == "vert" ~ "vert",
      
      is.na(zone_clean) |
        zone_clean == "" ~ NA_character_,
      
      TRUE ~ "autre"
    )
  )

# Vérification du recodage

print(table(df$zone_profil, useNA = "ifany"))

# ============================================================
# 2. IDENTIFICATION DES PECHEUSES
# ============================================================

# Identifiant individuel :
# village + id_pecheuse
#
# Cela évite de confondre des identifiants identiques
# dans deux villages différents.

df <- df %>%
  mutate(
    id_individu = if_else(
      !is.na(id_pecheuse),
      paste(
        village,
        id_pecheuse,
        sep = "_"
      ),
      NA_character_
    )
  )

# Nombre de pêcheuses identifiées

cat(
  "Nombre de pêcheuses identifiées :",
  n_distinct(df$id_individu, na.rm = TRUE),
  "\n"
)

# Nombre de lignes sans identifiant

cat(
  "Lignes sans identifiant :",
  sum(is.na(df$id_individu)),
  "\n"
)

# ============================================================
# 3. CONSTRUCTION DE LA BASE COMPORTEMENTALE
# ============================================================

# On conserve les observations avec :
# - un identifiant individuel
# - une zone renseignée

df_comportement <- df %>%
  filter(
    !is.na(id_individu),
    !is.na(zone_profil)
  )

# ------------------------------------------------------------
# 3.1 Compter les observations par pêcheuse et par zone
# ------------------------------------------------------------

comportement_individuel <- df_comportement %>%
  
  group_by(
    village,
    id_individu,
    zone_profil
  ) %>%
  
  summarise(
    nb_jours = n(),
    .groups = "drop"
  ) %>%
  
  # CORRECTION DU CONFLIT DE NOM :
  # la modalité "village" devient "zone_village"
  mutate(
    zone_profil = paste0(
      "zone_",
      zone_profil
    )
  ) %>%
  
  pivot_wider(
    names_from = zone_profil,
    values_from = nb_jours,
    values_fill = 0
  )

# Vérifier les noms des colonnes

print(names(comportement_individuel))

# ============================================================
# 4. COMPLETER LES COLONNES DE ZONES
# ============================================================

zones_attendues <- c(
  "zone_village",
  "zone_rouge",
  "zone_jaune",
  "zone_vert",
  "zone_autre"
)

# Ajouter les colonnes absentes si nécessaire

for(z in zones_attendues) {
  
  if(!z %in% names(comportement_individuel)) {
    
    comportement_individuel[[z]] <- 0
    
  }
  
}

# ============================================================
# 5. CALCUL DES PROPORTIONS INDIVIDUELLES
# ============================================================

comportement_individuel <- comportement_individuel %>%
  
  mutate(
    
    # Nombre total d'observations
    nb_jours_observes =
      zone_village +
      zone_rouge +
      zone_jaune +
      zone_vert +
      zone_autre,
    
    # Proportions par comportement
    prop_village =
      zone_village / nb_jours_observes,
    
    prop_rouge =
      zone_rouge / nb_jours_observes,
    
    prop_jaune =
      zone_jaune / nb_jours_observes,
    
    prop_vert =
      zone_vert / nb_jours_observes,
    
    prop_autre =
      zone_autre / nb_jours_observes
    
  )

# ------------------------------------------------------------
# 5.1 Vérification des proportions
# ------------------------------------------------------------

comportement_individuel <- comportement_individuel %>%
  mutate(
    somme_proportions =
      prop_village +
      prop_rouge +
      prop_jaune +
      prop_vert +
      prop_autre
  )

print(
  summary(
    comportement_individuel$somme_proportions
  )
)

# ============================================================
# 6. FILTRER LES PECHEUSES TROP PEU OBSERVEES
# ============================================================

# Seuil minimal de jours observés
# A adapter selon la qualité de la base

seuil_jours <- 5

comportement_classification <- comportement_individuel %>%
  filter(
    nb_jours_observes >= seuil_jours
  )

cat(
  "Pêcheuses retenues pour la classification :",
  nrow(comportement_classification),
  "\n"
)

cat(
  "Pêcheuses exclues :",
  nrow(comportement_individuel) -
    nrow(comportement_classification),
  "\n"
)

# ============================================================
# 7. CLASSIFICATION HIERARCHIQUE
# ============================================================

# ------------------------------------------------------------
# 7.1 Variables utilisées pour la classification
# ------------------------------------------------------------

variables_classification <- c(
  "prop_village",
  "prop_rouge",
  "prop_jaune",
  "prop_vert",
  "prop_autre"
)

# ------------------------------------------------------------
# 7.2 Matrice des variables
# ------------------------------------------------------------

matrice_classification <- comportement_classification %>%
  select(
    all_of(variables_classification)
  ) %>%
  as.data.frame()

# ------------------------------------------------------------
# 7.3 Standardisation
# ------------------------------------------------------------

matrice_standardisee <- scale(
  matrice_classification
)

# ------------------------------------------------------------
# 7.4 Calcul des distances
# ------------------------------------------------------------

distance_individus <- dist(
  matrice_standardisee,
  method = "euclidean"
)

# ------------------------------------------------------------
# 7.5 Classification hiérarchique de Ward
# ------------------------------------------------------------

classification_cah <- hclust(
  distance_individus,
  method = "ward.D2"
)

# ============================================================
# 8. CHOIX DU NOMBRE DE PROFILS
# ============================================================

# Tester de 2 à 8 profils

k_max <- min(
  8,
  nrow(comportement_classification) - 1
)

resultats_silhouette <- data.frame(
  k = 2:k_max,
  silhouette_moyenne = NA_real_
)

for(i in seq_len(nrow(resultats_silhouette))) {
  
  k <- resultats_silhouette$k[i]
  
  groupes <- cutree(
    classification_cah,
    k = k
  )
  
  sil <- silhouette(
    groupes,
    distance_individus
  )
  
  resultats_silhouette$silhouette_moyenne[i] <-
    mean(sil[, 3])
  
}

# Afficher les résultats

print(resultats_silhouette)

# ------------------------------------------------------------
# 8.1 Graphe de silhouette moyenne
# ------------------------------------------------------------

p_silhouette <- ggplot(
  resultats_silhouette,
  aes(
    x = k,
    y = silhouette_moyenne
  )
) +
  
  geom_line() +
  
  geom_point(size = 3) +
  
  scale_x_continuous(
    breaks = 2:k_max
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Choix du nombre de profils",
    x = "Nombre de profils",
    y = "Silhouette moyenne"
  )

print(p_silhouette)

# ------------------------------------------------------------
# 8.2 Nombre optimal de profils
# ------------------------------------------------------------

k_optimal <- resultats_silhouette$k[
  which.max(
    resultats_silhouette$silhouette_moyenne
  )
]

cat(
  "Nombre optimal de profils selon la silhouette :",
  k_optimal,
  "\n"
)

# ============================================================
# 9. DENDROGRAMME
# ============================================================

p_dendrogramme <- fviz_dend(
  classification_cah,
  k = k_optimal,
  rect = TRUE,
  rect_fill = TRUE,
  cex = 0.5,
  main = "Dendrogramme des profils de pêcheuses",
  xlab = "Pêcheuses",
  ylab = "Distance de classification"
)

print(p_dendrogramme)


# ============================================================
# 10. SILHOUETTE DE LA CLASSIFICATION FINALE
# ============================================================

groupes_optimaux <- cutree(
  classification_cah,
  k = k_optimal
)

silhouette_finale <- silhouette(
  groupes_optimaux,
  distance_individus
)

p_silhouette_finale <- fviz_silhouette(
  silhouette_finale,
  print.summary = TRUE
)

print(p_silhouette_finale)

# ============================================================
# 11. ATTRIBUTION DES PROFILS
# ============================================================

comportement_classification <- comportement_classification %>%
  mutate(
    profil = factor(
      groupes_optimaux
    )
  )

# ============================================================
# 12. EFFECTIFS ET POURCENTAGES DES PROFILS
# ============================================================

effectifs_profils <- comportement_classification %>%
  
  count(
    profil,
    name = "nb_pecheuses"
  ) %>%
  
  mutate(
    pourcentage =
      100 * nb_pecheuses /
      sum(nb_pecheuses)
  )

print(effectifs_profils)

# ============================================================
# 13. MATRICE EXPLICATIVE DES PROFILS
# ============================================================

matrice_profils <- comportement_classification %>%
  
  group_by(profil) %>%
  
  summarise(
    
    nb_pecheuses = n(),
    
    jours_observes_moyens =
      mean(nb_jours_observes),
    
    prop_village_moyenne =
      mean(prop_village),
    
    prop_rouge_moyenne =
      mean(prop_rouge),
    
    prop_jaune_moyenne =
      mean(prop_jaune),
    
    prop_vert_moyenne =
      mean(prop_vert),
    
    prop_autre_moyenne =
      mean(prop_autre),
    
    .groups = "drop"
    
  )

print(matrice_profils)

# ============================================================
# 14. GRAPHIQUE DES CARACTERISTIQUES DES PROFILS
# ============================================================

matrice_comportements <- comportement_classification %>%
  
  group_by(profil) %>%
  
  summarise(
    across(
      all_of(variables_classification),
      mean
    ),
    .groups = "drop"
  )

matrice_longue <- matrice_comportements %>%
  
  pivot_longer(
    cols = all_of(variables_classification),
    names_to = "comportement",
    values_to = "proportion"
  )

p_profils <- ggplot(
  matrice_longue,
  aes(
    x = comportement,
    y = proportion,
    fill = profil
  )
) +
  
  geom_col(
    position = "dodge"
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Caractéristiques comportementales des profils",
    x = "Comportement",
    y = "Proportion moyenne",
    fill = "Profil"
  ) +
  
  scale_y_continuous(
    labels = scales::percent
  )

print(p_profils)


# ============================================================
# 15. REPARTITION DES PROFILS PAR VILLAGE
# ============================================================

# ------------------------------------------------------------
# 15.1 Effectifs par village et profil
# ------------------------------------------------------------

profils_village <- comportement_classification %>%
  
  count(
    village,
    profil,
    name = "nb_pecheuses"
  )

# ------------------------------------------------------------
# 15.2 Pourcentage au sein de chaque village
# ------------------------------------------------------------

profils_village <- profils_village %>%
  
  group_by(village) %>%
  
  mutate(
    
    total_village = sum(nb_pecheuses),
    
    pourcentage_village =
      100 * nb_pecheuses /
      total_village
    
  ) %>%
  
  ungroup()

print(profils_village)

# ------------------------------------------------------------
# 15.3 Tableau croisé village x profil
# ------------------------------------------------------------

tableau_village_profil <- profils_village %>%
  
  select(
    village,
    profil,
    pourcentage_village
  ) %>%
  
  pivot_wider(
    names_from = profil,
    values_from = pourcentage_village,
    values_fill = 0
  )

print(tableau_village_profil)

# ------------------------------------------------------------
# 15.4 Graphique des profils par village
# ------------------------------------------------------------

p_village <- ggplot(
  profils_village,
  aes(
    x = village,
    y = pourcentage_village,
    fill = profil
  )
) +
  
  geom_col() +
  
  theme_minimal() +
  
  labs(
    title = "Répartition des profils par village",
    x = "Village",
    y = "Pourcentage de pêcheuses",
    fill = "Profil"
  ) +
  
  scale_y_continuous(
    labels = scales::percent_format(
      scale = 1
    )
  )

print(p_village)

























































































































































































































































































































































































































############################################################
# ANALYSE DES COMPORTEMENTS DES PECHEUSES MBISSA
# Profils combinant intensité, zones et rendement
############################################################

# ----------------------------------------------------------
# 1. NETTOYAGE ET PREPARATION DES DONNEES
# ----------------------------------------------------------

df <- df_brut %>%
  janitor::clean_names()

# Conversion des dates
convertir_date <- function(x) {
  
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  
  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  
  suppressWarnings(
    as.Date(lubridate::parse_date_time(
      as.character(x),
      orders = c("dmy", "ymd", "dmy HMS", "ymd HMS")
    ))
  )
}

df <- df %>%
  mutate(
    date_peche = convertir_date(date_peche),
    date_debut_mbissa = convertir_date(date_debut_mbissa),
    
    id_pecheuse = as.character(id_pecheuse),
    peche = as.numeric(as.character(peche)),
    
    rendement_pecheuse = as.numeric(rendement_pecheuse),
    
    village = stringr::str_to_title(
      stringr::str_trim(as.character(village))
    ),
    
    zone = stringr::str_to_lower(
      stringr::str_trim(as.character(zone))
    ),
    
    zone = na_if(zone, ""),
    id_pecheuse = na_if(id_pecheuse, "")
  )

# Vérifications initiales
cat("\nNombre de lignes :", nrow(df), "\n")
cat("Nombre de pêcheuses :", n_distinct(df$id_pecheuse, na.rm = TRUE), "\n")

print(table(df$peche, useNA = "ifany"))
print(table(df$zone, useNA = "ifany"))
print(table(df$village, useNA = "ifany"))

# Vérifier les valeurs de peche
if (any(!na.omit(df$peche) %in% c(0, 1))) {
  stop("La variable peche contient des valeurs autres que 0 et 1.")
}

# Vérifier qu'un identifiant correspond à un seul village
controle_village <- df %>%
  filter(!is.na(id_pecheuse)) %>%
  group_by(id_pecheuse) %>%
  summarise(
    nombre_villages = n_distinct(village, na.rm = TRUE),
    villages = paste(unique(na.omit(village)), collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(nombre_villages > 1)

if (nrow(controle_village) > 0) {
  print(controle_village)
  stop("Certains identifiants sont associés à plusieurs villages. Corriger avant de poursuivre.")
}


# ----------------------------------------------------------
# 2. CONSTRUCTION DES OBSERVATIONS JOURNALIERES
# ----------------------------------------------------------

# Une ligne par pêcheuse et par date.
# Si plusieurs lignes existent pour une journée :
# - peche_jour = 1 si au moins une ligne indique peche = 1 ;
# - le rendement est la moyenne des rendements des lignes de pêche ;
# - les zones sont regroupées.

donnees_journalieres <- df %>%
  filter(
    !is.na(id_pecheuse),
    !is.na(date_peche)
  ) %>%
  group_by(id_pecheuse, date_peche) %>%
  summarise(
    
    peche_jour = if (all(is.na(peche))) {
      NA_real_
    } else {
      max(peche, na.rm = TRUE)
    },
    
    rendement_jour = {
      valeurs <- rendement_pecheuse[peche == 1 & !is.na(peche)]
      if (length(valeurs) == 0 || all(is.na(valeurs))) {
        NA_real_
      } else {
        mean(valeurs, na.rm = TRUE)
      }
    },
    
    zones_jour = list(
      sort(unique(
        zone[peche == 1 & !is.na(peche) &
               !is.na(zone) &
               zone %in% c("rouge", "vert", "jaune")]
      ))
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    
    # Le village représente les journées sans pêche.
    # Les journées de pêche sans zone connue sont conservées
    # dans une modalité distincte.
    
    zone_jour = purrr::map_chr(
      seq_len(n()),
      function(i) {
        
        if (is.na(peche_jour[i])) {
          return(NA_character_)
        }
        
        if (peche_jour[i] == 0) {
          return("village")
        }
        
        zones <- zones_jour[[i]]
        
        if (length(zones) == 0) {
          return("non_renseignee")
        }
        
        if (length(zones) == 1) {
          return(zones)
        }
        
        return("multi_zone")
      }
    )
  )

# Vérification : une seule ligne par pêcheuse et par date
stopifnot(
  nrow(donnees_journalieres) ==
    nrow(distinct(donnees_journalieres, id_pecheuse, date_peche))
)

# Contrôle des journées avec plusieurs zones
jours_multi_zones <- donnees_journalieres %>%
  filter(zone_jour == "multi_zone")

print(jours_multi_zones)


# ----------------------------------------------------------
# 3. INDICATEURS INDIVIDUELS D'INTENSITE ET DE RENDEMENT
# ----------------------------------------------------------

indicateurs_individuels <- donnees_journalieres %>%
  group_by(id_pecheuse) %>%
  summarise(
    
    nombre_jours_renseignes = sum(!is.na(peche_jour)),
    
    nombre_jours_peche = sum(peche_jour == 1, na.rm = TRUE),
    
    nombre_jours_sans_peche = sum(peche_jour == 0, na.rm = TRUE),
    
    intensite_peche = ifelse(
      nombre_jours_renseignes > 0,
      nombre_jours_peche / nombre_jours_renseignes,
      NA_real_
    ),
    
    rendement_moyen = {
      x <- rendement_jour[peche_jour == 1]
      if (length(x) == 0 || all(is.na(x))) NA_real_
      else mean(x, na.rm = TRUE)
    },
    
    rendement_median = {
      x <- rendement_jour[peche_jour == 1]
      if (length(x) == 0 || all(is.na(x))) NA_real_
      else median(x, na.rm = TRUE)
    },
    
    .groups = "drop"
  )

print(indicateurs_individuels)


# ----------------------------------------------------------
# 4. INDICATEURS DE FREQUENTATION DES ZONES
# ----------------------------------------------------------

# Les proportions sont calculées sur les journées renseignées.
# Les journées de pêche sans zone et les journées multi-zones
# sont conservées comme modalités à part entière.

modalites_zones <- c(
  "village",
  "rouge",
  "vert",
  "jaune",
  "non_renseignee",
  "multi_zone"
)

frequentation_zones <- donnees_journalieres %>%
  filter(!is.na(peche_jour)) %>%
  count(id_pecheuse, zone_jour, name = "nombre_jours_zone") %>%
  tidyr::complete(
    id_pecheuse,
    zone_jour = modalites_zones,
    fill = list(nombre_jours_zone = 0)
  ) %>%
  group_by(id_pecheuse) %>%
  mutate(
    nombre_jours_classes = sum(nombre_jours_zone),
    proportion_zone = ifelse(
      nombre_jours_classes > 0,
      nombre_jours_zone / nombre_jours_classes,
      NA_real_
    )
  ) %>%
  ungroup()

matrice_zones <- frequentation_zones %>%
  select(id_pecheuse, zone_jour, proportion_zone) %>%
  pivot_wider(
    names_from = zone_jour,
    values_from = proportion_zone,
    names_prefix = "part_",
    values_fill = 0
  )

print(matrice_zones)


# ----------------------------------------------------------
# 5. CONSTRUCTION DE LA BASE INDIVIDUELLE
# ----------------------------------------------------------

village_individuel <- df %>%
  filter(!is.na(id_pecheuse), !is.na(village)) %>%
  distinct(id_pecheuse, village) %>%
  distinct(id_pecheuse, .keep_all = TRUE)

base_profils <- indicateurs_individuels %>%
  left_join(matrice_zones, by = "id_pecheuse") %>%
  left_join(village_individuel, by = "id_pecheuse")

# Couverture du suivi : part des dates présentes dans la base
nombre_dates_inventoriees <- n_distinct(
  df$date_peche,
  na.rm = TRUE
)

base_profils <- base_profils %>%
  mutate(
    couverture_suivi = nombre_jours_renseignes /
      nombre_dates_inventoriees
  )

# Vérification de la base
glimpse(base_profils)

# Vérifier que les proportions spatiales totalisent 1
base_profils <- base_profils %>%
  mutate(
    somme_proportions_zones = rowSums(
      across(starts_with("part_")),
      na.rm = TRUE
    )
  )

summary(base_profils$somme_proportions_zones)



# ----------------------------------------------------------
# 6. CLASSIFICATION COMPORTEMENTALE
# ----------------------------------------------------------

# Variables utilisées pour construire les profils :
# - fréquentation des zones ;
# - intensité de pêche ;
# - rendement moyen.
#
# La part de zones non renseignées est exclue de la classification,
# car elle représente une information sur la qualité du suivi.

variables_profils <- base_profils %>%
  select(
    part_village,
    part_rouge,
    part_vert,
    part_jaune,
    part_multi_zone,
    intensite_peche,
    rendement_moyen
  )

# Vérifier les valeurs manquantes
print(colSums(is.na(variables_profils)))

# Retirer les variables constantes ou entièrement manquantes
variables_utiles <- variables_profils %>%
  select(where(~ sum(!is.na(.x)) > 1 &&
                 n_distinct(.x, na.rm = TRUE) > 1))

if (ncol(variables_utiles) < 2) {
  stop("Pas assez de variables informatives pour réaliser la classification.")
}

# ----------------------------------------------------------
# 6.1 DISTANCE DE GOWER
# ----------------------------------------------------------

distance_profils <- cluster::daisy(
  variables_utiles,
  metric = "gower"
)

matrice_distance <- as.matrix(distance_profils)

if (anyNA(matrice_distance) || any(!is.finite(matrice_distance))) {
  stop("La matrice de distances contient des valeurs manquantes ou infinies.")
}

# ----------------------------------------------------------
# 6.2 COMPARAISON DU NOMBRE DE PROFILS
# ----------------------------------------------------------

n_individus <- nrow(variables_utiles)

if (n_individus < 4) {
  stop("Il faut au moins 4 pêcheuses pour réaliser la classification.")
}

k_max <- min(6, n_individus - 1)

silhouette_profils <- purrr::map_dfr(
  2:k_max,
  function(k) {
    
    modele <- cluster::pam(
      distance_profils,
      k = k,
      diss = TRUE
    )
    
    tibble(
      k = k,
      silhouette = modele$silinfo$avg.width
    )
  }
)

print(silhouette_profils)

# Graphique des silhouettes
graph_silhouette <- ggplot(
  silhouette_profils,
  aes(x = k, y = silhouette)
) +
  geom_line() +
  geom_point(size = 3) +
  geom_point(
    data = filter(silhouette_profils, k == 3),
    aes(x = k, y = silhouette),
    color = "#6A51A3",
    size = 4
  ) +
  scale_x_continuous(breaks = 2:k_max) +
  labs(
    title = "Comparaison du nombre de profils comportementaux",
    x = "Nombre de profils",
    y = "Silhouette moyenne"
  ) +
  theme_classic(base_size = 12)

print(graph_silhouette)

ggsave(
  file.path(dossier_resultats, "06_comparaison_silhouettes.png"),
  graph_silhouette,
  width = 16,
  height = 11,
  units = "cm",
  dpi = 300
)

# ----------------------------------------------------------
# 6.3 DENDROGRAMME
# ----------------------------------------------------------

# Classification hiérarchique à partir de la même distance de Gower.
# La méthode de Ward n'est pas utilisée car elle n'est pas adaptée
# à toutes les distances non euclidiennes.
# On utilise ici la méthode average (liaison moyenne).

modele_hierarchique <- hclust(
  as.dist(matrice_distance),
  method = "average"
)

# Dendrogramme avec trois groupes visualisés
graph_dendrogramme <- factoextra::fviz_dend(
  modele_hierarchique,
  k = 3,
  cex = 0.3,
  k_colors = c("#6A51A3", "#E76FAD", "#4D908E"),
  color_labels_by_k = TRUE,
  rect = TRUE,
  rect_fill = TRUE,
  rect_border = c("#6A51A3", "#E76FAD", "#4D908E"),
  show_labels = FALSE
) +
  labs(
    title = "Dendrogramme des ressemblances comportementales",
    subtitle = "Classification hiérarchique fondée sur la distance de Gower",
    x = "Pêcheuses",
    y = "Dissimilarité"
  ) +
  theme_classic(base_size = 12)

print(graph_dendrogramme)

ggsave(
  file.path(dossier_resultats, "07_dendrogramme_profils.png"),
  graph_dendrogramme,
  width = 20,
  height = 13,
  units = "cm",
  dpi = 300
)

# ----------------------------------------------------------
# 6.4 CLASSIFICATION PAM EN TROIS PROFILS
# ----------------------------------------------------------

# Trois profils sont retenus pour disposer d'une typologie
# suffisamment synthétique pour le paramétrage des agents du SMA.

nombre_profils <- 3

modele_profils <- cluster::pam(
  distance_profils,
  k = nombre_profils,
  diss = TRUE
)

# Affectation des profils aux pêcheuses
base_profils <- base_profils %>%
  mutate(
    profil = factor(
      modele_profils$clustering,
      levels = 1:3,
      labels = c("Profil 1", "Profil 2", "Profil 3")
    )
  )

# Effectifs par profil
effectifs_profils <- base_profils %>%
  count(profil, name = "nombre_pecheuses") %>%
  mutate(
    pourcentage = 100 * nombre_pecheuses / sum(nombre_pecheuses)
  )

print(effectifs_profils)


# ----------------------------------------------------------
# 7. DESCRIPTION DETAILLEE DES PROFILS
# ----------------------------------------------------------

moyenne_sans_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

mediane_sans_na <- function(x) {
  if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE)
}

description_profils <- base_profils %>%
  group_by(profil) %>%
  summarise(
    
    nombre_pecheuses = n(),
    
    # Fréquentation des zones
    part_village = moyenne_sans_na(part_village),
    part_rouge = moyenne_sans_na(part_rouge),
    part_vert = moyenne_sans_na(part_vert),
    part_jaune = moyenne_sans_na(part_jaune),
    part_multi_zone = moyenne_sans_na(part_multi_zone),
    
    # Qualité du renseignement spatial
    part_non_renseignee = moyenne_sans_na(part_non_renseignee),
    
    # Intensité de pêche
    intensite_moyenne = moyenne_sans_na(intensite_peche),
    intensite_mediane = mediane_sans_na(intensite_peche),
    
    # Rendement
    rendement_moyen_profil = moyenne_sans_na(rendement_moyen),
    rendement_median_profil = mediane_sans_na(rendement_moyen),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Conversion des proportions en pourcentages
    across(
      starts_with("part_"),
      ~ 100 * .x
    ),
    intensite_moyenne = 100 * intensite_moyenne,
    intensite_mediane = 100 * intensite_mediane
  )

print(description_profils, width = Inf)

# Export des descriptions
write_xlsx(
  list(
    "Effectifs" = effectifs_profils,
    "Description profils" = description_profils,
    "Silhouettes" = silhouette_profils
  ),
  file.path(dossier_resultats, "08_description_profils_comportementaux.xlsx")
)



# ----------------------------------------------------------
# 8. REPARTITION DES PROFILS PAR VILLAGE
# ----------------------------------------------------------

repartition_profils_village <- base_profils %>%
  filter(!is.na(village), !is.na(profil)) %>%
  mutate(
    village = stringr::str_to_lower(stringr::str_trim(village))
  ) %>%
  count(village, profil, name = "nombre_pecheuses") %>%
  group_by(village) %>%
  mutate(
    effectif_village = sum(nombre_pecheuses),
    pourcentage = 100 * nombre_pecheuses / effectif_village
  ) %>%
  ungroup()

print(repartition_profils_village)

# Graphique
graph_repartition_village <- ggplot(
  repartition_profils_village,
  aes(
    x = profil,
    y = pourcentage,
    fill = village
  )
) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      "falia" = "#6A51A3",
      "niodior" = "#E76FAD"
    ),
    labels = c(
      "falia" = "Falia",
      "niodior" = "Niodior"
    )
  ) +
  labs(
    title = "Répartition des profils comportementaux par village",
    x = "Profil comportemental",
    y = "Pêcheuses (%)",
    fill = "Village"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

print(graph_repartition_village)


























































































































#######################################################################
## Évolution annuelle de l'intensité de pêche par village
#######################################################################

library(tidyverse)
library(lubridate)

#######################################################################
## 1. Préparation des données
#######################################################################

donnees_intensite <- df %>%
  filter(
    !is.na(date_peche),
    peche %in% c(0, 1),
    !is.na(id_individu),
    !is.na(village)
  ) %>%
  mutate(
    date_peche = as.Date(date_peche),
    annee = year(date_peche),
    village = str_to_lower(village)
  )

#######################################################################
## 2. Calcul du nombre de pêcheuses ayant pêché chaque jour
#######################################################################

nombre_pecheuses_journalier <- donnees_intensite %>%
  filter(peche == 1) %>%
  group_by(annee, date_peche, village) %>%
  summarise(
    nombre_pecheuses = n_distinct(id_individu),
    .groups = "drop"
  ) %>%
  mutate(
    jour_annee = yday(date_peche)
  )

#######################################################################
## 3. Préparation des segments à relier
#######################################################################

# Les points sont reliés uniquement si moins de 4 jours les séparent.
# Le calcul est réalisé séparément pour chaque village et chaque année.

segments_peche <- nombre_pecheuses_journalier %>%
  arrange(annee, village, date_peche) %>%
  group_by(annee, village) %>%
  mutate(
    date_suivante = lead(date_peche),
    nombre_suivant = lead(nombre_pecheuses),
    jour_suivant = lead(jour_annee),
    ecart_jours = as.numeric(date_suivante - date_peche)
  ) %>%
  filter(
    !is.na(date_suivante),
    ecart_jours < 4
  ) %>%
  ungroup()

#######################################################################
## 4. Position des mois sur l'axe horizontal
#######################################################################

# On utilise une année bissextile de référence pour positionner les mois.

positions_mois <- tibble(
  mois = 1:12,
  jour = yday(
    as.Date(paste0("2024-", sprintf("%02d", 1:12), "-15"))
  )
)

#######################################################################
## 5. Graphique
#######################################################################

graphique_intensite_peche <- ggplot() +
  
  # Lignes reliant les dates suffisamment proches
  geom_segment(
    data = segments_peche,
    aes(
      x = jour_annee,
      xend = jour_suivant,
      y = nombre_pecheuses,
      yend = nombre_suivant,
      color = village
    ),
    linewidth = 0.8
  ) +
  
  # Points correspondant aux dates observées
  geom_point(
    data = nombre_pecheuses_journalier,
    aes(
      x = jour_annee,
      y = nombre_pecheuses,
      color = village
    ),
    size = 1.8
  ) +
  
  # Un panneau par année
  facet_wrap(
    ~ annee,
    ncol = 1,
    scales = "free_y"
  ) +
  
  # Couleurs des villages
  scale_color_manual(
    values = c(
      "falia" = "#6A51A3",
      "niodior" = "#E76FAD"
    ),
    labels = c(
      "falia" = "Falia",
      "niodior" = "Niodior"
    ),
    name = "Village"
  ) +
  
  # Axe horizontal : mois
  scale_x_continuous(
    breaks = positions_mois$jour,
    labels = month.abb,
    limits = c(1, 366),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    breaks = scales::pretty_breaks()
  ) +
  
  labs(
    x = "Mois de l'année",
    y = "Nombre de pêcheuses ayant pêché",
    title = "Évolution annuelle de l'intensité de pêche",
    subtitle = "Nombre de pêcheuses ayant pêché chaque jour, par village",
    caption = "Les points correspondent aux dates observées. Les lignes relient uniquement les dates séparées de moins de 4 jours."
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    axis.text.x = element_text(
      size = 10
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Afficher le graphique
print(graphique_intensite_peche)
























#######################################################################
## Évolution annuelle du pourcentage de pêcheuses ayant pêché
## Comparaison entre Falia et Niodior
#######################################################################

library(tidyverse)
library(lubridate)

#######################################################################
## 1. Préparation des données
#######################################################################

donnees_intensite <- df %>%
  filter(
    !is.na(date_peche),
    peche %in% c(0, 1),
    !is.na(id_individu),
    !is.na(village)
  ) %>%
  mutate(
    date_peche = as.Date(date_peche),
    annee = year(date_peche),
    village = str_to_lower(village)
  )

#######################################################################
## 2. Calcul du nombre de pêcheuses interrogées et ayant pêché
#######################################################################

effectifs_journaliers <- donnees_intensite %>%
  group_by(annee, date_peche, village) %>%
  summarise(
    # Nombre total de pêcheuses interrogées ce jour-là
    nombre_interrogees = n_distinct(id_individu),
    
    # Nombre de pêcheuses ayant effectivement pêché
    nombre_pecheuses = n_distinct(
      id_individu[peche == 1]
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Pourcentage de pêcheuses ayant pêché
    pourcentage_peche = 100 * nombre_pecheuses /
      nombre_interrogees
  )

#######################################################################
## 3. Position des dates sur l'axe annuel
#######################################################################

# On utilise une année de référence bissextile pour que les mois
# soient alignés de la même manière sur tous les graphiques.

effectifs_journaliers <- effectifs_journaliers %>%
  mutate(
    jour_annee = yday(
      as.Date(
        paste0(
          "2024-",
          format(date_peche, "%m-%d")
        )
      )
    )
  )

#######################################################################
## 4. Préparation des segments à relier
#######################################################################

# Les points sont reliés uniquement si moins de 4 jours les séparent.
# Les segments sont calculés séparément par année et par village.

segments_peche <- effectifs_journaliers %>%
  arrange(annee, village, date_peche) %>%
  group_by(annee, village) %>%
  mutate(
    date_suivante = lead(date_peche),
    pourcentage_suivant = lead(pourcentage_peche),
    jour_suivant = lead(jour_annee),
    ecart_jours = as.numeric(
      date_suivante - date_peche
    )
  ) %>%
  filter(
    !is.na(date_suivante),
    ecart_jours < 4
  ) %>%
  ungroup()

#######################################################################
## 5. Position des mois sur l'axe horizontal
#######################################################################

positions_mois <- tibble(
  mois = 1:12,
  jour = yday(
    as.Date(
      paste0(
        "2024-",
        sprintf("%02d", 1:12),
        "-15"
      )
    )
  )
)

# Noms des mois en français
noms_mois <- c(
  "Janv.", "Févr.", "Mars", "Avr.",
  "Mai", "Juin", "Juil.", "Août",
  "Sept.", "Oct.", "Nov.", "Déc."
)

#######################################################################
## 6. Graphique
#######################################################################

graphique_intensite_peche <- ggplot() +
  
  # Lignes reliant les dates observées suffisamment proches
  geom_segment(
    data = segments_peche,
    aes(
      x = jour_annee,
      xend = jour_suivant,
      y = pourcentage_peche,
      yend = pourcentage_suivant,
      color = village
    ),
    linewidth = 0.8
  ) +
  
  # Points correspondant aux dates où des pêcheuses ont été interrogées
  geom_point(
    data = effectifs_journaliers,
    aes(
      x = jour_annee,
      y = pourcentage_peche,
      color = village
    ),
    size = 1.8
  ) +
  
  # Un panneau par année
  facet_wrap(
    ~ annee,
    ncol = 1
  ) +
  
  # Couleurs des villages
  scale_color_manual(
    values = c(
      "falia" = "#6A51A3",
      "niodior" = "#E76FAD"
    ),
    labels = c(
      "falia" = "Falia",
      "niodior" = "Niodior"
    ),
    name = "Village"
  ) +
  
  # Axe horizontal : mois
  scale_x_continuous(
    breaks = positions_mois$jour,
    labels = noms_mois,
    limits = c(1, 366),
    expand = c(0, 0)
  ) +
  
  # Axe vertical : pourcentage
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    limits = c(0, 100),
    labels = function(x) paste0(x, " %")
  ) +
  
  labs(
    x = "Mois de l'année",
    y = "Pêcheuses ayant pêché (%)",
    title = "Évolution annuelle de l'intensité de pêche",
    subtitle = "Proportion journalière de pêcheuses ayant pêché, par village",
    caption = paste(
      "Le pourcentage est calculé parmi les pêcheuses interrogées",
      "chaque jour.\n Les points sont reliés uniquement lorsque",
      "moins de 4 jours séparent les dates observées."
    )
  ) +
  
  theme_minimal() +
  
  theme(
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    axis.text.x = element_text(
      size = 10
    ),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

#######################################################################
## 7. Affichage du graphique
#######################################################################

print(graphique_intensite_peche)






























































#######################################################################
## La participation à la pêche diminue-t-elle au cours de l'année ?
## Analyse par village et par année
#######################################################################

library(tidyverse)
library(lubridate)

#######################################################################
## 1. Préparation des données individuelles
#######################################################################

donnees_analyse <- df %>%
  filter(
    !is.na(date_peche),
    peche %in% c(0, 1),
    !is.na(id_individu),
    !is.na(village)
  ) %>%
  mutate(
    date_peche = as.Date(date_peche),
    village = str_to_lower(village),
    annee = year(date_peche),
    jour_annee = yday(date_peche)
  ) %>%
  filter(
    village %in% c("falia", "niodior")
  )


#######################################################################
## 2. Calcul des effectifs journaliers
#######################################################################

effectifs_journaliers <- donnees_analyse %>%
  group_by(date_peche, annee, village) %>%
  summarise(
    nombre_interrogees = n_distinct(id_individu),
    
    nombre_pecheuses = n_distinct(
      id_individu[peche == 1]
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    pourcentage_peche = 100 *
      nombre_pecheuses / nombre_interrogees,
    
    # Jour de l'année : de 1 à 365 (ou 366)
    jour_annee = yday(date_peche)
  )

#######################################################################
## 3. Vérification
#######################################################################

names(effectifs_journaliers)

head(
  effectifs_journaliers %>%
    select(date_peche, annee, village,
           jour_annee, nombre_interrogees,
           nombre_pecheuses, pourcentage_peche)
)


#######################################################################
## 4. Modèle statistique binomial
#######################################################################

# On teste si la probabilité de pêcher diminue avec le jour de l'année.
#
# jour_annee est une variable continue.
# village permet de comparer Falia et Niodior.
# annee permet de tenir compte des différences entre années.
#
# L'interaction jour_annee:village teste si la tendance temporelle
# est différente entre les deux villages.

modele_peche <- glm(
  cbind(
    nombre_pecheuses,
    nombre_interrogees - nombre_pecheuses
  ) ~
    jour_annee * village +
    factor(annee),
  family = binomial,
  data = effectifs_journaliers
)

#######################################################################
## 5. Résultats du modèle
#######################################################################

summary(modele_peche)

#######################################################################
## 6. Vérification de la surdispersion
#######################################################################

# Une surdispersion peut apparaître lorsque les observations
# journalières sont plus variables que prévu par le modèle binomial.

dispersion <- sum(
  residuals(modele_peche, type = "pearson")^2
) / df.residual(modele_peche)

cat(
  "\nCoefficient de dispersion :",
  round(dispersion, 3),
  "\n"
)

#######################################################################
## 7. Modèle corrigé si surdispersion
#######################################################################

# Si la dispersion est nettement supérieure à 1,
# on utilise un modèle quasibinomial pour ajuster les erreurs standard.

if (dispersion > 1.5) {
  
  modele_final <- glm(
    cbind(
      nombre_pecheuses,
      nombre_interrogees - nombre_pecheuses
    ) ~
      jour_annee * village +
      factor(annee),
    family = quasibinomial,
    data = effectifs_journaliers
  )
  
  cat("\nModèle quasibinomial utilisé en raison de la surdispersion.\n")
  
} else {
  
  modele_final <- modele_peche
  
  cat("\nModèle binomial utilisé.\n")
}

#######################################################################
## 8. Résultats statistiques finaux
#######################################################################

resultats <- summary(modele_final)$coefficients

print(resultats)

#######################################################################
## 9. Interprétation de la tendance temporelle
#######################################################################

# Le coefficient jour_annee correspond à la tendance de référence
# (Falia, si Falia est le premier niveau du facteur village).
#
# Un coefficient négatif indique une diminution de la probabilité
# de pêcher au cours de l'année.
#
# L'interaction jour_annee:village indique si cette tendance
# diffère entre Falia et Niodior.

cat("\n\nInterprétation des coefficients :\n")
cat("- Coefficient temporel négatif : diminution de la participation.\n")
cat("- Coefficient temporel positif : augmentation de la participation.\n")
cat("- Interaction temporelle significative : tendances différentes entre villages.\n")

#######################################################################
## 10. Calcul des probabilités prédites
#######################################################################

# On construit une grille de dates pour visualiser la tendance estimée.

grille_prediction <- expand_grid(
  jour_annee = seq(
    min(effectifs_journaliers$jour_annee),
    max(effectifs_journaliers$jour_annee),
    length.out = 200
  ),
  village = c("falia", "niodior"),
  annee = sort(unique(effectifs_journaliers$annee))
) %>%
  mutate(
    village = factor(
      village,
      levels = levels(factor(effectifs_journaliers$village))
    ),
    annee = factor(annee)
  )

# Prédictions sur l'échelle des probabilités

predictions <- predict(
  modele_final,
  newdata = grille_prediction,
  type = "link",
  se.fit = TRUE
)

grille_prediction <- grille_prediction %>%
  mutate(
    prediction = plogis(predictions$fit) * 100,
    borne_inf = plogis(
      predictions$fit - 1.96 * predictions$se.fit
    ) * 100,
    borne_sup = plogis(
      predictions$fit + 1.96 * predictions$se.fit
    ) * 100
  )

#######################################################################
## 11. Graphique de la tendance annuelle
#######################################################################

# Conversion du jour de l'année en date fictive pour afficher les mois.

grille_prediction <- grille_prediction %>%
  mutate(
    date_fictive = as.Date("2024-01-01") +
      jour_annee - 1
  )

effectifs_journaliers <- effectifs_journaliers %>%
  mutate(
    date_fictive = as.Date("2024-01-01") +
      jour_annee - 1
  )

graphique_tendance <- ggplot() +
  
  # Données observées : pourcentage journalier
  geom_point(
    data = effectifs_journaliers,
    aes(
      x = date_fictive,
      y = pourcentage_peche,
      color = village
    ),
    alpha = 0.45,
    size = 1.5
  ) +
  
  # Intervalle de confiance de la tendance estimée
  geom_ribbon(
    data = grille_prediction,
    aes(
      x = date_fictive,
      ymin = borne_inf,
      ymax = borne_sup,
      fill = village
    ),
    alpha = 0.15,
    color = NA
  ) +
  
  # Tendance estimée par le modèle
  geom_line(
    data = grille_prediction,
    aes(
      x = date_fictive,
      y = prediction,
      color = village
    ),
    linewidth = 1
  ) +
  
  # Un panneau par année
  facet_wrap(~ annee) +
  
  scale_color_manual(
    values = c(
      "falia" = "#6A51A3",
      "niodior" = "#E76FAD"
    ),
    labels = c(
      "falia" = "Falia",
      "niodior" = "Niodior"
    ),
    name = "Village"
  ) +
  
  scale_fill_manual(
    values = c(
      "falia" = "#6A51A3",
      "niodior" = "#E76FAD"
    ),
    labels = c(
      "falia" = "Falia",
      "niodior" = "Niodior"
    ),
    name = "Village"
  ) +
  
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b",
    limits = as.Date(c("2024-01-01", "2024-12-31"))
  ) +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, " %")
  ) +
  
  labs(
    x = "Mois de l'année",
    y = "Pêcheuses ayant pêché (%)",
    title = "Évolution de la participation à la pêche",
    subtitle = "Points : données observées | Lignes : tendance estimée par le modèle",
    caption = "Les bandes représentent les intervalles de confiance à 95 %."
  ) +
  
  theme_minimal() +
  
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(graphique_tendance)












