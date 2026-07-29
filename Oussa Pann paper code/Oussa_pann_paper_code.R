#
#         CODE FOR THE FIGURES AND STATISTICAL ANALYSIS IN THE ARTICLE 
#                 ON GAME SESSIONS IN FALIA AND NIODIOR IN 2026
#
# M. REYNAUD
# 2026-07-16

################################################################################

# Initialisation
rm(list = ls())

library(tidyverse)
library(dplyr)
library(Epi)
library(readxl)

# Working directory containing the data (TO BE CHANGED)
setwd("/Mes Donnees/Analyses")

# Read database from the XLSX file (CHECK VERSION)
# All data is in the same Excel file, on different sheets
file <- "base_de_donnees_jeu_pann-concession_20260511.xlsx"

# fishing base
data <- read_excel(file, sheet="BDD_peche_&_SMA")

data <- data[
  data$id_partie %in% 3:46 &          # Remove session 1 and 2 (not the same game rules)
    data$id_partie != 4 &             # Remove session 4 (only 7 rounds)
    data$id_partie != 8 &             # Played with ressource managers
    data$id_partie != 3 &             # One player already played to the game
    data$id_partie != 11 &            # Only 4 players
    data$id_partie != 16,             # Played with ressource managers
]

data <- data[data$annee <= 8, ]       # Remove extra turns

head(data)

# players base
dataPlayers <- read_excel(file, sheet="BDD_joueurses")

dataPlayers <- dataPlayers[
  dataPlayers$id_partie %in% 3:46 &
    dataPlayers$id_partie != 3 &
    dataPlayers$id_partie != 4 &
    dataPlayers$id_partie != 8 &
    dataPlayers$id_partie != 11 &
    dataPlayers$id_partie != 16,           
]

head(dataPlayers)

# shellfish size base
taille <- read_excel(file, sheet="BDD_stock_SMA")

taille <- taille[
  taille$id_partie %in% 3:46 &
    taille$id_partie != 3 &
    taille$id_partie != 4 &
    taille$id_partie != 8 &
    taille$id_partie != 11 &
    taille$id_partie != 16,
]

taille <- taille[taille$annee <= 8, ]       # Remove extra turns

head(taille)

# shellfish stock base
stock <- read_excel(file, sheet="BDD_stock_SMA")

stock <- stock[
  stock$id_partie %in% 3:46 &
    stock$id_partie != 3 &
    stock$id_partie != 4 &
    stock$id_partie != 8 &
    stock$id_partie != 11 &
    stock$id_partie != 16,
]

stock <- stock[stock$annee <= 8, ]       # Remove extra turns

head(stock)



































################################################################################
##                               2.	Materials and methods
##                               2.4.	Data and analysis
##                                  Data description
################################################################################

################################################################################
## Number of sessions
nb_sessions <- dataPlayers %>%
  summarise(
    sessions = n_distinct(id_partie)
  )

nb_sessions










################################################################################
## Total number of player sessions
dataPlayers %>%
  summarise(total_joueuses = sum(nb_joueurses, na.rm = TRUE))










################################################################################
## Session duration
library(dplyr)

duree_sessions <- dataPlayers %>%
  distinct(id_partie, heure_debut, heure_fin) %>%
  mutate(
    duree_min = as.numeric(
      difftime(heure_fin, heure_debut, units = "mins")
    )
  )

summary(duree_sessions$duree_min)










################################################################################
## Number of observed game actions
library(dplyr)

nb_actions_sessions <- data %>%
  filter(id_partie <= 16) %>%
  nrow()

nb_actions_sessions










################################################################################
## Number of simulated game actions
nb_actions_simulations <- data %>%
  filter(id_partie >= 17) %>%
  nrow()

nb_actions_simulations

































################################################################################
##                                 3.	Results
##                      3.1.	Living with chronic crisis
################################################################################

################################################################################
## Crisis thresholds

dd_obs <- data %>%
  filter(village != "sma_aleatoire")

stock_obs <- stock %>%
  filter(village != "sma_aleatoire")

taille_obs <- taille %>%
  filter(village != "sma_aleatoire")

gains <- dd_obs %>%
  group_by(id_partie, village, annee) %>%
  summarise(
    gains = sum(quantite),
    .groups="drop"
  )

fishing <- dd_obs %>%
  filter(localisation != "village") %>%
  group_by(id_partie, village, annee) %>%
  summarise(
    pressure = n(),
    .groups="drop"
  )

village <- dd_obs %>%
  filter(localisation == "village") %>%
  group_by(id_partie, village, annee) %>%
  summarise(
    presence = n(),
    .groups="drop"
  )

stock2 <- stock_obs %>%
  group_by(id_partie, village, annee) %>%
  summarise(
    stock = sum(quantite),
    .groups="drop"
  )

size <- taille_obs %>%
  group_by(id_partie, village, annee) %>%
  summarise(
    size = weighted.mean(classe_taille,
                         quantite),
    .groups="drop"
  )

library(nlme)

fit_breakpoint <- function(data, response){
  
  AICs <- data.frame(
    breakpoint = 2:7,
    AIC = NA
  )
  
  mods <- vector("list",6)
  
  for(k in 2:7){
    
    data$hinge <- pmax(0, data$annee-k)
    
    f <- as.formula(
      paste(response,"~ annee + hinge")
    )
    
    mod <- lme(
      fixed = f,
      random = ~1|id_partie,
      data = data,
      method="ML"
    )
    
    mods[[k-1]] <- mod
    AICs$AIC[AICs$breakpoint==k] <- AIC(mod)
    
  }
  
  best <- which.min(AICs$AIC)
  
  list(
    table=AICs,
    model=mods[[best]],
    breakpoint=AICs$breakpoint[best]
  )
}

# Stock
bp.stock <- fit_breakpoint(stock2,"stock")

bp.stock$table
bp.stock$breakpoint
summary(bp.stock$model)

# Gain
bp.gains <- fit_breakpoint(gains, "gains")

bp.gains$table
bp.gains$breakpoint
summary(bp.gains$model)

# Fishing pressure
bp.pressure <- fit_breakpoint(fishing, "pressure")

bp.pressure$table
bp.pressure$breakpoint
summary(bp.pressure$model)

# Village presence
bp.village <- fit_breakpoint(village, "presence")

bp.village$table
bp.village$breakpoint
summary(bp.village$model)

# Size
bp.size <- fit_breakpoint(size, "size")

bp.size$table
bp.size$breakpoint
summary(bp.size$model)

# Summary table
breakpoints <- data.frame(
  Variable = c(
    "Stock",
    "Economic gains",
    "Shellfish size",
    "Fishing pressure",
    "Village presence"
  ),
  Breakpoint = c(
    bp.stock$breakpoint,
    bp.gains$breakpoint,
    bp.size$breakpoint,
    bp.pressure$breakpoint,
    bp.village$breakpoint
  )
)

breakpoints

rbind(
  cbind(Variable = "Stock", bp.stock$table),
  cbind(Variable = "Economic gains", bp.gains$table),
  cbind(Variable = "Shellfish size", bp.size$table),
  cbind(Variable = "Fishing pressure", bp.pressure$table),
  cbind(Variable = "Village presence", bp.village$table)
)












################################################################################
## Fig. 4. Emergence of a socio-ecological crisis

library(tidyverse)
library(readxl)
library(patchwork)

# Couleurs
cols_villages <- c(
  "Falia" = "#6A51A3",
  "Niodior" = "#E76FAD",
  "sma_aleatoire" = "#5FC2FC"
)

stock_total <- stock %>%
  group_by(village, id_partie, annee) %>%
  summarise(
    value = sum(quantite, na.rm=TRUE),
    .groups="drop"
  )

stock_summary <- stock_total %>%
  group_by(village, annee) %>%
  summarise(
    mean = mean(value),
    min = min(value),
    max = max(value),
    .groups="drop"
  )

taille_summary <- stock %>%
  group_by(village,id_partie,annee) %>%
  summarise(
    value = sum(classe_taille*quantite,na.rm=T) /
      sum(quantite,na.rm=T),
    .groups="drop"
  ) %>%
  group_by(village,annee) %>%
  summarise(
    mean = mean(value),
    min = min(value),
    max = max(value),
    .groups="drop"
  )

gains_summary <- data %>%
  group_by(village,id_partie,nom_personnage,annee) %>%
  summarise(
    value=sum(quantite,na.rm=T),
    .groups="drop"
  ) %>%
  group_by(village,annee) %>%
  summarise(
    mean=mean(value),
    min=min(value),
    max=max(value),
    .groups="drop"
  )

theme_game <- theme_minimal(base_size=14) +
  theme(
    legend.position="bottom",
    legend.title=element_blank(),
    panel.grid.minor=element_blank()
  )

plot_evolution <- function(df,title,ylabel,breakpoint){
  
  ggplot(df,
         aes(annee,mean,
             colour=village,
             fill=village)) +
    
    geom_vline(
      xintercept=breakpoint,
      colour="red3",
      linetype="dashed"
    ) +
    
    geom_ribbon(
      aes(ymin=min,ymax=max),
      alpha=.15,
      colour=NA
    ) +
    
    geom_line(linewidth=1.2)+
    geom_point(size=2.3)+
    
    scale_color_manual(
      values = cols_villages,
      labels = c(
        "Falia" = "Falia",
        "Niodior" = "Niodior",
        "sma_aleatoire" = "Random simulation"
      )
    )+
    scale_fill_manual(
      values = cols_villages,
      labels = c(
        "Falia" = "Falia",
        "Niodior" = "Niodior",
        "sma_aleatoire" = "Random simulation"
      )
    )+
    
    scale_x_continuous(breaks=1:8)+
    
    labs(
      title=title,
      x=NULL,
      y=ylabel
    )+
    theme_game
}

p1 <- plot_evolution(
  stock_summary,
  "A. Shellfish stock",
  "Mean stock",
  4
)

p2 <- plot_evolution(
  taille_summary,
  "B. Shellfish size",
  "Mean size",
  4
)

p3 <- plot_evolution(
  gains_summary,
  "C. Economic gains",
  "Mean gains",
  5
)

n_games <- stock %>%
  filter(village!="sma_aleatoire") %>%
  distinct(village,id_partie) %>%
  nrow()

n_simulations <- stock %>%
  filter(village=="sma_aleatoire") %>%
  distinct(id_partie) %>%
  nrow()

(p1 | p2 | p3) +
  plot_layout(guides="collect") &
  theme(
    legend.position="bottom"
  )

























################################################################################
##                                 3.	Results
##           3.2.	Shifting strategies as a response to resource scarcity
################################################################################

################################################################################
## Decrease in fishing activity (paired Wilcoxon signed-rank test)

dd2 <- data %>%
  filter(
    !id_partie %in% c(1, 2, 3, 4, 8, 11, 16)
  ) %>%
  mutate(
    fishing = localisation %in% c("jaune", "vert", "rouge"),
    period = ifelse(annee <= 4, "before", "after")
  )

sort(unique(dd2$id_partie))

dd2 %>%
  group_by(id_partie) %>%
  summarise(
    n_characters = n_distinct(nom_personnage),
    .groups = "drop"
  )

fishing_frequency <- dd2 %>%
  group_by(id_partie, nom_personnage, period) %>%
  summarise(
    n_rounds = n(),
    n_fishing = sum(fishing),
    prop_fishing = mean(fishing),
    .groups = "drop"
  )

fishing_frequency %>%
  dplyr::count(id_partie, nom_personnage, period)

fishing_frequency_wide <- fishing_frequency %>%
  select(id_partie, nom_personnage, period, prop_fishing) %>%
  pivot_wider(
    names_from = period,
    values_from = prop_fishing
  )

fishing_frequency_wide %>%
  group_by(id_partie) %>%
  summarise(
    n_characters = n(),
    .groups = "drop"
  )

wilcox.test(
  fishing_frequency_wide$before,
  fishing_frequency_wide$after,
  paired = TRUE
)

fishing_frequency_wide %>%
  summarise(
    mean_before = mean(before, na.rm = TRUE),
    mean_after = mean(after, na.rm = TRUE),
    difference = mean_after - mean_before
  )

ggplot(fishing_frequency,
       aes(x = period, y = prop_fishing)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  theme_classic() +
  labs(
    x = "",
    y = "Proportion of fishing trips"
  )










################################################################################
## Increase of village-based activities (binomial GLM)

library(dplyr)

village_activity <- data %>%
  filter(id_partie %in% 1:16) %>%
  mutate(
    village = localisation == "village"
  )

m <- glm(
  village ~ annee,
  family = binomial,
  data = village_activity
)

anova(m, test = "Chisq")
summary(m)










################################################################################
## Village use - differences between study sites

dd2 <- data %>%
  filter(village %in% c("Falia", "Niodior"))

prop_partie <- dd2 %>%
  group_by(village, id_partie, annee) %>%
  summarise(
    n_total = n(),
    n_village = sum(localisation == "village"),
    prop_village = n_village / n_total,
    .groups = "drop"
  )

post4 <- prop_partie %>%
  filter(annee >= 4)

result <- post4 %>%
  group_by(village) %>%
  summarise(
    mean_prop_village = mean(prop_village, na.rm = TRUE),
    sd_prop_village = sd(prop_village, na.rm = TRUE),
    n = n()
  )

result

t.test(prop_village ~ village, data = post4)











################################################################################
## Validation of qualitative profiles ? 

library(dplyr)
library(tidyr)
library(ggplot2)

freq_pecheuse <- data %>%
  filter(id_partie <= 16) %>%
  distinct(id_partie, village, nom_personnage) %>%
  left_join(
    data %>%
      filter(id_partie <= 16,
             localisation %in% c("jaune", "vert", "rouge")) %>%
      dplyr::count(id_partie, nom_personnage, name = "nb_sorties"),
    by = c("id_partie", "nom_personnage")
  ) %>%
  mutate(nb_sorties = replace_na(nb_sorties, 0))

summary(freq_pecheuse$nb_sorties)

ggplot(freq_pecheuse, aes(nb_sorties)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  theme_classic()

# Test 3 profiles ?
set.seed(123)

km3 <- kmeans(freq_pecheuse["nb_sorties"], centers = 3)

freq_pecheuse$profil_kmeans <- factor(km3$cluster)

sort(km3$centers[,1])

ggplot(freq_pecheuse,
       aes(profil_kmeans, nb_sorties, fill = profil_kmeans)) +
  geom_boxplot() +
  theme_classic()

# Test x profiles
library(mclust)

mc <- Mclust(freq_pecheuse$nb_sorties)

summary(mc)
plot(mc, what = "BIC")

freq_pecheuse$profil_mclust <- factor(mc$classification)

freq_pecheuse %>%
  group_by(profil_mclust) %>%
  summarise(
    n = n(),
    moyenne = mean(nb_sorties),
    min = min(nb_sorties),
    max = max(nb_sorties)
  )

## Comparison : 3 vs x profils

mc_auto <- Mclust(freq_pecheuse$nb_sorties)
mc3 <- Mclust(freq_pecheuse$nb_sorties, G = 3)

mc_auto$bic
mc3$bic

# Test with a Poisson mixture model
library(flexmix)

mods <- lapply(1:5, function(k)
  flexmix(nb_sorties ~ 1,
          data = freq_pecheuse,
          k = k,
          model = FLXMRglm(family = "poisson"))
)

BICs <- sapply(mods, BIC)

data.frame(k = 1:5, BIC = BICs)

plot(1:5, BICs, type = "b", pch = 19)










################################################################################
## Fig. 5. Heterogeneity in fishing engagement among participants

library(dplyr)
library(ggplot2)
library(patchwork)

cols_profils <- c(
  "Low fishing effort" = "#D9D9D9",
  "Intermediate fishing effort" = "#8C8C8C",
  "High fishing effort" = "#252525"
)

freq_pecheuse <- data %>%
  filter(id_partie <= 16) %>%
  distinct(id_partie, village, nom_personnage) %>%
  left_join(
    data %>%
      filter(id_partie <= 16,
             localisation %in% c("jaune", "vert", "rouge")) %>%
      dplyr::count(id_partie, nom_personnage, name = "nb_sorties"),
    by = c("id_partie", "nom_personnage")
  ) %>%
  mutate(
    nb_sorties = replace_na(nb_sorties, 0),
    profil = factor(
      case_when(
        nb_sorties <= 2 ~ "Low fishing effort",
        nb_sorties <= 5 ~ "Intermediate fishing effort",
        TRUE ~ "High fishing effort"
      ),
      levels = c(
        "Low fishing effort",
        "Intermediate fishing effort",
        "High fishing effort"
      )
    )
  )

prop_profils <- freq_pecheuse %>%
  dplyr::count(village, profil) %>%
  dplyr::group_by(village) %>%
  dplyr::mutate(prop = n / sum(n)) %>%
  dplyr::ungroup()

(
  ggplot(freq_pecheuse, aes(nb_sorties, fill = profil)) +
    geom_histogram(binwidth = 1, color = "white") +
    scale_fill_manual(values = cols_profils) +
    scale_x_continuous(breaks = 0:8) +
    labs(
      title = "A. Distribution of fishing effort",
      x = "Number of fishing trips",
      y = "Number of fishers",
      fill = NULL
    ) +
    theme_classic() +
    theme(plot.margin = margin(r = 20))
) +
(
  ggplot(prop_profils, aes(village, prop, fill = profil)) +
    geom_col(color = "white", width = 0.4) +
    scale_fill_manual(values = cols_profils) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(
      title = "B. Behavioural profiles by village",
      x = "Village",
      y = "Proportion of fishers",
      fill = NULL
    ) +
    theme_classic() +
    theme(plot.margin = margin(l = 20))
) +
plot_layout(guides = "collect") &
theme(
  legend.position = "right",
  plot.title = element_text(face = "bold")
)












################################################################################
## Fig. 6. Temporal dynamics of spatial allocation across game rounds

library(dplyr)
library(ggplot2)
library(patchwork)

cols <- c(
  "village" = "#B26E6E",
  "rouge"   = "#C00000",
  "vert"    = "#70AD47",
  "jaune"   = "#FFC000"
)

dd2 <- data %>%
  filter(village %in% c("Falia", "Niodior")) %>%
  mutate(
    localisation = factor(
      localisation,
      levels = c("village", "jaune", "vert", "rouge")
    )
  )

get_prop <- function(data, by_village = TRUE){
  
  if(by_village){
    data %>%
      dplyr::count(village, annee, localisation) %>%
      group_by(village, annee) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup()
    
  } else {
    data %>%
      dplyr::count(annee, localisation) %>%
      group_by(annee) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup()
  }
}
prop_data <- get_prop(dd2, TRUE)

prop_global <- get_prop(dd2, FALSE)

plot_stack <- function(data, title_text){
  
  data <- data %>%
    mutate(
      localisation = factor(
        localisation,
        levels = c("village","jaune","vert","rouge")
      )
    ) %>%
    arrange(annee, localisation) %>%
    group_by(annee) %>%
    mutate(
      ymax = cumsum(prop),
      ymin = ymax - prop
    ) %>%
    ungroup()
  
  
  ggplot(
    data,
    aes(
      x = annee,
      ymin = ymin,
      ymax = ymax,
      fill = localisation
    )
  ) +
    geom_ribbon(
      colour = "white",
      linewidth = 0.3
    ) +
    
    scale_fill_manual(
      values = cols,
      labels = c(
        rouge = "Red zone",
        vert = "Green zone",
        jaune = "Yellow zone",
        village = "Village"
      )
    ) +
    
    scale_x_continuous(breaks = 1:8) +
    
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0,1)
    ) +
    
    labs(
      title = title_text,
      x = "Game round",
      y = "Choice proportion",
      fill = NULL
    ) +
    
    theme_bw(base_size = 13) +
    theme(
      plot.title = element_text(face="bold"),
      legend.position = "bottom"
    )
}

g_global <- plot_stack(prop_global, "All sessions")

g_falia <- prop_data %>%
  filter(village == "Falia") %>%
  plot_stack("Falia")

g_niodior <- prop_data %>%
  filter(village == "Niodior") %>%
  plot_stack("Niodior")


(g_global + g_falia + g_niodior) +
  plot_layout(
    ncol = 3,
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom"
  )










################################################################################
## Impact of previous gains on decisions (GLMM, likelihood-ratio test)

library(dplyr)
library(lme4)

learn <- data %>%
  arrange(id_partie, nom_personnage, annee) %>%
  group_by(id_partie, nom_personnage) %>%
  mutate(
    succes_precedent = lag(quantite),
    retour_meme_zone = localisation == lag(localisation)
  ) %>%
  ungroup() %>%
  filter(!is.na(succes_precedent))

m <- glmer(
  retour_meme_zone ~ succes_precedent + (1 | id_partie),
  family = binomial,
  data = learn
)

summary(m)
drop1(m, test = "Chisq")






































################################################################################
##                                 3.	Results
##  3.3.	Situated knowledge and real-world representations for decision-making
################################################################################

################################################################################
## Percentage choosing fishing during the first round

library(dplyr)

data %>%
  filter(id_partie <= 16,
         annee == 1) %>%
  summarise(
    pourcentage_peche = mean(localisation %in% c("jaune", "vert", "rouge")) * 100
  )










################################################################################
## Consistency of spatial choices with an efficiency-driven allocation (chi-squared test)

prod <- c(
  jaune = 2,
  vert = 4,
  rouge = 6
)

prod_prop <- prod / sum(prod)
prod_prop

dd2 <- data %>%
  filter(village %in% c("Falia", "Niodior"),
         localisation %in% c("jaune", "vert", "rouge"))

obs <- dd2 %>%
  group_by(localisation) %>%
  summarise(n = n()) %>%
  mutate(prop_obs = n / sum(n))

obs

compare <- obs %>%
  mutate(
    prop_theorique = prod_prop[localisation],
    ratio = prop_obs / prop_theorique
  )

compare

chisq.test(
  x = obs$n,
  p = prod_prop
)




























################################################################################
##                                 3.	Results
##                  3.4.	Emerging informal coordination
################################################################################

################################################################################
## Spatial concentration
library(dplyr)

tab <- data %>%
  filter(id_partie <= 16) %>%
  group_by(id_partie, annee, localisation) %>%
  summarise(n = n(), .groups = "drop")

tab %>%
  filter(n >= 4) %>%
  arrange(desc(n))

concentration <- tab %>%
  group_by(id_partie, annee) %>%
  summarise(max_joueuses = max(n), .groups = "drop")

sum(concentration$max_joueuses == 5)

sum(concentration$max_joueuses >= 4)

table(concentration$max_joueuses)

