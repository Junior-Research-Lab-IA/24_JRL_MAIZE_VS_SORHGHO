# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(stats)
library(ggpubr)

# Charger les données
data <- Pasta1_xlsx_Feuille_2

# Convertir les colonnes numériques
data$WUE <- as.numeric(gsub(",", ".", data$WUE))

# Vérifier les colonnes
print(colnames(data))

# Comparaisons sans tenir compte de la phase pour le maïs
maize_data <- filter(data, Species == "maize")

# Test pour Condition (ws vs ww) sans phase
t_test_maize_condition <- t.test(WUE ~ Condition, data = maize_data)
print(t_test_maize_condition)

# Visualisation pour Condition (ws vs ww) sans phase
ggplot(maize_data, aes(x = Condition, y = WUE, fill = Condition)) +
  geom_boxplot() +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  labs(title = "Maize: WUE Comparison by Condition (Without Phase)", x = "Condition", y = "WUE") +
  theme_minimal()

# ANOVA pour Phase 1
# data_phase1 <- filter(data, Phase == 1)
anova_phase1 <- aov(WUE ~ Species * Condition, data = data_phase1)
summary(anova_phase1)

ggplot(data_phase1, aes(x = interaction(Species, Condition), y = WUE, fill = interaction(Species, Condition))) +
  geom_boxplot() +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")) +
  stat_compare_means(aes(label = ..p.signif..), method = "anova", label.y = 1.1 * max(data_phase1$WUE, na.rm = TRUE)) +
  labs(title = " WUE by Species and Condition", x = "Species and Condition", y = "WUE") +
  theme_minimal()

# Visualisation supplémentaire pour Phase 1 avec Condition
ggplot(data_phase1, aes(x = Condition, y = WUE, fill = Condition)) +
  geom_boxplot() +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  labs(title = "Phase 1: WUE Comparison by Condition", x = "Condition", y = "WUE") +
  theme_minimal()

# # ANOVA pour Phase 2
# data_phase2 <- filter(data, Phase == 2)
# anova_phase2 <- aov(WUE ~ Species * Condition, data = data_phase2)
# summary(anova_phase2)
# 
# ggplot(data_phase2, aes(x = interaction(Species, Condition), y = WUE, fill = interaction(Species, Condition))) +
#   geom_boxplot() +
#   scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728")) +
#   stat_compare_means(aes(label = ..p.signif..), method = "anova", label.y = 1.1 * max(data_phase2$WUE, na.rm = TRUE)) +
#   labs(title = "Phase 2: WUE by Species and Condition", x = "Species and Condition", y = "WUE") +
#   theme_minimal()

# Visualisation supplémentaire pour Phase 2 avec Condition
ggplot(data_phase2, aes(x = Condition, y = WUE, fill = Condition)) +
  geom_boxplot() +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  labs(title = "Phase 2: WUE Comparison by Condition", x = "Condition", y = "WUE") +
  theme_minimal()

# Comparaisons deux à deux pour le maïs
# Test pour Phase 1 maïs
phase1_maize <- filter(maize_data, Phase == 1)
t_test_phase1_maize <- t.test(WUE ~ Condition, data = phase1_maize)
print(t_test_phase1_maize)

# Visualisation pour Phase 1 maïs
ggplot(phase1_maize, aes(x = Condition, y = WUE, fill = Condition)) +
  geom_boxplot() +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  labs(title = "Phase 1: Maize WUE Comparison by Condition", x = "Condition", y = "WUE") +
  theme_minimal()

# Test pour Phase 2 maïs
phase2_maize <- filter(maize_data, Phase == 2)
t_test_phase2_maize <- t.test(WUE ~ Condition, data = phase2_maize)
print(t_test_phase2_maize)

# Visualisation pour Phase 2 maïs
ggplot(phase2_maize, aes(x = Condition, y = WUE, fill = Condition)) +
  geom_boxplot() +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  labs(title = "Phase 2: Maize WUE Comparison by Condition", x = "Condition", y = "WUE") +
  theme_minimal()

# Comparaisons deux à deux pour le sorgho
sorgho_data <- filter(data, Species == "sorghum")

# Test pour Phase 1 sorgho
phase1_sorgho <- filter(sorgho_data, Phase == 1)
t_test_phase1_sorgho <- t.test(WUE ~ Condition, data = phase1_sorgho)
print(t_test_phase1_sorgho)

# Visualisation pour Phase 1 sorgho
ggplot(phase1_sorgho, aes(x = Condition, y = WUE, fill = Condition)) +
  geom_boxplot() +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  labs(title = "Phase 1: Sorghum WUE Comparison by Condition", x = "Condition", y = "WUE") +
  theme_minimal()

# # Test pour Phase 2 sorgho
# phase2_sorgho <- filter(sorgho_data, Phase == 2)
# t_test_phase2_sorgho <- t.test(WUE ~ Condition, data = phase2_sorgho)
# print(t_test_phase2_sorgho)
# 
# # Visualisation pour Phase 2 sorgho
# ggplot(phase2_sorgho, aes(x = Condition, y = WUE, fill = Condition)) +
#   geom_boxplot() +
#   stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
#   labs(title = "Phase 2: Sorghum WUE Comparison by Condition", x = "Condition", y = "WUE") +
#   theme_minimal()

# Comparaison des espèces et des conditions
species_condition_data <- filter(data, Condition %in% c("ww", "ws"))

# Test pour maïs vs sorgho (ww)
ww_data <- filter(species_condition_data, Condition == "ww")
t_test_species_ww <- t.test(WUE ~ Species, data = ww_data)
print(t_test_species_ww)

# Visualisation pour maïs vs sorgho (ww)
ggplot(ww_data, aes(x = Species, y = WUE, fill = Species)) +
  geom_boxplot() +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  labs(title = "WUE Comparison: Maize vs Sorghum (WW)", x = "Species", y = "WUE") +
  theme_minimal()

# Test pour maïs vs sorgho (ws)
ws_data <- filter(species_condition_data, Condition == "ws")
t_test_species_ws <- t.test(WUE ~ Species, data = ws_data)
print(t_test_species_ws)

# Visualisation pour maïs vs sorgho (ws)
ggplot(ws_data, aes(x = Species, y = WUE, fill = Species)) +
  geom_boxplot() +
  stat_compare_means(aes(label = ..p.signif..), method = "t.test") +
  labs(title = "WUE Comparison: Maize vs Sorghum (WS)", x = "Species", y = "WUE") +
  theme_minimal()

# Test complexe avec interactions (espèce, condition et phase)
interaction_model <- aov(WUE ~ Species * Condition * Phase, data = species_condition_data)
summary(interaction_model)

# Visualisation des interactions
ggplot(species_condition_data, aes(x = interaction(Species, Condition), y = WUE, fill = interaction(Species, Condition))) +
  geom_boxplot() +
  facet_wrap(~ Phase) +
  stat_compare_means(aes(label = ..p.signif..), method = "anova", label.y = 1.1 * max(species_condition_data$WUE, na.rm = TRUE)) +
  labs(title = "Interaction Effects: Species, Condition, and Phase", x = "Species and Condition", y = "WUE") +
  theme_minimal()
