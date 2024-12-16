# Load necessary libraries
library(ggplot2)
library(dplyr)
library(stats)
library(ggpubr)


# Convert relevant columns to numeric, replacing commas with dots
Sampling$R_S_Ratio <- as.numeric(gsub(",", ".", Sampling$R_S_Ratio))
Sampling$Phase <- as.factor(Sampling$Phase)

# 1. Compare WW and WS during Phase 1
phase1_data <- Sampling %>% filter(Phase == "1" & condition %in% c("ww", "ws"))
if (length(unique(phase1_data$condition)) == 2) {
  t_test_phase1 <- t.test(R_S_Ratio ~ condition, data = phase1_data, var.equal = TRUE)
  print(t_test_phase1)
} else {
  print("Error: Not enough data for WW and WS comparison in Phase 1.")
}

# 2. Compare WW and WS during Phase 2
phase2_data <- Sampling %>% filter(Phase == "2" & condition %in% c("ww", "ws"))
if (length(unique(phase2_data$condition)) == 2) {
  t_test_phase2 <- t.test(R_S_Ratio ~ condition, data = phase2_data, var.equal = TRUE)
  print(t_test_phase2)
} else {
  print("Error: Not enough data for WW and WS comparison in Phase 2.")
}

# 3. Compare Phase 1 and Phase 2 for WW and WS separately
# Compare WW in Phase 1 vs Phase 2
ww_data <- Sampling %>% filter(condition == "ww" & Phase %in% c("1", "2"))
if (length(unique(ww_data$Phase)) == 2) {
  t_test_ww <- t.test(R_S_Ratio ~ Phase, data = ww_data, var.equal = TRUE)
  print(t_test_ww)
} else {
  print("Error: Not enough data for WW comparison between Phase 1 and Phase 2.")
}

# Compare WS in Phase 1 vs Phase 2
ws_data <- Sampling %>% filter(condition == "ws" & Phase %in% c("1", "2"))
if (length(unique(ws_data$Phase)) == 2) {
  t_test_ws <- t.test(R_S_Ratio ~ Phase, data = ws_data, var.equal = TRUE)
  print(t_test_ws)
} else {
  print("Error: Not enough data for WS comparison between Phase 1 and Phase 2.")
}

# Interphase comparison for WW and WS
ggplot(Sampling, aes(x = interaction(condition, Phase), y = R_S_Ratio, fill = interaction(condition, Phase))) +
  geom_boxplot() +
  scale_fill_manual(values = c("ww.1" = "#8500bd", "ws.1" = "#ff7f0e", "ww.2" = "#17becf", "ws.2" = "#bcbd22")) +
  stat_compare_means(aes(group = interaction(condition, Phase)), method = "t.test", label = "p.signif",
                     comparisons = list(c("ww.1", "ww.2"), c("ws.1", "ws.2"))) +
  labs(title = "Root-Shoot Ratio Interphase Comparison",
       x = "Condition and Phase",
       y = "Root-Shoot Ratio") +
  theme_minimal()

# Root-Shoot Ratio Visualization for WW and WS within each phase
custom_colors <- c("ww" = "#8500bd", "ws" = "#ff7f0e")  # Define custom colors
ggplot(Sampling, aes(x = condition, y = R_S_Ratio, fill = condition)) +
  geom_boxplot() +
  facet_wrap(~ Phase, scales = "fixed") +  # Ensure plots have the same scale
  scale_fill_manual(values = custom_colors) +
  stat_compare_means(comparisons = list(c("ww", "ws")), method = "t.test", label = "p.signif",
                     label.y = 1.1 * max(Sampling$R_S_Ratio, na.rm = TRUE)) +  # Adjust annotation position
  labs(title = "Root-Shoot Ratio Comparison Between Conditions Across Phases",
       x = "Condition",
       y = "Root-Shoot Ratio") +
  theme_minimal()

# 4. Compare WW and WS across all phases
data_combined <- Sampling %>% filter(condition %in% c("ww", "ws"))
if (length(unique(data_combined$condition)) == 2) {
  t_test_combined <- t.test(R_S_Ratio ~ condition, data = data_combined, var.equal = TRUE)
  print(t_test_combined)
} else {
  print("Error: Not enough data for combined WW and WS comparison.")
}

# Plot for WW and WS across all phases
ggplot(data_combined, aes(x = condition, y = R_S_Ratio, fill = condition)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors) +
  stat_compare_means(comparisons = list(c("ww", "ws")), method = "t.test", label = "p.signif",
                     label.y = 1.1 * max(data_combined$R_S_Ratio, na.rm = TRUE)) +
  labs(title = "Root-Shoot Ratio Comparison Between WW and WS Across All Phases",
       x = "Condition",
       y = "Root-Shoot Ratio") +
  theme_minimal()


# Sorghum-specific tests (duplicated for Sorghum)
# 1. Compare WW and WS during Phase 1 (Sorghum)
phase1_data_sorghum <- Sampling %>% filter(Phase == "1" & condition %in% c("ww", "ws") & species == "sorghum")
phase1_data_sorghum <- phase1_data_sorghum %>% filter(!is.na(R_S_Ratio))
if (length(unique(phase1_data_sorghum$condition)) == 2) {
  t_test_phase1_sorghum <- t.test(R_S_Ratio ~ condition, data = phase1_data_sorghum, var.equal = TRUE)
  print(t_test_phase1_sorghum)
} else {
  print("Error: Not enough data for Sorghum WW and WS comparison in Phase 1.")
}

# # 2. Compare WW and WS during Phase 2 (Sorghum)
# phase2_data_sorghum <- Sampling %>% filter(Phase == "2" & condition %in% c("ww", "ws") & species == "sorghum")
# if (length(unique(phase2_data_sorghum$condition)) == 2) {
#   t_test_phase2_sorghum <- t.test(R_S_Ratio ~ condition, data = phase2_data_sorghum, var.equal = TRUE)
#   print(t_test_phase2_sorghum)
# } else {
#   print("Error: Not enough data for Sorghum WW and WS comparison in Phase 2.")
# }

# # 3. Compare Phase 1 and Phase 2 for WW and WS separately (Sorghum)
# ww_data_sorghum <- Sampling %>% filter(condition == "ww" & Phase %in% c("1", "2") & species == "sorghum")
# if (length(unique(ww_data_sorghum$Phase)) == 2) {
#   t_test_ww_sorghum <- t.test(R_S_Ratio ~ Phase, data = ww_data_sorghum, var.equal = TRUE)
#   print(t_test_ww_sorghum)
# } else {
#   print("Error: Not enough data for Sorghum WW comparison between Phase 1 and Phase 2.")
# }

# ws_data_sorghum <- Sampling %>% filter(condition == "ws" & Phase %in% c("1", "2") & species == "sorghum")
# if (length(unique(ws_data_sorghum$Phase)) == 2) {
#   t_test_ws_sorghum <- t.test(R_S_Ratio ~ Phase, data = ws_data_sorghum, var.equal = TRUE)
#   print(t_test_ws_sorghum)
# } else {
#   print("Error: Not enough data for Sorghum WS comparison between Phase 1 and Phase 2.")
# }

# ANOVA for species and conditions
data_anova <- Sampling %>% filter(condition %in% c("ww", "ws") & species %in% c("maize", "sorghum"))
anova_result <- aov(R_S_Ratio ~ species * condition, data = data_anova)
summary(anova_result)

# Visualization for ANOVA results
ggplot(data_anova, aes(x = interaction(species, condition), y = R_S_Ratio, fill = species)) +
  geom_boxplot() +
  labs(title = "Root-Shoot Ratio Comparison Between Species and Conditions",
       x = "Species and Condition",
       y = "Root-Shoot Ratio") +
  theme_minimal()

# Post-hoc Tukey test
posthoc_result <- TukeyHSD(anova_result)
print(posthoc_result)

# Visualisation améliorée avec significativité
ggplot(data_anova, aes(x = interaction(species, condition), y = R_S_Ratio, fill = species)) +
  geom_boxplot() +
  stat_compare_means(aes(group = interaction(species, condition)), method = "t.test", label = "p.signif") +
  labs(title = "Root-Shoot Ratio Comparison Between Species and Conditions",
       x = "Species and Condition",
       y = "Root-Shoot Ratio") +
  theme_minimal()

# Tukey's HSD Test
posthoc_result <- TukeyHSD(anova_result)

# Add annotations to the boxplot
ggplot(data_anova, aes(x = interaction(species, condition), y = R_S_Ratio, fill = species)) +
  geom_boxplot() +
  scale_fill_manual(values = c("maize" = "#1f77b4", "sorghum" = "#ff7f0e")) +
  stat_compare_means(method = "anova") + # Overall ANOVA result
  stat_compare_means(comparisons = list(
    c("maize:ws", "sorghum:ws"),
    c("maize:ww", "sorghum:ww"),
    c("maize:ww", "maize:ws"),
    c("sorghum:ww", "sorghum:ws")
  ), label = "p.signif") +
  labs(title = "Root-Shoot Ratio Comparison Between Species and Conditions",
       x = "Species and Condition",
       y = "Root-Shoot Ratio") +
  theme_minimal()
