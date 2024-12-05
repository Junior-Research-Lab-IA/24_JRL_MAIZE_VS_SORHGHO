library(ggplot2)
library(dplyr)
library(nlme)
library(lme4)
library(robustbase)
library(lmerTest)
library(car)

# Importation des données
data <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/datauntil0212.csv", dec=",")
data <- data %>% select(-lenght,-width, -coef)
data$date <- as.Date(data$date, format = "%d/%m")

# Ajout du temps thermique
thermaltime <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/thermaltimeuntil0212.csv", dec=",")
thermaltime$date <- as.Date(thermaltime$date, format = "%d/%m")
data <- merge(thermaltime, data, by = "date")

# Ajout de l'humidité
humidity <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/humidityuntil0212.csv", dec=",")
humidity$date <- as.Date(humidity$date, format = "%d/%m")
humidity <- humidity %>% select(-species)
data <- merge(humidity, data, by = c("date", "rhizotron"))

# Calcul de l'aire totale
data <- data %>%
  group_by(species, rhizotron, plant, condition, date, humidity, thermal.time) %>%
  summarize(tot_area = sum(area), .groups = "drop")

# Calcul de la croissance par jour
data<-as.data.frame(data)
data <- data %>%
  group_by(condition, plant) %>%
  arrange(date) %>%
  mutate(TimeDiff = as.numeric(difftime(date, lag(date), units = "days")),
         AreaDiff = tot_area - lag(tot_area),
         GrowthRate = AreaDiff / TimeDiff)
data <- data %>% filter(!is.na(AreaDiff))

# Calcul de la croissance cumulée
data <- data %>%
  group_by(condition, plant) %>%
  arrange(date) %>%
  mutate(AreaCum = cumsum(AreaDiff))



#### Maize

# Dataset pour la phase 1 chez le maïs
data_maize <- data %>% filter (species=="maize")
data_m1 <- data_maize %>% filter (date %in% c("2024-10-28","2024-10-30","2024-11-01","2024-11-04","2024-11-06","2024-11-08","2024-11-11","2024-11-13","2024-11-15"))

# Intégration de l'humidité moyenne des rhizotrons WS
humidity_ws <- data_m1 %>%
  filter(condition == "ws",humidity != 0) %>%
  group_by(thermal.time) %>%
  summarise(humidity_ws = mean(humidity, na.rm = TRUE))
data_m1 <- data_m1 %>%
  left_join(humidity_ws, by = "thermal.time")

## Visualisation de l'évolution de l'aire totale

# données brutes
ggplot(data_m1, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*1000), size = 1, color = "blue") +
  labs(
    title = "Total average maize leaves area and humidity evolution",
    x = "Thermal time (degree days)",
    y = "Total average area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total area (cm²)", sec.axis = sec_axis(~ . / 1000, name = "WS average humidity")
  )

# données ajustées a un modèle linéaire
ggplot(data_m1, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*1000), size = 1, color = "blue") +
  geom_smooth(aes(group = condition), method = "lm", se=TRUE) +
  labs(
    title = "Adjusted total average maize leaves area and humidity evolution during phase 1",
    x = "Thermal time (degree days)",
    y = "Total average area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total area (cm²)", sec.axis = sec_axis(~ . / 1000, name = "WS average humidity")
  )

# données smooth
ggplot(data_m1, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*1000), size = 1, color = "blue") +
  geom_smooth(aes(group = condition), se=TRUE) +
  labs(
    title = "Adjusted total average maize leaves area and humidity evolution during phase 1",
    x = "Thermal time (degree days)",
    y = "Total average area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total area (cm²)", sec.axis = sec_axis(~ . / 1000, name = "WS average humidity")
  )

## Statistical tests on conditions

ggplot(data_m1, aes(x = thermal.time, y = tot_area, color = condition)) +
  geom_point() +
  geom_line(stat = "summary", fun = "mean", size = 1) +  # Moyenne par condition
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +  # Tendance lissée
  labs(
    title = "Comparaison de l'évolution des aires entre WW et WS",
    x = "Temps (jours ou thermal time)",
    y = "Aire totale (cm²)"
  ) +
  theme_minimal()

# Comparaison des régressions linéaires de l'aire

slopes_m1_area <- data_m1 %>%
  group_by(plant, rhizotron, condition) %>%
  summarise(slope_a = coef(lm(tot_area ~ date))[2]) 
shapiro.test(slopes_m1_area$slope_a[slopes_m1_area$condition == "ww"]) #ok
shapiro.test(slopes_m1_area$slope_a[slopes_m1_area$condition == "ws"]) #ok
ggplot(slopes_m1_area, aes(x = slope_a, fill = condition)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution des pentes par condition") +
  theme_minimal()
leveneTest(slope_a ~ condition, data = slopes_m1_area) #ok
t_test <- t.test(slope_a ~ condition, data = slopes_m1_area, var.equal = TRUE)
print(t_test)
#*significatif*#

ggplot(data_m1, aes(x = date, y = tot_area, color = condition)) +
  geom_line(aes(group = rhizotron)) +
  facet_wrap(~ condition) +
  labs(title = "Évolution de l'aire totale par condition", 
       x = "Temps (jours)", 
       y = "Aire totale moyenne") +
  theme_minimal()

# Comparaison du taux de croissance

ggplot(data_m1, aes(x = tot_area, y = GrowthRate, color = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Growth Rate vs. Area",
    x = "Area (cm²)",
    y = "Growth Rate (cm²/day)"
  ) +
  theme_minimal()

# le 06/11 (humidité maize = 1.1)
data_m1.1 <- data_m1 %>%
  filter(date == as.Date("2024-11-06")) # Replace with the specific date
shapiro.test(data_m1.1$GrowthRate[data_m1.1$condition == "ww"]) #ok
shapiro.test(data_m1.1$GrowthRate[data_m1.1$condition == "ws"]) #ok
leveneTest(GrowthRate ~ condition, data = data_m1.1) #ok
t.test(GrowthRate ~ condition, data = data_m1.1) #pas significatif
ggplot(data_m1.1, aes(x = condition, y = GrowthRate, fill = condition)) +
  geom_boxplot() +
  labs(title = "Growth Rate by Condition", x = "Condition", y = "Growth Rate") +
  theme_minimal()
anova_result <- aov(GrowthRate ~ condition, data = data_m1.1)
summary(anova_result) #pas significatif

# le 11/11 (humidité maize = 0.9)
data_m0.9 <- data_m1 %>%
  filter(date == as.Date("2024-11-11")) # Replace with the specific date
shapiro.test(data_m0.9$GrowthRate[data_m0.9$condition == "ww"]) #ok
shapiro.test(data_m0.9$GrowthRate[data_m0.9$condition == "ws"]) #pas ok
leveneTest(GrowthRate ~ condition, data = data_m0.9) #ok
wilcox.test(GrowthRate ~ condition, data = data_m0.9) #significatif!

# le 13/11 (humidité maize = 0.82)
data_m0.8 <- data_m1 %>%
  filter(date == as.Date("2024-11-13")) # Replace with the specific date
shapiro.test(data_m0.8$GrowthRate[data_m0.8$condition == "ww"]) #ok
shapiro.test(data_m0.8$GrowthRate[data_m0.8$condition == "ws"]) #pas ok
leveneTest(GrowthRate ~ condition, data = data_m0.8) #pas trop ok
wilcox.test(GrowthRate ~ condition, data = data_m0.8) #significatif!


# Comparaison croissance relative

data_m1 <- data_m1 %>%
  group_by(condition, plant) %>%
  arrange(date) %>%
  mutate(
    RelativeGrowth = GrowthRate/tot_area
  ) %>%
  ungroup()

ggplot(data_m1, aes(x = tot_area, y = RelativeGrowth, color = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Relative Growth vs. Area",
    x = "Area (cm²)",
    y = "Relative Growth (cm²/day)"
  ) +
  theme_minimal()

# le 06/11 (humidité maize = 1.1)
data_m1.1 <- data_m1 %>%
  filter(date == as.Date("2024-11-06")) # Replace with the specific date
shapiro.test(data_m1.1$RelativeGrowth[data_m1.1$condition == "ww"]) #ok
shapiro.test(data_m1.1$RelativeGrowth[data_m1.1$condition == "ws"]) # pas ok
leveneTest(RelativeGrowth ~ condition, data = data_m1.1) #ok
wilcox.test(RelativeGrowth ~ condition, data = data_m1.1) #pas significatif
ggplot(data_m1.1, aes(x = condition, y = RelativeGrowth, fill = condition)) +
  geom_boxplot() +
  labs(title = "RelativeGrowth by Condition", x = "Condition", y = "RelativeGrowth") +
  theme_minimal()
#(anova_result <- aov(RelativeGrowth ~ condition, data = data_m1.1)
summary(anova_result) #pas significatif)#

# le 11/11 (humidité maize = 0.9)
data_m0.9 <- data_m1 %>%
  filter(date == as.Date("2024-11-11")) # Replace with the specific date
shapiro.test(data_m0.9$RelativeGrowth[data_m0.9$condition == "ww"]) #ok
shapiro.test(data_m0.9$RelativeGrowth[data_m0.9$condition == "ws"]) #pas ok
leveneTest(RelativeGrowth ~ condition, data = data_m0.9) #ok
wilcox.test(RelativeGrowth ~ condition, data = data_m0.9) #significatif!

# le 13/11 (humidité maize = 0.82)
data_m0.8 <- data_m1 %>%
  filter(date == as.Date("2024-11-13")) # Replace with the specific date
shapiro.test(data_m0.8$RelativeGrowth[data_m0.8$condition == "ww"]) #ok
shapiro.test(data_m0.8$RelativeGrowth[data_m0.8$condition == "ws"]) #ok
leveneTest(RelativeGrowth ~ condition, data = data_m0.8) #ok
t.test(RelativeGrowth ~ condition, data = data_m0.8) #significatif!



#### Sorghum

# Dataset pour la phase 1 chez le maïs
data_sorg <- data %>% filter (species=="sorghum")
data_s1 <- data_sorg %>% filter (date %in% c("2024-10-28","2024-10-30","2024-11-01","2024-11-04","2024-11-06","2024-11-08","2024-11-11","2024-11-13","2024-11-15", "2024-11-18","2024-11-20","2024-11-22","2024-11-25", "2024-11-27", "2024-12-02"))

humidity_ws_s1 <- data_s1 %>%
  filter(condition == "ws",humidity != 0) %>%
  group_by(thermal.time) %>%
  summarise(humidity_ws = mean(humidity, na.rm = TRUE))

data_s1 <- data_s1 %>%
  left_join(humidity_ws_s1, by = "thermal.time")

## Visualisation of area evolution

ggplot(data_s1, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*1000), size = 1, color = "blue") +
  labs(
    title = "Total average sorghum leaves area and humidity evolution",
    x = "Thermal time (degree days)",
    y = "Total average area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total area (cm²)", sec.axis = sec_axis(~ . / 1000, name = "WS average humidity")
  )

ggplot(data_s1, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*1000), size = 1, color = "blue") +
  geom_smooth(aes(group = condition), method = "lm", se=TRUE) +
  labs(
    title = "Adjusted total average maize leaves area and humidity evolution during phase 1",
    x = "Thermal time (degree days)",
    y = "Total average area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total area (cm²)", sec.axis = sec_axis(~ . / 1000, name = "WS average humidity")
  )

ggplot(data_s1, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*1000), size = 1, color = "blue") +
  geom_smooth(aes(group = condition), se=TRUE) +
  labs(
    title = "Adjusted total average sorghum leaves area and humidity evolution during phase 1",
    x = "Thermal time (degree days)",
    y = "Total average area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total area (cm²)", sec.axis = sec_axis(~ . / 1000, name = "WS average humidity")
  )

## Tests statistiques sur les conditions

ggplot(data_s1, aes(x = thermal.time, y = tot_area, color = condition)) +
  geom_point() +
  geom_line(stat = "summary", fun = "mean", size = 1) +  # Moyenne par condition
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +  # Tendance lissée
  labs(
    title = "Comparaison de l'évolution des aires entre WW et WS",
    x = "Temps (jours ou thermal time)",
    y = "Aire totale (cm²)"
  ) +
  theme_minimal()

# Comparaison du taux de croissance

ggplot(data_s1, aes(x = tot_area, y = GrowthRate, color = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Growth Rate vs. Area",
    x = "Area (cm²)",
    y = "Growth Rate (cm²/day)"
  ) +
  theme_minimal()

# le 11/11 (humidité sorg = 1.1)
data_s1.1 <- data_s1 %>%
  filter(date == as.Date("2024-11-11")) # Replace with the specific date
shapiro.test(data_s1.1$GrowthRate[data_s1.1$condition == "ww"]) #ok
shapiro.test(data_s1.1$GrowthRate[data_s1.1$condition == "ws"]) #ok
leveneTest(GrowthRate ~ condition, data = data_s1.1) #ok
t.test(GrowthRate ~ condition, data = data_s1.1) #pas significatif
ggplot(data_s1.1, aes(x = condition, y = GrowthRate, fill = condition)) +
  geom_boxplot() +
  labs(title = "Growth Rate by Condition", x = "Condition", y = "Growth Rate") +
  theme_minimal()
anova_result <- aov(GrowthRate ~ condition, data = data_s1.1)
summary(anova_result) #pas significatif

# le 25/11 (humidité sorg = 0.9)
data_s0.9 <- data_s1 %>%
  filter(date == as.Date("2024-11-25")) # Replace with the specific date
shapiro.test(data_s0.9$GrowthRate[data_s0.9$condition == "ww"]) #ok
shapiro.test(data_s0.9$GrowthRate[data_s0.9$condition == "ws"]) #ok
leveneTest(GrowthRate ~ condition, data = data_s0.9) #ok
t.test(GrowthRate ~ condition, data = data_s0.9) #pas significatif
ggplot(data_s0.9, aes(x = condition, y = GrowthRate, fill = condition)) +
  geom_boxplot() +
  labs(title = "Growth Rate by Condition", x = "Condition", y = "Growth Rate") +
  theme_minimal()
anova_result <- aov(GrowthRate ~ condition, data = data_s0.9)
summary(anova_result) #pas significatif

# le 27/11 (humidité sorg = 0.85)
data_s0.8 <- data_s1 %>%
  filter(date == as.Date("2024-11-27")) # Replace with the specific date
shapiro.test(data_s0.8$GrowthRate[data_s0.8$condition == "ww"]) #ok
shapiro.test(data_s0.8$GrowthRate[data_s0.8$condition == "ws"]) #ok
leveneTest(GrowthRate ~ condition, data = data_s0.8) #ok
t.test(GrowthRate ~ condition, data = data_s0.8) #significatif


# relative area growth rate comparison

data_s1<- data_s1 %>%
  group_by(condition, plant) %>%
  arrange(date) %>%
  mutate(
    RelativeGrowth = GrowthRate/tot_area
  ) %>%
  ungroup()

ggplot(data_s1, aes(x = tot_area, y = RelativeGrowth, color = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Relative growth vs. Area",
    x = "Area (cm²)",
    y = "Relative growth"
  ) +
  theme_minimal()

# le 11/11 (humidité sorg = 1.1)
data_s1.1 <- data_s1 %>%
  filter(date == as.Date("2024-11-11")) # Replace with the specific date
shapiro.test(data_s1.1$RelativeGrowth[data_s1.1$condition == "ww"]) #ok
shapiro.test(data_s1.1$RelativeGrowth[data_s1.1$condition == "ws"]) #ok
leveneTest(RelativeGrowth ~ condition, data = data_s1.1) #ok
t.test(RelativeGrowth ~ condition, data = data_s1.1) #pas significatif
ggplot(data_s1.1, aes(x = condition, y = RelativeGrowth, fill = condition)) +
  geom_boxplot() +
  labs(title = "RelativeGrowth by Condition", x = "Condition", y = "RelativeGrowth") +
  theme_minimal()
anova_result <- aov(RelativeGrowth ~ condition, data = data_s1.1)
summary(anova_result) #pas significatif mais presque

# le 25/11 (humidité sorg = 0.9)
data_s0.9 <- data_s1 %>%
  filter(date == as.Date("2024-11-25")) # Replace with the specific date
shapiro.test(data_s0.9$RelativeGrowth[data_s0.9$condition == "ww"]) #ok
shapiro.test(data_s0.9$RelativeGrowth[data_s0.9$condition == "ws"]) #ok
leveneTest(RelativeGrowth ~ condition, data = data_s0.9) #ok
t.test(RelativeGrowth ~ condition, data = data_s0.9) #pas significatif
ggplot(data_s0.9, aes(x = condition, y = RelativeGrowth, fill = condition)) +
  geom_boxplot() +
  labs(title = "RelativeGrowth by Condition", x = "Condition", y = "RelativeGrowth") +
  theme_minimal()
anova_result <- aov(RelativeGrowth ~ condition, data = data_s0.9)
summary(anova_result) #pas significatif

# le 27/11 (humidité sorg = 0.85)
data_s0.8 <- data_s1 %>%
  filter(date == as.Date("2024-11-27")) # Replace with the specific date
shapiro.test(data_s0.8$RelativeGrowth[data_s0.8$condition == "ww"]) #pas ok
shapiro.test(data_s0.8$RelativeGrowth[data_s0.8$condition == "ws"]) #ok
ggplot(data_s0.8, aes(x = RelativeGrowth, fill = condition)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution des pentes par condition") +
  theme_minimal()
leveneTest(RelativeGrowth ~ condition, data = data_s0.8) #ok
wilcox.test(RelativeGrowth ~ condition, data = data_s0.8) #significatif!



#### Comparison of maize and sorghum

## Visualisation

data_phase1 <- bind_rows(data_m1, data_s1)

ggplot(data = data_phase1) +
  # Courbes des moyennes d'aire totale
  geom_smooth(aes(x = date, y = tot_area, 
                  color = species, linetype = condition), 
              size = 1.2) +
  # Courbes des humidités WS (humidité multipliée pour ajuster l'échelle)
  geom_line(aes(x = date, y = humidity_ws * 1000, 
                color = species), size = 1, linetype = "dotted") +
  # Personnalisation des axes et des labels
  labs(
    title = "Évolution de l'aire totale des plantes et de l'humidité des rhizotrons",
    x = "Temps (jours)",
    y = "Total area (cm²)",
    color = "Espèce",
    linetype = "Condition"
  ) +
  # Ajouter un axe secondaire pour l'humidité
  scale_y_continuous(
    name = "Total area (cm²)", 
    sec.axis = sec_axis(~ . / 1000, name = "WS average humidity (%)")
  ) +
  # Thème minimal avec personnalisation de l'apparence
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right"
  )

ggplot(data = data_phase1) +
  # Courbes des moyennes d'aire totale
  geom_smooth(aes(x = humidity_ws, y = tot_area, 
                  color = species, linetype = condition), 
              size = 1.2) +
  # Courbes des humidités WS (humidité multipliée pour ajuster l'échelle)
  geom_line(aes(x = humidity_ws, y = humidity_ws * 1000, 
                color = species), size = 1, linetype = "dotted") +
  # Personnalisation des axes et des labels
  labs(
    title = "Évolution de l'aire totale des plantes et de l'humidité des rhizotrons",
    x = "Temps (jours)",
    y = "Total area (cm²)",
    color = "Espèce",
    linetype = "Condition"
  ) +
  # Ajouter un axe secondaire pour l'humidité
  scale_y_continuous(
    name = "Total area (cm²)", 
    sec.axis = sec_axis(~ . / 1000, name = "WS average humidity (%)")
  ) +
  # Thème minimal avec personnalisation de l'apparence
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right"
  )

## Tests statistiques

# Comparaison de l'aire totale

ggplot(data_phase1, aes(x = condition, y = tot_area, color = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Répartition de l'aire totale par condition et espèce", y = "Total area (cm²)", x = "Condition")

# pour humidité = 1.1
data_1.1 <- data_phase1[data_phase1$humidity_ws >= 1.08 & data_phase1$humidity_ws <= 1.12, ]
data_1.1 <- data_1.1 %>% filter(!is.na(tot_area))
anova1.1 <- aov(tot_area ~ species * condition, data=data_1.1)
summary(anova1.1) #effet espèce
shapiro.test(residuals(anova1.1)) #ok

# pour humidité = 0.9
data_0.9 <- data_phase1[data_phase1$humidity_ws >= 0.89 & data_phase1$humidity_ws <= 0.91, ]
data_0.9 <- data_0.9 %>% filter(!is.na(tot_area))
anova0.9 <- aov(tot_area ~ species * condition, data=data_0.9)
summary(anova0.9) #effet espèce et condition
shapiro.test(residuals(anova0.9)) #ok

# pour humidité = 0.8
data_0.8 <- data_phase1[data_phase1$humidity_ws >= 0.76 & data_phase1$humidity_ws <= 0.86, ]
data_0.8 <- data_0.8 %>% filter(!is.na(tot_area))
anova0.8 <- aov(tot_area ~ species * condition, data=data_0.8)
summary(anova0.8) #effet espèce et condition
shapiro.test(residuals(anova0.8)) #ok

# pour la dernière humidité
data_fin <- data_phase1[data_phase1$humidity_ws <= quantile(data_phase1$humidity_ws[data_phase1$species == "maize"], 0.05), ]
anovafin <- aov(tot_area ~ species * condition, data=data_fin)
summary(anovafin) #effet espèce et condition
shapiro.test(residuals(anovafin)) #ok

#ANCOVA
ancova1 <- aov(tot_area ~ species * condition + humidity + date, data = data_phase1)
summary(ancova1)
plot(residuals(ancova1))
ancova1_sqrt <- aov(sqrt(tot_area) ~ species * condition + humidity + date, data = data_phase1)
summary(ancova1_sqrt)
plot(residuals(ancova1_sqrt))
# Normalité des résidus
residuals <- residuals(ancova1)
residuals_sqrt <- residuals(ancova1_sqrt)
shapiro.test(residuals) # Test de Shapiro-Wilk pour la normalité
shapiro.test(residuals_sqrt)
# Homogénéité des variances (test de Levene)
leveneTest(tot_area ~ species * condition, data = data_phase1)
# Diagnostic des résidus
par(mfrow = c(2, 2)) # Fenêtre avec 4 graphiques
plot(ancova1) # Résidus vs ajustements, QQ-plot, etc.


# Comparaison du taux de croissance

ggplot(data_phase1, aes(x = tot_area, y = GrowthRate, color = species, linetype = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Growth Rate vs. Area",
    x = "Area (cm²)",
    y = "Growth Rate (cm²/day)"
  ) +
  theme_minimal()

#ANCOVA
ancova1 <- aov(GrowthRate ~ species * condition + humidity + date, data = data_phase1)
summary(ancova1)
plot(residuals(ancova1))
# Normalité des résidus
residuals <- residuals(ancova1)
shapiro.test(residuals) # Test de Shapiro-Wilk pour la normalité
# Homogénéité des variances (test de Levene)
leveneTest(GrowthRate ~ species * condition, data = data_phase1)
# Diagnostic des résidus
par(mfrow = c(2, 2)) # Fenêtre avec 4 graphiques
plot(ancova1) # Résidus vs ajustements, QQ-plot, etc.


# Comparaison de la croissance cumulée

# pour humidité = 1.1
anova1.1 <- aov(AreaCum ~ species * condition, data=data_1.1)
summary(anova1.1) #effet espèce
shapiro.test(residuals(anova1.1)) #ok

# pour humidité = 0.9
anova0.9 <- aov(AreaCum ~ species * condition, data=data_0.9)
summary(anova0.9) #effet espèce et condition
shapiro.test(residuals(anova0.9)) #ok

# pour humidité = 0.8
anova0.8 <- aov(AreaCum ~ species * condition, data=data_0.8)
summary(anova0.8) #effet espèce et condition
shapiro.test(residuals(anova0.8)) #ok

# pour la dernière humidité
anovafin <- aov(AreaCum ~ species * condition, data=data_fin)
summary(anovafin) #effet espèce et condition
shapiro.test(residuals(anovafin)) #ok


# Comparaison de la croissance relative

ggplot(data_phase1, aes(x = tot_area, y = RelativeGrowth, color = species, linetype = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Relative growth vs. Area",
    x = "Area (cm²)",
    y = "Relative growth"
  ) +
  theme_minimal()

# pour humidité = 1.1
anova1.1 <- aov(RelativeGrowth ~ species * condition, data=data_1.1)
summary(anova1.1) #effet espèce et condition
shapiro.test(residuals(anova1.1)) #ok

# pour humidité = 0.9
anova0.9 <- aov(RelativeGrowth ~ species * condition, data=data_0.9)
summary(anova0.9) #effet espèce et condition
shapiro.test(residuals(anova0.9)) # PAS OK !!

# pour humidité = 0.8
anova0.8 <- aov(RelativeGrowth ~ species * condition, data=data_0.8)
summary(anova0.8) #effet espèce et condition
shapiro.test(residuals(anova0.8)) #ok

# pour la dernière humidité
anovafin <- aov(RelativeGrowth ~ species * condition, data=data_fin)
summary(anovafin) #effet espèce et condition
shapiro.test(residuals(anovafin)) #PAS OK!!

# Modèle linéaire robuste 
model_robust <- lmrob(RelativeGrowth ~ species * condition + humidity + date, data = data_phase1)
summary(model_robust)
plot(model_robust$residuals)
hist(residuals(model_robust), main = "Histogramme des résidus", xlab = "Résidus")
shapiro.test(residuals(model_robust))

# Modèle mixte robuste
model_mixed <- lmer(RelativeGrowth ~ species * condition + humidity + date + (1|plant), data = data_phase1)
summary(model_mixed)
hist(residuals(model_mixed), main = "Histogramme des résidus du modèle mixte", xlab = "Résidus")
qqnorm(residuals(model_mixed))
qqline(residuals(model_mixed), col = "red")
shapiro.test(residuals(model_mixed))
plot(model_mixed)
vif(model_mixed)


             
