library(ggplot2)
library(dplyr)
library(nlme)
library(lme4)
library(robustbase)
library(lmerTest)
library(car)

# Importation des données
data <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/dataleaves.csv", dec=",")
data <- data %>% select(-lenght,-width, -coef)
data$date <- as.Date(data$date, format = "%d/%m")

# Ajout du temps thermique
thermaltime <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/thermaltime.csv", dec=",")
thermaltime$date <- as.Date(thermaltime$date, format = "%d/%m")
data <- merge(thermaltime, data, by = "date")

# Ajout de l'humidité
humidity <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/humidity.csv", dec=",")
humidity$date <- as.Date(humidity$date, format = "%d/%m")
humidity <- humidity %>% select(-species)
data <- merge(humidity, data, by = c("date", "rhizotron"))

# Calcul de l'aire totale
data$area <- as.numeric(gsub(",", ".", data$area))
data <- data %>%
  group_by(species, rhizotron, plant, condition, date, humidity, thermal.time, days.after.sowing) %>%
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

# Calcul de la croissance relative
data <- data %>%
  group_by(condition, plant) %>%
  arrange(date) %>%
  mutate(
    RelativeGrowth = GrowthRate/tot_area
  ) %>%
  ungroup()


#### Maize

# Dataset pour la phase 1 chez le maïs
data_maize <- data %>% filter (species=="maize")
data_m1 <- data_maize %>% filter (date %in% c("2024-10-28","2024-10-30","2024-11-01","2024-11-04","2024-11-06","2024-11-08","2024-11-11","2024-11-13","2024-11-15","2024-11-18"))

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

# Comparaison aire totale

# le 06/11 (humidité maize = 1.1)
data_m1.1 <- data_m1 %>%
  filter(date == as.Date("2024-11-06")) # Replace with the specific date
shapiro.test(data_m1.1$tot_area[data_m1.1$condition == "ww"]) #ok
shapiro.test(data_m1.1$tot_area[data_m1.1$condition == "ws"]) #ok
leveneTest(tot_area ~ condition, data = data_m1.1) #ok
t.test(tot_area ~ condition, data = data_m1.1) #pas significatif
ggplot(data_m1.1, aes(x = condition, y = tot_area, fill = condition)) +
  geom_boxplot() +
  labs(title = "Total area by Condition", x = "Condition", y = "Total area") +
  theme_minimal()

# le 11/11 (humidité maize = 0.9)
data_m0.9 <- data_m1 %>%
  filter(date == as.Date("2024-11-11")) # Replace with the specific date
shapiro.test(data_m0.9$tot_area[data_m0.9$condition == "ww"]) #ok
shapiro.test(data_m0.9$tot_area[data_m0.9$condition == "ws"]) #ok
leveneTest(tot_area ~ condition, data = data_m0.9) #ok
t.test(tot_area ~ condition, data = data_m0.9) #a peine significatif
ggplot(data_m0.9, aes(x = condition, y = tot_area, fill = condition)) +
  geom_boxplot() +
  labs(title = "Total area by Condition", x = "Condition", y = "Total area") +
  theme_minimal()

# le 13/11 (humidité maize = 0.82)
data_m0.8 <- data_m1 %>%
  filter(date == as.Date("2024-11-13")) # Replace with the specific date
shapiro.test(data_m0.8$tot_area[data_m0.8$condition == "ww"]) #ok
shapiro.test(data_m0.8$tot_area[data_m0.8$condition == "ws"]) #ok
leveneTest(tot_area ~ condition, data = data_m0.8) #pas trop ok
t.test(tot_area ~ condition, data = data_m0.8) #significatif!
ggplot(data_m0.8, aes(x = condition, y = tot_area, fill = condition)) +
  geom_boxplot() +
  labs(title = "Total area by Condition", x = "Condition", y = "Total area") +
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

# Dataset pour la phase 1 chez le sorgho  AJOUTER 04/12
data_sorg <- data %>% filter (species=="sorghum")
data_s1 <- data_sorg %>% filter (date %in% c("2024-10-28","2024-10-30","2024-11-01","2024-11-04","2024-11-06","2024-11-08","2024-11-11","2024-11-13","2024-11-15", "2024-11-18","2024-11-20","2024-11-22","2024-11-25", "2024-11-27", "2024-12-02", "2024-12-04"))

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

# Comparaison des régressions linéaires de l'aire

slopes_s1_area <- data_s1 %>%
  group_by(plant, rhizotron, condition) %>%
  summarise(slope_a = coef(lm(tot_area ~ date))[2]) 
shapiro.test(slopes_s1_area$slope_a[slopes_s1_area$condition == "ww"]) #ok
shapiro.test(slopes_s1_area$slope_a[slopes_s1_area$condition == "ws"]) #ok
ggplot(slopes_s1_area, aes(x = slope_a, fill = condition)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution des pentes par condition") +
  theme_minimal()
leveneTest(slope_a ~ condition, data = slopes_s1_area) #ok
t_test <- t.test(slope_a ~ condition, data = slopes_s1_area, var.equal = TRUE)
print(t_test)
#*significatif*#

# Comparaison de l'aire totale

# le 11/11 (humidité sorg = 1.1)
data_s1.1 <- data_s1 %>%
  filter(date == as.Date("2024-11-11")) # Replace with the specific date
shapiro.test(data_s1.1$tot_area[data_s1.1$condition == "ww"]) #ok
shapiro.test(data_s1.1$tot_area[data_s1.1$condition == "ws"]) #ok
leveneTest(tot_area ~ condition, data = data_s1.1) #ok
t.test(tot_area ~ condition, data = data_s1.1) #pas significatif
ggplot(data_s1.1, aes(x = condition, y = tot_area, fill = condition)) +
  geom_boxplot() +
  labs(title = "Total area by Condition", x = "Condition", y = "Total area") +
  theme_minimal()
anova_result <- aov(tot_area ~ condition, data = data_s1.1)
summary(anova_result) #pas significatif

# le 25/11 (humidité sorg = 0.9)
data_s0.9 <- data_s1 %>%
  filter(date == as.Date("2024-11-25")) # Replace with the specific date
shapiro.test(data_s0.9$tot_area[data_s0.9$condition == "ww"]) #ok
shapiro.test(data_s0.9$tot_area[data_s0.9$condition == "ws"]) #ok
leveneTest(tot_area ~ condition, data = data_s0.9) #ok
t.test(tot_area ~ condition, data = data_s0.9) #pas significatif
ggplot(data_s0.9, aes(x = condition, y = tot_area, fill = condition)) +
  geom_boxplot() +
  labs(title = "Total area by Condition", x = "Condition", y = "Total area") +
  theme_minimal()
anova_result <- aov(tot_area ~ condition, data = data_s0.9)
summary(anova_result) #pas significatif

# le 27/11 (humidité sorg = 0.85)
data_s0.8 <- data_s1 %>%
  filter(date == as.Date("2024-11-27")) # Replace with the specific date
shapiro.test(data_s0.8$tot_area[data_s0.8$condition == "ww"]) #ok
shapiro.test(data_s0.8$tot_area[data_s0.8$condition == "ws"]) #ok
leveneTest(tot_area ~ condition, data = data_s0.8) #ok
t.test(tot_area ~ condition, data = data_s0.8) #significatif
ggplot(data_s0.8, aes(x = condition, y = tot_area, fill = condition)) +
  geom_boxplot() +
  labs(title = "Total area by Condition", x = "Condition", y = "Total area") +
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
  geom_smooth(aes(x = days.after.sowing, y = tot_area, 
                  color = species, linetype = condition), 
              size = 1.2) +
  # Courbes des humidités WS (humidité multipliée pour ajuster l'échelle)
  geom_line(aes(x = days.after.sowing, y = humidity_ws * 1000, 
                color = species), size = 1, linetype = "dotted") +
  # Personnalisation des axes et des labels
  labs(
    title = "Plants total area and WS rhizotrons humidity evolution",
    x = "Days after sowing",
    y = "Total area (cm²)",
    color = "Species",
    linetype = "Condition"
  ) +
  # Ajouter un axe secondaire pour l'humidité
  scale_y_continuous(
    name = "Total area (cm²)", 
    sec.axis = sec_axis(~ . / 1000, name = "Soil water content (%)")
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
  # Personnalisation des axes et des labels
  labs(
    title = "Plants total area evolution depending on humidity",
    x = "Soil water content (%)",
    y = "Total area (cm²)",
    color = "Species",
    linetype = "Condition"
  ) +
  # Thème minimal avec personnalisation de l'apparence
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right"
  )

## Tests statistiques

# Comparaison des données ajustées a un modèle linéaire

ggplot(data_phase1, aes(x = days.after.sowing, y = tot_area, color=species, linetype=condition)) +
  # Courbes des humidités WS (humidité multipliée pour ajuster l'échelle)
  geom_line(aes(x = days.after.sowing, y = humidity_ws * 1000, 
                color = species), size = 1, linetype = "dotdash") +
  geom_smooth(aes(group = plant), method = "lm", se=FALSE, ) +
  labs(
    title = "Adjusted total average maize leaves area and humidity evolution during phase 1",
    x = "Days after sowing",
    y = "Total average area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total area (cm²)", sec.axis = sec_axis(~ . / 1000, name = "Soil water content (gH2O g-1 dry soil)")
  )

#diagnostic des régressions linéaires
slopes_1 <- data_phase1 %>%
  group_by(plant, rhizotron, condition, species) %>%
  summarise(
    slope = coef(lm(tot_area ~ date))[2],
    intercept = coef(lm(tot_area ~ date))[1],
    r_squared = summary(lm(tot_area ~ date))$r.squared,
    p_value = summary(lm(tot_area ~ date))$coefficients[2, 4] # p-value du slope
  )
summary(slopes_1)
hist(slopes_1$r_squared) # Distribution des R^2

ggplot(slopes_1, aes(x = condition, y= slope, color = species)) +
  geom_boxplot() +
  labs(title = "Area growth slopes coefficient values by species and condition") +
  theme_minimal()
anovaslopes1 <- aov(slope ~ species * condition, data=slopes_1)
summary(anovaslopes1)
shapiro.test(residuals(anovaslopes1))#ok
tukey_slopes1 <- TukeyHSD(anovaslopes1, "species:condition")
print(tukey_slopes1)
#*significatif*#

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
ggplot(data_1.1, aes(x = condition, y= tot_area, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition") +
  theme_minimal()
tukey_sc1.1 <- TukeyHSD(anova1.1, "species:condition")
print(tukey_sc1.1)

# pour humidité = 0.9
data_0.9 <- data_phase1[data_phase1$humidity_ws >= 0.89 & data_phase1$humidity_ws <= 0.91, ]
data_0.9 <- data_0.9 %>% filter(!is.na(tot_area))
anova0.9 <- aov(tot_area ~ species * condition, data=data_0.9)
summary(anova0.9) #effet espèce et condition
shapiro.test(residuals(anova0.9)) #ok
ggplot(data_0.9, aes(x = condition, y= tot_area, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition") +
  theme_minimal()
tukey_sc0.9 <- TukeyHSD(anova0.9, "species:condition")
print(tukey_sc0.9)

# pour humidité = 0.8
data_0.8 <- data_phase1[data_phase1$humidity_ws >= 0.76 & data_phase1$humidity_ws <= 0.86, ]
data_0.8 <- data_0.8 %>% filter(!is.na(tot_area))
anova0.8 <- aov(tot_area ~ species * condition, data=data_0.8)
summary(anova0.8) #effet espèce et condition
shapiro.test(residuals(anova0.8)) #ok
ggplot(data_0.8, aes(x = condition, y= tot_area, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition") +
  theme_minimal()
tukey_sc0.8 <- TukeyHSD(anova0.8, "species:condition")
print(tukey_sc0.8)

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
  theme_minimal() +
  scale_x_continuous(name = "Total area (cm²)", 
    sec.axis = sec_axis(~ . / 2, name = "WS average humidity (%)"))
  

# pour humidité = 1.1
anova1.1 <- aov(I(RelativeGrowth^2) ~ species * condition, data=data_1.1)
summary(anova1.1) #effet espèce et condition
shapiro.test(residuals(anova1.1)) #pas ok
residuals <- residuals(anova1.1)
data_1.1$residuals <- residuals
leveneTest(residuals ~ species*condition, data=data_1.1) #ok
ggplot(data_1.1, aes(x = condition, y= RelativeGrowth^2, color = species)) +
  geom_boxplot() +
  labs(title = "Relative growth by species and condition") +
  theme_minimal()
tukey_sc1.1 <- TukeyHSD(anova1.1, "species:condition")
print(tukey_sc1.1)

# pour humidité = 0.9
anova0.9 <- aov(I(RelativeGrowth^2) ~ species * condition, data=data_0.9)
summary(anova0.9) #effet espèce et condition
shapiro.test(residuals(anova0.9)) # c'est presque ok !!
residuals <- residuals(anova0.9)
data_0.9$residuals <- residuals
leveneTest(residuals ~ species*condition, data=data_0.9) #ok
ggplot(data_0.9, aes(x = condition, y= RelativeGrowth^2, color = species)) +
  geom_boxplot() +
  labs(title = "Relative growth by species and condition") +
  theme_minimal()
tukey_sc0.9 <- TukeyHSD(anova0.9, "species:condition")
print(tukey_sc0.9)

# pour humidité = 0.8
anova0.8 <- aov(log(RelativeGrowth) ~ species * condition, data=data_0.8)
summary(anova0.8) #effet espèce et condition
shapiro.test(residuals(anova0.8)) #ok
residuals <- residuals(anova0.8)
data_0.8$residuals <- residuals
leveneTest(residuals ~ species*condition, data=data_0.8) #ok
ggplot(data_0.8, aes(x = condition, y= log(RelativeGrowth), color = species)) +
  geom_boxplot() +
  labs(title = "Relative growth by species and condition") +
  theme_minimal()
tukey_sc0.8 <- TukeyHSD(anova0.8, "species:condition")
print(tukey_sc0.8)


# pour la dernière humidité
anovafin <- aov(RelativeGrowth ~ species * condition, data=data_fin)
summary(anovafin) #effet espèce et condition
shapiro.test(residuals(anovafin)) #PAS OK!!



             
