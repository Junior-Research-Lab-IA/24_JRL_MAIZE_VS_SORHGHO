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

# Dataset pour la phase 2 chez le maïs
data_maize <- data %>% filter (species=="maize")
data_m2 <- data_maize %>% filter (date %in% c("2024-11-18","2024-11-20", "2024-11-22", "2024-11-25"))
data_m2 <- data_m2 %>% filter (rhizotron %in% c("1","2","3","4"))

# Intégration de l'humidité moyenne des rhizotrons WS
humidity_ws <- data_m2 %>%
  filter(condition == "ws",humidity != 0) %>%
  group_by(thermal.time) %>%
  summarise(humidity_ws = mean(humidity, na.rm = TRUE))
data_m2 <- data_m2 %>%
  left_join(humidity_ws, by = "thermal.time")

## Visualisation de l'évolution de l'aire totale

# données brutes
ggplot(data_m2, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
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
ggplot(data_m2, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
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
ggplot(data_m2, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_smooth(aes(group = condition), se=TRUE) +
  labs(
    title = "Adjusted total average maize leaves area and humidity evolution during phase 1",
    x = "Thermal time (degree days)",
    y = "Total average area (cm²)",
    color = "Condition"
  )

## Statistical tests on conditions A MODIFIER

ggplot(data_m2, aes(x = thermal.time, y = tot_area, color = condition)) +
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

slopes_m2_area <- data_m2 %>%
  group_by(plant, rhizotron, condition) %>%
  summarise(slope_a = coef(lm(tot_area ~ date))[2]) 
shapiro.test(slopes_m2_area$slope_a[slopes_m2_area$condition == "ww"]) #ok
shapiro.test(slopes_m2_area$slope_a[slopes_m2_area$condition == "ws"]) #ok
ggplot(slopes_m2_area, aes(x = slope_a, fill = condition)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution des pentes par condition") +
  theme_minimal()
leveneTest(slope_a ~ condition, data = slopes_m2_area) #pas ok
t_test <- t.test(slope_a ~ condition, data = slopes_m2_area, var.equal = FALSE)
print(t_test)
#*résultats significatifs !!*#
ggplot(data_m2, aes(x = date, y = tot_area, color = condition)) +
  geom_line(aes(group = rhizotron)) +
  facet_wrap(~ condition) +
  labs(title = "Évolution de l'aire totale par condition", 
       x = "Temps (jours)", 
       y = "Aire totale moyenne") +
  theme_minimal()

# Comparaison aire totale

# le 18/11 (sampling 1)
data_ms1 <- data_m2 %>%
  filter(date == as.Date("2024-11-18")) # Replace with the specific date
shapiro.test(data_ms1$tot_area[data_ms1$condition == "ww"]) #ok
shapiro.test(data_ms1$tot_area[data_ms1$condition == "ws"]) #ok
leveneTest(tot_area ~ condition, data = data_ms1) #ok
t.test(tot_area ~ condition, data = data_ms1) #significatif
ggplot(data_ms1, aes(x = condition, y = tot_area, fill = condition)) +
  geom_boxplot() +
  labs(title = "Growth Rate by Condition", x = "Condition", y = "Growth Rate") +
  theme_minimal()

# le 22/11 (entre les 2 sampling)
data_mbs <- data_m2 %>%
  filter(date == as.Date("2024-11-22")) # Replace with the specific date
shapiro.test(data_mbs$tot_area[data_mbs$condition == "ww"]) #ok
shapiro.test(data_mbs$tot_area[data_mbs$condition == "ws"]) #ok
leveneTest(tot_area ~ condition, data = data_mbs) #ok
t.test(tot_area ~ condition, data = data_mbs) #significatif
ggplot(data_mbs, aes(x = condition, y = tot_area, fill = condition)) +
  geom_boxplot() +
  labs(title = "Growth Rate by Condition", x = "Condition", y = "Growth Rate") +
  theme_minimal()

# le 25/11 (sampling 2)
data_ms2 <- data_m2 %>%
  filter(date == as.Date("2024-11-25")) # Replace with the specific date
shapiro.test(data_ms2$tot_area[data_ms2$condition == "ww"]) #ok
shapiro.test(data_ms2$tot_area[data_ms2$condition == "ws"]) #ok
leveneTest(tot_area ~ condition, data = data_ms2) #ok
t.test(tot_area ~ condition, data = data_ms2) #significatif
ggplot(data_ms2, aes(x = condition, y = tot_area, fill = condition)) +
  geom_boxplot() +
  labs(title = "Growth Rate by Condition", x = "Condition", y = "Growth Rate") +
  theme_minimal()


# Comparaison du taux de croissance

ggplot(data_m2, aes(x = tot_area, y = GrowthRate, color = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Growth Rate vs. Area",
    x = "Area (cm²)",
    y = "Growth Rate (cm²/day)"
  ) +
  theme_minimal()

# le 18/11 (sampling 1)
data_ms1 <- data_m2 %>%
  filter(date == as.Date("2024-11-18")) # Replace with the specific date
shapiro.test(data_ms1$GrowthRate[data_ms1$condition == "ww"]) #ok
shapiro.test(data_ms1$GrowthRate[data_ms1$condition == "ws"]) #ok
leveneTest(GrowthRate ~ condition, data = data_ms1) #ok
t.test(GrowthRate ~ condition, data = data_ms1) #significatif
ggplot(data_ms1, aes(x = condition, y = GrowthRate, fill = condition)) +
  geom_boxplot() +
  labs(title = "Growth Rate by Condition", x = "Condition", y = "Growth Rate") +
  theme_minimal()

# le 22/11 (entre les 2 sampling)
data_mbs <- data_m2 %>%
  filter(date == as.Date("2024-11-22")) # Replace with the specific date
shapiro.test(data_mbs$GrowthRate[data_mbs$condition == "ww"]) #ok
shapiro.test(data_mbs$GrowthRate[data_mbs$condition == "ws"]) #pas ok
leveneTest(GrowthRate ~ condition, data = data_mbs) #ok
wilcox.test(GrowthRate ~ condition, data = data_mbs) #significatif

# le 25/11 (sampling 2)
data_ms2 <- data_m2 %>%
  filter(date == as.Date("2024-11-25")) # Replace with the specific date
shapiro.test(data_ms2$GrowthRate[data_ms2$condition == "ww"]) #ok
shapiro.test(data_ms2$GrowthRate[data_ms2$condition == "ws"]) #ok
leveneTest(GrowthRate ~ condition, data = data_ms2) #ok
t.test(GrowthRate ~ condition, data = data_ms2) #pas significatif !!
ggplot(data_ms2, aes(x = condition, y = GrowthRate, fill = condition)) +
  geom_boxplot() +
  labs(title = "Growth Rate by Condition", x = "Condition", y = "Growth Rate") +
  theme_minimal()


# Comparaison croissance relative

data_m2 <- data_m2 %>%
  group_by(condition, plant) %>%
  arrange(date) %>%
  mutate(
    RelativeGrowth = GrowthRate/tot_area
  ) %>%
  ungroup()

ggplot(data_m2, aes(x = tot_area, y = RelativeGrowth, color = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Relative Growth vs. Area",
    x = "Area (cm²)",
    y = "Relative Growth (cm²/day)"
  ) +
  theme_minimal()

# le 18/11 (sampling 1)
data_ms1 <- data_m2 %>%
  filter(date == as.Date("2024-11-18")) # Replace with the specific date
shapiro.test(data_ms1$RelativeGrowth[data_ms1$condition == "ww"]) #ok
shapiro.test(data_ms1$RelativeGrowth[data_ms1$condition == "ws"]) # pas ok
leveneTest(RelativeGrowth ~ condition, data = data_ms1) #ok
wilcox.test(RelativeGrowth ~ condition, data = data_ms1) #significatif

# le 22/11 (between samplings)
data_mbs <- data_m2 %>%
  filter(date == as.Date("2024-11-22")) # Replace with the specific date
shapiro.test(data_mbs$RelativeGrowth[data_mbs$condition == "ww"]) #ok
shapiro.test(data_mbs$RelativeGrowth[data_mbs$condition == "ws"]) #ok
leveneTest(RelativeGrowth ~ condition, data = data_mbs) #ok
t.test(RelativeGrowth ~ condition, data = data_mbs) #pas significatif
ggplot(data_mbs, aes(x = condition, y = RelativeGrowth, fill = condition)) +
  geom_boxplot() +
  labs(title = "Relative Growth by Condition", x = "Condition", y = "Growth Rate") +
  theme_minimal()

# le 25/11 (sampling 2)
data_ms2 <- data_m2 %>%
  filter(date == as.Date("2024-11-25")) # Replace with the specific date
shapiro.test(data_ms2$RelativeGrowth[data_ms2$condition == "ww"]) #ok
shapiro.test(data_ms2$RelativeGrowth[data_ms2$condition == "ws"]) #ok
leveneTest(RelativeGrowth ~ condition, data = data_ms2) #ok
t.test(RelativeGrowth ~ condition, data = data_ms2) #pas significatif
ggplot(data_ms2, aes(x = condition, y = RelativeGrowth, fill = condition)) +
  geom_boxplot() +
  labs(title = "Relative Growth by Condition", x = "Condition", y = "Growth Rate") +
  theme_minimal()



#### Sorghum

# Dataset pour la phase 1 chez le sorgho
data_sorg <- data %>% filter (species=="sorghum")
data_s2 <- data_sorg %>% filter (date %in% c("2024-12-04","2024-12-06","2024-12-09","2024-12-11"))
data_s2 <- data_s2 %>% filter (rhizotron %in% c("9","10","11","12"))

humidity_ws_s2 <- data_s2 %>%
  filter(condition == "ws",humidity != 0) %>%
  group_by(thermal.time) %>%
  summarise(humidity_ws = mean(humidity, na.rm = TRUE))

data_s2 <- data_s2 %>%
  left_join(humidity_ws_s2, by = "thermal.time")

## Visualisation of area evolution

ggplot(data_s2, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
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

ggplot(data_s2, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
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

ggplot(data_s2, aes(x = thermal.time, y = tot_area, color=condition, group_by(condition))) +
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

## Statistical tests on conditions





#### Comparison of maize and sorghum

#### Comparison of maize and sorghum

## Visualisation

data_phase2 <- bind_rows(data_m2, data_s2)

ggplot(data = data_phase2) +
  # Courbes des moyennes d'aire totale
  geom_smooth(aes(x = days.after.sowing, y = tot_area, 
                  color = species, linetype = condition), 
              size = 1.2) +
  # Personnalisation des axes et des labels
  labs(
    title = "Plants total area and WS rhizotrons humidity evolution",
    x = "Days after sowing",
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

ggplot(data_phase2, aes(x = days.after.sowing, y = tot_area, color=species, linetype=condition)) +
  geom_smooth(aes(group = plant), method = "lm", se=FALSE, ) +
  labs(
    title = "Adjusted total average maize leaves area and humidity evolution during phase 1",
    x = "Days after sowing",
    y = "Total average area (cm²)",
    color = "Condition")

#diagnostic des régressions linéaires
slopes_2 <- data_phase2 %>%
  group_by(plant, rhizotron, condition, species) %>%
  summarise(
    slope = coef(lm(tot_area ~ date))[2],
    intercept = coef(lm(tot_area ~ date))[1],
    r_squared = summary(lm(tot_area ~ date))$r.squared,
    p_value = summary(lm(tot_area ~ date))$coefficients[2, 4] # p-value du slope
  )
summary(slopes_2)
hist(slopes_2$r_squared) # Distribution des R^2

ggplot(slopes_2, aes(x = condition, y= slope, color = species)) +
  geom_boxplot() +
  labs(title = "Area growth slopes coefficient values by species and condition") +
  theme_minimal()
anovaslopes2 <- aov(slope ~ species * condition, data=slopes_2)
summary(anovaslopes2)
shapiro.test(residuals(anovaslopes2))#ok
tukey_slopes2 <- TukeyHSD(anovaslopes2, "species:condition")
print(tukey_slopes2)
#*significatif*#

# Comparaison de l'aire totale

ggplot(data_phase2, aes(x = condition, y = tot_area, color = species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Répartition de l'aire totale par condition et espèce", y = "Total area (cm²)", x = "Condition")

# pour sampling 1
data_S1 <- data_phase2 %>% filter((species == "maize" & date == as.Date("2024-11-18")) |
           (species == "sorghum" & date == as.Date("2024-12-04")))
data_S1 <- data_S1 %>% filter(!is.na(tot_area))
anovaS1 <- aov(tot_area ~ species * condition, data=data_S1)
summary(anovaS1) #
shapiro.test(residuals(anovaS1)) #ok
ggplot(data_S1, aes(x = condition, y= tot_area, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition") +
  theme_minimal()
tukey_scS1 <- TukeyHSD(anovaS1, "species:condition")
print(tukey_scS1)

# between samplings
data_BS <- data_phase2 %>% filter((species == "maize" & date == as.Date("2024-11-22")) |
                                    (species == "sorghum" & date == as.Date("2024-12-09")))
data_BS <- data_BS %>% filter(!is.na(tot_area))
anovaBS <- aov(tot_area ~ species * condition, data=data_BS)
summary(anovaBS) 
shapiro.test(residuals(anovaBS)) #ok
ggplot(data_BS, aes(x = condition, y= tot_area, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition") +
  theme_minimal()
tukey_scBS <- TukeyHSD(anovaBS, "species:condition")
print(tukey_scBS)

# pour sampling 2
data_S2 <- data_phase2 %>% filter((species == "maize" & date == as.Date("2024-11-25")) |
                                    (species == "sorghum" & date == as.Date("2024-12-11")))
data_S2<- data_S2 %>% filter(!is.na(tot_area))
anovaS2 <- aov(tot_area ~ species * condition, data=data_S2)
summary(anovaS2) #effet espèce et condition
shapiro.test(residuals(anovaS2)) #ok
ggplot(data_S2, aes(x = condition, y= tot_area, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition") +
  theme_minimal()
tukey_scS2 <- TukeyHSD(anovaS2, "species:condition")
print(tukey_scS2)


# Comparaison du taux de croissance

ggplot(data_phase2, aes(x = tot_area, y = GrowthRate, color = species, linetype = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Growth Rate vs. Area",
    x = "Area (cm²)",
    y = "Growth Rate (cm²/day)"
  ) +
  theme_minimal()


# Comparaison de la croissance cumulée

# Comparaison de la croissance relative

ggplot(data_phase2, aes(x = tot_area, y = RelativeGrowth, color = species, linetype = condition)) +
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


# pour sampling 1
data_S1 <- data_phase2 %>% filter((species == "maize" & date == as.Date("2024-11-18")) |
                                    (species == "sorghum" & date == as.Date("2024-12-04")))
data_S1 <- data_S1 %>% filter(!is.na(RelativeGrowth))
anovaS1 <- aov(RelativeGrowth ~ species * condition, data=data_S1)
summary(anovaS1) #
shapiro.test(residuals(anovaS1)) #ok
ggplot(data_S1, aes(x = condition, y= RelativeGrowth, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition") +
  theme_minimal()
tukey_scS1 <- TukeyHSD(anovaS1, "species:condition")
print(tukey_scS1)

# between samplings
data_BS <- data_phase2 %>% filter((species == "maize" & date == as.Date("2024-11-22")) |
                                    (species == "sorghum" & date == as.Date("2024-12-09")))
data_BS <- data_BS %>% filter(!is.na(RelativeGrowth))
anovaBS <- aov(RelativeGrowth ~ species * condition, data=data_BS)
summary(anovaBS) 
shapiro.test(residuals(anovaBS)) #ok
ggplot(data_BS, aes(x = condition, y= RelativeGrowth, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition") +
  theme_minimal()
tukey_scBS <- TukeyHSD(anovaBS, "species:condition")
print(tukey_scBS)

# pour sampling 2
data_S2 <- data_phase2 %>% filter((species == "maize" & date == as.Date("2024-11-25")) |
                                    (species == "sorghum" & date == as.Date("2024-12-11")))
data_S2<- data_S2 %>% filter(!is.na(RelativeGrowth))
anovaS2 <- aov(RelativeGrowth ~ species * condition, data=data_S2)
summary(anovaS2) #effet espèce et condition
shapiro.test(residuals(anovaS2)) #ok
ggplot(data_S2, aes(x = condition, y= RelativeGrowth, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition") +
  theme_minimal()
tukey_scS2 <- TukeyHSD(anovaS2, "species:condition")
print(tukey_scS2)


