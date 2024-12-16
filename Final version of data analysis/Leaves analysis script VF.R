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


#### Phase 1

### Maize

# Dataset pour la phase 1 chez le maïs
data_maize <- data %>% filter (species=="maize")
data_m1 <- data_maize %>% filter (date %in% c("2024-10-28","2024-10-30","2024-11-01","2024-11-04","2024-11-06","2024-11-08","2024-11-11","2024-11-13","2024-11-15","2024-11-18"))
humidity_ws <- data_m1 %>%
  filter(condition == "ws",humidity != 0) %>%
  group_by(thermal.time) %>%
  summarise(humidity_ws = mean(humidity, na.rm = TRUE))
data_m1 <- data_m1 %>%
  left_join(humidity_ws, by = "thermal.time")


### Sorghum

# Dataset pour la phase 1 chez le sorgho
data_sorg <- data %>% filter (species=="sorghum")
data_s1 <- data_sorg %>% filter (date %in% c("2024-10-28","2024-10-30","2024-11-01","2024-11-04","2024-11-06","2024-11-08","2024-11-11","2024-11-13","2024-11-15", "2024-11-18","2024-11-20","2024-11-22","2024-11-25", "2024-11-27", "2024-12-02", "2024-12-04"))
humidity_ws_s1 <- data_s1 %>%
  filter(condition == "ws",humidity != 0) %>%
  group_by(thermal.time) %>%
  summarise(humidity_ws = mean(humidity, na.rm = TRUE))
data_s1 <- data_s1 %>%
  left_join(humidity_ws_s1, by = "thermal.time")


### Comparison of maize and sorghum under the 2 conditions

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
    title = "Species average total area and WS rhizotrons humidity evolution",
    x = "Days after sowing",
    y = "Total area (cm²)",
    color = "Species",
    linetype = "Condition"
  ) +
  # Ajouter un axe secondaire pour l'humidité
  scale_y_continuous(
    name = "Total area (cm²)", 
    sec.axis = sec_axis(~ . / 1000, name = "WS rhizotrons average SWC (g H2O g-1 Dry Soil)")
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
    title = "Species average total area evolution depending on humidity",
    x = "WS rhizotrons average SWC (g H2O g-1 Dry Soil)",
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

#1 Comparaison des données ajustées a un modèle linéaire

ggplot(data_phase1, aes(x = days.after.sowing, y = tot_area, color=species, linetype=condition)) +
  # Courbes des humidités WS (humidité multipliée pour ajuster l'échelle)
  geom_line(aes(x = days.after.sowing, y = humidity_ws * 1000, 
                color = species), size = 1, linetype = "dotdash") +
  geom_smooth(aes(group = plant), method = "lm", se=FALSE, ) +
  labs(
    title = "Adjusted plants average total area and rhizotrons humidity evolution during phase 1",
    x = "Days after sowing",
    y = "Total area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total area (cm²)", sec.axis = sec_axis(~ . / 1000, name = "WS rhizotrons average SWC (g H2O g-1 Dry Soil)")
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

ggplot(slopes_1, aes(x = condition, y= slope, color = species)) +
  geom_boxplot() +
  labs(title = "Total area ~ Time slopes coefficient values by species and condition") +
  theme_minimal()
anovaslopes1 <- aov(slope ~ species * condition, data=slopes_1)
summary(anovaslopes1)
shapiro.test(residuals(anovaslopes1))#ok
tukey_slopes1 <- TukeyHSD(anovaslopes1, "species:condition")
print(tukey_slopes1)


#2 Comparaison de l'aire totale

# pour humidité = 1.1
data_1.1 <- data_phase1[data_phase1$humidity_ws >= 1.08 & data_phase1$humidity_ws <= 1.12, ]
data_1.1 <- data_1.1 %>% filter(!is.na(tot_area))
anova1.1 <- aov(tot_area ~ species * condition, data=data_1.1)
summary(anova1.1) #effet espèce
shapiro.test(residuals(anova1.1)) #ok
ggplot(data_1.1, aes(x = condition, y= tot_area, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition for SWC = 1.1", x = "Condition",
       y = "Total area (cm²)") +
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
  labs(title = "Total area by species and conditionfor SWC = 0.9", x = "Condition",
       y = "Total area (cm²)") +
  theme_minimal()
tukey_sc0.9 <- TukeyHSD(anova0.9, "species:condition")
print(tukey_sc0.9)

# pour humidité = 0.8
data_0.8 <- data_phase1[data_phase1$humidity_ws >= 0.75 & data_phase1$humidity_ws <= 0.85, ]
data_0.8 <- data_0.8 %>% filter(!is.na(tot_area))
anova0.8 <- aov(tot_area ~ species * condition, data=data_0.8)
summary(anova0.8) #effet espèce et condition
shapiro.test(residuals(anova0.8)) #ok
ggplot(data_0.8, aes(x = condition, y= tot_area, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition for SWC = 0.8", x = "Condition",
       y = "Total area (cm²)") +
  theme_minimal()
tukey_sc0.8 <- TukeyHSD(anova0.8, "species:condition")
print(tukey_sc0.8)


#3 Comparaison de la croissance relative

ggplot(data_phase1, aes(x = tot_area, y = RelativeGrowth, color = species, linetype = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Relative growth depending on total area",
    x = "Total area (cm²)",
    y = "Relative growth (day-1)"
  ) +
theme_minimal()

# pour humidité = 1.1
anova1.1 <- aov(RelativeGrowth ~ species * condition, data=data_1.1)
summary(anova1.1) #effet espèce et condition
shapiro.test(residuals(anova1.1)) #ok
residuals <- residuals(anova1.1)
data_1.1$residuals <- residuals
leveneTest(residuals ~ species*condition, data=data_1.1) #pas ok
ggplot(data_1.1, aes(x = condition, y= RelativeGrowth, color = species)) +
  geom_boxplot() +
  labs(title = "Relative growth by species and condition for SWC = 1.1", x = "Condition",
       y = "Relative Growth (day-1)") +
  theme_minimal()
tukey_sc1.1 <- TukeyHSD(anova1.1, "species:condition")
print(tukey_sc1.1)

# pour humidité = 0.9
anova0.9 <- aov(RelativeGrowth ~ species * condition, data=data_0.9)
summary(anova0.9) #effet espèce et condition
shapiro.test(residuals(anova0.9)) # c'est presque ok !!
residuals <- residuals(anova0.9)
data_0.9$residuals <- residuals
leveneTest(residuals ~ species*condition, data=data_0.9) #ok
ggplot(data_0.9, aes(x = condition, y= RelativeGrowth, color = species)) +
  geom_boxplot() +
  labs(title = "Relative growth by species and condition for SWC = 0.9", x = "Condition",
       y = "Relative Growth (day-1)") +
  theme_minimal()
tukey_sc0.9 <- TukeyHSD(anova0.9, "species:condition")
print(tukey_sc0.9)

# pour humidité = 0.8
anova0.8 <- aov(RelativeGrowth ~ species * condition, data=data_0.8)
summary(anova0.8) #effet espèce et condition
shapiro.test(residuals(anova0.8)) #ok
residuals <- residuals(anova0.8)
data_0.8$residuals <- residuals
leveneTest(residuals ~ species*condition, data=data_0.8) #ok
ggplot(data_0.8, aes(x = condition, y= RelativeGrowth, color = species)) +
  geom_boxplot() +
  labs(title = "Relative growth by species and condition for SWC = 0.8", x = "Condition",
       y = "Relative Growth (day-1)") +
  theme_minimal()
tukey_sc0.8 <- TukeyHSD(anova0.8, "species:condition")
print(tukey_sc0.8)



#### Phase 2

### Maize

# Dataset pour la phase 2 chez le maïs
data_maize <- data %>% filter (species=="maize")
data_m2 <- data_maize %>% filter (date %in% c("2024-11-18","2024-11-20", "2024-11-22", "2024-11-25"))
data_m2 <- data_m2 %>% filter (rhizotron %in% c("1","2","3","4"))
humidity_ws <- data_m2 %>%
  filter(condition == "ws",humidity != 0) %>%
  group_by(thermal.time) %>%
  summarise(humidity_ws = mean(humidity, na.rm = TRUE))
data_m2 <- data_m2 %>%
  left_join(humidity_ws, by = "thermal.time")


### Sorghum

# Dataset pour la phase 2 chez le sorgho
data_sorg <- data %>% filter (species=="sorghum")
data_s2 <- data_sorg %>% filter (date %in% c("2024-12-04","2024-12-06","2024-12-09","2024-12-11"))
data_s2 <- data_s2 %>% filter (rhizotron %in% c("9","10","11","12"))
humidity_ws_s2 <- data_s2 %>%
  filter(condition == "ws",humidity != 0) %>%
  group_by(thermal.time) %>%
  summarise(humidity_ws = mean(humidity, na.rm = TRUE))
data_s2 <- data_s2 %>%
  left_join(humidity_ws_s2, by = "thermal.time")


### Comparison of maize and sorghum under the 2 conditions

## Visualisation

data_phase2 <- bind_rows(data_m2, data_s2)

ggplot(data = data_phase2) +
  # Courbes des moyennes d'aire totale
  geom_smooth(aes(x = days.after.sowing, y = tot_area, 
                  color = species, linetype = condition), 
              size = 1.2) +
  # Personnalisation des axes et des labels
  labs(
    title = "Species average total area evolution",
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

#1 Comparaison des données ajustées a un modèle linéaire

ggplot(data_phase2, aes(x = days.after.sowing, y = tot_area, color=species, linetype=condition)) +
  geom_smooth(aes(group = plant), method = "lm", se=FALSE, ) +
  labs(
    title = "Adjusted plants average total area evolution during phase 2",
    x = "Days after sowing",
    y = "Total area (cm²)",
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

ggplot(slopes_2, aes(x = condition, y= slope, color = species)) +
  geom_boxplot() +
  labs(title = "Total area ~ Time slopes coefficient values by species and condition") +
  theme_minimal()
anovaslopes2 <- aov(slope ~ species * condition, data=slopes_2)
summary(anovaslopes2)
shapiro.test(residuals(anovaslopes2))#ok
tukey_slopes2 <- TukeyHSD(anovaslopes2, "species:condition")
print(tukey_slopes2)
#*significatif*#


#2 Comparaison de l'aire totale

# pour sampling 1
data_S1 <- data_phase2 %>% filter((species == "maize" & date == as.Date("2024-11-18")) |
                                    (species == "sorghum" & date == as.Date("2024-12-04")))
data_S1 <- data_S1 %>% filter(!is.na(tot_area))
anovaS1 <- aov(tot_area ~ species * condition, data=data_S1)
summary(anovaS1) #
shapiro.test(residuals(anovaS1)) #ok
ggplot(data_S1, aes(x = condition, y= tot_area, color = species)) +
  geom_boxplot() +
  labs(title = "Total area by species and condition for sampling 1", x = "Condition",
       y = "Total area (cm²)") +
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
  labs(title = "Total area by species and condition for a date between samplings", x = "Condition",
       y = "Total area (cm²)") +
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
  labs(title = "Total area by species and condition for sampling 2", x = "Condition",
       y = "Total area (cm²)") +
  theme_minimal()
tukey_scS2 <- TukeyHSD(anovaS2, "species:condition")
print(tukey_scS2)


#3 Comparaison de la croissance relative

ggplot(data_phase2, aes(x = tot_area, y = RelativeGrowth, color = species, linetype = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Relative growth depending on total area",
    x = "Area (cm²)",
    y = "Relative growth (day-1)"
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
  labs(title = "Total area by species and condition for sampling 1", x = "Condition",
       y = "Relative Growth (day-1)") +
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
  labs(title = "Total area by species and condition for a date between samplings", x = "Condition",
       y = "Relative Growth (day-1)") +
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
  labs(title = "Total area by species and condition for sampling 2", x = "Condition",
       y = "Relative Growth (day-1)") +
  theme_minimal()
tukey_scS2 <- TukeyHSD(anovaS2, "species:condition")
print(tukey_scS2)
