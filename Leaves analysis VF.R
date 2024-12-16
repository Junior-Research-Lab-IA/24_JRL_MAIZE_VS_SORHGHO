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

data$condition <- factor(data$condition, levels = c("ww", "ws"))


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
    title = " ",
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
  scale_color_manual(
    values = c("maize" = "darkorange", "sorghum" = "darkgreen")) +
  # Thème minimal avec personnalisation de l'apparence
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "right"
  )

ggplot(data = data_phase1) +
  # Courbes des moyennes d'aire totale
  geom_smooth(aes(x = humidity_ws, y = RelativeGrowth, 
                  color = species, linetype = condition), 
              size = 1.2, se = FALSE) +
  # Personnalisation des axes et des labels
  labs(
    title = "Variation in total plant area of maize and sorghum as a function of soil water content",
    x = "WS rhizotrons average SWC (g H2O g-1 Dry Soil)",
    y = "Total area (cm²)",
    color = "Species",
    linetype = "Condition"
  ) +
  scale_color_manual(
    values = c("maize" = "black", "sorghum" = "darkgrey")) +
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


## Relative Growth 

# Agrégation des données pour chaque espèce et date (calcul de la moyenne de RGR pour chaque groupe)
data_ws_agg <- data_phase1 %>%
  filter(condition == "ws") %>%
  group_by(species, humidity_ws) %>%
  summarise(RGRws = mean(RelativeGrowth))  # Moyenne de RGR pour chaque groupe
data_ww_agg <- data_phase1 %>%
  filter(condition == "ww") %>%
  group_by(species, humidity_ws) %>%
  summarise(RGRww = mean(RelativeGrowth))  # Moyenne de RGR pour chaque groupe
# Fusionner les données agrégées
data_merged <- left_join(data_ws_agg, data_ww_agg, by = c("species", "humidity_ws"))
# Calculer le ratio RGRws / RGRww
data_ratioRGR <- data_merged %>%
  mutate(
    RGRratio = RGRws / RGRww
  ) %>%
  na.omit()  # Retirer les lignes avec NA dans le ratio
# Tracer le graphique du ratio RGR pour chaque espèce en fonction du temps (date)
ggplot(data_ratioRGR, aes(x = humidity_ws, y = RGRratio, color = species, group = species)) +
  geom_line() +  # Tracer une ligne pour chaque espèce
  geom_smooth()+
  labs(
    title = "Ratio RGRws / RGRww en fonction du temps",
    x = "SWC",
    y = "Ratio RGRws / RGRww"
  ) +
  theme_minimal()  # Thème minimal pour le graphique


data_p1_area200 <- data_phase1 %>% filter (tot_area>300)
ggplot(data_p1_area200, aes(x = tot_area, y = RelativeGrowth, color = species, linetype = condition)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Relative growth depending on total area",
    x = "Total area (cm²)",
    y = "Relative growth (day-1)"
  ) +
  scale_color_manual(
    values = c("maize" = "black", "sorghum" = "darkgrey")) +
  theme_minimal()

# Diviser les données en fonction de l'espèce et de la condition
split_data <- split(data_p1_area200, list(data_p1_area200$species, data_p1_area200$condition))
# Appliquer la régression pour chaque sous-ensemble et calculer les résidus
for (group in names(split_data)) {
  # Extraire le sous-ensemble de données
  subset_data <- split_data[[group]]
  plot(subset_data$tot_area, subset_data$RelativeGrowth,
       main = paste("Espèce:", unique(subset_data$species), "Condition:", unique(subset_data$condition)),
       xlab = "Taille de l'aire (Total Area)",
       ylab = "Croissance relative (RGR)",
       pch = 19, col = "blue")
  # Ajouter une ligne de régression
  abline(lm(RelativeGrowth ~ tot_area, data = subset_data), col = "red", lwd = 2)
  # Calculer la corrélation de Pearson
  correlation <- cor(subset_data$tot_area, subset_data$RelativeGrowth)
  print(paste("Corrélation (", group, "): ", round(correlation, 2)))
  # Test statistique de la corrélation
  cor_test <- cor.test(subset_data$tot_area, subset_data$RelativeGrowth)
  print(cor_test)}
  
for (group in names(split_data)) {
   # Extraire le sous-ensemble de données
  subset_data <- split_data[[group]]
  # Régression linéaire de RelativeGrowth sur Total_Area
  model <- lm(RelativeGrowth ~ tot_area, data = subset_data)
  # Ajouter les résidus du modèle comme nouvelle colonne (c'est-à-dire la croissance relative ajustée)
  subset_data$residuals <- residuals(model)
  # Optionnellement, ajouter cette nouvelle colonne aux données globales
  data_p1_area200[data_p1_area200$species == unique(subset_data$species) & 
                data_p1_area200$condition == unique(subset_data$condition), "RGRresiduals"] <- subset_data$residuals
  
  # Afficher les résidus (croissance relative sans l'influence de Total_Area)
  print(paste("Résidus pour", group))
  print(head(subset_data$residuals))
}

data_1.1 <- data_p1_area200[data_p1_area200$humidity_ws >= 1.07 & data_p1_area200$humidity_ws <= 1.13, ]
# pour humidité = 1.1
anova1.1 <- aov(RGRresiduals ~ species * condition, data=data_1.1)
summary(anova1.1) #effet espèce
shapiro.test(residuals(anova1.1)) #ok
residuals <- residuals(anova1.1)
data_1.1$residuals <- residuals
leveneTest(residuals ~ species*condition, data=data_1.1) #ok
ggplot(data_1.1, aes(x = condition, y= RGRresiduals, color = species)) +
  geom_boxplot() +
  labs(title = "Relative growth by species and condition for SWC = 1.1", x = "Condition",
       y = "Relative Growth residuals (day-1)") +
  theme_minimal()

# pour humidité = 1
data_1 <- data_p1_area200[data_p1_area200$humidity_ws >= 0.99 & data_p1_area200$humidity_ws <= 1.03, ]
anova1 <- aov(RGRresiduals ~ species * condition, data=data_1)
summary(anova1) #effet espèce et condition
shapiro.test(residuals(anova1)) #ok
residuals <- residuals(anova1)
data_1$residuals <- residuals
leveneTest(residuals ~ species*condition, data=data_1) #ok
ggplot(data_1, aes(x = condition, y= RGRresiduals, color = species)) +
  geom_boxplot() +
  labs(title = "Relative growth by species and condition for SWC = 1", x = "Condition",
       y = "Relative Growth residuals (day-1)") +
  theme_minimal()

# pour humidité = 0.9
data_0.9 <- data_p1_area200[data_p1_area200$humidity_ws >= 0.89 & data_p1_area200$humidity_ws <= 0.91, ]
anova0.9 <- aov(RGRresiduals ~ species * condition, data=data_0.9)
summary(anova0.9) # pas d'effet
shapiro.test(residuals(anova0.9)) # pas ok
residuals <- residuals(anova0.9)
data_0.9$residuals <- residuals
leveneTest(residuals ~ species*condition, data=data_0.9) #ok
ggplot(data_0.9, aes(x = condition, y= RGRresiduals, color = species)) +
  geom_boxplot() +
  labs(title = "Relative growth by species and condition for SWC = 0.9", x = "Condition",
       y = "Relative Growth (day-1)") +
  theme_minimal()

# pour humidité = 0.8
data_0.8 <- data_p1_area200[data_p1_area200$humidity_ws >= 0.75 & data_p1_area200$humidity_ws <= 0.85, ]
anova0.8 <- aov(RelativeGrowth ~ species * condition, data=data_0.8)
summary(anova0.8) #effet espèce et condition
shapiro.test(residuals(anova0.8)) #ok
residuals <- residuals(anova0.8)
data_0.8$residuals <- residuals
leveneTest(residuals ~ species*condition, data=data_0.8) #pas ok
ggplot(data_0.8, aes(x = condition, y= RelativeGrowth, color = species)) +
  geom_boxplot() +
  labs(title = "Relative growth by species and condition for SWC = 0.8", x = "Condition",
       y = "Relative Growth (day-1)") +
  theme_minimal()
tukey_sc0.8 <- TukeyHSD(anova0.8, "species:condition")
print(tukey_sc0.8)

# pour humidité = 0.7
data_0.7 <- data_p1_area200[data_p1_area200$humidity_ws >= 0.71 & data_p1_area200$humidity_ws <= 0.75, ]
anova0.7 <- aov(RelativeGrowth ~ species * condition, data=data_0.7)
summary(anova0.7) #effet espèce et condition
shapiro.test(residuals(anova0.7)) #ok
residuals <- residuals(anova0.7)
data_0.7$residuals <- residuals
leveneTest(residuals ~ species*condition, data=data_0.7) #pas ok
ggplot(data_0.7, aes(x = condition, y= RelativeGrowth, color = species)) +
  geom_boxplot() +
  labs(title = "Relative growth by species and condition for SWC = 0.7", x = "Condition",
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
    color = "Condition") +
  scale_color_manual(
    values = c("maize" = "black", "sorghum" = "darkgrey")) +
  theme_minimal()


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


# sampling 1 - sampling 2

# Fusionner les données pour les deux jours sur la base de l'espèce, la condition, et le plant
data_merged <- full_join(data_S1, data_S2, by = c("species", "condition", "plant"))

# Calculer la différence du RGR entre les deux jours pour chaque condition
data_diff <- data_merged %>%
  group_by(species, condition, plant) %>%
  mutate(
    area_diff = tot_area.y - tot_area.x  # RGR.y = jour 2, RGR.x = jour 1
  ) %>%
  select(species, condition, plant, area_diff)  # Garder seulement les colonnes d'intérêt

ggplot(data_diff, aes(x = species, y = area_diff, color = condition)) +
  geom_boxplot()+
  labs(title = "Différence du RGR entre jour 1 et jour 2",
       x = "Species",
       y = "Area difference") +
  theme_minimal()

anovadiff <- aov(area_diff ~ species * condition, data=data_diff)
summary(anovadiff)
shapiro.test(residuals(anovadiff))

datasampling <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/datasampling.csv", dec = ",")

datar_S1 <- datasampling %>% filter (Phase=="1")
datar_S2 <- datasampling %>% filter (Phase=="2")

data_merged <- full_join(datar_S1, datar_S2, by = c("species", "condition", "sampled_plant"))

data_merged$longer_root_lenght.y <- ave(data_merged$longer_root_lenght.y, data_merged$phase, data_merged$species, data_merged$condition, 
                 FUN = function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

mean_ws <- mean(datar_S2$longer_root_lenght[datar_S2$species == "sorghum" & datar_S2$condition == "ws"], na.rm = TRUE)
mean_ww <- mean(datar_S2$longer_root_lenght[datar_S2$species == "sorghum" & datar_S2$condition == "ww"], na.rm = TRUE)
new_rows <- data.frame(
  species = c("sorghum", "sorghum"),
  Phase = c(2, 2),
  condition = c("ws", "ww"),
  rhizotron = c(NA, NA),  # Mettre NA si non applicable
  sampled_plant = c(6, 7),
  plant = c(NA, NA),
  shoot_fresh_weight = c(NA, NA),
  root_fresh_weight = c(NA, NA),
  longer_root_lenght = c(mean_ws, mean_ww),
  shoot_dry_weight = c(NA, NA),
  root_dry_weight = c(NA, NA),
  R_S_Ratio = c(NA, NA), 
  X = c(NA, NA),
  X.1 = c(NA, NA)
)
datar_S2 <- rbind(datar_S2, new_rows)

data_diff <- data_merged %>%
  group_by(species, condition) %>%
  mutate(
    roots_diff = longer_root_lenght.y - longer_root_lenght.x 
  ) %>%
  select(species, condition, roots_diff)  # Garder seulement les colonnes d'intérêt

ggplot(data_diff, aes(x = species, y = roots_diff, color = condition)) +
  geom_boxplot()+
  labs(title = "Différence du RGR entre jour 1 et jour 2",
       x = "Species",
       y = "RGR difference") +
  theme_minimal()

anovadiff <- aov(roots_diff ~ species * condition, data=data_diff)
summary(anovadiff)
shapiro.test(residuals(anovadiff))


#3 Comparaison de la croissance relative

# Agrégation des données pour chaque espèce et date (calcul de la moyenne de RGR pour chaque groupe)
data_ws_agg <- data_phase2 %>%
  filter(condition == "ws") %>%
  group_by(species, days.after.sowing) %>%
  summarise(RGRws = mean(RelativeGrowth))  # Moyenne de RGR pour chaque groupe
data_ww_agg <- data_phase2 %>%
  filter(condition == "ww") %>%
  group_by(species, days.after.sowing) %>%
  summarise(RGRww = mean(RelativeGrowth))  # Moyenne de RGR pour chaque groupe
# Fusionner les données agrégées
data_merged <- left_join(data_ws_agg, data_ww_agg, by = c("species", "days.after.sowing"))
# Calculer le ratio RGRws / RGRww
data_ratioRGR <- data_merged %>%
  mutate(
    RGRratio = RGRws / RGRww
  ) %>%
  na.omit()  # Retirer les lignes avec NA dans le ratio
# Tracer le graphique du ratio RGR pour chaque espèce en fonction du temps (date)
ggplot(data_ratioRGR, aes(x = days.after.sowing, y = RGRratio, color = species, group = species)) +
  geom_line() +  # Tracer une ligne pour chaque espèce
  geom_smooth()+
  labs(
    title = "Ratio RGRws / RGRww en fonction du temps",
    x = "SWC",
    y = "Ratio RGRws / RGRww"
  ) +
  theme_minimal()  # Thème minimal pour le graphique

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


# sampling 1 - sampling 2

# Fusionner les données pour les deux jours sur la base de l'espèce, la condition, et le plant
data_merged <- full_join(data_S1, data_S2, by = c("species", "condition", "plant"))

# Calculer la différence du RGR entre les deux jours pour chaque condition
data_diff <- data_merged %>%
  group_by(species, condition, plant) %>%
  mutate(
    RGR_diff = RelativeGrowth.y - RelativeGrowth.x  # RGR.y = jour 2, RGR.x = jour 1
  ) %>%
  select(species, condition, plant, RGR_diff)  # Garder seulement les colonnes d'intérêt

ggplot(data_diff, aes(x = species, y = RGR_diff, color = condition)) +
  geom_boxplot()+
  labs(title = "Différence du RGR entre jour 1 et jour 2",
       x = "Species",
       y = "RGR difference") +
  scale_color_manual(
    values = c("ww" = "black", "ws" = "darkgrey")) +
  theme_minimal()

anovadiff <- aov(RGR_diff ~ species * condition, data=data_diff)
summary(anovadiff)
shapiro.test(residuals(anovadiff))
tukey_diff <- TukeyHSD(anovadiff, "species:condition")
print(tukey_diff)
