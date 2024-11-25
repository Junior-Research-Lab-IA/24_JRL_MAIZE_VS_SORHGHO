
library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/datauntil2211.csv", dec=",")
data <- data %>% select(-lenght,-width, -coef)
data$date <- as.Date(data$date, format = "%d/%m")

thermaltime <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/thermaltimeuntil2211.csv", dec=",")
thermaltime$date <- as.Date(thermaltime$date, format = "%d/%m")

humidity <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/humidityuntil2211.csv", dec=",")
humidity$date <- as.Date(humidity$date, format = "%d/%m")
humidity <- humidity %>% select(-species)

data <- merge(thermaltime, data, by = "date")
data <- merge(humidity, data, by = c("date", "rhizotron"))

data <- data %>%
  group_by(plant, leaf) %>%
  arrange(date) %>%
  mutate(area_growth = (area - lag(area)) / as.numeric(date - lag(date))) %>%
  mutate(height_growth = (height - lag(height)) / as.numeric(date - lag(date)))


### Work on plant L1 
## working on each leaf

data_L1 <- data %>% filter (plant == "L1")

ggplot(data_L1 %>% filter(area != 0), aes(x = date, y = area, color = leaf, group = leaf)) +
  geom_line(size = 1) +                # Ligne pour chaque feuille
  geom_point(size = 2) +               # Points pour chaque mesure
  labs(
    title = "Area evolution of L1 leaves",
    x = "Date",
    y = "Area"
  ) +
  theme_minimal() +                    # Thème propre
  scale_color_brewer(palette = "Set1") # Palette de couleurs

ggplot(data_L1 %>% filter(height != 0), aes(x = date, y = height, color = leaf, group = leaf)) +
  geom_line(size = 1) +                # Ligne pour chaque feuille
  geom_point(size = 2) +               # Points pour chaque mesure
  labs(
    title = "Height evolution of L1 leaves",
    x = "Date",
    y = "Height"
  ) +
  theme_minimal() +                    # Thème propre
  scale_color_brewer(palette = "Set1") # Palette de couleurs

ggplot(data_L1, aes(x = thermal.time, y = area_growth, color= leaf)) +
  geom_line() +
  labs(title = "L1 leaves area growth",
       x = "Time (degree days)", y = "Growth speed (cm²/dd)") +
  theme_minimal()

ggplot(data_L1, aes(x = thermal.time, y = height_growth, color= leaf)) +
  geom_line() +
  labs(title = "L1 leaves height growth",
       x = "Temps (jours)", y = "Growth speed (cm²/dd)") +
  theme_minimal()

## Working on the average

data_L1_total_area <- data_L1 %>%
  group_by(date, thermal.time, humidity) %>%
  summarise(total_area = sum(area))

ggplot(data_L1_total_area, aes(x = thermal.time, y = total_area)) +
  geom_line(size = 1)+
  geom_point(size = 2) +               # Points pour chaque mesure
  labs(
    title = "Leaf area evolution of L1",
    x = "thermal time",
    y = "Area"
  ) +
  theme_minimal()                   # Thème data_L1_avg_height <- data_L1 %>%

data_L1_total_height <- data_L1 %>%
  group_by(date, thermal.time, humidity) %>%
  summarise(total_height = sum(height))
  
ggplot(data_L1_total_height, aes(x = thermal.time, y = total_height)) +
  geom_line(size = 1) +
  geom_point(size = 2) +               # Points pour chaque mesure
  labs(
    title = "Leaf heigth evolution of L1",
    x = "thermal time",
    y = "Height"
  ) +
  theme_minimal()  

data_L1_total_area_growth <- data_L1 %>%
  group_by(date, thermal.time, humidity) %>%
  summarise(total_area_growth = sum(area_growth))

ggplot(data_L1_total_area_growth, aes(x = thermal.time, y = total_area_growth)) +
  geom_line() +
  labs(title = "L1 area growth speed",
       x = "thermal time (degree days)", y = "area growth speed") +
  theme_minimal()

# don't know if usefull ?
modele_L1_total_area_growth <- lm(total_area_growth ~ thermal.time, data = data_L1_total_area_growth)
data_L1_total_area_growth$total_area_growth_predicted <- predict(modele_L1_total_area_growth, newdata = data_L1_total_area_growth)
ggplot(data_L1_total_area_growth, aes(x = thermal.time, y = total_area_growth)) +
  geom_point() +
  geom_line(aes(y = total_area_growth_predicted), color = "blue") +
  labs(title = "fitted area growth speed",
       x = "thermal time", y = "total area growth speed (cm²/dd)") +
  theme_minimal()

data_L1_total_height_growth <- data_L1 %>%
  group_by(date, thermal.time, humidity) %>%
  summarise(total_height_growth = sum(height_growth))

ggplot(data_L1_total_height_growth, aes(x = thermal.time, y = total_height_growth)) +
  geom_line() +
  labs(title = "L1 height growth speed",
       x = "thermal time (degree days)", y = " total height growth speed") +
  theme_minimal()

# don't know if usefull ?
modele_L1_total_height_growth <- lm(total_height_growth ~ thermal.time, data = data_L1_total_height_growth)
data_L1_total_height_growth$total_height_growth_predicted <- predict(modele_L1_total_height_growth, newdata = data_L1_total_height_growth)
ggplot(data_L1_total_height_growth, aes(x = thermal.time, y = total_height_growth)) +
  geom_point() +
  geom_line(aes(y = total_height_growth_predicted), color = "blue") +
  labs(title = "fitted height growth speed",
       x = "thermal time", y = "area growth speed (cm²/dd)") +
  theme_minimal()


### Work on rhizotron 1 (L1 and R1 average)

data_1 <- data %>% filter(rhizotron == "1")

data_L1 <- data_1 %>% filter(plant == "L1")
data_L1 <- data_L1 %>%
  mutate(leaf = gsub("L1_", "", leaf))  # Supprime le préfixe L1_

data_R1 <- data_1 %>% filter(plant == "R1")
data_R1 <- data_R1 %>%
  mutate(leaf = gsub("R1_", "", leaf))  # Supprime le préfixe R1_

## Fusion des données de L1 et R1 
merged_data_1 <- left_join(data_L1, data_R1, join_by("rhizotron", "species", "condition","date", "leaf"),suffix = c("_L1", "_R1"))
data_avg_1 <- merged_data_1 %>%
  mutate(
    avg_area_growth = (area_growth_L1 + area_growth_R1)/2 ,
    avg_height_growth = (height_growth_L1 + height_growth_R1)/2 ,
    avg_area = (area_L1 + area_R1) / 2 , # Moyenne de l'aire entre L1 et R1
    avg_height = (height_L1 + height_R1) / 2 
  )

data_avg_1 <- data_avg_1 %>% select(rhizotron, leaf, avg_area, avg_height, avg_area_growth, avg_height_growth)
data_avg_1$date <- data_L1$date
data_avg_1$thermal.time <- data_L1$thermal.time
data_avg_1$humidity <- data_L1$humidity

data_1_tot_area_growth <- data_avg_1 %>%
  group_by(date, thermal.time, humidity) %>%
  summarise(tot_area_growth_1 = sum(avg_area_growth))

data_1_tot_height_growth <- data_avg_1 %>%
  group_by(date, thermal.time, humidity) %>%
  summarise(tot_height_growth_1 = sum(avg_height_growth))

data_1_tot_area <- data_avg_1 %>%
  group_by(date, thermal.time, humidity) %>%
  summarise(tot_area_1 = sum(avg_area))

data_1_tot_height <- data_avg_1 %>%
  group_by(date, thermal.time, humidity) %>%
  summarise(tot_height_1 = sum(avg_height))

ggplot(data_1_tot_area, aes(x = thermal.time, y = tot_area_1)) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity*1000), size = 1, color = "red") +
  labs(title = "Rhizotron 1 average leaf area evolution", x = "thermal time",y = "total area (cm²)") +
  scale_y_continuous(name = "Total area (cm²)", sec.axis = sec_axis(~ . / 1000, name = "Humidity"))

ggplot(data_1_tot_height, aes(x = thermal.time, y = tot_height_1)) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity*1000), size = 1, color = "red") +
  labs(title = "Rhizotron 1 average leaf height evolution", x = "thermal time",y = "total height (cm²)") +
  scale_y_continuous(name = "Total height (cm)", sec.axis = sec_axis(~ . / 1000, name = "Humidity"))

ggplot(data_1_tot_area_growth, aes(x = thermal.time, y = tot_area_growth_1)) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity*100), size = 1, color = "red") +
  labs(title = "Rhizotron 1 average leaf area growth", x = "thermal time",y = "total area growth (cm²/dd)") +
  scale_y_continuous(name = "Total area growth (cm²/dd)", sec.axis = sec_axis(~ . / 100, name = "Humidity"))

ggplot(data_1_tot_height_growth, aes(x = thermal.time, y = tot_height_growth_1)) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity*100), size = 1, color = "red") +
  labs(title = "Rhizotron 1 average leaf height growth", x = "thermal time",y = "total height growth (cm/dd)") +
  scale_y_continuous(name = "Total height growth (cm/dd)", sec.axis = sec_axis(~ . / 100, name = "Humidity"))


### work on maize (rhizotron 1 to 8)

## creating all averages of each rhizotron

final_data_avg_maize <- list()

for (i in 1:8) {

  data_i <- data %>% filter(rhizotron == i)

  data_Li <- data_i %>% filter(plant == paste0("L", i))
  data_Ri <- data_i %>% filter(plant == paste0("R", i))

  data_Li <- data_Li %>%
    mutate(leaf = gsub(paste0("L", i, "_"), "", leaf))  # Supprimer le préfixe L1_, L2_, ...
  data_Ri <- data_Ri %>%
    mutate(leaf = gsub(paste0("R", i, "_"), "", leaf))  # Supprimer le préfixe R1_, R2_, ...

  merged_data_i <- left_join(data_Li, data_Ri, by = c("rhizotron", "species", "condition", "date", "leaf", "thermal.time", "humidity"), suffix = c("_Li", "_Ri"))

  data_avg_i <- merged_data_i %>%
    mutate(
      avg_area = (area_Li + area_Ri) / 2,
      avg_height = (height_Li + height_Ri) / 2,
      avg_area_growth = (area_growth_Li + area_growth_Ri) / 2,
      avg_height_growth = (height_growth_Li + height_growth_Ri) / 2
    )
  data_avg_tot_i <- data_avg_i %>%
    select(date, thermal.time, humidity, condition, avg_area, avg_height, avg_area_growth, avg_height_growth) %>%
    group_by(date, thermal.time, humidity, condition) %>%
    summarise(
      tot_area = sum(avg_area, na.rm = TRUE),
      tot_height = sum(avg_height, na.rm = TRUE),
      tot_area_growth = sum(avg_area_growth, na.rm = TRUE),
      tot_height_growth = sum(avg_height_growth, na.rm = TRUE)
    )
  data_avg_tot_i$rhizotron <- i
  final_data_avg_maize[[i]] <- data_avg_tot_i
}

final_data_avg_maize <- bind_rows(final_data_avg_maize)

humidity_avg_ws <- final_data_avg_maize %>%
  filter(condition == "ws",humidity != 0) %>%
  group_by(thermal.time) %>%
  summarise(humidity_ws = mean(humidity, na.rm = TRUE))

final_data_avg_maize <- final_data_avg_maize %>%
  left_join(humidity_avg_ws, by = "thermal.time")

final_data_avg_maize <- final_data_avg_maize %>%
  filter(!(date %in% c("2024-11-20", "2024-11-22") & rhizotron %in% 5:8))

ggplot(final_data_avg_maize %>% filter(tot_area != 0), aes(x = thermal.time, y = tot_area, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  geom_line(aes(y = humidity_ws*1000), size = 1, color = "blue") +
  labs(
    title = "Total mean area evolution",
    x = "Date",
    y = "Total mean area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total mean area (cm²)", sec.axis = sec_axis(~ . / 1000, name = "WS average humidity")
  )

ggplot(final_data_avg_maize %>% filter(tot_height != 0), aes(x = thermal.time, y = tot_height, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  geom_line(aes(y = humidity_ws*1000), size = 1, color = "blue") +
  labs(
    title = "Total mean height evolution",
    x = "Date",
    y = "Total mean height (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total mean height (cm)", sec.axis = sec_axis(~ . / 1000, name = "WS average humidity")
  )

ggplot(final_data_avg_maize, aes(x = thermal.time, y = tot_area_growth, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  geom_line(aes(y = humidity_ws*100), size = 1, color = "blue") +
  labs(
    title = "Total mean area growth",
    x = "Date",
    y = "Total average area growth (cm²/dd)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average area growth (cm²/dd)", sec.axis = sec_axis(~ . / 100, name = "WS average humidity")
  )

ggplot(final_data_avg_maize, aes(x = thermal.time, y = tot_height_growth, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  geom_line(aes(y = humidity_ws*100), size = 1, color = "blue") +
  labs(
    title = "Total mean height growth",
    x = "Date",
    y = "Total average height growth (cm/dd)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average height growth (cm/dd)", sec.axis = sec_axis(~ . / 100, name = "WS average humidity")
  )

final_data_avg_maize <- final_data_avg_maize %>% 
  filter(tot_area!=0)

maize_avg_area_growth <- final_data_avg_maize %>%
  group_by(condition, thermal.time, humidity_ws) %>%
  summarise(maize_area_growth = mean(tot_area_growth))

maize_avg_height_growth <- final_data_avg_maize %>%
  group_by(condition, thermal.time, humidity_ws) %>%
  summarise(maize_height_growth = mean(tot_height_growth))

maize_avg_area <- final_data_avg_maize %>%
  group_by(condition, thermal.time, humidity_ws) %>%
  summarise(maize_area = mean(tot_area))

maize_avg_height <- final_data_avg_maize %>%
  group_by(condition, thermal.time, humidity_ws) %>%
  summarise(maize_height = mean(tot_height))

ggplot(maize_avg_area, aes(x = thermal.time, y = maize_area, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*1000), size = 1, color = "blue") +
  labs(
    title = "Total average maize leaves area and humidity evolution",
    x = "Thermal time (degree days)",
    y = "Total average area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average area (cm²)", sec.axis = sec_axis(~ . / 1000, name = "WS average humidity")
  )

ggplot(maize_avg_height, aes(x = thermal.time, y = maize_height, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*1000), size = 1, color = "blue") +
  labs(
    title = "Total average maize leaves height and humidity evolution",
    x = "Date",
    y = "Total average height (cm)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average height (cm)", sec.axis = sec_axis(~ . / 1000, name = "WS average humidity")
  )

ggplot(maize_avg_area_growth, aes(x = thermal.time, y = maize_area_growth, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*100), size = 1, color = "blue") +
  labs(
    title = "Total mean area growth",
    x = "Date",
    y = "Total average area growth (cm/dd)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average area growth (cm/dd)", sec.axis = sec_axis(~ . / 100, name = "WS average humidity")
  )

ggplot(maize_avg_height_growth, aes(x = thermal.time, y = maize_height_growth, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*100), size = 1, color = "blue") +
  labs(
    title = "Total mean height growth",
    x = "Date",
    y = "Total average height growth (cm/dd)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average height growth (cm/dd)", sec.axis = sec_axis(~ . / 100, name = "WS average humidity")
  )

## tests statistiques sur l'effet de la condition au cours du temps
data_maize_test <- final_data_avg_maize %>% select(-humidity_ws)
data_maize_test$condition <- factor(data_maize_test$condition, levels = c("ww", "ws"))
data_maize_test$condition <- relevel(data_maize_test$condition, ref = "ww")

# area

data_maize_test$sqrt_tot_area <- sqrt(data_maize_test$tot_area)
modele_mixed_sqrt <- lmer(sqrt_tot_area ~ condition * date + (1 | rhizotron), data = data_maize_test)
residus_sqrt <- resid(modele_mixed_sqrt)
qqnorm(residus_sqrt)
qqline(residus_sqrt)
shapiro.test(residus_sqrt) # sqrt premet d'avoir normality
plot(fitted(modele_mixed_sqrt), residus_sqrt)
abline(h = 0, col = "red")

modele_lm <- lm(residus ~ fitted(modele_mixed_sqrt))
test_bp <- bptest(modele_lm)
print(test_bp)  # mais on a pas homocesdasticity

model <- lme(tot_area ~ date * condition, random = ~ 1 | rhizotron, data = data_maize_test)
summary(model)
# on peut pas l'appliquer ? faut trouver un modele robuste d'apres ChatGPT

# height

data_maize_test$sqrt_tot_height <- sqrt(data_maize_test$tot_height)
modele_mixed_sqrt <- lmer(sqrt_tot_height ~ condition * date + (1 | rhizotron), data = data_maize_test)
residus_sqrt <- resid(modele_mixed_sqrt)
qqnorm(residus_sqrt)
qqline(residus_sqrt)
shapiro.test(residus_sqrt) # sqrt premet d'avoir normality
plot(fitted(modele_mixed_sqrt), residus_sqrt)
abline(h = 0, col = "red")

modele_lm <- lm(residus ~ fitted(modele_mixed_sqrt))
test_bp <- bptest(modele_lm)
print(test_bp)  # mais on a pas homocesdasticity

model <- lme(tot_height ~ date * condition, random = ~ 1 | rhizotron, data = data_maize_test)
summary(model) # idem?


### work on sorghum (rhizotron 9 to 16) 

## creating all averages of each rhizotron

data_sorg_area <- data %>% select(-height, -height_growth)

# we have to delete the tillers for the height analysis
data_sorg_height <- data %>% 
  filter(height!=0)
data_sorg_height <- data_sorg_height %>% select(-area, -area_growth)

# for area analysis
final_data_avg_sorg_area <- list()

for (i in 9:16) {
  data_i <- data_sorg_area %>% filter(rhizotron == i)
  
  plants_in_rhizotron <- unique(data_i$plant)
  
  if (length(plants_in_rhizotron) == 2) {
    # Cas normal avec deux plantes
    data_Li <- data_i %>% filter(plant == paste0("L", i))
    data_Ri <- data_i %>% filter(plant == paste0("R", i))
    
    data_Li <- data_Li %>%
      mutate(leaf = gsub(paste0("L", i, "_"), "", leaf))  # Supprimer le préfixe L1_, L2_, ...
    data_Ri <- data_Ri %>%
      mutate(leaf = gsub(paste0("R", i, "_"), "", leaf))  # Supprimer le préfixe R1_, R2_, ...
    
    merged_data_i <- left_join(data_Li, data_Ri, by = c("rhizotron", "species", "condition", "date", "leaf", "thermal.time", "humidity"), suffix = c("_Li", "_Ri"))
    
    data_avg_i <- merged_data_i %>%
      mutate(
        avg_area = (area_Li + area_Ri) / 2,
        avg_area_growth = (area_growth_Li + area_growth_Ri) / 2
      )
  } else {
    
    # Cas avec une seule plante (par exemple, L10, R10 n'existe pas)
    data_avg_i <- data_i %>%
      mutate(
        avg_area = area,
        avg_area_growth = area_growth
      )
  }
  
  data_avg_tot_i <- data_avg_i %>%
    select(date, thermal.time, condition, avg_area, avg_area_growth, leaf, humidity) %>%
    group_by(date, thermal.time, condition, humidity) %>%
    summarise(
      tot_area = sum(avg_area, na.rm = TRUE),
      tot_area_growth = sum(avg_area_growth, na.rm = TRUE),
      .groups = "drop"
    )

  data_avg_tot_i$rhizotron <- i
  final_data_avg_sorg_area[[i]] <- data_avg_tot_i
}

final_data_avg_sorg_area <- bind_rows(final_data_avg_sorg_area)

humidity_avg_ws <- final_data_avg_sorg_area %>%
  filter(condition == "ws",humidity != 0) %>%
  group_by(thermal.time) %>%
  summarise(humidity_ws = mean(humidity, na.rm = TRUE))

final_data_avg_sorg_area <- final_data_avg_sorg_area %>%
  left_join(humidity_avg_ws, by = "thermal.time")

# for height analysis
final_data_avg_sorg_height <- list()

for (i in 9:16) {
  data_i <- data_sorg_height %>% filter(rhizotron == i)
  
  plants_in_rhizotron <- unique(data_i$plant)
  
  if (length(plants_in_rhizotron) == 2) {
    # Cas normal avec deux plantes
    data_Li <- data_i %>% filter(plant == paste0("L", i))
    data_Ri <- data_i %>% filter(plant == paste0("R", i))
    
    data_Li <- data_Li %>%
      mutate(leaf = gsub(paste0("L", i, "_"), "", leaf))  # Supprimer le préfixe L1_, L2_, ...
    data_Ri <- data_Ri %>%
      mutate(leaf = gsub(paste0("R", i, "_"), "", leaf))  # Supprimer le préfixe R1_, R2_, ...
    
    merged_data_i <- left_join(data_Li, data_Ri, by = c("rhizotron", "species", "condition", "date", "leaf", "thermal.time", "humidity"), suffix = c("_Li", "_Ri"))
    
    data_avg_i <- merged_data_i %>%
      mutate(
        avg_height = (height_Li + height_Ri) / 2,
        avg_height_growth = (height_growth_Li + height_growth_Ri) / 2
      )
  } else {
    
    # Cas avec une seule plante (par exemple, L10, R10 n'existe pas)
    data_avg_i <- data_i %>%
      mutate(
        avg_height = height,
        avg_height_growth = height_growth
      )
  }
  
  data_avg_tot_i <- data_avg_i %>%
    select(date, thermal.time, condition, avg_height, avg_height_growth, leaf, humidity) %>%
    group_by(date, thermal.time, condition, humidity) %>%
    summarise(
      tot_height = sum(avg_height, na.rm = TRUE),
      tot_height_growth = sum(avg_height_growth, na.rm = TRUE),
      .groups = "drop"
    )
  
  data_avg_tot_i$rhizotron <- i
  final_data_avg_sorg_height[[i]] <- data_avg_tot_i
}

final_data_avg_sorg_height <- bind_rows(final_data_avg_sorg_height)

humidity_avg_ws <- final_data_avg_sorg_height %>%
  filter(condition == "ws",humidity != 0) %>%
  group_by(thermal.time) %>%
  summarise(humidity_ws = mean(humidity, na.rm = TRUE))

final_data_avg_sorg_height <- final_data_avg_sorg_height %>%
  left_join(humidity_avg_ws, by = "thermal.time")


ggplot(final_data_avg_sorg_area, aes(x = thermal.time, y = tot_area, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  geom_line(aes(y = humidity_ws*100), size = 1, color = "blue") +
  labs(
    title = "Total average area evolution",
    x = "Date",
      y = "Total average area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average area (cm²)", sec.axis = sec_axis(~ . / 100, name = "WS average humidity")
  )

ggplot(final_data_avg_sorg_height, aes(x = thermal.time, y = tot_height, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  geom_line(aes(y = humidity_ws*100), size = 1, color = "blue") +
  labs(
    title = "Total average height evolution",
    x = "Date",
    y = "Total average height (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average height (cm)", sec.axis = sec_axis(~ . / 100, name = "WS average humidity")
  )

ggplot(final_data_avg_sorg_area, aes(x = thermal.time, y = tot_area_growth, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  geom_line(aes(y = humidity_ws*10), size = 1, color = "blue") +
  labs(
    title = "Total average area growth",
    x = "Date",
    y = "Total average area growth (cm²/dd)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average area growth (cm²/dd)", sec.axis = sec_axis(~ . / 10, name = "WS average humidity")
  )

ggplot(final_data_avg_sorg_height, aes(x = thermal.time, y = tot_height_growth, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  geom_line(aes(y = humidity_ws*10), size = 1, color = "blue") +
  labs(
    title = "Total average height growth",
    x = "Date",
    y = "Total average height growth (cm/dd)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average height growth (cm/dd)", sec.axis = sec_axis(~ . / 10, name = "WS average humidity")
  )

sorg_avg_area_growth <- final_data_avg_sorg_area %>%
  group_by(condition, thermal.time, humidity_ws) %>%
  summarise(sorg_area_growth = mean(tot_area_growth))

sorg_avg_height_growth <- final_data_avg_sorg_height %>%
  group_by(condition, thermal.time, humidity_ws) %>%
  summarise(sorg_height_growth = mean(tot_height_growth))

sorg_avg_area <- final_data_avg_sorg_area %>%
  group_by(condition, thermal.time, humidity_ws) %>%
  summarise(sorg_area = mean(tot_area))

sorg_avg_height <- final_data_avg_sorg_height %>%
  group_by(condition, thermal.time, humidity_ws) %>%
  summarise(sorg_height = mean(tot_height))

ggplot(sorg_avg_area, aes(x = thermal.time, y = sorg_area, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*100), size = 1, color = "blue") +
  labs(
    title = "Total average sorghum leaves area and humidity evolution",
    x = "Date",
    y = "Total average area (cm²)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average area (cm²)", sec.axis = sec_axis(~ . / 100, name = "WS average humidity")
  )

ggplot(sorg_avg_height, aes(x = thermal.time, y = sorg_height, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*100), size = 1, color = "blue") +
  labs(
    title = "Total average sorghum leaves height and humidity evolution",
    x = "Date",
    y = "Total average height (cm)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average height (cm)", sec.axis = sec_axis(~ . / 100, name = "WS average humidity")
  )

ggplot(sorg_avg_area_growth, aes(x = thermal.time, y = sorg_area_growth, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*10), size = 1, color = "blue") +
  labs(
    title = "Total average height growth",
    x = "Date",
    y = "Total average height growth (cm/dd)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average height growth (cm/dd)", sec.axis = sec_axis(~ . / 10, name = "WS average humidity")
  )

ggplot(sorg_avg_height_growth, aes(x = thermal.time, y = sorg_height_growth, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  geom_line(aes(y = humidity_ws*10), size = 1, color = "blue") +
  labs(
    title = "Total average height growth",
    x = "Date",
    y = "Total average height growth (cm/dd)",
    color = "Condition"
  )+
  scale_y_continuous(name = "Total average height growth (cm/dd)", sec.axis = sec_axis(~ . / 10, name = "WS average humidity")
  )

# demander si il faut faire lm sur tout ou que sur les croissances ?
# mettre bien les légendes et mettre sur diapo
# quels tests statistiques faire ? faire ? est ce qu'on peut comparer le mais et le sorgho ?
