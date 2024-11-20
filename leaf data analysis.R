
library(ggplot2)
library(dplyr)

data <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/datauntil1511.csv", dec=",")
data <- data %>% select(-Lenght.of.the.leaf,-Width.of.the.leaf, -Coef)
data$date <- as.Date(data$date, format = "%d/%m")

thermaltime <- read.csv("C:/Users/Emilie/Desktop/projet JRL/data analysis/thermaltimeuntil1511.csv", dec=",")
thermaltime$date <- as.Date(thermaltime$date, format = "%d/%m")

data <- merge(thermaltime, data, by = "date")

# we will work on this dataset for area growth and height growth
data <- data %>%
  group_by(plant, leaf) %>%
  arrange(date) %>%
  mutate(area_growth = (area - lag(area)) / as.numeric(date - lag(date))) %>%
  filter(!is.na(area_growth)) %>%
  mutate(height_growth = (height - lag(height)) / as.numeric(date - lag(date))) %>%
  filter(!is.na(height_growth))


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

data_L1_avg_area <- data_L1 %>%
  group_by(date, thermal.time) %>%
  summarise(avg_area = mean(area))

ggplot(data_L1_avg_area, aes(x = thermal.time, y = avg_area)) +
  geom_line(size = 1)+
  geom_point(size = 2) +               # Points pour chaque mesure
  labs(
    title = "Leaf area evolution of L1",
    x = "thermal time",
    y = "Area"
  ) +
  theme_minimal()                   # Thème data_L1_avg_height <- data_L1 %>%

data_L1_avg_height <- data_L1 %>%
  group_by(date, thermal.time) %>%
  summarise(avg_height = mean(height))
  
ggplot(data_L1_avg_height, aes(x = thermal.time, y = avg_height)) +
  geom_line(size = 1) +
geom_point(size = 2) +               # Points pour chaque mesure
  labs(
    title = "Leaf heigth evolution of L1",
    x = "thermal time",
    y = "Height"
  ) +
  theme_minimal()  

data_L1_avg_area_growth <- data_L1 %>%
  group_by(date, thermal.time) %>%
  summarise(avg_area_growth = mean(area_growth))

ggplot(data_L1_avg_area_growth, aes(x = thermal.time, y = avg_area_growth)) +
  geom_line() +
  labs(title = "L1 area growth speed",
       x = "thermal time (degree days)", y = "area growth speed") +
  theme_minimal()

# don't know if usefull ?
modele_L1_avg_area_growth <- lm(avg_area_growth ~ thermal.time, data = data_L1_avg_area_growth)
data_L1_avg_area_growth$avg_area_growth_predicted <- predict(modele_L1_avg_area_growth, newdata = data_L1_avg_area_growth)
ggplot(data_L1_avg_area_growth, aes(x = thermal.time, y = avg_area_growth)) +
  geom_point() +
  geom_line(aes(y = avg_area_growth_predicted), color = "blue") +
  labs(title = "fitted area growth speed",
       x = "thermal time", y = "area growth speed (cm²/dd)") +
  theme_minimal()

data_L1_avg_height_growth <- data_L1 %>%
  group_by(date, thermal.time) %>%
  summarise(avg_height_growth = mean(height_growth))

ggplot(data_L1_avg_height_growth, aes(x = thermal.time, y = avg_height_growth)) +
  geom_line() +
  labs(title = "L1 height growth speed",
       x = "thermal time (degree days)", y = "height growth speed") +
  theme_minimal()

# don't know if usefull ?
modele_L1_avg_height_growth <- lm(avg_height_growth ~ thermal.time, data = data_L1_avg_height_growth)
data_L1_avg_height_growth$avg_height_growth_predicted <- predict(modele_L1_avg_height_growth, newdata = data_L1_avg_height_growth)
ggplot(data_L1_avg_height_growth, aes(x = thermal.time, y = avg_height_growth)) +
  geom_point() +
  geom_line(aes(y = avg_height_growth_predicted), color = "blue") +
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

data_avg_1 <- data_avg_1 %>% select(avg_area, avg_height, avg_area_growth, avg_height_growth)
data_avg_1$date <- data_L1$date
data_avg_1$thermal.time <- data_L1$thermal.time

data_avg_1_area_growth <- data_avg_1 %>%
  group_by(date, thermal.time) %>%
  summarise(avg_1_area_growth = mean(avg_area_growth))

data_avg_1_height_growth <- data_avg_1 %>%
  group_by(date, thermal.time) %>%
  summarise(avg_1_height_growth = mean(avg_height_growth))

data_avg_1_area <- data_avg_1 %>%
  group_by(date, thermal.time) %>%
  summarise(avg_1_area = mean(avg_area))

data_avg_1_height <- data_avg_1 %>%
  group_by(date, thermal.time) %>%
  summarise(avg_1_height = mean(avg_height))

ggplot(data_avg_1_area, aes(x = thermal.time, y = avg_1_area)) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "Rhizotron 1 average leaf area evolution", x = "thermal time",y = "Area (cm²)")

ggplot(data_avg_1_height, aes(x = thermal.time, y = avg_1_height)) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "Rhizotron 1 average leaf height evolution", x = "thermal time",y = "Area (cm²)")

ggplot(data_avg_1_area_growth, aes(x = thermal.time, y = avg_1_area_growth)) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "Rhizotron 1 average area growth", x = "thermal time",y = "Area growth speed (cm²/dd)")

ggplot(data_avg_1_height_growth, aes(x = thermal.time, y = avg_1_height_growth)) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "Rhizotron 1 average height growth", x = "thermal time",y = "Area growth speed (cm²/dd)")

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

  merged_data_i <- left_join(data_Li, data_Ri, by = c("rhizotron", "species", "condition", "date", "leaf", "thermal.time"), suffix = c("_Li", "_Ri"))

  data_avg_i <- merged_data_i %>%
    mutate(
      avg_area = (area_Li + area_Ri) / 2,
      avg_height = (height_Li + height_Ri) / 2,
      avg_area_growth = (area_growth_Li + area_growth_Ri) / 2,
      avg_height_growth = (height_growth_Li + height_growth_Ri) / 2
    )
  data_avg_i <- data_avg_i %>%
    select(date, thermal.time, condition, avg_area, avg_height, avg_area_growth, avg_height_growth) %>%
    group_by(date, thermal.time, condition) %>%
    summarise(
      avg_area = mean(avg_area, na.rm = TRUE),
      avg_height = mean(avg_height, na.rm = TRUE),
      avg_area_growth = mean(avg_area_growth, na.rm = TRUE),
      avg_height_growth = mean(avg_height_growth, na.rm = TRUE)
    )
  data_avg_i$rhizotron <- i
  final_data_avg_maize[[i]] <- data_avg_i
}

final_data_avg_maize <- bind_rows(final_data_avg_maize)

ggplot(final_data_avg_maize, aes(x = thermal.time, y = avg_area, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  labs(
    title = "Évolution de l'aire moyenne totale par rhizotron",
    x = "Date",
    y = "Aire moyenne totale (cm²)",
    color = "Condition"
  )

ggplot(final_data_avg_maize, aes(x = thermal.time, y = avg_height, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  labs(
    title = "Évolution de l'aire moyenne totale par rhizotron",
    x = "Date",
    y = "Aire moyenne totale (cm²)",
    color = "Condition"
  )

ggplot(final_data_avg_maize, aes(x = thermal.time, y = avg_area_growth, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  labs(
    title = "Évolution de l'aire moyenne totale par rhizotron",
    x = "Date",
    y = "Aire moyenne totale (cm²)",
    color = "Condition"
  )

ggplot(final_data_avg_maize, aes(x = thermal.time, y = avg_height_growth, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  labs(
    title = "Évolution de l'aire moyenne totale par rhizotron",
    x = "Date",
    y = "Aire moyenne totale (cm²)",
    color = "Condition"
  )

maize_avg_area_growth <- final_data_avg_maize %>%
  group_by(condition, thermal.time) %>%
  summarise(maize_area_growth = mean(avg_area_growth))

maize_avg_height_growth <- final_data_avg_maize %>%
  group_by(condition, thermal.time) %>%
  summarise(maize_height_growth = mean(avg_height_growth))

maize_avg_area <- final_data_avg_maize %>%
  group_by(condition, thermal.time) %>%
  summarise(maize_area = mean(avg_area))

maize_avg_height <- final_data_avg_maize %>%
  group_by(condition, thermal.time) %>%
  summarise(maize_height = mean(avg_height))

ggplot(maize_avg_area, aes(x = thermal.time, y = maize_area, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "maize average leaf area evolution", x = "thermal time",y = "Area (cm²)")

ggplot(maize_avg_height, aes(x = thermal.time, y = maize_height, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "maize average leaf height evolution", x = "thermal time",y = "height (cm)")

ggplot(maize_avg_area_growth, aes(x = thermal.time, y = maize_area_growth, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "maize average leaf height growth", x = "thermal time",y = "Area growth (cm²)")

ggplot(maize_avg_height_growth, aes(x = thermal.time, y = maize_height_growth, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "maize average leaf height growth", x = "thermal time",y = "height growth (cm²)")


### work on sorghum (rhizotron 9 to 16)

## creating all averages of each rhizotron

final_data_avg_sorg <- list()

for (i in 9:16) {
  data_i <- data %>% filter(rhizotron == i)
  
  plants_in_rhizotron <- unique(data_i$plant)
  
  if (length(plants_in_rhizotron) == 2) {
    # Cas normal avec deux plantes
    data_Li <- data_i %>% filter(plant == paste0("L", i))
    data_Ri <- data_i %>% filter(plant == paste0("R", i))
    
    data_Li <- data_Li %>%
      mutate(leaf = gsub(paste0("L", i, "_"), "", leaf))  # Supprimer le préfixe L1_, L2_, ...
    data_Ri <- data_Ri %>%
      mutate(leaf = gsub(paste0("R", i, "_"), "", leaf))  # Supprimer le préfixe R1_, R2_, ...
    
    merged_data_i <- left_join(data_Li, data_Ri, by = c("rhizotron", "species", "condition", "date", "leaf", "thermal.time"), suffix = c("_Li", "_Ri"))
    
    data_avg_i <- merged_data_i %>%
      mutate(
        avg_area = (area_Li + area_Ri) / 2,
        avg_height = (height_Li + height_Ri) / 2,
        avg_area_growth = (area_growth_Li + area_growth_Ri) / 2,
        avg_height_growth = (height_growth_Li + height_growth_Ri) / 2
      )
  } else {
    
    # Cas avec une seule plante (par exemple, L10, R10 n'existe pas)
    data_avg_i <- data_i %>%
      mutate(
        avg_area = area,
        avg_height = height,
        avg_area_growth = area_growth,
        avg_height_growth = height_growth
      )
  }
  
  data_avg_i <- data_avg_i %>%
    select(date, thermal.time, condition, avg_area, avg_height, avg_area_growth, avg_height_growth) %>%
    group_by(date, thermal.time, condition) %>%
    summarise(
      avg_area = mean(avg_area, na.rm = TRUE),
      avg_height = mean(avg_height, na.rm = TRUE),
      avg_area_growth = mean(avg_area_growth, na.rm = TRUE),
      avg_height_growth = mean(avg_height_growth, na.rm = TRUE),
      .groups = "drop"
    )

  data_avg_i$rhizotron <- i
  final_data_avg_sorg[[i]] <- data_avg_i
}

final_data_avg_sorg <- bind_rows(final_data_avg_sorg)

ggplot(final_data_avg_sorg, aes(x = thermal.time, y = avg_area, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  labs(
    title = "Évolution de l'aire moyenne totale par rhizotron",
    x = "Date",
    y = "Aire moyenne totale (cm²)",
    color = "Condition"
  )

ggplot(final_data_avg_sorg, aes(x = thermal.time, y = avg_height, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  labs(
    title = "Évolution de l'aire moyenne totale par rhizotron",
    x = "Date",
    y = "Aire moyenne totale (cm²)",
    color = "Condition"
  )

ggplot(final_data_avg_sorg, aes(x = thermal.time, y = avg_area_growth, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  labs(
    title = "Évolution de l'aire moyenne totale par rhizotron",
    x = "Date",
    y = "Aire moyenne totale (cm²)",
    color = "Condition"
  )

ggplot(final_data_avg_sorg, aes(x = thermal.time, y = avg_height_growth, color = condition, group = rhizotron)) +
  geom_line(size = 1) +                   # Ajouter des lignes pour chaque rhizotron
  geom_point(size = 2) +                  # Ajouter des points pour chaque date
  labs(
    title = "Évolution de l'aire moyenne totale par rhizotron",
    x = "Date",
    y = "Aire moyenne totale (cm²)",
    color = "Condition"
  )

sorg_avg_area_growth <- final_data_avg_sorg %>%
  group_by(condition, thermal.time) %>%
  summarise(sorg_area_growth = mean(avg_area_growth))

sorg_avg_height_growth <- final_data_avg_sorg %>%
  group_by(condition, thermal.time) %>%
  summarise(sorg_height_growth = mean(avg_height_growth))

sorg_avg_area <- final_data_avg_sorg %>%
  group_by(condition, thermal.time) %>%
  summarise(sorg_area = mean(avg_area))

sorg_avg_height <- final_data_avg_sorg %>%
  group_by(condition, thermal.time) %>%
  summarise(sorg_height = mean(avg_height))

ggplot(sorg_avg_area, aes(x = thermal.time, y = sorg_area, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "sorg average leaf area evolution", x = "thermal time",y = "Area (cm²)")

ggplot(sorg_avg_height, aes(x = thermal.time, y = sorg_height, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "sorg average leaf height evolution", x = "thermal time",y = "height (cm²)")

ggplot(sorg_avg_area_growth, aes(x = thermal.time, y = sorg_area_growth, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "sorg average leaf area evolution growth")
ggplot(sorg_avg_height_growth, aes(x = thermal.time, y = sorg_height_growth, color=condition, group_by(condition))) +
  geom_line(size = 1) +  # Tracer la courbe de la moyenne
  geom_point(size = 2) +  # Ajouter des points pour chaque mesure
  labs(title = "sorg average leaf height growth", x = "thermal time",y = "height growth (cm)")
