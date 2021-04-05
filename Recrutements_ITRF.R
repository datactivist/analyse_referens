library(tidyr)
library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(ggplot2)
library(forcats)
library(tm)
library(RColorBrewer)

# Importation des données issues de https://framagit.org/FalaF/data-itrf

postes <- read.csv2("liste-postes.csv", na.strings=c('_','nc')) %>%
  mutate(Id = row_number())
names(postes) #noms de colonnes

str(postes)

postes_filtered_intitule <- postes %>%
  semi_join(intitule_filtered, by = c("EMPLOIS.TYPES" = "referens_intitule"))

postes_filtered_densite <- postes %>%
  semi_join(top_metiers, by = c("EMPLOIS.TYPES" = "referens_intitule"))

postes_filtered <- union(postes_filtered_densite,postes_filtered_intitule)

intitule_filtered <- intitule_filtered %>% select(Id)

top_metiers <- top_metiers %>% select(Id)

metiers_donnee_uniques <- union(intitule_filtered,top_metiers)

chronologie <- postes_filtered %>% 
  group_by(année) %>%
  summarise(somme =sum(NOMBRE.d.emplois))

chronologie %>%
  ggplot(aes(x = année, y = somme)) +
  geom_point() +
  scale_y_continuous(limits=c(0,40)) +
  labs(x="Année", y="Nombre de postes ouverts au concours BIATSS",
       title="Evolution des recrutements de titulaires\n pour 14 métiers des données",
       caption="Données P.E. Turgo – Visualisation CC-BY Antoine Blanchard / Datactivist") +
  theme_ipsum()
