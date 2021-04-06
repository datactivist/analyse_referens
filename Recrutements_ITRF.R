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
  mutate(Id = row_number()) %>%
  mutate(année = factor(année, levels=seq(2017,2021)))
names(postes) #noms de colonnes

str(postes)

postes_filtered <- postes %>%
  semi_join(metiers_donnee, by = c("EMPLOIS.TYPES" = "referens_intitule"))

chronologie <- postes_filtered %>% 
  group_by(année) %>%
  summarise(somme =sum(NOMBRE.d.emplois))

chronologie %>%
  ggplot(aes(x = année, y = somme, label=somme)) +
  geom_point() +
  geom_text(aes(label=somme), hjust=0, vjust=-1) +
  scale_y_continuous(limits=c(0,40)) +
  scale_x_discrete(drop=FALSE) +
  labs(x="Année", y="Nombre de postes ouverts au concours BIATSS",
       title="Evolution des recrutements de titulaires\n pour 13 métiers des données",
       caption="Données 2019 manquantes\n Données P.E. Turgo – Analyse CC-BY Antoine Blanchard / Datactivist") +
  theme_ipsum()
