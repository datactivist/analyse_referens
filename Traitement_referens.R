library(tidyr)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(forcats)
library(tm)
library(RColorBrewer)

data <- read.csv2("fr-esr_referentiel_metier_referens_3.csv") %>%
  mutate(Id = row_number())
names(data) #noms de colonnes

donnees <- "Donnees|donnees|Données|données|Donnee|donnee|Donnée|donnée"

variables <- data %>%
  select(-Id) %>%
  colnames()

denombrement <- data %>%
  column_to_rownames("Id") %>%
  #select(Nom des colonnes, séparées par une virgule) %>%
  mutate_all(str_count, donnees) %>%
  mutate(somme = rowSums(.)) %>%
  rownames_to_column("Id") %>%
  filter(somme > 0) %>%
  arrange(desc(somme))
  
data_filtered <- data %>%
  filter(Id %in% denombrement$Id) %>%
  mutate(Id = as.character(Id)) %>%
  left_join(denombrement %>% select(Id, somme), by = "Id") %>%
  arrange(desc(somme))

# Classer les corps dans l'ordre hiérarchique
ordre_corps <- c("ATRF", "TECH", "AI", "IE", "IR")

data_filtered %>%
  mutate(referens_id = fct_reorder(referens_id, -somme),
         referens_cs=factor(referens_cs, levels = ordre_corps)) %>%
  ggplot(aes(x = referens_id, y = somme, color=referens_cs)) +
  geom_point() +
  labs(y = "Nombre d'occurrences", color="Corps") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Histogramme des variables où apparaissent le plus l'expression "données"
  
denombrement %>%
  select(-somme) %>%
  column_to_rownames("Id") %>%
  summarise_all(sum) %>%
  rownames_to_column("Id") %>%
  gather("variable", "somme", -Id) %>%
  filter(somme > 0) %>%
  ggplot(aes(x=fct_reorder(variable, somme), y = somme)) +
  geom_col(aes(fill = variable), color = "grey50") +
  labs(x = "", y = "Fréquence")+
  coord_flip()+
  theme_classic() +
  theme(legend.position = "none")

# Histogramme des BAP où apparaissent le plus l'expression "données"

BAP_filtered <- data_filtered %>%
  count(bap) %>%
  arrange(bap)

BAP <- data %>%
  count(bap)

BAP_filtered_pourcentage <- BAP_filtered %>%
  left_join(BAP, BAP_filtered, by = "bap") %>%
  mutate(pourcentage_bap=round(n.x/n.y,2),
         bap=fct_reorder(bap, bap, .desc = TRUE))

BAP_filtered_pourcentage %>%
  ggplot(aes(x = bap, y = pourcentage_bap)) +
  geom_col(aes(fill = bap), color = "grey50") +
  labs(x = "", y = "Proportion de fiches métiers contenant ''données'' par BAP") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_manual(values = brewer.pal(8, "YlOrRd")) +
  scale_y_continuous(labels = scales::percent)

# Histogramme des corps où apparaissent le plus l'expression "données"

corps_filtered <- data_filtered %>%
  count(referens_cs) %>%
  arrange(desc(n))

corps <- data %>%
  count(referens_cs)

corps_filtered_pourcentage <- corps_filtered %>%
  left_join(corps, corps_filtered, by = "referens_cs") %>%
  mutate(pourcentage_corps=round(n.x/n.y,2),
         referens_cs=factor(referens_cs, levels = ordre_corps))

corps_filtered_pourcentage %>%
  ggplot(aes(x = referens_cs, y = pourcentage_corps)) +
  geom_col(aes(fill = referens_cs), color = "grey50") +
  labs(x = "", y = "Proportion de fiches métiers contenant ''données'' par corps") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))

# Tableau des 11 métiers dont l'intitulé ou le métier contient l'expression "données"
  
intitule_TRUE <- denombrement %>%
  filter(denombrement$referens_intitule >0 | denombrement$referens_metiers >0)

intitule_filtered <- data_filtered %>%
  filter(Id %in% intitule_TRUE$Id) %>%
  mutate(Id = as.character(Id)) %>%
  left_join(data_filtered %>% select(Id, somme), by = "Id") %>%
  select(referens_intitule, referens_metiers, bap, referens_fap, referens_cs, Id) %>%
  group_by(bap) %>%
  arrange(referens_cs, .by_group = TRUE)

View(intitule_filtered)

## Regarder les métiers qui concentrent le plus le terme "données"

data_filtered %>%
  filter(somme > 10) %>%
  arrange(desc(somme)) %>%
  select (referens_intitule, referens_metiers, bap, referens_fap, referens_cs, somme) %>%
  head(15)

## Regarder les métiers où "données" apparaît dans facteur_d_evolution_moyen_terme

evolution_metiers <- data %>%
  filter(Id %in% denombrement$Id) %>%
  filter(denombrement$referens_facteurs_d_evolution_moyen_terme >0) %>%
  mutate(Id = as.character(Id)) %>%
  left_join(denombrement %>% select(Id, somme), by = "Id")

# Il y a un problème avec la ligne "Id = 34" qui apparaît alors que denombrement[34,"referens_facteurs_d_evolution_moyen_terme"] == NULL 0

## Regarder les métiers où le terme données apparaît comme "intitulé précédent" et a disparu de l'intitulé actuel 

suppression_metiers <- data %>%
  filter(Id %in% denombrement$Id) %>%
  filter(denombrement, referens_intitule_precedent > 0) %>%
  filter(denombrement, referens_intitule == 0) %>%
  mutate(Id = as.character(Id)) %>%
  left_join(denombrement %>% select(Id, somme), by = "Id") %>%
  arrange(desc(somme)) %>%
  select(referens_intitule, bap, referens_fap, referens_facteurs_d_evolution_moyen_terme, referens_cs) %>%
  group_by(bap) %>%
  arrange(referens_cs, .by_group = TRUE)

kable(evolution_metiers, caption = "Métiers dont l'ancien intitulé incluait le terme 'données' mais pas l'intitulé actuel")

## boxplot (quartiles) de la distribution des fiches métiers qui concentrent le plus le terme 'données" par corps

data_filtered %>%
  mutate(referens_cs=factor(referens_cs, levels = ordre_corps)) %>%
  ggplot(aes(x = referens_cs, y = somme, color=referens_cs)) +
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter (0.2)) +
  labs(x= "Corps", y = "Nombre d'occurrences", color="Corps")+
  theme(legend.position = "none")
## Afficher le terme "données" en kwic (keyword-in-context) et représenter son voisinnage lexical