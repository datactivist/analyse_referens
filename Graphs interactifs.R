library(ggiraph)

# Il convient d'exécuter le fichier Notebook.rmd avant d'exécuter ce script, qui produit des figures interactives grâce à ggiraph

# Distribution des fiches métiers en fonction du nombre d'occurrences du terme "données"
distribution_interactive <- distribution_corps_avec_intitule %>%
  ggplot() + 
  geom_point_interactive(aes(tooltip = referens_intitule), size = 2)
girafe(code = print(distribution_interactive))

# Dispersion des fiches métiers autour de la moyenne par BAP
dispersion_interactive <- dispersion_corps + geom_point_interactive(aes(tooltip = referens_intitule), size = 2)
girafe(code = print(dispersion_interactive))


distribution_interactive = ggplot(data = distribution_corps_avec_intitule,
                                  mapping = aes(x = referens_id, y = somme, color=referens_cs, tooltip = referens_intitule)) +
                                    geom_point_interactive(size = 2) +
labs(x="Rang", y="Nombre d'occurrences du terme \"données\"", color="Corps",
     title="Distribution des fiches métiers",
     caption="Données MESRI – Analyse CC-BY Antoine Blanchard / Datactivist") +
  scale_color_hue(labels = c("ATRF (Cat. C)", "TECH (Cat. B)", "AI (Cat. A)", "IE (Cat. A)", "IR (Cat. A)")) +
  theme_ipsum() +
  theme_ipsum() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank())
girafe(code = print(distribution_interactive))
