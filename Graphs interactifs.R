library(ggiraph)

# Il convient d'exécuter le fichier Notebook.rmd avant d'exécuter ce script, qui produit des figures interactives grâce à ggiraph

# Distribution des fiches métiers en fonction du nombre d'occurrences du terme "données"
distribution_interactive <- distribution_corps_avec_intitule + geom_point_interactive(aes(tooltip = referens_intitule), size = 2)
girafe(code = print(distribution_interactive))

# Dispersion des fiches métiers autour de la moyenne par BAP
dispersion_interactive <- dispersion_corps + geom_point_interactive(aes(tooltip = referens_intitule), size = 2)
girafe(code = print(dispersion_interactive))