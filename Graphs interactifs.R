library(ggiraph)

distribution_interactive <- distribution_corps_avec_intitule + geom_point_interactive(aes(tooltip = referens_intitule), size = 2)
girafe(code = print(distribution_interactive))