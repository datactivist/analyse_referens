library(ggiraph)


```{r distribution_interactive, results ='asis'}
distribution_interactive <- distribution_corps_avec_intitule + geom_point_interactive(aes(tooltip = referens_intitule), size = 2)
girafe(code = print(distribution_interactive))
```


```{r dispersion_interactive, results ='asis'}
dispersion_interactive <- dispersion_corps + geom_point_interactive(aes(tooltip = referens_intitule), size = 2)
girafe(code = print(dispersion_interactive))
```