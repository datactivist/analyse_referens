data_filtered %>%
  mutate(referens_id = fct_reorder(referens_id, -somme),
         referens_cs=factor(referens_cs, levels = ordre_corps)) %>%
  ggplot(aes(x = referens_id, y = somme, color=referens_cs, label=referens_intitule)) +
  geom_point() +
  geom_text_repel(size=2.5,box.padding=0.5,max.overlaps = Inf,force=8, aes(label=ifelse(somme>=17,referens_intitule,'')), show.legend=FALSE,
                  nudge_x           = 0.2,
                  direction         = "both",
                  segment.size      = 0.2,
                  segment.curvature = -0.1,
                  segment.inflect=FALSE) +
  labs(x="Rang", y="Nombre d'occurrences du terme ''données''", color="Corps",
       title="Distribution des fiches métiers",
       caption="Données MESRI – Analyse CC-BY Antoine Blanchard / Datactivist") +
  theme_ipsum() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

data_filtered %>%
  mutate(referens_id = fct_reorder(referens_id, -somme),
         referens_cs=factor(referens_cs, levels = ordre_corps)) %>%
  ggplot(aes(x = referens_id, y = somme, xend=180,color=referens_cs, label=referens_intitule)) +
  geom_point() +
  geom_text_repel(size=2.5,
                  box.padding=0.8,
                  max.overlaps = Inf,
                  aes(label=ifelse(somme>=17,referens_intitule,'')),
                  show.legend=FALSE,
                  nudge_y           = 1.6,
                  nudge_x=0,
                  direction         = "x"
) +
  labs(x="Rang", y="Nombre d'occurrences du terme ''données''", color="Corps",
       title="Distribution des fiches métiers",
       caption="Données MESRI – Analyse CC-BY Antoine Blanchard / Datactivist") +
  theme_ipsum() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
