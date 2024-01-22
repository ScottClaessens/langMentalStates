# custom functions

# plot proportions of definitions
plotProportions <- function(d) {
  # plot
  out <-
    d %>%
    group_by(language) %>%
    summarise(
      Overall              = mean(mental_state),
      `Belief/\nKnowledge` = mean(BK),
      `Desire/\nWish`      = mean(DW),
      Intention            = mean(IN),
      Perception           = mean(PE),
      Emotion              = mean(EM),
      Arousal              = mean(AR),
      Other                = mean(OT)
    ) %>%
    pivot_longer(cols = !language) %>%
    ggplot(aes(x = fct_reorder2(name, language, value), y = value,
               fill = language)) +
    geom_col(position = "dodge", colour = "black") +
    scale_fill_manual(
      name = "Language",
      labels = c("English","Tongan"),
      values = c("gray25","white")
      ) +
    labs(
      x = "Mental state class",
      y = "Proportion of definitions"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  # save
  ggsave(out, filename = "plots/proportions.pdf", width = 6, height = 4)
  return(out)
}
