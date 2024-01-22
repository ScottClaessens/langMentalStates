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

# pivot data wider
pivotDataWider <- function(d) {
  d %>%
    group_by(word) %>%
    summarise(
      # english or tongan
      language = unique(language),
      # number of definitions with mental states
      mental_state = sum(mental_state),
      BK = sum(BK),
      DW = sum(DW),
      IN = sum(IN),
      PE = sum(PE),
      EM = sum(EM),
      AR = sum(AR),
      OT = sum(OT),
      # total number of definitions
      n = n(),
      .groups = "drop"
    )
}

# fit model 1 (no controls)
fitModel1 <- function(dWide, outcome) {
  brm(
    formula = bf(
      paste0(outcome, " | trials(n) ~ 0 + language"),
      "phi ~ 0 + language"
      ),
    data = dWide,
    family = beta_binomial,
    prior = c(
      # priors based on prior predictive check
      prior(normal(-2, 1.0), class = b, coef = languageeng),
      prior(normal(-2, 1.0), class = b, coef = languageton),
      prior(normal(-1, 0.5), class = b, coef = languageeng, dpar = phi),
      prior(normal(-1, 0.5), class = b, coef = languageton, dpar = phi)
    ),
    sample_prior = "yes",
    cores = 4
  )
}
