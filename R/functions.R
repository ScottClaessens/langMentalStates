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
      # word class proportions
      noun       = mean(noun,       na.rm = TRUE),
      adjective  = mean(adjective,  na.rm = TRUE),
      adverb     = mean(adverb,     na.rm = TRUE),
      verb       = mean(verb,       na.rm = TRUE),
      comb_other = mean(comb_other, na.rm = TRUE),
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
      prior(normal(-2, 1), class = b, coef = languageeng),
      prior(normal(-2, 1), class = b, coef = languageton),
      prior(normal(-1, 0.5), class = b, coef = languageeng, dpar = phi),
      prior(normal(-1, 0.5), class = b, coef = languageton, dpar = phi)
    ),
    sample_prior = "yes",
    cores = 4
  )
}

# fit model 2 (with word class controls)
fitModel2 <- function(dWide, outcome) {
  brm(
    formula = bf(
      paste0(outcome, " | trials(n) ~ 0 + language",
             " + noun + adjective + adverb + verb"),
      "phi ~ 0 + language"
    ),
    data = dWide,
    family = beta_binomial,
    prior = c(
      # priors based on prior predictive check
      prior(normal(-2, 1), class = b, coef = languageeng),
      prior(normal(-2, 1), class = b, coef = languageton),
      prior(normal(0, 1), class = b, coef = noun),
      prior(normal(0, 1), class = b, coef = adjective),
      prior(normal(0, 1), class = b, coef = adverb),
      prior(normal(0, 1), class = b, coef = verb),
      prior(normal(-1, 0.5), class = b, coef = languageeng, dpar = phi),
      prior(normal(-1, 0.5), class = b, coef = languageton, dpar = phi)
    ),
    sample_prior = "yes",
    cores = 4
  )
}

# plot results from models
plotModel <- function(hyp, filename) {
  # extract posterior log odds differences
  post <-
    lapply(hyp, function(x) x$samples[,1]) %>%
    as_tibble()
  # labels for plot
  colnames(post) <- c(
    "Overall",
    "Belief/\nKnowledge",
    "Desire/\nWish",
    "Intention",
    "Perception",
    "Emotion",
    "Arousal",
    "Other"
  )
  out <-
    post %>%
    pivot_longer(everything()) %>%
    mutate(
      # correct ordering for plot
      name = factor(name, levels = c("Overall", "Belief/\nKnowledge", "Emotion",
                                     "Intention", "Perception", "Desire/\nWish",
                                     "Arousal", "Other")),
      # calculate odds ratio
      OR = exp(value)
      ) %>%
    # plot
    ggplot(aes(x = OR, y = fct_rev(name))) +
    # very small effect size window
    # https://easystats.github.io/effectsize/articles/interpret.html#chen2010big
    annotate("rect", xmin = 0.60, xmax = 1.68, ymin = -Inf, ymax = Inf,
              fill = "grey", alpha = 0.15) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    # posterior odds ratios
    stat_halfeye() +
    # add direction annotations
    annotate("text", label = "more common\nin English", x = 2.2, y = 4.4,
             size = 2.8, fontface = "italic", colour = "grey") +
    annotate("text", label = "more common\nin Tongan", x = 0.2, y = 4.4,
             size = 2.8, fontface = "italic", colour = "grey") +
    # add arrows
    geom_segment(aes(x = 2.05, y = 4.85, xend = 2.35, yend = 4.85),
                 arrow = arrow(length = unit(0.2, "cm")), colour = "grey") +
    geom_segment(aes(x = 0.35, y = 4.85, xend = 0.05, yend = 4.85),
                 arrow = arrow(length = unit(0.2, "cm")), colour = "grey") +
    # axes and theme
    ylab("Mental state class") +
    scale_x_continuous(
      name = "Posterior odds ratio\n(difference between English and Tongan)",
      breaks = seq(0, 2, by = 0.5),
      limits = c(0, 2.35)
      ) +
    theme_minimal()
  # save plot
  ggsave(out, filename = filename, width = 6, height = 5)
  return(out)
}