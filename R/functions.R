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
plotModelResults <- function(hyp1, hyp2) {
  # extract posterior from list of brmsfit.hypothesis objects
  extractPost <- function(hyp, label) {
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
    post %>%
      pivot_longer(
        cols = everything(),
        names_to = "model",
        values_to = "diff"
        ) %>%
      mutate(
        # correct ordering for plot
        model = factor(model, levels = c("Overall", "Belief/\nKnowledge", "Emotion",
                                         "Intention", "Perception", "Desire/\nWish",
                                         "Arousal", "Other")),
        # calculate odds ratio
        OR = exp(diff),
        # label this set of models
        label = label
      )
  }
  out <-
    # combine both sets of models
    bind_rows(
      extractPost(hyp1, label = "No controls\n(n = 78685)"),
      extractPost(hyp2, label = "Controlling for\nword class\n(n = 47260)")
    ) %>%
    mutate(label = factor(label, levels = c("No controls\n(n = 78685)",
                                            "Controlling for\nword class\n(n = 47260)"))) %>%
    # plot
    ggplot(aes(x = diff, y = fct_rev(model), colour = label)) +
    # effect size windows
    # https://easystats.github.io/effectsize/articles/interpret.html#chen2010big
    annotate("rect", xmin = -1.25, xmax = 1.25, ymin = -Inf, ymax = Inf,
             fill = "grey", alpha = 0.1) +
    annotate("rect", xmin = -0.5, xmax = 0.5, ymin = -Inf, ymax = Inf,
             fill = "grey", alpha = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    # posterior odds ratios
    stat_pointinterval(position = position_dodge(width = 0.4)) +
    # add direction annotations
    annotate("text", label = "more common\nin English", x = 1.55, y = 4.4,
             size = 2.5, fontface = "italic", colour = "grey") +
    annotate("text", label = "more common\nin Tongan", x = -1.55, y = 4.4,
             size = 2.5, fontface = "italic", colour = "grey") +
    # add arrows
    geom_segment(aes(x = 1.40, y = 4.8, xend = 1.70, yend = 4.8),
                 arrow = arrow(length = unit(0.2, "cm")), colour = "grey") +
    geom_segment(aes(x = -1.40, y = 4.8, xend = -1.70, yend = 4.8),
                 arrow = arrow(length = unit(0.2, "cm")), colour = "grey") +
    # axes and theme
    ylab("Mental state class") +
    scale_x_continuous(
      name = "Posterior log odds difference\nbetween English and Tongan",
      breaks = seq(-1.5, 1.5, by = 0.5),
      limits = c(-1.75, 1.75)
      ) +
    guides(colour = guide_legend(byrow = TRUE)) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.spacing.y = unit(2, 'mm')
      )
  # save plot
  ggsave(out, filename = "plots/models.pdf", width = 7, height = 4)
  return(out)
}