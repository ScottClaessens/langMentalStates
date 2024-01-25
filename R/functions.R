# custom functions for study 1

# plot proportions of definitions
plotProportions <- function(d, title, file) {
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
      y = "Proportion of definitions",
      title = title
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  # save
  ggsave(out, filename = file, width = 6, height = 4)
  return(out)
}

# pivot data wider
pivotDataWider <- function(d) {
  # if word column doesn't exist, create it
  if (!("word" %in% colnames(d))) d$word <- d$word_cc
  # pivot wider
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
plotModelResults <- function(hyp, title, file) {
  # list of labels
  labels <- c(
      "mentalState" = "Overall",
      "BK" = "Belief/\nKnowledge",
      "IN" = "Intention",
      "EM" = "Emotion",
      "DW" = "Desire/\nWish",
      "PE" = "Perception",
      "AR" = "Arousal",
      "OT" = "Other"
      )
  # extract posterior log odds differences
  out <-
    lapply(hyp, function(x) x$samples[,1]) %>%
    as_tibble() %>%
    pivot_longer(
      cols = everything(),
      names_to = "model",
      values_to = "diff"
      ) %>%
    mutate(
      model = str_remove(model, "hyp_"),
      model = str_replace(model, "mental_state", "mentalState")
    ) %>%
    separate(model, into = c("study", "outcome", "controls"), sep = "_") %>%
    # correct ordering for plot
    mutate(
      outcome = factor(labels[outcome], levels = labels),
      controls = ifelse(study == "dWide1",
                        ifelse(controls == "fitModel1",
                               "No controls\n(78685 words)",
                               "Controlling for\nword class\n(47260 words)"),
                        ifelse(controls == "fitModel1",
                               "No controls\n(2342 words) ",
                               "Controlling for\nword class\n(2031 words) ")),
      controls = fct_rev(controls)
    ) %>%
    # plot
    ggplot(aes(x = diff, y = fct_rev(outcome), colour = controls)) +
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
    labs(
      x = "Posterior log odds difference\nbetween English and Tongan",
      y = "Mental state class",
      title = title
      ) +
    scale_x_continuous(
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
  ggsave(out, filename = file, width = 7, height = 4)
  return(out)
}
