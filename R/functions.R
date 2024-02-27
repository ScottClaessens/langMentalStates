# custom functions

# plot proportions of definitions
plotProportions1 <- function(d1) {
  # list of labels
  labels <- c("Overall","Belief/\nKnowledge","Emotion","Intention",
              "Desire/\nWish","Perception","Arousal","Other")
  # plot
  out <-
    d1 %>%
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
    mutate(name = factor(name, levels = labels)) %>%
    ggplot(aes(x = name, y = value,
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
      title = "Dictionary study"
    ) +
    ylim(c(0, 0.074)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  # save
  ggsave(out, filename = "plots/study1/proportions.pdf", width = 6, height = 4)
  return(out)
}

# fit model 1 (no controls)
fitModel1 <- function(d, outcome) {
  # wrangle data
  if (!("word" %in% colnames(d))) d$word <- d$word_cc
  # fit model
  brm(
    formula = bf(
      paste0(outcome, " ~ 0 + language + (1 | word:language)")
      ),
    data = d,
    family = bernoulli,
    prior = c(
      # priors based on prior predictive check
      prior(normal(-2, 1), class = b, coef = languageeng),
      prior(normal(-2, 1), class = b, coef = languageton),
      prior(exponential(1), class = sd)
    ),
    cores = 4
  )
}

# fit model 2 (with word class controls)
fitModel2 <- function(d, outcome) {
  # wrangle data
  if (!("word" %in% colnames(d))) d$word <- d$word_cc
  # fit model
  brm(
    formula = bf(
      paste0(outcome, " ~ 0 + language + noun + adjective + adverb + verb + (1 | word:language)")
    ),
    data = d,
    family = bernoulli,
    prior = c(
      # priors based on prior predictive check
      prior(normal(-2, 1), class = b, coef = languageeng),
      prior(normal(-2, 1), class = b, coef = languageton),
      prior(normal(0, 1), class = b, coef = nounTRUE),
      prior(normal(0, 1), class = b, coef = adjectiveTRUE),
      prior(normal(0, 1), class = b, coef = adverbTRUE),
      prior(normal(0, 1), class = b, coef = verbTRUE),
      prior(exponential(1), class = sd)
    ),
    cores = 4
  )
}

# plot results from models
plotModelResults1 <- function(hyp) {
  # list of labels
  labels <- c(
      "mentalState" = "Overall",
      "BK" = "Belief/\nKnowledge",
      "EM" = "Emotion",
      "IN" = "Intention",
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
      controls = ifelse(study == "d1",
                        ifelse(controls == "fitModel1",
                               "No controls\n(78685 words)",
                               "Controlling for\nword class\n(47125 words)"),
                        ifelse(controls == "fitModel1",
                               "No controls\n(2400 words) ",
                               "Controlling for\nword class\n(2068 words) ")),
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
    annotate("text", label = "more common\nin English", x = 1.55, y = 5.0,
             size = 2.5, fontface = "italic", colour = "grey") +
    annotate("text", label = "more common\nin Tongan", x = -1.55, y = 5.0,
             size = 2.5, fontface = "italic", colour = "grey") +
    # add arrows
    geom_segment(aes(x = 1.40, y = 5.4, xend = 1.70, yend = 5.4),
                 arrow = arrow(length = unit(0.2, "cm")), colour = "grey") +
    geom_segment(aes(x = -1.40, y = 5.4, xend = -1.70, yend = 5.4),
                 arrow = arrow(length = unit(0.2, "cm")), colour = "grey") +
    # axes and theme
    labs(
      x = "Posterior log odds difference\nbetween English and Tongan",
      y = "Mental state class",
      title = "Dictionary study"
      ) +
    scale_x_continuous(
      breaks = seq(-2, 2, by = 1),
      limits = c(-2, 2)
      ) +
    guides(colour = guide_legend(byrow = TRUE)) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.spacing.y = unit(2, 'mm')
      )
  # save plot
  ggsave(out, filename = "plots/study1/models.pdf", width = 7, height = 4)
  return(out)
}

# load common crawl data
loadData2 <- function(fileData2) {
  # load rds file
  readRDS(fileData2) %>%
    # summarise for modelling
    group_by(word, language, domain, domain_type, domain_n_words_all,
             domain_n_words_coded, domain_n_pages, word_n_uses_in_domain,
             word_n_pages_in_domain) %>%
    # proportion of definitions at word-level
    summarise_at(
      vars(noun, verb, adjective, adverb, comb_other,
           mental_state, BK, DW, IN, PE, EM, AR, OT),
      function(x) mean(x, na.rm = TRUE)
      ) %>%
    ungroup()
}

# plot proportions
plotProportions2 <- function(d2) {
  # list of labels
  labels <- c("Overall","Belief/\nKnowledge","Emotion","Intention",
              "Desire/\nWish","Perception","Arousal","Other")
  # plot
  out <-
    d2 %>%
    transmute(
      language = language,
      word = word,
      domain = domain,
      # for plotting, binarise into mental state word or not
      # if at least one definition matches, categorise as mental state
      Overall              = ifelse(mental_state == 0, 0, 1),
      `Belief/\nKnowledge` = ifelse(BK == 0, 0, 1),
      `Desire/\nWish`      = ifelse(DW == 0, 0, 1),
      Intention            = ifelse(IN == 0, 0, 1),
      Perception           = ifelse(PE == 0, 0, 1),
      Emotion              = ifelse(EM == 0, 0, 1),
      Arousal              = ifelse(AR == 0, 0, 1),
      Other                = ifelse(OT == 0, 0, 1),
      # calculate proportion of times word used on domain
      propUsage = word_n_uses_in_domain / domain_n_words_all
    ) %>%
    pivot_longer(
      cols = Overall:Other,
      names_to = "mentalStateClass",
      values_to = "mentalStateCode"
    ) %>%
    mutate(
      mentalStateClass = factor(mentalStateClass, levels = labels),
      language = ifelse(language == "eng", "English", "Tongan")
      ) %>%
    # focus only on mental state words for plot
    filter(mentalStateCode != 0) %>%
    group_by(language, mentalStateClass) %>%
    # get average percentage of word uses on domains
    summarise(propUsage = mean(propUsage, na.rm = TRUE)) %>%
    # plotting
    ggplot(aes(x = mentalStateClass, y = propUsage, fill = language)) +
    geom_col(position = "dodge", colour = "black") +
    scale_fill_manual(
      name = "Language",
      labels = c("English","Tongan"),
      values = c("gray25","white")
    ) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = "Mental state class",
      y = "Average percentage of word uses on domains",
      title = "Common crawl study"
    ) +
    #ylim(c(0, 0.074)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  # save
  ggsave(out, filename = "plots/study2/proportions.pdf", width = 6, height = 4)
  return(out)
}

# fit usage model 1
fitUsageModel1 <- function(d2, mentalStateVar) {
  # mental state variable in data
  d2$mentalState <- pull(d2, !!mentalStateVar)
  # fit model
  brm(
    formula = bf(
      paste0(
        "word_n_uses_in_domain | trials(domain_n_words_all)",
        " ~ 1 + language*mentalState",
        " + (1 | word) + (1 | domain)"
        )
    ),
    data = d2,
    family = binomial,
    prior = c(
      # priors based on prior predictive check
      prior(normal(-5, 1), class = Intercept),
      prior(normal(0, 1), class = b),
      prior(exponential(1), class = sd)
    ),
    cores = 4
  )
}

# fit usage model 2
fitUsageModel2 <- function(d2, mentalStateVar) {
  # mental state variable in data
  d2$mentalState <- pull(d2, !!mentalStateVar)
  # fit model
  brm(
    formula = bf(
      paste0(
        "word_n_uses_in_domain | trials(domain_n_words_all)",
        " ~ 1 + language*mentalState",
        " + noun + verb + adjective + adverb + domain_type",
        " + (1 | word) + (1 | domain)"
      )
    ),
    data = d2,
    family = binomial,
    prior = c(
      # priors based on prior predictive check
      prior(normal(-5, 1), class = Intercept),
      prior(normal(0, 1), class = b),
      prior(exponential(1), class = sd)
    ),
    cores = 4
  )
}
