
library(brms)
library(here)
library(tidybayes)
library(modelr)
library(ggplot2)
library(forcats)
library(patchwork)
library(inspectdf)
library(GGally)
library(scales)
library(mice)
library(dplyr)
library(ggplot2)
library(tidyr)

source(here("scripts/plotting_functions.R"))
source(here("scripts/utility_functions.R"))

modeling_df <- read.csv(here("data/modeling_df.csv"))

# modeling

# an appropriate distribution assumption for the response (achievement and ego inclusion 3/4 ordered categories)
# is the ordered logit with 6 categories (rethinking 385)
# also: https://bookdown.org/content/4857/monsters-and-mixtures.html#ordered-categorical-outcomes

inspect_na(modeling_df) %>% show_plot()
inspect_num(modeling_df) %>% show_plot()

table(is.na(modeling_df$achievement), is.na(modeling_df$ego_inclusion))

# pairwise plots

modeling_df %>% 
  select(c(bet_log,triangle_count_log,ascribed_power_log,
           case,achievement,ego_inclusion, type_reduced)) %>% 
  mutate(ego_inclusion = factor(ego_inclusion,
                                labels = c("lowest","low","high","highest")), 
         achievement = factor(achievement,
                              labels = c("low","medium", "high")),
         type_reduced = factor(type_reduced, 
                               levels = c("Other",
                                          "higher_admin",
                                          "Municipality",
                                          "NGO_organization_association"),
                               labels = c("Other", 
                                      "Higher admin", 
                                      "Municipality",
                                      "NGO"))) %>% 
  filter(!(is.na(ego_inclusion) | is.na(achievement))) %>% 
  # ggpairs(aes(colour = factor(case), alpha = 0.4))
  ggpairs(columnLabels = c("Log \n betweenness",
                           "Log \n triangle count",
                           "Log \n ascribed power",
                           "Case",
                           "Priority issue \n status",
                           "Satisfaction \n with inclusion",
                           "Actor type"),
          upper = list(continuous = "points")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.text.y = element_blank())

ggsave(here("viz/pairwise_plots.png"), 
       height = 210, 
       width = 297, 
       units = "mm")

# heat map with three vars


modeling_df %>% 
  group_by(achievement, ego_inclusion) %>% 
  mutate(bet_log_mean = mean(bet_log),
         power_mean = mean(ascribed_power_log),
         bond_mean = mean(triangle_count_log)) %>% 
  drop_na(c(achievement, ego_inclusion)) %>% 
  ggplot(aes(x = triangle_count_log, y = bet_log)) +
  geom_point() +
  geom_hline(aes(yintercept = bet_log_mean)) +
  geom_vline(aes(xintercept = bond_mean)) +
  facet_wrap(vars(achievement, ego_inclusion), as.table = FALSE) +
  xlab("Bonding ties") +
  ylab("Power")

modeling_df %>% 
  group_by(achievement, ego_inclusion) %>% 
  mutate(bet_log_mean = mean(bet_log),
         power_mean = mean(ascribed_power_log),
         bond_mean = mean(triangle_count_log)) %>% 
  drop_na(c(achievement, ego_inclusion)) %>% 
  ggplot(aes(x = triangle_count_log, y = ascribed_power_log)) +
  geom_point() +
  geom_hline(aes(yintercept = power_mean)) +
  geom_vline(aes(xintercept = bond_mean)) +
  facet_wrap(vars(achievement, ego_inclusion), as.table = FALSE)

# main model -------

m_sat <-
  brm(
    data = modeling_df,
    family = cumulative,
    formula = 
      bf(ego_inclusion ~ 
           1 + 
           (1 + mo(achievement) + ascribed_power_log + triangle_count_log + bet_log || case) + 
           (1 | top_target) + 
           (1 | case:top_target) + 
           mo(achievement) +
           bet_log +
           triangle_count_log +
           ascribed_power_log +
           bet_log * triangle_count_log +
           ascribed_power_log * bet_log +
           ascribed_power_log * triangle_count_log +
           type_reduced),
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b)
    ),
    # sample_prior = "only",
    # inits = "0",
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    file = here("model_fits/m_sat"),
    backend = "cmdstanr"
  )

summary(m_sat)

plot_4_interactions(m_sat)

ggsave(here("viz/sat_interactions_uni.png"), width = 32, height = 18, units = "cm")

# imputing missing achievement data with mice ---------

modeling_imp_df <- modeling_df %>% 
  filter(!is.na(ego_inclusion)) %>% 
  select(c(bet_log,triangle_count_log,ascribed_power_log,
           case,achievement,ego_inclusion, type_reduced, top_target)) 

modeling_imp_df  <- mice(modeling_imp_df,m = 20, print = FALSE)

m_sat_imp_mice <-
  brm_multiple(
    data = modeling_imp_df,
    family = cumulative,
    formula = 
      bf(ego_inclusion ~ 
           1 + 
           (1 + mo(achievement) + ascribed_power_log + triangle_count_log + bet_log || case) + 
           (1 | top_target) + 
           (1 | case:top_target) + 
           mo(achievement) +
           bet_log +
           triangle_count_log +
           ascribed_power_log +
           bet_log * triangle_count_log +
           ascribed_power_log * bet_log +
           ascribed_power_log * triangle_count_log +
           type_reduced),
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b)
    ),
    # sample_prior = "only",
    # inits = "0",
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    file = here("model_fits/m_sat_imp_mice"),
    backend = "cmdstanr"
  )

summary(m_sat_imp_mice)

# look at r-hats for chains across all 20 imputed datasets - all good
round(m_sat_imp_mice$rhats, 2)

plot_4_interactions(m_sat_imp_mice)

ggsave(here("viz/sat_interactions_imp_mice_uni.png"), width = 32, height = 18, units = "cm")


# excluding achievement in priority goal as predictor

m_sat_noach <-
  brm(
    data = modeling_df,
    family = cumulative,
    formula = 
      bf(ego_inclusion ~ 
           1 + 
           (1 + ascribed_power_log + triangle_count_log + bet_log || case) + 
           (1 | top_target) + 
           (1 | case:top_target) + 
           bet_log +
           triangle_count_log +
           ascribed_power_log +
           bet_log * triangle_count_log +
           ascribed_power_log * bet_log +
           ascribed_power_log * triangle_count_log +
           type_reduced),
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b)
    ),
    # sample_prior = "only",
    # inits = "0",
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    file = here("model_fits/m_sat_no_ach"),
    backend = "cmdstanr"
  )

# no difference in tendencies
summary(m_sat_noach)

plot_4_interactions(m_sat_noach)

# model with type interactions

m_sat_typeinter <-
  brm(
    data = modeling_df,
    family = cumulative,
    formula = 
      bf(ego_inclusion ~ 
           1 + 
           (1 + mo(achievement) + ascribed_power_log + triangle_count_log + bet_log || case) + 
           (1 | top_target) + 
           (1 | case:top_target) + 
           mo(achievement) +
           bet_log +
           triangle_count_log +
           ascribed_power_log +
           bet_log * triangle_count_log +
           ascribed_power_log * bet_log +
           ascribed_power_log * triangle_count_log +
           ascribed_power_log * type_reduced +
           triangle_count_log * type_reduced +
           bet_log * type_reduced +
           type_reduced),
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b)
    ),
    # sample_prior = "only",
    # inits = "0",
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    file = here("model_fits/m_sat_typeinter"),
    backend = "cmdstanr"
  )

summary(m_sat_typeinter)

plot_4_interactions(m_sat_typeinter)

# prior predictive for m_sat ---------

m_sat_priorpred <-
  brm(
    data = modeling_df,
    family = cumulative,
    formula = 
      bf(ego_inclusion ~ 
           1 + 
           (1 + mo(achievement) + ascribed_power_log + triangle_count_log + bet_log || case) + 
           (1 | top_target) + 
           (1 | case:top_target) + 
           mo(achievement) +
           bet_log +
           triangle_count_log +
           ascribed_power_log +
           bet_log * triangle_count_log +
           ascribed_power_log * bet_log +
           ascribed_power_log * triangle_count_log +
           type_reduced),
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b)
    ),
    sample_prior = "only",
    # inits = "0",
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    file = here("model_fits/m_sat_priorpred"),
    backend = "cmdstanr"
  )

summary(m_sat_priorpred)

p_pp_prior <- 
 pp_check(m_sat_priorpred, type = "bars", ndraws = NULL) +
  guides(fill = FALSE, color = FALSE) +
  scale_y_continuous(limits = c(NA, 200)) + ggtitle("Prior predictions")
p_pp_post <-
  pp_check(m_sat_imp_mice, type = "bars", ndraws = NULL) +
  scale_y_continuous(limits = c(NA, 200)) + ggtitle("Posterior predictions")

p_pp_prior + p_pp_post

ggsave(here("viz/prior_vs_posterior_predictions.png"), 
       width = 25, height = 12, units = "cm")

# model comparisons inclusion -----------

m_sat <- add_criterion(m_sat,criterion = "loo")
m_sat_typeinter <- add_criterion(m_sat_typeinter,criterion = "loo")


#In the context of model selection, an LOOIC difference greater than twice its corresponding standard error 
#can be interpreted as suggesting that the model with the lower LOOIC value fits the data substantially better

loo_compare(m_sat, m_sat_typeinter)

# posterior summary plots --------

vars_sat_imp <- get_variables(m_sat_imp_mice)
#beta parameters
vars_sat_imp[grepl("b_.*|bsp",vars_sat_imp)]

vars_sat_imp_named <- c(
  `b_Intercept[1]` = "CDF Threshold 1",
  `b_Intercept[2]` = "CDF Threshold 2",
  `b_Intercept[3]` = "CDF Threshold 3",
  "b_bet_log"  = "Log Betweenness main effect",
  "b_triangle_count_log"  = "Log triangle count (bonding) main effect",
  "b_ascribed_power_log"  = "Log ascribed power main effect",
  "b_type_reducedMunicipality" = "Actor type: Municipality",
  "b_type_reducedNGO_organization_association" = "Actor type: NGO",
  "b_type_reducedOther" = "Actor type: Other",
  "b_bet_log:triangle_count_log" = "Betweenness x Triangle count interaction",
  "b_bet_log:ascribed_power_log" = "Betweenness x Power interaction",
  "b_triangle_count_log:ascribed_power_log" = "Triangle count x Power interaction",
  "bsp_moachievement" = "Status of priority goal (monotonic predictor)"
)

m_sat_imp_mice %>%
  gather_draws(`b_.*|bsp.*`, regex = TRUE) %>% 
  ggplot(aes(y = factor(.variable,
                        levels = rev(names(vars_sat_imp_named)),
                        labels = rev(vars_sat_imp_named)), x = .value)) +
  stat_halfeye(.width = c(0.66, 0.88), point_interval = median_hdi) + 
  geom_vline(xintercept = 0) +
  theme_minimal() + ylab("Parameters") + xlab("Posterior distribution")

ggsave(here("viz/posterior_beta.png"),
       width = 25, height = 12, units = "cm", bg = "white")

# m_sat_imp_mice %>%
#   spread_draws(b_Intercept[1:3]) %>% 
#   ggplot(aes(y = factor(`1:3`), x = b_Intercept)) +
#   stat_halfeye(.width = c(0.66, 0.88), point_interval = median_hdi) + theme_minimal() 

# m_sat_imp_mice %>%
#   gather_draws(`r_case.*`, regex = TRUE) %>% 
#   ggplot(aes(y = factor(.variable), x = .value)) +
#   stat_halfeye(.width = c(0.66, 0.88), point_interval = median_hdi) + 
#   geom_vline(xintercept = 0) +
#   theme_gray()

# varying intercept plots

# per case intercepts
m_sat_imp_mice %>%
  gather_draws(`r_case\\[.*`, regex = TRUE) %>% 
  filter(grepl("Intercept",.variable)) %>% 
  mutate(var_shortened = gsub("r_case|\\[|\\]|,Intercept","",.variable)) %>% 
  mutate(var_shortened = gsub("\\."," ",var_shortened)) %>% 
  filter(!grepl(",",var_shortened)) %>% 
  ggplot(aes(y = factor(var_shortened), x = .value)) +
  stat_halfeye(.width = c(0.66, 0.88), point_interval = median_hdi) + 
  geom_vline(xintercept = 0) +
  theme_minimal() + 
  xlab("Posterior distribution") + ylab("Varying intercepts (by case)")

ggsave(here("viz/posterior_var_intercepts_case.png"),
       width = 25, height = 12, units = "cm", bg = "white")

# per target

vars_sat_imp[grepl("r_top_target\\[.*",vars_sat_imp)]
vars_sat_imp_named_issues <- 
  c(
    "Andere_Wirtschaft" = "Other economic",
    "Biodiversitat" = "Biodiversity",
    "Erholungswert" = "Recreation",
    "Hochwasserschutz" = "Flood protection",
    "Landschaftschutz" = "Landscape protection",
    "Landwirtschaftliche_Produktivitat" = "Agricultural productivity",
    "Profitabilitat_Forstwirtschaft" = "Timber production",
    "Schutz_gegen_Waldbrande" = "Wildfire protection",
    "Schutz_vor_Sturmen" = "Storm protection"
  )

m_sat_imp_mice %>%
  gather_draws(`r_top_target\\[.*`, regex = TRUE) %>% 
  mutate(var_shortened = gsub("r_top_target|\\[|\\]|,Intercept","",.variable)) %>% 
  mutate(var_shortened = gsub("\\."," ",var_shortened)) %>% 
  filter(!grepl(",",var_shortened)) %>% 
  ggplot(aes(y = factor(var_shortened,
                        levels = names(vars_sat_imp_named_issues),
                        labels = vars_sat_imp_named_issues), x = .value)) +
  stat_halfeye(.width = c(0.66, 0.88), point_interval = median_hdi) + 
  geom_vline(xintercept = 0) +
  theme_minimal() + 
  xlab("Posterior distribution") + ylab("Varying intercepts (by target)")

ggsave(here("viz/posterior_var_intercepts_issue.png"),
       width = 25, height = 12, units = "cm", bg = "white")

# target case interactions
plotting_df_case_issue <-
m_sat_imp_mice %>%
  gather_draws(`r_case:top_target\\[.*`, regex = TRUE) %>% 
  mutate(var_shortened = gsub("r_case\\:top_target|\\[|\\]|,Intercept","",.variable)) %>% 
  mutate(var_shortened = gsub("\\."," ",var_shortened)) %>% 
  group_by(var_shortened) %>% 
  summarize(median_hdi(.value, .width = .88)) %>% 
  ungroup() %>% 
  mutate(var_shortened = factor(var_shortened)) %>% 
  mutate(var_shortened = fct_reorder(var_shortened,y)) %>% 
  mutate(case = str_extract(var_shortened,"^[^_]+(?=_)"))
 
levels(plotting_df_case_issue$var_shortened) <- unlist(lapply(
  levels(plotting_df_case_issue$var_shortened), function(x){
      split <- strsplit(as.character(x), "\\_")
      case <- split[[1]][1]
      pat <- gsub(paste(case,"_",sep = ""),"",x)
      gsub(pat,vars_sat_imp_named_issues[pat],x)
    }
  ))

plotting_df_case_issue %>% 
  filter(!is.na(var_shortened)) %>% 
  ggplot(aes(y = var_shortened, x = y, xmin = ymin, xmax = ymax,
             color = case)) +
  geom_pointinterval() + 
  geom_vline(xintercept = 0) + scale_color_viridis_d("Case") +
  theme_minimal() +
  xlab("Posterior distribution") + ylab("Varying intercepts (by issue nested in target)")

ggsave(here("viz/posterior_var_intercepts_caseissue.png"),
       width = 25, height = 12, units = "cm", bg = "white")

# # per case varying effects for bet log
# m_sat_imp_mice %>%
#   gather_draws(`r_case\\[.*`, regex = TRUE) %>% 
#   filter(grepl("bet\\_log",.variable)) %>% 
#   mutate(var_shortened = gsub("r_case|\\[|\\]|,bet\\_log","",.variable)) %>%
#   mutate(var_shortened = gsub("\\."," ",var_shortened)) %>% 
#   filter(!grepl(",",var_shortened)) %>% 
#   ggplot(aes(y = factor(var_shortened), x = .value)) +
#   stat_halfeye(.width = c(0.66, 0.88), point_interval = median_hdi) + 
#   geom_vline(xintercept = 0) +
#   theme_gray()


# per case varying effects for all
m_sat_imp_mice %>%
  gather_draws(`r_case\\[.*`, regex = TRUE) %>% 
  filter(grepl("triangle|bet|power",.variable)) %>% 
  mutate(var_fx = ifelse(grepl("triangle",.variable),"bonding",
                         ifelse(grepl("bet",.variable), "betweenness",
                                "power"))) %>% 
  mutate(var_shortened = gsub("r_case|\\[|\\]","",.variable)) %>%
  mutate(var_shortened = gsub("\\."," ",var_shortened)) %>% 
  mutate(case = strsplit(var_shortened,",")[[1]][1]) %>% 
  ggplot(aes(y = factor(case), x = .value, color = case)) +
  stat_halfeye(.width = c(0.66, 0.88), point_interval = median_hdi) + 
  geom_vline(xintercept = 0) +
  facet_wrap(vars(var_fx)) +
  theme_minimal() + scale_color_viridis_d("Case") +
  ylab("Case") + xlab("Posterior distrbution of varying effects (by case)")

ggsave(here("viz/posterior_varfx_case.png"),
       width = 25, height = 12, units = "cm", bg = "white")

# sensitivity checks ------------

prior_summary(m_sat_imp_mice)

# varying broad priors

m_sat_priorvar1 <-
  brm(
    data = modeling_df,
    family = cumulative,
    formula = 
      bf(ego_inclusion ~ 
           1 + 
           (1 + mo(achievement) + ascribed_power_log + triangle_count_log + bet_log || case) + 
           (1 | top_target) + 
           (1 | case:top_target) + 
           mo(achievement) +
           bet_log +
           triangle_count_log +
           ascribed_power_log +
           bet_log * triangle_count_log +
           ascribed_power_log * bet_log +
           ascribed_power_log * triangle_count_log +
           type_reduced),
    prior = c(prior(normal(0, 1), class = Intercept),
              prior(normal(0, 1), class = b)
    ),
    # sample_prior = "only",
    # inits = "0",
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    file = here("model_fits/m_sat_priorvar1"),
    backend = "cmdstanr"
  )

summary(m_sat_priorvar1)

m_sat_priorvar2 <-
  brm(
    data = modeling_df,
    family = cumulative,
    formula = 
      bf(ego_inclusion ~ 
           1 + 
           (1 + mo(achievement) + ascribed_power_log + triangle_count_log + bet_log || case) + 
           (1 | top_target) + 
           (1 | case:top_target) + 
           mo(achievement) +
           bet_log +
           triangle_count_log +
           ascribed_power_log +
           bet_log * triangle_count_log +
           ascribed_power_log * bet_log +
           ascribed_power_log * triangle_count_log +
           type_reduced),
    prior = c(prior(normal(0, 10), class = Intercept),
              prior(normal(0, 10), class = b)
    ),
    # sample_prior = "only",
    # inits = "0",
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    file = here("model_fits/m_sat_priorvar2"),
    backend = "cmdstanr"
  )

summary(m_sat_priorvar2)

m_sat_priorvar3 <-
  brm(
    data = modeling_df,
    family = cumulative,
    formula = 
      bf(ego_inclusion ~ 
           1 + 
           (1 + mo(achievement) + ascribed_power_log + triangle_count_log + bet_log || case) + 
           (1 | top_target) + 
           (1 | case:top_target) + 
           mo(achievement) +
           bet_log +
           triangle_count_log +
           ascribed_power_log +
           bet_log * triangle_count_log +
           ascribed_power_log * bet_log +
           ascribed_power_log * triangle_count_log +
           type_reduced),
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b),
              prior(dirichlet(c(2,1)), coef = moachievement1, class = simo)
    ),
    # sample_prior = "only",
    # inits = "0",
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    file = here("model_fits/m_sat_priorvar3"),
    backend = "cmdstanr"
  )

m_sat_priorvar4 <-
  brm(
    data = modeling_df,
    family = cumulative,
    formula = 
      bf(ego_inclusion ~ 
           1 + 
           (1 + mo(achievement) + ascribed_power_log + triangle_count_log + bet_log || case) + 
           (1 | top_target) + 
           (1 | case:top_target) + 
           mo(achievement) +
           bet_log +
           triangle_count_log +
           ascribed_power_log +
           bet_log * triangle_count_log +
           ascribed_power_log * bet_log +
           ascribed_power_log * triangle_count_log +
           type_reduced),
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b),
              prior(student_t(3,0,1), class = sd)
    ),
    # sample_prior = "only",
    # inits = "0",
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    file = here("model_fits/m_sat_priorvar4"),
    backend = "cmdstanr"
  )

m_sat_priorvar5 <-
  brm(
    data = modeling_df,
    family = cumulative,
    formula = 
      bf(ego_inclusion ~ 
           1 + 
           (1 + mo(achievement) + ascribed_power_log + triangle_count_log + bet_log || case) + 
           (1 | top_target) + 
           (1 | case:top_target) + 
           mo(achievement) +
           bet_log +
           triangle_count_log +
           ascribed_power_log +
           bet_log * triangle_count_log +
           ascribed_power_log * bet_log +
           ascribed_power_log * triangle_count_log +
           type_reduced),
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b),
              prior(student_t(3,0,10), class = sd)
    ),
    # sample_prior = "only",
    # inits = "0",
    iter = 2000, warmup = 1000, cores = 4, chains = 4,
    file = here("model_fits/m_sat_priorvar5"),
    backend = "cmdstanr"
  )

# plot comparison

b_draws <-
m_sat %>%
  gather_draws(`b_.*|bsp.*`, regex = TRUE) %>% 
  mutate(prior_setting = "\u03C4 and \u03B2 normal(0,5)")

b_draws_1 <-
  m_sat_priorvar1 %>%
  gather_draws(`b_.*|bsp.*`, regex = TRUE) %>% 
  mutate(prior_setting = "\u03C4 and \u03B2 normal(0,1)")

b_draws_2 <-
  m_sat_priorvar2 %>%
  gather_draws(`b_.*|bsp.*`, regex = TRUE) %>% 
  mutate(prior_setting = "\u03C4 and \u03B2 normal(0,10)")

b_draws_3 <-
  m_sat_priorvar3 %>%
  gather_draws(`b_.*|bsp.*`, regex = TRUE) %>% 
  mutate(prior_setting = "monotonic simplex dirichlet(2,1)")

b_draws_4 <-
  m_sat_priorvar4 %>%
  gather_draws(`b_.*|bsp.*`, regex = TRUE) %>% 
  mutate(prior_setting = "\u03B1 varying fx student_t(3,0,1)")

b_draws_5 <-
  m_sat_priorvar5 %>%
  gather_draws(`b_.*|bsp.*`, regex = TRUE) %>% 
  mutate(prior_setting = "\u03B1 varying fx student_t(3,0,10)")

bind_rows(b_draws, b_draws_1, b_draws_2, b_draws_3, b_draws_4, b_draws_5) %>% 
  ggplot(aes(y = factor(.variable,
                        levels = rev(names(vars_sat_imp_named)),
                        labels = rev(vars_sat_imp_named)), 
             x = .value,
             color = prior_setting, fill = prior_setting)) +
  stat_halfeye(.width = c(0.66, 0.88), point_interval = median_hdi,
               alpha = 0.3) + 
  geom_vline(xintercept = 0) + 
  scale_color_viridis_d("\u03C4 and \u03B2 distributions") +
  scale_fill_viridis_d("\u03C4 and \u03B2 distributions") +
  theme_minimal() + ylab("Parameters") + xlab("Posterior distribution")

ggsave("viz/posterior_prior_sensitivity.png",
       width = 25, height = 12, units = "cm", bg = "white")
