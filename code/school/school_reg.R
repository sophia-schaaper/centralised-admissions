library(tidyverse)
library(tidyr)
library(readxl)
library(janitor)
library(did)
library(purrr)
library(tibble)
library(kableExtra)
library(segregation)
library(cobalt)
library(car)
library(splines)
library(dplyr)
library(ggplot2)
library(forcats)
options(dplyr.summarise.inform = FALSE)

## Load Data ---- 

setwd("C:/Users/sophi/Documents/LSE/Education/school_segregation/data")

school <- read_csv("school_panel_cov.csv")

covariates <- c("PUPTEACH", "lunch_pct", "MEMBER", 
                "CHARTER", "MAGNET", "TYPE")
outcome_list <- c(H_diversity = "H_diversity", ls = "ls",
                  white_share = "white_share", rep_black = "rep_black",
                  rep_hisp = "rep_hisp", rep_nonwhite = "rep_nonwhite")

school_clean <- school %>%
  filter(lunch_pct >= -0.1, lunch_pct <= 2) %>%
  mutate(
    lunch_pct = case_when(
      lunch_pct < 0 ~ 0,
      lunch_pct > 1 ~ 1,
      TRUE          ~ lunch_pct
    ),
    # robust recodes (as already fixed)
    CHARTER = case_when(
      substr(str_squish(CHARTER), 1, 1) == "1" ~ 1L,
      substr(str_squish(CHARTER), 1, 1) == "2" ~ 0L,
      TRUE                                     ~ NA_integer_
    ),
    MAGNET = case_when(
      substr(str_squish(MAGNET), 1, 1) == "1" ~ 1L,
      substr(str_squish(MAGNET), 1, 1) == "2" ~ 0L,
      TRUE                                    ~ NA_integer_
    ),
    TYPE = case_when(
      substr(str_squish(TYPE), 1, 1) == "1" ~ "Regular",
      substr(str_squish(TYPE), 1, 1) == "2" ~ "SpecialEd",
      substr(str_squish(TYPE), 1, 1) == "3" ~ "CTE",
      substr(str_squish(TYPE), 1, 1) == "4" ~ "AltEd",
      TRUE                                  ~ NA_character_
    ),
    TYPE = factor(TYPE, levels = c("Regular", "CTE", "SpecialEd", "AltEd")),
    # numeric transforms
    log_member = log(pmax(MEMBER, 1)),
    log_pt     = log1p(PUPTEACH)
  ) %>%
  # drop NAs on ALL covariates you listed + derived logs + keys you require
  drop_na(PUPTEACH, lunch_pct, MEMBER, CHARTER, MAGNET, TYPE,
          G_year, unit_id, year, log_member, log_pt) %>%
  # optional: factors for reporting
  mutate(
    CHARTER_f = factor(CHARTER, levels = c(0,1), labels = c("No","Yes")),
    MAGNET_f  = factor(MAGNET,  levels = c(0,1), labels = c("No","Yes"))
  ) %>%
  select(-LOCALE_FINAL)


## Sanity Checks ----

## check NAs

check_covariates <- function(df, covariates) {
  df %>% 
    select(all_of(covariates)) %>% 
    summarise(across(everything(), ~ sum(is.na(.))))
}
check_covariates(school_clean, covariates)


## check type
school_clean <- school_clean %>%
  mutate(
    across(c("CHARTER", "MAGNET", "TYPE"), as.factor),
    MEMBER = as.numeric(MEMBER),
    lunch_pct = as.numeric(lunch_pct)
  )
## check vif
lm_check <- lm(as.formula(paste("white_share ~", paste(covariates, collapse = " + "))), 
               data = school_clean)
vif(lm_check)

## Run Regs ----

school_simple_rows <- list()
school_group_rows  <- list()

add_significance_info <- function(att, se, digits = 4) {
  p_val <- 2 * (1 - pnorm(abs(att / se)))
  stars <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01  ~ "**",
    p_val < 0.05  ~ "*",
    p_val < 0.1   ~ ".",
    TRUE          ~ ""
  )
  list(
    ATT_raw   = round(att, digits),
    Std_Error = round(se, digits),
    p_value   = p_val,
    stars     = stars,
    ATT       = paste0(round(att, digits), stars),
    CI_Lower  = round(att - 1.96 * se, digits),
    CI_Upper  = round(att + 1.96 * se, digits)
  )
}

# Main loop
walk(names(outcome_list), function(out) {
  att <- att_gt(
    yname       = outcome_list[[out]],
    tname       = "year",
    idname      = "unit_id",
    gname       = "G_year",
    xformla     = ~ log_pt + lunch_pct + log_member + CHARTER + MAGNET + TYPE,
    data        = as.data.frame(school_clean),  # coerce to df
    panel       = FALSE,
    est_method  = "reg",
    clustervars = "LEAID"
  )
  
  res <- list(
    att_gt       = att,
    aggte_simple = aggte(att, type = "simple", na.rm = TRUE),
    aggte_group  = aggte(att, type = "group",  na.rm = TRUE)
  )
  
  # Save .rds
  #saveRDS(res, file = file.path(results_dir, paste0("did_", out, "_reg.rds")))
  
  # Append row for school_simple
  school_simple_rows[[out]] <- tibble(
    Outcome = out,
    Estimate = "Simple ATT",
    !!!add_significance_info(res$aggte_simple$overall.att, res$aggte_simple$overall.se)
  )
  
  # Append row for school_group
  school_group_rows[[out]] <- tibble(
    Outcome = out,
    Estimate = "Group ATT",
    !!!add_significance_info(res$aggte_group$overall.att, res$aggte_group$overall.se)
  )
  
  # Save dynamic plot
  dynamic_plot <- plot(aggte(att, type = "dynamic", na.rm = TRUE))
  ggsave(paste0("../main/school_plot_", out, ".png"), plot = dynamic_plot,
         width = 8, height = 5, dpi = 300)
})

# Combine and export tables
school_simple <- bind_rows(school_simple_rows) %>%
  select(Outcome, Estimate, ATT, Std_Error, CI_Lower, CI_Upper)

school_group <- bind_rows(school_group_rows) %>%
  select(Outcome, Estimate, ATT, Std_Error, CI_Lower, CI_Upper)

# Save as HTML tables
save_kable(
  school_simple %>%
    kable(format = "html", caption = "School-Level Simple ATT Estimates") %>%
    kable_styling(full_width = FALSE),
  "../main/school_simple.html"
)

save_kable(
  school_group %>%
    kable(format = "html", caption = "School-Level Group ATT Estimates") %>%
    kable_styling(full_width = FALSE),
  "../appendix/school_group.html"
)

saveRDS(school_simple, file = "../school_simple.rds")



# Interactions ----

## 1) Make lunch terciles (pre-treatment)

pre_lunch <- school_clean %>%
  group_by(unit_id) %>%
  summarise(
    pre_lunch = mean(lunch_pct[ (G_year == 0) | (year < G_year) ], na.rm = TRUE),
    .groups = "drop"
  )

cuts <- quantile(pre_lunch$pre_lunch, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
pre_lunch <- pre_lunch %>%
  mutate(lunch_band = cut(pre_lunch, breaks = cuts,
                          include.lowest = TRUE, labels = c("Low","Mid","High")))

school_aug <- school_clean %>%
  left_join(pre_lunch, by = "unit_id") %>%
  mutate(
    sg_charter    = as.integer(CHARTER == 1),
    sg_noncharter = as.integer(CHARTER == 0),
    sg_magnet     = as.integer(MAGNET  == 1),
    sg_nonmagnet  = as.integer(MAGNET  == 0),
    sg_lunch_low  = as.integer(lunch_band == "Low"),
    sg_lunch_mid  = as.integer(lunch_band == "Mid"),
    sg_lunch_high = as.integer(lunch_band == "High")
  )

sg_names <- c("charter","noncharter","magnet","nonmagnet","lunch_low","lunch_mid","lunch_high")

## 2) Build weights for (g,t) and (e) grids

gt_weights_by_sg <- function(df, sg_var) {
  df %>%
    filter(G_year > 0, .data[[sg_var]] == 1, year >= G_year) %>%
    count(g = G_year, t = year, name = "n_sg_gt") %>%
    mutate(w = n_sg_gt / sum(n_sg_gt))
}

egt_weights_by_sg <- function(df, sg_var) {
  df %>%
    filter(G_year > 0, .data[[sg_var]] == 1, year >= G_year) %>%
    count(g = G_year, e = year - G_year, name = "n_sg_ge") %>%
    group_by(e) %>%
    mutate(w = n_sg_ge / sum(n_sg_ge)) %>%
    ungroup()
}

w_gt_list  <- setNames(
  lapply(sg_names, function(sg) gt_weights_by_sg(school_aug, paste0("sg_", sg))),
  sg_names
)
w_egt_list <- setNames(
  lapply(sg_names, function(sg) egt_weights_by_sg(school_aug, paste0("sg_", sg))),
  sg_names
)

## 3) Post-stratification function

extract_gt <- function(att_obj) {
  tibble(g = att_obj$group, t = att_obj$t, att = att_obj$att, se = att_obj$se) %>%
    filter(!is.na(att))
}
extract_egt <- function(agg_dyn) {
  tibble(e = agg_dyn$egt, att = agg_dyn$att.egt, se = agg_dyn$se.egt) %>%
    filter(!is.na(att))
}

poststratify_saved <- function(att_obj, w_gt, w_egt) {
  gt <- extract_gt(att_obj) %>% left_join(w_gt, by = c("g","t"))
  gt <- gt %>% filter(!is.na(w))
  att_overall <- sum(gt$w * gt$att)
  se_overall  <- sqrt(sum((gt$w^2) * (gt$se^2)))
  
  ad <- aggte(att_obj, type = "dynamic", na.rm = TRUE)
  egt <- extract_egt(ad) %>% left_join(w_egt, by = "e")
  dyn <- egt %>%
    group_by(e) %>%
    summarise(
      estimate = sum(w * att, na.rm = TRUE),
      se       = sqrt(sum((w^2) * (se^2), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(ci_lo = estimate - 1.96*se, ci_hi = estimate + 1.96*se)
  
  list(
    overall = tibble(estimate = att_overall, se = se_overall,
                     ci_lo = att_overall - 1.96*se_overall,
                     ci_hi = att_overall + 1.96*se_overall),
    dynamic = dyn
  )
}

### Loop ----

post_by_sg <- imap(did_results, function(att_obj, out_key) {
  imap(w_gt_list, function(wgt, sg) {
    res <- poststratify_saved(att_obj$att_gt, wgt, w_egt_list[[sg]])
    tibble(
      outcome  = out_key,
      subgroup = sg,
      type     = "overall",
      res_overall = list(res$overall),
      res_dynamic = list(res$dynamic)
    )
  }) %>% bind_rows()
}) %>% bind_rows()

overall_ps <- post_by_sg %>%
  select(outcome, subgroup, res_overall) %>%
  tidyr::unnest(res_overall)

dynamic_ps <- post_by_sg %>%
  select(outcome, subgroup, res_dynamic) %>%
  tidyr::unnest(res_dynamic)

saveRDS(dynamic_ps, file = "../main/dynamic_ps.rds")
saveRDS(overall_ps, file = "../main/overall_ps.rds")



### Plot  ----

outcome_labs <- c(
  H_diversity = "H (entropy-diversity)",
  ls          = "Local segregation (ls)",
  white_share = "White share",
  rep_black   = "Representation: Black",
  rep_hisp    = "Representation: Hispanic",
  rep_nonwhite= "Representation: Nonwhite"
)

#### Charter ----

dyn_charter <- dynamic_ps %>%
  filter(subgroup %in% c("charter","noncharter")) %>%
  mutate(
    outcome  = forcats::fct_recode(factor(outcome), !!!outcome_labs),
    subgroup = case_when(
      subgroup == "charter"    ~ "Charter",
      subgroup == "noncharter" ~ "Non-charter",
      TRUE ~ subgroup
    )
  )

ggplot(dyn_charter, aes(x = e, y = estimate, color = subgroup)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = subgroup),
              alpha = 0.15, colour = NA, show.legend = FALSE) +
  geom_line(size = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(
    title = "Dynamic post-stratified effects: Charter vs Non-charter",
    x = "Event time (years since treatment)",
    y = "ATT",
    color = "School type"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("../main/school_charter_plot.png", width = 10, height = 6, dpi = 300)

#### Magnet ----

dyn_magnet <- dynamic_ps %>%
  filter(subgroup %in% c("magnet","nonmagnet")) %>%
  mutate(
    outcome  = forcats::fct_recode(factor(outcome), !!!outcome_labs),
    subgroup = case_when(
      subgroup == "magnet"    ~ "Magnet",
      subgroup == "nonmagnet" ~ "Non-magnet",
      TRUE ~ subgroup
    )
  )

ggplot(dyn_magnet, aes(x = e, y = estimate, color = subgroup)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = subgroup),
              alpha = 0.15, colour = NA, show.legend = FALSE) +
  geom_line(size = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(
    title = "Dynamic post-stratified effects: Magnet vs Non-magnet",
    x = "Event time (years since treatment)",
    y = "ATT",
    color = "School type"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("../main/school_magnet_plot.png", width = 10, height = 6, dpi = 300)

#### Lunch ----

dyn_lunch <- dynamic_ps %>%
  filter(subgroup %in% c("lunch_low","lunch_mid","lunch_high")) %>%
  mutate(
    outcome  = forcats::fct_recode(factor(outcome), !!!outcome_labs),
    subgroup = dplyr::recode(subgroup,
                             "lunch_low"  = "Low lunch % (pre-treat)",
                             "lunch_mid"  = "Mid lunch %",
                             "lunch_high" = "High lunch %"
    )
  )


ggplot(dyn_lunch, aes(x = e, y = estimate, color = outcome)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = outcome),
              alpha = 0.10, colour = NA, show.legend = FALSE) +
  geom_line(size = 0.9) +
  geom_point(size = 1.5) +
  facet_wrap(~ subgroup, nrow = 1) +
  labs(
    title = "Dynamic post‑stratified effects by pre‑treatment lunch band",
    x = "Event time (years since treatment)",
    y = "ATT (post‑stratified within subgroup)",
    color = "Outcome"
  ) + 
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("../main/school_lunch_plot.png", width = 10, height = 6, dpi = 300)
