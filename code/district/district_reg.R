library(kableExtra)
library(tidyverse)
library(did)
library(segregation)
library(cobalt)
# Run REG ----

setwd("C:/Users/sophi/Documents/LSE/Education/school_segregation/data/cov")
seg_df <- read_csv("../theil_seg_measures_with_dis_cov.csv")
seg_df <- seg_df %>%
  mutate(
    log_enroll       = log(enrollment),
    log_enroll_p1    = log(enrollment + 1),
    log_enroll_sq    = log(enrollment)^2,
    
    log_currpp       = log(current_pp),
    log_currpp_p1    = log(current_pp + 1),
    log_currpp_sq    = log(current_pp)^2,
    
    pupteach_sq      = pupteach^2,
    sec_teach_pct_sq = sec_teach_pct^2,
    
    poly_pupteach1   = poly(pupteach, 2)[,1],
    poly_pupteach2   = poly(pupteach, 2)[,2],
    poly_sec1        = poly(sec_teach_pct, 2)[,1],
    poly_sec2        = poly(sec_teach_pct, 2)[,2]
  )

# Check CORR
cor(seg_df %>% 
      select(log_currpp, log_currpp_sq, 
             log_enroll, log_enroll_sq, 
             pupteach, pupteach_sq, 
             sec_teach_pct, sec_teach_pct_sq),
    use = "complete.obs")

## IPW ----


# M outcome
att_results_M <- att_gt(
  yname = "M",               
  tname = "year",
  idname = "unit_id",            
  gname = "G_year",   
  xformla = ~ log_enroll + pupteach + pupteach_sq + 
    sec_teach_pct + sec_teach_pct_sq + log_currpp,
  data = seg_df,
  panel = FALSE,
  est_method = "ipw"
)

# H outcome
att_results_H <- att_gt(
  yname = "H",                
  tname = "year",
  idname = "unit_id",         
  gname = "G_year",   
  xformla = ~ poly_sec1 + poly_sec2 +
    log_currpp_p1 + log_enroll_p1 +
    poly_pupteach1 + poly_pupteach2,
  data = seg_df,
  panel = FALSE,
  est_method = "ipw"
)

# D outcome
att_results_D <- att_gt(
  yname = "Dis",                
  tname = "year",
  idname = "unit_id",         
  gname = "G_year",   
  xformla = ~ log_currpp + log_enroll + pupteach + sec_teach_pct,
  data = seg_df,
  panel = FALSE,
  est_method = "ipw"
)

# P outcome
att_results_P <- att_gt(
  yname  = "P_nonwhite_white",
  tname  = "year",
  idname = "unit_id",
  gname  = "G_year",
  xformla = ~ log_enroll + pupteach + pupteach_sq +
    sec_teach_pct + sec_teach_pct_sq +
    log_currpp,
  data   = seg_df,
  panel  = FALSE,
  est_method = "ipw"
)


agg_M_simple  <- aggte(att_results_M, type = "simple", na.rm = TRUE)
agg_M_group   <- aggte(att_results_M, type = "group", na.rm = TRUE)
agg_H_simple  <- aggte(att_results_H, type = "simple", na.rm = TRUE)
agg_H_group   <- aggte(att_results_H, type = "group", na.rm = TRUE)
agg_D_simple  <- aggte(att_results_D, type = "simple", na.rm = TRUE)
agg_D_group   <- aggte(att_results_D, type = "group", na.rm = TRUE)
agg_P_simple <- aggte(att_results_P, type = "simple", na.rm = TRUE)
agg_P_group <- aggte(att_results_P, type = "group", na.rm = TRUE)

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
    ATT_raw = round(att, digits),
    Std_Error = round(se, digits),
    p_value = p_val,
    stars = stars,
    ATT = paste0(round(att, digits), stars),
    CI_Lower = round(att - 1.96 * se, digits),
    CI_Upper = round(att + 1.96 * se, digits)
  )
}

results_df <- tibble(
  Outcome = c("M", "M", "H", "H", "Dis", "Dis", "P", "P"),
  Estimate = c("Simple ATT", "Group ATT", "Simple ATT", "Group ATT", "Simple ATT", "Group ATT", "Simple ATT", "Group ATT")
)

agg_list <- list(
  agg_M_simple, agg_M_group,
  agg_H_simple, agg_H_group,
  agg_D_simple, agg_D_group,
  agg_P_simple, agg_P_group
)

results_df <- results_df %>%
  mutate(
    info = map(agg_list, ~ add_significance_info(.x$overall.att, .x$overall.se))
  ) %>%
  unnest_wider(info)

### Table ----
table_reg <- results_df %>%
  select(Outcome, Estimate, ATT, Std_Error, CI_Lower, CI_Upper) %>%
  kable(format = "html", caption = "ATT Estimates with Significance Stars") %>%
  kable_styling(full_width = FALSE)

### Plot ----
event_M   <- aggte(att_results_M,   type = "dynamic", na.rm = TRUE)
event_H   <- aggte(att_results_H,   type = "dynamic", na.rm = TRUE)
event_D <- aggte(att_results_D,   type = "dynamic", na.rm = TRUE)
event_P <- aggte(att_results_P,   type = "dynamic", na.rm = TRUE)

extract_event_data <- function(event_obj, outcome_label) {
  tibble(time = event_obj$egt, att = event_obj$att.egt, se = event_obj$se.egt,
         ci_lo = att - 1.96 * se, ci_hi = att + 1.96 * se, 
         outcome = outcome_label)
}

event_data <- bind_rows(extract_event_data(event_M, "M"),
                        extract_event_data(event_H, "H"),
                        extract_event_data(event_D, "Dis"),
                        extract_event_data(event_P, "P"))

plot_reg <- ggplot(event_data, aes(x = time, y = att, color = outcome)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = outcome),
              alpha = 0.2, color = NA) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Dynamic Treatment Effects by Outcome",
       x = "Years Since Treatment (Event Time)",
       y = "ATT Estimate",
       color = "Outcome",
       fill = "Outcome") +
  theme_minimal(base_size = 14)

### Check IPW
# Estimate the propensity scores (the IPW weights)
#ps_model <- glm(I(G_year != 0) ~ enrollment + pupteach + sec_teach_pct + current_pp,
               # family = binomial(),
               # data = seg_df)

#seg_df$pscore <- predict(ps_model, type = "response")
#seg_df$ipw_weight <- ifelse(seg_df$G_year != 0, 1, seg_df$pscore / (1 - seg_df$pscore))

# Now check distribution
#summary(seg_df$ipw_weight)
#hist(seg_df$ipw_weight, breaks = 100, main = "Histogram of IPW Weights")

### Balance ----
bal.tab(
  x = G ~ enrollment + pupteach + sec_teach_pct + current_pp,
  weights = seg_df$ipw_weight,
  data = seg_df,
  method = "weighting"
)
love.plot(
  bal.tab(
    G ~ enrollment + pupteach + sec_teach_pct + current_pp,
    weights = seg_df$ipw_weight,
    data = seg_df,
    method = "weighting"
  ),
  thresholds = c(m = .1),  # adds reference line at SMD = 0.1
  var.order = "unadjusted", 
  abs = TRUE
)

## REG ----

seg_df_clean <- seg_df %>%
  drop_na(log_enroll, pupteach, pupteach_sq, sec_teach_pct, 
          sec_teach_pct_sq, log_currpp)

# M outcome
att_results_M <- att_gt(
  yname = "M",               
  tname = "year",
  idname = "unit_id",            
  gname = "G_year",   
  xformla = ~ log_enroll + pupteach + pupteach_sq + 
    sec_teach_pct + sec_teach_pct_sq + log_currpp,
  data = seg_df,
  panel = FALSE,
  est_method = "reg"
)

# H outcome
att_results_H <- att_gt(
  yname = "H",                
  tname = "year",
  idname = "unit_id",         
  gname = "G_year",   
  xformla = ~ poly_sec1 + poly_sec2 +
    log_currpp_p1 + log_enroll_p1 +
    poly_pupteach1 + poly_pupteach2,
  data = seg_df,
  panel = FALSE,
  est_method = "reg"
)

# D outcome
att_results_D <- att_gt(
  yname = "Dis",                
  tname = "year",
  idname = "unit_id",         
  gname = "G_year",   
  xformla = ~ log_currpp + log_enroll + pupteach + sec_teach_pct,
  data = seg_df,
  panel = FALSE,
  est_method = "reg"
)

# P outcome
att_results_P <- att_gt(
  yname  = "P_nonwhite_white",
  tname  = "year",
  idname = "unit_id",
  gname  = "G_year",
  xformla = ~ log_enroll + pupteach + pupteach_sq +
    sec_teach_pct + sec_teach_pct_sq +
    log_currpp,
  data   = seg_df,
  panel  = FALSE,
  est_method = "reg"
)


agg_M_simple  <- aggte(att_results_M, type = "simple", na.rm = TRUE)
agg_M_group   <- aggte(att_results_M, type = "group", na.rm = TRUE)
agg_H_simple  <- aggte(att_results_H, type = "simple", na.rm = TRUE)
agg_H_group   <- aggte(att_results_H, type = "group", na.rm = TRUE)
agg_D_simple  <- aggte(att_results_D, type = "simple", na.rm = TRUE)
agg_D_group   <- aggte(att_results_D, type = "group", na.rm = TRUE)
agg_P_simple <- aggte(att_results_P, type = "simple", na.rm = TRUE)
agg_P_group <- aggte(att_results_P, type = "group", na.rm = TRUE)

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
    ATT_raw = round(att, digits),
    Std_Error = round(se, digits),
    p_value = p_val,
    stars = stars,
    ATT = paste0(round(att, digits), stars),
    CI_Lower = round(att - 1.96 * se, digits),
    CI_Upper = round(att + 1.96 * se, digits)
  )
}

## try sep group and simple
# --- Prepare results table
results_df <- tibble(
  Outcome = c("M", "M", "H", "H", "Dis", "Dis", "P", "P"),
  Estimate = c("Simple ATT", "Group ATT", "Simple ATT", "Group ATT",
               "Simple ATT", "Group ATT", "Simple ATT", "Group ATT")
)

agg_list <- list(
  agg_M_simple, agg_M_group,
  agg_H_simple, agg_H_group,
  agg_D_simple, agg_D_group,
  agg_P_simple, agg_P_group
)

# Add significance information
results_df <- results_df %>%
  mutate(
    info = map(agg_list, ~ add_significance_info(.x$overall.att, .x$overall.se))
  ) %>%
  unnest_wider(info)

# --- Split into two tables
simple_table <- results_df %>%
  filter(Estimate == "Simple ATT") %>%
  select(Outcome, Estimate, ATT, Std_Error, CI_Lower, CI_Upper)

group_table <- results_df %>%
  filter(Estimate == "Group ATT") %>%
  select(Outcome, Estimate, ATT, Std_Error, CI_Lower, CI_Upper)

# --- Save to HTML
save_kable(
  simple_table %>%
    kable(format = "html", caption = "Simple ATT Estimates with Significance Stars") %>%
    kable_styling(full_width = FALSE),
  "../../main/district_simple.html"
)

save_kable(
  group_table %>%
    kable(format = "html", caption = "Group ATT Estimates with Significance Stars") %>%
    kable_styling(full_width = FALSE),
  "../../appendix/district_group.html"
)


### Plot ----
event_M   <- aggte(att_results_M,   type = "dynamic", na.rm = TRUE)
event_H   <- aggte(att_results_H,   type = "dynamic", na.rm = TRUE)
event_D <- aggte(att_results_D,   type = "dynamic", na.rm = TRUE)
event_P <- aggte(att_results_P,   type = "dynamic", na.rm = TRUE)

extract_event_data <- function(event_obj, outcome_label) {
  tibble(time = event_obj$egt, att = event_obj$att.egt, se = event_obj$se.egt,
         ci_lo = att - 1.96 * se, ci_hi = att + 1.96 * se, 
         outcome = outcome_label)
}

event_data <- bind_rows(extract_event_data(event_M, "M"),
                        extract_event_data(event_H, "H"),
                        extract_event_data(event_D, "Dis"),
                        extract_event_data(event_P, "P"))

saveRDS(list(simple_table = simple_table, group_table  = group_table,
             event_data   = event_data), file = "../../district_did.rds")

ggplot(event_data, aes(x = time, y = att, color = outcome)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = outcome),
              alpha = 0.2, color = NA) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Dynamic Treatment Effects by Outcome",
       x = "Years Since Treatment (Event Time)",
       y = "ATT Estimate",
       color = "Outcome",
       fill = "Outcome") +
  theme_minimal(base_size = 14)

ggsave("../../main/district_plot.png", width = 8, height = 5, dpi = 300)
