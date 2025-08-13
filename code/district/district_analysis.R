library(tidyverse)
library(segregation)
library(did)
library(cobalt)  
setwd("C:/Users/sophi/Documents/LSE/Education/school_segregation/data")
fix_leaid <- function(df) {
  df %>%
    mutate(
      LEAID = str_pad(as.character(LEAID), 7, pad = "0"))
  }

# Measures ----
all_data <- read_csv("school_count_data.csv")%>%
  mutate(LEAID = as.character(LEAID), NCESSCH = as.character(NCESSCH)) %>%
  fix_leaid()

long_df <- all_data %>% #setup data
  pivot_longer(cols = c(white, black, hispanic, asian, other),
               names_to = "race", values_to = "n")

## Calc Dis ----

############## question!!! whats "n" for weight!!! is it correct???

dis_long <- long_df %>%
  mutate(race_bin = if_else(race == "white", "white", "nonwhite"))

dis_df <- dis_long %>%
  group_by(LEAID, grade, year) %>%
  group_modify(~ dissimilarity(.x,
                               group = "race_bin",
                               unit  = "NCESSCH",
                               weight = "n")) %>%
  ungroup() %>% select(-stat) %>% rename(Dis = est)          

## Calc M and H ----
seg_df <- long_df %>%
  group_by(LEAID, grade, year) %>%
  group_modify(~ mutual_total(.x, group = "race", unit = "NCESSCH", 
                              weight = "n")) %>% ungroup() %>%
  #this calculates within-district segregation!!!
  
  pivot_wider(names_from = stat, values_from = est) 

## Calc Exposure (P*) ----

#  • P_white_nonwhite  = White exposure to non-white peers
#  • P_nonwhite_white  = Non-white exposure to white peers
#  • I_white           = White isolation (white exposure to itself)

exp_raw <- long_df %>%
  mutate(race_bin = if_else(race == "white", "white", "nonwhite")) %>%
  group_by(LEAID, grade, year) %>%
  group_modify(~ exposure(
    .x,
    group = "race_bin",
    unit = "NCESSCH",
    weight = "n"
  )) %>%
  ungroup()

exp_df <- exp_raw %>%
  filter(
    (of == "white"     & to == "nonwhite") |
      (of == "nonwhite" & to == "white")     |
      (of == "white"     & to == "white")
  ) %>%
  mutate(
    stat = case_when(
      of == "white"     & to == "nonwhite" ~ "P_white_nonwhite",
      of == "nonwhite" & to == "white"     ~ "P_nonwhite_white",
      of == "white"     & to == "white"    ~ "P_white_white"
    )
  ) %>%
  select(LEAID, grade, year, stat, exposure) %>%
  pivot_wider(
    names_from = stat,
    values_from = exposure
  )

##### to addd!!!!!!!!!

### decompose M and H 

seg_decomp <- long_df %>% 
  group_by(year, grade) %>% 
  group_modify(~{
    tot    <- mutual_total(.x, "race", "NCESSCH", weight = "n")
    within <- mutual_total(.x, "race", "NCESSCH", within = "LEAID", weight = "n")
    tibble(
      M_total = tot$est[tot$stat=="M"],
      M_within = within$est[within$stat=="M"],
      M_between = M_total - M_within,
      H_total = tot$est[tot$stat=="H"],
      H_within = within$est[within$stat=="H"],
      H_between = H_total - H_within
    )
  }) %>% 
  ungroup()

write_csv(seg_decomp, "theil_seg_decomposition.csv")

#seg_decomp %>%
#  mutate(M_within_ratio = M_within / M_total) %>%
#  ggplot(aes(x = year, y = M_within_ratio, color = as.factor(grade))) +
#  geom_line(size = 1) +
#  labs(
#    title = "Share of Total School Segregation Due to Within-District Sorting",
#    x = "Year",
#    y = "Within-District Share of Mutual Information (M)",
#    color = "Grade") +  theme_minimal() + ylim(0, 1)


# Join in
g_year_df <- all_data %>%
  filter(D == 1) %>%
  group_by(LEAID, grade) %>%
  summarise(G_year = min(year, na.rm = TRUE), .groups = "drop")

seg_df <- seg_df %>%
  left_join(dis_df, by = c("LEAID", "grade", "year")) %>%
  left_join(all_data %>%
              select(LEAID, grade, year, D, G) %>%
              distinct(),
            by = c("LEAID", "grade", "year")) %>%
  left_join(g_year_df, by = c("LEAID", "grade")) %>%
  mutate(
    G_year = if_else(G == 0, 0, G_year),
    unit_id = as.numeric(factor(paste0(LEAID, "_", grade)))
  )

seg_df <- seg_df %>%
  left_join(exp_df, by = c("LEAID", "grade", "year")) #add exp


write_csv(seg_df, "theil_seg_measures.csv")

# Add Covariates ----

## Post 1998 ----

dis_cov <- read_csv("cov/ELSI_csv_export_district.csv", skip = 6, 
                    col_types = cols(.default = "c"))
leaid_keep <- readRDS("../code/matching/leaid_keep.rds")

nyc_known_leaids <- c("3600075", "3600076", "3600077", "3600078", "3600079", "3600081", 
                      "3600083", "3600084","3600085", "3600086", "3600087", "3600088", 
                      "3600090", "3600091", "3600092","3600094", "3600095", "3600096", 
                      "3600097", "3600098", "3600099", "3600100","3600101", "3600102", 
                      "3600103", "3600119", "3600120", "3600121", "3600122","3600123", 
                      "3600135", "3600151", "3600152", "3600153")

ccd_long <- dis_cov %>% 
  pivot_longer(cols = matches("\\d{4}-\\d{2}$"), 
               names_to  = "raw_name", values_to = "value") %>%
  
  separate(raw_name, into = c("var_label", "school_year"),
           sep  = " (?=\\d{4}-\\d{2}$)", remove = FALSE) %>%
  
  mutate(
    year = as.integer(substr(school_year, 1, 4)),
    value = as.numeric(value),
    LEAID = str_pad(as.character
                    (`Agency ID - NCES Assigned [District] Latest available year`), 
                    7, pad = "0")) %>% filter(LEAID %in% leaid_keep) 

ccd_recode <- ccd_long %>% 
  mutate(var = case_when(
    str_detect(var_label, fixed("Total Number Operational Schools [Public School]")) ~ "n_sch_raw",
    str_detect(var_label, fixed("Total Number Operational Charter Schools [Public School]")) ~ "n_charter_raw",
    str_detect(var_label, fixed("Locale [District]")) ~ "loc",
    str_detect(var_label, fixed("Pupil/Teacher Ratio [District]")) ~ "pupteach",
    str_detect(var_label, fixed("Secondary Teachers [District]")) ~ "sec_teacher_raw",
    str_detect(var_label, fixed("Full-Time Equivalent (FTE) Teachers [District]")) ~ "fte_teacher_raw",
    str_detect(var_label, fixed("Secondary School Counselor [District]")) ~ "sec_counselor_raw",
    str_detect(var_label, fixed("Total Staff [District]")) ~ "total_staff_raw",
    str_detect(var_label, fixed("Local Rev. - Property Taxes (T06) [District Finance]")) ~ "prop_tax_rev",
    str_detect(var_label, fixed("Total Revenue (TOTALREV) [District Finance]")) ~ "total_rev",
    str_detect(var_label, fixed("Total Revenue - Local Sources (TLOCREV) [District Finance]")) ~ "local_rev",
    str_detect(var_label, fixed("Total Revenue (TOTALREV) per Pupil (V33) [District Finance]")) ~ "total_rev_pp",
    str_detect(var_label, fixed("Instructional Expenditures (E13) per Pupil (V33) [District Finance]")) ~ "inst_pp_e13",
    str_detect(var_label, fixed("Total Current Expenditures - Instruction (TCURINST) per Pupil (V33) [District Finance]")) ~ "inst_pp_tcur",
    str_detect(var_label, fixed("Total Current Expenditures - Instruction (TCURINST) as Percentage")) ~ "inst_pct_tcur",
    str_detect(var_label, fixed("Total Current Expenditures - Support Services (TCURSSVC) per Pupil")) ~ "support_pp",
    str_detect(var_label, fixed("Total Current Expenditures - Support Services (TCURSSVC) as Percentage")) ~ "support_pct",
    str_detect(var_label, fixed("Total Current Expenditures (TCURELSC) per Pupil")) ~ "current_pp",
    str_detect(var_label, fixed("Fall Membership (V33) [District Finance]")) ~ "enrollment",
    TRUE ~ NA_character_)) %>% filter(!is.na(var))

ccd_override <- ccd_recode %>%
  mutate(LEAID = case_when(
    # NYC: aggregate ALL subdistricts into 3620580 starting in 2005
    LEAID %in% nyc_known_leaids & year >= 2005 ~ "3620580",
    # Detroit: override 2612000 with 2601103 starting in 2016
    LEAID == "2601103" & year >= 2016 ~ "2612000",
    # Memphis: override 4700148 with 4700148 starting in 2013
    LEAID == "4700148" & year >= 2013 ~ "4702940",
    TRUE ~ LEAID)) %>%
  group_by(LEAID, year, var) %>%
  summarise(value = case_when(is.na(first(LEAID)) ~ NA_real_,
                              first(LEAID) == "3620580" ~ sum(value, na.rm = TRUE),
                              TRUE ~ last(value[!is.na(value)])), .groups = "drop")

ccd_wide <- ccd_override %>%
  pivot_wider(names_from  = var, values_from = value) %>%
  mutate(charter_percent = 100 * n_charter_raw / n_sch_raw,
         sec_teach_pct = 100 * sec_teacher_raw / fte_teacher_raw,
         non_teacher_staff = total_staff_raw - fte_teacher_raw,
         sec_counselor_pct = 100 * sec_counselor_raw / non_teacher_staff,
         prop_tax_share = 100 * prop_tax_rev / total_rev)

ccd_final <- ccd_wide %>% 
  transmute(leaid = LEAID, year, n_sch = n_sch_raw, charter_percent, loc,
            pupteach, sec_teach_pct, sec_counselor_pct, prop_tax_share,
            enrollment, inst_pp_tcur, support_pp, current_pp, total_rev_pp)


## Pre 1998 ----
leg_raw <- read_csv("cov/ELSI_csv_export_prey_dis_cov.csv", skip = 6, 
                    col_types = cols(.default = "c"))

leg_long <- leg_raw %>% 
  pivot_longer(cols = matches("\\d{4}-\\d{2}$"), names_to  = "raw_name",
               values_to = "value") %>% 
  separate(raw_name, into = c("var_label", "school_year"),
           sep = " (?=\\d{4}-\\d{2}$)",remove = FALSE) %>% 
  mutate(year = as.integer(substr(school_year, 1, 4))) %>%
  filter(year >= 1994)

leg_rec <- leg_long %>% 
  mutate(var = case_when(
    str_detect(var_label, fixed("Total Number Operational Schools [Public School]")) ~ "n_sch_raw",
    str_detect(var_label, fixed("Locale [District]")) ~ "loc",
    str_detect(var_label, fixed("Pupil/Teacher Ratio [District]"))  ~ "pupteach",
    str_detect(var_label, fixed("Secondary Teachers [District]"))  ~ "sec_teacher_raw",
    str_detect(var_label, fixed("Full-Time Equivalent (FTE) Teachers [District]")) ~ "fte_teacher_raw",
    str_detect(var_label, fixed("Total Staff [District]"))  ~ "total_staff_raw",
    str_detect(var_label, fixed("Local Rev. - Property Taxes (T06) [District Finance]")) ~ "prop_tax_rev",
    str_detect(var_label, fixed("Total Revenue (TOTALREV) [District Finance]")) ~ "total_rev",
    str_detect(var_label, fixed("Total Revenue (TOTALREV) per Pupil (V33) [District Finance]")) ~ "total_rev_pp",
    str_detect(var_label, fixed("Instructional Expenditures (E13) per Pupil (V33) [District Finance]")) ~ "inst_pp_e13",
    str_detect(var_label, fixed("Total Current Expenditures - Instruction (TCURINST) per Pupil")) ~ "inst_pp_tcur",
    str_detect(var_label, fixed("Total Current Expenditures - Support Services (TCURSSVC) per Pupil")) ~ "support_pp",
    str_detect(var_label, fixed("Total Current Expenditures (TCURELSC) per Pupil")) ~ "current_pp",
    str_detect(var_label, fixed("Fall Membership (V33) [District Finance]")) ~ "enrollment",
    str_detect(var_label, fixed("Total Students All Grades (Excludes AE) [District]")) ~ "enroll_alt",
    TRUE ~ NA_character_)) %>% filter(!is.na(var))

leg_rec <- leg_rec %>% # Ensure LEAID is character
  mutate(LEAID = str_pad(as.character(`Agency ID - NCES Assigned [District] Latest available year`), 7, pad = "0"))

leg_wide <- leg_rec %>% 
  mutate(LEAID = as.character(LEAID))%>%
  select(LEAID, year, var, value) %>% 
  mutate(value = as.numeric(value)) %>% 
  pivot_wider(names_from  = var, values_from = value,
              values_fn = list(value = mean)) %>% 
  mutate(enrollment = coalesce(enrollment, enroll_alt),
         sec_teach_pct = 100 * sec_teacher_raw / fte_teacher_raw,
         prop_tax_share = 100 * prop_tax_rev / total_rev)

legacy_final <- leg_wide %>% 
  transmute(leaid = LEAID, year, n_sch = n_sch_raw,
            charter_percent = NA_real_, # not in 1994-98 file
            loc, pupteach, sec_teach_pct,
            sec_counselor_pct = NA_real_, # counsellors not present
            prop_tax_share, enrollment, inst_pp_tcur, # == NA when absent
            support_pp,current_pp, total_rev_pp) %>% filter(year >= 1994)

ccd_full <- full_join(ccd_final, legacy_final, by = c("leaid", "year"), 
                      suffix = c(".new", ".legacy")) %>%
  mutate(n_sch = coalesce(n_sch.legacy, n_sch.new),
         charter_percent = coalesce(charter_percent.legacy, charter_percent.new),
         loc = coalesce(loc.legacy, loc.new),
         pupteach = coalesce(pupteach.legacy, pupteach.new),
         sec_teach_pct = coalesce(sec_teach_pct.legacy, sec_teach_pct.new),
         sec_counselor_pct = coalesce(sec_counselor_pct.legacy, 
                                      sec_counselor_pct.new),
         prop_tax_share = coalesce(prop_tax_share.legacy, prop_tax_share.new),
         enrollment = coalesce(enrollment.legacy, enrollment.new),
         inst_pp_tcur = coalesce(inst_pp_tcur.legacy, inst_pp_tcur.new),
         support_pp = coalesce(support_pp.legacy, support_pp.new),
         current_pp  = coalesce(current_pp.legacy, current_pp.new),
         total_rev_pp = coalesce(total_rev_pp.legacy, total_rev_pp.new)) %>%
  
  select(leaid, year, n_sch, charter_percent, loc, pupteach,
         sec_teach_pct, sec_counselor_pct, prop_tax_share, enrollment,
         inst_pp_tcur, support_pp, current_pp, total_rev_pp)

write.csv(ccd_full, "dis_cov.csv", row.names = FALSE)

## Merge with seg ----

seg_df <- read_csv("theil_seg_measures.csv")%>%
  mutate(LEAID = as.character(LEAID)) %>% 
  fix_leaid()

seg_df <- seg_df %>% filter(year >= 1994)

ccd_full <- ccd_full %>% rename(LEAID = leaid)

seg_df <- seg_df %>%
  left_join(ccd_full, by = c("LEAID", "year")) %>%
  mutate(core_exp_pp = inst_pp_tcur + support_pp,
         avg_sch_size = enrollment / n_sch) %>% 
  arrange(LEAID, year) %>% 
  group_by(LEAID) %>% 
  tidyr::fill(current_pp, enrollment, pupteach, sec_teach_pct,
              .direction = "downup")

write_csv(seg_df, "theil_seg_measures_with_dis_cov.csv")

#length(unique(seg_df$year))
