#####
# FIGURES
#####

library(Rmisc)
library(extrafont)
# Run visualize.R first
a_incidence_by_age
b_incidence_over_time
c_incidence_by_age
d_incidence_by_sex_over_time
e_incidence_by_age_over_time
f_incidence_by_sex_and_age
g_hiv_status_amont_incident_tb_over_time
h_total_tb_and_hiv_coinfections
i_incidence_smear_cases_tested_over_time
j_smeared_non_smeared_cases_over_time
k_hiv_status_among_incident_extrapul_tb_over_time
l_percentage_of_previously_treated_patients_among_incident_cases_over_time
m_outcomes_over_time
n_outcomes_by_previous_status
o_lost_to_followup_by_age
p_death_by_age

# FIGURE 1: GENERALLY INCIDENCE TRENDS
pdf(file = 'figure_1_incidence.pdf')
multiplot(b_incidence_over_time,
          j_smeared_non_smeared_cases_over_time,
          d_incidence_by_sex_over_time,
          g_hiv_status_amont_incident_tb_over_time,
          cols = 2)
dev.off()

# FIGURE 2: RISK FACTORS
pdf(file = 'figure_2_risk_factors.pdf')
multiplot(k_hiv_status_among_incident_extrapul_tb_over_time,
          h_total_tb_and_hiv_coinfections,
          f_incidence_by_sex_and_age,
          p_death_by_age,
          cols = 2)
dev.off()

# ALL FIGURES IN ONE DOCUMENT
pdf(file = 'all_figures.pdf', height = 4.5, width = 4.5)
a_incidence_by_age
b_incidence_over_time
c_incidence_by_age
d_incidence_by_sex_over_time
e_incidence_by_age_over_time
f_incidence_by_sex_and_age
g_hiv_status_amont_incident_tb_over_time
h_total_tb_and_hiv_coinfections
i_incidence_smear_cases_tested_over_time
j_smeared_non_smeared_cases_over_time
k_hiv_status_among_incident_extrapul_tb_over_time
l_percentage_of_previously_treated_patients_among_incident_cases_over_time
m_outcomes_over_time
n_outcomes_by_previous_status
o_lost_to_followup_by_age
p_death_by_age
dev.off()

#####
# TABLES
#####
library(stargazer)
library(Hmisc)
library(tidyr)

# Get time period
time_periods <- 
  data.frame(year = 1997:2012,
             period = c(rep('1997-2000', 4), 
                        rep('2001-2004', 4), 
                        rep('2005-2008', 4), 
                        rep('2009-2012', 4)))
tb <- tb %>%
  left_join(time_periods)

# TREATMENT RESULTS BY SEX AND HIV STATUS
tbl1 <- tb %>%
  filter(incident_case) %>%
  group_by(period, hiv_status, ttm_result) %>%
  tally %>%
  filter(!is.na(ttm_result)) %>%
  mutate(n = ifelse(is.na(n), 0, n))

tbl1 <- spread(data = tbl1, 
       key = hiv_status, 
       value = n) %>%
  mutate(negative = ifelse(is.na(negative), 0, negative),
         positive = ifelse(is.na(positive), 0, positive),
         unknown = ifelse(is.na(unknown), 0, unknown)) %>%
  mutate(period = as.character(period)) %>%
  mutate(period = capitalize(period),
         ttm_result = capitalize(ttm_result)) %>%
  group_by(ttm_result) %>%
  mutate(negative = paste0(negative, 
                           ' (',
                           round(100 * negative / sum(negative), digits = 1),
                           '%)'),
         positive = paste0(positive, 
                           ' (',
                           round(100 * positive / sum(positive), digits = 1),
                           '%)'),
         unknown = paste0(unknown, 
                           ' (',
                           round(100 * unknown / sum(unknown), digits = 1),
                           '%)'))
names(tbl1) <- capitalize(names(tbl1))
tbl1 <- data.frame(tbl1)
names(tbl1)[2] <- 'Treatment outcome'

# Remove the repetivie periods
tbl1$Period[c(2:6, 8:12, 14:18, 20:24)] <- ''

stargazer(tbl1, 
          # type = 'html',
          title = 'Treatment outcomes by time and HIV status',
          summary = FALSE,
          rownames = FALSE)

