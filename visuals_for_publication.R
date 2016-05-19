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
multiplot(a_incidence_by_age,
          k_hiv_status_among_incident_extrapul_tb_over_time,
          f_incidence_by_sex_and_age,
          h_total_tb_and_hiv_coinfections,
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


# TREATMENT RESULTS BY SEX AND HIV STATUS
tbl1 <- tb %>%
  filter(incident_case) %>%
  group_by(sex, hiv_status, ttm_result) %>%
  tally %>%
  filter(!is.na(ttm_result))

tbl1 <- spread(data = tbl1, 
       key = hiv_status, 
       value = n) %>%
  mutate(sex = capitalize(sex),
         ttm_result = capitalize(ttm_result))
names(tbl1) <- capitalize(names(tbl1))
tbl1 <- data.frame(tbl1)
names(tbl1)[2] <- 'Treatment outcome'

# Remove the repetivie genders
tbl1$Sex[c(2:6, 8:12)] <- ''

stargazer(tbl1, 
          # type = 'html',
          title = 'Treatment outcomes by sex and HIV status',
          summary = FALSE,
          rownames = FALSE)

