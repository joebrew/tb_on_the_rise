#####
# FIGURES
#####

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
pdf(file = 'figure_1_incidence_with_titles.pdf')
Rmisc::multiplot(b_incidence_over_time, #+
                   # ggtitle('A'),
          j_smeared_non_smeared_cases_over_time +
            theme(legend.position="bottom"), #+
            # ggtitle('C'),
          d_incidence_by_sex_over_time +
            theme(legend.position="bottom"), #+
            # ggtitle('B'),
          g_hiv_status_amont_incident_tb_over_time +
            theme(legend.position="bottom"), #+
            # ggtitle('D'),
          cols = 2)
dev.off()

# FIGURE 1: GENERALLY INCIDENCE TRENDS
label_df <- data.frame(x = c(2009, 2009),
                       y = c(400, 650),
                       label = c('Women', 'Men'))
# Rmisc::multiplot(
a_plot <- 
  b_incidence_over_time +
  ggtitle('A') +
  scale_x_discrete(limit = c(1997:2012)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
a <- ggplot_gtable(ggplot_build(a_plot))

# b_plot <- j_smeared_non_smeared_cases_over_time_with_line
b_plot <- j_smeared_non_smeared_cases_over_time
b <- b_plot

c_plot <- d_incidence_by_sex_over_time +
  theme(legend.position="none") +
  ggtitle('B') +
  scale_x_discrete(limit = c(1997:2012)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(data = label_df,
            aes(x = x, y = y, label = label),
            alpha = 0.6)
c <- ggplot_gtable(ggplot_build(c_plot))

d_plot <- g_hiv_status_amont_incident_tb_over_time +
  theme(legend.position="bottom") +
  ggtitle('D') +
  scale_x_discrete(limit = c(1997:2012)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
d <- ggplot_gtable(ggplot_build(d_plot))
pdf(file = 'figure_1_incidence_with_letters.pdf', width = 10, height = 7)
grid.arrange(a, c, b, d)
dev.off()
# png(filename = 'figure_1_incidence.png',
#     width = 500, height = 500)
# Rmisc::multiplot(b_incidence_over_time,
#                  j_smeared_non_smeared_cases_over_time +
#                    theme(legend.position="bottom") +
#                    ggtitle('Patients tested for sputum smear'),
#                  d_incidence_by_sex_over_time +
#                    theme(legend.position="top"),
#                  g_hiv_status_amont_incident_tb_over_time +
#                    theme(legend.position="bottom"),
#                  cols = 2)
# dev.off()



# FIGURE 2: RISK FACTORS

lay_out = function(...) {    
  x <- list(...)
  n <- max(sapply(x, function(x) max(x[[2]])))
  p <- max(sapply(x, function(x) max(x[[3]])))
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(n, p)))    
  
  for (i in seq_len(length(x))) {
    print(x[[i]][[1]], vp = grid::viewport(layout.pos.row = x[[i]][[2]], 
                                           layout.pos.col = x[[i]][[3]]))
  }
} 

pdf(file = 'figure_2_risk_factors_with_letters.pdf',
    height = 7, width = 9)
lay_out(list(lines4 +
               ggtitle('A'), 1, 1:2),
           list(h_total_tb_and_hiv_coinfections +
                  theme(legend.position = 'bottom') +
                  ggtitle('B'), 2, 1),
           list(zzz + ggtitle('C') +
                  theme(legend.position="bottom"), 2, 2))
dev.off()

# New version for resubmission

pdf(file = 'resubmission_combined_figure.pdf',
    height = 11, width = 9)
lay_out(
  list(a_plot, 1, 1),
  list(b_plot +
         ggtitle('B') +
         theme(legend.position="bottom") +
         scale_x_discrete(limit = c(1997:2012)), 1, 2),
  list(c_plot +
         ggtitle('C'), 2, 1),
  list(d_plot +
         ggtitle('D'), 2, 2),
  list(lines4 +
         ggtitle('E'), 3, 1:2),
  list(h_total_tb_and_hiv_coinfections +
         theme(legend.position = 'bottom') +
         ggtitle('F'), 4, 1),
  list(zzz + ggtitle('G') +
         theme(legend.position="bottom"), 4, 2))
dev.off()

# Print all figures individually
setwd('~/Desktop/figures_for_alberto')
jpeg(filename = '1a.jpeg', width = 800, height = 800)
plot(a)
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

# # Get total for each period
# tbl1 <-
#   tbl1 %>%
#   group_by(period) %>%
#   dplyr::mutate(total = sum(n))
# tbl1$total <- tbl1$n

# Get percentage by hiv status and period
tbl1 <-
  tbl1 %>%
  group_by(period, hiv_status) %>%
  dplyr::mutate(p = round(n / sum(n) * 100, digits = 2))

# Combine n and p
tbl1$n <-
  ifelse(tbl1$n > 1,
         paste0(tbl1$n, ' (', tbl1$p, '%)'),
         '')
tbl1$p <- NULL
tbl1 <- spread(data = tbl1,
       key = hiv_status,
       value = n) #%>%
  # mutate(negative = ifelse(is.na(negative), 0, negative),
  #        positive = ifelse(is.na(positive), 0, positive),
  #        unknown = ifelse(is.na(unknown), 0, unknown))


tbl1 <- tbl1 %>%
  mutate(period = as.character(period)) %>%
  mutate(period = capitalize(period),
         ttm_result = capitalize(ttm_result))
#
# tbl1$negative[tbl1$negative == 0] <- ''
# tbl1$positive[tbl1$positive <= 1] <- ''
# Get percentage by period
#
# tbl1 <- tbl1 %>%
#   group_by(Period) %>%
#   mutate(negative = ifelse(period %in% c('2005-2008',
#                                          '2009-2012'),
#                            paste0(negative,
#                                   ' (',
#                                   round(100 * as.numeric(negative) /
#                                           sum(as.numeric(negative), na.rm = T), digits = 1),
#                                   '%)'),
#                            negative),
#          positive = ifelse(period %in% c('2005-2008',
#                                          '2009-2012'),
#                            paste0(positive,
#                                   ' (',
#                                   round(100 * as.numeric(positive) /
#                                           sum(as.numeric(positive), na.rm = T), digits = 1),
#                                   '%)'),
#                            positive),
#          unknown = ifelse(period %in% c('2005-2008',
#                                          '2009-2012'),
#                            paste0(unknown,
#                                   ' (',
#                                   round(100 * as.numeric(unknown) /
#                                           sum(as.numeric(unknown), na.rm = T), digits = 1),
#                                   '%)'),
#                            unknown))

names(tbl1) <- capitalize(names(tbl1))
tbl1 <- data.frame(tbl1)
names(tbl1)[2] <- 'Treatment outcome'



# # Rearrange columns
# tbl1 <-
#   tbl1[,c('Period',
#           'Treatment outcome',
#           'Negative',
#           'Positive',
#           'Unknown',
#           'Total')]

# Add a column for totals
extract_number <- function(z){
  ifelse(is.na(z) | z == '', 0,
         as.numeric(lapply(strsplit(z, ' '),
                                  function(x){x[[1]]})))
  }

tbl1$Total <-
  extract_number(tbl1$Negative) +
  c(extract_number(tbl1$Positive[1:12]), extract_number(tbl1$Positive[13:24])) +
  extract_number(tbl1$Unknown)

# Get Total of each period
tbl1 <-
  tbl1 %>%
  group_by(Period) %>%
  dplyr::mutate(Total = paste0(Total,
                        ' (',
                        round(Total / sum(Total) * 100, digits = 2),
                        '%)'))

# Remove the repetivie periods
tbl1$Period <- as.character(tbl1$Period)
tbl1$Period[c(2:6, 8:12, 14:18, 20:24)] <- ''

stargazer(tbl1,
          # type = 'html',
          title = 'Treatment outcomes by time and HIV status',
          summary = FALSE,
          rownames = FALSE)

