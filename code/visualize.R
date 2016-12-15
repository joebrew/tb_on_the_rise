# Directories
root <- getwd() # should be the tb_on_the_rise directory
setwd('data')
data_dir <- getwd()
setwd(root)
setwd('code')
code_dir <- getwd()
setwd(root)

# Libraries

# library(Rmisc)
# library(extrafont)
library(foreign)
library(dplyr)
library(ggplot2)
library(readr)
library(ggthemes)
library(gtable)
library(grid)
library(gridExtra)

# Read and clean
source(paste0(code_dir, '/read_and_clean.R'))

# Remove duplicates

# Theme for plotting
source(paste0(code_dir, '/theme.R'))

# Write csv for alberto
readr::write_csv(tb, '~/Desktop/tb.csv')

##### RESULTS SECTION

# Overall and demographic characteristics

# Number of overall cases
length(which(tb$incident_case))

# Distribution by geography
table(tb$health_unit_register[tb$incident_case])
prop.table(table(tb$health_unit_register[tb$incident_case])) * 100

# Mean age of registered tb patients
summary(tb$years_old[tb$incident_case], na.rm = TRUE)
sd(tb$years_old[tb$incident_case], na.rm = TRUE)

# Mode
tb %>%
  filter(incident_case) %>%
  group_by(years_old_round) %>%
  tally() %>%
  arrange(desc(n))

# Sex
table(tb$gender[tb$incident_case])
prop.table(table(tb$gender[tb$incident_case]))

# Overall incidence by age group
temp <- 
  tb %>%
  filter(incident_case) %>%
  group_by(age_group) %>%
  summarise(cases = n()) %>%
  # join to population (annual AVERAGE denominator)
  left_join(population %>%
              group_by(age_group) %>%
              summarise(n = sum(n, na.rm = TRUE))) %>%
  ungroup %>%
  # get percentage
  mutate(p = cases / n * 100,
         incidence = cases / n * 100000) %>%
  filter(!is.na(age_group)) %>%
  arrange(age_group) 

# Greatest NUMBER of cases
temp %>% arrange(desc(cases)) %>% mutate(p_of_all = cases / sum(cases) * 100)

# Greatest INCIDENCE rate of cases
temp %>% arrange(desc(incidence))

# Figure of overall incidence by age group
ggplot(data = temp,
       aes(x = age_group,
           y = incidence)) +
  geom_bar(stat = 'identity',
           fill = 'darkorange',
           alpha = 0.6) +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  xlab('Age group') +
  ylab('Incidence rate (per 100,000)') +
  ggtitle('Average TB incidence by age group')

a_incidence_by_age <- last_plot()

# Population of district over all years
temp <- population %>%
  filter(year >= 1997,
         year <= 2012) %>%
  group_by(year) %>%
  summarise(n = sum(n))
temp

# Growth over those years
(temp$n[length(temp$n)] / temp$n[1] * 100) - 100

# Incident rate per 100,000
incidence <- 
  tb %>%
  filter(incident_case) %>%
  filter(year >= 1997,
         year <= 2012) %>%
  group_by(year, age_group, sex) %>%
  summarise(cases = n()) %>%
  # join to population for period
  left_join(population %>%
              filter(year >= 1997,
                     year <= 2012) %>%
              group_by(age_group, year, sex) %>%
              summarise(n = sum(n, na.rm = TRUE))) %>%
              ungroup %>%
  # calculate incidence per 100000
  mutate(incidence = cases / n * 100000)

# Absolute numbers from 2008-2012 versus 1997-2001
temp <- 
  incidence %>%
  group_by(time_period = ifelse(year >= 2008 & year <= 2012,
                                '2008-2012',
                                ifelse(year >= 1997 & year <= 2001,
                                       '1997-2001',
                                       NA))) %>%
  summarise(cases = sum(cases)) %>%
  filter(!is.na(temp$time_period))
# Growth in cases from 2008-2012 compared to 1997-2001
(temp$cases[temp$time_period == '2008-2012'] / 
  temp$cases[temp$time_period == '1997-2001'] * 100) -100

# Get incidence per 100000

# total number of incident cases during study period
nrow(tb[tb$incident_case,])
table(tb$incident_case)
prop.table(table(tb$incident_case))

# Get incidence not broken down by age group or sex
ts <- incidence %>%
  group_by(year) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>%
  mutate(incidence = cases / n * 100000)

ggplot(data = ts,
       aes(x = year, y = incidence)) +
  geom_area(fill = 'darkgrey',
            alpha = 0.8) +
  xlab('Year') +
  ylab('Incidence rate (per 100,000)') +
  theme_tb() +
  ggtitle('TB incidence over time')
b_incidence_over_time <- last_plot()


# Overall incidence by age group
temp <- 
  tb %>%
  filter(incident_case) %>%
  group_by(age_group) %>%
  summarise(cases = n()) %>%
  # join to population (annual AVERAGE denominator)
  left_join(population %>%
              group_by(age_group) %>%
              summarise(n = sum(n, na.rm = TRUE))) %>%
  ungroup %>%
  # get percentage
  mutate(p = cases / n * 100,
         incidence = cases / n * 100000) %>%
  filter(!is.na(age_group)) %>%
  arrange(age_group) 

# Figure of overall incidence by age group
ggplot(data = temp,
       aes(x = age_group,
           y = incidence)) +
  geom_bar(stat = 'identity',
           fill = 'darkorange',
           alpha = 0.6) +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  xlab('Age group') +
  ylab('Incidence rate (per 100,000)') +
  ggtitle('Average TB incidence by age group')
c_incidence_by_age <- last_plot()

# Plot incidence by sex
temp <- 
  incidence %>%
  group_by(year, sex = Hmisc::capitalize(sex)) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>%
  mutate(incidence = cases / n * 100000)

# cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(temp$sex)))
cols <- c(grey(0.2), grey(0.6))
ggplot(data = temp,
       aes(x = year,
           y = incidence)) +
  geom_line(aes(color = sex, group = sex),
            alpha = 0.6, size = 2) +
  scale_color_manual(name = 'Sex',
                     values = cols) +
  guides(col = guide_legend(reverse = TRUE)) +
  xlab('Year') +
  ylab('Incidence rate (per 100,000)') +
  ggtitle('Incidence rate over time by sex') +
  theme_tb()
d_incidence_by_sex_over_time <- last_plot()

# Plot incidence by age group AND year
temp <- 
  incidence %>%
  group_by(year, age_group) %>%
  summarise(cases = sum(cases, na.rm = TRUE),
            n = sum(n, na.rm = TRUE)) %>%
  mutate(incidence = cases / n * 100000)

cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(temp$age_group)))
ggplot(data = temp,
       aes(x = year,
           y = incidence)) +
  geom_line(aes(color = age_group, group = age_group)) +
  scale_color_manual(name = 'Age group',
                     values = cols) +
  xlab('Year') +
  ylab('Incidence rate (per 100,000)') +
  ggtitle('Incidence rate over time by age group') +
  theme_tb()
e_incidence_by_age_over_time <- last_plot()

# Which group over all time?
temp <- 
  incidence %>%
  group_by(age_group, sex) %>%
  summarise(cases = sum(cases),
            n = sum(n)) %>%
  mutate(incidence = cases / n * 100000) %>%
  ungroup %>%
  arrange(desc(incidence)) %>%
  filter(!is.na(age_group))

# Compared to women?
temp$incidence[temp$age_group == '60-64' &
                 temp$sex == 'male']
temp$incidence[temp$age_group == '60-64' &
                 temp$sex == 'female']

# Chart men vs. women full period (by age group)
cols <- adjustcolor(c('darkgreen', 'darkorange'), alpha.f = 0.6)
temp$sex <- Hmisc::capitalize(temp$sex)
ggplot(data = temp,
       aes(x = age_group, y = incidence, group = sex, fill = sex)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('Age group') +
  ylab('Incidence rate (per 100,000)') +
  ggtitle('Incidence rate by sex and age group') +
  scale_fill_manual(name = 'Sex',
                    values = cols) +
  guides(fill = guide_legend(reverse = TRUE))
f_incidence_by_sex_and_age <- last_plot()

# Sex, age group, HIV and TB
temp <- 
  tb %>%
  filter(incident_case) %>%
  filter(year >= 1997,
         year <= 2012) %>%
  group_by(year, age_group, sex) %>%
  summarise(cases = n()) %>%
  # join to population for period
  left_join(population %>%
              filter(year >= 1997,
                     year <= 2012) %>%
              group_by(age_group, year, sex) %>%
              summarise(n = sum(n, na.rm = TRUE))) %>%
  ungroup %>%
  # calculate incidence per 100000
  mutate(incidence = cases / n * 100000) %>%
  # Get full period
  group_by(age_group, sex) %>%
  summarise(cases = sum(cases),
            n = sum(n)) %>%
  # Get the hiv incidence data in there too
  left_join(tb %>%
              filter(incident_case) %>%
              filter(year >= 1997,
                     year <= 2012) %>%
              group_by(age_group, sex) %>%
              summarise(hiv_cases = length(which(hiv_status == 'positive')))) %>%
  mutate(incidence = cases / n * 100000,
         co_incidence = hiv_cases / n * 100000) %>%
  filter(!is.na(age_group))

# Get long
temp_long <- tidyr::gather(temp, key, value, incidence:co_incidence)

cols <- adjustcolor(c('darkgreen', 'darkorange'), alpha.f = 0.6)
temp$sex <- Hmisc::capitalize(temp$sex)

# Make new variable for plotting
temp_long$Group <-
  ifelse(temp_long$sex == 'female' & temp_long$key == 'incidence',
         'Female\nTB (all)',
         ifelse(temp_long$sex == 'female' & temp_long$key == 'co_incidence',
                'Female\nTB+HIV',
                ifelse(temp_long$sex == 'male' & temp_long$key == 'incidence',
                       'Male\nTB (all)',
                       ifelse(temp_long$sex == 'male' & temp_long$key == 'co_incidence',
                              'Male\nTB+HIV',
                              NA))))

cols <- c('darkred', 'darkorange', 'darkgreen', 'lightgreen')

ggplot() +
  geom_point(data = temp_long,
           aes(x = age_group, 
               y = value, 
               # alpha = key,
               color = Group,
               group = Group),
           alpha = 0.6) +
  geom_line(data = temp_long,
             aes(x = age_group, 
                 y = value, 
                 # alpha = key,
                 color = Group,
                 group = Group),
            alpha = 0.6) +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('Age group') +
  ylab('Incidence rate (per 100,000)') +
  ggtitle('Incidence rate by sex, age group, and HIV/TB status') +
  scale_color_manual(name = 'Sex',
                    values = cols) #+
  # guides(fill = guide_legend(reverse = TRUE))
z_incidence_by_sex_age_group_and_coinfection <- last_plot()

# BY SUBGROUP
sex <- temp %>%
  filter(incident_case) %>%

#### HIV STATUS AND TB

# how many hivs before 2006
table(tb$hiv_status[tb$year < 2006])

# What percentage unknown since 2008
table(tb$hiv_status[tb$year >= 2008])
prop.table(table(tb$hiv_status[tb$year >= 2008], useNA = 'ifany'))

# Total number known
table(tb$hiv_status %in% c('negative', 'positive'))
prop.table(table(tb$hiv_status %in% c('negative', 'positive')))
table(tb$hiv_status[tb$hiv_status != 'unknown'])
prop.table(table(tb$hiv_status[tb$hiv_status != 'unknown']))
table(tb$sex[tb$hiv_status == 'positive'])
prop.table(table(tb$sex[tb$hiv_status == 'positive']))
temp <-
  tb %>%
  filter(incident_case) %>%
  group_by(year, hiv_status) %>%
  tally %>%
  filter(!is.na(year),
         !is.na(hiv_status))

# Make sure to have all years
left <- 
  expand.grid(year = unique(sort(temp$year)),
              hiv_status = sort(unique(temp$hiv_status)))
temp <- left_join(left, temp)
temp$n[is.na(temp$n)] <- 0
# Reorder factor levels for plotting
temp$hiv_status <- Hmisc::capitalize(temp$hiv_status)
temp$hiv_status <- factor(temp$hiv_status,
                          levels = c('Unknown',
                                     'Negative',
                                     'Positive'))

# What percentage of all coinefections were among patients 
# aes 15 to 49
tb %>%
  filter(!is.na(years_old_round)) %>%
  filter(hiv_status == 'positive') %>%
  group_by(fi = years_old_round >= 18 & years_old_round <= 49) %>%
  tally %>%
  mutate(p = n / sum(n) * 100)


# cols <- c('darkorange', 'darkgreen', 'grey')
cols <- c(grey(0.1), grey(0.5), grey(0.9))

# Change levels to ensure HIV positive is on the bottom
temp$hiv_status <-
  factor(temp$hiv_status,
         levels = c('Positive',
                    'Negative',
                    'Unknown'))
# Order the rows also
temp <- 
  rbind(temp %>% filter(hiv_status == 'Positive'),
        temp %>% filter(hiv_status == 'Negative'),
        temp %>% filter(hiv_status == 'Unknown'))

ggplot(data = temp,
       aes(x = year, 
           y = n)) +
  geom_area(aes(fill = hiv_status),
            # color = 'black',
            position = 'stack', alpha = 0.7) +
  scale_fill_manual(name = 'HIV status',
                    values = cols) +
  xlab('Year') +
  ylab('Incident TB cases') +
  theme_tb() +
  ggtitle('HIV status among incident TB cases by year')
g_hiv_status_amont_incident_tb_over_time <- last_plot()

#### AGE GROUP AMONG TB HIV CASES
temp <- tb %>%
  filter(year >= 2007) %>%
  filter(!is.na(hiv_status) & hiv_status !='unknown') %>%
  group_by(age_group) %>%
  summarise(hiv = length(which(hiv_status == 'positive')),
            tb = length(which(hiv_status == 'positive')) +
              length(which(hiv_status == 'negative'))) %>%
  ungroup %>%
  filter(!is.na(age_group))
# Gather
temp <-
  temp %>%
  tidyr::gather(key, value, hiv:tb)
temp$key <-
  ifelse(temp$key == 'hiv', 'TB & HIV',
         ifelse(temp$key == 'tb', 'TB',
                NA))

# cols <- adjustcolor(c('darkgreen', 'darkorange'), alpha.f = 0.6)
cols <- c(grey(0.3), grey(0.8))
ggplot(data = temp,
       aes(x = age_group, y = value, group = key, fill = key)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('Age group') +
  ylab('Total cases') +
  ggtitle('Total TB and TB-HIV co-infections') +
  scale_fill_manual(name = 'Status',
                    values = cols) 
  # guides(fill = guide_legend(reverse = TRUE))
h_total_tb_and_hiv_coinfections <- last_plot()

##### TB TYPE AND SMEAR TESTING

# Total number of pulmonary
table(tb$tb_type)
prop.table(table(tb$tb_type))

# Smear result among pulomonary
table(tb$smear_result[tb$tb_type == 'pulmonary'] == 'not done',
      useNA = 'ifany')
prop.table(table(tb$smear_result[tb$tb_type == 'pulmonary'] == 'not done', useNA = 'ifany'))
table(tb$smear_result[tb$tb_type == 'pulmonary'], useNA = 'ifany')
prop.table(table(tb$smear_result[tb$tb_type == 'pulmonary'], useNA = 'ifany'))

# Incident cases who get smear test by year
temp <-
  tb %>% 
  filter(incident_case) %>%
  group_by(year) %>%
  summarise(cases = n(),
            smears = length(which(smear_result != 'not done' &
                                    !is.na(smear_result)))) %>%
  mutate(smear_rate = smears / cases * 100)
temp

ggplot(data = temp,
       aes(x = year, y = smear_rate)) +
  geom_area(fill = 'darkorange', alpha = 0.6) +
  ylim(0, 100) +
  xlab('Year') +
  ylab('Percentage') +
  theme_tb() +
  ggtitle('Percentage of incident smear cases tested by year')
i_incidence_smear_cases_tested_over_time <- last_plot()

# NUMBER of smears
# Keep only pulomonary cases
temp <-
  tb %>% 
  filter(#incident_case, 
         tb_type == 'pulmonary') %>%
  group_by(year) %>%
  summarise(cases = n(),
            smears = length(which(smear_result != 'not done' &
                                    !is.na(smear_result))),
            smear_positive = length(which(smear_result == 'smear positive')),
            smear_negative = length(which(smear_result == 'smear negative'))) %>%
  mutate(smear_rate = smears / cases * 100) %>%
  mutate(positive_rate = smear_positive / smears * 100,
         negative_rate = smear_negative / smears * 100) %>%
  mutate(no_smear_result = cases - smears)
gathered <- tidyr::gather(temp, key, value, cases:no_smear_result) 
gathered <- gathered %>% filter(key %in% c('smear_positive',
                                           'smear_negative',
                                           'no_smear_result'))

# gathered$key <-
#   ifelse(gathered$key == 'cases', 'No smear result',
#          ifelse(gathered$key == 'smears', 'Smeared',
#                 NA))
# gathered$key <-
#   factor(gathered$key,
#          levels = c('No smear result', 'Smeared'))
cols <- c('darkgreen', 'darkorange', 'darkblue')
# cols <- c(grey(0.1), grey(0.5), grey(0.9))

gathered$key <- Hmisc::capitalize(gsub('_', '\n', gathered$key))
gathered$key <-
  factor(gathered$key,
         levels = c('No\nsmear\nresult',
                    'Smear\npositive',
                    'Smear\nnegative'))

ggplot() +
  geom_area(data = gathered,
            aes(x = year, 
                y = value,
                group = key,
                fill = key),
            position = 'stack', alpha = 0.8) +
  scale_fill_manual(name = '',
                    values = cols) +
  # guides(col = guide_legend(reverse = TRUE)) +
  theme_tb() +
  ggtitle('Pulmonary smeared and non-smeared incident cases') +
  xlab('Year') +
  ylab('Pulmonary cases') +
  theme(axis.text.x = element_text(angle = 90)) 
j_smeared_non_smeared_cases_over_time <- last_plot()

# Create a version of the above with two y-axes

# Base plot
p1 <- j_smeared_non_smeared_cases_over_time +
  # + theme_bw() + %+replace%  +
  theme_tb() +
  theme(panel.background = element_rect(fill = NA),
        panel.grid = element_line(colour = NA)) +
  theme(legend.position="bottom") +
  labs(title = 'C') +
  theme(panel.grid.major=element_blank()) 

# Overline
gathered <- data.frame(gathered)
data2 <- gathered %>%
  filter(key != 'No\nsmear\nresult') %>%
  group_by(year) %>%
  summarise(p_smear_negative = sum(value[key == 'Smear\nnegative']) / 
              sum(value) * 100,
            p_smear_positive = sum(value[key == 'Smear\npositive']) / 
              sum(value) * 100) %>%
  mutate(z = '% of positive pulmonary cases')

# Create a dataframe for labeling the red line
red_line_label <-
  data.frame(x = 1999,
             y = 75,
             label = '% smear positive of\nall pulmonary cases\nwith smear result')
  
p2 <- ggplot() +
  geom_line(data = data2,
            aes(x = year, y = p_smear_positive, color = z),
            alpha = 0.8) + #theme_bw() %+replace% 
  theme_tb() +
  theme(panel.background = element_rect(fill = NA),
        panel.grid = element_line(colour = NA)) +
  theme(panel.grid.major=element_blank()) +
  scale_colour_manual(name = '',
                      values = 'darkred') +
  theme(legend.position="bottom") +
  geom_text(data = red_line_label,
            aes(x = x, y = y, label = label),
            color = 'darkred',
            size = 2) +
  scale_y_continuous(breaks = seq(0, 100, 25),
                     labels = paste0(seq(0, 100, 25), '%'),
                     limits = c(0,100))


# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
# ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
library(extrafont)

j_smeared_non_smeared_cases_over_time_with_line <- g
grid.draw(j_smeared_non_smeared_cases_over_time_with_line)

# Save it
pdf('Panel C.pdf', width = 5, height = 3.5)
grid.draw(j_smeared_non_smeared_cases_over_time_with_line)
dev.off()

# Add a line to j
j_smeared_non_smeared_cases_over_time <- 
  j_smeared_non_smeared_cases_over_time +
  geom_hline(aes(yintercept = 100), lty = 3)

# Proportion of smear-negative results among those
# having a smear test
temp <-
  tb %>%
  filter(incident_case,
         !is.na(smear_result),
         smear_result != 'not done') %>%
  mutate(year_group = ifelse(year <= 2000,
                             '1997-2000',
                             ifelse(year <= 2004,
                                    '2001-2004',
                                    ifelse(year <= 2008,
                                           '2005-2008',
                                           ifelse(year <= 2012,
                                                  '2009-2012',
                                                  NA))))) %>%
  group_by(year_group) %>%
  summarise(pos = length(which(smear_result == 'smear positive')),
            neg = length(which(smear_result == 'smear negative')),
            n = n()) %>%
  mutate(p_pos = pos / n * 100,
         p_neg = neg / n * 100)
chisq.test(temp[,c('pos', 'neg')])

# Among smear-negative, how many are hiv
temp <-
  tb %>%
  filter(incident_case,
         !is.na(smear_result),
         smear_result != 'not done',
         hiv_status != 'unknown') 
tbl <- table(temp$smear_result, temp$hiv_status)
tbl
prop.table(tbl, 1) * 100
chisq.test(tbl)


# EXTRAPULOMNARY TB
temp <-
  tb %>%
  filter(incident_case) %>%
  group_by(tb_type) %>%
  tally %>%
  filter(!is.na(tb_type)) %>%
  mutate(p = n / sum(n) * 100)

# extrapulm with known hiv status
temp <-
  tb %>%
  filter(incident_case,
         tb_type == 'extrapulmonary',
         !is.na(hiv_status),
         hiv_status != 'unknown') %>%
  group_by(hiv_status) %>%
  tally %>%
  mutate(p = n / sum(n) * 100)

# Get proportion of coinfection of hiv and extrapulmonary by time
temp <-
  tb %>%
  filter(incident_case,
         tb_type == 'extrapulmonary',
         !is.na(hiv_status)) %>%
  group_by(year, hiv_status) %>%
  tally %>%
  ungroup %>%
  group_by(year) %>%
  mutate(p = n / sum(n) * 100)

# Make sure to have all years
left <- 
  expand.grid(year = unique(sort(temp$year)),
              hiv_status = sort(unique(temp$hiv_status)))
temp <- left_join(left, temp)
temp$n[is.na(temp$n)] <- 0
temp$p[is.na(temp$p)] <- 0
# Reorder factor levels for plotting
temp$hiv_status <- Hmisc::capitalize(temp$hiv_status)
temp$hiv_status <- factor(temp$hiv_status,
                          levels = c('Unknown',
                                     'Negative',
                                     'Positive'))


cols <- c('darkgrey', 'darkgreen', 'darkorange')

ggplot(data = temp,
       aes(x = year, 
           y = n)) +
  geom_area(aes(fill = hiv_status),
            # color = 'black',
            position = 'stack', alpha = 0.7) +
  scale_fill_manual(name = 'HIV status',
                    values = cols) +
  xlab('Year') +
  ylab('Incident extrapulmonary TB cases') +
  theme_tb() +
  ggtitle('HIV among incident\nextrapulmonary TB cases')
k_hiv_status_among_incident_extrapul_tb_over_time <- last_plot()

##### PREVIOUSLY TREATED OR NEW CASE
table(tb$new_case)
prop.table(table(tb$new_case)) * 100

# Previously treated patients by year
temp <-
  tb %>%
  group_by(year, new_case) %>%
  tally %>%
  filter(new_case != 'not known',
         !is.na(new_case)) %>%
  ungroup %>%
  group_by(year) %>%
  mutate(p = n / sum(n) * 100)

ggplot(data = temp %>% filter(new_case == 'previously treated cases'),
       aes(x = year,
           y = p)) +
  geom_line(color = 'darkorange', alpha = 0.8, size = 2) +
  ylim(0, 100) +
  xlab('Year') +
  ylab('Percentage') +
  ggtitle('Percentage of previously treated patients among incident cases by year') +
  theme_tb()
l_percentage_of_previously_treated_patients_among_incident_cases_over_time <- last_plot()

# Previously treated with known hiv status
# what % were hiv pos
tb %>%
  filter(new_case != 'not known',
         !is.na(new_case),
         hiv_status != 'unknown',
         new_case == 'previously treated cases') %>%
  group_by(hiv_status) %>%
  tally %>%
  mutate(p = n / sum(n) * 100)

##### TREATMENT OUTCOMES

# Overall treatment success
tb %>%
  filter(incident_case) %>%
  group_by(ttm_result) %>%
  ungroup %>%
  mutate(success = ttm_result %in% c('cured',
                                     'treatment completed')) %>%
  group_by(success) %>%
  tally %>%
  mutate(p = n / sum(n) * 100)

# Treatment status over time
temp <- 
  tb %>%
  group_by(year, ttm_result) %>%
  tally %>%
  mutate(p = n / sum(n) * 100,
         ttm_result = Hmisc::capitalize(ttm_result)) 

cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(
  length(unique(temp$ttm_result)))
cols <- rev(cols)

ggplot(data = temp,
       aes(x = year, 
           y = p,
           group = ttm_result,
           color = ttm_result)) +
  geom_line() +
  scale_color_manual(name = 'Treatment result',
                     values = cols) +
  xlab('Year') +
  ylab('Percentage') +
  theme_tb() +
  ggtitle('Treatment outcomes by year: all cases')
m_outcomes_over_time <- last_plot()

# How many total died?
tb %>%
  filter(incident_case) %>%
  group_by(ttm_result) %>%
  tally %>%
  mutate(p = n / sum(n) * 100)

# Treatment result by previously treated status
temp <-
  tb %>%
  # filter(new_case == 'previously treated cases') %>%
  filter(new_case != 'not known',
         !is.na(new_case)) %>%
  group_by(new_case,ttm_result) %>%
  tally %>%
  mutate(p = n / sum(n) * 100) %>%
  ungroup %>%
  arrange(desc(p)) %>%
  mutate(new_case = ifelse(new_case == 'previously treated cases',
                           'Previously treated',
                           ifelse(new_case == 'new case',
                                  'New',
                                  NA)),
         ttm_result = Hmisc::capitalize(ttm_result),
         ttm_result = gsub(': transferred out', '', ttm_result))

cols <- c('darkgreen', 'darkorange')

ggplot(data = temp,
       aes(x = ttm_result, 
           y = p,
           group = new_case,
           fill = new_case)) +
  geom_bar(stat = 'identity', position = 'dodge', alpha = 0.7) +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  xlab('Treatment result') +
  ylab('Percentage of all cases') +
  ggtitle('Treatment result by new/previous case') +
  scale_fill_manual(name = 'Case status',
                    values = cols)
n_outcomes_by_previous_status <- last_plot()
  
# Likelihood of cured different by previously treated status
temp <- tb %>%
  filter(new_case != 'not known',
         !is.na(new_case))
tbl <- table(temp$ttm_result == 'cured', temp$new_case)
chisq.test(tbl)
#likelihood of completing treatment
tbl <- table(temp$ttm_result != 'treatment completed' |
               temp$ttm_result == 'lost to follow up', 
             temp$new_case)
chisq.test(tbl)

# Treatment success by sex
# Overall treatment success
temp <- 
  tb %>%
  filter(incident_case) %>%
  mutate(success = ttm_result %in% c('cured',
                                     'treatment completed')) %>%
  group_by(sex, success) %>%
  tally %>%
  ungroup %>%
  group_by(sex) %>%
  mutate(p = n / sum(n) * 100)
temp <- 
  tb %>%
  mutate(success = ttm_result %in% c('cured',
                                     'treatment completed')) 
tbl <- table(temp$success, temp$sex)
chisq.test(tbl)

# DEATHS BY SEX
tb %>%
  filter(incident_case) %>%
  group_by(sex, death = ttm_result == 'death') %>%
  tally %>% 
  filter(!is.na(death))
tbl <- table(tb$ttm_result[tb$incident_case] == 'death', tb$sex[tb$incident_case])
tbl
prop.table(tbl, 2)
chisq.test(tbl)

# LOST TO FOLLOW UP BY SEX
tbl <- table(tb$ttm_result == 'lost to follow up',
             tb$sex)
tbl
prop.table(tbl, 2)
chisq.test(tbl)

# LOST TO FOLLOW UP BY AGE
tbl <- 
  table(tb$ttm_result == 'lost to follow up',
        tb$age_group)
tbl
prop.table(tbl, 2) * 100
chisq.test(tbl)

temp <- 
  tb %>%
  group_by(lost = ifelse(ttm_result == 'lost to follow up',
                         'Lost to follow up',
                         'Followed up'),
           age_group) %>%
  tally() %>%
  ungroup %>%
  group_by(age_group) %>%
  mutate(p = n / sum(n) * 100) %>%
  filter(lost == 'Lost to follow up')

ggplot(data = temp,
       aes( x= age_group,
            y = p)) +
  geom_bar(fill = 'darkorange', alpha = 0.8, stat = 'identity') +
  xlab('Age group') +
  ylab('Percentage') +
  ggtitle('Patients lost to follow up by age group') +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))
o_lost_to_followup_by_age <- last_plot()

# Among patients with known HIV status, likelihood of death
temp <-
  tb %>%
  filter(!is.na(hiv_status),
         hiv_status != 'unknown') %>%
         # !ttm_result %in% c('lost to follow up',
                           # 'not evaluated: transferred out')) %>%
  mutate(death = ifelse(ttm_result == 'death', 1, 0)) %>%
  dplyr::select(death, hiv_status) 
tbl <- table(temp$death, temp$hiv_status)
tbl
prop.table(tbl, 2)
fit <- glm(death ~ hiv_status, data = temp)
exp(coef(fit))
exp(confint(fit))

# Death by age group
temp <-
  tb %>%
  mutate(death = ifelse(ttm_result == 'death', 1, 0)) %>%
  group_by(age_group) %>%
  summarise(deaths = length(which(death == 1)),
            lives = length(which(death == 0)),
            n = n()) %>%
  mutate(p = deaths / n * 100)

# Get confidence intervals
simpasym <- function(n, p, z=1.96, cc=TRUE){
  out <- list()
  if(cc){
    out$lb <- p - z*sqrt((p*(1-p))/n) - 0.5/n
    out$ub <- p + z*sqrt((p*(1-p))/n) + 0.5/n
  } else {
    out$lb <- p - z*sqrt((p*(1-p))/n)
    out$ub <- p + z*sqrt((p*(1-p))/n)
  }
  out
}
ci <- simpasym(n = temp$n,
               p = temp$p / 100)
temp$lwr <- ci$lb * 100
temp$upr <- ci$ub * 100

ggplot(data = temp,
       aes(x = age_group, y = p)) +
  # geom_point(group = 1, color = 'darkorange', alpha = 0.8) +
  geom_point(color = 'darkorange', alpha = 0.6) +
  # geom_pointrange(aes(ymax = upr, ymin = lwr),
  #                 color = 'darkorange', alpha = 0.6) +
  # geom_line(group = 1, color = 'darkorange', alpha = 0.6) +
  theme_tb() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  xlab('Age group') +
  ylab('Percentage') +
  ggtitle('Death as treatment outcome by age group')
p_death_by_age <- last_plot()

# Treatment failure by hiv status
temp <- 
  tb %>%
  filter(!is.na(hiv_status),
         hiv_status != 'unknown') %>%
  mutate(failure = ttm_result == 'treatment failed') %>%
  dplyr::select(hiv_status, failure)
tbl <- table(temp$hiv_status, temp$failure)
tbl
prop.table(tbl, 1)
chisq.test(tbl)

# Treatment failure by sex
temp <- 
  tb %>%
  mutate(failure = ttm_result == 'treatment failed') %>%
  dplyr::select(sex, failure)
tbl <- table(temp$sex, temp$failure)
tbl
prop.table(tbl, 1)
chisq.test(tbl)

### Make a chart of all treatment outcomes
temp <- 
  tb %>%
  filter(incident_case) %>%
  filter(!is.na(ttm_result)) %>%
  group_by(year, ttm_result) %>%
  tally %>%
  mutate(ttm_result = Hmisc::capitalize(ttm_result))

# Get percentage version
temp <- 
  temp %>%
  group_by(year) %>%
  mutate(denom = sum(n, na.rm = TRUE)) %>%
  ungroup %>%
  mutate(n = n / denom * 100)


# Order differently
temp <-
  rbind(temp %>%
          filter(ttm_result == 'Death'),
        temp %>%
          filter(ttm_result == 'Lost to follow up'),
        temp %>%
          filter(ttm_result == 'Not evaluated: transferred out'),
        temp %>%
          filter(ttm_result == 'Treatment failed'),
        temp %>%
          filter(ttm_result == 'Treatment completed'),
        temp %>%
          filter(ttm_result == 'Cured'))

# Level differently
temp$ttm_result <-
  factor(temp$ttm_result,
         levels = c('Death',
                    'Lost to follow up',
                    'Not evaluated: transferred out',
                    'Treatment failed',
                    'Treatment completed',
                    'Cured'))


# cols <- brewer.pal(n = 9, name = 'Greys')
# cols <- cols[3:9]
cols <- brewer.pal(n = 9, name = 'Spectral')
cols <- colorRampPalette(cols)(length(unique(temp$ttm_result)))
cols[4] <- 'black'
ggplot(data = temp,
       aes(x = year, 
             y = n,
           group = ttm_result,
           fill = ttm_result)) +
  # geom_bar(stat = 'identity', position = 'stack') +
  geom_area(position = 'stack') +
  scale_fill_manual(name = 'Outcome',
                    values = cols,
                    guide = guide_legend(reverse = TRUE)) +
  xlab('Year') +
  ylab('Proportion of cases starting treatment') +
  theme_tb() +
  ggtitle('Treatment outcomes')
zzz <- last_plot()


##### GET NUMBERS FOR 4 different categories
# - In “Incidence by sex, age group and HIV/TB”, make 4 lines be: males, females, overall, smear positive

sex <- 
  tb %>%
  filter(incident_case) %>%
  group_by(age_group, sex) %>%
  summarise(cases = n()) %>%
# Get denominator
  left_join(popuation %>%
              group_by(age_group, sex) %>%
              summarise(n = sum(n))) %>%
  mutate(incidence_rate = cases / n * 100000)

overall <-
  tb %>%
  filter(incident_case) %>%
  group_by(age_group) %>%
  summarise(overall = n(),
            positive = length(which(smear_result == 'smear positive')),
            negative = length(which(smear_result == 'smear negative'))) %>%
  # mutate(positive_rate = positive / (positive + negative) * 100) %>%
  left_join(population %>%
              group_by(age_group) %>%
              summarise(n = sum(n))) %>%
              mutate(overall_incidence_rate = overall / n * 100000,
                     smear_positive_incidence_rate = positive / n * 100000)

# Combine
sex$indicator <- paste0(sex$sex, ' incidence rate')
sex$value <- sex$incidence_rate
overall <- overall %>%
  tidyr::gather(indicator, value, contains('rate'))
sex <-  sex %>%
  dplyr::select(age_group, indicator, value) %>% ungroup
overall <- overall  %>%
  dplyr::select(age_group, indicator, value) %>% ungroup
combined <- 
  rbind(sex,
        overall)
combined$indicator <-
  Hmisc::capitalize(gsub('_', ' ', combined$indicator))

ggplot(data = combined,
       aes(x = age_group, y = value,
           # color = indicator,
           group = indicator,
           lty = indicator,
           color = indicator)) + 
  geom_line() +
  # theme(legend.position = 'bottom')
  theme_tb() +
  xlab('Age group') +
  ylab('Incidence rate (per 100,000)') +
  ggtitle('Average annualized incidence and smear positivity rates') +
  scale_linetype_manual("", values=c(1,2,3,4)) +
  scale_color_manual(name = '',
                     values = brewer.pal(4, 'Spectral')) #+
  # geom_hline(yintercept = 100, lty = 2, color = 'darkgrey', alpha = 0.6)
  
lines4 <- last_plot()


#### Likelihood of lost to followup
fit <- glm(tb$ttm_result == 'lost to follow up' ~
             tb$new_case == 'previously treated cases',
           family = 'binomial')
# Get OR
exp(coef(fit))
exp(confint(fit))

fit <- glm(tb$ttm_result == 'lost to follow up' ~
             tb$years_old < 18,
           family = 'binomial')
# Get OR
exp(coef(fit))
exp(confint(fit))

#### Likelihood of death by hiv status
temp <- tb[tb$hiv_status != 'unknown' &
             !is.na(tb$hiv_status),]
temp$death <- temp$ttm_result == 'death'
temp$hiv <- temp$hiv_status == 'positive'
fit <- glm(death ~ hiv,
           data = temp,
           family = 'binomial')
exp(coef(fit))
exp(confint(fit))

###
# By gender:
tb %>% group_by(gender, year) %>% tally %>% ungroup %>% group_by(year) %>% mutate(p = n / sum(n) * 100) %>% ggplot() + geom_line(aes(x = year, y = p, group = gender, color = gender)) + ylim(0, 100)

# Increasing amount of cases by hiv status
tb %>%
  filter(year >= 2007) %>%
  filter(hiv_status != 'unknown' & !is.na(hiv_status)) %>%
  group_by(year, hiv_status) %>% 
  tally %>%
  mutate(p = n / sum(n) * 100) %>%
  ggplot() +
  geom_line(aes(x = year, y = p, group = hiv_status, color = hiv_status))
