# Directories
root <- getwd() # should be the tb_on_the_rise directory
setwd('data')
data_dir <- getwd()
setwd(root)
setwd('code')
code_dir <- getwd()
setwd(root)

# Libraries
library(foreign)
library(dplyr)
library(ggplot2)
library(readr)

# Read and clean
source(paste0(code_dir, '/read_and_clean.R'))

##### RESULTS SECTION

# Overall and demographic characteristics

# Number of overall cases
length(which(tb$incident_case == 'incident case'))

# Distribution by geography
table(tb$health_unit_register[tb$incident_case == 'incident case'])
prop.table(table(tb$health_unit_register[tb$incident_case == 'incident case']))

# Mean age of registered tb patients
summary(tb$years_old[tb$incident_case == 'incident case'], na.rm = TRUE)
sd(tb$years_old[tb$incident_case == 'incident case'], na.rm = TRUE)
tb %>%
  filter(incident_case == 'incident case') %>%
  group_by(years_old_round) %>%
  tally() %>%
  arrange(desc(n))

# Sex
table(tb$gender[tb$incident_case == 'incident case'])
prop.table(table(tb$gender[tb$incident_case == 'incident case']))
