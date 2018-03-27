library(tidyverse)
library(lubridate)

# function to strip time information and get date in clean format

dateOnly <- function (long_date) {
  return(as.character(as.Date(strsplit(as.character(long_date), ' ')[[1]][1], format = '%m/%d/%Y')))
}

# import data from reports

enom_first <- read_csv('/srv/shiny-server/dataSources/individual_enom_reports/enomReport.csv') %>%
  mutate(url = domainname,
         reg_date = sapply(EffStartDate, dateOnly),
         exp_date = sapply(CalEndDate, dateOnly),
         action = actiondesc
         ) %>%
  filter(action %in% c('Register', 'Renew')) %>%
  select(url, action, reg_date, exp_date)

enom_cyber_registrations <- read_csv('/srv/shiny-server/dataSources/individual_enom_reports/enom_cyber_reg_Feb6.csv') %>%
  mutate(reg_date = sapply(creation_date, dateOnly),
         exp_date = sapply(expiration_date, dateOnly),
         action = 'Register'
  ) %>%
  select(url, action, reg_date, exp_date)

enom_cyber_renewals <- read_csv('/srv/shiny-server/dataSources/individual_enom_reports/enom_cyber_renew_Feb6.csv') %>%
  mutate(reg_date = sapply(creation_date, dateOnly),
         exp_date = sapply(expiration_date, dateOnly),
         action = 'Renew'
  ) %>%
  select(url, action, reg_date, exp_date)

enom_dtlt_registrations <- read_csv('/srv/shiny-server/dataSources/individual_enom_reports/enom_new_reg_Feb6.csv') %>%
  mutate(reg_date = sapply(creation_date, dateOnly),
         exp_date = sapply(expiration_date, dateOnly),
         action = 'Register'
  ) %>%
  select(url, action, reg_date, exp_date)

enom_dtlt_renewals <- read_csv('/srv/shiny-server/dataSources/individual_enom_reports/enom_new_renew_Feb6.csv') %>%
  mutate(reg_date = sapply(creation_date, dateOnly),
         exp_date = sapply(expiration_date, dateOnly),
         action = 'Renew'
  ) %>%
  select(url, action, reg_date, exp_date)

# combine all reports into single data frame
# write to CSV

enom_all <- enom_first %>%
  full_join(enom_cyber_registrations) %>%
  full_join(enom_cyber_renewals) %>%
  full_join(enom_dtlt_registrations) %>%
  full_join(enom_dtlt_renewals) %>%
  unique() %>%
  mutate(reg_date = ymd(reg_date),
         exp_date = ymd(exp_date)) %>%
  group_by(url) %>%
  summarize(reg_date = min(reg_date),
            exp_date = max(exp_date))

write_csv(enom_all, '/srv/shiny-server/dataSources/enom_full_report.csv')

