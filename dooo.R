source('/srv/shiny-server/import_banner_data.R')
source('/srv/shiny-server/merge_enom_reports.R')
source('/srv/shiny-server/parse_wp_api.R')
source('/srv/shiny-server/enom_api.R')

# function to find best signup date
find_earliest_date <- function(date1, date2, date3, date4) {
  date_list <- c(date1, date2, date3, date4)
  return(as.character(min(date_list[!is.na(date_list)])))
}

# function to find best expiration date
find_latest_date <- function(date1, date2) {
  date_list <- c(date1, date2)
  return(as.character(max(date_list[!is.na(date_list)])))
}

dooo <- merge(banner, 
              wp_users,
              by = 'netid',
              all = TRUE,
              incomparables = NA) %>%
  full_join(enom_all) %>%
  unique() %>%
  as_tibble() %>%
  full_join(enom_data %>% 
              select(url, enom_api_exp_date)) %>%
  unique() 

best_exp_date <- ymd(mapply(find_latest_date, dooo$exp_date, dooo$enom_api_exp_date))

dooo <- cbind(dooo, best_exp_date) %>%
  unique() %>%
  mutate(exp_minus_one = ymd(best_exp_date) - years(1))

signup <- ymd(mapply(find_earliest_date, dooo$whmcs_signup, dooo$wp_signup, dooo$reg_date, dooo$exp_minus_one))

dooo <- cbind(dooo, signup) %>%
  unique() %>%
  as_tibble()

# find most reliable signup date
write_csv(dooo, '/srv/shiny-server/dataOutput/dooo_merged.csv')

acad_year <- function(year, month) {
  return(ifelse(!is.na(year) & !is.na(month),
                ifelse (as.numeric(month) >= 1 & as.numeric(month) <= 7,
                        paste((as.numeric(year) - 1), '-', as.numeric(year), sep=''),
                        paste(as.numeric(year), '-', (as.numeric(year) + 1), sep='')),
                NA))
}

find_term <- function(year, month) {
  return(ifelse(!is.na(year) & !is.na(month) & as.numeric(month) >= 1 & as.numeric(month) <= 12,
                ifelse (as.numeric(month) >= 8 & as.numeric(month) <= 12,
                        'fall',
                        ifelse(as.numeric(month) >= 1 & as.numeric(month) <= 4,
                               'spring',
                               'summer')),
                NA))
}

dooo_simplified <- dooo %>%
  select(netid,
         email,
         url,
         groupname,
         stst,
         status, 
         class,
         grad_date,
         grad_date_sort,
         last_active_semester,
         signup,
         exp_date = best_exp_date) %>%
  mutate(year = format(signup, '%Y'),
         month = format(signup, '%m'),
         year_month = ifelse(!is.na(year) & !is.na(month),
                             paste(year, '-', month, sep=''),
                             NA),
         academic_year = mapply(acad_year, year, month),
         term = find_term(year, month),
         year_term = ifelse(!is.na(academic_year) & !is.na(term),
                            paste(academic_year, term),
                            NA))

# is domain active on a particular date?

isDomainActive <- function (record, date) {
  if (!is.na(record['url'])) {
    registered <- as.character(record['signup'])
    expired <- as.character(record['exp_date'])
    if (!is.na(registered) & !is.na(expired) & date >= registered & date < expired) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

countActiveDomains <- function (date, data_source) {
  return(table(apply(data_source, 1, isDomainActive, date))['TRUE'][[1]])
}

yearOnly <- function (full_date) {
  return(strsplit(as.character(full_date), '-')[[1]][1])
}

monthOnly <- function (full_date) {
  return(strsplit(as.character(full_date), '-')[[1]][2])
}

firstOfMonth <- function (month, year) {
  return(paste(year, sprintf('%02d', month), '01', sep='-'))
}

wholeYearOfFirstMonths <- function (year) {
  return(sapply(1:12, firstOfMonth, year))
}

constructListOfDates <- function (start_date, end_date) {
  start_year <- yearOnly(start_date)
  start_month <- monthOnly(start_date)
  end_year <- yearOnly(end_date)
  end_month <- monthOnly(end_date)
  years <- (as.numeric(start_year) + 1):(as.numeric(end_year) - 1)
  months <- 1:12
  
  whole_first_year_dates <- sapply(months, firstOfMonth, start_year)
  first_year_dates <- whole_first_year_dates[whole_first_year_dates >= start_date]
  
  whole_last_year_dates <- sapply(months, firstOfMonth, end_year)
  last_year_dates <- whole_last_year_dates[whole_last_year_dates <= end_date]
  
  middle_dates <- sapply(years, wholeYearOfFirstMonths)
  all_dates <- c(first_year_dates, middle_dates, last_year_dates)
  return(all_dates)
}

activeDomainCount <- function (date_list) {
  return()
}

# run domain total analysis

date_list <- constructListOfDates('2012-06-01', max(enom_all$reg_date))
activeDomains <- sapply(date_list, 
                        countActiveDomains, 
                        dooo_simplified) %>%
  as.numeric()
domainActivity <- as_tibble(cbind(date = date_list, domains = activeDomains))

write_csv(domainActivity, '/srv/shiny-server/dataOutput/active_domains_by_month.csv')
write_csv(as_tibble(countActiveDomains(Sys.Date(), dooo_simplified)), '/srv/shiny-server/dataOutput/current_active_domains.csv')


# clean user status (student, faculty/staff, unknown)
get_status <- function(groupname, stst) {
  return(ifelse(groupname %in% c('Faculty/Staff', 'Faculty/Staff,Student'),
                'Faculty/Staff',
                ifelse(stst %in% c('AS', 'CA', 'IG', 'IS'),
                       'Student',
                       'Faculty/Staff')
                )
  )
}

dooo_with_status <- dooo_simplified %>%
  mutate(group_status = mapply(get_status, groupname, stst))

write_csv(dooo_with_status, '/srv/shiny-server/dataOutput/dooo_merged_dates.csv')


# look for duplicate netids and urls
dooo_with_status <- read_csv('/srv/shiny-server/dataOutput/dooo_merged_dates.csv')

duplicate_netids <- dooo_with_status %>%
  group_by(netid) %>%
  summarize(count = n()) %>%
  filter(count >= 2)

duplicate_domains <- dooo_with_status %>%
  group_by(url) %>%
  summarize(count = n()) %>%
  filter(count >= 2)

multiple_domain <- dooo_with_status %>%
  filter(url %in% duplicate_domains$url,
         !is.na(url))

multiple_netid <- dooo_with_status %>%
  filter(!is.na(netid),
         netid %in% duplicate_netids$netid)
