# This script imports data from umw.domains WordPress user database and 
# Enom user/url database, merges and cleans data, and exports to single table:
# dooo_simplified_and_merged.csv

## read in data from umw.domains WP database dump
dooo_temp <- read.csv('dataSources/wordpress_users.csv')
dooo_faculty_status <- read.csv('dataSources/wordpress_users_meta.csv')

## remove entries with no url

dooo_temp <- dooo_temp[dooo_temp$user_url != '',]

## convert user_registered (date) string to POSIX date format

date <- as.character(strptime(dooo_temp$user_registered, '%m/%d/%Y'))

## extract year and month from POSIX date

year <- as.numeric(substr(date, 1, 4))
month <- as.numeric(substr(date, 6, 7))

## drop http/https, subdomains, and anything post-slash from url

stripSubdomain <- function (url) {
  if (length(strsplit(url, '[.]')[[1]]) <= 2) {
    return(url)
  } else {
    total <- length(strsplit(url, '[.]')[[1]])
    root_url <- paste(strsplit(url, '[.]')[[1]][total-1], 
                      strsplit(url, '[.]')[[1]][total], 
                      sep = '.')
    return(root_url)
  }
}

get_root_url <- function(temp_url) {
  temp <- gsub('https://', 
               '', 
               gsub('http://', 
                    '', 
                    temp_url, 
                    ignore.case=TRUE), 
               ignore.case=TRUE)
  url <- strsplit(temp, '/')[[1]][1]
  return(stripSubdomain(url))
}

url <- sapply(dooo_temp$user_url, get_root_url)

## add new columns to the data frame
dooo_with_good_dates <- cbind(dooo_temp, date, year, month, url)

## merge DoOO data with faculty status
dooo <- merge(dooo_with_good_dates, dooo_faculty_status, by.x = 'ID', by.y = 'user_id', all.x = TRUE, all.y = FALSE)


## clean email address and extract netID

is.umw <- function (email_address) {
  if (grepl('umw.edu', email_address, ignore.case = TRUE)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

clean_umw_email <- function (email_address) {
  if (grepl('@mail.umw.edu', email_address, ignore.case = TRUE)) {
    return(gsub('@mail.umw.edu', '@umw.edu', email_address, ignore.case = TRUE))
  } else if (grepl('@umw.edu', email_address, ignore.case = TRUE)) {
    return(tolower(email_address))
  } else {
    return('no UMW email found')
  }
}

get_netID <- function (email_address) {
  clean <- clean_umw_email(email_address)
  if (grepl('umw.edu', clean)) {
    return(gsub('@umw.edu', '', clean))
  } else {
    return(NA)
  }
}

################# import enom data and process for merge with umw.domains data

## read in and subset data from enom report

enom_temp <- read.csv('dataSources/enomReport.csv')
enom_temp <- subset(enom_temp, select = c('domainname', 'EffStartDate', 'CalEndDate'))

## function to strip time information and get date in clean format

dateOnly <- function (long_date) {
  return(as.character(as.Date(strsplit(as.character(long_date), ' ')[[1]][1], format = '%m/%d/%Y')))
}

## clean up domain name type and assemble new data frame with clean information

url <- as.character(enom_temp$domainname)
reg_date <- sapply(enom_temp$EffStartDate, dateOnly)
exp_date <- sapply(enom_temp$CalEndDate, dateOnly)
enom <- as.data.frame(cbind(url, reg_date, exp_date))

## is domain active on a particular date?

isDomainActive <- function (record, date) {
  registered <- record[2]
  expired <- record[3]
  if (date >= registered & date < expired) {
    return(TRUE)
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

## make single record for each domain in enom

combineDomainRecords <- function (domain, data_source) {
  data_subset <- data_source[data_source$url == domain,]
  url <- as.character(data_subset$url[1])
  reg_date <- as.character(sort(data_subset$reg_date)[1])
  exp_date <- as.character(sort(data_subset$exp_date, decreasing = TRUE)[1])
  return(cbind(url, reg_date, exp_date))
}

singleRecordPerDomain <- function (data_source) {
  domains <- unique(data_source$url)
  new_db <- sapply(domains, combineDomainRecords, data_source)
  final_db <- as.data.frame(t(new_db))
  colnames(final_db) <- c('url', 'reg_date', 'exp_date')
  return(final_db)
}

enom_simplified <- singleRecordPerDomain(enom)

## merge dooo with enom

dooo <- merge(dooo, 
              enom_simplified, 
              by.x = 'url', 
              by.y = 'url', 
              all.x = TRUE, 
              all.y = TRUE)



## create academic year (August-July) and term designations

month <- function(unparsed_date) {
  return(as.integer(substr(unparsed_date, 6, 7)))
}

year <- function(unparsed_date) {
  return(as.integer(substr(unparsed_date, 1, 4)))
}

acad <- function(unparsed_date) {
  year <- year(unparsed_date)
  month <- month(unparsed_date)
  if (month >= 1 & month <= 7) {
    return(paste((year - 1), '-', year, sep=''))
  } else if (month >= 8 & month <= 12) {
    return(paste(year, '-', (year + 1), sep=''))
  } else {
    return(NA)
  }
}

yearMonth <- function(unparsed_date) {
  year <- year(unparsed_date)
  month <- month(unparsed_date)
  return(paste(year, '-', sprintf('%02d', month), sep = ''))
}

find_term <- function(unparsed_date) {
  year <- year(unparsed_date)
  month <- month(unparsed_date)
  if (month >= 8 & month <= 12) {
    return('fall')
  } else if (month >= 1 & month <= 4) {
    return('spring')
  } else if (month >= 5 & month <= 7) {
    return('summer')
  }
}

yearTerm <- function(unparsed_date) {
  academic_year <- acad(unparsed_date)
  term <- find_term(unparsed_date)
  return(paste(academic_year, term))
}

dateToUse <- function(record) {
  if (!is.na(record['reg_date']) & !is.na(record['date'])) {
    if (record['reg_date'] <= record['date']) {
      return(record['reg_date'])
    } else {
      return(record['date'])
    }
  } else if (!is.na(record['reg_date'])) {
    return(record['reg_date'])
  } else if (!is.na(record['date'])) {
    return(record['date'])
  } else {
    return(NA)
  }
}

signup_date <- apply(dooo, MARGIN = 1, dateToUse)
dooo <- cbind(dooo, signup_date)

academic_year <- sapply(dooo$signup_date, acad)
year_month <- sapply(dooo$signup_date, yearMonth)
term <- sapply(dooo$signup_date, find_term)
year_term <- sapply(dooo$signup_date, yearTerm)
netID <- sapply(dooo$user_email, get_netID)

## add singup times to dooo data frame

dooo <- cbind(dooo, academic_year, year_month, term, year_term, netID)

## read in data from Banner database dump

banner_temp <- read.csv('dataSources/banner_data.csv')
banner <- subset(banner_temp, select = c('netid', 'groupname', 'stst'))

## fix groupname from stst

change_groupname <- function(record) {
  group <- record[2]
  statuscode <- record[3]
  if (group == 'Student') {
    return(group)
  } else if (group %in% c('Faculty', 'Staff')) {
    return('Faculty/Staff')
  } else if (group == 'Faculty/Staff,Student') {
    return('Faculty/Staff')
  } else if (statuscode %in% c('AS', 'IG', 'IS')) {
    return('Student')
  } else {
    return('Student')
  }
}

status_temp <- apply(banner, MARGIN = 1, change_groupname)

banner_fac_stud <- cbind(banner, status_temp)
banner_fac_stud <- subset(banner_fac_stud, select = c('netid', 'status_temp'))

## merge DoOO and Banner data frames

dooo_fac_stud <- merge(dooo, banner_fac_stud, by.x = 'netID', by.y = 'netid', all.x = TRUE, all.y = FALSE)

## fix status from umw.domains meta tag

change_status <- function(record) {
  faculty_status <- record['meta_value']
  banner_status <- record['status_temp']
  if (banner_status %in% c('Faculty/Staff')) {
    return(banner_status)
  } else if (faculty_status %in% c('yes')) {
    return('Faculty/Staff')
  } else {
    return('Student')
  }
}

status <- apply(dooo_fac_stud, MARGIN = 1, change_status)
dooo_fac_stud <- cbind(dooo_fac_stud, status)
dooo_fac_stud <- subset(dooo_fac_stud, 
                        select = c('netID', 'user_email', 'url', 'user_url', 
                                   'display_name', 'status', 'date', 'reg_date',
                                    'exp_date', 'signup_date', 'year', 
                                   'month', 'year_month', 'academic_year', 
                                   'term', 'year_term'))


## remove duplicates in dooo_fac_stud

keepNetIDVersion <- function (domain, data_source) {
  data_subset <- data_source[data_source$url == domain,]
  if (sum(!is.na(data_subset$netID)) > 0) {
    subset_with_netID <- data_subset[!is.na(data_subset$netID),][1,]
  } else {
    subset_with_netID <- data_subset[1,]
  }
  
  netID <- as.character(subset_with_netID$netID[1])
  user_email <- as.character(subset_with_netID$user_email[1])
  url <- as.character(subset_with_netID$url[1])
  user_url <- as.character(subset_with_netID$user_url[1])
  display_name <- as.character(subset_with_netID$display_name[1])
  status <- as.character(subset_with_netID$status[1])
  date <- as.character(subset_with_netID$date[1])
  reg_date <- as.character(subset_with_netID$reg_date[1])
  exp_date <- as.character(subset_with_netID$exp_date[1])
  signup_date <- as.character(subset_with_netID$signup_date[1])
  year <- as.character(subset_with_netID$year[1])
  month <- as.character(subset_with_netID$month[1])
  year_month <- as.character(subset_with_netID$year_month[1])
  academic_year <- as.character(subset_with_netID$academic_year[1])
  term <- as.character(subset_with_netID$term[1])
  year_term <- as.character(subset_with_netID$year_term[1])
  
  return(cbind(netID, user_email, url, user_url, display_name, status, date,
               reg_date, exp_date, signup_date, year, month, year_month, 
               academic_year, term, year_term))
}

removeUMWDomainsDuplicates <- function (data_source) {
  domains <- unique(data_source$url)
  new_db <- sapply(domains, keepNetIDVersion, data_source)
  final_db <- as.data.frame(t(new_db))
  colnames(final_db) <- colnames(data_source)
  return(final_db)
}

## generate and save merged and cleaned table

dooo_fac_stud_simplified <- removeUMWDomainsDuplicates(dooo_fac_stud)
write.csv(dooo_fac_stud_simplified, 'dataOutput/dooo_simplified_and_merged.csv')

# is domain active on a particular date?

isDomainActive <- function (record, date) {
  registered <- as.character(record['signup_date'])
  expired <- as.character(record['exp_date'])
  if (!is.na(registered) & !is.na(expired) & date >= registered & date < expired) {
    return(TRUE)
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

date_list <- constructListOfDates('2012-06-01', '2016-11-28')
activeDomains <- as.data.frame(sapply(date_list, 
                                      countActiveDomains, 
                                      dooo_fac_stud_simplified))
domainActivity <- cbind(date_list, activeDomains)
colnames(domainActivity) <- c('date', 'domains')

## new signup tables

signups_by_year_total <- as.data.frame(table(dooo_fac_stud_simplified$academic_year))
colnames(signups_by_year_total) <- c('academicYear', 'total')
signups_by_year_students <- as.data.frame(table(dooo_fac_stud_simplified[dooo_fac_stud_simplified$status=='Student',]$academic_year))
colnames(signups_by_year_students) <- c('academicYear', 'students')
signups_by_year_facStaff <- as.data.frame(table(dooo_fac_stud_simplified[dooo_fac_stud_simplified$status=='Faculty/Staff',]$academic_year))
colnames(signups_by_year_facStaff) <- c('academicYear', 'facStaff')
signups_by_year_temp <- merge(signups_by_year_total, signups_by_year_students, by.x = 'academicYear', by.y = 'academicYear', all.x = TRUE, all.y = TRUE)
signups_by_year <- merge(signups_by_year_temp, signups_by_year_facStaff, by.x = 'academicYear', by.y = 'academicYear', all.x = TRUE, all.y = TRUE)

signups_by_year_term_total <- as.data.frame(table(dooo_fac_stud_simplified$year_term))
colnames(signups_by_year_term_total) <- c('term', 'total')
signups_by_year_term_students <- as.data.frame(table(dooo_fac_stud_simplified[dooo_fac_stud_simplified$status=='Student',]$year_term))
colnames(signups_by_year_term_students) <- c('term', 'students')
signups_by_year_term_facStaff <- as.data.frame(table(dooo_fac_stud_simplified[dooo_fac_stud_simplified$status=='Faculty/Staff',]$year_term))
colnames(signups_by_year_term_facStaff) <- c('term', 'facStaff')
signups_by_year_term_temp <- merge(signups_by_year_term_total, signups_by_year_term_students, by.x = 'term', by.y = 'term', all.x = TRUE, all.y = TRUE)
signups_by_year_term <- merge(signups_by_year_term_temp, signups_by_year_term_facStaff, by.x = 'term', by.y = 'term', all.x = TRUE, all.y = TRUE)

signups_by_year_month_total <- as.data.frame(table(dooo_fac_stud_simplified$year_month))
colnames(signups_by_year_month_total) <- c('month', 'total')
signups_by_year_month_students <- as.data.frame(table(dooo_fac_stud_simplified[dooo_fac_stud_simplified$status=='Student',]$year_month))
colnames(signups_by_year_month_students) <- c('month', 'students')
signups_by_year_month_facStaff <- as.data.frame(table(dooo_fac_stud_simplified[dooo_fac_stud_simplified$status=='Faculty/Staff',]$year_month))
colnames(signups_by_year_month_facStaff) <- c('month', 'facStaff')
signups_by_year_month_temp <- merge(signups_by_year_month_total, signups_by_year_month_students, by.x = 'month', by.y = 'month', all.x = TRUE, all.y = TRUE)
signups_by_year_month <- merge(signups_by_year_month_temp, signups_by_year_month_facStaff, by.x = 'month', by.y = 'month', all.x = TRUE, all.y = TRUE)

## active domains over time

month <- 0:(length(domainActivity$date) - 1)
domainActivity <- cbind(domainActivity, month)
row.names(domainActivity) <- NULL

# Export data to CSV files for the Shiny App

write.csv(signups_by_year, 'dataOutput/signups_academic_year.csv', row.names = FALSE)
write.csv(signups_by_year_term, 'dataOutput/signups_academic_term.csv', row.names = FALSE)
write.csv(signups_by_year_month, 'dataOutput/signups_month.csv', row.names = FALSE)
write.csv(domainActivity, 'dataOutput/active_domains_by_month.csv', row.names = FALSE)

