library(tidyverse)
library(lubridate)

# functions to clean dates

to_month <- function(month_string) {
  if (month_string == 'JAN') {
    return('01')
  } else if (month_string == 'JAN') {
    return('02')
  } else if (month_string == 'MAR') {
    return('03')
  } else if (month_string == 'APR') {
    return('04')
  } else if (month_string == 'MAY') {
    return('05')
  } else if (month_string == 'JUN') {
    return('06')
  } else if (month_string == 'JUL') {
    return('07')
  } else if (month_string == 'AUG') {
    return('08')
  } else if (month_string == 'SEP') {
    return('09')
  } else if (month_string == 'OCT') {
    return('10')
  } else if (month_string == 'NOV') {
    return('11')
  } else if (month_string == 'DEC') {
    return('12')
  } 
}

to_grad_date <- function(grad_date_raw) {
  if (!is.na(grad_date_raw)) {
    grad_date <- paste('20',
                       substr(as.character(grad_date_raw),8,9), 
                       '-', 
                       to_month(substr(as.character(grad_date_raw),4,6)),
                       '-',
                       substr(as.character(grad_date_raw),1,2),
                       sep = '')
    return(as.character(grad_date))
  } else {
    return(NA)
  }
}

dateOnly <- function (long_date) {
  return(ymd(as.character(as.Date(strsplit(as.character(long_date), ' ')[[1]][1], format = '%m/%d/%Y'))))
}

# function to find or generate netid

generate_netid <- function (netid, email) {
  new_netid <- ifelse(is.na(netid) | netid == 'Not Provided',
                      ifelse(grepl('umw.edu', email, ignore.case = TRUE),
                             tolower(unlist(strsplit(banner_temp[tolower(banner_temp$email) == tolower(email),]$email, '@'))[1]),
                             NA),
                      tolower(netid)
                      )
  return(new_netid)
}

clean_netid <- function (record_number) {
  record = banner_temp[record_number,]
  cbind(username = record['username'],
        firstname = record['firstname'],
        lastname = record['lastname'],
        email = record['email'],
        email2 = record['email2'],
        netid = generate_netid(record['netid'], record['email']),
        groupname = record['groupname'],
        stst = record['stst'],
        status = record['status'],
        datecreated = record['datecreated'],
        class = record['class'],
        grad_date = record['grad_date'],
        last_active_semester = record['last_active_semester'],
        domain = record['domain'])
}

# import data from banner report

banner_temp <- read_csv('/srv/shiny-server/dataSources/dooo_accounts_current.csv')

banner <- banner_temp %>%
  mutate(netid = mapply(generate_netid, netid, email),
         grad_date_sort = ymd(sapply(grad_date, to_grad_date)),
         whmcs_signup = mdy(datecreated)) %>%
  select(netid, email, url = domain, whmcs_signup, groupname, stst, status, class, grad_date, grad_date_sort, last_active_semester)
