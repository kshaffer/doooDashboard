library(jsonlite)
library(tidyverse)
library(lubridate)

# download user table from WP REST API

wp_rest_api_url <- ''
wp <- fromJSON(wp_rest_api_url)


# function to extract netid from UWM email addresses

email_to_netid <- function (email) {
  if (grepl('@mail.umw.edu', email, ignore.case = TRUE)) {
    return(tolower(unlist(strsplit(email, '@'))[1]))
  } else if (grepl('@umw.edu', email, ignore.case = TRUE)) {
    return(tolower(unlist(strsplit(email, '@'))[1]))
  } else {
    return(NA)
  }
}


# function to extract desirable information from WP REST API JSON data
# generate tidy tibble of data, one record at a time

parse_wp_json <- function(user_number) {
  user_record <- unlist(wp[[user_number]])
  udf <- cbind(netid = email_to_netid(unlist(user_record['data.user_email'])),
               wp_id = unlist(user_record['data.ID']),
               wp_login = unlist(user_record['data.user_login']),
               wp_email = unlist(user_record['data.user_email']),
               wp_url = unlist(user_record['data.user_url']),
               wp_role = unlist(user_record['roles']),
               wp_signup = substr(as.character(unlist(user_record['data.user_registered'])), 1, 10))
  return(as_tibble(udf))
}


# join all records into a single tibble

wp_users <- as_tibble(rbind(t(sapply(1:length(wp), parse_wp_json)))) %>%
  unnest() %>%
  mutate(wp_signup = ymd(wp_signup))

write_csv(wp_users, '/srv/shiny-server/dataSources/wp_api_data.csv')
