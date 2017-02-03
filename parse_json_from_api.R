library(jsonlite)
library(tidyverse)

wp <- fromJSON('users_backup.json')

parse_wp_json <- function(user_number) {
  user_record <- unlist(wp[[user_number]])
  udf <- cbind(wp_id = unlist(user_record['data.ID']),
               wp_login = unlist(user_record['data.user_login']),
               wp_email = unlist(user_record['data.user_email']),
               wp_url = unlist(user_record['data.user_url']),
               wp_role = unlist(user_record['roles']))
  return(as_tibble(udf))
}

db <- as_tibble(rbind(t(sapply(1:length(wp), parse_wp_json)))) %>%
  unnest()
