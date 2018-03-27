library(tidyverse)
library(stringr)

domains <- read.csv('dataSources/whmcs_domains.csv')
clients <- read.csv('dataSources/whmcs_clients.csv')

whmcs <- domains %>%
  full_join(clients) %>%
  unique() %>%
  select(email, url = domain, whmcs_client = client_name, whmcs_id = user_id)
