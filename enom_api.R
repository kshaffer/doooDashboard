library(XML)
library(tidyverse)
library(lubridate)

enom_api_url <- ''
enom <- xmlParse(readLines(enom_api_url)) %>%
  xmlToList()

data_out <- tibble()
for(i in 1:length(enom$GetReport$ReportDetail)) {
  data_out <- rbind(data_out, as_tibble(t(enom$GetReport$ReportDetail[i][[1]])))
}

enom_data <- data_out %>%
  mutate(ExpirationDate = mdy(ExpirationDate)) %>%
  select(url = DomName,
         reg_status = registrationstatus,
         enom_api_exp_date = ExpirationDate)

write_csv(enom_data, '/srv/shiny-server/dataSources/enom_api_data.csv')