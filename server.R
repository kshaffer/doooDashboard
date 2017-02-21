# Domain of One's Own Dashboard - Shiny App server script

# Import ggplot and data tables created by dooo.R
library(tidyverse)
library(lubridate)
dooo <- read_csv('/srv/shiny-server/dataOutput/dooo_merged_dates.csv')
active_domains <- read_csv('/srv/shiny-server/dataOutput/active_domains_by_month.csv')
current_domains <- read_csv('/srv/shiny-server/dataOutput/current_active_domains.csv')

# Server backend

shinyServer(
  function(input, output) {
    
    # Text info about DoOO
    output$active_total <- renderUI(
      {HTML(paste('UMW Domain of One\'s Own has <strong>',
                  tail(active_domains, n=1)$domains, 
                  ' active domains,</strong> as of ',
                  gsub(' 0', ' ', format(strptime(max(ymd(dooo[!is.na(dooo$signup),]$signup)), '%Y-%m-%d'), '%B %d, %Y')),
                  '.',
                  sep=''))})
    
    output$domain_total <- renderUI(
      {HTML(paste('We have assigned a total of <strong>',
                  length(unique(dooo[!is.na(dooo$url),]$url)), 
                  ' domains</strong> to <strong>',
                  length(unique(dooo %>% filter(!is.na(url), !is.na(netid), group_status == 'Student') %>% select(netid))$netid), 
                  ' students</strong> and <strong>',
                  length(unique(dooo %>% filter(!is.na(url), !is.na(netid), group_status == 'Faculty/Staff') %>% select(netid))$netid), 
                  ' faculty/staff</strong> since the beginning of the Domain of One\'s Own program.',
                  sep=''))})
    
    # Table of currently active domains
    output$active_domain_table <- renderDataTable(
      { subset(active_domains[order(active_domains$date, decreasing=TRUE),], select = c('date', 'domains')) },
      options = list(lengthMenu = c(10, 20, 30), 
                     pageLength = 10
                     )
      )
    
    # Plot of active domains over time
    output$domains_over_time <- renderPlot({
      active_domains %>%
        ggplot(aes(ymd(date), as.numeric(domains), fill = -as.numeric(domains))) +
        geom_col() +
        scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
        #geom_smooth(se = FALSE) +
        ggtitle('Total Active Domains by Month') +
        theme(plot.title = element_text(lineheight=.8, face='bold', size=16, hjust=0.5),
              legend.position="none") +
        xlab('Month') +
        ylab('Number of active domains')
    })
    
    # Plot of domain registration over time, by user-selected timeframe and status parameters
    reg_data <- reactive({
      switch(input$user_group,
             'All' = c('Student', 'Faculty/Staff', 'Unknown', NA),
             'Students' = 'Student',
             'Faculty/Staff' = 'Faculty/Staff')
    })
    
    time_frame <- reactive({
      switch(input$time_frame,
             'Academic Year' = 'academic_year',
             'Semester' = 'year_term',
             'Month' = 'year_month'
             )
    })
    
    color <- reactive({
      switch(input$user_group,
             'All' = 'cornflowerblue',
             'Students' = 'darkolivegreen4',
             'Faculty/Staff' = 'darkred')
    })
    
    output$regs_over_time <- renderPlot({
      reg_data <- reg_data()
      time_frame <- time_frame()
      color <- color()
      
      plot_table <- as_tibble(dooo[dooo$group_status %in% reg_data,][,c('group_status', time_frame)])
      colnames(plot_table) <- c('group_status', 'dooo_time')
      plot_table <- plot_table %>% filter(!is.na(dooo_time))
      
      plot_table %>%
        ggplot() +
          geom_bar(aes(x = dooo_time,
                       fill = group_status
                       )) +
          ggtitle(paste('New DoOO Registrations per', input$time_frame)) +
          theme(plot.title = element_text(lineheight=.8, face='bold', size=16, hjust=0.5)) +
          labs(fill = 'Status') +
          xlab(input$time_frame) +
          ylab('New registrations') +
          coord_flip()
    })
    
    # Table of new domain signups, by user-selected timeframe parameter
    output$signups_table <- renderDataTable(
      {
        #chooseTimeFrame(time_frame())[order(chooseTimeFrame(time_frame())[1], decreasing=TRUE),]
        time_frame <- time_frame()
        group_list <- lapply(c(time_frame, 'group_status'), as.symbol)
        dooo %>%
          select(group_status, matches(time_frame)) %>%
          group_by_(.dots=group_list) %>%
          summarize(count = n()) %>%
          spread(group_status, count) 
          
        },
      options = list(lengthMenu = c(12, 24, 36), pageLength = 12)
    )
  }
)