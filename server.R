# Domain of One's Own Dashboard - Shiny App server script

# Import ggplot and data tables created by dooo.R
library('ggplot2')
signups_by_year <- read.csv('dataOutput/signups_academic_year.csv')
signups_by_term <- read.csv('dataOutput/signups_academic_term.csv')
signups_by_month <- read.csv('dataOutput/signups_month.csv')
dooo <- read.csv('dataOutput/dooo_simplified_and_merged.csv')
active_domains <- read.csv('dataOutput/active_domains_by_month.csv')

# Collect time frame from UI menu

chooseTimeFrame <- function (time_frame) {
  if (time_frame == 'academic_year') {
    return(signups_by_year)
  } else if (time_frame == 'year_term') {
    return(signups_by_term)
  } else {
    return(signups_by_month)
  }
}


# Server backend

shinyServer(
  function(input, output) {
    
    # Text info about DoOO
    output$active_total <- renderUI(
      {HTML(paste('UMW Domain of One\'s Own has <strong>',
                  tail(active_domains, n=1)$domains, 
                  ' active domains,</strong> as of ',
                  gsub(' 0', ' ', format(strptime(tail(active_domains, n=1)$date, '%Y-%m-%d'), '%B %d, %Y')),
                  '.',
                  sep=''))})
    
    output$domain_total <- renderUI(
      {HTML(paste('We have assigned a total of <strong>',
                  length(unique(dooo$url)), 
                  ' domains</strong> to <strong>',
                  length(unique(dooo[dooo$status=='Student',]$netID)), 
                  ' students</strong> and <strong>',
                  length(unique(dooo[dooo$status=='Faculty/Staff',]$netID)), 
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
      ggplot(active_domains) +
        geom_point(aes(month, domains)) +
        geom_smooth(aes(month, domains), se = FALSE) +
        ggtitle('Total Active Domains by Month') +
        theme(plot.title = element_text(lineheight=.8, face='bold', size=16, hjust=0.5)) +
        xlab('Months since June 2012 (beginning of DoOO pilot)') +
        ylab('Number of active domains')
    })
    
    # Plot of domain registration over time, by user-selected timeframe and status parameters
    reg_data <- reactive({
      switch(input$user_group,
             'All' = c('Student', 'Faculty/Staff', NA),
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
      
      # barplot(table(
      #   dooo[dooo$status %in% reg_data,][,time_frame]), 
      #   main = paste('New DoOO Registrations per', input$time_frame), 
      #   xlab = input$time_frame,
      #   col = color)
      
      plot_table <- as.data.frame(dooo[dooo$status %in% reg_data,][,c('status', time_frame)])
      colnames(plot_table) <- c('status', 'dooo_time')
      
      ggplot(plot_table) +
        geom_bar(aes(x = dooo_time,
                     fill = status
                     )) +
        ggtitle(paste('New DoOO Registrations per', input$time_frame)) +
        theme(plot.title = element_text(lineheight=.8, face='bold', size=16, hjust=0.5)) +
        xlab(input$time_frame) +
        ylab('New registrations') +
        coord_flip()
    })
    
    # Table of new domain signups, by user-selected timeframe parameter
    output$signups_table <- renderDataTable(
      {
        chooseTimeFrame(time_frame())[order(chooseTimeFrame(time_frame())[1], decreasing=TRUE),]
        },
      options = list(lengthMenu = c(12, 24, 36), pageLength = 12)
    )
  }
)