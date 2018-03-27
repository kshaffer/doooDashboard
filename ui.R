# Domain of One's Own Dashboard - Shiny App UI script
library(shiny)
library(shinydashboard)

# Title header
header <- dashboardHeader(
  title = 'Domain of One\'s Own',
  titleWidth = 300
)

# Sidebar with DoOO links
sidebar <- dashboardSidebar(
  width = 300,
  div(
    style='margin: 12px',
    p('For more information about Domain of One\'s Own, visit the ', 
      a(href='http://umwdtlt.com', 'Division of Teaching and Learning Technologies website.', style='text-decoration: underline')
    ),
    p('To claim your domain, visit ', 
      a(href='http://umwdomains.com', 'umwdomains.com.', style='text-decoration: underline')
    ),
    p('Other Resources:'),
    tags$ul(
      tags$li(a(href='http://umwdtlt.com/documentation', 
                'DoOO Documentation and Curriculum', 
                style='text-decoration: underline',
                target='_blank')),
      tags$li(a(href='http://umwdtlt.com/dtlt-blog', 
                'DTLT Blog', 
                style='text-decoration: underline',
                target='_blank'
                )),
      tags$li(a(href='http://dkc.umw.edu', 
                'Digital Knowledge Center', 
                style='text-decoration: underline',
                target='_blank'
                ))
    )
  )
  )

# Main dashboard layout
body <- dashboardBody(
  # Title header
  fluidRow(
    h1(strong('Domain of One\'s Own'), align='center'),
    h1('University of Mary Washington', align='center'),
    h1(htmlOutput('domains_served'), align='center'),
    br()
  ),
  
  fluidRow(
    # Total active domains over time - plot/table tabs selected by user
    box(
      tabsetPanel(
        tabPanel('Plot', plotOutput('domains_over_time')),
        tabPanel('Table', dataTableOutput('active_domain_table'))
      )
    ),
    
    # General text information about DoOO with highlight total stats
    box(
        h1('Overview'),
        hr(),
        p('Domain of One\'s Own (DoOO) is a project at the University of Mary Washington managed by the Division of Teaching and Learning Technologies (DTLT).'),
        p('The DoOO project allows UMW students, faculty, and staff to register their own domain name and associate it with a hosted web space, free of charge while at UMW. With their Domain and corresponding web space, users will have the opportunity and flexibility to design and create a meaningful and vibrant digital presence.'),
        
        p(htmlOutput('active_total')),
        p(htmlOutput('domain_total'))
    )
  ),
  
  # Title header for second row of dashboard
  fluidRow(
    hr(),
    h1('Domain signups over time', align='center'),
    hr()
  ),

  fluidRow(
    column(8,
      
      # New registrations over time - plot/table tabs selected by user
      tabsetPanel(
               tabPanel('Plot', plotOutput('regs_over_time')),
               tabPanel('Table', dataTableOutput('signups_table'))
      )
    ),
    column(4,
       # Menus for user-selected time frame and status filters
       wellPanel(
         selectInput('user_group',
                     label = 'Choose a user group',
                     choices = c('All', 'Students', 'Faculty/Staff'),
                     selected = 'All'),
         selectInput('time_frame',
                     label = 'Group registrations by',
                     choices = c('Academic Year', 'Semester', 'Month'),
                     selected = 'Academic Year')
       )
    )
  ),
  
  # Title header for third row of dashboard
  fluidRow(
    hr(),
    h1('Student cohorts', align='center')
  ),
  
  fluidRow(
    column(2),
    
    column(8,
      # General text information about DoOO with highlight total stats
      h3('After starting strong with upper-class students, DTLT and the DKC hav made a concerted effort to engage new UMW students with Domain of One\'s Own in their first year of study, through both Freshman Seminars (FSEMs) and new-student orientation.'),
      hr()
    ),
    
    column(2)
  ),
  
  fluidRow(
    column(6,
           
           # New registrations over time - plot/table tabs selected by user
           tabsetPanel(
             tabPanel('Plot', plotOutput('grad_class_column_plot')),
             tabPanel('Table', dataTableOutput('grad_class_column_table'))
           )
    ),
    column(6,
           # New registrations over time - plot/table tabs selected by user
           tabsetPanel(
             tabPanel('Plot', plotOutput('grad_class_line_plot')),
             tabPanel('Table', dataTableOutput('grad_class_line_table'))
           )
    )
  )
)


# Build the dashboard page
shinyUI(
  dashboardPage(header, sidebar, body, skin = 'black')
)


