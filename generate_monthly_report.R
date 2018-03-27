library(rmarkdown)

render('report.Rmd', output_file = paste('monthlyReports/', Sys.Date(), '_monthly_report.html', sep = ''))
