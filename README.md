# doooDashboard
R/Shiny scripts to generate a [public dashboard](http://data.umwdomains.com) of analytics and projections for UMW's Domain of One's Own initiative

## scripts

```dooo.R``` is the master script that calls other R scripts to collect, clean, transform, merge, and analyze data.

```enom_api.R``` queries our account for Enom (domain registrar) and fetches information about registered domains, including registration status and expiration date. (Query URL contains account and password, and has been omitted for security purposes.)

```import_banner_data.R``` imports and cleans data in a file pulled manually from our institutional information system database (```dooo_accounts_2017_02_10.csv```, which has been omitted for user privacy).

```merge_enom_reports.R``` pulls historical transaction data out of a set of manual reports pulled from Enom. These contain registration, renewal, and expiration dates for all domains in the history of the account, some of which information is not easily available for all domains via regularly occurring API calls. These data files (omitted for user privacy) are located in ```dataSources/individual_enom_reports```.

```parse_wp_api.R``` pulls account data for all users on [umw.domains](https://umw.domains), the site where users generate and access their Domain of One's Own account.

```report.R``` uses ```rmarkdown``` and ```knitr``` to generate a monthly HTML report detailing and visualizing signups, renewals, and expirations by student/faculty status and by class (Fr/So/Jr/Sr) in the previous month and using historical data to predict signups and expirations in the coming month.

```server.R``` and ```ui.R``` contain the code for our Shiny Server dashboard, found at [data.umwdomains.com](http://data.umwdomains.com). This dashboard presents and visualizes information about the scale of the Domain of One's Own program over time, and user signups over time, broken down by academic year, semester, and month, as well as by student and faculty/staff.
