dooo_with_status <- read_csv('dataOutput/dooo_merged_dates.csv')
dooo_with_status <- dooo_with_status %>%
  left_join(user_list) %>%
  mutate(matriculation_year = mapply(find_matriculation_year, matriculation_date, grad_date_sort),
         class_of = matriculation_year + 3,
         year_of_study = as.numeric(str_sub(academic_year, 6, 9)) - matriculation_year + 1)


dooo_with_status %>% 
  filter(is.na(student_class) | !student_class == 'GR') %>%
  count(class_of) %>% 
  ggplot(aes(class_of, n)) + 
  geom_col() +
  ggtitle('Domain of One\'s Own signups by graduating class\n(matriculation dates used for current and inactive students,\ngraduate students excluded)') +
  xlab('Graduating class') +
  ylab('Total signups')

dooo_with_status %>% 
  filter(is.na(student_class) | !student_class == 'GR') %>%
  filter(!is.na(class_of),
         class_of >= 2014) %>%
  count(academic_year, class_of) %>%
  ggplot(aes(academic_year, n, fill = as.factor(class_of))) + 
  geom_col() +
  ggtitle('Domain of One\'s Own signups by academic year\n(matriculation dates used for current and inactive students,\ngraduate students excluded)') +
  xlab('Signup year') +
  ylab('Total signups') +
  guides(fill=guide_legend(title = 'Graduating class'))

dooo_with_status %>% 
  filter(is.na(student_class) | !student_class == 'GR') %>%
  filter(!is.na(class_of),
         class_of >= 2014) %>%
  count(academic_year, class_of) %>%
  ggplot(aes(as.numeric(str_sub(academic_year, 6, 9)), n, color = as.character(class_of))) + 
  geom_line(alpha = 0.8, size = 0.7) +
  ggtitle('Domain of One\'s Own signups by academic year\n(matriculation dates used for current and inactive students,\ngraduate students excluded)') +
  xlab('Signup year (academic year ending)') +
  ylab('Total signups') +
  guides(color = guide_legend(title = 'Graduating class'))

dooo_with_status %>%
  filter(is.na(student_class) | !student_class == 'GR') %>%
  filter(!is.na(class_of),
         class_of >= 2014) %>%
  filter(year_of_study < 7,
         year_of_study > 0) %>%
  count(year_of_study, class_of) %>%
  ggplot(aes(year_of_study, n, color = as.factor(class_of))) +
  geom_line() +
  ggtitle('Domain of One\'s Own signups by year of study\n(graduate students excluded)') +
  xlab('Year of study (year of matriculation = 1)') +
  ylab('Total signups') +
  guides(color = guide_legend(title = 'Graduating class'))

dooo_with_status %>%
  filter(is.na(student_class) | !student_class == 'GR') %>%
  filter(!is.na(class_of),
         class_of >= 2014) %>%
  filter(year_of_study < 7,
         year_of_study > 0) %>%
  count(year_of_study, class_of) %>%
  ggplot(aes(year_of_study, n, fill = as.factor(class_of))) +
  geom_col() +
  ggtitle('Domain of One\'s Own signups by year of study\n(graduate students excluded)') +
  xlab('Year of study (year of matriculation = 1)') +
  ylab('Total signups') +
  guides(fill = guide_legend(title = 'Graduating class'))


dooo_with_status %>%
  filter(is.na(student_class) | !student_class == 'GR') %>%
  filter(!is.na(class_of),
         class_of >= 2014) %>%
  filter(year_of_study < 7,
         year_of_study > 0,
         academic_year >= 2012,
         academic_year < 2017) %>%
  count(academic_year, year_of_study) %>%
  ggplot(aes(as.numeric(str_sub(academic_year, 6, 9)), n, color = as.factor(year_of_study))) +
  geom_line(alpha = 0.8, size = 0.8) +
  ggtitle('Domain of One\'s Own signups by year of study\n(graduate students excluded)') +
  xlab('Signup year(academic year ending)') +
  ylab('Total signups') +
  guides(color = guide_legend(title = 'Year of study\n(Fr=1, So=2, ...)'))

dooo_with_status %>%
  filter(is.na(student_class) | !student_class == 'GR') %>%
  filter(!is.na(class_of),
         class_of >= 2014) %>%
  filter(year_of_study < 7,
         year_of_study > 0) %>%
  count(academic_year, year_of_study) %>%
  ggplot(aes(as.numeric(str_sub(academic_year, 6, 9)), n, fill = as.factor(year_of_study))) +
  geom_col() +
  ggtitle('Domain of One\'s Own signups by year of study\n(graduate students excluded)') +
  xlab('Signup year(academic year ending)') +
  ylab('Total signups') +
  guides(fill = guide_legend(title = 'Year of study\n(Fr=1, So=2,...)'))
