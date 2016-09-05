library(dplyr)

page_url_regex = grep('*leagues*', session_data$page_url)
visit_league = session_data[page_url_regex,]

visit_league = visit_league %>%
  filter(!is.na(href)) %>%
  filter(!is.na(ngClick)) %>%
  filter(ngClick != "" | (href != ""))

backup = visit_league
backup = backup %>%
  mutate(sort = ifelse(ngClick != "", ngClick, 
                       ifelse(grepl('*match*', href), 'match', 
                       ifelse(grepl('*video*', href), 'video',
                       ifelse(grepl('*team*', href), 'team',
                       ifelse(grepl('*images*', href), 'image',
                       ifelse(grepl('*users*', href), 'user',
                       ifelse(grepl('*league*', href), 'league', href))))))))
backup = select(backup, -compare_time)
backup = backup[!duplicated(backup),]
backup = filter(backup, ngClick != '$ionicGoBack()')


sort_count = as.data.frame(table(backup$sort))
sort_count = sort_count[order(-sort_count$Freq),]
sort_count = sort_count %>%
  mutate(percentage = round(Freq/sum(sort_count$Freq)*100,2))

sort_count_ex_match = sort_count %>%
  mutate(new_percentage = round(Freq/sum(sort_count$Freq[2:nrow(sort_count)])*100,2))


pie(sort_count$Freq[2:nrow(sort_count)], 
    col = sort_count$Var1[2:nrow(sort_count)], 
    labels = sort_count$Var1,
    clockwise = TRUE)

sum(sort_count$percentage[2:nrow(sort_count)])
sum(sort_count_ex_match$new_percentage[2:nrow(sort_count_ex_match)])
pie(sort_count_ex_match$new_percentage[2:nrow(sort_count_ex_match)], 
    col = sort_count_ex_match$Var1[2:nrow(sort_count_ex_match)], 
    labels = sort_count_ex_match$Var1,
    clockwise = TRUE)


