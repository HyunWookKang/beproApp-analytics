library(bigrquery)
library(dplyr)

all_date = c(20160315:20160331, 20160401:20160430, 20160501:20160531, 20160601:20160614)
match_full_sql = paste(sql, connect_query_table(all_date), 'where structPayload.eventData.type == "click" or 
                    structPayload.eventData.type == "touch" or
                    structPayload.eventData.type == "view"')

match_rawtable = query_exec(match_full_sql, project, max_pages = Inf)

head(match_rawtable)

names(match_rawtable) = c('time', 'ngclick', 'url', 'href', 'type', 'userid')

match_rawtable %>%
  filter(!is.na(href), !is.na(ngclick), !is.na(url)) %>%
  filter(ngclick != "" | (href != ""))  %>%
  filter(ngclick == "openModal('WriteMatchResult')") %>%
  summarise(match_record = n())

match_rawtable %>%
  filter(ngclick == "showPopup('changeTime')") %>%
  summarise(time_click = n())

match_rawtable = arrange(match_rawtable, userid, desc(time))

match_rawtable[which(match_rawtable$ngclick %in% c("openModal('WriteMatchResult')")) + c(-2:2), ][101:200,]

readable_user_id(1063)
