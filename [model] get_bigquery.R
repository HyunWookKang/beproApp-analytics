library(bigrquery)
library(dplyr)
#이번주 데이터 불러오기
tw_date = c(20160509:20160515)
tw_full_sql = paste(sql, connect_query_table(tw_date), 'where structPayload.eventData.type == "click" or 
                 structPayload.eventData.type == "swipe" or
                 structPayload.eventData.type == "touch" or
                 structPayload.eventData.type == "view"')

tw_rawtable = query_exec(tw_full_sql, project, max_pages = Inf)
names(tw_rawtable) = c('time', 'ngClick', 'page_url', 'href', 'event_type', 'user_id')

tw_event_table = tw_rawtable[order(tw_rawtable$user_id, tw_rawtable$time, decreasing = TRUE),]

# 지난주 데이터 불러오기
lw_date = c(20160502:20160508)

lw_full_sql = paste(sql, connect_query_table(lw_date), 'where structPayload.eventData.type == "click" or 
                 structPayload.eventData.type == "swipe" or
                 structPayload.eventData.type == "touch" or
                 structPayload.eventData.type == "view"')
lw_rawtable = query_exec(lw_full_sql, project, max_pages = Inf)
names(lw_rawtable) = c('time', 'ngClick', 'page_url', 'href', 'event_type', 'user_id')

lw_event_table = lw_rawtable[order(lw_rawtable$user_id, lw_rawtable$time, decreasing = TRUE),]

# 한 달 데이터 불러오기
all_month_date = c(20160416:20160430, 20160501:20160515)

month_sql = 'select metadata.timestamp, structPayload.userId, from '

all_month_full_sql = paste(month_sql, connect_query_table(all_month_date))
all_month_rawtable = query_exec(all_month_full_sql, project, max_pages = Inf)
names(all_month_rawtable) = c('time', 'user_id')

all_month_event_table = all_month_rawtable %>%
  arrange(user_id, desc(time))

# 세션 카운트 / 세션 시작 시간 설정

session_table = filter(tw_event_table, !is.na(user_id))
session_table$time = as.POSIXct(strptime(session_table$time, '%Y-%m-%d %H:%M:%S'))

session_data = session_table %>%
  group_by(user_id) %>%
  mutate(compare_time = as.numeric(lag(time) - time))

session_data$compare_time[is.na(session_data$compare_time)] = 0

session_data = session_data %>%
  group_by(user_id) %>%
  mutate(session = cumsum(compare_time >= 1800) +1L)

session_data = session_data %>%
  group_by(user_id, session) %>%
  mutate(session_time = time[1])

# 전체 영상 데이터 불러오기
total_date = c(20160315:20160331, 20160401:20160430, 20160501:20160515)

total_video_sql = 'select structPayload.eventData.utcTime,
structPayload.eventData.type,
structPayload.eventData.url,
structPayload.userId
from '
total_video_full_sql = paste(total_video_sql, connect_query_table(total_date), 
                             'where structPayload.eventData.type == "view" and structPayload.eventData.url like "%video%"')

total_video_rawtable = query_exec(total_video_full_sql, project, max_pages = Inf)
names(total_video_rawtable) = c('time', 'type', 'page_url', 'user_id')


