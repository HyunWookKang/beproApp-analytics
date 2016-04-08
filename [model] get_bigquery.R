library(bigrquery)

#이번주 데이터 불러오기
tw_date = c(20160328:20160331, 20160401:20160403)
tw_full_sql = paste(sql, connect_query_table(tw_date), 'where structPayload.eventData.type == "click" or 
                 structPayload.eventData.type == "swipe" or
                 structPayload.eventData.type == "touch" or
                 structPayload.eventData.type == "view"')

tw_rawtable = query_exec(tw_full_sql, project, max_pages = Inf)
names(tw_rawtable) = c('time', 'ngClick', 'page_url', 'event_type', 'user_id', 'user_agent')

tw_event_table = tw_rawtable[order(tw_rawtable$user_id, tw_rawtable$time, decreasing = TRUE),]

# 지난주 데이터 불러오기
lw_date = c(20160321:20160327)

lw_full_sql = paste(sql, connect_query_table(lw_date), 'where structPayload.eventData.type == "click" or 
                 structPayload.eventData.type == "swipe" or
                 structPayload.eventData.type == "touch" or
                 structPayload.eventData.type == "view"')
lw_rawtable = query_exec(lw_full_sql, project, max_pages = Inf)
names(lw_rawtable) = c('time', 'ngClick', 'page_url', 'event_type', 'user_id', 'user_agent')

lw_event_table = lw_rawtable[order(lw_rawtable$user_id, lw_rawtable$time, decreasing = TRUE),]

# 한 달 데이터 불러오기
all_month_date = c(20160315:20160331, 20160401:20160403)

month_sql = 'select metadata.timestamp, structPayload.userId, from '

all_month_full_sql = paste(month_sql, connect_query_table(all_month_date))
all_month_rawtable = query_exec(all_month_full_sql, project, max_pages = Inf)
names(all_month_rawtable) = c('time', 'user_id')

all_month_event_table = all_month_rawtable[order(all_month_rawtable$user_id, all_month_rawtable$time, decreasing = TRUE),]
