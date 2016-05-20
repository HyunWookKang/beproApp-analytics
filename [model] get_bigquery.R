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

all_month_event_table = all_month_rawtable[order(all_month_rawtable$user_id, all_month_rawtable$time, decreasing = TRUE),]

# 세션 카운트 / 세션 시작 시간 설정

set_session_info = function(df) {
  for (i in 1:nrow(df)) {
    j = i+1
    if (j < nrow(df)) {
      if (as.numeric(difftime(df[,'time'][i], df[,'time'][j]), units='secs') >= 1800) {
        if (match(TRUE, is.na(df[,'session'])) == 1) {
          df[,'session'][match(TRUE, is.na(df[,'session'])):i] = 1
          df[,'session_time'][match(TRUE, is.na(df[,'session_time'])):i] = df[i,'time']
        } else {
          k = df[,'session'][match(TRUE, is.na(df[,'session']))-1] + 1
          df[,'session'][match(TRUE, is.na(df[,'session'])):i] = k
          df[,'session_time'][match(TRUE, is.na(df[,'session_time'])):i] = df[j,'time']
        }
      }
    } else if (j == nrow(df)) {
      if (match(TRUE, is.na(df[,'session'])) == 1) {
        df[, 'session'][match(TRUE, is.na(df['session'])):j] = 1
        df[,'session_time'][match(TRUE, is.na(df[,'session_time'])):j] = df[j,'time']
      } else {
        k = df[,'session'][match(TRUE, is.na(df[,'session']))-1] + 1
        df[,'session'][match(TRUE, is.na(df[,'session'])):j] = k
        df[,'session_time'][match(TRUE, is.na(df[,'session_time'])):j] = df[j,'time']
      }
    } else if (nrow(df) == 1) {
      df[, 'session'][match(TRUE, is.na(df['session']))] = 1
      df[,'session_time'][match(TRUE, is.na(df[,'session_time']))] = df[1,'time']
    }
  }
  return(df)
}

completeFun = function(data, desiredCols) {
  completeVec = complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

session_table = tw_event_table

refined_table = completeFun(session_table, 'user_id')
refined_table$session = rep(NA, nrow(refined_table))
refined_table$session_time = rep(NA, nrow(refined_table))

system.time(
  for (i in unique(refined_table$user_id)[!is.na(unique(refined_table$user_id))]) {
    newdf = set_session_info(filter(refined_table, user_id == as.integer(i)))
    refined_table[refined_table$user_id==as.integer(i), 'session'] = newdf[newdf$user_id==as.integer(i), 'session']
    refined_table[refined_table$user_id==as.integer(i), 'session_time'] = newdf[newdf$user_id==as.integer(i), 'session_time']
  }
)
#select(filter(test3, user_id == as.integer(9985)), session) = 1

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


