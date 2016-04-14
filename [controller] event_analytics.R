tw_click_event = tw_event_table[tw_event_table$event_type == 'click',]
tw_count_click_event = as.data.frame(table(tw_click_event$ngClick))
names(tw_count_click_event) = c('event', 'freq')
tw_count_click_event = tw_count_click_event[order(-tw_count_click_event$freq),]
write.table(tw_count_click_event$event, file = 'tw_count_click_event', row.names = FALSE, fileEncoding = 'UTF-8')

this_week_date = c(20160403:20160410)

# 영상 분석 시작
tw_view_event = tw_event_table[tw_event_table$event_type == 'view',]
video_regex = grep('*video*', tw_view_event$page_url)
video_table = tw_view_event[video_regex,c('page_url', 'user_id')]

video_table = video_table[order(video_table$page_url),]

#영상 조회수 랭킹
video_view_rank = as.data.frame(table(video_table$page_url))[order(-as.data.frame(table(video_table$page_url))$Freq),]
names(video_view_rank) = c('name', 'freq')
top6 = video

tw_user = unique(tw_event_table$user_id)
lw_user = unique(lw_event_table$user_id)
retention_user = tw_user[tw_user %in% lw_user]

video_watched_by_retention = video_table[video_table$user_id %in% retention_user,]
video_data$date = sapply(video_data$created, function(x) extract_date_from_rawtable(x))

# 재방문 유저 중 영상을 시청한 유저
unique(video_watched_by_retention$user_id)

# 이번주 영상을 관람한 전체 유저
length(unique(video_table$user_id))

length(unique(video_watched_by_retention$user_id)) / length(unique(video_table$user_id))

# 전체 재방문 유저 중 영상 시청 유저 비중
length(unique(video_watched_by_retention$user_id)) / length(retention_user)


#이번주에 올라온 신규 영상
nrow(video_data[video_data$date %in% this_week_date, ])

