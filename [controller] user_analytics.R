library('dplyr')
library('ggplot2')

# MAU / WAU / DAU 계산
extract_date_from_rawtable = function(x) {
  y = strsplit(as.character(x), ' ')[[1]]
  z = strsplit(as.character(y), '-')[[1]]
  paste0(z[1], z[2], z[3])
}

all_month_event_table$date = sapply(all_month_event_table$time, function(x) extract_date_from_rawtable(x))
month_active_user = all_month_event_table[, c('user_id', 'date')]
month_active_user = month_active_user[!duplicated(month_active_user), ]
month_active_user = month_active_user[order(month_active_user$date),]

this_week_date = c(20160403:20160410)

MAU = nrow(as.data.frame(table(month_active_user$user_id))) / nrow(valid_user)
WAU = nrow((table(month_active_user[month_active_user$date %in% this_week_date,]))) / nrow(valid_user)
Avg.DAU = mean(as.data.frame(table(month_active_user[month_active_user$date %in% this_week_date,]$date))$Freq) / nrow(valid_user)
min.DAU = min(as.data.frame(table(month_active_user[month_active_user$date %in% this_week_date,]$date))$Freq) / nrow(valid_user)
max.DAU = max(as.data.frame(table(month_active_user[month_active_user$date %in% this_week_date,]$date))$Freq) / nrow(valid_user)
MAU
WAU
Avg.DAU
min.DAU
max.DAU


#DAU 변화 그래프 그리기
monthly_user = as.data.frame(table(month_active_user$date))
names(monthly_user) = c('date', 'num')
ggplot(data=monthly_user, aes(x=date, y=num, group=1)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label=monthly_user$num, size = 20),
            check_overlap = TRUE,
            position=position_dodge(width=0.9), 
            size = 8,
            vjust=-1) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90))


#가입 날짜만 잘라내기
extract_date_from_created = function(x) {
  y = strsplit(as.character(x), ' ')[[1]][1]
  z = strsplit(as.character(y), '-')[[1]]
  paste0(z[1], z[2], z[3])
}
user_data$created_day = sapply(user_data$created, function(x) extract_date_from_created(x))

#가입날짜별로 카운트
user_signup_day = user_data[, c('id','created_day')]
names(user_signup_day) = c('user_id', 'created_day')
count_signup = as.data.frame(table(user_signup_day$created_day))
names(count_signup) = c('date', 'num')

# 월별 신규 가입자 그래프 그리기
this_month_date = c(20160311:20160331, 20160401:20160410)
count_signup_this_month = count_signup[count_signup$date %in% this_month_date, ]
ggplot(data=count_signup_this_month, aes(x=date, y=num, group=1)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label=count_signup_this_month$num, size = 20),
            check_overlap = TRUE,
            position=position_dodge(width=0.9), 
            size = 8,
            vjust=-1) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90))

#Join Visited user + Sign up user
consolidate_table = merge(x = count_signup_this_month, y = monthly_user, by="date", all.y = TRUE)
names(consolidate_table) = c('date', 'signup', 'visit')

consolidate_table$rate = round((consolidate_table$signup / consolidate_table$visit) * 100, 2)
consolidate_table = consolidate_table[-c(1),]

# Active user 중 신규 가입자가 차지하는 비율 그래프 그리기
ggplot(data=consolidate_table, aes(x=date, y=rate, group=1)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label=paste(consolidate_table$rate,'%'), size = 15),
            check_overlap = TRUE,
            position=position_dodge(width=0.9),
            size = 8,
            vjust=-1) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90))

