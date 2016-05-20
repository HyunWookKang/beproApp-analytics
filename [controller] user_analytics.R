library('dplyr')
library('ggplot2')

# MAU / WAU / DAU 계산

all_month_event_table$date = gsub('-', '', as.Date(all_month_event_table$time))

month_active_user = all_month_event_table[, c('user_id', 'date')]
month_active_user = month_active_user[!duplicated(month_active_user), ]
month_active_user = arrange(month_active_user, desc(date))

this_week_date = tw_date

month_active_user %>%
  summarise(MAU = n_distinct(user_id), MAU_ratio = n_distinct(user_id) / nrow(valid_user)) 

month_active_user %>%
  filter(date %in% this_week_date) %>%
  summarise(WAU = n_distinct(user_id), WAU_ration = n_distinct(user_id) / nrow(valid_user),
            Avg.DAU = mean())

month_active_user %>%
  filter(date %in% this_week_date) %>%
  group_by(date) %>%
  summarise(n = n()) %>%
  summarise(Avg.DAU = mean(n), Avg.ratio = mean(n)/nrow(valid_user),
            min.DAU = min(n), min.ratio = min(n)/nrow(valid_user), 
            max.DAU =max(n), max.ratio = max(n)/nrow(valid_user),
            user = nrow(valid_user))
  
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
user_data$created_day = gsub('-', '', as.Date(user_data$created))

#가입날짜별로 카운트
count_signup = user_data %>%
  select(id, created_day) %>%
  group_by(created_day) %>%
  summarise(n = n())
count_signup = as.data.frame(count_signup)
names(count_signup) = c('date', 'num')

# 월별 신규 가입자 그래프 그리기
this_month_date = all_month_date
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

# 세션 평균 지속 시간
test = refined_table
head(refined_table)
test$time = as.POSIXct(strptime(test$time, '%Y-%m-%d %H:%M:%S'))

avg_duration = vector(mode = 'numeric', length = 0)

avg_duration

test2 = test %>%
  group_by(user_id, session) %>%
  summarise(min_value = min(time), max_value = max(time))
test2 = as.data.frame(test2)
test2[1:30,]
test[test$user_id == 10044, c('time', 'session', 'session_time')]
test[test$user_id == 1005, c('time', 'session', 'session_time')]
