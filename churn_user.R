library(dplyr)

total_event_table$date = as.Date(total_event_table$time)
total_event_table$user_id = as.integer(total_event_table$user_id)
total_event_table = total_event_table %>%
  filter(!is.na(user_id)) %>%
  filter(user_id != 1)

backup = total_event_table[,2:3]
backup = total_event_table[!duplicated(backup),]

# 모든 세션 접속일자
include_all_visit = backup %>%
  mutate(churn_date = as.numeric(Sys.Date() - date)) %>%
  mutate(recent_visit = ifelse(lag(user_id, default = 1) - user_id == 0, 0, 1))

# 가장 최근 접속한 세션만 필터
only_unique_visit = include_all_visit %>%
  filter(recent_visit == 1) %>%
  mutate(is_churned = ifelse(churn_date <= 50, 0, 1))

sign_in_date = valid_user %>%
  select(phone_verified_time, id) %>%
  mutate(created_date = as.Date(phone_verified_time)) %>%
  select(id, created_date)

# 가입정보, 이탈 유저 여부, 세션 정보 통합 
join_signin_info = merge(include_all_visit, sign_in_date, by.x = 'user_id', by.y = 'id', all.x = TRUE) %>%
  filter(!is.na(created_date)) %>%
  mutate(from_created_date = as.integer(date - created_date)) %>%
  arrange(user_id, desc(date))

join_full_info = merge(join_signin_info, only_unique_visit, by = 'user_id', all.x = TRUE) %>%
  select(user_id, time.x, date.x, churn_date.x, recent_visit.x, created_date, from_created_date, is_churned)
names(join_full_info) = c('user_id', 'time', 'date', 'churn_date', 'recent_visit', 'created_date', 'from_created_date', 'is_churned')

join_full_info %>% filter(is_churned == 1)

filter(join_full_info, user_id == 7782)

player_league(7782)
