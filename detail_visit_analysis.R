# 이번 주 한 번 이상 방문한 유저들의 소속 리그
library('tidyr')

tw_visit_user = as.data.frame(table(tw_event_table$user_id))
names(tw_visit_user) = c('user_id', 'freq')

tw_visit_user$league = sapply(tw_visit_user$user_id, function(x) player_league(x))

delete_dup = function (x) {
  if (is.null(unlist(x))) {
    'NA'
  } else {
    unlist_list = unlist(x)
    unique(unlist_list[!duplicated(unlist_list)])
  }
}
tw_visit_user$league = sapply(tw_visit_user$league, function(x) delete_dup(x))

test = unnest(tw_visit_user, league)
test = as.data.frame(test)
league_count = as.data.frame(table(test$league))
names(league_count) = c('league', 'freq')
league_count = league_count[order(-league_count$freq),]
league_count

