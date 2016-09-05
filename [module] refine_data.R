source('[model] Rmysql.R')

#user_id 실명 변경 함수 (parameter = user_id Column Ex. ga.screen$dimension1)
#readable_user_id(data, col)
readable_user_id = function(user_id) {
  user_data[user_data$id==user_id, 'name']
}

#readable_match_id(match_id)
readable_match_id = function(match_id) {
  get_match_team_id = match_data[match_data$id==match_id, c('home_match_team_id', 'away_match_team_id')]
  get_team_id = c(match_team_data[match_team_data$id == get_match_team_id[1,1], 'team_id'], match_team_data[match_team_data$id == get_match_team_id[1,2], 'team_id'])
  get_team_name = c(team_data[team_data$id==get_team_id[1], 'name'], team_data[team_data$id==get_team_id[2], 'name']) 
  paste(get_team_name[1], 'vs', get_team_name[2])
}

#readable_team_id(team_id)
readable_team_id = function(team_id) {
  team_name = team_data[team_data$id==team_id, 'name']
  team_name
}

#readable_league_id(league_id)
readable_league_id = function(league_id) {
  league_name = league_data[league_data$id==league_id, 'name']
  league_name
}

#선수 소속팀
player_team = function(userId) {
  search_team_id = subset(player_data, select=team_id, subset=(user_id==userId))$team_id
  team_pk = search_team_id[!is.na(search_team_id)]
  team_name = sapply(team_pk, function (x) team_data[team_data$id == x , 'name'])
  team_name
}
# 팀 소속 리그
team_league = function(team_id) {
  season_id = season_team_data[season_team_data$team_id==team_id, 'season_id']
  league_id = sapply(season_id, function (x) season_data[season_data$id == x , 'league_id'])
  league_name = sapply(league_id, function(x) league_data[league_data$id == x, 'name'])
  league_name
}

# 선수 소속 리그
player_league = function(userId) {
  search_team_id = subset(player_data, select=team_id, subset=(user_id==userId))$team_id
  team_pk = search_team_id[!is.na(search_team_id)]
  season_id = season_team_data[season_team_data$team_id==team_pk, 'season_id']
  league_id = sapply(season_id, function (x) season_data[season_data$id == x , 'league_id'])
  league_name = sapply(league_id, function(x) league_data[league_data$id == x, 'name'])
  as.vector(league_name)
}
# readable_video_id
readable_video_id = function(video_id) {
  video_name = video_data[video_data$id==video_id, 'title']
  video_name
}


#Query table 만들기 (parameter = date)
#connect_query_table(date)
connect_query_table = function(date) {
  make_query_table = function(x) {
    paste('[event_log.compute_googleapis_com_event_log_', x, ']', sep="")
  }
  new_query_table = sapply(date, make_query_table)
  result = paste(new_query_table, collapse=',')
  result
}

#초기 Query문 만들기
project = 'bepro11-api'
sql = 'select structPayload.eventData.utcTime,
structPayload.eventData.ngClick,
structPayload.eventData.url,
structPayload.eventData.href,
structPayload.eventData.type,
structPayload.userId
from '

#dbClearResult(dbListResults(con)[[1]])
#리그 전체 가입선수 명단 확인 모듈
user_registered_in_league = function(league_name, season_id) {
  user_registered_query = paste('select * from bepro_api.player where is_active != 0 and player.team_id in (select team_id from bepro_api.season_team where season_id =', season_id, ');')
  user_registered_rs = dbSendQuery(con, user_registered_query)
  user_registered_data = fetch(user_registered_rs, n = -1)
  user_registered_data = user_registered_data[,c('user_id','team_id')]
  user_registered_data$user_id = sapply(user_registered_data$user_id, function(x) readable_user_id(x))
  user_registered_data$team_id = sapply(user_registered_data$team_id, function(x) readable_team_id(x))
  write.table(user_registered_data$user_id, file = paste(league_name, '유저'), row.names = FALSE, col.names = FALSE, fileEncoding = 'UTF-8')
  write.table(user_registered_data$team_id, file = paste(league_name, '팀'), row.names = FALSE, col.names = FALSE, fileEncoding = 'UTF-8')
}


# 중복 Row 판별식
dupsBetweenGroups <- function (df, idcol) {
  # df: the data frame
  # idcol: the column which identifies the group each row belongs to
  
  # Get the data columns to use for finding matches
  datacols <- setdiff(names(df), idcol)

  # Sort by idcol, then datacols. Save order so we can undo the sorting later.
  sortorder <- do.call(order, df)
  df <- df[sortorder,]
  
  # Find duplicates within each id group (first copy not marked)
  dupWithin <- duplicated(df)
  
  # With duplicates within each group filtered out, find duplicates between groups. 
  # Need to scan up and down with duplicated() because first copy is not marked.
  dupBetween = rep(NA, nrow(df))
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols])
  dupBetween[!dupWithin] <- duplicated(df[!dupWithin,datacols], fromLast=TRUE) | dupBetween[!dupWithin]
  
  # ============= Replace NA's with previous non-NA value ==============
  # This is why we sorted earlier - it was necessary to do this part efficiently
  
  # Get indexes of non-NA's
  goodIdx <- !is.na(dupBetween)
  
  # These are the non-NA values from x only
  # Add a leading NA for later use when we index into this vector
  goodVals <- c(NA, dupBetween[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  # The original vector, now with gaps filled
  dupBetween <- goodVals[fillIdx]
  
  # Undo the original sort
  dupBetween[sortorder] <- dupBetween
  
  # Return the vector of which entries are duplicated across groups
  return(dupBetween)
}