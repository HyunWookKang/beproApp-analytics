library(bigrquery)
library(httr)

media_sql = 'select * from flatten(
  flatten(
    flatten(
      flatten(
        flatten(
          [event_log.video_log],
        match_event_ids), 
      team_ids), 
    match_ids), 
  user_ids), 
league_ids)'

r = GET('http://dev.bepro11-api.appspot.com/api/scripts/update-media-table?x_bepro_request=crowd')
status_code(r)

project = 'bepro11-api'
media_rawtable = query_exec(media_sql, project, max_pages = Inf)

readable_video_match = function(video_id) {
  match_ids = media_rawtable[media_rawtable$media_video_id == video_id, 'match_ids']
  sapply(match_ids, function(x) readable_match_id(x))
}
readable_video_league = function(video_id) {
  league_ids = media_rawtable[media_rawtable$media_video_id == video_id, 'league_ids']
  league_ids
}
readable_video_team = function(video_id) {
  team_ids = media_rawtable[media_rawtable$media_video_id == video_id, 'team_ids']
  sapply(team_ids, function(x) readable_team_id(team_ids))
}
