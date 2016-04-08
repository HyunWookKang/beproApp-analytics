library('RMySQL')
library('sqldf')

#DB 연결
con = dbConnect(MySQL(),
                user = 'root',
                password='qlvmfh0627',
                host='173.194.232.68')
#한글 인코딩
dbSendQuery(con, 'set names utf8')

#가입한 모든 유저 불러오기 (핸드폰 미인증 유저 제외)
rs = dbSendQuery(con, 'select * from bepro_api.user;')
data = fetch(rs, n = -1)

valid_user = data[!is.na(data$phone_verified_time),]

#가입한 모든 유저 불러오기 (핸드폰 미인증 유저 포함)
query_user = dbSendQuery(con, 'select * from bepro_api.user;')
user_data = fetch(query_user, n = -1)

#Match 데이터 불러오기
query_match = dbSendQuery(con, 'select * from bepro_api.match;')
match_data = fetch(query_match, n = -1)

#match_team_pk 데이터 불러오기
query_match_team = dbSendQuery(con, 'select * from bepro_api.match_team;')
match_team_data = fetch(query_match_team, n=-1)

#Team 데이터 불러오기
query_team = dbSendQuery(con, 'select * from bepro_api.team;')
team_data = fetch(query_team, n = -1)

#League 데이터 불러오기
query_league = dbSendQuery(con, 'select * from bepro_api.league;')
league_data = fetch(query_league, n = -1)

#Player 데이터 불러오기
query_player = dbSendQuery(con, 'select * from bepro_api.player;')
player_data = fetch(query_player, n = -1)

#Season_team 데이터 불러오기
query_season_team = dbSendQuery(con, 'select * from bepro_api.season_team;')
season_team_data = fetch(query_season_team, n = -1)

#Season 데이터 불러오기
query_season = dbSendQuery(con, 'select * from bepro_api.season;')
season_data = fetch(query_season, n = -1)
