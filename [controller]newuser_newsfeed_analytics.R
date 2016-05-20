# 소수점 이하 3자리까지 사용
options(digits.secs=3)

# Copy the original data, Change the time column from String to POSIX
temp_tw_event_table = tw_event_table
temp_user_data = user_data
temp_tw_event_table$time = as.POSIXct(temp_tw_event_table$time, tz='GMT')
temp_user_data$created = as.POSIXct(temp_user_data$created, tz='GMT')
temp_tw_event_table = temp_tw_event_table[order(temp_tw_event_table$user_id, temp_tw_event_table$time), ]
rownames(temp_tw_event_table) = 1:nrow(temp_tw_event_table)


# Get row indexes of "views at newsfeed page" and 10 followed actions
feed_row = strtoi(rownames(temp_tw_event_table[(temp_tw_event_table$page_url == '/feeds' 
                                                & temp_tw_event_table$event_type=='view'), ]))
for(i in 1:10){
  assign(paste("feed_row", i, sep=""), feed_row+i)
}
feed_row_combine = c(feed_row, feed_row1, feed_row2, feed_row3, feed_row4, feed_row5,
                     feed_row6, feed_row7, feed_row8, feed_row9, feed_row10)


# Include only 10 actions after "View at Newsfeed page" into feed_data
feed_data = temp_tw_event_table[feed_row_combine, ]
feed_data = feed_data[!duplicated(feed_data), ]
feed_data = feed_data[order(feed_data$user_id, feed_data$time), ]
feed_data = feed_data[!is.na(feed_data$user_id),] # exclude when user_id is NA


# Include actions right after "creating userID"
colnames(temp_user_data)[1] = 'user_id'
feed_data = merge(feed_data, temp_user_data[,c('user_id','created')], by='user_id')
feed_data = feed_data[,c(2,3,4,5,6,7,1,8)]
feed_data = cbind(feed_data, difftime(feed_data$time, feed_data$created, units="secs"))
colnames(feed_data)[9] = 'time_diff'
feed_data = feed_data[(feed_data$time_diff > 0 & feed_data$time_diff < 3600), ]
feed_data = feed_data[,1:7]

# Include actions which changed the page by click
# (except same action by same person in a short period)
# 1. Add next action to rightside
empty_row = data.frame("time"=NA, "ngClick"=NA, "page_url"=NA, "href"=NA, "class_name"=NA,
                       "event_type"=NA, "user_id"=NA)
feed_data_next = rbind(feed_data, empty_row)
feed_data_now = rbind(empty_row, feed_data)
feed_data_now$time = as.POSIXct(feed_data_now$time, origin = "1970-01-01", tz="GMT")
colnames(feed_data_next) = c("time2", "ngClick2", "page_url2", "href2", 
                             "class_name2", "event_type2", "user_id2")
feed_data_compare = cbind(feed_data_now, feed_data_next)
feed_data_compare = cbind(feed_data_compare,
                          event_time_dif = difftime(feed_data_compare$time2, feed_data_compare$time, units="secs"))

# 2. Exclude unknown users
is_na_user = is.na(feed_data_compare$user_id) & is.na(feed_data_compare$user_id2) # exclude unknown user
feed_data_compare = feed_data_compare[!is_na_user, ]

# 3. Exclude rows of user_change / rows of another session
is_same_user = (feed_data_compare$user_id == feed_data_compare$user_id2) %in% TRUE 
is_same_session = (feed_data_compare$event_time_dif < 300) %in% TRUE
feed_data_compare = feed_data_compare[(is_same_user | is_same_session), ]

# 4. Include only page_change or openModal_action 
is_page_differ = (feed_data_compare$page_url == '/feeds' & feed_data_compare$page_url2 != '/feeds') %in% TRUE
is_openModal = (grepl('openModal', feed_data_compare$ngClick) & feed_data_compare$page_url == '/feeds') %in% TRUE
feed_data_compare = feed_data_compare[(is_page_differ | is_openModal), ]

# 5. Exclude unknown actions
is_unknown_action = (feed_data_compare$page_url =='/feeds' & feed_data_compare$event_type == 'view')
feed_data_compare = feed_data_compare[!is_unknown_action, ]
feed_data_compare = feed_data_compare[,1:7]

# 6. Exclude similar actions by same person in same session (=10 minutes)
feed_filtered_data = data.frame()
for(i in 1:(nrow(feed_data_compare)-1)){
  row = feed_data_compare[i,]
  
  if(nrow(feed_filtered_data) == 0){
    feed_filtered_data = rbind(feed_filtered_data, row)
  }
  else if(grepl('openModal', row$ngClick) & row$page_url == '/feeds'){
    feed_filtered_data = rbind(feed_filtered_data, row)
  }
  else if((row$user_id != tail(feed_filtered_data, 1)$user_id) |
          (difftime(row$time, tail(feed_filtered_data, 1)$time, units="secs") > 600) |
          ((tail(feed_filtered_data, 1)$ngClick != row$ngClick) %in% TRUE &
           (tail(feed_filtered_data, 1)$class_name != row$class_name) %in% TRUE)){
    feed_filtered_data = rbind(feed_filtered_data, row)
  }
}


# Get each actions
is_default = grepl("default", feed_filtered_data$href) |
  (grepl("openModal", feed_filtered_data$ngClick) &
     (grepl("label-create", feed_filtered_data$class_name) |
        grepl("important-outline", feed_filtered_data$class_name) |
        grepl("", feed_filtered_data$class_name)))

is_highlight = grepl("highlight", feed_filtered_data$href)
is_lineup = grepl("lineup", feed_filtered_data$href)
is_media = grepl("media", feed_filtered_data$href)
is_feedleague = grepl("/leagues", feed_filtered_data$href) & !grepl("close", feed_filtered_data$ngClick)
is_feedteam = grepl("/teams", feed_filtered_data$href) & !grepl("close", feed_filtered_data$ngClick)
is_feeduser = grepl("/users", feed_filtered_data$href) & !grepl("close", feed_filtered_data$ngClick)
is_newsfeed = is_default | is_highlight | is_lineup | is_media | 
  is_feedleague | is_feedteam | is_feeduser 


is_leftmenu = grepl("closeLeftSideMenu()", feed_filtered_data$ngClick) |
  (grepl("openModal", feed_filtered_data$ngClick) & 
     (grepl("bepro.activated", feed_filtered_data$class_name) |
        grepl("label-btn", feed_filtered_data$class_name)))
is_rightmenu = grepl("closeRightSideMenu()", feed_filtered_data$ngClick)

is_remove = grepl("remove", feed_filtered_data$ngClick)
is_belowTap = grepl("root.", feed_filtered_data$ngClick)                
is_search = grepl("/search", feed_filtered_data$href) &
            !grepl("default", feed_filtered_data$href) &
            grepl("search-strong", feed_filtered_data$class_name)
is_others = is_remove | is_belowTap | is_search            

feed_default_click = feed_filtered_data[is_default, ]
feed_highlight_click = feed_filtered_data[is_highlight, ]
feed_lineup_click = feed_filtered_data[is_lineup, ]
feed_league_click = feed_filtered_data[is_feedleague, ]
feed_team_click = feed_filtered_data[is_feedteam, ]
feed_user_click = feed_filtered_data[is_feeduser, ]
feed_media_click = feed_filtered_data[is_media, ]

BelowTap_click = feed_filtered_data[is_belowTap, ]
search_click = feed_filtered_data[is_search, ]
feed_remove_click = feed_filtered_data[is_remove,]


all_feed_click = feed_filtered_data[is_newsfeed, ]
leftmenu_click = feed_filtered_data[is_leftmenu, ]
rightmenu_click = feed_filtered_data[is_rightmenu, ]
others_click = feed_filtered_data[is_others, ]
remained_data = feed_filtered_data[!is_newsfeed & !is_leftmenu & !is_rightmenu &
                                     !is_belowTap & !is_search,]


#filter_click = feed_filtered_data[grepl("Filter", feed_filtered_data$ngClick), ]



# Pie Chart for Newsfeed / LeftMenu / RightMenu / Others
table = c(nrow(all_feed_click), nrow(leftmenu_click), nrow(rightmenu_click), nrow(others_click))
names = c('Newsfeed Click', 'LeftMenu Click', 'RightMenu Click', 'Others')
colors = c("red", "yellow", "pink", "cyan")
pie(table, labels = names, col = colors, main="First Action at Newsfeed Page")

# Pie Chart for each news in newsfeed
newsfeed_detail = c(nrow(feed_default_click), nrow(feed_highlight_click), 
                    nrow(feed_lineup_click), nrow(feed_league_click), 
                    nrow(feed_team_click), nrow(feed_user_click), nrow(feed_media_click))
newsfeed_names = c('Default news', 'Match highlight', 'Lineup', 
                   'League name', 'Team name', 'User name', 'Media')
colors = c("red", "yellow", "pink", "cyan", "blue", "skyblue", "green")
pie(newsfeed_detail, labels = newsfeed_names, col = colors, main="Newsfeed elements detail")