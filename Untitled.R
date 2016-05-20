head(tw_event_table)
head(session_table)
head(refined_table)
options(digits.secs=3)

test3 = completeFun(session_table, 'user_id')
test3$session = rep(NA, nrow(test3))
test3$session_time = rep(NA, nrow(test3))
test3$time = as.POSIXct(strptime(test3$time, '%Y-%m-%d %H:%M:%S'))

head(test3)

new_vector = test3$time[2:nrow(test3)]
new_vector = c(new_vector, as.POSIXct(strptime('1970-01-01 00:00:00', '%Y-%m-%d %H:%M:%S')))

test3 = cbind(test3, new_vector)
test3$time_diff = as.numeric(difftime(test3$time, test3$new_vector))
which(test3$time_diff > 1800)
test3[1:168,]

test$time = as.POSIXct(strptime(test$time, '%Y-%m-%d %H:%M:%S'))

help(strptime)
help(match)
help(which)
