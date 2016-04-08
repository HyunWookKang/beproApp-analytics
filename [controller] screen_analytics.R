library(dplyr)

head(ga.screen)
table = as.data.frame(table(ga.screen$dimension1))
nrow(table)
ga.screen %>%
  group_by(dimension1) %>%
  summarise(n = n())

head(ga.event.category)
table2 = as.data.frame(table(ga.screen$dimension1))
nrow(table2)
head(table2)
ga.week.active

valid_user[valid_user$name=='강현욱',]
valid_user[valid_user$id==5521,'name']

#문제가 있는 라인
ga.screen[ga.screen$dimension1 == 4594,]
ga.event.category[ga.event.category$dimension1 == 4594,]

#문제가 있는 라인
ga.screen[ga.screen$dimension1 == 1458,]
ga.event.category[ga.event.category$dimension1 == 1458,]

#문제가 있는 라인
ga.screen[ga.screen$dimension1 == 5203,]
ga.event.category[ga.event.category$dimension1 == 5203,]

ga.screen[ga.screen$dimension1 == 5342,]
ga.event.category[ga.event.category$dimension1 == 5342,]

ga.screen[ga.screen$dimension1 == 5521,]
ga.event.category[ga.event.category$dimension1 == 5521,]

ga.screen[ga.screen$dimension1 == 1405,]
ga.event.category[ga.event.category$dimension1 == 1405,]
