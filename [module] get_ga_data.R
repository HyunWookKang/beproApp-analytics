library('RGA')
library('ggplot2')
#Get access token
authorize()

# Get a GA profiles
ga_profiles = list_profiles()
ga_profiles
# get GA report data
ga.user = get_ga(119252266,
                 start.date = '2016-02-25',
                 end.date = '2016-03-30',
                 metrics = 'ga:users',
                 dimensions = 'ga:date',
                 sort = 'ga:date'
                 )


ga.user$date = sapply(ga.user$date, function(x) substr(as.character(x), 6, 10))
ga.user

ggplot(data=ga.user, aes(x=date, y=users, group=1)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label=ga.user$users, size=15),
            check_overlap = TRUE,
            position=position_dodge(width=0.9), vjust=-1) +
  theme(text = element_text(size=14),
        axis.text.x = element_text(angle=90))
  
# get GA OS User data
ga.os = get_ga(117496006,
                 start.date = '2016-03-02',
                 end.date = '2016-03-27',
                 metrics = 'ga:users',
                 dimensions = 'ga:operatingSystem',
                 sort = 'ga:operatingSystem'
                 )
pct = round(ga.os$users / sum(ga.os$users) * 100)
lbls = paste(ga.os$operatingSystem, " ",pct, '%', sep="")
pie(pct, labels=lbls, col=c('#999999','#E69F00'), border = NA)

#get GA active user data
ga.week.active = get_ga(119252266,
                   start.date = '2016-03-02',
                   end.date = '2016-03-30',
                   metrics = 'ga:7dayUsers',
                   dimensions = 'ga:date',
                   sort = 'ga:date')
ga.month.active = get_ga(117496006,
                        start.date = '2016-03-02',
                        end.date = '2016-03-27',
                        metrics = 'ga:30dayUsers',
                        dimensions = 'ga:date',
                        sort = 'ga:date')
ga.active = merge(x=ga.week.active, y=ga.month.active, by='date', all.y=TRUE)
ga.active$date = sapply(ga.active$date, function(x) substr(as.character(x), 6, 10))

#get GA screen data
ga.screen = get_ga(117496006,
                   start.date = '2016-03-02',
                   end.date = '2016-03-30',
                   dimensions = 'ga:screenName, ga:dimension1',
                   metrics = 'ga:screenviews',
                   sort = '-ga:dimension1')

#get GA event category
ga.event.category = get_ga(117496006,
                           start.date = '2016-03-02',
                           end.date = '2016-03-30',
                           dimensions = 'ga:eventCategory, ga:dimension1, ga:eventAction, ga:eventLabel', 
                           metrics = 'ga:totalEvents',
                           sort = 'ga:dimension1')
