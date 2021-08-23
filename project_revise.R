require(tidyverse)
require(ggplot2)
require(ggthemes)
require(hrbrthemes)
require(magrittr)
theme_set(theme_ipsum())
names(the_counted_2015)[5]= "origin"
names(the_counted_2015)[9]= "adress"
names(the_counted_2015)[12]="grouping"
names(the_counted_2015)[13]="jurisdiction"
names(the_counted_2015)[1]= "id"
names(the_counted_2015)
sum(is.na(the_counted_2015$id))
sum(is.na(the_counted_2015$name))
sum(is.na(the_counted_2015$age))
sum(is.na(the_counted_2015$gender))
sum(is.na(the_counted_2015$origin))
sum(is.na(the_counted_2015$month))
sum(is.na(the_counted_2015$day))
sum(is.na(the_counted_2015$year))
sum(is.na(the_counted_2015$adress))
sum(is.na(the_counted_2015$city))
sum(is.na(the_counted_2015$state))
sum(is.na(the_counted_2015$grouping))
sum(is.na(the_counted_2015$jurisdiction))
sum(is.na(the_counted_2015$armed))
the_counted_2015 <- the_counted_2015  %>% filter(age != "Unknown") 
#
my_data_new=na.omit(the_counted_2015)
#
my_data_new %>% ggplot(aes(age))+
  geom_bar(fill = 'steelblue')+
  labs(x='AGE', Y='Value Count', title = 'Trend in Age')+
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(face = 'bold', size = 12, angle = 90),
        axis.text.y = element_text(face = 'bold', size = 12))+
  scale_fill_brewer(palette = 'set1')
#
my_data_new %>% ggplot(aes(origin))+
  geom_bar(fill = 'steelblue')+
  labs(x='AGE', Y='Value Count', title = 'Trend in Age')+
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(face = 'bold', size = 12, angle = -45),
        axis.text.y = element_text(face = 'bold', size = 12))+
  scale_fill_brewer(palette = 'set1')
#
my_data_new %>% ggplot(aes(state))+
  geom_bar(fill = 'steelblue')+
  labs(x='AGE', Y='Value Count', title = 'Trend in Age')+
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(face = 'bold', size = 12, angle = -45),
        axis.text.y = element_text(face = 'bold', size = 12))+
  scale_fill_brewer(palette = 'set1')
#
my_data_new %>% ggplot(aes(grouping))+
  geom_bar(fill = 'steelblue')+
  labs(x='AGE', Y='Value Count', title = 'Trend in Age')+
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(face = 'bold', size = 12, angle = -45),
        axis.text.y = element_text(face = 'bold', size = 12))+
  scale_fill_brewer(palette = 'set1')
#
my_data_new %>% ggplot(aes(origin))+
  geom_bar(fill = 'steelblue')+
  facet_wrap(~grouping)+
  labs(x='', Y='', title = '')+
  theme(axis.title = element_text(size = 12),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(face = 'bold', size = 12, angle = -45),
        axis.text.y = element_text(face = 'bold', size = 12))+
  scale_fill_brewer(palette = 'set1')
#
x=my_data_new %>%  
  mutate(age=as.numeric(age),
         month_int=recode(my_data_new$month, 
                          "January"=1, "February"=2, "March"=3, "April"=4, "May"=5,
                          "June"=6, "July"=7, "August"=8, "September"=9, "October"=10,
                          "November"=11, "December"=12),
         date = strptime(as.factor(paste(month_int, day, year, sep= "/")),format="%m/%d/%Y"),
         date_conv = as.Date(date, "%m/%d/%y"))
#
ggplot(x, aes(x=date_conv, y=age)) +
  geom_line(color="#69b3a2") + 
  xlab("")+
  scale_x_date(date_labels = "%b")+
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
#
qplot(x=date_conv, y=age,
      data=x, na.rm=TRUE,
      main=" ",
      xlab="Date", ylab=" ")
#
write.csv(x,"clean_data.csv", row.names = FALSE)
####################################

#to conclude that there are no outliers 
#judging by the lack of disproportionate 
#effect on the time series model, 
#called special causes which are associated 
#with abnormal events, 
#there are no sudden shifts in the trend 
#in the trend, the data exhibits a linear 
#trend which is used in modeling and 
#there data shows no clear patterns which 
#implies a random variation.
#The data depicts an additive seasonal change, 
#the data values tend to increase over time,
#but the magnitude of the seasonal change remains
#the same.
  
  
  
  
  
  
  
  
  
  
















