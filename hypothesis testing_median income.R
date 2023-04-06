MI<-read.csv(file.choose("median_income_by_state.csv"))
MI

print(any(is.na(MI$Y2010)))
MI<- na.omit(MI)

summary(MI)
str(MI)


install.packages("usmap")
library(usmap)

class(MI$X2010)

#Remove commas to change to numeric ####
MI$X2010<-as.numeric(gsub(",", "", MI$X2010) )
MI$X2010


# Southern states map ####
plot_usmap(data = MI, values = "X2010", include = c( "Alabama","Delaware","D.C" , "Maryland", "West Virginia", "Virginia", "North Carolina", "Florida","South Carolina", "Georgia", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas"), labels = TRUE, color = "blue") + 
  scale_fill_continuous(low = "white", high = "blue", name = "Income Level", label = scales::comma) + 
  labs(title = "Income of Southern States") +
  theme(legend.position = "right")


#NORTHERN STATES MAP ####
plot_usmap(data = MI, values = "X2010", include = c( "Maine", "New Hampshire", "Massachussetts", "New Jersey", "Connecticut", "Iowa", "Indiana", "New York", "Wyoming", "Vermont","Nebraska", "North Dakota", "South Dakota", "Minnesota", "Wisconsin", "Michigan", "Montana", "Oregon","Washington", "Idaho", "Pennsylvania", "Rhode Island", "Illinois"), labels = TRUE, color = "blue") + 
  scale_fill_continuous(low = "gray", high = "pink", name = "Income Level", label = scales::comma) + 
  labs(title = "Income of Northern States") +
  theme(legend.position = "left")

#Plot
ggplot(MI1,aes(x=state,y=X2010))+geom_bar(stat = "identity",color="purple",width = 0.7, fill="cyan")+coord_flip()+theme_classic()+labs(x="SOUTH STATES",y="MEDIAN INCOME", title = "SOUTH STATES' INCOME")
ggplot(MI2,aes(x=state,y=X2010))+geom_bar(stat = "identity",color="cyan",width = 0.7, fill="violet")+coord_flip()+theme_classic()+labs(x= "NORTH STATES", y="MEDIAN INCOME", title= "NORTH STATES' INCOME")


#Remove Unites States and D.C as they are not states ####
MI<-MI %>% slice(-c(1,9))

install.packages("usmap")
library(usmap)

MI<-MI %>% 
  rename(
    state = State)

quantile(MI$X2010)
MI1 <- MI %>% filter(MI$state %in% c("Delaware", "Maryland", "West Virginia", "Virginia", "North Carolina", "Florida","South Carolina", "Georgia", "Alabama", "Kentucky", "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas"))
MI2<- MI %>% filter(MI$state %in% c("Maine", "New Hampshire", "Massachussetts", "New Jersey", "Connecticut", "Iowa", "Indiana", "New York", "Wyoming", "Vermont","Nebraska", "North Dakota", "South Dakota", "Minnesota", "Wisconsin", "Michigan", "Montana", "Oregon","Washington", "Idaho", "Pennsylvania", "Rhode Island", "Illinois"))
t.test(MI$X2010)
t.test(MI1$X2010)
t.test(MI2$X2010)
t.test(MI1$X2010, mu = 51712.59,
         alternative = "less", conf.level=.99)
MI_mean<-mean(MI$X2010)
US_Median <- 55520
mean(MI1$X2010)
mean(MI$X2010)
#Hypothesis ####
#A
t.test(MI$X2010, mu=US_Median)
#B
t.test(MI1$X2010, mu=US_Median)
#C
t.test(MI1$X2010, mu=US_Median, alternative = "less")
#D
t.test(MI1$X2010, mu=MI_mean, alternative = "less")
ggttest(t.test(MI1$X2010, mu=MI_mean, alternative = "less"))

#E
t.test(MI2$X2010, mu=US_Median, alternative = "greater")
#F
t.test(MI2$X2010, mu=MI_mean, alternative = "greater")
ggttest(t.test(MI2$X2010, mu=MI_mean, alternative = "greater"))
#G
t.test(MI1$X2010, MI2$X2010, mu=US_Median, alternative = "less")
#H Two sided
(t.test(MI1$X2010, alternative= "two.sided"))
t.test(MI1$X2010, MI2$X2010, mu=MI_mean, alternative = "two.sided")
ggttest(t.test(MI1$X2010, MI2$X2010, mu=MI_mean, alternative = "two.sided"))
