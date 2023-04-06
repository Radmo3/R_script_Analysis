BOOKS<-fread('/Users/kiku/Downloads/bestsellers with categories.csv')
print(any(is.na((BOOKS))))
library(vtable)
library(skimr)
library(psych)
library(gridExtra)
#STATISTICAL DESCRIPTION TABLES

summary(BOOKS)
vtable(BOOKS)
sumtable(BOOKS)
st(BOOKS,group = "Genre", group.test = TRUE)
Description<-stat.desc(BOOKS)
round(Description,2)
Psych<- describe(BOOKS)
SK<-skim_without_charts(BOOKS)

#GENRES 
GENRE_CO<-group_by(BOOKS, Genre) %>% 
  summarise(
    count = n())
ggbarplot(GENRE_CO, x = "Genre", y ="count", main="GENRES", color = "pink", fill = "navy blue")
#USER RATING 
UR<-ggplot(data=BOOKS,aes(x=`User Rating`))+
     geom_histogram(aes(y=(..count..)/sum(..count..)),fill="pink",col="blue",binwidth=0.05) + xlim(0,5)+
    scale_y_continuous(labels = percent)

BOOK1<-BOOKS %>%
  rename(User_Rating = `User Rating`) %>%
  select(Year, Name, Author, Genre, everything()) %>%
  arrange(Year) %>%
  mutate(Year = factor(Year), Genre = factor(Genre))

YEARS <- BOOK1 %>%
  group_by(Year) %>%
  summarise(avg_rating = mean(User_Rating),
            avg_review = mean(Reviews),
            avg_price = mean(Price))

#GGPLOT ON YEARS
V1<- YEARS %>%
  ggplot(aes(Year, avg_rating, group = 1)) +
  geom_line() +
  labs(title = "Average User Ratings by Year", y = "User rating",
       x = "YEARS") +
  theme_dark()

V2<-YEARS %>%
  ggplot(aes(Year, avg_review, group = 1)) +
  geom_line() +
  labs(title = "Average Reviews by Year", y = "Reviews",
       x = "Years") +
  theme_dark()

V3<-YEARS%>%
  ggplot(aes(Year, avg_price, group = 1)) +
  geom_line() +
  labs(title = "Average Prices by Year", y = "Prices",
       x = "Years") +
  theme_dark()
grid.arrange(V1, V2, V3, nrow = 3)

#PRICES FOR BOOKS
quantile(BOOKS$Price)
BOOKS %>%
  ggplot(aes(Price)) +
  geom_histogram(binwidth = 5, fill = "pink",
                 color="green") +
  theme_light() +
  labs(title = "Book Prices") +
  scale_x_continuous(breaks = seq(0, 110, by = 10))

#AUTHOR

PUBLISH=BOOKS%>%count(BOOKS$Author)%>%top_n(10)%>%head(10)
PUBLISH=PUBLISH%>%rename(Books=n,Author=`BOOKS$Author`)
PUBLISH=PUBLISH%>%arrange(Books)
View(PUBLISH)
summary(PUBLISH)

PUBLISH=data.frame(PUBLISH)
ggplot(data=PUBLISH,aes(x=Author,y=Books,fill=Books))+
  ggtitle("Top 10 Authors published maximum Books")+
  geom_bar(stat = "identity", color="magenta",fill="cyan")

#FICTION AUTHOR
FF<-BOOKS %>%
  filter(Genre == "Fiction") %>%
  group_by(Author) %>%
  summarize(num = n()) %>%
  arrange(-num) %>%
  slice_head(n=10) %>% 
  ggplot(aes(reorder(Author, num), factor(num))) +
  geom_point(size = 1) +
  theme_bw() +
  coord_flip() +
  labs(title = "The Top 10 Fiction Authors",
       x = "AUTHORS", y = "NUMBER OF BOOKS")

#NON FICTION
NF<-BOOKS %>%
  filter(Genre == "Non Fiction") %>%
  group_by(Author) %>%
  summarize(Books = n()) %>%
  arrange(-Books) %>%
  slice_head(n=10)

ggplot(NF, aes(x=Author, y=Books)) + geom_jitter(position=position_jitter(0.2), color="dark blue" )+
  coord_flip()+
  labs(title = "The Top 10 NON-Fiction Authors",
       x = "AUTHORS", y = "NUMBER OF BOOKS")
  
chisq.test(BOOKS$Genre, BOOKS$Year) 


#PRICE/REVIEWS/RATINGS per YEAR
GG1 <- BOOKS %>%
  group_by(Year, Genre) %>%
  summarise(meanPRICE = mean(Price)) %>%
  ggplot(aes(Year, meanPRICE, color = Genre, group = Genre)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_bw() +
  labs(title = "AVG BOOK PRICE PER YEAR",
       x = "YEARS", y = "AVG PRICE")
GG2 <- BOOKS %>%
  group_by(Year, Genre) %>%
  summarise(meanREVIEWS = mean(Reviews)) %>%
  ggplot(aes(Year, meanREVIEWS, color = Genre, group = Genre)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_bw() +
  labs(title = "AVG REVIEWS PER YEAR",
       x = "YEARS", y = "AVG REVIEW")
GG3 <-  BOOKS%>%
  group_by(Year, Genre) %>%
  summarise(meanRATINGS = mean(`User Rating`)) %>%
  ggplot(aes(Year, meanRATINGS, color = Genre, group = Genre)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_bw() +
  labs(title = "AVG RATINGS PER YEAR",
       x = "YEARS" , y = "AVG RATING")
  
grid.arrange(GG1, GG2, GG3, nrow = 3)
  
#RATING & YEAR
BKS=BOOKS%>%rename(User_rating=`User Rating`)
YEAR.rating=BOOKS%>%count(BOOKS$Year,BKS$User_rating)
YEAR.rating=YEAR.rating%>%
  rename(Year=`BOOKS$Year`,User_rating=`BKS$User_rating`,Total=n)
View(YEAR.rating)
#RATING AND PRICE
BKS$User_rating = as.character(BKS$User_rating)
ggplot(data=BKS,aes(x=User_rating,y=Price,fill=User_rating))+
  geom_boxplot()+ geom_abline()

#PRICE DISTRIBUTION
ggplot(data=BKS,aes(x=Price))+ geom_histogram(aes(y =..density..),
                                               colour = "violet", 
                                               fill = "cyan3",binwidth=4) +
  stat_function(fun=dnorm,args=list(mean=mean(BKS$Price),sd=sd(BKS$Price)))+
  xlim(-20,110)+ title(main = "PRICE DISTRIBUTION")

#GENRE AND PRICE

ggplot(data=BKS,aes(x=Genre,y=Price,fill=Genre))+geom_boxplot()

ggplot(data=BKS,aes(x=Genre,y=Price,fill=Genre))+geom_boxplot()+
  facet_grid(~Year)
  