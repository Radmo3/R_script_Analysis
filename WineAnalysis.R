wine<-fread('/Users/kiku/Downloads/wine_tasting.csv')
wine
library(data.table)
library(dplyr)
library(gsubfn)
library(ggplot2)
library(stringr)
library(magrittr)
install.packages("ggridges")
library(ggridges)
library("ggpubr")
install.packages("ngram")
library(ngram)
library(readr)
library(broom)
library(dplyr)
library(tidyr)
library(naniar)
library(ngram)
library(stringr)
library(ggplot2)
library(purrr)
library(ggrepel)
str(wine)
summary(wine)
colSums(is.na(wine))
wine<-na.omit(wine)
wine <- wine%>% rename(Ratings=points)
wine = subset(wine, select = -c(region_2) )
wine[861,"country"]<- "International"
#COUNTRY
PT<-table(wine$country)
PT<-as.data.frame(PT)
treemap(PT,
        index=c("Var1"),
        vSize="Freq",
        title="Countries",
        type="index",
        palette = "Set3",
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")))

colSums(is.na(wine))
rowSums(is.na(wine))
quantile(wine$points)
quantile(wine$price)
#POINTS
POINT_table <- table(wine$Ratings)
barplot(POINT_table, main= "WINE POINTS", xlab = "Ratings", ylab="DENSITY", col = "blue", ylim = c(0,211))
#VARIETY
VARIETY_OF_WINE<-table(wine$variety)
VARIETY_OF_WINE<-as.data.frame(VARIETY_OF_WINE)
str(VARIETY_OF_WINE)
summary(VARIETY_OF_WINE)
install.packages("treemap")
library(treemap)
p <- treemap(VARIETY_OF_WINE,
             index=c("Var1"),
             vSize="Freq",
             title="TREE MAP OF VARIETY OF WINES",
             type="index",
             palette = "Set2",
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")))
VARWINE<-filter(VARIETY_OF_WINE, Freq>20)
ggplot(data=VARWINE, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="maroon")+
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  theme(axis.text.x = element_text(angle = 90))+
  labs(y="Production", x="Popular Variety of Wine")                     
#plot of variety of wines best
#PROVINCE
PROVINCe<- table(wine$province)
PROVINCe<-as.data.frame(PROVINCe)
quantile(PROVINCe$Freq)
PROVINCE<-filter(PROVINCe, Freq>9)
q <- treemap(PROVINCE,
             index=c("Var1"),
             vSize="Freq",
             title="TREE MAP OF POPULAR PROVINCES (HIGHER PRODUCTION)",
             type="index",
             palette = "Set3",
             align.labels=list(
               c("center", "center"), 
               c("right", "bottom")))
P10<- filter(PROVINCE, Freq>23)
ggplot(P10, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", color= "pink", fill="sky blue" ) +
   labs(x="PROVINCE", y="PRODUCTION") #highest production in province 
#DESIGNATION
DESIG<-table(wine$designation)
DESIG<-as.data.frame(DESIG)
View(DESIG)
DESIGNATION<- DESIG %>% filter(Freq>3, Freq<230)
ggplot(DESIGNATION, aes(x=Var1, y=Freq)) + geom_bar(stat="identity", color= "red", fill="light yellow" ) +
      labs(x="DESIGNATION", y="FREQUENCY")
DESIG<-filter(DESIG,Freq>2,Freq<230)

treemap(DESIG,
        index=c("Var1"),
        vSize="Freq",
        title="Popular Designations",
        type="index",
        palette = "Set1",
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")))
#WINERIES
WINERIES<-table(wine$winery)
WINERIES<- as.data.frame(WINERIES)

PRiCE= wine %>% 
  select(price, country) %>%               
  group_by(country) %>%
  summarise(Count = n(),
            Avg_Price = mean(price))

ggplot(wine, aes(x=price, y=Ratings)) +   geom_boxplot(color="blue", fill="purple", alpha=0.2)
POPR <- wine %>% select(Ratings, price)
ggplot(POPR, aes(x=price, y=Ratings)) +   geom_boxplot(color="blue", fill="purple", alpha=0.2)
plot(POPR, col="purple", ylab = "PRICE", xlab = "Ratings", main = "PRICE~Ratings Scatter Plot", ylim=c(0,800))
mean(POPR$Ratings)
mean(POPR$price)
PO_DS <- POPR %>% filter(Ratings >= 90)
PO_DS$Ratings = as.character(PO_DS$Ratings) 
ggplot(PO_DS, 
       aes(y = Ratings,
           x = price,
           fill= Ratings,
           color=Ratings)) +
  xlim(0, 800) +
  geom_density_ridges() + 
  theme_minimal() + 
  theme(legend.position = "none")
ggqqplot(POPR$price, ylab = "price")
ggqqplot(POPR$Ratings, ylab = "Ratings")
library("ggpubr")
library("ggpubr")
ggscatter(POPR, x = "Ratings", y = "price" , 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "RATINGS", ylab = "PRICE", color = "maroon")
#Pearson correlation test
cor.test(POPR$price, POPR$Ratings,  method = "spearman")  
cor.test(POPR$price, POPR$Ratings, method = "pearson")
shapiro.test(POPR$price)
shapiro.test(POPR$Ratings)
table(POPR)     
hist(POPR)
str(POPR)
summary(wine)
wine$Category<-ifelse(wine$Ratings>=80 & wine$Ratings<85,"A.80-85",ifelse(wine$Ratings>=85 & wine$Ratings<90,"B.85-90",ifelse(wine$Ratings>=90 & wine$Ratings<=95,"C.90-95","D.95-100")))
gg1<-wine %>% group_by(taster_name) %>% summarise(Ratings=mean(Ratings),Count=n(),price)
ggplot(wine, aes(x=Category, y=price)) +   geom_boxplot(color="blue", fill="purple", alpha=0.25, abline(h = mean(wine$price), col = "#7127D1",     lwd = 1))

CS<-crosstable(gg1)

boxplot(wine$price ~ wine$Category,
        ylab = "Price",
        xlab = "Category",
        las = 1,
        col = brewer.pal(4,"Set2"),
        notch=FALSE,
        boxwex = 0.8,
        main="PRICE PER CATEGORY",
        ylim=c(0,800))

#hypothesis 
#remove duplicate descriptions
wine<- wine[!duplicated(wine$description,)]
wine_chard <- subset(wine, country=='Chile' & variety == "Chardonnay")
wine_pinot <- subset(wine, country == "US" & variety == "Pinot Noir")
wine_combined <- rbind(wine_chard ,wine_pinot)
mu <- ddply(wine_combined, "variety", summarise, mean=mean(Ratings), sd=sd(Ratings),variance= var(Ratings) )
wine_cabarnet <- subset(wine,  variety == "Cabernet Sauvignon")
wine_Char <- subset(wine, variety=="Chardonnay")
ggplot(wine_combined, aes(x=Ratings, fill=variety, color=variety)) +
  geom_histogram(position="identity")
wine_comb <- rbind(wine_Char, wine_cabarnet)
ggplot(wine_comb, aes(x-Ratings, fill=variety, color= variety))+
  geom_histogram(position= "identity")
wine_price= wine %>% 
  select(price, variety) %>%               
  group_by(variety) %>%
  summarise(count=n(),
            Avg_Price = mean(price), var= var(price))

wine_ratings= wine %>% 
  select(Ratings, variety) %>%               
  group_by(variety) %>%
  summarise(count=n(),
            Avg_ratings = mean(Ratings), var= var(Ratings))


ggplot(wine_comb, aes(x-Ratings, fill=variety, color= variety))+
  geom_histogram(position= "identity")
ggplot(wine_cabarnet, aes(x=price, fill = variety, color = variety))+
  geom_histogram(position = "identity")
ggplot(wine_Char, aes(x=price, fill=variety, color=variety))+
  geom_histogram(position="identity")
#hypothesis between chardonay and cabarnet wines price is equal 
t.test(wine_Char$price, wine_cabarnet$price)
ggttest(t.test(wine_Char$price, wine_cabarnet$price))
mean(wine$price)
#hypo to check if pinot has mean of price equal to mean of price for the populaton
t.test(wine_pinot$price, mu=37.35207)
ggttest(t.test(wine_pinot$price, mu=37.35207))
#H0 bord price mean is less than mean of population 
t.test(wine_bord$price, mu=37.35207, alternative = "greater")
# u test char price and cabarnet price
wilcox.test(wine_Char$price, wine_cabarnet$price)
#hypo that correlation between chard and cabarnet's price 
cor.test(wine_Char$price, wine_cabarnet$price)



#description 
wine$fruity_wine <- str_detect(wine$description, "fruit|fruity")
w1 <- ggplot(wine, aes(fruity_wine, Ratings, fill = fruity_wine))+
  geom_boxplot()
w1

wine$smokey_wine <- str_detect(wine$description, "smoke")
w2 <- ggplot(wine, aes(smokey_wine, Ratings, fill= smokey_wine ))+
  geom_boxplot()
w2
wine$bad_taste <- str_detect(wine$description, "bland| pickled| raspy| forced| diluted| weedy| dilute| quaffer")
w3 <- ggplot(wine, aes(bad_taste, Ratings, fill= bad_taste ))+
  geom_boxplot()
w3 

GLO <- c("fruity_wine", "smokey_wine", "bad_taste")
wine <- wine %>% 
  mutate_at(GLO, funs(ifelse(. == TRUE,1,0))) %>% 
  mutate_at(GLO, as.factor)

#regression 
# new version 
wine_V1 <-
  wine%>%
  select(-V1, 
         -taster_twitter_handle,
         -designation) %>%
  na.omit()

wine_V1$words <- sapply(wine_V1$description, wordcount)

wine_V2 <- wine_V1
wine_V2$description <- str_replace_all(wine_V2$description, "-", " ")
wine_V2$word_count <- sapply(wine_V2$description, wordcount)

#Word count by taster name 
ggplot(wine_V2, aes(x = taster_name,
                    y = word_count)) +
  geom_boxplot(color="maroon") +
  coord_flip() +
  labs(title = "Word Count distribution by Taster",
       x = "Taster's Name",
       y = "Words")


ggplot(wine_V2, aes(x = Ratings,
                      y = word_count)) +
  geom_point(position = "jitter", alpha = 0.7) +
  geom_smooth(method = "lm") +
  labs(title = "`Description Word Count by Wine Rating",
       x = "Rating",
       y = "Words",
       caption = "Points score is 80 - 100")

#regression model 1
model_wc<- lm(words ~ Ratings, data = wine_V2)
summary(model_wc)


ggplot(wine_V2, aes(x = Ratings,
                    y = word_count)) +
  geom_point(position = "jitter", alpha = 0.7) +
  geom_smooth(method = "lm", se=FALSE) +
  labs(title = "Description Word Count by Wine Rating",
       x = "Rating",
       y = "Words")+ facet_wrap(~taster_name)

#hypothesis testing
#H0 = Mean return is equal to 0
t.test(wine_V2$Ratings,wine_V2$word_count, var.equal = TRUE)
ggttest(t.test(wine_V2$Ratings,wine_V2$word_count, var.equal = TRUE))


table(wine_V2$taster_name)
tn <- table(wine_V2$taster_name)
tn <- as.data.frame(tn)
quantile(tn$freq)
#removing viewers less than 25 reviews
wine_V3 <-
 wine_V2 %>%
  filter(taster_name != "Jeff Jenssen",
         taster_name != "Mike DeSimone",
         taster_name != "Alexander Peartree",
         taster_name != "Susan Kostrzewa",
         taster_name != "Lauren Buzzeo",
         taster_name != "Anne Krebiehl¬†MW")

#removing viewers less than 25 reviews
Reviewers <- wine_V2 %>%  filter(taster_name,Freq>40)

#create groups for tasters & nest the remaining data

Wine_nested <- wine_V3 %>%
  group_by(taster_name) %>%
  nest()

#separate regression models 

model1 <-
  Wine_nested %>%
  mutate(lm_mod = map(data, 
                      ~lm(formula = word_count ~ Ratings, 
                          data = .x)))


#create coefficient column
model1 <-
  model1 %>%
  mutate(coeffs = map(lm_mod, tidy),
         values = map(lm_mod, glance))

# unnest coeffs column and create dataset of reviewer and estimate

taster <-
  model1 %>%
  select(taster_name, coeffs) %>%
  unnest()

taster <-
  taster %>%
  select(taster_name, estimate)

#unnest values and create dataset of tasters and R-squared value

taster_v1<-
  model1 %>%
  select(taster_name, values) %>%
  unnest() %>%
  select(taster_name, r.squared) %>%
  rename(r_squared = r.squared)

taster_v <-
  taster %>%
  left_join(taster_v1)

ggplot(taster_v, aes(x = estimate,
                      y = r_squared)) +
  geom_point() +
  geom_text_repel(label = taster_v$taster_name) +
  labs(title = "Effect of Rating \non Description Word Count",
       x = "Word increase per additional Rating \n(regression gradient coefficient)",
       y = "Amount of variability explained by ratings \n(R-squared)")


#Regression model 2
#Is price dependent on ratings 
model_us <- lm(price~Ratings, data = US_wines)
summary(model_us)
model_char<- lm(price~Ratings, data = wine_Char)
summary(model_char)
ggplot(wine_Char, aes(x = Ratings,
                      y = price)) +
  geom_point(position = "jitter", alpha = 0.5) +
  geom_smooth(method = "lm", se=FALSE) +
  labs(title = "Wine Rating by Price",
       x = "Rating",
       y = "Price")



model_rat <- lm(Ratings~price, data= wine_V2)
summary(model_rat)
anova(model_rat)
avPlots(model_rat)
#add country 
C1 <- wine_V2$country
model_3 <- Ratings~price + C1
model_3.1 <- lm(model_3, data=wine_V2)
summary(model_3.1)
avPlots(model_3.1)

Country_nested <- wine_V3 %>%
  group_by(country) %>%
  nest()

model2 <-
  Country_nested %>%
  mutate(lm_mod = map(data, 
                      ~lm(formula = Ratings ~ price, 
                          data = .x)))

model2 <-
  model2 %>%
  mutate(coeffs = map(lm_mod, tidy),
         values = map(lm_mod, glance))
#unnest
country1 <-
  model2 %>%
  select(country, coeffs) %>%
  unnest()

country1 <-
  country1 %>%
  select(country, estimate)
country1.1<-
  model2 %>%
  select(country, values) %>%
  unnest() %>%
  select(country, r.squared) %>%
  rename(r_squared = r.squared)

country_v <-
  country1 %>%
  left_join(country1.1)
na.omit(country_v)


ggplot(country_v, aes(x = estimate,
                     y = r_squared)) +
  geom_point() +
  geom_text_repel(label = country_v$country) +
  labs(title = "Effect of Rating \non Country",
       x = "country Rating \n(regression gradient coefficient)",
       y = "Amount of variability explained by ratings \n(R-squared)")

