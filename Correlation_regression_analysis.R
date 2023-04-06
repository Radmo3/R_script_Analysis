LED<-fread('/Users/kiku/Downloads/Life Expectancy Data.csv')
library(tidyverse)
library(dplyr)
library(GGally)
library(caTools)
library(MLmetrics)
library(carat)
LED <- LED %>% drop_na()
range(LED$`Life expectancy`)
LED_num <- LED %>% select(-Country, -Year, -Status)
LED1 <- sample.split(LED_num, SplitRatio=0.8)
LED1.1 <- subset(LED_num,split=TRUE) 
LED1.2 <- subset(LED_num,split=FALSE)

LED_numeric <- LED_num %>% select_if(is.numeric)
rcorr(as.matrix(LED_numeric))
ggcorr(LED_numeric, 
       label = T, 
       label_size = 2,
       label_round = 2,
       hjust = 1,
       size = 3, 
       color = "navyblue",
       layout.exp = 5,
       low = "cyan", 
       mid = "pink", 
       high = "yellow",
       name = "Correlation")

#Removing variables ~ Life Expectancy Variables
LED_num <- LED_num %>% 
  select(-'Population', -'Measles', -'infant deaths', 
         -'under-five deaths', -'Total expenditure', -'thinness  1-19 years', -'Income composition of resources', -'Schooling', -'thinness 5-9 years', -'Diphtheria')
LED_num <- LED_num %>% 
  select( -'thinness  1-19 years', -'Income composition of resources', -'Schooling', -'thinness 5-9 years', -'Diphtheria')

LED_num <- LED_num %>% 
  select(-'percentage expenditure', -'GDP', -'BMI')
LED_num <- LED_num %>% 
  select(-'Alcohol')
cor(LED_num, use = "complete.obs")

ggcorr(LED_num, 
       label = T, 
       label_size = 2,
       label_round = 2,
       hjust = 1,
       size = 3, 
       color = "navyblue",
       layout.exp = 5,
       low = "cyan", 
       mid = "pink", 
       high = "yellow",
       name = "Correlation")


LED1 <- sample.split(LED_num, SplitRatio=0.8)
LED1.1 <-subset(LED_num,split=TRUE)       
LED1.2 <-subset(LED_num,split=FALSE)

ggplot (data = LED_num, aes (x=`Life expectancy`,y=`Adult Mortality`)) + geom_point() +
  ggtitle('Regression: Life Expectancy vs Adult Mortality') +
  xlab('Life Expectancy') + geom_smooth(method="loess") + ylab('Adult Mortality')

ggplot (data = LED_num, aes (x=`Life expectancy`,y=`HIV/AIDS`)) + geom_point() +
  ggtitle('Regression: Life Expectancy vs HIV AIDS') +
  xlab('Life Expectancy') + geom_smooth(method="loess") + ylab('HIV AIDS')

LED_model1 <- lm(formula = `Life expectancy` ~ `Polio`, data= LED1.1)
summary(LED_model1)
LED_model2 <- lm(formula = `Life expectancy` ~ `Adult Mortality`, data= LED1.1)
LED_model3 <- lm(formula = `Life expectancy` ~ `Hepatitis B`, data= LED1.1)
LED_model4 <- lm(formula = `Life expectancy` ~ `HIV/AIDS`, data= LED1.1)

LED_predict <- predict(LED_model3, LED1.1)

LEpred <- predict(LED_mode3, newdata = LED1.1)
data.frame(Method = "RMSE", 
           Error.Value = RMSE(LEpred, LED1.2$`Life expectancy`))

ggplot (data = LED1.2, aes (x=`Life expectancy`,y=LEpred)) +
  geom_smooth(se=F, method="lm", colour = "gray35") + geom_point() +
  ggtitle('Linear Regression: Ground Truth vs Predicted') +
  xlab('Ground Truth') +
  ylab('Predicted')

