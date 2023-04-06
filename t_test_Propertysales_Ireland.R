Ireland<-read.csv(file.choose("Ireland Property.csv"))
head(Ireland)
str(Ireland)
Ireland$Price.... <- gsub("," ,"",  Ireland$Price....)
Ireland$Price.... <- gsub("€" ,"",  Ireland$Price....)
Ireland$Price.... <- as.numeric(Ireland$Price....)

County<- table(Ireland$County)
County <- as.data.frame(County)
Dublin <- subset(Ireland, County == "Dublin")

#Provinces 
connacht <- Ireland %>% filter(Ireland$County %in% c("Galway", "Leitrim", "Mayo", "Roscommon", "Sligo"))
Ulster <- Ireland %>% filter(Ireland$County %in% c("Cavan", "Donegal", "Down", "Fremanagh", "Londonderry", "Monaghan", "Tyrone"))
Munster <- Ireland %>%  filter(Ireland$County %in% c("Cork", "Kerry", "Clare", "Limerick", "Tipperary", "Waterford"))
Leinster <- Ireland %>% filter(Ireland$County %in% c("Carlow", "Dublin", "Kildare", "Kilkenny", "Laois", "Longford", "Louth", "Meanth", "Offaly", "Westmeath", "Wexford", "Wicklow"))



#Safest places
Roscommon <- subset(Ireland, County=="Roscommon")
Mayo <- subset(Ireland, County == "Mayo")


#Expensive 
Wicklow <- subset(Ireland, County == "Wicklow")
Kildare <- subset(Ireland, County == "Kildare")
Meath <- subset(Ireland, County == "Meath")

#description
New <- subset(Ireland, Description.of.Property == "New Dwelling house /Apartment")
Second_hand <- subset(Ireland, Description.of.Property == "Second-Hand Dwelling house /Apartment" )


#Description of Data set
Price_County= Ireland %>% 
  select(Price...., County) %>%               
  group_by(County) %>%
  summarise(Count = n(),
            Avg_Price = mean(Price....), Median= median(Price....))

Price_dates= Ireland %>% 
  select(Price...., Date.of.Sale..dd.mm.yyyy.) %>%               
  group_by(Date.of.Sale..dd.mm.yyyy.) %>%
  summarise(Count = n(),
            Avg_Price = mean(Price....))

Price_desc= Ireland %>% 
  select(Price...., Description.of.Property ) %>%               
  group_by(Description.of.Property) %>%
  summarise(Count = n(),
            Avg_Price = mean(Price....))

ggplot(County,aes(x=Var1,y=Freq))+geom_bar(stat = "identity",color="purple",width = 0.7, fill="cyan")+coord_flip()+theme_classic()+labs(x="Counties",y="Frequency", title = "Counties")
ggplot(Price_County,aes(x=County,y=Avg_Price))+geom_bar(stat = "identity",color="Violet",width = 0.5, fill="violet")+coord_flip()+theme_classic()+labs(x= "Counties", y="Property Price", title= "Property prices in County")
ggplot(Price_dates,aes(x=Date.of.Sale..dd.mm.yyyy.,y= Count))+geom_bar(stat = "identity",color="skyBlue",width = 0.5, fill="skyblue")+coord_flip()+theme_classic()+labs(x= "Dates", y="Properties", title= "Property Sales each day in January")

ggplot(connacht, aes(x=County, y=Price....)) +   geom_boxplot(color="red", fill="purple", alpha=0.25)+labs(x="COUNTY", y="PRICE", title = "CONNACHT PROVINCE")

ggplot(Ulster, aes(x=County, y=Price....)) +   geom_boxplot(color="cyan", fill="magenta", alpha=0.25)+labs(x="COUNTY", y="PRICE", title = "ULSTER PROVINCE")
ggplot(Leinster, aes(x=County, y=Price....)) +   geom_boxplot(color="black", fill="blue", alpha=0.25)+labs(x="COUNTY", y="PRICE", title = "LIENSTER PROVINCE")
ggplot(Munster, aes(x=County, y=Price....)) +   geom_boxplot(color="navyblue", fill="brown", alpha=0.25)+labs(x="COUNTY", y="PRICE", title = "MUNSTER PROVINCE")

#hypothesis testing
#PROVINCES
mu_Populn <- mean(Ireland$Price....)
t.test(Leinster$Price...., mu= mu_Populn, alternative = "greater")
ggttest(t.test(Leinster$Price...., mu= mu_Populn, alternative = "greater"))
t.test(Ulster$Price...., mu=mu_Populn, alternative = "greater")
t.test(Munster$Price...., mu=mu_Populn, alternative = "greater")
t.test(connacht$Price...., mu=mu_Populn, alternative = "greater")

#Counties- Expensive
t.test(Wicklow$Price...., Dublin$Price...., var.equal = TRUE, alternative = "two.sided")
ggttest(t.test(Wicklow$Price...., Dublin$Price...., var.equal = TRUE, alternative="two.sided"))
t.test(Leinster$Price....,Wicklow$Price...., alternative="greater", var.equal = TRUE)

#Counties - Safest
t.test(Roscommon$Price...., Mayo$Price...., var.equal = TRUE)

#Types of housing Price comparison
t.test(New$Price...., Second_hand$Price....)

