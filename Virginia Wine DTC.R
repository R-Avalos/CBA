#########################################
###    Wine and Virginia CBA          ##
#######################################

# Setup workspace
library("plm")
library("stargazer")
library("car")
library("lubridate")
library("ggplot2")
library("ggthemes")
library("directlabels")

### Load Data ####
Accidents <- read.csv("Accidents.csv", stringsAsFactors=FALSE) # Partial Panel for test
AccidentsPanel <- read.csv("Accidents2000_2005.csv", stringsAsFactors = FALSE) # Data from NHTSA, US Census, NIAA, Note DTC is DTC for VA only and does not include other state's with DTC 
VA <- read.csv("Virginia Price and Consumption.csv", stringsAsFactors = FALSE) # 2 year data set for demand function


### Transform Data
AccidentsPanel$Number.Killed.in.Alcohol.Related.Crashes <- as.numeric(gsub(",", "", AccidentsPanel$Number.Killed.in.Alcohol.Related.Crashes)) #convert to numeric but remove commas to avoid NAs
AccidentsPanel$Population..US.Census <- as.numeric(gsub(",", "", AccidentsPanel$Population..US.Census)) #convert to numeric but remove commas to avoid NAs
VA$Beer.Consumption.Million.Gallons <- VA$Beer.Per.Capita.Consumption.Gallons*VA$Population.Millions

# Subset to Brick & Mortar and to Online
Brick <- subset(VA, Seller=="Brick & Mortar")
Online <- subset(VA, Seller=="Online")
Year2002 <- subset(VA, Year==2002)
Year2004 <- subset(VA, Year==2004)

# Average prices by year
year1 <- ymd("2002/1/1")
year2 <- ymd("2004/1/1")
years <- c(year(year1), year(year2))
avg_price <- c(mean(Year2002[["Mean.Price"]]), 
               mean(Year2004[["Mean.Price"]]))
wine_percap <- c(mean(Year2002[["Wine.Per.Capita.Consumption.Gallons"]]), 
                        mean(Year2004[["Wine.Per.Capita.Consumption.Gallons"]]))
VAavg <- data.frame(years, avg_price, wine_percap) #setup dataframe for consumer surplus change

# Change in Qunatity (million gallons), 0.37-.34 = 0.03
# Change in Price = -$4.025
Price2004-Price2002
Price2002

VAavg$avg_price[1]

Positions <- data.frame(
        quantity = c(0, 0, VAavg$wine_percap[2], VAavg$wine_percap[1]),
        price = c(VAavg$avg_price[1], 
              VAavg$avg_price[2], 
              VAavg$avg_price[2], 
              VAavg$avg_price[1]),
        year = c(years[1], years[2], years[2], years[1])
        )


### Plot

plot_consumer_surplus <- ggplot(Positions, aes(x = quantity, y = price, color)) +
        geom_point(alpha = Positions$quantity) +
        geom_polygon(fill = "blue", alpha = 0.25) +
        scale_y_continuous(limits = c(0,40)) +
        scale_x_continuous(limits = c(0, 0.5)) +
        ylab("Mean Price per Gallon") +
        xlab("Per Capita Consumption of Wine (Gallons)") +
        ggtitle("Virginia Direct to Consumer Shipping \n Wine Price and Quantity 2002 and 2004") +
        theme_tufte()

plot_consumer_surplus <- plot_consumer_surplus + 
        annotate("text", x = 0.345, y = 28, label = "2002") +
        annotate("text", x = 0.375, y = 24, label = "2004")
plot_consumer_surplus
ggsave("plot_consumer_surplus.png", width = 8, height = 6)

### Models ####

# Change in consumer surplus
model1 <- lm(log(Wine.Consumption.Million.Liters) ~ log(Mean.Price), VA) 
summary(model1)


stargazer(model1, output = "html", out = "model1.htm")

# Change in Alcohol related deaths
#coplot(Alcohol.Related.Vehicle.Accident.Fatalities ~ Year | State, type="b", data = Accidents) # test panel
#scatterplot(Alcohol.Related.Vehicle.Accident.Fatalities ~ Year | State, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data = Accidents) # test panel

coplot(Number.Killed.in.Alcohol.Related.Crashes ~ Year | factor(State), type="b", data = AccidentsPanel) # full panel

scatterplot(Number.Killed.in.Alcohol.Related.Crashes ~ Year | State, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data = AccidentsPanel) #full panel



#testmodel <- plm(Alcohol.Related.Vehicle.Accident.Fatalities ~ Direct.Shipping.Lawful + Population.Millions + Wine.Per.Capita.Consumption.Gallons*Direct.Shipping.Lawful + Beer.Per.Capita.Consumption.Gallons + Spirits.Per.Capita.Consumption.Gallons, index=c("Year", "State"), model="within", data = Accidents)
#summary(panel)

#testpanelLog <-  plm(log(Alcohol.Related.Vehicle.Accident.Fatalities) ~ Direct.Shipping.Lawful + log(Population.Millions) + log(Wine.Per.Capita.Consumption.Gallons) + log(Beer.Per.Capita.Consumption.Gallons) + log(Spirits.Per.Capita.Consumption.Gallons), index=c("Year", "State"), model="within", data = Accidents)

panelm1 <-  plm(Number.Killed.in.Alcohol.Related.Crashes ~ Population..US.Census + Wine.Per.Capita + Beer.Per.Capita + Spirits.Per.Capita, index=c("Year", "State"), model="within", data = AccidentsPanel)
summaryM1 <- summary(panelm1)
summaryM1

panelm2 <-  plm(Number.Killed.in.Alcohol.Related.Crashes ~ Population..US.Census + Wine.Per.Capita + Beer.Per.Capita + Spirits.Per.Capita + I(Direct.Ship) , index=c("Year", "State"), model="within", data = AccidentsPanel)
summaryM2 <- summary(panelm2)
summaryM2



panelDirect <- plm(Wine.Per.Capita ~ I(Direct.Ship) + Population..US.Census + Wine.Per.Capita + Beer.Per.Capita + Spirits.Per.Capita, index=c("Year", "State"), model="within", data = AccidentsPanel)
summary(panelDirect)

stargazer(AccidentsPanel, output = "html", out = "accidentsummary.htm")
stargazer(panelm1, panelm2, output = "html", out = "summaryM1.htm")


panelLogDirect <- plm(log(Wine.Per.Capita) ~ I(Direct.Ship) + log(Population..US.Census) + log(Beer.Per.Capita) + log(Spirits.Per.Capita), index=c("Year", "State"), model="within", data = AccidentsPanel)
summary(panelLogDirect)



x <- (exp(0.468154)-1)*100
y <- (exp(-0.468154)-1)*100

#






panelLog <-  plm(log(Number.Killed.in.Alcohol.Related.Crashes) ~ log(Population..US.Census) + log(Wine.Per.Capita) + log(Beer.Per.Capita) + log(Spirits.Per.Capita), index=c("Year", "State"), model="within", data = AccidentsPanel)
summary(panelLog)

panelLog2 <-  plm(log(Number.Killed.in.Alcohol.Related.Crashes) ~ I(Direct.Ship) + log(Population..US.Census) + log(Beer.Per.Capita) + log(Spirits.Per.Capita), index=c("Year", "State"), model="within", data = AccidentsPanel)
summary(panelLog2)

panelIVlog <- plm(log(Number.Killed.in.Alcohol.Related.Crashes) ~ log(Population..US.Census) + log(Wine.Per.Capita) + log(Beer.Per.Capita) + log(Spirits.Per.Capita) | log(Wine.Per.Capita) + log(Population..US.Census) + log(Beer.Per.Capita) + log(Spirits.Per.Capita) + Direct.Ship, index=c("Year", "State"), model="within", data = AccidentsPanel)
summary(panelIVlog)
summary(AccidentsPanel)

panelIV <- plm(log(Number.Killed.in.Alcohol.Related.Crashes) ~ Population..US.Census + Wine.Per.Capita + + Beer.Per.Capita + Spirits.Per.Capita | Population..US.Census+ Wine.Per.Capita + Beer.Per.Capita + Spirits.Per.Capita + Direct.Ship, index=c("Year", "State"), model="within", data = AccidentsPanel)
summary(panelIV)




stargazer(panelLog, output = "html", out = "panel.htm")

z <- ggplot(data= AccidentsPanel, aes(x = Year, y = Number.Killed.in.Alcohol.Related.Crashes, group = State, color = State)) +
        geom_line()+
        theme_bw()
direct.label(z, top.points)

StateFacet <- ggplot(data= AccidentsPanel, aes(x = Year, y = Number.Killed.in.Alcohol.Related.Crashes, group = State)) +
        geom_line() +
        facet_wrap( ~ State) +
        theme_bw()
StateFacet

VAgraph <- subset(AccidentsPanel, State == "VA")
StateFacet + geom_line(data = VAgraph, color = "red")
