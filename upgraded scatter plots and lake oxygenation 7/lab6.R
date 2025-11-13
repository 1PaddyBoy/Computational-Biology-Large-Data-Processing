print(getwd())

library(dplyr)
library(ggplot2)
setwd("/media/patrick/250GB/school/comp bio/Lab 6 01/Data")
print("start sucess")
tempsData <- read.csv("global_temps.csv")
rr <- paste("R^2 = ", round(summary(lm(annual_temp_c ~ year, data = tempsData))$r.squared, 4), " | slope: ", round(unname(coef(lm(annual_temp_c ~ year, tempsData))[2]),5))
print(unname(coef(lm(annual_temp_c ~ year, tempsData))[1]))
print(unname(coef(lm(annual_temp_c ~ year, tempsData))[2]))
p <- ggplot(data = tempsData, aes(x= year, y = annual_temp_c))+
    geom_point()+
    geom_smooth(method = "lm", se = F,
        color = "red", linetype = "dashed")+
    theme_classic()+
    ylab("Annual Temp Celcius Change From Base")+
    xlab("Year")+
    annotate("text", x = max(tempsData$year) - (max(tempsData$year) - min(tempsData$year)) * 0.3, y = max(tempsData$annual_temp_c),label = rr, size = 7,color = "red")+
    ggtitle(label = "Global Annual Temperature Over Time From Mid Century Tempearture baseline.")    
#p = p + ggtitle(label = "Big Moose Lake Crustacean Density Over Time") + labs(caption = "Crustacean Density scatter plotted for Big Moose lake (mg ww / L) along the Y axis each year since 1994 along the X axis.\n The line of best fit for the data is plotted with a red dotted line \n and the fit % measureing adherence to the model in R^2 is displayed")
# save your plot using ggsave or "Export"
ggsave("Annual_Temp_C.jpeg", plot = p)

tempsData <- filter(tempsData, year >= 1950)
rr1 <- paste("R^2 = ", round(summary(lm(annual_temp_c ~ year, data = tempsData))$r.squared, 4), " | slope: ", round(unname(coef(lm(annual_temp_c ~ year, tempsData))[2]),5))
print(unname(coef(lm(annual_temp_c ~ year, tempsData))[1]))
print(unname(coef(lm(annual_temp_c ~ year, tempsData))[2]))
p1 <- ggplot(data = tempsData, aes(x= year, y = annual_temp_c))+
    geom_point()+
    geom_smooth(method = "lm", se = F,
        color = "red", linetype = "dashed")+
    theme_classic()+
    ylab("Annual Temp Celcius Change From Base")+
    xlab("Year")+
    annotate("text", x = max(tempsData$year) - (max(tempsData$year) - min(tempsData$year)) * 0.3, y = max(tempsData$annual_temp_c),label = rr1, size = 7,color = "red")+
    ggtitle(label = "Global Annual Temperature Over Time From Mid Century Tempearture Baseline since 1950.")    
#p = p + ggtitle(label = "Big Moose Lake Crustacean Density Over Time") + labs(caption = "Crustacean Density scatter plotted for Big Moose lake (mg ww / L) along the Y axis each year since 1994 along the X axis.\n The line of best fit for the data is plotted with a red dotted line \n and the fit % measureing adherence to the model in R^2 is displayed")
# save your plot using ggsave or "Export"
ggsave("Annual_Temp_C_Since_1950.jpeg", plot = p1)




COdata <- read.csv("combo.csv")
#tempsData <- COdata
rr2 <- paste("R^2 = ", round(summary(lm(annual_co2_ppm ~ year, data = COdata))$r.squared, 4), " | slope: ", round(unname(coef(lm(annual_co2_ppm ~ year, COdata))[2]),5))
print(unname(coef(lm(annual_co2_ppm ~ year, COdata))[1]))
print(unname(coef(lm(annual_co2_ppm ~ year, COdata))[2]))
p2 <- ggplot(data = COdata, aes(x= year, y = annual_co2_ppm))+
    geom_point()+
    geom_smooth(method = "lm", se = F,
        color = "red", linetype = "dashed")+
    theme_classic()+
    ylab("Atmostphereic CO2 Concentration (PPM)")+
    xlab("Year")+
    annotate("text", x = max(COdata$year) - (max(COdata$year) - min(COdata$year)) * 0.3, y = max(COdata$annual_co2_ppm),label = rr2, size = 7,color = "red")+
    ggtitle(label = "Global Atmostpheric CO2 Conncetration Over Time in PPM")    
#p = p + ggtitle(label = "Big Moose Lake Crustacean Density Over Time") + labs(caption = "Crustacean Density scatter plotted for Big Moose lake (mg ww / L) along the Y axis each year since 1994 along the X axis.\n The line of best fit for the data is plotted with a red dotted line \n and the fit % measureing adherence to the model in R^2 is displayed")
# save your plot using ggsave or "Export"
ggsave("CO2 over time.jpeg", plot = p2)


COdata <- read.csv("combo.csv")
#tempsData <- COdata
rr2 <- paste("R^2 = ", round(summary(lm(annual_temp_c ~ annual_co2_ppm, data = COdata))$r.squared, 4), " | slope: ", round(unname(coef(lm(annual_temp_c ~ annual_co2_ppm, COdata))[2]),5))
print(unname(coef(lm(annual_temp_c ~ annual_co2_ppm, COdata))[1]))
print(unname(coef(lm(annual_temp_c ~ annual_co2_ppm, COdata))[2]))
p2 <- ggplot(data = COdata, aes(x= annual_co2_ppm, y = annual_temp_c))+
    geom_point()+
    geom_smooth(method = "lm", se = F,
        color = "red", linetype = "dashed")+
    theme_classic()+
    ylab("annual_temp_c")+
    xlab("Atmostphereic CO2 Concentration (PPM)")+
    annotate("text", x = max(COdata$annual_co2_ppm) - (max(COdata$annual_co2_ppm) - min(COdata$annual_co2_ppm)) * 0.3, y = max(COdata$annual_temp_c),label = rr2, size = 7,color = "red")+
    ggtitle(label = "Atmostpheric CO2 ppm vs annual Temperature increase from 1952 to 2020.jpeg")    
#p = p + ggtitle(label = "Big Moose Lake Crustacean Density Over Time") + labs(caption = "Crustacean Density scatter plotted for Big Moose lake (mg ww / L) along the Y axis each year since 1994 along the X axis.\n The line of best fit for the data is plotted with a red dotted line \n and the fit % measureing adherence to the model in R^2 is displayed")
# save your plot using ggsave or "Export"
ggsave("CO2 vs annual Temperature increase from 1952 to 2020.jpeg", plot = p2)