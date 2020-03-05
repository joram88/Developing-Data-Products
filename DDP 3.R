#Week 3 project for Coursera Developing Data Products
#By JRP

library(tidyverse)
library(plotly)
library(scales)

#Data Source
#https://www.kaggle.com/lewisduncan93/the-economic-freedom-index/data#economic_freedom_index2019_data.csv

url <- "https://raw.github.com/jrpineda/Developing-Data-Products/master/economic_freedom_index2019_data.csv"
efi <- read_csv(url)

efi2 <- efi %>% 
        select('2019 Score', 'GDP Growth Rate (%)', 'GDP per Capita (PPP)', 'WEBNAME', 'Region') %>% 
        rename(Score = '2019 Score', GDP_growth='GDP Growth Rate (%)',
               Country='WEBNAME', Region='Region', GDP_pc='GDP per Capita (PPP)') %>% 
        filter(Score!="N/A")

#We do a bit of cleaning with the variables we will use

efi2$Score <- as.numeric(efi2$Score)
efi2$GDP_growth <- as.numeric(efi2$GDP_growth)
efi2$GDP_pc <- as.numeric(gsub("[\\$,]","",efi2$GDP_pc))

#Removing outliers (Venezuela, Cuba, North Korea)

efi2 <- efi2 %>% 
        filter(Score>35)

#Create formats for later

f <- list(
        family = "Cambria, monospace",
        size = 18,
        color = "#7f7f7f"
)
x <- list(
        title = "Economic Freedom Index",
        titlefont = f
)
y <- list(
        title = "GDP growth (%)",
        titlefont = f
)
z <- list(
        title = "GDP per capita (USD)",
        titlefont = f
)

#First plot - Showing GDP per capita and Score

fig <- efi2 %>% 
        plot_ly(
                type = 'scatter',
                mode = 'markers',
                x = ~Score, 
                y = ~GDP_growth, 
                color = ~Region,
                text = ~paste("GDP per capita:", GDP_growth, '%<br>Country:', Country, '<br>Score:', Score)) 

fig %>% layout(title = "Countries of the World", xaxis = x, yaxis = y)

#No correlation. So how about GDP per capita and Score

summary(fit <- lm(Score ~ GDP_pc, data = efi2))

#Strong correlation shown here

ggplot(efi2, aes(x = Score, y = GDP_pc))+
        geom_point(aes(col = Region))+geom_smooth(method = "lm", se = F)+
        scale_y_continuous(label = dollar, limits = c(0,130000))+
        labs(title = "Countries of the World", y = "GDP per capita (USD)", x = "Economic Freedom Index")

#Correlation holds even when broken down by region

ggplot(efi2, aes(x = Score, y = GDP_pc))+
        geom_point(aes(col = Region))+geom_smooth(method = "lm", se = F)+
        facet_wrap(~Region)+
        scale_y_continuous(label = dollar)+
        labs(title = "Regions of the World", y = "GDP per capita (USD)", x = "Economic Freedom Index")

#Next plot it out

fig <- efi2 %>% 
        plot_ly(
                type = 'scatter',
                mode = 'markers',
                x = ~Score, 
                y = ~GDP_pc, 
                color = ~Region,
                text = ~paste("GDP per capita: $", GDP_pc, '<br>Country:', Country, '<br>Score:', Score)) 

fig %>% layout(title = "Countries of the World", xaxis = x, yaxis = z)

#Finally, we show each region separately to help us see individual cases

p <- efi2 %>%
        plot_ly(
                type = 'scatter', 
                x = ~Score, 
                y = ~GDP_pc,
                text = ~paste("GDP per capita: $", GDP_pc, '<br>Country:', Country, '<br>Score:', Score),
                hoverinfo = 'text',
                mode = 'markers', 
                transforms = list(
                        list(
                                type = 'filter',
                                target = ~Region,
                                operation = '=',
                                value = unique(efi2$Region)[1]
                        )
                )) %>% layout(
                        updatemenus = list(
                                list(
                                        type = 'dropdown',
                                        active = 0,
                                        buttons = list(
                                                list(method = "restyle",
                                                     args = list("transforms[0].value", unique(efi2$Region)[1]),
                                                     label = unique(efi2$Region)[1]),
                                                list(method = "restyle",
                                                     args = list("transforms[0].value", unique(efi2$Region)[2]),
                                                     label = unique(efi2$Region)[2]),
                                                list(method = "restyle",
                                                     args = list("transforms[0].value", unique(efi2$Region)[3]),
                                                     label = unique(efi2$Region)[3]),
                                                list(method = "restyle",
                                                     args = list("transforms[0].value", unique(efi2$Region)[4]),
                                                     label = unique(efi2$Region)[4]),
                                                list(method = "restyle",
                                                     args = list("transforms[0].value", unique(efi2$Region)[5]),
                                                     label = unique(efi2$Region)[5])
                                        )
                                )
                        )
                )
p %>% layout(title = "Regions of the World", xaxis = c(x,list(range = c(0,100))), 
             yaxis = c(z,list(range = c(0, 130000))))
