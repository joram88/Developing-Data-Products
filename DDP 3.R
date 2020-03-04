#Week 3 project for Coursera Developing Data Products
#By JRP

library(tidyverse)
library(plotly)

#Data Source
#https://www.kaggle.com/lewisduncan93/the-economic-freedom-index/data#economic_freedom_index2019_data.csv

url <- "https://raw.github.com/jrpineda/Developing-Data-Products/master/economic_freedom_index2019_data.csv"
efi <- read_csv(url)

efi2 <- efi %>% 
  select('2019 Score', 'GDP Growth Rate (%)', 'GDP per Capita (PPP)', 'WEBNAME', 'Region') %>% 
  rename(Score = '2019 Score', GDP_growth='GDP Growth Rate (%)',
         Country='WEBNAME', Region='Region', GDP_pc='GDP per Capita (PPP)') %>% 
  filter(Score!="N/A")
  
efi2$Score <- as.numeric(efi2$Score)
efi2$GDP_growth <- as.numeric(efi2$GDP_growth)
efi2$GDP_pc <- as.numeric(gsub("[\\$,]","",efi2$GDP_pc))

#Removing outliers (Venezuela, Cuba, North Korea)

efi2 <- efi2 %>% 
  filter(Score>35)

ggplot(efi2, aes(x = Score, y = GDP_growth, col=Region))+
  geom_point()

summary(fit <- lm(Score ~ GDP_growth, data = efi2))

ggplot(efi2, aes(x = Score, y = GDP_pc))+
  geom_point(aes(col = Region))+geom_smooth(method = "lm", se = F)

ggplot(efi2, aes(x = Score, y = GDP_pc))+
  geom_point(aes(col = Region))+geom_smooth(method = "lm", se = F)+
  facet_wrap(~Region)

summary(fit2 <- lm(GDP_pc ~ Score, data = efi2))

g <- plot_ly(x = efi2$Score) %>% 
  #add_markers(y = ~efi2$GDP_pc) %>% 
  add_lines(y = fitted(fit2)) %>% 
  layout(yaxis = list(range = c(0, 130000)))

  
fig <- plot_ly(data=efi2, x = ~efi2$Score, y = ~efi2$GDP_pc, color = ~Region, colors="Set2",
        text = ~paste("GDP per capita: $", GDP_pc, '<br>Country:', Country), mode="markers") 

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
  title = "GDP per capita (USD)",
  titlefont = f
)

fig %>% layout(title = "Countries of the World", xaxis = x, yaxis = y)


#ADD BUTTONS
#https://plot.ly/~chelsea_lyn/12871/_2016-summer-olympic-medal-count/#code 


ame <- as.vector(efi2$Region =="Americas")


  
