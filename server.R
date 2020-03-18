function(input, output, session) {

    output$graph <- renderPlot({
        
        library(readxl)
        library(tidyverse)
        library(shiny)
        library(scales)
        library(zoo)
        library(lubridate)
        library(ggthemes)
        library(shinyjs)
        library(directlabels)
        
        url <- "https://www.census.gov/foreign-trade/statistics/state/metrohistory.xlsx"
        
        p1f <- tempfile()
        download.file(url, p1f, mode="wb")
        metro18<-read_excel(path = p1f, sheet="2018", 
                            range="2018!A4:G51")
        metro17 <- read_excel(path = p1f, sheet="2017",
                              range="2017!A4:G51")
        metro16 <- read_excel(path = p1f, sheet="2016",
                              range="2016!A4:G51")
        
        #Clean up and arranging into several structured data frames
        
        names(metro18)[1] <- "metro"; metro18 <- metro18[,1:5];
        names(metro17)[1] <- "metro"; metro17 <- metro17[,1:5];
        names(metro16)[1] <- "metro"; metro16 <- metro16[,1:5];
        
        metro <- merge(x = metro18, y = metro17, by = "metro", all = TRUE)
        metro <- merge(x = metro, y = metro16, by = "metro", all = TRUE)
        
        region <- c("Aggregate","Northeast", "South", "Northeast", "Northeast", "Northeast",
                    "South", "Midwest", "South", "Midwest", "Midwest",
                    "South", "Midwest", "South", "South", "Northeast",
                    "South", "Midwest", "Midwest", "West", "South",
                    "Midwest", "Midwest", "South", "Northeast", "Aggregate","Midwest",
                    "Northeast", "West", "Northeast", "West", "Northeast",
                    "West", "Northeast", "West", "West", "West",
                    "West", "West", "West", "Aggregate", "Aggregate", "Aggregate", 
                    "Aggregate", "Midwest", "Aggregate", "South")

        metro$Region <- region


        metro2 <- metro %>% 
            gather(quarter,exports,-metro, -Region)
        
        metro3 <- metro2 %>% 
            mutate(day=1) %>% 
            separate(quarter, c("Year","Quarter"), sep = " ") %>% 
            mutate(month=ifelse(Quarter=="Q1", 2, 
                                ifelse(Quarter=="Q2", 5,
                                       ifelse(Quarter=="Q3", 8,
                                              10)))) %>% 
            mutate(date=(paste(Year,month,day, sep="-"))) %>% 
            mutate(date=as_date(date)) %>% 
            mutate(date2=as.yearqtr(date, format = "%Y-%m-%d"))
        
        abv <- c("MET", "ALL", "ATL", "BOS", "BRD", "BUF", "CHA", "CHI",
                 "CIN", "CLE", "COL", "DAL", "DET", "PAS", "GRN", "HAR",
                 "HOU", "IND", "KAN", "LAS", "MIA", "MIL", "MIN", "NOL",
                 "NY", "OTH", "PEO", "PHI", "PHX", "PIT", "POR", "PRO",
                 "RIV", "ROC", "SAC", "SLC", "SD", "SF", "SJO", "SEA","MID",
                 "NE", "SO", "WST", "STL", "TOT", "WAS")
        
        metro3$abv <- abv
        
        metroshiny <- metro3 %>% 
            filter(metro %in% input$variable)
        
        ggplot(metroshiny, aes(x = date2, y = exports, color=Region, group=metro))+
            geom_line()+geom_point(size=3)+
            scale_x_yearqtr(name = "Quarter",
                            limits = c(min(metro3$date2), max(metro3$date2)),
                            format = "%YQ%q")+
            scale_y_continuous(name="Exports in USD (millions)", labels = dollar)+
            labs(title = "Metropolitan Exports", caption = "Source: US Census Bureau")+
            theme_economist()+
            geom_dl(aes(label = abv), method = list("last.points"), cex = 1.2)


        })
    observeEvent(input$reset_input, {
        shinyjs::reset("side-panel")
    })

}