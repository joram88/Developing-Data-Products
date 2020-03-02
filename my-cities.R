#Places I have lived and visited
#BY Jose Ramon Pineda

library(leaflet)
library(dplyr)
library(lubridate)

#Today's date is:
(today())


cities <- data.frame(name = c("Veracruz, VER", "Alexandria, VA", "Syracuse, NY", "Sendai, Tohoku Prefecture", "London, UK","Madrid, Spain",
                              "California", "Pennsylvania", "Alaska", "Colorado", "Texas",
                              "Nevada", "Illinois", "Georgia", "Florida", "South Carolina",
                              "North Carolina", "Tennessee", "West Virginia", "Delaware",
                              "Maryland", "DC", "Ohio", "Connecticut", "Rhode Island", 
                              "Massachusetts",
                              "Canada", "Ireland", "France", "Germany", "Denmark",
                              "Sweden", "Finland", "Russia", "Czechia", "Italy",
                              "Switzerland", "Greece", "Belgium", "Netherlands", "Luxembourg",
                              "Egypt", "Ethiopia", "China", "South Korea",
                              "Tamaulipas", "CDMX","Guanajuato","Puebla",
                              "Oaxaca", "Chiapas","Yucatan","QR"),
                     lat=c(19.200331,38.804836,43.047939, 38.251970, 51.507351,40.4196202,
                           34.053345,40.040394,58.299764,39.739212,32.775937,
                           36.167114,41.883718,33.748188,25.7279534,32.776499,
                           35.596198,36.2944928,39.0623301,39.158168,
                           39.2908816,38.895530700683594,39.9622601,41.3082138,41.64004898071289,
                           42.3602534,
                           61.0666922,52.865196,46.603354,51.0834196,55.670249,
                           59.6749712,63.2467777,64.6863136,49.8167003,42.6384261,
                           46.7985624,38.9953683,50.6402809,52.5001698,49.8158683,
                           26.2540493,10.2116702,35.000074,36.638392,
                           23.9891553,19.4326296,20.9876996,18.833333,
                           17,16.5000001,20.6845957,19.6666671),
                     lng=c(-96.138931,-77.046921,-76.147453,140.886220,-0.127758,-3.6918167,
                           -118.242349, -76.304053,-134.406746,-104.9903028,-96.796781,
                           -115.149334,-87.632382,-84.390865,-80.2340487,-79.930773,
                           -82.549193,-82.4734089,-78.9694723,-75.5243682,
                           -76.610759,-77.0319595336914,-83.0007065,-72.9250518,-71.52472686767578,
                           -71.0582912,
                           -107.9917071,-7.9794599,1.8883335,10.4234469,10.3333283,
                           14.5208584,25.9209164,97.7453061,15.4749544,12.674297,
                           8.2319736,21.9877132,4.6667145,5.7480821,6.1296751,
                           29.2675469,38.6521203,104.999927,127.6961188,
                           -98.7026825,19.4326296,-101,-98,
                           -96.5,-92.5000001,-88.8755669,-88.5000001),
                     tag=c("Hometown","Lived","Lived","Lived","Lived","Lived",
                           "Visited","Visited","Visited","Visited","Visited",
                           "Visited","Visited","Visited","Visited","Visited",
                           "Visited","Visited","Visited","Visited",
                           "Visited","Visited","Visited","Visited","Visited",
                           "Visited",
                           "Visited","Visited","Visited","Visited","Visited",
                           "Visited","Visited","Visited","Visited","Visited",
                           "Visited","Visited","Visited","Visited","Visited",
                           "Visited","Visited","Visited","Visited",
                           "Visited","Visited","Visited","Visited",
                           "Visited","Visited","Visited","Visited"))
                     

cities <- cities %>% 
        mutate(col=(ifelse(tag=="Hometown","green", 
                           ifelse(tag=="Visited", "blue","red")))) %>% 
        mutate(radius=(ifelse(tag=="Hometown",4,
                              ifelse(tag=="Lived",3,1.2))))

cities %>% 
        leaflet() %>% 
        addTiles() %>% 
        addCircleMarkers(color = cities$col, radius=cities$radius, popup=cities$name) %>% 
        addLegend(labels = unique(cities$tag), colors = c("green","red","blue"))
