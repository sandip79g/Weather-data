#Sandeep Adhikari (NP000420)
#installing the package
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotrix")
install.packages("tidyverse")
#implementing the package
library(dplyr)
library(ggplot2)
library(plotrix)
library(tidyverse)
#Reading importing the CSV file from directory
data=read.csv("Weather.csv", TRUE, sep = ",")
class(data)
print(data)
data("iris")
temp <- max(data$temp)
print(sal)
day <- min(data$hour)
print(day)
retval <-subset(data, hour ==max(hour))
print(retval)


library(readxl)
dataset <- read_excel(NULL)
View(dataset)


agg <- aggregate(data$val1, list(data$temp, data$hour), mean)
print(agg)
ChickWeight
cw <- ChickWeight
data(cw)
aggregate(month~day,weather,mean)

#Grouping the data IN R
mydata<-read.csv("Weather.csv", stringsAsFactors = FALSE)


#AGGREGRATE FUNCTION
aggregate(humid ~ month, mydata, mean)

mydata %>%
  select(
    month,day
  )
#plotting the data in the graphical form
plot(mydata$month, mydata$day,
     main = "temp",
     xlab = "month",
     ylab = "day",
     )
setwd("E:\\R-Project")
write.csv("Weather.csv", na = "0")

#Analysis: 1 (Wind gust and wind speed)

a <- mydata %>%
  select(
    origin,
    wind_gust,wind_speed
  ) %>% filter(!is.na(wind_gust))%>%filter(!is.na(wind_speed))
summary(a)
ggplot(a) +
  aes(x = wind_speed, y = wind_gust, colour = origin) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  stat_smooth(method="lm", formula=y~x)+
  labs(
    x = "wind speed",
    y = "wind gust",
    title = "Wind speed and wind gust point analysis"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L,
                                face = "bold"),
    axis.title.x = element_text(size = 15L,
                                face = "bold")
  )

#  Analysis:2 Temperature and pressure
ggplot(mydata) +
  aes(x = temp, y = pressure, colour = origin) +
  geom_point(shape = "square", size = 1.5) +
  scale_color_brewer(palette = "Set1", direction = 1) +
  stat_smooth(method="lm", formula=y~x)+
  labs(
    x = "Tempurature",
    y = "Pressure",
    title = "Tempurature and presssure of point data"
  ) +
  theme_gray() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L,
                                face = "bold"),
    axis.title.x = element_text(size = 15L,
                                face = "bold")
  )
#Analysis: 3 (Months and the dew-point)
b <- mydata %>%
  dplyr::mutate(months=month.name[month]) 
head(b)
      b%>%
        arrange(
          sort(factor(months, levels = month.name))) %>%
      ggplot(b, mapping = aes(x = dewp, colour = origin )) +
      geom_histogram(bins = 30L, fill = "#112446") +
      scale_color_hue(direction = 1) +
      theme_gray() +
      theme(
        plot.title = element_text(size = 20L,
                                  face = "bold",
                                  hjust = 0.5),
        axis.title.y = element_text(size = 15L,
                                    face = "bold"),
        axis.title.x = element_text(size = 15L,
                                    face = "bold")
      ) +
      facet_wrap(vars(months))
    
#Analysis: 4 (Temperature and Dewp) 

ggplot(mydata) +
  aes(x = temp, y = dewp, colour = origin) +
  stat_smooth(method = "lm", formula = y~x)+
  geom_point(shape = "circle plus", size = 1.5) +
  scale_color_brewer(palette = "Set1", direction = 1) +
  theme_minimal()

  
# #Analysis: 5 ( Humidity and Visibility) 

ggplot(mydata) +
  aes(x = humid, y = temp, colour = origin) +
  geom_point(shape = "circle", size = 1.5) +
  geom_smooth(method = "lm", formula = y~x) +
  scale_color_viridis_d(option = "inferno", direction = 1) +
  labs(
    x = "Humidity",
    y = "Tempurature",
    title = "Tempurature and humidity"
  ) +
  theme_dark() +
  theme(
    plot.title = element_text(size = 20L,
                              face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(size = 15L,
                                face = "bold"),
    axis.title.x = element_text(size = 15L,
                                face = "bold")
  )

#Analysis: 7 (Precipitation and Wind_speed)
#values <- c(x=Percip, y=wind_speed, color = origin) 
 # pie3D(values, labels = values, main = "Piechart", explode = 0.1, 
  #      )
   #temp%>%value
   #summary(select(trees, Height, Volume))
   #trees%>%
    # select(Height, Volume)%>%
#     summary()
   #analysis : 6(dewp and visibility)
   ggplot(data) +
     aes(x = dewp, y = visib, colour = origin, size = visib) +
     geom_tile() +
     scale_color_hue(direction = 1) +
     labs(
       x = "dewp",
       y = "visib",
       title = "Dewpoint and Visibility"
     ) +
     theme_gray() +
     theme(
       plot.title = element_text(size = 15L,
                                 face = "bold",
                                 hjust = 0.5),
       axis.title.y = element_text(size = 13L,
                                   face = "bold"),
       axis.title.x = element_text(size = 13L,
                                   face = "bold")
     ) +
     facet_wrap(vars(origin))
   
  # Analysis: 7(Precipitation and visibility  )
   s <-subset(mydata,precip>0)
   summary(s)
ggplot(s) +
 aes(x = precip, y = factor(visib), colour = origin) +
 geom_point(shape = "diamond", size = 1.5) +
 scale_color_manual(values = c(JFK = "#0D0887", LGA = "#7421F9")) +
 theme_gray() +
  labs(
    x = "Percipitation",
    y = "Visibility",
    title = "Percipitation and visibility point graph"
  )+
 facet_wrap(vars(origin))

  # Analysis:8 (Humidity and visibility)
   
ggplot(mydata) +
 aes(x = humid, y = visib, colour = origin) +
 geom_jitter(size = 1.5) +
 scale_color_hue(direction = 1) +
 labs(x = "Humidity ", y = "Visibility", title = "Jitter plot of the humidity and visibility") +
 theme_gray() +
 theme(plot.title = element_text(size = 20L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 15L, 
 face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

# Analysis:9 wind speed and month
c <- mydata%>%
  select(
    wind_speed,month,origin
  ) %>% filter(!is.na(wind_speed))

ggplot(c) +
 aes(x = factor(month), y = wind_speed, colour = origin) +
 geom_jitter(size = 1.5) +
 scale_color_brewer(palette = "Dark2", 
 direction = 1) +
 labs(x = "Months", y = "Wind Speed", title = "Jitter plot of the windspeed and the months") +
 theme_minimal() +
 theme(plot.title = element_text(size = 20L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 15L, 
 face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

#Analysis: 10 (wind gust and month)

d <- mydata %>%
  select(
    origin,
    wind_gust,month
  ) %>% filter(!is.na(wind_gust))

ggplot(d) +
 aes(x = factor(month), y = wind_gust, colour = origin) +
 geom_point(shape = "circle", size = 1.5) +
 scale_color_hue(direction = 1) +
  labs(
    title = "Wind speed and month plot",
    x = "Month",
    y = "Wind_gust",
  )+
 theme_minimal()

#Analysis:11 Wind-speed and pressure

e <- mydata%>%
  select(
    pressure,wind_speed,origin
  ) %>% filter(!is.na(pressure))%>%filter(!is.na(wind_speed))
ggplot(e) +
 aes(x = pressure, y = wind_speed, colour = origin) +
 geom_point(shape = "circle", size = 1.5) +
 scale_color_hue(direction = 1) +
 theme_minimal()+
  labs(
    title = "Wind_speed and the pressure",
    x = "Pressure",
    y = "Wind_speed",
  )+
stat_smooth( method = "lm", formula = y~x)


#Analysis: 12 Temperature and month

b %>%
  arrange(
    sort(factor(months,levels = month.name))
  ) %>% 
ggplot(b, mapping = aes(x = fct_inorder(months), y = temp, colour = origin)) +
 geom_boxplot(size = 1.5) +
  labs(
    x="month",
    y="Temperature",
    title="temperature and month box plot"
  ) +
 scale_color_hue(direction = 1) +
 theme_minimal()

#Analysis: 13 (Precipitation and Humidity) 

ggplot(s) +
 aes(x = humid, y = precip, colour = origin) +
 geom_point(shape = "circle", size = 1.5) +
 scale_color_viridis_d(option = "inferno", direction = 1) +
 labs(x = "Humidity", y = "Percipitation", 
 title = "Humidity and percipitation in point plot") +
 theme_gray() +
 theme(legend.position = "top", 
 plot.title = element_text(size = 20L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 15L, 
 face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

#Analysis: 14 ( Humidity and dew-point)

plot <- ggplot(mydata) +
 aes(x = humid, y = dewp, colour = origin) +
 geom_point(shape = "circle", size = 1.5) +
 stat_smooth(method = "lm", formula = y~x)+
 scale_color_manual(values = c(JFK = "#D23CBE", LGA = "#292026")) +
 labs(x = "Humid", y = "Dewpoint", 
 title = "Humidity and the dewpoint Point plot") +
 theme_light() +
 theme(plot.title = element_text(size = 20L, 
 face = "bold", hjust = 0.5), axis.title.y = element_text(size = 15L, face = "bold"),
 axis.title.x = element_text(size = 15L, 
 face = "bold"))
plot

#Extra Features:
   exPlotSave<- function(e){
     cat("1: Save as PNG \n2: Save as JPEG \n3: Save as PDF \n Choose Option (1/2/3):")
     option = readline()
     cat("\n")
     cat("Enter the filename: ")
     fname = readline()
     cat("Specify the file path (e.g E:\\R-Project): ")
     fpath=readline()
     if (fpath==''){
       fpath=getwd()
     }
     switch (option,
             "1" = ggsave(filename=paste(fname,"png", sep="."),
                          plot=e,
                          device = "png",
                          path=fpath ),
             "2" = ggsave(filename=paste(fname,"jpg", sep="."),
                          plot=e,
                          device = "jpg",
                          path=fpath),
             "3" = ggsave(filename=paste(fname,"pdf", sep="."),
                          plot=e,
                          device = "pdf",
                          path=fpath)
     )
   } 
   
   exPlotSave(plot)

#Analysis  (Day and month)
   b
     ggplot(b) +
       aes(x = day, y = factor(month), fill = visib) +
       geom_tile(size = 1.2) +
       scale_fill_gradient() +
       theme_gray() +
       labs(
         x = "Day",
         y = "Month",
         Title = "Day and month analysis of tile plot"
       )+
       facet_wrap(vars(origin))
     
       #Additional Features: 1
       ## Showing the data of the co-ordination polar plot in the wind speed.
       Result<- ggplot(data = data) +
         geom_bar(mapping = aes(x = wind_speed, na.rm = TRUE)) +
         coord_polar() +
         labs(title = 'Polar Bar Plot of Wind Speed',x = 'Wind Speed ') + theme_bw()+
         theme(panel.background = element_rect(fill = "grey"))
       print(Result)
       
     #Additional Features: 2 
     #showing the data of the Wind speed and the temperature in Heat-map.
      ggplot(data, mapping=aes(x = wind_speed, y = temp))+
      geom_bin2d(size = 1.2, bins = 30L, na.rm = TRUE)+
      scale_fill_distiller(palette = "Set1",   direction = 1)+
      labs(x = "Wind speed", 
            y = "Temperature", 
            title = "Wind Speed and the Temperaute Analysis in Heatmap")+
      theme_minimal()
