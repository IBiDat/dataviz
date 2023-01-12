#### Final project for Data visualization
#Jorge Pascual Segovia

#### Libraries
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lubridate)
library(directlabels)
library(ggforce)
library(gghighlight)
library(ggtext)
library(gganimate)
library(gifski)
library(magick)
library(av)

#### Read and clean the Dataset
spoti<-read_csv("Datos Spotify - Dataviz.csv", skip = 2,
                col_names = c("Date","As It Was","Despechá","Me Porto Bonito","Pink Venom","Quevedo: Bzrp Music 
Sessions, Vol. 52","Tití Me Preguntó"))

#Pivot long the Song
spoti <- spoti %>% 
  pivot_longer(-Date,
               names_to = "Song",
               values_to = "Ranking")

#Transform Date column into "date type" 
spoti$Date <- paste(spoti$Date, "2022",sep="-")
spoti$Date<-  gsub("jul",07,spoti$Date)
spoti$Date<-  gsub("ago",08,spoti$Date)
spoti$Date <- dmy(spoti$Date)

#### the adjustments
theme_set(theme_minimal()) #Set the minimal theme
sysfonts::font_add_google("Roboto") #Adding a google font type
#showtext::showtext_auto()

#for the location of the legend annotations
df.labs <- spoti %>% 
  filter(Date == "2022-08-23")

####Code for the Replication Plot
p <- ggplot(spoti) +
  aes(Date, Ranking) +                                                 #set x and y axis
  geom_line(aes(color=Song), size = 1) +                               #set the visualization mark
  scale_y_continuous(trans = "reverse", n.breaks = 10) +               #set the order of y axis guides
  scale_x_date(
  date_labels = "%d-%b",                                               #set the date format of the y tickmarks
  breaks = seq(as.Date("2022-07-11"), as.Date("2022-08-25"), by = 6)) +
  theme(text = element_text(family="Roboto")) +                        #set the font
  labs(
    title="Evolución diaria del ránking en Spotify", 
    subtitle = "Posición de las seis canciones más escuchadas en la plataforma a nivel mundial") +
  theme(plot.title = element_text(face = "bold", size = 16.5)) +       #type of letter of titles
  theme(plot.subtitle = element_text(size = 10.5, colour = "#6f6f6f")) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme(axis.text.y = element_text(colour = "#a8a8a8", size = 10),     #edit the theme
        axis.line.y = element_line(size = 0, color = "#eeeeee"),
        axis.text.x = element_text(colour = "#a8a8a8", size = 10),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position="none") +                                      #remove the legend
  geom_text(aes(
    label=Song, colour = Song), df.labs, hjust=0, nudge_x=0.7) +       #Annotations for legend
  expand_limits(x= as.Date("2022-9-1")) +                              #Create space for the annotations
  expand_limits(y= 9) + 
  scale_color_manual(values=c(                                         #Add the colors from the original graph
  "#8dc7ad", "#5cb689", "#267d59", "#54b182", "#c71e1d", "#4ea07c"))   #https://imagecolorpicker.com/
p

####Improved Plot
theme_set(theme_minimal()) #Set the minimal theme

p2 <- ggplot(spoti) +
  aes(Date, Ranking) +                                                 #set x and y axis
  geom_line(aes(color=Song), size = 1) +                               #set the visualization mark
  scale_y_continuous(trans = "reverse", n.breaks = 10) +               #set the order of y axis guides
  scale_x_date(
    date_labels = "%d-%b",                                             #set the date format of the y tickmarks
    breaks = seq(as.Date("2022-07-11"), as.Date("2022-08-25"), by = 6)) +
  theme(text = element_text(family="Roboto")) +                        #set the font
  labs(
    title="Evolución diaria del ránking en Spotify", 
    subtitle = "Posición de las seis canciones más escuchadas mundialmente en la plataforma en 2022") +
  theme(plot.title = element_text(face = "bold", size = 16.5)) +       #type of letter of titles
  theme(plot.subtitle = element_text(size = 10.5, colour = "#6f6f6f")) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme(axis.text.y = element_text(colour = "black", size = 10),       #edit the theme
        axis.line.y = element_line(size = 0, color = "#6c6c6c"),
        axis.text.x = element_text(colour = "#6c6c6c", size = 10),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position="none") +                                      #remove the legend
  geom_text(aes(
    label=Song, colour = Song), df.labs, hjust=0, nudge_x=0.7) +       #Annotations for legend
  expand_limits(x= as.Date("2022-9-1")) +                              #Create space for the annotations
  expand_limits(y= 9) 
p2  

#### Alternative plot (animated)
#Song name (X axis)
#Rank position (Y axis)
#Song genre* (colour)
#Time (animated)

# Create the "Genre" column
spotig <- spoti %>% 
mutate("Genre" = case_when(
    endsWith(Song, "52") ~ "Pop",
    endsWith(Song, "As It Was") ~ "Pop",
    endsWith(Song, "Pink Venom") ~ "K-Pop",
    endsWith(Song, "Me Porto Bonito") ~ "Reggaeton/Latino",
    endsWith(Song, "Tití Me Preguntó") ~ "Reggaeton/Latino",
    endsWith(Song, "Despechá") ~ "Reggaeton/Latino"
  ))

# Choose theme
theme_set(theme_minimal())

#### Alternative plot code
pa <- ggplot(spotig) +
  aes(Song, Ranking) +
  geom_text(aes(color = Genre, label = Song)) + 
  scale_y_continuous(trans = "reverse", n.breaks = 9) +
  expand_limits(x=c(0, 7)) +
  labs(
    title="Evolución diaria del ránking en Spotify", 
    subtitle = "Evolución de las 6 canciones más escuchadas mundialmente en verano de 2022",
    tag = "Date: 
    {frame_time}",
    x = NULL, y = NULL
    ) +
  transition_time(Date) +                                             # Date as the variable represented by the animation 
  ease_aes('linear') + 
  theme(
    plot.title = element_text(face = "bold", size = 16.5),
    plot.subtitle = element_text(size = 10.5, colour = "#6f6f6f"),
    plot.tag.position = c(0.836,0.65),
    plot.tag = element_text(face = "bold", size = 12, hjust = 0),
    legend.title = element_text(face = "bold", size = 12, hjust = 0),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "light grey"),
    axis.text.x = element_blank()
  ) 
  

#Run the animation (Choose one)
animate(pa, renderer = gifski_renderer(), duration = 14) #as a gif format
animate(pa, renderer = magick_renderer()) #as a magick image format 
animate(pa, renderer = av_renderer()) #as a AV video file
