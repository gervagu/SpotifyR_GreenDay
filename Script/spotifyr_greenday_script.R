library(spotifyr)
library(tidyverse)
library(ggridges)

#Set up account with Spotify Web Api
Sys.setenv(SPOTIFY_CLIENT_ID = "your spotify client ID")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "your spotify client secret")

#Pull audio features from Spotify. In this case I selected Green Day
greenday <- get_artist_audio_features('Green Day')


#Filter out for record albums (no live and recopilation albums)
greenday_album <- greenday %>% 
filter(!grepl('Deluxe|Live|Slappy|Unplugged|Demolicious|No Fun Mondays|Kerplunk!', album_name))

#Check for duplicates
duplicated(greenday_album$track_name)

#Do away repeated tracks
gd_track <- greenday_album %>% 
  distinct(track_name,.keep_all = T)


#Calculate more common key modes
gd_track %>% 
  count(key_mode, sort = TRUE) %>% 
  head(10) %>% 
  kable()

#Calculate track with the most energy
gd_track %>% 
  arrange(-energy) %>% 
  select(.data$track_name, .data$energy) %>% 
  head(10) %>% 
  kable()

#Calculate the most positive songs (Valence scale)
gd_track %>% 
  arrange(-valence) %>% 
  select(.data$track_name, .data$valence) %>% 
  head(10) %>% 
  kable()

#Calculate albums by level of positiveness 
gd_track %>% 
  group_by(album_name) %>%  
  summarise_at(vars(valence), list(name=median))


#Create a distribution graph of Green Day's albums by positive level
gd_track %>%
  arrange(album_release_year) %>%   
  mutate(album_name=factor(album_name, levels = rev(unique(gd_track$album_name)))) %>%  #Unique level for albums
  ggplot(aes(x= valence, y= album_name, fill= album_name)
  )+
  geom_density_ridges(from = 0, to = 1)+
  labs(
    x = "Valence scale: Tracks with high valence sound more positive and viceversa",
    y = "",
    title = "Distribution of Green Day's Musical Positiveness",
    subtitle = "Studio Albums based on Valence Scale",
    caption = " Gersán Vásquez Gutiérrez (@gervagu) | Source: Spotify's Web API"
  ) +
  scale_fill_cyclical(
    values = c("#9DE071")
  ) +
  theme_ridges()+ 
  theme(
    plot.title=element_text(hjust = 0.5, size = 20, color = "#3E8914", face="bold"),
    plot.title.position = "plot",
    plot.subtitle=element_text(face="italic", hjust = 0.5, size=16),
    axis.text.y = element_text(size = 14, face="bold")
  )

ggsave("greenday.png",width=60,height=30,units='cm',dpi=300)