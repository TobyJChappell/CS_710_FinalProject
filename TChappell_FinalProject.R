library(waffle)
library(magrittr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyquant)
library(reshape2)
library(tibble)

setwd("~/Documents/CS_Courses/CS_710/CS_710_FinalProject")

# DISNEY
disney <- read.csv(file = 'Data/disney_plus_shows.csv')

# Splits genre into 9 columns
genre_list <- c("genre_1","genre_2","genre_3","genre_4","genre_5","genre_6","genre_7","genre_8","genre_9")
genre_split <- disney
genre_split$genre_temp <- genre_split$genre
genre_split <- separate(data = genre_split, col = genre_temp, into = genre_list, sep = ",\\s+")
genre_split$imdb_score <- as.numeric(genre_split$imdb_rating)
genre_split <- genre_split[!is.na(genre_split$imdb_score),]

# Create 9 separate data sets that group data by one of the genre columns. The average IMDb score and count are stored.
genres_1 <- genre_split %>% group_by(genre=genre_1) %>% summarize(rating_1=mean(imdb_score), n_1 = n()) 
genres_2 <- genre_split %>% group_by(genre=genre_2) %>% summarize(rating_2=mean(imdb_score), n_2 = n()) %>% drop_na()
genres_3 <- genre_split %>% group_by(genre=genre_3) %>% summarize(rating_3=mean(imdb_score), n_3 = n()) %>% drop_na()
genres_4 <- genre_split %>% group_by(genre=genre_4) %>% summarize(rating_4=mean(imdb_score), n_4 = n()) %>% drop_na()
genres_5 <- genre_split %>% group_by(genre=genre_5) %>% summarize(rating_5=mean(imdb_score), n_5 = n()) %>% drop_na()
genres_6 <- genre_split %>% group_by(genre=genre_6) %>% summarize(rating_6=mean(imdb_score), n_6 = n()) %>% drop_na()
genres_7 <- genre_split %>% group_by(genre=genre_7) %>% summarize(rating_7=mean(imdb_score), n_7 = n()) %>% drop_na()
genres_8 <- genre_split %>% group_by(genre=genre_8) %>% summarize(rating_8=mean(imdb_score), n_8 = n()) %>% drop_na()
genres_9 <- genre_split %>% group_by(genre=genre_9) %>% summarize(rating_9=mean(imdb_score), n_9 = n()) %>% drop_na()

# Merge new data sets into a single data set
genres <- merge(x=genres_1,y=genres_2,by="genre",all=TRUE)
genres <- merge(x=genres,y=genres_3,by="genre",all=TRUE)
genres <- merge(x=genres,y=genres_4,by="genre",all=TRUE)
genres <- merge(x=genres,y=genres_5,by="genre",all=TRUE)
genres <- merge(x=genres,y=genres_6,by="genre",all=TRUE)
genres <- merge(x=genres,y=genres_7,by="genre",all=TRUE)
genres <- merge(x=genres,y=genres_8,by="genre",all=TRUE)
genres <- merge(x=genres,y=genres_9,by="genre",all=TRUE)

genres[is.na(genres)] <- 0

# Get the cumulative average IMDb score by genre and count by genre
genres_group <- genres %>% group_by(genre) %>% summarise(mean_imdb = (rating_1*n_1+rating_2*n_2+rating_3*n_3+rating_4*n_4+rating_5*n_5+rating_6*n_6+rating_7*n_7+rating_8*n_8+rating_9*n_9)/(n_1+n_2+n_3+n_4+n_5+n_6+n_7+n_8+n_9),n = n_1+n_2+n_3+n_4+n_5+n_6+n_7+n_8+n_9)
genres_group <- as.data.frame(genres_group)
genres_group <- genres_group[order(genres_group$n,decreasing = TRUE),]

# Keep 10 specific genres for analysis
genres_ten <- head(genres_group[-c(1,9),],10)

# Scatter plot for 10 genres of interest
genre_point <- ggplot(genres_ten, aes(x=genre,y=mean_imdb,size=n,color=n)) + 
  geom_point() +
  geom_text(aes(label=n),color="white",family="Artifakt Element",size=6)+
  theme_classic() +
  scale_color_gradient(low="#111E4F",high="#74D3E6",guide=FALSE) +
  scale_size_continuous(range = c(15, 40),guide=guide_legend(override.aes=list(size = c(7.5,10,12.5,15),colour=c("#111E4F","#325a81","#5396b3","#74D3E6")))) + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent",colour = NA),
    legend.title = element_blank(),
    legend.text = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="#111E4F",size = 12, family="Artifakt Element"),
    legend.key.width = unit(.5,"cm")
  )
ggsave("Images/Genre_Point.png", genre_point, width=11, height=7, bg = "transparent")

# Bar chart for 10 genres of interest (not used in final visualization)
genre_bar <- ggplot(genres_ten,aes(x=genre,y=n,fill=mean_imdb)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label=round(mean_imdb,digits=2)),vjust = 1.5,color="white",family="Artifakt Element",size=6) + 
  theme_classic() +
  scale_fill_gradient(low="#111E4F",high="#74D3E6",name="IMDb Score") +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent",colour = NA),
    legend.title = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="#111E4F",size = 12, family="Artifakt Element"),
    legend.key.width = unit(.5,"cm")
  )
ggsave("Images/Genre_Bar.png", genre_bar, width=11, height=7, bg = "transparent")

# Randomly select one of the 10 genres if a title has more than one of the 10 and store in a new column
genre_ten <- genres_ten$genre
genre_split <- genre_split %>% add_column(genres_ten = NA)
for (row in 1:nrow(genre_split)) {
  genre_ten <- sample(genre_ten)
  for (genre in genre_ten) {
    if (grepl(genre,genre_split[row,"genre"],fixed=TRUE)) {
      genre_split[row,"genres_ten"] <- genre
      break
    }
  }
}
genre_split <- genre_split[!is.na(genre_split$genres_ten),]
genre_split$date_released <- as.Date(genre_split$released_at,"%d %b %Y")
genre_split <- genre_split[!is.na(genre_split$date_released),]

# Get the average IMDb score by year
genre_avg <- genre_split
genre_avg <- genre_avg %>% mutate(year = year(date_released)) %>% group_by(year) %>% summarize(avg_imdb=mean(imdb_score))
genre_avg$year <- as.Date(as.character(genre_avg$year),"%Y")

# Scatter plot for IMDb score over time colored by genre
time_plot <- ggplot() + 
  geom_point(data=genre_split,aes(x=date_released,y=imdb_score,color=genres_ten)) +
  geom_line(data=genre_avg,aes(x=year,y=avg_imdb),color = "#111E4F",size=1.5) +
  theme_classic() +
  scale_colour_manual(name="Genre",values=c("#111E4F","#1c325f","#274670","#325a81","#3c6e92","#4882a2","#5396b3","#5eaac4","#68bed5","#74D3E6")) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent",colour = NA),
    legend.title = element_blank(),
    legend.text = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(color="#111E4F",size = 12, family="Artifakt Element")
  )
ggsave("Images/Time_Scatter.png", time_plot, width = 9, height = 8, bg = "transparent")

# Show counts of all ratings
ratings <- disney %>% group_by(rated) %>% count()

# Select specific ratings from data set (group movie and tv ratings together manually) 
ratings <- data.frame("rated"=c("All","G","PG","PG-13"),"n"=c(94,339,196,41))

# Get data in format for a donut chart
ratings$fraction <- ratings$n/sum(ratings$n)
ratings$ymax = cumsum(ratings$fraction)
ratings$ymin = c(0, head(ratings$ymax, n=-1))
ratings$labelPosition <- (ratings$ymax + ratings$ymin) / 2
ratings$label <- paste0(ratings$n)

# Donut chart for ratings
ratings_plot <- ggplot(ratings, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=rated)) +
  geom_rect() +
  geom_text(x=3.5, aes(y=labelPosition, label=label), color="white",family="Artifakt Element",size=5) +
  scale_fill_manual(name="Rating",values=c("#111E4F","#325A81","#5397B4","#74D3E6")) +
  coord_polar(theta="y") +
  xlim(c(1, 4)) +
  theme_void() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    legend.background = element_rect(fill = "transparent",colour = NA),
    legend.title = element_blank(),
    legend.text = element_blank()
  )
ggsave("Images/Rating_Donut.png", ratings_plot, width=7,bg = "transparent")

# COMPARISON
streaming <- read.csv(file = 'Data/MoviesOnStreamingPlatforms_updated.csv')
streaming <- streaming[!is.na(streaming$Prime.Video | streaming$Netflix | streaming$Hulu | streaming$Disney.),]

# Create 4 separate data sets for each streaming platform that contains the IMDb score and calculates the average IMDb score
prime <- streaming[streaming$Prime.Video==1,]
prime <- prime[!is.na(prime$IMDb),]
prime_mu <- mean(prime$IMDb)
netflix <- streaming[streaming$Netflix==1,]
netflix <- netflix[!is.na(netflix$IMDb),]
netflix_mu <- mean(netflix$IMDb)
hulu <- streaming[streaming$Hulu==1,]
hulu <- hulu[!is.na(hulu$IMDb),]
hulu_mu <- mean(hulu$IMDb)
disney <- streaming[streaming$Disney.==1,]
disney <- disney[!is.na(disney$IMDb),]
disney_mu <- mean(disney$IMDb)

# Density Plot for IMDb scores by streaming platform
density_plot <- ggplot() + 
  geom_density(data=prime,aes(x=IMDb),alpha=0.2,color="grey60",fill="grey60") + 
  geom_density(data=netflix,aes(x=IMDb),alpha=0.2,color="grey75",fill="grey75") + 
  geom_density(data=hulu,aes(x=IMDb),alpha=0.2,color="grey90",fill="grey90") + 
  geom_density(data=disney,aes(x=IMDb),alpha=0.4,color="#111E4F",fill="#111E4F") + 
  geom_line(aes(x=prime_mu,y=c(0,.4)), color="grey60", linetype="dashed",size=1.5) +
  geom_line(aes(x=netflix_mu,y=c(0,.4)), color="grey75", linetype="dashed",size=1.5) +
  geom_line(aes(x=hulu_mu,y=c(0,.4)), color="grey90", linetype="dashed",size=1.5) +
  geom_line(aes(x=disney_mu,y=c(0,.4)), color="#111E4F", linetype="dashed",size=1.5) +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA),
    axis.title = element_blank(),
    axis.text = element_text(color="#333333",size=12,family="Artifakt Element")
  )
ggsave("Images/Quality_Density.png", density_plot, width=10, height=6, bg = "transparent")

# Get counts of titles by platform
platform <- streaming %>% summarize(Netflix=sum(Netflix),Hulu=sum(Hulu),Prime.Video=sum(Prime.Video),Disney.=sum(Disney.))
platform <- data.frame("Platform"=c('Prime Video','Netlflix','Hulu','Disney+'),"Count"=c(12354,3560,903,554))
platform$Platform <- factor(platform$Platform,levels=c('Prime Video','Netlflix','Hulu','Disney+'))

# Waffle plot colored by streaming platform (1 square = 10 titles)
waffle_plot <- ggplot(platform,aes(fill = Platform, values = Count/10)) +
  expand_limits(x=c(0,0), y=c(0,0)) +
  coord_equal() +
  labs(fill = NULL, colour = NULL) +
  theme_ipsum_rc(grid="") +
  theme_enhance_waffle() +
  scale_fill_manual(values=c("grey60","grey75","grey90","#111E4F")) + 
  geom_waffle(n_rows = 20, 
              size = 0, 
              radius = unit(1, "pt"),
              height = 0.8, 
              width = 0.8) +
  theme(
    legend.position = "bottom", 
    legend.text = element_blank()
  )
ggsave("Images/Quantity_Waffle.png", waffle_plot, bg = "transparent")
