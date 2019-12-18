library(sf)
library(ggplot2)

#install.packages('devtools')
#devtools::install_github("tidyverse/ggplot2")
require(ggplot2)


lga.df <- read.csv("data/yvw.lgas.csv")
centroid.df <-  read.csv("data/yvw-centroids.csv")
lga.sf <-  st_read("data/yvw.geojson")

lga.sf <- merge(lga.sf, lga.df, by = "GAZ_LGA")
lga.sf <- merge(lga.sf, centroid.df, by = "GAZ_LGA")

names(lga.sf)[names(lga.sf)=='X.y'] <- 'xcoord'
names(lga.sf)[names(lga.sf)=='Y'] <- 'ycoord'

interval.length <- (max(lga.sf$mean.affordability) - min(lga.sf$mean.affordability))/6
# mean.affordability choropleth
ggplot() +                                                                          # initialise a ggplot object
  geom_sf(data = lga.sf,                                                            # add a simple features (sf) object
          aes(fill = cut_interval(x=mean.affordability, 5)),   # group percent into equal n and use for fill (see also cut_interval for equal intervals)
          alpha = 0.8,                                                              # add transparency to the fill
          colour = 'grey',                                                          # make polygon boundaries grey (see R's predefined named colours)
          size = 0.2) +                                                             # adjust width of polygon boundaries
  scale_fill_brewer(palette = "RdYlBu", direction = -1,                             # choose a http://colorbrewer2.org/ palette, reverse direction
                    name = "Legend (%)",
                    labels = c("1.15 to 1.38", "1.38 to 1.6", "1.6 to 1.83", "2.06 to 2.28")) +   # add legend title
  geom_text(x=lga.sf$xcoord, y=lga.sf$ycoord, data = lga.sf, aes(label = LGA_NAME16),
            check_overlap = TRUE,  parse = FALSE) +                                 # label polygons
  labs(x = NULL, y = NULL,                                                          # drop axis titles
       title = "Mean Affordability",                                                # add title
       subtitle = "LGAs in 2017",                                                   # add subtitle
       caption = "Mean percentage of disposable income spent on water bill.") +     # add caption
  theme(panel.background = element_blank()) +                                       # remove background gridlines
coord_sf(datum = NA)

ggsave("img/lga-mean-affordability.png", plot = last_plot())

# percent.unaffordable choropleth
ggplot() +                                                                          # initialise a ggplot object
  geom_sf(data = lga.sf,                                                            # add a simple features (sf) object
          aes(fill = cut_interval(x=percent.unaffordable, 5)),   # group percent into equal intervals and use for fill
          alpha = 0.8,                                                              # add transparency to the fill
          colour = 'grey',                                                          # make polygon boundaries grey
          size = 0.3) +                                                             # adjust width of polygon boundaries
  scale_fill_brewer(palette = "RdYlBu", direction = -1,                             # choose a http://colorbrewer2.org/ palette
                    name = "Legend (%)",
                    labels = c("5.71 to 9.06", "9.06 to 12.4", "12.4 to 15.7", "19.1 to 22.4")) +   # add legend title
  geom_text(x=lga.sf$xcoord, y=lga.sf$ycoord, data = lga.sf, aes(label = LGA_NAME16),
            check_overlap = TRUE,  parse = FALSE) +                                 # label polygons
  labs(x = NULL, y = NULL,                                                          # drop axis titles
       title = "Affordability Stress",                                              # add title
       subtitle = "LGAs in 2017",                                                   # add subtitle
       caption = "Percent of households who spend > 3% of disposable income on bill.") +            # add caption
  theme(panel.background = element_blank()) +                                       # remove background gridlines
  coord_sf(datum = NA)

ggsave("img/lga-percent-unaffordable.png", plot = last_plot())

# percent.unaffordable lowest 40% choropleth
ggplot() +                                                                          # initialise a ggplot object
  geom_sf(data = lga.sf,                                                            # add a simple features (sf) object
          aes(fill = cut_interval(pct.unafford.40, 5)),                          # group percent into equal intervals and use for fill
          alpha = 0.8,                                                              # add transparency to the fill
          colour = 'grey',                                                          # make polygon boundaries grey
          size = 0.3) +                                                             # adjust width of polygon boundaries
  scale_fill_brewer(palette = "RdYlBu", direction = -1,                             # choose a http://colorbrewer2.org/ palette
                    name = "Legend (%)",
                    labels = c("10.5 to 17.4", "17.4 to 24.3", "24.3 to 31.2", "31.2 to 38.1", "38.1 to 45")) +   # add legend title
  geom_text(x=lga.sf$xcoord, y=lga.sf$ycoord, data = lga.sf, aes(label = LGA_NAME16),
            check_overlap = TRUE,  parse = FALSE) +                                 # label polygons
  labs(x = NULL, y = NULL,                                                          # drop axis titles
       title = "Affordability Stress",                                              # add title
       subtitle = "LGAs in 2017",                                                   # add subtitle
       caption = "Percent of households who spend > 3% of disposable income on bill.") +            # add caption
  theme(panel.background = element_blank()) +                                       # remove background gridlines
  coord_sf(datum = NA)

ggsave("img/lga-pct-unafford-40.png", plot = last_plot())

ggplot(lga.df, aes(x=GAZ_LGA, y=mean.affordability)) +
  geom_bar(stat='identity', position='dodge') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 3) + 
  xlab("LGA") + ylab("Mean Affordability %") +
  ggtitle("Mean Affordability by LGA")

ggsave("img/lga-mean-afford-bar.png", plot = last_plot())

ggplot(lga.df, aes(x=GAZ_LGA, y=percent.unaffordable)) +
  geom_bar(stat='identity', position='dodge') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("LGA") + ylab("Affordability Stress %") +
  ggtitle("Affordability Stress by LGA") 

ggsave("img/lga-percent-unafford-bar.png", plot = last_plot())

ggplot(lga.df, aes(x=GAZ_LGA, y=pct.unafford.40)) +
  geom_bar(stat='identity', position='dodge') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("LGA") + ylab("Affordability Stress %") +
  ggtitle("Affordability Stress Amongst Lowest 40% of Income by LGA") 

ggsave("img/lga-percent-unafford-40-bar.png", plot = last_plot())

lga.df$X <- NULL

# renaming
names(lga.df)[names(lga.df)=='GAZ_LGA'] <- 'LGA'
names(lga.df)[names(lga.df)=='mean.affordability'] <- 'Mean.Affordability'
names(lga.df)[names(lga.df)=='percent.unaffordable'] <- 'Affordability.Stress'
names(lga.df)[names(lga.df)=='pct.unafford.40'] <- 'Affordability.Stress.Lowest.40'

library(knitr)
kable(lga.df,digits = 2)