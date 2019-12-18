library(sf)
library(ggplot2)

#install.packages('devtools')
#devtools::install_github("tidyverse/ggplot2")
require(ggplot2)


sa2.df <- read.csv("data/yvw.sa2s.csv")
sa2.sf <-  st_read("data/yvw.sa2.geojson")

sa2.sf <- merge(sa2.sf, sa2.df, by = "SA2_MAIN16")

# mean.affordability choropleth
ggplot() +                                                                          # initialise a ggplot object
  geom_sf(data = sa2.sf,                                                            # add a simple features (sf) object
          aes(fill = cut_interval(mean.affordability,6)),                            # group percent into equal n and use for fill (see also cut_interval)
          alpha = 0.8,                                                              # add transparency to the fill
          colour = 'grey',                                                          # make polygon boundaries grey (see R's predefined named colours)
          size = 0.2) +                                                             # adjust width of polygon boundaries
  scale_fill_brewer(palette = "RdYlBu", direction = -1,                             # choose a http://colorbrewer2.org/ palette, reverse direction
                    name = "Legend (%)",
                    labels = c("1.15 to 1.44", "1.44 to 1.61", "1.61 to 1.64", "1.64 to 1.71", "1.71 to 2.28")) +                                          # add legend title
  labs(x = NULL, y = NULL,                                                          # drop axis titles
       title = "Mean Affordability",                                                # add title
       subtitle = "LGAs in 2017",                                                   # add subtitle
       caption = "Mean percentage of disposable income spent on water bill.") +      # add caption
  theme(panel.background = element_blank()) +                                       # remove background gridlines
coord_sf(datum = NA)

ggsave("img/lga-mean-affordability-sa2.png", plot = last_plot())

# percent.unaffordable choropleth
ggplot() +                                                                          # initialise a ggplot object
  geom_sf(data = sa2.sf,                                                            # add a simple features (sf) object
          aes(fill = cut_number(percent.unaffordable, 5)),                          # group percent into equal intervals and use for fill
          alpha = 0.8,                                                              # add transparency to the fill
          colour = 'grey',                                                          # make polygon boundaries grey
          size = 0.3) +                                                             # adjust width of polygon boundaries
  scale_fill_brewer(palette = "RdYlBu", direction = -1,                             # choose a http://colorbrewer2.org/ palette
                    name = "Legend (%)") +                                          # add legend title
  labs(x = NULL, y = NULL,                                                          # drop axis titles
       title = "Affordability Stress",                                              # add title
       subtitle = "LGAs in 2017",                                                   # add subtitle
       caption = "Percent of households who spend > 3% of disposable income on bill.") +            # add caption
  theme(panel.background = element_blank()) +                                       # remove background gridlines
  coord_sf(datum = NA)

ggsave("img/lga-percent-unaffordable-sa2.png", plot = last_plot())

# percent.unaffordable lowest 40% choropleth
ggplot() +                                                                          # initialise a ggplot object
  geom_sf(data = sa2.sf,                                                            # add a simple features (sf) object
          aes(fill = cut_number(pct.unafford.40, 5)),                               # group percent into equal intervals and use for fill
          alpha = 0.8,                                                              # add transparency to the fill
          colour = 'grey',                                                          # make polygon boundaries grey
          size = 0.3) +                                                             # adjust width of polygon boundaries
  scale_fill_brewer(palette = "RdYlBu", direction = -1,                             # choose a http://colorbrewer2.org/ palette
                    name = "Legend (%)") +                                          # add legend title
  labs(x = NULL, y = NULL,                                                          # drop axis titles
       title = "Affordability Stress Amongst Lowest 40% of Income",                 # add title
       subtitle = "LGAs in 2017",                                                   # add subtitle
       caption = "Percent of households who spend > 3% of disposable income on bill, amongst lowest 40%.") +            # add caption
  theme(panel.background = element_blank()) +                                       # remove background gridlines
  coord_sf(datum = NA)

ggsave("img/lga-pct-unafford-40-sa2.png", plot = last_plot())

sa2.df$X <- NULL

# renaming
names(sa2.df)[names(sa2.df)=='SA2_NAME'] <- 'SA2'
names(sa2.df)[names(sa2.df)=='mean.affordability'] <- 'Mean.Affordability'
names(sa2.df)[names(sa2.df)=='percent.unaffordable'] <- 'Affordability.Stress'
names(sa2.df)[names(sa2.df)=='pct.unafford.40'] <- 'Affordability.Stress.Lowest.40'

library(knitr)
kable(sa2.df,digits = 2)