library(tidyverse)
library(ggsci)
library(ggthemes)
library(DataExplorer)
library(ggrepel)
library(ggimage)
library(extrafont)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

summary_coffee <- coffee_ratings %>% group_by(country_of_origin) %>%
  summarise(count=n(), alt=mean(altitude_mean_meters, na.rm=T), 
            points = mean(total_cup_points)) %>% arrange(desc(points)) %>%
  filter(count > 5) %>% mutate(image="https://static.thenounproject.com/png/120176-200.png")

ggplot(summary_coffee, aes(alt, points, size=count, label=country_of_origin)) + 
  geom_image(aes(image=image), size=0.03, by="height", alpha=0.5) + 
  geom_text_repel(size=4, vjust=2.25, family = "Georgia") +
  theme_few() +
  labs(title="The best coffee is from Ethopia. But drink Nicaraguan if you want to get high.", 
       x="Mean altitude", y= "Mean coffee score",
       caption="Source: Coffee Quality Institute / Buzzfeed") +
  theme(plot.title = element_text(family="DIN Condensed Bold", size=25),
        text = element_text(family="Georgia"))
ggsave("gethigh.png", dpi="retina")

filt.coff <- summary_coffee %>% 
    filter(count>37) %>% select(country_of_origin)

medians <- coffee_ratings %>% group_by(country_of_origin) %>%
  summarise(median_alt=median(altitude_mean_meters, na.rm=TRUE),
            median_score=median(total_cup_points, na.rm=TRUE))

coffee_ratings <- coffee_ratings %>% 
  left_join(medians, by="country_of_origin")

ggplot(coffee_ratings %>% filter(country_of_origin %in% filt.coff$country_of_origin,
                                 country_of_origin != "United States (Hawaii)",
                                 total_cup_points>50, altitude_mean_meters < 10000), 
       aes(altitude_mean_meters, total_cup_points, col=country_of_origin,
           label=company)) +
  geom_jitter(alpha=0.3, width=50, size=3) +
  geom_rug(alpha=0.5) +
  facet_wrap(~country_of_origin) + 
  geom_hline(aes(yintercept=median_score), alpha=0.1) +
  geom_vline(aes(xintercept=median_alt), alpha=0.1) +
   theme_few() + scale_color_lancet() +
  labs(title="High on coffee", 
       caption="Source: Coffee Quality Institute / Buzzfeed",
       subtitle="Bean altitude and coffee ratings by country (minimum 40 bean types sampled)", x="Altitude", y= "Coffee score", col="Country of origin") +
  theme(plot.title = element_text(family="DIN Condensed Bold", size=25),
        text = element_text(family="Georgia", size=16),
        legend.position = "none")
ggsave("high.png", dpi="retina")

