library(tidyverse)

us_states <- map_data("state")
us_iowa <- map_data("state", 'iowa')
us_counties <- map_data("county", "iowa")


us_states %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(color = "grey95", size = 0.5, fill = "white", alpha = 0.1) +
  geom_polygon(data = us_iowa, color = "#1cade4", size = 1.2, fill = "white",
               alpha = 0.9) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45, clip = "off",
            xlim = c(-106, -82), ylim = c(42, 48)) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())
ggsave("images/iowa.png", width = 13.3, height = 7.5)



us_states %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(color = "grey95", size = 0.5, fill = "white", alpha = 0.1) +
  geom_polygon(data = us_counties, color = "#13698a", size = 0.75, fill = "white") +
  geom_polygon(data = us_iowa, color = "#1cade4", size = 1.2, fill = "white",
               alpha = 0.9) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45, clip = "off",
            xlim = c(-106, -82), ylim = c(42, 48)) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())
ggsave("images/iowa_county.png", width = 13.3, height = 7.5)




# Figures zoomed to Iowa

us_states %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(color = "grey95", size = 0.5, fill = "white", alpha = 0.1) +
  geom_polygon(data = us_iowa, color = "lightblue1", size = 1, fill = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45, clip = "off",
            xlim = c(-98, -89), ylim = c(42, 42.1)) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())
ggsave("images/iowa2.png", width = 10, height = 6)



us_states %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(color = "grey95", size = 0.5, fill = "white", alpha = 0.1) +
  geom_polygon(data = us_counties, color = "aliceblue", size = 0.75, fill = "white") +
  geom_polygon(data = us_iowa, color = "lightblue1", size = 1, fill = NA) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45, clip = "off",
            xlim = c(-98, -89), ylim = c(42, 42.1)) +
  theme(axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())
ggsave("images/iowa_county2.png", width = 10, height = 6)

