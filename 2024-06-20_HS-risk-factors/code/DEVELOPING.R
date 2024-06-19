library(tidyverse)

# Read INDO info
hs_inds_desc <-
  read_csv("data/_HS-risk-indicators-info.csv", name_repair = janitor::make_clean_names) %>%
  filter(!is.na(indicator_name)) %>%
  select(indicator_name, indicator_title, indicator_type, indicator_description)


# List of HS data files
hs_data_files <-
  list.files("data/", pattern = "^hs_", full.names = TRUE)


# Read all data
hs_ind_data <- map_df(hs_data_files, read_csv, col_types = cols(.default = "c")) 


# Available data years for each indicator
hs_ind_data %>%
  distinct(indicator_name, YEAR) %>%
  left_join(hs_inds_desc, by = join_by(indicator_name)) %>%
  ggplot(aes(x = YEAR, y = indicator_title)) +
  geom_point(col = "navyblue") +
  labs(title = "Available Data Years",
       x = NULL, 
       y = NULL) +
  scale_y_discrete(position = "right", limits = rev) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = .5)
  )


i = 4
fig <- 
  hs_ind_data %>%
  filter(indicator_name == hs_inds_desc$indicator_name[i],
         FIPS == 19) %>%
  mutate(index = round(as.numeric(index), 2)) %>%
  ggplot(aes(x = YEAR, y = index)) +
  labs(title = hs_inds_desc$indicator_title[i],
       subtitle = hs_inds_desc$indicator_description[i],
       x = NULL) +
  geom_col(fill = "slategray") + 
  theme_light()
if(hs_inds_desc$indicator_type[i] == "percent") {
  fig + scale_y_continuous(labels = scales::percent)
} else {
  fig
}

# County ordered by Ranking
my_df <-
  hs_ind_data %>%
  filter(indicator_name == hs_inds_desc$indicator_name[i]) %>%
  filter(YEAR == max(YEAR)) 
my_df %>%
  filter(FIPS != 19) %>%
  mutate(rank = as.numeric(county_rank),
         index = round(as.numeric(index), 2)) %>%
  ggplot(aes(y = reorder(county_name, rank), x = index)) +
  geom_col(fill = "slategray") + 
  geom_vline(xintercept = 
               my_df %>%
               filter(FIPS == 19) %>%
               pull(index) %>% as.numeric(),
             col = "orange") +
  labs(title = hs_inds_desc$indicator_title[i],
       subtitle = (unique(my_df$YEAR)),
       x = NULL,
       y = NULL) +
  theme_light()


# County's top 10 problems
hs_ind_data %>%
  filter(FIPS == 19019) %>%
  group_by(indicator_name) %>%
  filter(YEAR == max(YEAR)) %>%
  arrange(county_rank) %>%
  head() 


