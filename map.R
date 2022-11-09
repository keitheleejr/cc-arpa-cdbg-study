library(tidyverse)
library(showtext)

font_add_google("Merriweather", family = "merriweather")
showtext_auto()


ga_covid_cases <- 
  read_csv("covid-cases.csv")  %>%
  filter(county_name != "Non-GA Resident/Unknown State" & 
           county_name != "Unknown") %>% 
  rename(county = county_name,
         hospitalization = confirmed_case_hospitalization) %>%
  select(county, population, cases, hospitalization, deaths) %>%
  mutate(county = tolower(county),
         case_rate = cases / population * 100,
         hosp_rate = hospitalization / population *100,
         death_rate = deaths / population * 100,
         across(where(is.numeric), ~ round(., 2)),
         case_rate_bins = cut(case_rate, breaks = c(0,15,30,45,65)))

ga_counties <- map_data("county") %>% 
  filter(region == "georgia") %>% 
  rename(county = subregion) %>% 
  mutate(county = ifelse(
    county == "de kalb", "dekalb", county))

# Alternatively, could use case_when() function. Particularly useful if
# more than one case. 
# mutate(county = case_when(
#   county == "de kalb" ~ "dekalb",
#   TRUE ~ county)
# ))

anti_join(ga_covid_cases, ga_counties,
          by = "county")

anti_join(ga_counties, ga_covid_cases,
          by = "county")

map_data <- inner_join(ga_covid_cases, ga_counties,
           by = "county")

outliers_rm <- map_data %>% 
  mutate(case_rate = na_if(case_rate, 63.48),
         case_rate = na_if(case_rate, 41.26))

ggplot(outliers_rm, aes(x = long, 
                     y = lat, 
                     fill = case_rate, 
                     group = group)) +
  geom_polygon(color = "black") + 
  coord_map() +
  theme_void() +
  scale_fill_gradient(low="white", high="red", na.value = "grey75") +
  labs(fill = "Cases per capita",
       title = "COVID-19 by County",
       caption = "Data Source: Georgia Department of Public Health, 11/7/22") +
  theme(legend.position = "right",
        plot.title = element_text(family = "merriweather",
                                  size = 30),
        plot.caption = element_text(hjust = -1,
                                    family = "merriweather",
                                    size = 14),
        legend.title = element_text(family = "merriweather",
                                    size = 14))
ggsave("map.png")  

