# Produce lexis surfaces and comparative level plots 

rm(list = ls())

pacman::p_load(
  tidyverse,
  scales,
  stringr
)

dta <- read_csv("data/tidied/hmd_tidied.csv")
lookup <- read_csv("data/tidied/code_lookup.csv")


# SLP Scotland, 
# SLP England & Wales,
# SLP NI
# SLP Ireland 

# CLP Scotland - England & Wales
# CLP Scotland - NI
# CLP Scotland - Ireland 


dta %>% 
  filter(code %in% c("GBR_SCO", "GBRCENW", "GBR_NIR", "IRL")) %>% 
  left_join(lookup) %>% 
  mutate(population_name = factor(
    population_name,
    ordered = T,
    levels = c("Scotland", "England and Wales", "Northern Ireland", "Ireland")
    )
  ) %>% 
  filter(sex != "total") %>% 
  filter(age <= 100) %>% 
  mutate(mr = death_count / exposure) %>% 
  mutate(lmr = log(mr, 10)) %>% 
  ggplot(aes(x = year, y = age, fill = lmr)) +
  geom_tile() +
  facet_grid(population_name ~ sex) + 
  scale_fill_gradientn(
    expression(paste(log[10], " mortality rate")),
    colours = scales::brewer_pal(palette = "Paired")(12),
    limits = c(-5, 0)
  ) + 
  theme_dark(
  ) +
  theme(
   strip.text = element_text(face = "bold"),
   legend.position = "bottom",
   axis.text.x = element_text(angle = 90)
  ) + 
  coord_equal() +
  labs(title = "Shaded level plot of mortality rates in Scotland,\nEngland & Wales, Northern Ireland, and the Republic of Ireland",
       x = "Year", y = "Age in years",
       subtitle = "Source: Human Mortality Database") +
  scale_x_continuous(
    minor_breaks = seq(1840, 2010, by = 10),
    breaks = seq(1840, 2010, by = 20)
  ) +
  scale_y_continuous(
    minor_breaks = seq(0, 100, by = 10),
    breaks = seq(0, 100, by = 20)
  )


ggsave("figures/slp_scot_neighbours.eps")
ggsave("figures/slp_scot_neighbours.svg")


# Now comparative level plots 



dta %>% 
  filter(code %in% c("GBR_SCO", "GBRCENW", "GBR_NIR", "IRL")) %>% 
  left_join(lookup) %>% 
  mutate(population_name = factor(
    population_name,
    ordered = T,
    levels = c("Scotland", "England and Wales", "Northern Ireland", "Ireland")
  )
  ) %>% 
  filter(sex != "total") %>% 
  filter(age <= 100) %>% 
  mutate(mr = death_count / exposure) %>% 
  mutate(lmr = log(mr, 10)) %>% 
  select(place =  population_name, year, age, sex, lmr) %>% 
  spread(place, lmr) %>% 
  mutate(`cf England and Wales` = `Scotland` - `England and Wales`) %>% 
  mutate(`cf Northern Ireland` = `Scotland` - `Northern Ireland`) %>% 
  mutate(`cf Ireland` = `Scotland` - `Ireland`) %>% 
  filter(year >= 1950) %>% 
  select(year, age, sex, `cf England and Wales`, `cf Northern Ireland`, `cf Ireland`) %>% 
  gather(key = place, value = dif_lmr, `cf England and Wales`:`cf Ireland`) -> dta_comparative_scot


dta_comparative_scot %>% 
  mutate(dif_lmr = case_when(
      dif_lmr < -0.5 ~ -0.5,
      dif_lmr > 0.5  ~ 0.5,
      TRUE ~ dif_lmr
    )
  ) %>% 
  ggplot(aes(x = year, y = age, fill = dif_lmr)) +
  geom_tile() +
  facet_grid(place ~ sex) + 
  scale_fill_gradientn(
    expression(paste("Difference in ", log[10], " mortality rate")),
    colours = scales::brewer_pal(palette = "RdBu", direction = -1)(11),
    limits = c(-0.5, 0.5)
  ) + 
  theme_dark(
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90)
  ) + 
  coord_equal() +
  labs(title = "Comparative Levelplot of Scottish Mortality compared with,\nEngland & Wales, Northern Ireland, and the Republic of Ireland",
       x = "Year", y = "Age in years",
       subtitle = "Source: Human Mortality Database") +
  scale_x_continuous(
    minor_breaks = seq(1950, 2010, by = 10),
    breaks = seq(1960, 2010, by = 20)
  ) +
  scale_y_continuous(
    minor_breaks = seq(0, 100, by = 10),
    breaks = seq(0, 100, by = 20)
  )



