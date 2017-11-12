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
   axis.text.x = element_text(angle = 90),
   text = element_text(size=16),
   legend.key.width = unit(0.06, "npc")
  ) + 
  coord_equal() +
  labs(title = "Shaded level plot of mortality rates in Scotland,\nEngland & Wales, Northern Ireland, and Ireland",
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


#ggsave("figures/slp_scot_neighbours.eps")
#ggsave("figures/slp_scot_neighbours.svg")

ggsave("figures/slp_scot_neighbours.png",
       units = "cm",
       height = 30, width = 20, dpi = 300
       )
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
    expression(paste("Difference")),
    colours = scales::brewer_pal(palette = "RdBu", direction = -1)(11),
    limits = c(-0.5, 0.5)
  ) + 
  theme_dark(
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90),
    text = element_text(size=16),
    legend.key.width = unit(0.07, "npc")
  ) + 
  coord_equal() +
  labs(title = "Comparative Levelplot - Scotland\nand its Neighbours",
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

ggsave("figures/clp_scot_neighbours.png",
       units = "cm",
       height = 30, width = 15, dpi = 300
)

# For other places  -------------------------------------------------------


# Level Plot

# Germany, United States, Norway, Taiwan, Japan

tmp <- dta %>% 
  filter(code %in% c("DEUTE" ,"DEUTW")) %>% 
  filter(sex != "total") %>% 
  group_by(year, age, sex) %>% 
  summarise(
    death_count = death_count[code == "DEUTE"] + death_count[code == "DEUTW"],
    population_count = population_count[code == "DEUTE"] + population_count[code == "DEUTW"],
    exposure = exposure[code == "DEUTE"] + exposure[code == "DEUTW"]
  ) %>% 
  ungroup() %>% 
  mutate(code = "DEUT") %>% 
  select(code, year, age, sex, death_count, population_count, exposure) 

dta %>% 
  bind_rows(tmp) %>% 
  filter(code %in% c("TWN", "JPN", "DEUT","NOR", "USA")) %>% 
  left_join(lookup) %>% 
  mutate(population_name = ifelse(code == "DEUT", "Germany", population_name)) %>% 
  mutate(population_name = ifelse(code == "USA", "United States", population_name)) %>% 
  mutate(population_name = factor(
    population_name,
    ordered = T,
    levels = c("Taiwan", "Japan", "Germany", "Norway", "United States")
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
    axis.text.x = element_text(angle = 90),
    text = element_text(size=16),
    legend.key.width = unit(0.06, "npc")
  ) + 
  coord_equal() +
  labs(title = "Shaded level plot of mortality rates in \nselected additional HMD countries",
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

ggsave("figures/slp_other.png",
       units = "cm",
       height = 30, width = 20, dpi = 300
)

# Selected comparisons 

# Taiwan compared with Japan
# Germany compared with England and Wales
# United States compared with Norway


dta %>% 
  bind_rows(tmp) %>% 
  filter(code %in% c("TWN", "JPN", "DEUT","NOR", "USA", "GBRCENW")) %>%
  filter(sex != "total") %>% 
  group_by(code, year, age, sex) %>% 
  summarise( # duplication in Japan
    death_count = mean(death_count, na.rm = T),
    population_count = mean(population_count, na.rm = T),
    exposure = mean(exposure, na.rm = T)
  ) %>% 
  ungroup %>% 
  mutate(mr = death_count / exposure) %>% 
  mutate(lmr = log(mr, 10)) %>% 
  select(code, year, age, sex, lmr) %>% 
  spread(code, lmr) %>% 
  mutate(
    `Taiwan cf Japan` = TWN - JPN,
    `E&W cf Germany` = GBRCENW - DEUT,
    `USA cf Norway` = USA - NOR
    ) %>% 
  select(year, age, sex, `Taiwan cf Japan`:`USA cf Norway`) %>% 
  gather(key = comparison, value = dif_lmr, `Taiwan cf Japan`:`USA cf Norway`) %>% 
  filter(!is.na(dif_lmr)) -> other_comparisons


other_comparisons %>% 
  filter(age <= 100) %>% 
  filter(year >= 1950) %>% 
  mutate(dif_lmr = case_when(
    dif_lmr < -0.5 ~ -0.5,
    dif_lmr > 0.5  ~ 0.5,
    TRUE ~ dif_lmr
  )
  ) %>% 
  ggplot(aes(x = year, y = age, fill = dif_lmr)) +
  geom_tile() +
  facet_grid(comparison ~ sex) + 
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
    axis.text.x = element_text(angle = 90),
    text = element_text(size=16),
    legend.key.width = unit(0.07, "npc")
  ) + 
  coord_equal() +
  labs(title = "Comparative Levelplot of other populations",
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

ggsave("figures/clp_other.png",
       units = "cm",
       height = 30, width = 15, dpi = 300
)


  
# Same as above but only from 1950 onwards ----


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
  filter(year >= 1950) %>% 
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
    axis.text.x = element_text(angle = 90),
    text = element_text(size=16),
    legend.key.width = unit(0.06, "npc")
  ) + 
  coord_equal() +
  labs(title = "Shaded level plot of mortality rates\nin the British Isles",
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


#ggsave("figures/slp_scot_neighbours.eps")
#ggsave("figures/slp_scot_neighbours.svg")

ggsave("figures/slp_scot_neighbours_recent.png",
       units = "cm",
       height = 30, width = 16, dpi = 300
)
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
      expression(paste("Difference")),
      colours = scales::brewer_pal(palette = "RdBu", direction = -1)(11),
      limits = c(-0.5, 0.5)
    ) + 
    theme_dark(
    ) +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 90),
      text = element_text(size=16),
      legend.key.width = unit(0.07, "npc")
    ) + 
    coord_equal() +
    labs(title = "Comparative Levelplot - Scotland\nand its Neighbours",
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

ggsave("figures/clp_scot_neighbours_recent.png",
       units = "cm",
       height = 30, width = 16, dpi = 300
)

# For other places  -------------------------------------------------------


# Level Plot

# Germany, United States, Norway, Taiwan, Japan

tmp <- dta %>% 
  filter(code %in% c("DEUTE" ,"DEUTW")) %>% 
  filter(sex != "total") %>% 
  group_by(year, age, sex) %>% 
  summarise(
    death_count = death_count[code == "DEUTE"] + death_count[code == "DEUTW"],
    population_count = population_count[code == "DEUTE"] + population_count[code == "DEUTW"],
    exposure = exposure[code == "DEUTE"] + exposure[code == "DEUTW"]
  ) %>% 
  ungroup() %>% 
  mutate(code = "DEUT") %>% 
  select(code, year, age, sex, death_count, population_count, exposure) 

dta %>% 
  bind_rows(tmp) %>% 
  filter(code %in% c("TWN", "JPN", "DEUT","NOR", "USA")) %>% 
  left_join(lookup) %>% 
  mutate(population_name = ifelse(code == "DEUT", "Germany", population_name)) %>% 
  mutate(population_name = ifelse(code == "USA", "United States", population_name)) %>% 
  mutate(population_name = factor(
    population_name,
    ordered = T,
    levels = c("Taiwan", "Japan", "Germany", "Norway", "United States")
  )
  ) %>% 
  filter(sex != "total") %>% 
  filter(age <= 100) %>% 
  filter(year >= 1950) %>% 
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
    axis.text.x = element_text(angle = 90),
    text = element_text(size=16),
    legend.key.width = unit(0.06, "npc")
  ) + 
  coord_equal() +
  labs(title = "Mortality rates in\nvarious HMD countries",
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

ggsave("figures/slp_other_recent.png",
       units = "cm",
       height = 30, width = 16, dpi = 300
)

# Selected comparisons 

# Taiwan compared with Japan
# Germany compared with England and Wales
# United States compared with Norway


dta %>% 
  bind_rows(tmp) %>% 
  filter(code %in% c("TWN", "JPN", "DEUT","NOR", "USA", "GBRCENW")) %>%
  filter(sex != "total") %>% 
  group_by(code, year, age, sex) %>% 
  summarise( # duplication in Japan
    death_count = mean(death_count, na.rm = T),
    population_count = mean(population_count, na.rm = T),
    exposure = mean(exposure, na.rm = T)
  ) %>% 
  ungroup %>% 
  mutate(mr = death_count / exposure) %>% 
  mutate(lmr = log(mr, 10)) %>% 
  select(code, year, age, sex, lmr) %>% 
  spread(code, lmr) %>% 
  mutate(
    `Taiwan cf Japan` = TWN - JPN,
    `E&W cf Germany` = GBRCENW - DEUT,
    `USA cf Norway` = USA - NOR
  ) %>% 
  select(year, age, sex, `Taiwan cf Japan`:`USA cf Norway`) %>% 
  gather(key = comparison, value = dif_lmr, `Taiwan cf Japan`:`USA cf Norway`) %>% 
  filter(!is.na(dif_lmr)) -> other_comparisons


other_comparisons %>% 
  filter(age <= 100) %>% 
  filter(year >= 1950) %>% 
  mutate(dif_lmr = case_when(
    dif_lmr < -0.5 ~ -0.5,
    dif_lmr > 0.5  ~ 0.5,
    TRUE ~ dif_lmr
  )
  ) %>% 
  ggplot(aes(x = year, y = age, fill = dif_lmr)) +
  geom_tile() +
  facet_grid(comparison ~ sex) + 
  scale_fill_gradientn(
    expression(paste("Difference")),
    colours = scales::brewer_pal(palette = "RdBu", direction = -1)(11),
    limits = c(-0.5, 0.5)
  ) + 
  theme_dark(
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90),
    text = element_text(size=16),
    legend.key.width = unit(0.07, "npc")
  ) + 
  coord_equal() +
  labs(title = "Comparative Level Plot\nvarious populations",
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

ggsave("figures/clp_other_recent.png",
       units = "cm",
       height = 30, width = 16, dpi = 300
)



# Legend plot 


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
  filter(year >= 1950) %>% 
  mutate(mr = death_count / exposure) %>% 
  mutate(lmr = log(mr, 10)) %>% 
  ggplot(aes(x = year, y = age, fill = lmr)) +
  scale_x_continuous(
    minor_breaks = seq(1840, 2010, by = 10),
    breaks = seq(1840, 2010, by = 20)
  ) +
  scale_y_continuous(
    minor_breaks = seq(0, 100, by = 10),
    breaks = seq(0, 100, by = 20)
  ) + 
  geom_tile(alpha = 0) +
  scale_fill_gradientn(
    expression(paste(log[10], " mortality rate")),
    colours = scales::brewer_pal(palette = "Paired")(12),
    limits = c(-5, 0)
  ) + 
  theme_dark(
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 90),
    text = element_text(size=16),
    legend.key.width = unit(0.06, "npc")
  ) + 
  coord_equal() +
  geom_abline(
    slope = 1, intercept = seq(-2010, -1850, by = 10),
    colour = "grey42"          
  ) +
  labs(title = "",
       x = "Year", y = "Age in years"
  ) 

ggsave("figures/plain_template.svg")
ggsave("figures/plain_template.eps")
