library(readr)
library(lubridate)

## Read data
data <- read_csv(
  file = "./Data/Whamageddon.csv",
  col_types = cols( 
    ID = col_character(),
    When = col_datetime(format = "%d-%m-%Y %H:%M"),
    Hit = col_integer(),
    Comments = col_character()
  )
)
# Data cleaning
data_complete <- data %>%
  dplyr::arrange(
    When
  ) %>% 
  tidyr::complete(
    Who,
    When = c(
      seq(
        from = ymd_hm("2020-12-01 00:00"),
        to = ymd_hm("2020-12-24 23:00"), 
        by = "hour"
      ),
      seq(
        from = ymd_hm("2021-12-01 00:00"), 
        to = ymd_hm("2021-12-24 23:00"), 
        by = "hour"
      ),
      seq(
        from = ymd_hm("2022-12-01 00:00"),
        to = ymd_hm("2022-12-24 23:00"),
        by = "hour"
      )
    )
  ) %>% 
  tidyr::replace_na(
    list(Hit =  0)
  ) %>% 
  dplyr::select(
    c(When, Who, Hit)
  ) %>% 
  dplyr::mutate(
    Year = lubridate::year(When)
  )
## Cumulative counts
data_cumulative <- data_complete %>%
  group_by(
    Year, When, Who
  ) %>% 
  summarise(
    Hit = sum(Hit)
  ) %>% 
  group_by(
    Year, Who
  ) %>% 
  mutate(
    `Wham Count` = dplyr::case_when(
      When <= lubridate::now() ~ cumsum(Hit)
    )
  )
## Plot
ggplot(
  data = data_cumulative, 
  aes(
    x = When, 
    y = `Wham Count`
  )
) + 
  geom_step(
    lwd = 1.5,
    aes(
      colour = Who
    )
  ) + 
  scale_colour_brewer(
    palette = "Set1"
  ) +
  scale_x_datetime(
    "", 
    date_breaks = "2 days",
    date_labels = "%b-%d", 
    expand = c(0.02,0.02)
  ) +
  scale_y_continuous(
    "Wham count", 
    limits = c(0,16), 
    breaks = seq(0,16,2)
  ) +
  facet_wrap(
    ~Year, 
    scales = "free_x", 
    ncol = 3
  ) +
  labs(
    title = "Wham counts per year"
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.15, 0.85),
    legend.direction = "horizontal",
    legend.background  = element_rect(color = "black", size = 0.75),
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title.y = element_text(vjust = -2.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
     #plot.background = element_rect(fill = "grey50")
  )
# WHAM Awards
## Longest period
data_cumulative %>% 
  filter(
    `Wham Count` == 0
  ) %>% 
  count(
    Year, Who, 
    name = "Hours"
  ) %>% 
  group_by(
    Who
  ) %>% 
  slice_max(
    order_by = Hours
  ) %>% 
  arrange(
    -Hours
  ) %>% 
  mutate(
    days = round(Hours / 24, 1)
  )
