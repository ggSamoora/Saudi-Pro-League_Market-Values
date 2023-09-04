# load libraries
library(tidyverse)
library(gghighlight)
library(ggimage)
library(cowplot)
library(svglite)

# Saudi's top 4 clubs
pif_teams <- c("Al-Hilal SFC", "Al-Nassr FC", "Al-Ahli SFC", "Al-Ittihad SFC")

# setting the colors for each of the 4 clubs
al_hilal_hex <- "#0028f1"
al_nassr_hex <- "#ffcf2d"
al_ahli_hex <- "#1f7e5b"
al_ittihad_hex <- "#080705"

# adding a custom font
sysfonts::font_add_google(name = "Amatic SC", family = "amatic-sc")
showtext::showtext_auto()

# loading and cleaning the data
df <- read_csv("data/SPL Market Value.csv") %>%
  mutate(club = str_replace(club, "Ittihad Club", "Al-Ittihad SFC"))

df <- df %>% 
  # remove blank market values
  filter(is.na(market_value)==FALSE) %>% 
  # create new columns
  mutate(period = dmy(period), # creates a date column
         month = month(period, label = T), # extracts month from date
         year = year(period), # extracts year from date
         floor_date = floor_date(period, "month"), #
         value_actual = if_else(str_detect(market_value, "m")==T,
                                as.numeric(str_remove(market_value, "m")),
                                as.numeric(str_remove(market_value, "k"))/1000),
         PIF = if_else(club %in% pif_teams, "PIF", "Non-PIF"))

# set the image settings to save the plot
png("saudi_plot.png", width = 10, height = 6, res = 300, units = "in")

# ggplot code
# we'll start by filtering the data, grouping it, summarizing, then ungrouping
df %>% 
  filter(floor_date >= "2022-07-01") %>% 
  group_by(floor_date, club) %>% 
  summarise(mv = mean(value_actual, na.rm = T)) %>% 
  ungroup() %>% 
  # ggplot code begins
  ggplot(aes(x=floor_date,y=mv,color=club)) + 
  theme_classic() + 
  geom_line(linewidth=1.25, alpha=0.75) + 
  labs(
    x = 'Period',
    y = 'Market Value (in Millions)',
    title = "Saudi Arabia's Massive Football Investment in Numbers",
    subtitle = "The Public Investment Fund (PIF) invested in Saudi's top 4 football clubs in June 2023.
This investment has allowed these clubs to invest in top-level players from European clubs,
hence drastically increasing each of their market values. Within one year, each club has experienced an
an average increase in market value by 378%.",
    caption = "Source: Transfermarkt\n\nAuthor: Samer Hijjazi"
  ) + 
  scale_y_continuous(
    labels = scales::dollar_format(prefix="€", suffix = "M")
  ) + 
  gghighlight(
    club %in% pif_teams,
    use_direct_label = F,
    use_group_by = F,
    unhighlighted_params = list(color = 'grey90', linewidth = 0.5)
  ) +
  scale_color_manual(
    values = c("Al-Ahli SFC" = al_ahli_hex,
               "Al-Hilal SFC" = al_hilal_hex,
               "Al-Nassr FC" = al_nassr_hex,
               "Al-Ittihad SFC" = al_ittihad_hex)
  ) + 
  annotate(
    'segment',
    x = as.Date("2023-01-01"),
    xend = as.Date("2023-01-01"),
    y = 0,
    yend = 75,
    linetype = 2,
    linewidth = 0.3,
    color = 'red'
  ) + 
  annotate(
    'segment',
    x = as.Date("2023-06-01"),
    xend = as.Date("2023-06-01"),
    y = 0,
    yend = 158,
    linetype = 2,
    linewidth = 0.3,
    color = 'red'
  ) + 
  geom_image(
    aes(image = "images/ronaldo.png",
        x = as.Date("2023-01-01"),
        y = 110),
    inherit.aes = FALSE,
    size = 0.11
  ) + 
  annotate(
    'text',
    x = as.Date("2023-01-01"),
    y = 140,
    size = 3,
    label = "Cristiano Ronaldo\nJoins Al-Nassr",
    color = "red",
    hjust = 0.5
  ) + 
  geom_image(
    aes(image = "images/pif_logo.png",
        x = as.Date("2023-06-01"),
        y = 180),
    inherit.aes = FALSE,
    size = 0.11
  ) + 
  annotate(
    'text',
    x = as.Date("2023-06-01"),
    y = 210,
    size = 3,
    #family = 'Source Sans Pro',
    label = "PIF Investment\nAnnounced",
    color = "red",
    hjust = 0.5
  ) + 
  annotate(
    'text',
    x = as.Date("2023-09-25"),
    y = c(260,210,185,119),
    size = 3.5,
    label = c("€260M","€210M","€185M","€119M"),
    color = c(al_hilal_hex, al_nassr_hex, al_ahli_hex, al_ittihad_hex),
    fontface = 'bold',
    hjust = 0.5
  ) + 
  annotate(
    'text',
    x = as.Date("2022-06-10"),
    y = c(65,50,35,20),
    size = 3.5,
    label = c("€61M","€48M","€29M","€24M"),
    color = c(al_hilal_hex, al_nassr_hex, al_ittihad_hex, al_ahli_hex),
    fontface = 'bold',
    hjust = 0.5
  ) + 
  annotate("segment", 
           x = as.Date("2023-09-10"),
           xend = as.Date("2023-09-10"),
           y = 12, 
           yend = 46,
           arrow = arrow(ends = "both", length = unit(.2,"cm"))) +
  annotate(
    'text',
    x = as.Date("2023-10-25"),
    y = 30,
    size = 2.5,
    label = "All Other Saudi Clubs",
    fontface = 'bold'
  ) + 
  coord_cartesian(ylim = c(0,265),
                  xlim = c(as.Date("2022-06-01"), as.Date("2023-12-01"))) + 
  theme(
    legend.position = 'bottom',
    axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 8),
    plot.subtitle = element_text(size = 10),
    plot.title = element_text(size=18,
                              face = "bold",
                              margin(t = 0, r = 0, b = 5, l = 0))
  ) + 
  scale_x_date(date_labels = "%b %Y")

dev.off()
