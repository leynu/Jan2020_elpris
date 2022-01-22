library(tidyverse)
library(readr)
library(here)
library(directlabels)
library(cowplot)
library(magick)
library(lubridate)
library(naniar)
library(stringr)
library(ggrepel)


man_to_month <- tribble(
  ~month, ~manad,
  "January",	"januari",
  "February",	"februari",
  "March",	"mars",
  "April",	"april",
  "May","maj",
  "June",	"juni",
  "July",	"juli",
  "August",	"augusti",
  "September",	"september",
  "October",	"oktober",
  "November",	"november",
  "December",	"december") %>% 
  mutate_all(~str_sub(., 1, 3))


anvisat <- read_delim("data/Rörligt pris-Table 1_1.csv", 
                                      delim = ";", 
                      escape_double = FALSE,  col_types = cols(.default = "c"),
                      trim_ws = TRUE)

names(anvisat)

na_strings <- c(",,", "\\..")


anvisat_wide <- gather(anvisat, manad_ar, varde_orig, `apr 2013`:`Nov 2021`) %>% 
  separate(Typkund, c("Typkund", "Elomrade"), sep = "-") %>% 
  fill(Elomrade) %>% 
  mutate(varde_temp = str_trim(varde_orig)) %>% 
  replace_with_na(replace = list(varde_temp = na_strings)) %>% 
  mutate(Typkund = str_trim(Typkund),
         Elomrade = str_trim(Elomrade),
         temp = str_replace(str_to_lower(manad_ar), "Okt", "oct"),
         varde_temp = str_replace(varde_temp, '\\.\\.', "" ),
         varde = as.numeric(str_replace(varde_temp, ",", "."))
  ) %>% 
  separate(temp, c("manad", "ar")) %>% 
  mutate(manad = str_sub(manad, 1, 3))  %>% 
  left_join(man_to_month) %>% 
  mutate(manad_ar = paste(str_to_title(manad), ar),
         month_year = dmy(paste("01 ", month, ar))) %>% 
  select(Typkund, Elomrade, varde_orig, varde, manad_ar, month_year) %>% 
  filter(Elomrade == "Elområde 3" , !Typkund %in% c("Näringsverksamhet(1)", "Småindustri(1)", "Villa med elvärme")) %>% 
  arrange(Typkund, month_year)
  

anvisat_wide <- anvisat_wide %>% 
  group_by(Typkund) %>% 
  mutate(min_max = ifelse(varde == min(varde, na.rm = T) | varde == max(varde, na.rm = T), varde, NA)) %>% 
  ungroup() 




min_max_df <- anvisat_wide %>% 
  filter(!is.na(min_max))
  
max_x <- unique(min_max_df$month_year)[2]
min_x <- unique(min_max_df$month_year)[1]

data_ends <- anvisat_wide %>% 
  filter(month_year == last(month_year))


table(anvisat_wide$Typkund, useNA = "ifany")

my_background <- "white"

cbbPalette <- c("#ee5935", "#4D4D4D", "#f6b042")

ggplot(anvisat_wide,
       aes(x = month_year, y = varde, group=Typkund, color=Typkund)) +
  # geom_segment(aes(x=max_x, xend=max_x,
  #                  y=0,  yend=122),
  #              col = "#4D4D4D",
  #              size = 0.2) +
  # geom_segment(aes(x=min_x, xend=min_x, 
  #                  y=0,  yend=100), 
  #              col = "#4D4D4D", 
  #              size = 0.5) +
  geom_line(position="identity", size = 1) + 
  geom_dl(data = data_ends, aes(label = sprintf("%0.1f", round(varde, digits = 2))),
          method = list(dl.trans(x = x + .2), "last.points", cex = .9)) +
  # Allow labels to bleed past the canvas boundaries
  coord_cartesian(clip = 'off') +
  annotate(geom="text", x=min_x, y=32, 
           label=expression(paste(~ bold("Jun 2020"))),
           color="#4D4D4D", 
           hjust=0.05, 
           size = 3) +
  annotate(geom="text", x=min_x, y=22, 
           label="Det lägsta \nmedelvärdet",
           color="#4D4D4D", 
           hjust=0, 
           lineheight = .85, 
           size = 3) +
  annotate(geom="text", x=max_x, y=120, 
           label=expression(paste(~ bold("Feb 2019"))),
           color="#4D4D4D", 
           hjust=0.05, 
           size = 3) +
  annotate(geom="text", x=max_x, y=110,
           label="Det högsta \nmedelvärdet",
           color="#4D4D4D",
           hjust=0,
           lineheight = .85,
           size = 3) +
  geom_point(data = data_ends,
             aes(x = month_year, y = varde,  color=factor(Typkund)),
             #shape = 21,
             #fill = "white",
             size = 2.5#,
             #stroke = 1.5
             ) +
  labs(title = "Elenergipriser - Elområde 3",
       subtitle = "vid avtal om rörligt pris",
       caption = "SCB: Elenergipriser, för olika typkunder vid tillsvidareprisavtal, Medelvärden \n@leynu | Jan 2022",
       y="Öre/kWh (exklusive skatter)") +
  theme_bw() + 
  theme(plot.title.position = "plot",
        axis.text.x = element_text(angle = 45),
        plot.margin = margin(0.3, .75, 0.2, 0.2, "cm"),
        plot.title = element_text(#hjust = -0.1, 
                                  size = rel(1.7), 
                                  #lineheight = .9, 
                                  face = "bold",
                                  color = "#4D4D4D"),
        plot.subtitle = element_text(#hjust = -0.4, 
                                     size = rel(1.05), 
                                     #face = "italic",
                                     color = "#4D4D4D"),
        plot.caption = element_text(size = rel(0.55),
                                    color = "#4D4D4D",
                                    hjust=0),
        plot.caption.position =  "plot",
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.border = element_blank(),
        plot.background = element_rect(fill = my_background),
        panel.background = element_rect(fill = my_background),
        legend.position = c(1, 1), 
        legend.justification = c(0.80, 0.075),
        # legend.position='top', 
        # legend.justification='right',
        legend.direction='vertical',
        legend.title = element_blank(),
        legend.text = element_text(color = "#4D4D4D"),
        legend.background = element_rect(fill = my_background),
        legend.key = element_rect(fill = my_background),
        legend.key.size = unit(0.5, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust=1, color = "#4D4D4D"),
        axis.ticks.x = element_blank()) +
scale_x_date(date_labels = "%Y", date_breaks = "1 year",) +
scale_y_continuous(breaks = seq(0, 125, 25), limits = c(0, 125)) +
scale_color_manual(values=cbbPalette)




