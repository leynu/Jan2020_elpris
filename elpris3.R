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
library(hrbrthemes)
library(ggthemes)
library(extrafont)
library(ggtext)

# load patchwork
library(patchwork)
# set ggplot theme
theme_set(theme_bw(base_family = "Roboto Condensed"))

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

ett_ar <- read_delim("data/1-års avtal-Table 1_1.csv", 
                      delim = ";", 
                      escape_double = FALSE,  col_types = cols(.default = "c"),
                      trim_ws = TRUE) %>% 
  mutate(avtal = "1-års avtal")

tva_ar <- read_delim("data/2-års avtal-Table 1_1.csv", 
                     delim = ";", 
                     escape_double = FALSE,  col_types = cols(.default = "c"),
                     trim_ws = TRUE) %>% 
  mutate(avtal = "2-års avtal")


tre_ar <- read_delim("data/3-års avtal-Table 1_1.csv", 
                     delim = ";", 
                     escape_double = FALSE,  col_types = cols(.default = "c"),
                     trim_ws = TRUE) %>% 
  mutate(avtal = "3-års avtal")




anvisat <- read_delim("data/Anvisat avtal-Table 1_1.csv", 
                      delim = ";", 
                      escape_double = FALSE,  col_types = cols(.default = "c"),
                      trim_ws = TRUE) %>% 
  mutate(avtal = "Anvisat avtal")

rorligt <- read_delim("data/Rörligt pris-Table 1_1.csv", 
                      delim = ";", 
                      escape_double = FALSE,  col_types = cols(.default = "c"),
                      trim_ws = TRUE)%>% 
  mutate(avtal = "Rörligt pris")

elpris <- bind_rows(anvisat, rorligt, ett_ar, tva_ar, tre_ar)



na_strings <- c(",,", "\\..")


elpris_wide <- gather(elpris, manad_ar, varde_orig, `apr 2013`:`Nov 2021`) %>% 
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
  select(avtal, Typkund, Elomrade, varde_orig, varde, manad_ar, month_year) %>% 
  filter(Elomrade == "Elområde 3" , 
         !Typkund %in% c("Näringsverksamhet(1)", "Småindustri(1)", "Villa med elvärme"),
         Typkund == "Lägenhet") %>% 
  arrange(Typkund, month_year)
  


max_x <- max(unique(elpris_wide$month_year))
min_x <- min(unique(elpris_wide$month_year))


data_ends <- elpris_wide %>% 
  filter(month_year == last(month_year))

# Some parameters ----
my_background <- "#ECECEC"
cbbPalette <- c("#ebc845", "#ef6c38","#c02e27", "#113c55", "#3a95ba")

# Just so I can hide the 2022 value on the x axis
x_label_color <- c(rep("#4D4D4D", length(unique(year(elpris_wide$month_year)))), "#ECECEC")

cap_1 <- "<span style = 'font-size:7pt; font-family:Helvetica;'>Ett <b>anvisat pris</b> får du om du inte gjort ett aktivt val av elavtal. Anvisningspris är den dyraste avtalsformen på elmarknaden. Det förekommer även att<br>
 du får ett anvisat pris när ditt elavtal har löpt ut och du inte gjort något nytt val. Det anvisade avtalet gäller tillsvidare med <b>14 dagars</b> uppsägningstid. <br> 
       @leynu | Jan 2022 | <i>Källa: SCB</i>
       </span>" 

# Image ----

p1 <- ggplot(elpris_wide,
       aes(x = month_year, y = varde, group=avtal, color=avtal)) +
  geom_line(position="identity", size = 1) + 
  geom_dl(data = data_ends, aes(label = sprintf("%0.1f", round(varde, digits = 2))),
          method = list(dl.trans(x = x + .2), "last.points", cex = .9)) +
  # Allow labels to bleed past the canvas boundaries
  coord_cartesian(clip = 'off', ylim = c(0, 125), 
                  xlim = c(min_x, max_x)) +
  geom_point(data = data_ends,
             aes(x = month_year, y = varde,  color=factor(avtal)),
             size = 2.5
             ) +
  labs(title = "Elenergipriser",
       subtitle = "Medelvärden för de olika avtalstyper \nApril, 2013 - November, 2021",
       caption = cap_1,
       y="Öre/kWh (exklusive nätavgift)"
       ) +
  theme(plot.title.position = "plot",
        plot.margin = margin(0.3, 0.45, 0.2, 0.2, "cm"),
        plot.title = element_text(size = rel(3.1),
                                  face = "bold",
                                  color = "#4D4D4D"),
        plot.subtitle = element_text(size = rel(1),
                                     color = "#4D4D4D"),
        plot.caption = ggtext::element_markdown(color = "#4D4D4D",
                                                hjust=0
                                                ),
        plot.caption.position =  "plot",
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.border = element_blank(),
        plot.background = element_rect(fill=my_background, 
                                       color=my_background),
        panel.background = element_rect(fill = my_background),
        legend.position='bottom',
        legend.title = element_blank(),
        legend.text = element_text(color = "#4D4D4D"),
        legend.background = element_rect(fill = my_background),
        legend.key = element_rect(fill = my_background),
        legend.key.size = unit(0.5, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(hjust=0.9, 
                                    color = "#4D4D4D", 
                                    size =10),
        axis.text.x = element_text(hjust=-1.1,
                                   color=x_label_color),
        axis.ticks.length=unit(.5, "cm"),
        axis.ticks.y = element_blank()
  ) +
scale_x_date(date_labels = "%Y", 
             date_breaks = "1 year") +
scale_y_continuous(breaks = seq(0, 125, 25)) +
scale_color_manual(values=cbbPalette) 

p2 <- ggplot() + 
  geom_segment(aes(x = 0, xend = 0, 
                   y = 0, yend = 125),
               size = 0.85,
               colour = my_background) +
  geom_text(aes(x = 0.30, y = c(0, 50, 100), 
                label = c("Årsförbrukning", "Typkund", "Elområde")),
            color = "#4D4D4D", 
            size = 3.25,
            hjust = 0.5,
            vjust = 0)+
  geom_text(aes(x = 0.30, y = c(10, 60), 
                label = c("2 000 kWh", "Lägenhet")),
            fontface = "bold",
            color = "#4D4D4D", 
            size = 5,
            hjust = 0.5,
            vjust = 0)+
  geom_text(aes(x = 0.30, y = 110, 
                label = "3"),
            fontface = "bold",
            color = "#4D4D4D", 
            size = 14,
            hjust = 0.5,
            vjust = 0)+
  theme(plot.background = element_rect(fill=my_background, 
                                       color=my_background),
        panel.background = element_rect(fill = my_background),
        plot.margin = margin(0, 0.05, 0, 0, "cm"),
        panel.grid.minor=element_blank(), 
        panel.grid.major=element_blank(), 
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  ) +
  scale_x_continuous(limits = c(0, .6))        
p2  

# two plots side by side
p <- p1 + p2 +
  plot_layout(widths = c(6, 1))

ggdraw() +
  draw_plot(p) +
  draw_image(file.path(here("elomrade.png")),
             x = -0.09, y = 0.3, scale = .35)



ggsave("~/Desktop/elpris3.png",
       width=2400/300, 
       height =1600/300)


