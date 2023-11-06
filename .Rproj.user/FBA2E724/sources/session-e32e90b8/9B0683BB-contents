library(readxl)
library(tidyverse)
library(tidyverse)
library(haven)
library(psych)
library(sjmisc)
library(dplyr)
library(ggpubr)

data <- read_xlsx("1_raw_data/2023-11-03_tswift_spotify.xlsx")
str(data)
range(data$popularity)

# Filter to only include albums that have been re-released

data_sub <- data%>% 
  filter(grepl("Fearless|Red|Speak Now|1989", album))

data_sub <- data %>% 
  filter(album == "Speak Now" | album == "Speak Now (Taylor's Version)" |
           album == "Fearless" | album == "Fearless (Taylor's Version)" |
           album == "Red" | album == "Red (Taylor's Version)" |
           album == "1989" | album == "1989 (Taylor's Version)")

# Group by album and calculate the average popularity
data_sub <- data_sub %>%
  group_by(album) %>%
  summarize(average_popularity = mean(popularity))

data_sub <- data_sub %>%
  mutate(tv_status = case_when(
    grepl("Taylor's Version", album) ~ 2,
    TRUE ~ 1  # Default value for other cases
  ))


data_sub <- data_sub %>% 
  mutate(album_rec = case_when(album == "Red" | album == "Red (Taylor's Version)" ~ "Red",
                               album == "Speak Now" | album == "Speak Now (Taylor's Version)" ~ "Speak Now",
                               album == "Fearless" | album == "Fearless (Taylor's Version)" ~ "Fearless",
                               album == "1989" | album == "1989 (Taylor's Version)" ~ "1989"))

# Reorder TV factor
new_order <- c("Fearless", "Speak Now", "Red", "1989")
data_sub$album_rec_reorder <- relevel(data_sub$album_rec, ref = new_order)




# data_sub$tv_status <- factor(data_sub$tv_status)
# ggplot(data_sub, aes(fill=tv_status, y=average_popularity, x=album_rec)) + 
#   geom_bar(position="stack", stat="identity")

ggplot(data_sub, aes(fill=tv_status, y=average_popularity, x=album_rec)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylim(0, 100) + 
  theme_classic()
  

ggplot(data_sub, aes(fill=album, y=average_popularity, x=album)) +
  geom_bar(position="dodge", stat="identity") +
  ylim(0, 100) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none", text = element_text(family = "sans", size = 20), axis.title = element_text(size = 25)) +
  labs(title = "\n Spotify Popularity of Original Albums vs Taylor's Version", y = "\n Popularity", x = "\n Album") +
  scale_fill_manual(values = c("#3281B5", "#93CDDF", "#E2A641", "#EDD685", "#97373D", "#BB777A", "#6A62A1", "#ADAAC8"))







data$tv_status <- factor(data$tv_status)
str(data$tv_status)

gghistogram(data_sub, x = "popularity", y = "count", fill = "tv_status")


# Stacked barplot with multiple groups
ggplot(data=data_sub, aes(x=album, y=average_popularity, fill=album)) +
  geom_bar(stat="identity")
# Use position=position_dodge()
ggplot(data=data_sub, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", position=position_dodge())
























## ---- Interactive 
ggplotly(plot) 

## ---- Grouped Scatter plot with density plots
library(ggpubr)
plot <- ggscatterhist(data, x = "release_date", y = "popularity",  
              color = "tv_status", size = 3, alpha = 0.6,  palette = c("#E7B800", "#00AFBB"),  margin.params = list(fill = "tv_status", color = "black", size = 0.4))

library(ggthemes)
library(plotly)
(scatter_plot<-data %>% 
    ggplot(aes( x = release_date, y = popularity, color = as_factor(tv_status))) +     # describe how data should be mapped to features of the plot
    geom_point(position = "jitter", alpha=0.8) +                                               #transparency of the dots; jitter adds noise to data so the dots aren't stacked on top of each other.
    labs(x="Social Anxiety Year 1", y= "Social Anxiety Year 4", # labels
         title="The Association between Social Anxiety at Year 1 and 4 by Sex") +               # title
    geom_smooth(method=lm, se=TRUE)+                                                           #add regression line without se
    #facet_wrap(~as_factor(aa_A3_Sex))+                                                        #create separate plots for sex
    geom_rangeframe(color= "black") +                                                           #add outline around plots in black
    theme_tufte(base_size = 16) +                                                              # add a theme https://ggplot2.tidyverse.org/reference/ggtheme.html
    labs(color="Gender")+ #legend title
    scale_color_manual(values = c("#993F00", "steelblue")))                                    #https://r-charts.com/color-palettes/






library(ggplot2)
library(plotly)

library(ggplot2)
library(plotly)

# Check unique values in the tv_status column
unique(data$tv_status)

# If there are unexpected values, remap them to valid colors or factors
data$tv_status <- ifelse(data$tv_status %in% c("ValidValue1", "ValidValue2"), data$tv_status, "Other")

# Create the ggplot2 plot
p <- ggplot(data, aes(x = release_date, y = popularity, text = name)) +
  geom_point(aes(color = tv_status, size = 3, alpha = 0.6), show.legend = TRUE, palette = c("#E7B800", "#00AFBB")) +
  scale_alpha(range = c(0.2, 0.8)) +
  labs(x = "Release Date", y = "Popularity") +
  theme_minimal() +
  theme(legend.position = "top") +
  geom_point(aes(fill = tv_status, color = "black"), size = 0.4) +
  scale_fill_identity() +
  scale_color_identity() +
  theme(legend.title = element_blank())

# Convert ggplot2 plot to Plotly
p <- ggplotly(p, tooltip = "text")

# Display the interactive Plotly plot
p

