library(readxl)
library(tidyverse)
library(tidyverse)
library(haven)
library(psych)
library(sjmisc)
library(dplyr)
library(ggpubr)

data <- read_xlsx("1_raw_data/2023-12-05_tswift_spotify.xlsx")
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

# Create album_rec variable

data_sub <- data_sub %>% 
  mutate(album_rec = case_when(album == "Red" | album == "Red (Taylor's Version)" ~ "Red",
                               album == "Speak Now" | album == "Speak Now (Taylor's Version)" ~ "Speak Now",
                               album == "Fearless" | album == "Fearless (Taylor's Version)" ~ "Fearless",
                               album == "1989" | album == "1989 (Taylor's Version)" ~ "1989"))

# Reorder album_rec for plots

new_order <- c("Fearless", "Speak Now", "Red", "1989")
data_sub$album_rec <- factor(data_sub$album_rec, levels = new_order)

# Reorder album for plots

new_order2 <- c("1989", "1989 (Taylor's Version)", "Red", "Red (Taylor's Version)", 
                "Speak Now", "Speak Now (Taylor's Version)", "Fearless", "Fearless (Taylor's Version)")
data_sub$album <- factor(data_sub$album, levels = new_order2)

# Set tv_status as a factor

data_sub$tv_status <- factor(data_sub$tv_status, labels = c("Original Version", "Taylor's Version"))


# Bar plot 1

ggplot(data_sub, aes(fill=tv_status, y=average_popularity, x=album_rec)) + 
  geom_bar(position="dodge", stat="identity") + 
  ylim(0, 100) + 
  theme_classic() +
  theme(legend.position = "right", 
        text = element_text(family = "sans", size = 20), 
        axis.title = element_text(size = 16), 
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust= 0.5, face = "bold")) +
  labs(title = "\n Spotify Popularity of Original Albums vs Taylor's Version", y = "\n Popularity", x = "\n Album", fill = "Album Version") +
  scale_fill_manual(values = c("#B0B0B0", "#5C5C5C"))

# Bar plot 2

ggplot(data_sub, aes(fill=album, y=average_popularity, x=album)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label = round(average_popularity)),
            position = position_stack(vjust = 1, reverse = TRUE),
            size = 7, hjust = 1.3, color = "white") +
  ylim(0, 100) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none", 
        text = element_text(family = "sans", size = 20), 
        axis.title.y = element_text(size = 16),  # Modify y-axis title appearance if needed
        axis.text.y = element_text(color = "black"),  # Modify y-axis text appearance if needed
        axis.ticks = element_blank(),  # Keep y-axis ticks
        axis.line.y = element_blank(),  # Keep y-axis line
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_blank(),   # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        axis.line.x = element_blank(),   # Remove x-axis line
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  labs(title = "\n Spotify Popularity of Original Albums vs Taylor's Version", y = "\n Popularity", x = "") +
  scale_fill_manual(values = c("#93CDDF","#3281B5","#BB777A", "#97373D","#ADAAC8","#6A62A1","#EDD685","#E2A641")) +
  theme(axis.text.y = element_text(margin = margin(r = -30)))

# Combine plots

# library(ggpubr)
# fullplot <- ggarrange(plot1, plot2, nrow = 1, ncol = 2)
# annotate_figure(fullplot, top = text_grob("Spotify Popularity of Original Albums vs Taylor's Version", color = "black", face = "bold", size = "25"))






