# Google Trends Data for CCA Project
# William Christiansen, Ph.D.
# 3/15/24

# install and load packages
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages("corrplot")
library(corrplot)


# load data
geo_dat <- read.csv("geoMap.csv")
time_dat <- read.csv("multiTimeline.csv")

# inspect structure of each data frame
str(geo_dat)
str(time_dat)

# reshape and clean geo dat
# Assuming geo_dat is your geographic data frame
geo_dat_long <- geo_dat %>%
  pivot_longer(cols = -Region, names_to = "Keyword", values_to = "Interest") %>%
  mutate(Interest = as.numeric(gsub("%", "", Interest))) %>%
  filter(!is.na(Interest)) %>%
  mutate(Keyword = gsub("\\.\\.\\.\\d+\\.\\d+\\.\\d+\\.\\.\\d+\\.\\d+\\.\\d+\\.", "", Keyword))

# Assuming geo_dat_long is your reshaped and cleaned geographic data
top_geo_dat <- geo_dat_long %>%
  group_by(Keyword) %>%
  top_n(10, Interest) %>%
  ungroup() %>%
  arrange(Keyword, desc(Interest))


# plot geo dat

ggplot(top_geo_dat, aes(x = Region, y = Interest, fill = Keyword)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 10 Regions by Interest and Keyword", x = "Region", y = "Interest") +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(~Keyword, scales = "free")




# Reshape from wide to long
time_dat_long <- time_dat %>%
  pivot_longer(
    cols = -Month,
    names_to = "Search_Term",
    values_to = "Interest"
  ) %>%
  mutate(
    Month = as.Date(paste0(Month, "-01")), # Convert Month to Date
    Search_Term = gsub("\\.\\.United\\.States\\.", "", Search_Term) # Clean up Search_Term names
  )
    
ggplot(time_dat_long, aes(x = Month, y = Interest, color = Search_Term)) +
  geom_line() + 
  theme_minimal() +
  labs(
    title = "Interest in Local Goods Over Time",
    x = "Month",
    y = "Interest",
    color = "Search Term"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Other cool plots

# top_geo_dat contains the top countries and their interests
ggplot(top_geo_dat, aes(x = Keyword, y = reorder(Region, Interest), fill = Interest)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "navy") +
  labs(title = "Search Interest Heatmap by Country and Keyword", x = "Keyword", y = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# reshaping for correlation analysis
time_dat_wide <- spread(time_dat_long, Search_Term, Interest)

# calculating correlation
corr_matrix <- cor(time_dat_wide[, -1], use = "complete.obs")

# plotting
corrplot(corr_matrix, method = "circle")
