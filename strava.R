library(tarchetypes)
library(conflicted)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(targets)
library(httpuv)
library(httr)
library(pins)
library(httr)
library(fs)
library(readr)
library(rStrava)
library(googlePolylines)
library(googleway)
library(sf)
library(ggmap)
library(osmdata)
library(rcartocolor)
library(gganimate)
library(decode)
library(vctrs)
library(ggplot2)
library(ggpubr)
library(egg)
library(pander)
library(patchwork)
library(gridExtra)
library(cowplot)
library(purrr)

# Specify which version of the filter function to use
conflict_prefer("filter", "dplyr")

# Set Strava API application information
app_name <- "r_api"
app_client_id <- "99795"
app_secret <- "fcd3ac724c9aa06f636a4a4f5f3d1e9a04dc2e29"
app_scope <- "activity:read_all"

# Obtain access token for Strava API

strava_token <- httr::config(token = strava_oauth(app_name,
                                                  app_client_id,
                                                  app_secret,
                                                  app_scope = "activity:read_all"))

# Retrieve list of activities for authenticated user

my_acts <- get_activity_list(strava_token)

# Extract summary polylines

polylist <- list()

for (i in 1:375){
  polylist[i] <-  my_acts[[i]][["map"]][["summary_polyline"]]
}

# Convert list to data frame and rename column
polylist_neat <- as.data.frame(unlist(polylist)) %>% 
  rename(polylines = `unlist(polylist)`)

# Filter out NA and empty string values
polylist_neat_filtered <- polylist_neat %>%
  filter(!is.na(polylines) & polylines != "")

# Convert list to data frame and add ID column
polylist_neat_filtered %>%
  as.data.frame(unlist(.)) %>%
  mutate(id = seq(1:nrow(polylist_neat_filtered)))



# Decode polyline - for one run
decode_poly <- decode(polylist_neat_filtered[355, 1])

# Extract longitude and latitude coordinates - for one run
decode_poly_lon <- decode_poly[[1]]$lon
decode_poly_lat <- decode_poly[[1]]$lat
decode_poly_df <- data.frame(cbind(decode_poly_lon,decode_poly_lat))

# Create ggplot object
plot1 <- decode_poly_df %>%
  ggplot(aes(x = decode_poly_lon, y = decode_poly_lat)) +
  geom_path() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    strip.text = element_blank()
  )

# Plot one run

plot1


####
decode_list <- list()

for (i in 1:356){
  decode_list[i] <- decode(polylist_neat_filtered[i,1])
}


plots <- list()

for (i in 1:356){
  plots[[i]] <-  local({
    i <-i
    p <- as.data.frame(decode_list[[i]]) %>%
           ggplot(aes(x = as.numeric(unlist(decode_list[[i]][1])), y = as.numeric(unlist(decode_list[[i]][2])))) +
           geom_path() +
           theme(
             axis.line = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             axis.title.x = element_blank(),
             axis.title.y = element_blank(),
             legend.position = "bottom",
             panel.background = element_blank(),
             panel.border = element_blank(),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.background = element_blank(),
             strip.text = element_blank()
           )
    })
}
plot_grid(plotlist=plots)
