# Ryan Grossman 
# 2021-01-21
# References:
# - https://www.reddit.com/r/formula1/comments/4v9t9f/f1_team_color_hex_codes/
# - https://flowingdata.com/2018/08/28/how-to-make-better-looking-more-readable-charts-in-r/

library(tidyverse)
library(beeswarm)

# Load & Process Data -----------------------------------------------------

# Load in the raw data
results <- read_csv("../data-viz/data/formula_one_results.csv")
drivers <- read_csv("../data-viz/data/formula_one_drivers.csv")
constructors <- read_csv("../data-viz/data/formula_one_constructors.csv")
races <- read_csv("../data-viz/data/formula_one_races.csv")

race_results <- 
  results %>% 
  inner_join(drivers, by = "driverId", suffix = c("", ".y")) %>% 
  inner_join(constructors, by = "constructorId", suffix = c("", ".y")) %>% 
  inner_join(races, by = "raceId", suffix = c("", ".y"))

# Pull out race finish positions for the past season
season_results <-
  race_results %>% 
  filter(year >= 2020) %>% 
  select(raceId, forename, surname, constructorRef, positionOrder) %>% 
  mutate(
    driver = paste(substr(forename, 1, 1), ". ", surname, sep = "")
  ) 
season_results


# Create Bee Swarm --------------------------------------------------------


# Clean up the constructor names
season_results <-
  season_results %>% 
  mutate(
    constructorRef = case_when(
     constructorRef %in% c("force_india", "racing_point") ~ "Racing Pt.",
     constructorRef %in% c("alphatauri", "toro_rosso") ~ "Alpha T.",
     constructorRef == "mercedes" ~ "Mercedes",
     constructorRef == "ferrari" ~ "Ferrari",
     constructorRef == "williams" ~ "Williams",
     constructorRef %in% c("sauber", "alfa") ~ "Alfa R.",
     constructorRef == "red_bull" ~ "Red Bull",
     constructorRef == "mclaren" ~ "McLaren",
     constructorRef == "lotus_f1" ~ "Lotus",
     constructorRef == "manor" ~ "Manor",
     constructorRef == "haas" ~ "Haas",
     constructorRef == "renault" ~ "Renault"
    )
  )

# Function to map the constructor to color 
colorByConstructor <- function(x){
  if(x == "Williams") {
    return("#0082FA")
  } else if (x == "Lotus") {
    return("#FFB800")
  } else if (x == "Ferrari") {
    return("#C80000")
  } else if (x == "Renault") {
    return("#FFF500")
  } else if (x == "McLaren") {
    return("#FF8700")
  } else if (x == "Red Bull") {
    return("#1E41FF")
  } else if (x == "Mercedes") {
    return("#00D2BE")
  } else if (x ==  "Racing Pt.") {
    return("#F596C8")
  } else if (x == "Alfa R.") {
    return("#9B0000")
  } else if (x == "Alpha T.") {
    return("#FFFFFF")
  } else if (x == "Manor") {
    return("#323232")
  } else if (x == "Haas") {
    return("#787878")
  } else {
    return('black')
  }
}

season_results$color <- sapply(
  season_results$constructorRef, colorByConstructor
)

png(
  filename = "formula-one-2020_bee-swarm.png", 
  width = 2456, height = 1744, units = "px", res = 175
)

# Set up the plot area
par(mfrow=c(24,1), mar =c(0,24,0,0), oma=c(4,0,5,1), bg="#2f2f2f")

# Add in the legend 
constructors <- unique(season_results$constructorRef)
col <- sapply(constructors, colorByConstructor)
xleg <- seq(1, 22, by=2.2)

beeswarm(xleg, pwcol=col,
         horizontal = TRUE,
         pch=15,
         method="center",
         cex=1,
         xlim=c(0,24),
         ylim=c(-10,4), 
         axes = FALSE
)
        

text(x=xleg+0.01, y=0, labels=toupper(constructors), col="white", pos=4, cex=1.1)


# Generate a plot for each driver
season_results <- season_results %>% arrange(constructorRef)
drivers <- unique(season_results$driver)
for (i in 1:length(drivers)) {
  
  # Subset to food items for current restaurant.
  cur <- season_results[season_results$driver == drivers[i],]
  
  # Draw the symbols
  beeswarm(cur$positionOrder,
           pwcol=cur$color,
           pch=15,
           method="center",
           cex=1, horizontal=TRUE, axes=FALSE, 
           ylim = c(-20, 20), xlim = c(0,24))
  
  mtext(drivers[i], side = 2, las=1, cex=.9, col = "white")
}

mtext(
  "2020 Season Race Outcomes", 
  side=3, outer=TRUE, line=1, cex=1.5, col = "white"
)
mtext(
  "POSITION", side=1, at=x[1], outer=FALSE, 
  col="white", line=2, adj=0.05, padj=.5, cex=.8
)

# Add the x axis
x <- seq(1, 20, 1)
axis(side = 1, labels = x, at=x, col="#2f2f2f", col.ticks = "white")
mtext(x, side = 1, outer=FALSE, at=x, line=.8, col="white", cex=.8)

dev.off()

