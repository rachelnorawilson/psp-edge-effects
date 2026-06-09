### Analysis of understory plant data collected in Pacific Spirit Park
### along a gradient of distance from the edge

### This script is to be used for long-term analysis of data and compiles
### across multiple years

# Packages needed:
# Packages needed:
library(tidyverse)
library(lme4)

# Load R data:
load("data/psp_ALL.Rdata") # diversity.ALL.noNA is used for invasive proportion only

### STEP 1: Visualize trends in transect data

# Organizing data
transect.ALL$months <- c(0, 0, 4, 4, 16, 16, 24, 24)
transect.ALL$direction <- factor(transect.ALL$direction, levels = c("W", "E"))

# Dog feces plot
(feces.plot <- ggplot(aes(x = months, y = feces), data = transect.ALL) +
    geom_point(aes(x = months, y = feces, color = direction),
                size = 1.5) +
    geom_smooth(method = "lm",
                se = FALSE, 
                aes(color = direction),
                size = 1) + 
    scale_color_manual(
      values = c("steelblue1", "sienna1"),
      labels = c( "Close (West)", "Far (East)"),
      name = "Proximity to edge") +
    labs(x = "Time since summer 2024", 
         y = "Mean dog feces encountered per transect") +
    theme_classic())

#Garbage plot
(garbage.plot <- ggplot(aes(x = months, y = garbage), data = transect.ALL) +
    geom_jitter(aes(x = months, y = garbage, color = direction),
               size = 1.5,
               width = 0,
               height = 0.03) +
    geom_smooth(method = "lm",
                formula = y ~ poly(x, 2),
                se = FALSE, 
                aes(color = direction),
                size = 1) + 
    scale_color_manual(
      values = c("steelblue1", "sienna1"),
      labels = c( "Close (West)", "Far (East)"),
      name = "Proximity to edge") +
    labs(x = "Time since summer 2024", 
         y = "Mean garbage encountered per transect") +
    theme_classic())



