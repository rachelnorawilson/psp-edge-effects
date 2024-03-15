### Analysis of understory plant data collected in Pacific Spirit Park
### along a gradient of distance from the edge

# Packages needed:
library(ggplot2)

# Load R data:
load("data/psp_dataframes.Rdata") # diversity.noNA is used for invasive proportion only



# Richness

(richness.plot <- ggplot(aes(x = distance, y = richness), data = diversity) +
  geom_smooth(method = "lm",
              se = FALSE, 
              aes(group = transect_name),
              color = "thistle2",
              size = 0.7) +
  geom_line(aes(x = distance, y = richness.pred), 
            size = 2, 
            color = "thistle4") +
  labs(x = "Distance from forest edge (m)", 
       y = "Species richness") + 
  ylim(-0.4, NA) +
  theme_classic())
ggsave("figures/richness_clean.png", plot = richness.plot, width = 3.5, height = 3.5)

(richness.plot.wpoints <- ggplot(aes(x = distance, y = richness), data = diversity) +
    geom_smooth(method = "lm",
                se = FALSE, 
                aes(group = transect_name),
                color = "thistle2",
                size = 0.7) +
    geom_jitter(width = 0.0, 
                height = 0.0, 
                aes(x = distance, y = richness), 
                data = diversity, 
                color = "thistle3",
                alpha = 0.2) +
    geom_line(aes(x = distance, y = richness.pred), 
              size = 2, 
              color = "thistle4") +
    labs(x = "Distance from forest edge (m)", 
         y = "Species richness") + 
    ylim(-0.4, NA) +
    theme_classic())
ggsave("figures/richness_points.png", plot = richness.plot.wpoints, width = 3.5, height = 3.5)


# Shannon

(shannon.plot <- ggplot(aes(x = distance, y = shannon), data = diversity) +
    geom_smooth(method = "lm",
                se = FALSE, 
                aes(group = transect_name),
                color = "honeydew2",
                size = 0.7) +
    geom_line(aes(x = distance, y = shannon.pred), 
              size = 2, 
              color = "honeydew4") +
    labs(x = "Distance from forest edge (m)", 
         y = "Shannon diversity index") + 
    ylim(-0.03, NA) +
    theme_classic())
ggsave("figures/shannon_clean.png", plot = shannon.plot, width = 3.5, height = 3.5)

(shannon.plot.wpoints <- ggplot(aes(x = distance, y = shannon), data = diversity) +
    geom_smooth(method = "lm",
                se = FALSE, 
                aes(group = transect_name),
                color = "honeydew2",
                size = 0.7) +
    geom_jitter(width = 0.0, 
                height = 0.00, 
                aes(x = distance, y = shannon), 
                data = diversity, 
                color = "honeydew3",
                alpha = 0.3) +
    geom_line(aes(x = distance, y = shannon.pred), 
              size = 2, 
              color = "honeydew4") +
    labs(x = "Distance from forest edge (m)", 
         y = "Shannon diversity index") + 
    ylim(-0.03, NA) +
    theme_classic())
ggsave("figures/shannon_points.png", plot = shannon.plot.wpoints, width = 3.5, height = 3.5)


# Invasive species proportion

(invasive.plot <- ggplot(aes(x = distance, y = invasive.prop), data = diversity.noNA) +
    geom_smooth(method = "lm",
                se = FALSE, 
                aes(group = transect_name),
                color = "lightpink1",
                size = 0.7) +
    geom_line(aes(x = distance, y = invasive.pred), 
              size = 2, 
              color = "lightpink4") +
    labs(x = "Distance from forest edge (m)", 
         y = "Proportion of invasive species") + 
    ylim(-0.03, NA) +
    theme_classic())
ggsave("figures/invasive_clean.png", plot = invasive.plot, width = 3.5, height = 3.5)

(invasive.plot.wpoints <- ggplot(aes(x = distance, y = invasive.prop), data = diversity.noNA) +
    geom_smooth(method = "lm",
                se = FALSE, 
                aes(group = transect_name),
                color = "lightpink1",
                size = 0.7) +
    geom_jitter(width = 0.0, 
                height = 0.00, 
                aes(x = distance, y = invasive.prop), 
                data = diversity, 
                color = "lightpink3",
                alpha = 0.3) +
    geom_line(aes(x = distance, y = invasive.pred), 
              size = 2, 
              color = "lightpink4") +
    labs(x = "Distance from forest edge (m)", 
         y = "Proportion of invasive species") + 
    ylim(-0.03, NA) +
    theme_classic())
ggsave("figures/invasive_points.png", plot = invasive.plot.wpoints, width = 3.5, height = 3.5)





