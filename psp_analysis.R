### Analysis of understory plant data collected in Pacific Spirit Park
### along a gradient of distance from the edge

### NOTE: Random slope & intercept models were tested but failed to converge, 
### even when adjusting the optimizer. Did not thoroughly explore options.

# Packages needed:
library(tidyverse)
library(vegan)
library(lme4)

# Datasets needed (double check that you have the correct year and term):
transect.raw <- read.csv("data/transect_data_Fall2024.csv", header = TRUE)
diversity.raw <- read.csv("data/diversity_data_Fall2024.csv", header = TRUE)



### Question 1: Do easterly transects contain more garbage, dog feces, or fallen trees?

if(is.numeric(transect.raw$dog_feces) == TRUE) {
  
  ## NEW: Numeric counts (# of times observed)
  
  feces <- transect.raw %>%
    group_by(direction) %>%
    summarise(feces = mean(dog_feces))
  garbage <- transect.raw %>%
    group_by(direction) %>%
    summarise(garbage = mean(garbage))
  transect_summary <- feces %>%
    left_join(garbage, by = "direction")
    
} else {
  
  ## OLD: Binary Y/N data
  
  # Subset transect data to Y only
  filter(transect.raw, dog_feces == "Y")
  
  # Create a data frame summarizing Y/N for each variable of interest
  feces <- transect.raw %>%
    filter(dog_feces == "Y") %>%
    count(direction, dog_feces, name = "total_dog_feces")
  garbage <- transect.raw %>%
    filter(garbage == "Y") %>%
    count(direction, garbage, name = "total_garbage")
  trees <- transect.raw %>%
    filter(fallen_trees == "Y") %>%
    count(direction, fallen_trees, name = "total_trees")
  
  # Join into one data frame, remove unhelpful columns, summarize by proportion of transects
  (transect_summary <- feces %>%
      left_join(garbage, by = "direction") %>%
      left_join(trees, by = "direction") %>%
      select(-dog_feces, -garbage, -fallen_trees) %>%
      mutate(prop_dog_feces = total_dog_feces / (nrow(transect.raw)/2),
             prop_garbage = total_garbage / (nrow(transect.raw)/2),
             prop_trees = total_trees / nrow(transect.raw)/2))
  
}

print(as.data.frame(transect_summary))
# Copy values into Table 2 in report.



### Question 2: Does diversity or invasive species change with distance from the forest edge?

div_long <- diversity.raw %>%
  gather(key = "distance", value = "cover", -transect_name, -species) %>%
  mutate(distance = str_replace(distance, "X", ""))

invasive.species.list <- c("Eng_holly", "Him_blackberry", "Eng_laurel", "wall_lettuce", "unk_vetch")

# STEP 1: Create a summary data frame that describes diversity at each distance and transect

diversity.list <- list() # Empty list to store all richness data

for(i in 1:length(unique(div_long$transect_name))) { # Loop for transect names
  
  T <- unique(div_long$transect_name)[i]
  diversity.plot.list <- list() # Empty list to store plot-specific richness data
  
  for(k in 1:length(unique(div_long$distance))) { # Loop for distance from edge
    
    D <- as.integer(unique(div_long$distance)[k])
    plot <- filter(div_long, transect_name == T, distance == D) # Filter to transect X distance
    
    richness.plot <- plot %>% 
      summarise(richness.value = sum(cover != 0)) %>% # Sum species for whom cover > 0
      pull(richness.value)
    
    diversity.plot <- plot %>%
      summarise(shannon.value = diversity(plot$cover, index = "shannon")) %>%
      pull(shannon.value)
    
    invasive.cover.plot <- plot %>%
      filter(species %in% invasive.species.list) %>%
      summarise(invasive.cover = sum(cover)) %>%
      pull(invasive.cover)
    
    invasive.prop.plot <- plot %>%
      summarise(invasive.value = invasive.cover.plot/sum(cover)) %>%
      pull(invasive.value)
    
    diversity.plot.list[[k]] <- data.frame(transect_name = T, 
                                           distance = D, 
                                           richness = richness.plot, 
                                           shannon = diversity.plot,
                                           invasive.prop = invasive.prop.plot)
  }
  
  diversity.plot.df <- bind_rows(diversity.plot.list)
  diversity.list[[i]] <- diversity.plot.df
}

diversity <- bind_rows(diversity.list)


# STEP 2: Create a mixed model for SHANNON

shannon.mod <- lmer(shannon ~ distance + (1|transect_name), data = diversity) # Summer 2024 singular fit warning; results dubious
summary(shannon.mod) # Distance coefficient estimate: 0.009295
confint(shannon.mod) # CI for distance: 0.002220494 - 0.01637023
var.components.shannon <- as.data.frame(VarCorr(shannon.mod))
transect.var.shannon <- var.components.shannon$vcov[1]
residual.var.shannon <- var.components.shannon$vcov[2]

# What % of remaining variance can be explained by transect?
(transect.var.shannon / (transect.var.shannon + residual.var.shannon)) * 100 # 0%

# Likelihood ratio test with REML. Divide P by 2 (testing on the boundary)
shannon.null <- lmer(shannon ~ 1 + (1|transect_name), data = diversity)
(shannon.anova <- anova(shannon.mod, shannon.null))
(shannon.anova.P <- shannon.anova$`Pr(>Chisq)`[2]/2) # 0.005304341

diversity$shannon.pred <- predict(shannon.mod, re.form = NA)



# STEP 3: Create a mixed model for RICHNESS

richness.mod <- lmer(richness ~ distance + (1|transect_name), data = diversity) # Summer 2024 singular fit warning; results dubious
summary(richness.mod) # Distance coefficient estimate: 0.02597
confint(richness.mod) # CI for distance: 0.003533727 - 0.04841432
var.components.richness <- as.data.frame(VarCorr(richness.mod))
transect.var.richness <- var.components.richness$vcov[1]
residual.var.richness <- var.components.richness$vcov[2]

# What % of remaining variance can be explained by transect?
(transect.var.richness / (transect.var.richness + residual.var.richness))  * 100 # 0%

# Likelihood ratio test with REML. Divide P by 2 (testing on the boundary)
richness.null <- lmer(richness ~ 1 + (1|transect_name), data = diversity)
(richness.anova <- anova(richness.mod, richness.null))
(richness.anova.P <- richness.anova$`Pr(>Chisq)`[2]/2) #0.01189881

diversity$richness.pred <- predict(richness.mod, re.form = NA)

# STEP 4: Create a mixed model for INVASIVE PROPORTION

# Special additional step: subset to NA-free rows
diversity.noNA <- diversity %>%
  filter(!is.na(invasive.prop))

invasive.mod <- lmer(invasive.prop ~ distance + (1|transect_name), data = diversity.noNA)
summary(invasive.mod) # Distance coefficient estimate: 0.0004215
confint(invasive.mod) # CI for distance: -0.00153514 - 0.00237792
var.components.invasive <- as.data.frame(VarCorr(invasive.mod))
transect.var.invasive <- var.components.invasive$vcov[1]
residual.var.invasive <- var.components.invasive$vcov[2]

# What % of remaining variance can be explained by transect?
(transect.var.invasive / (transect.var.invasive + residual.var.invasive))  * 100 # 5%

# Likelihood ratio test with REML. Divide P by 2 (testing on the boundary)
invasive.null <- lmer(invasive.prop ~ 1 + (1|transect_name), data = diversity.noNA)
(invasive.anova <- anova(invasive.mod, invasive.null))
(invasive.anova.P <- invasive.anova$`Pr(>Chisq)`[2]/2) # 0.3347469

diversity.noNA$invasive.pred <- predict(invasive.mod, re.form = NA)

# Joining NA-free dataframe to original diversity dataframe

#diversity.join <- full_join(diversity, diversity.noNA, by = c("transect_name", "distance", "shannon", "invasive.prop", "richness.pred", "shannon.pred", "richness"))



# STEP 5: Save diversity data frames as objects to pull in visualization script

save(diversity, diversity.noNA, file = "data/psp_dataframes.Rdata")






