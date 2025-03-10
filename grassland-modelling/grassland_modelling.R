

# Function to check if a package is installed, install it if necessary, and then load it
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

# List of packages you need
packages <-
  c(
    "reshape",
    "fasttime",
    "Distance",
    "MASS",
    "tidyverse",
    "activity",
    "dplyr",
    "lubridate",
    "stringr",
    "broom.mixed",
    "lme4",
    "ggeffects",
    "glmmTMB",
    "MuMIn",
    "kableExtra",
    "ggplot2"
    
  )

# Apply the function to each package
invisible(lapply(packages, install_and_load))


# #________________________________________________________________________________________________________________________________________________
# #                                             Modelling with encounter rates
# #________________________________________________________________________________________________________________________________________________


# Load data
cows_buff_24 <- read.csv("https://raw.githubusercontent.com/harryjobann/WP2-Elopa-grassland/refs/heads/main/grassland-modelling/raw_data/encounter_cows_buff.csv")
wild_raw_24 <- read.csv("https://raw.githubusercontent.com/harryjobann/WP2-Elopa-grassland/refs/heads/main/grassland-modelling/raw_data/encounters_wild_animals.csv")
cam_placements <- read.csv('https://raw.githubusercontent.com/harryjobann/WP2-Elopa-grassland/refs/heads/main/grassland-modelling/raw_data/Grassland_deployments_2024.csv')



###________________________________________________________________________________________________________________________________________________
###                     Data preparation
###________________________________________________________________________________________________________________________________________________

# create merged df
merged_df <- dplyr::inner_join(cows_buff_24, wild_raw_24, by = "station") %>%
  dplyr::inner_join(cam_placements, by = "station") %>%
  dplyr::mutate(
    Combined = Buffalo + Cow)

# cleaning the covariates so we can model with them
merged_df <- merged_df %>% mutate(Elevation = as.numeric(gsub(" m", "", Elevation)))
merged_df <- merged_df %>% dplyr::mutate(Habitat = as.factor(Habitat))

merged_df <- merged_df %>%
  dplyr::mutate(
    Elevation = as.numeric(gsub(" m", "", Elevation)),  
    Habitat = as.factor(Habitat),  
    log_Buffalo = log1p(Buffalo),  
    log_Cow = log1p(Cow),
    log_Combined = log1p(Combined),
    log_river_distance = log1p(river_distance),  
    log_mtn_distance = log1p(mtn_distance),  
    log_dist_to_cover = log1p(dist_to_cover) 
  )

# Select species columns
species_cols <- names(merged_df)[which(names(merged_df) == "Indian_hare"):which(names(merged_df) == "Asiatic_wild_dog")]





###________________________________________________________________________________________________________________________________________________
###                         CHECK FOR OVERDISPERSION
###________________________________________________________________________________________________________________________________________________

# Function to test overdispersion for a single species
check_overdispersion <- function(species) {
  model <- glm(as.formula(paste(species, "~ 1")), family = poisson, data = merged_df)
  
  deviance <- model$deviance
  df <- model$df.residual
  overdispersion_stat <- deviance / df
  
  cat("Species:", species, "\n")
  cat("Residual Deviance:", deviance, "\n")
  cat("Degrees of Freedom:", df, "\n")
  cat("Overdispersion Statistic:", overdispersion_stat, "\n\n")
  
  return(overdispersion_stat)
}

# Apply overdispersion check for all species
print(species_overdispersion <- sapply(species_cols, check_overdispersion))

### OK so Jungle cat is the only species we'll fit Poisson for. The rest are over-dispersed so we'll fit negative binomials. 



###________________________________________________________________________________________________________________________________________________
###                         Run the loop
###________________________________________________________________________________________________________________________________________________

# Identify appropriate model families
species_list <- c("Indian_hare", "Hog_deer", "Jungle_cat", "Sambar")
models <- list()
errors <- list()

for (species in species_list) {
  response_var <- species
  full_formula <- as.formula(paste(response_var, "~ log_Buffalo + log_Cow + log_Combined + ",
                                   "Elevation + log_river_distance + log_mtn_distance + log_dist_to_cover +",
                                   "offset(log(Trapnights)) + (1|station)"))
  
  model_family <- if (species == "Jungle_cat") poisson() else nbinom2()
  
  print(paste("Fitting full model for:", species))
  
  tryCatch({
    models[[species]] <- glmmTMB(
      formula = full_formula,
      family = model_family,
      data = merged_df,
      control = glmmTMBControl(
        optimizer = optim,
        optCtrl = list(maxit = 10000) 
      )
    )
    
    
    cat("✅ Full model for", species, "fitted successfully.\n")
    
  }, error = function(e) {
    cat("❌ Error fitting full model for", species, ":", conditionMessage(e), "\n")
    errors[[species]] <- conditionMessage(e)
  })
}

# Print model summaries
for (species in species_list) {
  if (!is.null(models[[species]])) {
    cat("\n🔍 Summary for full model for:", species, "\n")
    print(summary(models[[species]]))
  }
}



###________________________________________________________________________________________________________________________________________________
###                         Table of results 
###________________________________________________________________________________________________________________________________________________


# Create df to stole model sommarueis
model_results <- lapply(names(models), function(species_name) {
  model <- models[[species_name]]
  if (!is.null(model)) {
    tidy(model, effects = "fixed") %>%
      mutate(Species = species_name)  
  } else {
    NULL
  }
})

# combine results into single df
results_table <- bind_rows(model_results) %>%
  dplyr::select(Species, term, estimate, std.error, p.value) %>% 
  arrange(Species, term)  

# create formatted table using kable extda
formatted_table <- results_table %>%
  kbl(
    caption = "Model Results by Species",
    col.names = c("Species", "Predictor", "Estimate", "Standard Error", "P-value"),
    digits = 3,
    align = c("l", "l", "r", "r", "r")
  ) %>%
  kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "hover", "condensed", "responsive")
  ) %>%
  group_rows(
    "Indian Hare", 1, sum(results_table$Species == "Indian_hare")
  ) %>%
  group_rows(
    "Sambar", sum(results_table$Species == "Indian_hare") + 1, 
    sum(results_table$Species %in% c("Indian_hare", "Sambar"))
  ) %>%
  group_rows(
    "Jungle Cat", sum(results_table$Species %in% c("Indian_hare", "Sambar")) + 1, 
    sum(results_table$Species %in% c("Indian_hare", "Sambar", "Jungle_cat"))
  ) %>%
  group_rows(
    "Hog Deer", sum(results_table$Species %in% c("Indian_hare", "Sambar", "Jungle_cat")) + 1, 
    nrow(results_table)
  ) %>%
  row_spec(
    which(results_table$p.value < 0.05), 
    bold = TRUE, 
    background = "#f2f2f2"
  ) %>%
  column_spec(3:5, width = "8em")  # Adjust column width for readability

# Print the table
formatted_table


###________________________________________________________________________________________________________________________________________________
###                         Plots & visuals
###________________________________________________________________________________________________________________________________________________

# Plot hare and sambar count with distance to nearest cover
transformed_df <- merged_df %>%
  mutate(
    Indian_hare = ifelse(Indian_hare == 0, NA, Indian_hare),
    Sambar = ifelse(Sambar == 0, NA, Sambar)
  ) %>%
  pivot_longer(
    cols = c(Indian_hare, Sambar),
    names_to = "Species",
    values_to = "Count"
  ) %>%
  filter(!is.na(Count))  


 ggplot(transformed_df, aes(x = log_dist_to_cover, y = Count, colour = Species)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +  
  scale_y_continuous(trans = "log1p") +  
  scale_colour_manual(values = c("Indian_hare" = "#FF5733", "Sambar" = "#33C3FF")) +  
  labs(
    title = "Relationship Between Distance to Cover and Species Encounters",
    x = "Log(Distance to Cover)",
    y = "Log(Species Count)",
    colour = "Species"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 16, face = "bold"),  
    axis.text = element_text(size = 14)  
  )



