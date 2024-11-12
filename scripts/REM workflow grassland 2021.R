
# Function to check if a package is installed, install it if necessary, and then load it
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

# List of packages you need
packages <- c("reshape", "fasttime", "Distance", "MASS", "tidyverse",
              "activity", "dplyr", "lubridate", "stringr", "ggplot2", "readxl", "activity", "purrr")

# Apply the function to each package
invisible(lapply(packages, install_and_load))


# Load external packages
devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/camtools/master/get_detection_matrix.r")
devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/CTtracking/V0.3.2/CTtracking.r")
devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/camtools/refs/heads/master/camtools.R")
devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/distanceDF/master/distancedf.r")
devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/sbd/refs/heads/main/R/hmean.r")


# # #_________________________________________________________________________
#                         Read in dfs
# # #_________________________________________________________________________

# # exifdat
exifdat_test_05 <- read.csv('https://raw.githubusercontent.com/harryjobann/WP2-Elopa-grassland/refs/heads/main/data/2021_grassland_exifdat.csv', 
                             header = TRUE, colClasses = 'character')

# # depdat
deptab_test_05 <- read.csv('https://raw.githubusercontent.com/harryjobann/WP2-Elopa-grassland/refs/heads/main/data/2021_grassland_deptab.csv', header = TRUE)

# posdat
posdat_dibang <- read.csv(file = 'https://raw.githubusercontent.com/harryjobann/WP2-Elopa-grassland/refs/heads/main/data/2021_grassland_posdat.csv', header = TRUE)

# speed data (aggregated acros both seasons of Grassland data (2021 & 2024 to improve data integrity and robustness))
seqdat_dibang <- read.csv('https://raw.githubusercontent.com/harryjobann/WP2-Elopa-grassland/refs/heads/main/data/combined_seqdat.csv', header = TRUE)




# # #_________________________________________________________________________
# #                       Cleaning data 
# # #_________________________________________________________________________

exif47 <- exifdat_test_05 %>% filter(station == "ELOPA_47")

# Manually update the stop date for ELOPA_47 as this is missing currently 
deptab_test_05$stop[deptab_test_05$station == "ELOPA_47"] <- format(as.POSIXct("2020-12-21 18:00:00"), "%Y-%m-%d %H:%M:%S")

# Drop elopa 45 as this was missing 
deptab_test_05 <- deptab_test_05[-8, ]


# Convert the 'radiantime' column to numeric format
exifdat_test_05 <- exifdat_test_05 %>%
  mutate(radiantime = as.numeric(radiantime))

# Check the structure of the dataframe to confirm the change
str(exifdat_test_05)

exifdat_test_05$station <- sub(" ", "", exifdat_test_05$station)


#_________________________________________________________________________
#         Subsetting for contacts
#_________________________________________________________________________
contactdat_test_05 <- subset(exifdat_test_05, contact==1)


#_____________________________________________________________________________________________
#           Check all stations included
#______________________________________________________________________________________________
# Find shared stations
shared_stations <- intersect(contactdat_test_05$station, deptab_test_05$station)
shared_stations_df <- data.frame(matrix(unlist(shared_stations), nrow=length(shared_stations), byrow=TRUE))

#Stations being excluded
only_in_deptab <- deptab_test_05$station[!deptab_test_05$station %in% contactdat_test_05$station]
only_in_contactdat <- contactdat_test_05$station[!contactdat_test_05$station %in% deptab_test_05$station]

only_in_contactdat <- dplyr::filter(contactdat_test_05, !station %in% deptab_test_05$station)$station
only_in_deptab_df <- data.frame(matrix(unlist(only_in_deptab), nrow=length(only_in_deptab), byrow=TRUE))
only_in_contactdat_df <- data.frame(matrix(unlist(only_in_contactdat), nrow=length(only_in_contactdat), byrow=TRUE))


# Clean and convert the date column
contactdat_test_05$date <- trimws(contactdat_test_05$date)  # Remove any extra spaces


# Define the desired date-time format
correct_format <- "%d-%m-%Y %H:%M:%S"

# Convert 'start' and 'stop' columns in deptab_test_05 to the desired format
deptab_test_05 <- deptab_test_05 %>%
  mutate(
    start = format(as.POSIXct(start, tz = "UTC"), correct_format),
    stop = format(as.POSIXct(stop, tz = "UTC"), correct_format)
  )



# Add a row reference number as a new column
contactdat_test_05 <- contactdat_test_05 %>%
  mutate(row_ref = row_number())


# #__________________________________________________________________________________________________________________________________________________
# #                                     Plotting deployments to check for outliers 
# #_________________________________________________________________________________________________________________________________________________

# Convert dates to POSIXct format
deptab_test_05$start <- as.POSIXct(deptab_test_05$start, format = "%d-%m-%Y %H:%M:%S", tz = "UTC")
deptab_test_05$stop <- as.POSIXct(deptab_test_05$stop, format = "%d-%m-%Y %H:%M:%S", tz = "UTC")

# Modify the date column
contactdat_test_05 <- contactdat_test_05 %>%
  mutate(
    date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")
  ) %>%
  mutate(
    # Check if conditions match for updating the year
    date = ifelse(
      station == "ELOPA_40" & 
        format(date, "%Y") == "2021" & 
        format(date, "%m") %in% c("11", "12"),
      format(date, "2020-%m-%d %H:%M:%S"),
      as.character(date)  # Convert to character to prevent type issues with ifelse
    ),
    # Convert back to POSIXct after updating
    date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")
  )


#Check deployment dates align with deployment location file
chk <- check.dates(contactdat_test_05, deptab_test_05)
bad.data <- chk$bad.data


#exclude bad data
contactdat_test_05 <- dplyr::filter(
  contactdat_test_05,
  !date %in% bad.data$date
)

# renaming
contactdat_test_05$number.of.individuals <- contactdat_test_05$Number.of.individuals
contactdat_test_05$Number.of.individuals <- NULL


plot.deployments(contactdat_test_05, deptab_test_05)


# #_____________________________________________________________________________________________
# #                       Accounting for multiple individuals 
# #______________________________________________________________________________________________

# some contacts contain multiple individuals. We therefore need to account for this before calculating density. Below, this has been
# done by by duplicating certain rows which apply

# Step 2: Extract numeric values from the filtered number.of.individuals column
contactdat_test_05$individuals <- as.numeric(gsub("c-|[^0-9]", "", contactdat_test_05$number.of.individuals))

# Step 3: Function to duplicate rows based on individuals count (skip rows with 0)
duplicate_rows <- function(df) {
  # Filter out rows where individuals is 0, and then duplicate remaining rows
  df_filtered <- df[df$individuals > 0, ]
  replicated_rows <- df_filtered[rep(seq_len(nrow(df_filtered)), df_filtered$individuals), ]
  
  # Bind rows with individuals = 0 back to the replicated rows (no duplication for these rows)
  result <- rbind(replicated_rows, df[df$individuals == 0, ])
  
  # Calculate and print the number of new rows added
  new_rows <- nrow(result) - nrow(df)
  cat("Number of new rows added:", new_rows, "\n")
  
  result
}

# Step 4: Apply the function to the dataframe
contactdat_test_05 <- duplicate_rows(contactdat_test_05)


#_________________________________________________________________________#_________________________________________________________________________
#                                     Define a species list for the loops below
#_________________________________________________________________________#_________________________________________________________________________

#change wild pig to wild boar
contactdat_test_05$species <- sub("Wild_pig", "Wild_boar", contactdat_test_05$species)

species_list <- c(
                 "Hog_deer", "Sambar", "Barking_deer", "Indian_jackal", "Asiatic_wild_dog", "Small_indian_civet",  "Wild_boar",  "Jungle_cat" , "Large_indian_civet",  "Malayan_porcupine", "Yellow_throated_marten",
                  "Indian_hare"
                     #   "Buffalo", "Cow"
             )


# create trap rate df 
trdat <- event.count(contactdat_test_05, deptab_test_05)






# #_________________________________________________________________________#_________________________________________________________________________
# #                                     Activity plots
# #_________________________________________________________________________#________________________________________________________________________
# 
# 
# # Define the common directory to save all plots
# plot_dir <- "/Users/harryjobanputra/Documents/ZSL - Research assistant/WP2 - Grassland Elopa/Handover_Grassland/Results/Activity_plots/"
# dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
# 
# for (sp in species_list) {
#   try({
#     cat("Processing species:", sp, "\n")
#     
#     # Filter data for the current species
#     species_data <- dplyr::filter(contactdat_test_05, species == sp)
#     sample_size <- nrow(species_data)
#     cat("Number of records for", sp, ":", sample_size, "\n")
#     
#     if (sample_size > 0) {
#       # Activity level estimation
#       actmod_elopa_5 <- fitact(species_data$radiantime, sample = "data", reps = 100)
#       
#       # Save plot in the common directory with higher resolution
#       plot_file <- file.path(plot_dir, paste0(sp, "_activity_plot.png"))
#       png(filename = plot_file, width = 1600, height = 1200, res = 300)  # Higher resolution
#       plot(actmod_elopa_5)
#       
#       # Add "Winter 2022-23" and sample size to the plot (smaller text for both)
#       title(main = paste("Summer 2021", "- Sample Size:", sample_size), cex.main = 0.8)  # cex.main controls the text size
#       
#       dev.off()
#       
#       cat("Plot saved for species:", sp, "at", plot_file, "\n")
#     } else {
#       cat("No data available for species:", sp, "\n")
#     }
#   }, silent = TRUE)
# }










# # #________________________________________________________________________________
# # #                            REM:  Model Comparison Loop
# # #________________________________________________________________________________

# As we're running multiple models for each species parameter ( 1: hazard rate and 2: half-normal) this step is useful to manually inspect the results
# of each model. In the following loop, named "final loop", the preferred model can then be selected for each species. If no model is defined, the final 
# loop will default to the model with the lowest AIC score.


# Filter the contactdat_test_05 dataframe for the specified species and reassign it to the same column
contactdat_test_05 <- contactdat_test_05 %>%
  filter(species %in% species_list)

# Initialize an empty list to store results
model_results <- list()

# Initialize an empty dataframe to store model comparison results
model_comparison <- data.frame()

# Loop through each species
for (sp in species_list) {
  cat("Running models for species:", sp, "\n")

  # Camera detection zone estimation
  posdat_dibang$radius <- as.numeric(posdat_dibang$radius)

  dzdat_elopa_5_dibang <- subset(posdat_dibang, frame_count == 1 & species == sp)

  # List of models to test
  models <- c("hn", "hr")

  # Initialize a list to store model outputs
  model_outputs <- list()

  # Loop through each model
  for (model in models) {
    cat("Fitting model:", model, "for species:", sp, "\n")

    # Fit the model
    radmod <- tryCatch({
      fitdf(radius ~ 1, dzdat_elopa_5_dibang, transect = "point", order=0, key = model, truncation = 15)
    }, error = function(e) {
      cat("Error fitting model:", model, "for species:", sp, "\n", e$message, "\n")
      return(NULL)
    })

    if (!is.null(radmod)) {
      aic_value <- radmod$ddf$criterion
      estimate <- tryCatch(radmod$edd$estimate, error = function(e) NA)
      se <- tryCatch(radmod$edd$se, error = function(e) NA)

      # Store the results
      model_outputs[[model]] <- list(aic = aic_value, estimate = estimate, se = se)

      # Save model outputs to the model_comparison dataframe
      model_comparison <- rbind(model_comparison, data.frame(
        species = sp,
        model_type = "radius",
        model = model,
        aic = aic_value,
        estimate = estimate,
        se = se
      ))
    } else {
      cat("Model fitting failed for species:", sp, "\n")
    }
  }

  if (length(model_outputs) > 0) {
    model_results[[sp]] <- model_outputs
  } else {
    cat("No valid models for species:", sp, "\n")
  }

  # Angle estimation
  dzdat_elopa_5_dibang$angle <- abs(dzdat_elopa_5_dibang$angle)

  # Fit both models for angle and select the best
  angle_models <- c("hn", "hr")
  angle_model_outputs <- list()

  for (angle_model in angle_models) {
    angmod_dibang <- tryCatch({
      fitdf(angle ~ 1, dzdat_elopa_5_dibang, key = angle_model, order = 0)
    }, error = function(e) {
      cat("Error fitting angle model:", angle_model, "for species:", sp, "\n", e$message, "\n")
      return(NULL)
    })

    if (!is.null(angmod_dibang)) {
      aic_value <- angmod_dibang$ddf$criterion
      estimate <- tryCatch(angmod_dibang$edd$estimate, error = function(e) NA)
      se <- tryCatch(angmod_dibang$edd$se, error = function(e) NA)

      # Store the results
      angle_model_outputs[[angle_model]] <- list(aic = aic_value, estimate = estimate, se = se)

      # Save angle model outputs to the model_comparison dataframe
      model_comparison <- rbind(model_comparison, data.frame(
        species = sp,
        model_type = "angle",
        model = angle_model,
        aic = aic_value,
        estimate = estimate,
        se = se
      ))
    } else {
      cat("Angle model fitting failed for species:", sp, "\n")
    }
  }

  if (length(angle_model_outputs) > 0) {
    best_angle_model <- names(which.min(sapply(angle_model_outputs, function(x) x$aic)))
    best_angle_result <- angle_model_outputs[[best_angle_model]]
  } else {
    best_angle_model <- NA
    best_angle_result <- list(estimate = NA, se = NA)
  }
}

# Convert results to a dataframe
model_df <- map_dfr(names(model_results), function(sp) {
  map_dfr(names(model_results[[sp]]), function(model) {
    tibble(
      species = sp,
      model = model,
      aic = model_results[[sp]][[model]]$aic,
      estimate = model_results[[sp]][[model]]$estimate,
      se = model_results[[sp]][[model]]$se
    )
  })
})










# # #_______________________________________________________________________________
# # #                             the final loop
# # #________________________________________________________________________________

# As explained above, in this loop we will process the preferred model for each species parameter. The list below can specify the desired model.
# Alternatively, the loop will default to the model with the lowest AIC score. 



# Define species-specific model selections, if required.
radius_model_selection <- list(
  "Hog_deer" = "hn",
  "Indian_hare" = "hr",
  "Buffalo" = "hn"
)

angle_model_selection <- list(
  "Indian_jackal" = "hr",
  "Jungle_fowl" = "hr",
  "Sambar" = "hr"
)



# Initialize an empty list to store results
model_results <- list()

# Loop through each species
for (sp in species_list) {
  cat("Running models for species:", sp, "\n")

  # Camera detection zone estimation
  posdat_dibang$radius <- as.numeric(posdat_dibang$radius)

  dzdat_elopa_5_dibang <- subset(posdat_dibang, frame_count == 1 & species == sp)

  # Determine the radius model to use
  radius_models <- if (sp %in% names(radius_model_selection)) {
    radius_model_selection[[sp]]
  } else {
    c("hn", "hr")
  }

  # Initialize a list to store model outputs
  model_outputs <- list()

  # Loop through each model
  for (model in radius_models) {
    cat("Fitting radius model:", model, "for species:", sp, "\n")

    # Fit the model
    radmod <- tryCatch({
      fitdf(radius ~ 1, dzdat_elopa_5_dibang, transect = "point", order=0, key = model, truncation = 15)
    }, error = function(e) {
      cat("Error fitting radius model:", model, "for species:", sp, "\n", e$message, "\n")
      return(NULL)
    })



    if (!is.null(radmod)) {
      aic_value <- radmod$ddf$criterion
      estimate <- tryCatch(radmod$edd$estimate, error = function(e) NA)
      se <- tryCatch(radmod$edd$se, error = function(e) NA)

      # Store the results
      model_outputs[[model]] <- list(aic = aic_value, estimate = estimate, se = se)
    } else {
      cat("Radius model fitting failed for species:", sp, "\n")
    }
  }

  if (length(model_outputs) > 0) {
    model_results[[sp]] <- model_outputs
  } else {
    cat("No valid radius models for species:", sp, "\n")
  }
}

# Convert results to a dataframe
model_df <- map_dfr(names(model_results), function(sp) {
  map_dfr(names(model_results[[sp]]), function(model) {
    tibble(
      species = sp,
      model = model,
      aic = model_results[[sp]][[model]]$aic,
      estimate = model_results[[sp]][[model]]$estimate,
      se = model_results[[sp]][[model]]$se
    )
  })
})


print("Radius model fitting completed and results saved.")

# Find the best model for each species
best_models <- model_df %>%
  group_by(species) %>%
  filter(aic == min(aic, na.rm = TRUE)) %>%
  slice(1) %>%  # Ensure only one model per species
  dplyr::select(species, model)



# Initialize an empty list to store results
density_results <- list()

# Initialize an empty list to store speeds and other metrics
speeds_list <- list()

# Loop through each species and perform the density calculations
for (sp in species_list) {
  try({
    # Activity level estimation
    actmod_elopa_5 <- fitact(subset(contactdat_test_05, species == sp)$radiantime, sample = "data", reps = 100)

    # Create directory for plots
    plot_dir <- file.path("/Users/harryjobanputra/Documents/PROJECT/Data and Pipelines/Datasets/Winter 2020-2021/Data_prep_outputs/Deployment plots", sp)
    dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

    # Save activity plot
    png(file.path(plot_dir, "activity_plot.png"))
    plot(actmod_elopa_5, centre = "night", dline = list(col = "grey"))
    dev.off()

    # Calculating harmonic mean of speeds for "dibang"
    speeds_elopa_5_dibang <- subset(seqdat_dibang, species == sp)$speed

    # Set a threshold for speed between 0.001 and 10
    min_speed_threshold <- 0.002
    max_speed_threshold <- 10

    # Filter the speed values and remove NAs
    speeds_elopa_5_dibang <- na.omit(speeds_elopa_5_dibang[speeds_elopa_5_dibang > min_speed_threshold & speeds_elopa_5_dibang < max_speed_threshold])

    # If there are no valid speeds, skip to the next species
    if (length(speeds_elopa_5_dibang) == 0 || all(is.na(speeds_elopa_5_dibang))) {
      next
    }

    # Calculate harmonic mean
    spdest_dibang <- hmean(speeds_elopa_5_dibang)

    # Check if spdest has valid mean and se values
    if (is.na(spdest_dibang$mean) || is.na(spdest_dibang$se)) {
      next
    }

    # Save speeds and other metrics to the list
    speeds_list[[sp]] <- list(
      dibang_mean_speed = spdest_dibang$mean,
      dibang_se_speed = spdest_dibang$se,
      dibang_speed_sample_size = length(speeds_elopa_5_dibang)
    )


    # Dibang
    posdat_dibang$radius <- as.numeric(posdat_dibang$radius)
    dzdat_elopa_5_dibang <- subset(posdat_dibang, frame_count == 1 & species == sp)
    dzdat_elopa_5_dibang <- dzdat_elopa_5_dibang[!is.na(dzdat_elopa_5_dibang$radius) & is.finite(dzdat_elopa_5_dibang$radius), ]

    # Use the best model for the current species
    best_model <- best_models %>% filter(species == sp) %>% pull(model)
    radmod_dibang <- fitdf(radius ~ 1, dzdat_elopa_5_dibang, transect = "point", key = best_model, truncation = 15)

    print(radmod_dibang$edd)

    # Save radius plot for dibang
    png(file.path(plot_dir, "radmod_dibang.png"))
    plot(radmod_dibang$ddf, pdf = TRUE)
    dev.off()

    # Angle estimation
    dzdat_elopa_5_dibang$angle <- abs(dzdat_elopa_5_dibang$angle)

    # Determine the angle model to use
    angle_models <- if (sp %in% names(angle_model_selection)) {
      angle_model_selection[[sp]]
    } else {
      c("hn", "hr")
    }

    # Fit both models for angle and select the best
    angle_model_outputs <- list()

    for (angle_model in angle_models) {
      angmod_dibang <- tryCatch({
        fitdf(angle ~ 1, dzdat_elopa_5_dibang, key = angle_model, order = 0)
      }, error = function(e) {
        cat("Error fitting angle model:", angle_model, "for species:", sp, "\n", e$message, "\n")
        return(NULL)
      })

      if (!is.null(angmod_dibang)) {
        aic_value <- angmod_dibang$ddf$criterion
        estimate <- tryCatch(angmod_dibang$edd$estimate, error = function(e) NA)
        se <- tryCatch(angmod_dibang$edd$se, error = function(e) NA)

        # Store the results
        angle_model_outputs[[angle_model]] <- list(aic = aic_value, estimate = estimate, se = se)
      } else {
        cat("Angle model fitting failed for species:", sp, "\n")
      }
    }

    if (length(angle_model_outputs) > 0) {
      best_angle_model <- names(which.min(sapply(angle_model_outputs, function(x) x$aic)))
      best_angle_result <- angle_model_outputs[[best_angle_model]]
    } else {
      best_angle_model <- NA
      best_angle_result <- list(estimate = NA, se = NA)
    }

    # Save angle plot
    if (!is.null(angmod_dibang$ddf$Nhat)) {
      png(file.path(plot_dir, "angmod_dibang.png"))
      plot(angmod_dibang$ddf)
      dev.off()
    }

    # Save radius and angle to the list
    speeds_list[[sp]]$dibang_effective_radius <- radmod_dibang$edd$estimate
    speeds_list[[sp]]$dibang_radius_se <- radmod_dibang$edd$se
    speeds_list[[sp]]$dibang_effective_angle <- best_angle_result$estimate
    speeds_list[[sp]]$dibang_angle_se <- best_angle_result$se
    speeds_list[[sp]]$best_radius_model <- best_model
    speeds_list[[sp]]$best_angle_model <- best_angle_model

    # Specify mean value in spdtest list:
    mean_value <- spdest_dibang[["mean"]]

    # Specify value in spdest list:
    se_value <- spdest_dibang[["se"]]

    # Density calculation
    param_dibang <- list(
      v = mean_value * 14 * 60^2 / 1000,
      p = actmod_elopa_5@act["act"],
      r = radmod_dibang$edd$estimate / 1000,
      theta = best_angle_result$estimate * 2
    )

    paramse_dibang <- list(
      v = se_value * 14 * 60^2 / 1000,
      p = actmod_elopa_5@act["se"],
      r = radmod_dibang$edd$se / 1000,
      theta = best_angle_result$se * 2
    )

    density_dibang <- bootTRD(trdat[, sp], trdat$effort.days, param_dibang, paramse_dibang)

    # Save results in the list
    density_results[[sp]] <- list(dibang = density_dibang)
  }, silent = TRUE)
}

# Convert results to a dataframe and write to a CSV file
density_df <- do.call(rbind, lapply(names(density_results), function(sp) {
  data.frame(
    species = sp,
    density_dibang = density_results[[sp]]$dibang
  )
}))

# Convert speeds and other metrics to a dataframe and write to a CSV file
speeds_df <- do.call(rbind, lapply(names(speeds_list), function(sp) {
  if (is.null(speeds_list[[sp]]$dibang_mean_speed) ||
      is.null(speeds_list[[sp]]$dibang_se_speed) ||
      is.null(speeds_list[[sp]]$dibang_effective_radius) ||
      is.null(speeds_list[[sp]]$dibang_effective_angle)) {
    return(NULL)
  }
  data.frame(
    species = sp,
    dibang_mean_speed = speeds_list[[sp]]$dibang_mean_speed,
    dibang_se_speed = speeds_list[[sp]]$dibang_se_speed,
    dibang_speed_sample_size = speeds_list[[sp]]$dibang_speed_sample_size,
    dibang_effective_radius = speeds_list[[sp]]$dibang_effective_radius,
    dibang_radius_se = speeds_list[[sp]]$dibang_radius_se,
    dibang_effective_angle = speeds_list[[sp]]$dibang_effective_angle,
    dibang_angle_se = speeds_list[[sp]]$dibang_angle_se,
    best_radius_model = speeds_list[[sp]]$best_radius_model,
    best_angle_model = speeds_list[[sp]]$best_angle_model
  )
}))

# Remove NULL rows (if any)
speeds_df <- speeds_df[!sapply(speeds_df, is.null), ]

# Combine density_df and speeds_df with model_df to create all_info
all_info <- speeds_df %>%
  left_join(density_df, by = "species") %>%
  left_join(model_df, by = "species")

best_models_all_spec <- all_info %>%
  group_by(species) %>%
  slice(which.min(aic)) %>%
  ungroup()


# Initialize an empty list to store activity levels
activity_levels <- list()

# Loop through each species and perform the activity level estimation
for (sp in species_list) {
  try({
    # Activity level estimation
    actmod_elopa_5 <- fitact(subset(contactdat_test_05, species == sp)$radiantime, sample = "data", reps = 100)

    # Save activity level to the list
    activity_levels[[sp]] <- list(
      activity_level = actmod_elopa_5@act["act"],
      activity_se = actmod_elopa_5@act["se"],
      activity_lcl = actmod_elopa_5@act["lcl.2.5%"],
      activity_ucl = actmod_elopa_5@act["ucl.97.5%"],
      activity_sample_size = length(subset(contactdat_test_05, species == sp)$radiantime) # Save the sample size
    )
  }, silent = TRUE)
}

# Convert activity levels to a dataframe
activity_df <- do.call(rbind, lapply(names(activity_levels), function(sp) {
  data.frame(
    species = sp,
    activity_level = activity_levels[[sp]]$activity_level,
    activity_se = activity_levels[[sp]]$activity_se,
    activity_lcl = activity_levels[[sp]]$activity_lcl,
    activity_ucl = activity_levels[[sp]]$activity_ucl,
    activity_sample_size = activity_levels[[sp]]$activity_sample_size # Include the sample size in the dataframe
  )
}))

# Add radius and angle sample size
radius_sample_sizes <- list()
angle_sample_sizes <- list()

# Loop through each species and collect radius and angle sample sizes
for (sp in species_list) {
  try({
    # Collect radius sample size
    dzdat_elopa_5_dibang <- subset(posdat_dibang, frame_count == 1 & species == sp)
    dzdat_elopa_5_dibang <- dzdat_elopa_5_dibang[!is.na(dzdat_elopa_5_dibang$radius) & is.finite(dzdat_elopa_5_dibang$radius), ]
    radius_sample_size <- nrow(dzdat_elopa_5_dibang)
    radius_sample_sizes[[sp]] <- radius_sample_size

    # Collect angle sample size
    dzdat_elopa_5_dibang$angle <- abs(dzdat_elopa_5_dibang$angle)
    angle_sample_size <- nrow(dzdat_elopa_5_dibang)
    angle_sample_sizes[[sp]] <- angle_sample_size
  }, silent = TRUE)
}

# Convert radius and angle sample sizes to dataframes
radius_sample_size_df <- do.call(rbind, lapply(names(radius_sample_sizes), function(sp) {
  data.frame(
    species = sp,
    radius_sample_size = radius_sample_sizes[[sp]]
  )
}))

angle_sample_size_df <- do.call(rbind, lapply(names(angle_sample_sizes), function(sp) {
  data.frame(
    species = sp,
    angle_sample_size = angle_sample_sizes[[sp]]
  )
}))

# Combine all dataframes using full join to handle different sizes
new_info_final <- speeds_df %>%
  full_join(best_models_all_spec, by = "species") %>%
  full_join(radius_sample_size_df, by = "species") %>%
  full_join(angle_sample_size_df, by = "species") %>%
  full_join(activity_df, by = "species") %>%
  mutate(best_angle_model = best_angle_model) # Add the best angle model to the final dataframe

# Write the final combined dataframe to a CSV file
new_info_final$Year <- "2024"
new_info_final$best_radius_model <- new_info_final$model
new_info_final$model <- NULL

# write.csv(new_info_final, file = "/Users/harryjobanputra/Documents/ZSL - Research assistant/WP2 - Grassland Elopa/Handover_Grassland/Results/2024_density_results.csv", row.names = FALSE)






