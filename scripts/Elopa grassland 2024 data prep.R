#________________________________________________________________________________________________________________________________________________
#                                             Packages
#________________________________________________________________________________________________________________________________________________

# Function to check & install packages
install_and_load <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
    library(package_name, character.only = TRUE)
  }
}

# List of packages you need, excluding dplyr for now
packages <- c("lubridate", "stringr", "ggplot2", "readxl", "readr", "tidyr", "devtools", "writexl", "dplyr", "camtraptor", "tidyr", "camtrapDensity")

# Apply the function
invisible(lapply(packages, install_and_load))

# Load dplyr last to give its functions priority
install_and_load("dplyr")


# Load external packages
devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/camtools/master/get_detection_matrix.r")
devtools::source_url("https://raw.githubusercontent.com/MarcusRowcliffe/CTtracking/V0.3.2/CTtracking.r")



#________________________________________________________________________________________________________________________________________________
#                                             Read in cam calibration & deployment files
#________________________________________________________________________________________________________________________________________________

# cam calibration
raw_dibang_browning_calib <- read.csv("https://raw.githubusercontent.com/harryjobann/WP2-Elopa-grassland/refs/heads/main/data/Camera_Calibration_digitization.csv")
# cam placements
cam_placements <- read.csv("https://raw.githubusercontent.com/harryjobann/WP2-Elopa-grassland/refs/heads/main/data/cleaned_deployments.csv")
# tagging data
tagging_df <- read.csv("https://raw.githubusercontent.com/harryjobann/WP2-Elopa-grassland/refs/heads/main/data/2024_tagging_df.csv")
# tracking data
elopa_5_positions <- read.csv("https://raw.githubusercontent.com/harryjobann/WP2-Elopa-grassland/refs/heads/main/data/2021_tracking_df.csv")



#________________________________________________________________________________________________________________________________________________
#                                             Tracking df
#________________________________________________________________________________________________________________________________________________

# Doing some final transformations/renaming
elopa_5_positions$Location_ID <- elopa_5_positions$Location.ID
elopa_5_positions$species <- elopa_5_positions$name
elopa_5_positions$name <- NULL 
elopa_5_positions$Location.ID <- NULL

# Check unique deployments and species
unique(elopa_5_positions$Location_ID)
unique(elopa_5_positions$species)



#__________________________________________________________________________________________#
#                                             Cleaning: Location_ID
#__________________________________________________________________________________________#

#Standardising location id
elopa_5_positions$Location_ID <- sub(" ", "", elopa_5_positions$Location_ID)
elopa_5_positions$Location_ID <- sub("-", "", elopa_5_positions$Location_ID)


# Count the occurrences of each deployment
location_counts <- table(elopa_5_positions$Location_ID)

# Convert to a data frame for plotting
location_counts_df <- as.data.frame(location_counts)
colnames(location_counts_df) <- c("Location.ID", "Count")

# Sort the data frame by count in descending order
location_counts_df <- location_counts_df[order(-location_counts_df$Count), ]

# Calculate the total count and the number of unique Location.IDs
total_count <- sum(location_counts_df$Count)
unique_location_count <- nrow(location_counts_df)


# Plot the counts
ggplot(location_counts_df, aes(x = reorder(Location.ID, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3) +
  labs(title = "Count of Each Location ID", x = "Location ID", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = Inf, y = Inf, label = paste("Total count:", total_count),
           hjust = 1.1, vjust = 2, size = 4, color = "blue") +
  annotate("text", x = Inf, y = Inf, label = paste("Unique Location IDs:", unique_location_count),
           hjust = 1.1, vjust = 3.5, size = 4, color = "blue")


# #__________________________________________________________________________________________#
# #                                             Cleaning: Types 
# #__________________________________________________________________________________________#

unique(elopa_5_positions$Type)


elopa_5_positions <- elopa_5_positions %>%
  dplyr::mutate(Type = dplyr::recode(Type, "Trigger point" = "trigger"))

elopa_5_positions <- elopa_5_positions %>%
  dplyr::mutate(Type = dplyr::recode(Type, "Trigger_point" = "trigger"))

elopa_5_positions <- elopa_5_positions %>%
  dplyr::mutate(Type = dplyr::recode(Type, "Calibration-Top" = "Top"))


elopa_5_positions <- elopa_5_positions %>%
  dplyr::mutate(Type = dplyr::recode(Type, "Calibration-Bottom" = "Bottom"))


# Check to confirm the replacement
unique(elopa_5_positions$Type)



# #__________________________________________________________________________________________#
# #                           Cleaning: Ensure sequence_id is correct
# #__________________________________________________________________________________________#

elopa_5_positions$Location.ID <- elopa_5_positions$Location_ID
elopa_5_positions$Location_ID <- NULL

# Appending Location.ID to sequence_id if Location.ID is not empty
elopa_5_positions <- elopa_5_positions %>%
  mutate(
    sequence_id = ifelse(
      Location.ID != "",
      paste0(sequence_id, "_", Location.ID),
      as.character(sequence_id)
    )
  )

# Converting sequence_id to numeric by assigning unique numbers
elopa_5_positions <- elopa_5_positions %>%
  mutate(sequence_id = as.numeric(factor(sequence_id, levels = unique(sequence_id))))

# Creating a copy of the dataframe for further work
elopa_5_positions1 <- elopa_5_positions

# Re-assigning new sequence_ids where one sequence has multiple species
assign_new_ids <- function(df) {
  # Add a column to keep track of the old sequence_id values
  df <- df %>%
    mutate(old_seq_id = sequence_id)

  # Identify sequence_ids with multiple species
  multi_species_ids <- df %>%
    group_by(sequence_id) %>%
    filter(n_distinct(species) > 1) %>%
    pull(sequence_id) %>%
    unique()

  # Initialize a counter for new sequence_ids
  new_id_counter <- 10000

  # Assign new sequence_id for each unique species within the same sequence_id
  for (seq_id in multi_species_ids) {
    species <- df %>%
      filter(sequence_id == seq_id) %>%
      dplyr::select(species) %>%
      distinct() %>%
      pull(species)

    for (spec in species) {
      df <- df %>%
        mutate(sequence_id = ifelse(sequence_id == seq_id & species == spec, new_id_counter, sequence_id))
      new_id_counter <- new_id_counter + 1
    }
  }

  return(df)
}

# Apply the function
elopa_5_positions <- assign_new_ids(elopa_5_positions)

# Validate results
species_count_per_sequence <- elopa_5_positions %>%
  group_by(sequence_id) %>%
  summarise(species_count = n_distinct(species)) %>%
  as.data.frame()

# Identify sequences that still have multiple species
sequences_multiple_species <- species_count_per_sequence %>%
  filter(species_count > 1)

# Check for any NA values in the sequence_id column
print(na_end <- elopa_5_positions %>% filter(is.na(sequence_id)))



#________________________________________________________________________________________________________________________________________________
#                                             Tagging df
#________________________________________________________________________________________________________________________________________________

exifdat_test_05 <- tagging_df


# #__________________________________________________________________________________________#
# #                           Cleaning tagging df
# #__________________________________________________________________________________________#

exifdat_test_05 <- df

exifdat_test_05$'Date and Time' <- exifdat_test_05$Date.and.Time

exifdat_test_05 <- exifdat_test_05 %>%
  dplyr::select(-tag10, -tag9, -tag8, -tag7, -tag6, -tag5)


exifdat_test_05$Name <- exifdat_test_05$species

# Count the occurrences of each Name
name_counts <- table(exifdat_test_05$Name)

# Convert to a data frame for plotting
name_counts_df <- as.data.frame(name_counts)
colnames(name_counts_df) <- c("Name", "Count")

# Sort the data frame by count in descending order
name_counts_df <- name_counts_df[order(-name_counts_df$Count), ]

# Calculate the total count and the number of unique Names
total_count <- sum(name_counts_df$Count)
unique_name_count <- nrow(name_counts_df)


#__________________________________________________________________________________________#
#                                       Cleaning: species
#__________________________________________________________________________________________#


# Plot the counts
ggplot(name_counts_df, aes(x = reorder(Name, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3) +
  labs(title = "Count of Each Name", x = "Name", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = Inf, y = Inf, label = paste("Total count:", total_count),
           hjust = 1.1, vjust = 2, size = 4, color = "blue") +
  annotate("text", x = Inf, y = Inf, label = paste("Unique Names:", unique_name_count),
           hjust = 1.1, vjust = 3.5, size = 4, color = "blue")



unique(exifdat_test_05$Name)


#__________________________________________________________________________________________#
#                   #Drop unneeded entries
#__________________________________________________________________________________________#

# Specify the exact entries to remove
entries_to_remove <- c("b-Misfire", "b-Team_members", "b-People", "b-dog")

# Filter out the unwanted rows by exact match and overwrite the existing dataframe
exifdat_test_05 <- dplyr::filter(exifdat_test_05, !Name %in% entries_to_remove)

# Check the unique values after filtering to confirm
unique(exifdat_test_05$Name)

#Drop "b-" at the start of each species name:
exifdat_test_05$Name <- sub("b-", "", exifdat_test_05$Name)
exifdat_test_05$Name <- sub(" ", "", exifdat_test_05$Name)
exifdat_test_05$Name <- sub("indian_hare", "Indian_hare", exifdat_test_05$Name)
exifdat_test_05$Name <- sub("Nightjars", "Nightjar", exifdat_test_05$Name)
exifdat_test_05$Name <- sub("jungle_cat", "Jungle_cat", exifdat_test_05$Name)


unique(exifdat_test_05$Name)



#__________________________________________________________________________________________#
#                                       Cleaning: Tags column
#__________________________________________________________________________________________#

#create a copy of the df to work on
exifdat_test_05_alt <- exifdat_test_05
exifdat_test_05 <- exifdat_test_05_alt

#Rename col
exifdat_test_05$Tags <- exifdat_test_05$station
unique(exifdat_test_05$Tags)

#Change structure
exifdat_test_05$Tags <- sub("a-", "", exifdat_test_05$Tags)
exifdat_test_05$Tags <- sub("  ", "", exifdat_test_05$Tags)
exifdat_test_05$Tags <- sub(" ", "", exifdat_test_05$Tags)

#Check successful
unique(exifdat_test_05$Tags)


# Count the occurrences of each Tag
tag_counts <- table(exifdat_test_05$Tags)

# Count the NA values
na_count <- sum(is.na(exifdat_test_05$Tags))

# Convert to a data frame for plotting
tag_counts_df <- as.data.frame(tag_counts)
colnames(tag_counts_df) <- c("Tag", "Count")

# Add the NA count to the data frame
if (na_count > 0) {
  tag_counts_df <- rbind(tag_counts_df, data.frame(Tag = "NA", Count = na_count))
}

# Sort the data frame by count in descending order
tag_counts_df <- tag_counts_df[order(-tag_counts_df$Count), ]

# Calculate the total count and the number of unique Tags
total_count <- sum(tag_counts_df$Count)
unique_tag_count <- nrow(tag_counts_df)

# Plot the counts
ggplot(tag_counts_df, aes(x = reorder(Tag, -Count), y = Count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.3) +
  labs(title = "Count of Each Tag", x = "Tag", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  annotate("text", x = Inf, y = Inf, label = paste("Total count:", total_count),
           hjust = 1.1, vjust = 2, size = 4, color = "blue") +
  annotate("text", x = Inf, y = Inf, label = paste("Unique Tags:", unique_tag_count),
           hjust = 1.1, vjust = 3.5, size = 4, color = "blue")

unique(exifdat_test_05$Tags)

######## AT THIS STAGE - STOP AND MANUALLY CHECK IF ANY OF THE RAW TXT FILES HAVE NOT BEEN INCLUDED IN YOUR DF.




#__________________________________________________________________________________________#
#                                       Cleaning: Date and time
#__________________________________________________________________________________________#
#Create copy of df to work on 
exifdat_test_04 <- exifdat_test_05
exifdat_test_05 <- exifdat_test_04


# Step 1: change format
exifdat_test_05$Date.and.Time <- as.POSIXct(exifdat_test_05$Date.and.Time, format = "%d-%m-%Y %H:%M:%S", tz = "UTC")

# Check for any NA values introduced due to format mismatches
na_dates <- exifdat_test_05 %>% filter(is.na(Date.and.Time))
if (nrow(na_dates) > 0) {
  print("Some dates could not be parsed due to format inconsistencies:")
  print(na_dates$Date.and.Time)
}

# Step 2: Extract the time component as a proportion of the day
exifdat_test_05$Time_in_Day <- as.numeric(format(exifdat_test_05$Date.and.Time, format = "%H")) * 3600 +
  as.numeric(format(exifdat_test_05$Date.and.Time, format = "%M")) * 60 +
  as.numeric(format(exifdat_test_05$Date.and.Time, format = "%S"))

# Step 3: Convert time to radians (time in seconds / total seconds in a day * 2*pi)
exifdat_test_05$radiantime <- (exifdat_test_05$Time_in_Day / 86400) * 2 * pi


exifdat_test_05$Time_in_Day <- NULL


#rename columns
names(exifdat_test_05)[names(exifdat_test_05) == "Photo"] <- "SourceFile"
names(exifdat_test_05)[names(exifdat_test_05) == "Date.and.Time"] <- "DateTimeOriginal"
names(exifdat_test_05)[names(exifdat_test_05) == "Species"] <- "species"
exifdat_test_05$date <- exifdat_test_05$DateTimeOriginal


exifdat_test_05 <- exifdat_test_05 %>%
  dplyr::select(-Exp..Time, -F.Stop, -Exp..Bias, -Metering.Mode, 
         -Light.Source, -Flash, -Focal.Length, -ISO.Speed, 
         -Orientation, -Dimensions, -Exp..Program, -Focal.Length..35.mm.)



#__________________________________________________________________________________________#
#                                       Cleaning: Contacts col
#__________________________________________________________________________________________#
#renaming col
exifdat_test_05$new.contact.adult <- exifdat_test_05$new_contact_adult

#Change format
exifdat_test_05$new.contact.adult <- sub(" ", "", exifdat_test_05$new.contact.adult)

# Count the number of occurrences of each unique value in the 'New.contact.adult.juvenile' column
value_counts <- table(exifdat_test_05$new.contact.adult)

# Print the counts
print(value_counts)

#Create list of vals which equate to new adult contact 
exifdat_test_05$contact <- ifelse(exifdat_test_05$new.contact.adult %in% c("d-1", "d-2", "d-3", "d-4","d-5","d-6","d-7","d-8","d-9","d-10", "d-11", "d-12", "d-13", "d-19"), 1, NA)

#count the contacts
contact_counts <- table(exifdat_test_05$contact, useNA = "ifany")
print(contact_counts)

#rename col again
exifdat_test_05$station <- exifdat_test_05$Tags

# Check to confirm the replacement
unique(exifdat_test_05$station)
elopa_5_positions$Location_ID <- elopa_5_positions$Location.ID
elopa_5_positions$Location.ID <- NULL
unique(elopa_5_positions$Location_ID)





# #________________________________________________________________________________________________________________________________________________
# #                                             Deployments df 
# #________________________________________________________________________________________________________________________________________________

#rename df
deptab_test_05 <- cam_placements 

# rename cols
deptab_test_05$station <- deptab_test_05$LOCATION.ID

# edit formatting
deptab_test_05$station <- sub(" ", "", deptab_test_05$station)



unique(deptab_test_05$station)


# Stop times for the deployments are missing, so setting them to midday as a default value.
deptab_test_05$Time.stop <- "12:00 PM"


deptab_test_05 <- deptab_test_05 %>%
  dplyr::select(-PlacementDATE, 
          -Third.deployment, 
         -CAM..ID, -LOCATION.ID)


deptab_test_05$start <- with(deptab_test_05, 
                             as.POSIXct(paste(deployment_date, deployment_time), format = "%d/%m/%Y %I:%M %p"))


deptab_test_05$stop <- with(deptab_test_05, 
                             as.POSIXct(paste(Removal.date, Time.stop), format = "%d/%m/%Y %I:%M %p"))




# Subset the dataframe to include only the necessary columns.
deptab_test_05 <- deptab_test_05[, c("station", "start", "stop","Trapnights", "Notes")]

# filter NAs
deptab_test_05 <- deptab_test_05 %>% filter(!is.na(start))

# rename col
deptab_test_05$Trap_nights <- deptab_test_05$Trapnights


#________________________________________________________________________________________________________________________________________________
#                                             Cam calibration models
#________________________________________________________________________________________________________________________________________________

# rename df 
dibang_browning_calib <- raw_dibang_browning_calib

# Set middle and top heights of pole based on those used in the field
dibang_browning_calib$height[grepl("top", dibang_browning_calib$name)] <- 0.9
dibang_browning_calib$height[grepl("middle", dibang_browning_calib$name)] <- 0.5


# Adding in necessary cols so the function progresses
dibang_browning_calib$folder <- 0
dibang_browning_calib$ImageWidth <- 3712
dibang_browning_calib$ImageHeight <- 2088

# Pairup the calibration points, one for the top and the bottom of each pole
dibang_browning_calib <- pairup(dibang_browning_calib, pair=c("folder", "pair_id"))


cmods_dibang_browning <- cal.cam(dibang_browning_calib, "folder")




#__________________________________________________________________________________________#
#                                       Deployment calib models
#__________________________________________________________________________________________#

#rename col
elopa_5_positions$name <- elopa_5_positions$species

# Deployment calibration
deploy_calib_images <- elopa_5_positions
deploy_calib_images <- elopa_5_positions[elopa_5_positions$species == "Calibration", ]
deploy_calib_images$Location.ID <- deploy_calib_images$Location_ID
deploy_calib_images$folder <- 0


# Amend distances based on Sahil's input
deploy_calib_images$height[grepl("Top", deploy_calib_images$Type)] <- 0.9
deploy_calib_images$height[grepl("Bottom", deploy_calib_images$Type)] <- 0.4

# rename col
deploy_calib_images$species <- deploy_calib_images$name


# Create copy of the df to work on 
df <- deploy_calib_images
deploy_calib_images <- df


#__________________________________________________________________________________________#
#                     Dropping erroneous points from the calibration for AK22 & AK4
#__________________________________________________________________________________________#

# filter for all points for these deployments to manually check
err_points <- deploy_calib_images %>% filter(Location_ID %in% c("AK22", "AK4"))


# Create df of calibration data
caldat <- pairup(subset(deploy_calib_images, species=="Calibration"),
                 pairtag="image_name")


# filter out erroneous points 
caldat <- caldat %>% filter(image_name !=  "IMG_0196.JPG")
caldat <- caldat %>% filter(image_name !=  "IMG_0030.JPG")
caldat <- caldat %>% filter(image_name !=  "IMG_0073.JPG")
caldat <- caldat %>% filter(image_name !=  "IMG_0129.JPG")


# # Adding required cols to caldat
caldat$ImageWidth <- 3712
caldat$ImageHeight <- 2088
caldat$deployment <- caldat$Location.ID


# Deployment calib for dibang
dmods_dibang <- cal.dep(caldat, cmods=cmods_dibang_browning, deptag="deployment", lookup=deptab_test_05)


# Plots
print(plot(dmods_dibang))


# Define the directory to save the plots
plot_dir <- "/Users/harryjobanputra/Documents/ZSL - Research assistant/WP2 - Grassland Elopa/Handover_Grassland/Results/deployment_calibration_models"



# Save each plot for each deployment in dibang
for (i in seq_along(dmods_dibang)) {
  deployment_name <- names(dmods_dibang)[i]
  
  # Save the first plot
  png(file.path(plot_dir, paste0("dmods_dibang_", deployment_name, "_1.png")))
  plot(dmods_dibang[[i]])  # First plot
  dev.off()
  
}




#________________________________________________________________________________________________________________________________________________
#                                             Generating "Posdat": animal position coordinate data 
#________________________________________________________________________________________________________________________________________________

#rename col
elopa_5_positions$Location.ID <- elopa_5_positions$Location_ID


#Adding an x, y, ImageWidth and ImageHeight, and deployment to exifdat_test_05
exifdat_test_05$ImageWidth <- 3712
exifdat_test_05$ImageHeight <- 2088
exifdat_test_05$deployment <- exifdat_test_05$station
elopa_5_positions$species <- elopa_5_positions$name
elopa_5_positions$deployment <- elopa_5_positions$Location.ID
elopa_5_positions$ImageHeight <- 2088
elopa_5_positions$ImageWidth <- 3712

# Check any entries without a deployment listed
na_elopa_5_positions <- elopa_5_positions %>% filter(is.na(Location.ID))

#rename
elopa_5_positions$deployment <- elopa_5_positions$Location.ID


### Now, generating "posdat" - position data of animals, using the deployment models
animdat <- subset(elopa_5_positions, species!="Calibration")


unique(animdat$species)


#Drop location.id 
animdat$Location.ID <- NULL


# rename
animdat$Location.ID <- animdat$deployment


# Check for any leading/trailing spaces in animdat$deployment
animdat$deployment <- trimws(animdat$deployment)

# Ensure that deployment IDs in animdat and dmods_dibang are identical
animdat$deployment <- toupper(animdat$deployment)  # Convert to uppercase for consistency
names(dmods_dibang) <- toupper(names(dmods_dibang))  # Same for dmods_dibang



# Run the function
posdat_dibang <- predict.pos(animdat, dmods_dibang)



# #________________________________________________________________________________________________________________________#
#                     #Investigating abnormal radius value for indian hare. 
# #________________________________________________________________________________________________________________________#

# Filter out ak_23 and 26 from indian hare entries as radius values exceptionally high 
posdat_dibang <- posdat_dibang %>% 
  dplyr::filter(!(species == "Indian_hare" & Location.ID %in% c("AK23", "AK26")))

# Filter out infinite vals
posdat_dibang <- posdat_dibang %>% 
  dplyr::filter(!is.infinite(radius))

# Check results
posdat_hare <- posdat_dibang %>% filter(species == "Indian_hare")
mean(posdat_hare$radius)


# #________________________________________________________________________________________________________________________#
#                           Generating "Seqdat" : animal speeds
# #________________________________________________________________________________________________________________________#

# Check all species & deployments
unique(posdat_dibang$species)
print(unique(posdat_dibang$deployment))

# rename
posdat_dibang$Sequence.number <- posdat_dibang$sequence_id

# run the function
seqdat_dibang <- seq.summary(posdat_dibang)

# rename
posdat_dibang$name <- posdat_dibang$species
seqdat_dibang$name <- seqdat_dibang$species
exifdat_test_05$species <- exifdat_test_05$Name



#________________________________________________________________________________________________________________________#
#        2024: Write all df's to an output folder
#________________________________________________________________________________________________________________________


# # #exifdat
# write.csv(exifdat_test_05, file = '/Users/harryjobanputra/Documents/ZSL - Research assistant/WP2 - Grassland Elopa/data_for_scripts/exifdat_test_05.csv', row.names = FALSE)
# 
# #deptab
# write.csv(deptab_test_05, file = '/Users/harryjobanputra/Documents/ZSL - Research assistant/WP2 - Grassland Elopa/data_for_scripts/deptab_test_05.csv', row.names = FALSE)
# 
# 
# # #posdat
# write.csv(posdat_dibang, file = '/Users/harryjobanputra/Documents/ZSL - Research assistant/WP2 - Grassland Elopa/data_for_scripts/posdat_dibang.csv', row.names = FALSE)
# 
# 
# # seqdat
# write.csv(seqdat_dibang, file = '/Users/harryjobanputra/Documents/ZSL - Research assistant/WP2 - Grassland Elopa/data_for_scripts/seqdat_dibang.csv', row.names = FALSE)


