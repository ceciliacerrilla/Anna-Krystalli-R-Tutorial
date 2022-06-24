library(usethis)
library(here)
library(dplyr)
library(assertr)

# Download data from the NEON repository and save materials into 'data-raw' folder

usethis::use_course("bit.ly/wood-survey-data",
           destdir = "data-raw")


# Create a path to the downloaded directory

raw_data_path <- here::here("data-raw", "wood-survey-data-master")


# Use raw_data_path as basis for specifying paths to files within it using package fs()

fs::path(raw_data_path, "individual")


# Get a character vector of paths to all the individual files in directory 'individual'

individual_paths <- fs::dir_ls(fs::path(raw_data_path, "individual"))
head(individual_paths) # look at first few file names
length(individual_paths) # check # of files


# Read in first file and check it out

indiv_df <- readr::read_csv(individual_paths[1])


# Perform basic checks

View(indiv_df) # view data in new tab
names(indiv_df) # column names
str(indiv_df) # look at the structure of the variables (type of objects)
summary(indiv_df) # str summarised


# Now read in all the files and combine them together (iterating using 'for' loops). Apply a loop to read in all 67 files at once.

# Create an output vector to store the tibbles containing the read in data

indiv_df_list <- vector("list", length(individual_paths)) 
head(indiv_df_list)

# Create a sequence of indices as long as the input vector (individual_paths)

seq_along(individual_paths)

# Write the 'for' loop. At each step of the iteration, the file specified in the ith element of individual_paths is read in and assigned to th ith element of our output list.

for(i in seq_along(individual_paths)){
  indiv_df_list[[i]] <- readr::read_csv(individual_paths[i])
}

# We can extract individual tibbles using [[ sub-setting to inspect:

indiv_df_list[[1]]
indiv_df_list[[2]]

# Inspect the contents of our output list interactively

View(indiv_df_list)


# Collapsing our output list into a single tibble using Tidyverse

purrr::reduce(indiv_df_list, .f = rbind)


# Using functional programming instead of loops to collapse the data (why? allows us to replace many for loops with code that is more succinct and easier to read). Use function purrr

# Replace our 'for' loop with a function. 
# The first argument to map is the input vector of paths we want to iterate over. The next argument is a formula specifying the function we want to repeat as well as which argument the input is passed to. 
# Here we’re saying that we want to repeatedly run read_csv and we indicate the argument we want the input passed to (file) by .x. Note as well the ~ notation before the function definition which is shorthand for .f =.

indiv_df_list <- purrr::map(individual_paths,
           ~readr::read_csv(file = .x))

# Because we know the output of read_csv() is a tibble, we can use map_df() instead of map().

individual <- purrr::map_df(individual_paths,
           ~readr::read_csv(.x))



# Save this file out to our wood-survey-data-master folder

individual %>%
  readr::write_csv(file.path(raw_data_path, "vst_individual.csv"))


# Merging data.
# Example: Geolocate every individual. Currently, only the plot is geolocated, and the individuals are only located in reference to the plot location. To geolocate our individuals, we need to join information from vst_perplotperyear.csv and vst_mappingandtagging.csv into our 'individuals' tibble
# We do this using joins. In order to use joins, the files in question must be linked by sharing a variable name (e.g. 'SiteName')

# Let's start by merging data from vst_mappingandtagging.csv.

# Read the data in

maptag <- readr::read_csv(fs::path(raw_data_path, "vst_mappingandtagging.csv"))
names(maptag)

# Check which column names in individual have matches inn maptag column names:

names(individual)[names(individual) %in% names(maptag)]

# Because we want to match the rest of the tables to our individual data, we use left_join() and supply individual as the first argument and maptag as the second. This results in a tibble with all the individual entries (rows) of our individual tibble, but now containing the extra information (columns) from maptag. They've been joined by virtue of sharing the following variables "uid", "eventID" and "individualID"

individual %>%
  dplyr::left_join(maptag) 

# To check whether things have worked, we can start with inspecting the output for the columns of interest, in this case the maptag columns we are trying to join into individual.
# When working interactively and testing out pipes, you can pipe objects into View() for quick inspection. If you provide a character string as an argument, it is used as a name for the data view tab it launches

individual %>%
  dplyr::left_join(maptag) %>%
  View("default")

# All the data under the new columns from maptag are blank (listed as NA), so this has not worked. Basically, it failed to join the data.
# We know that the only column we are interested in matching on is individualID. We want to get the mapping associated with each individual, regardless of when the mapping was collected. We can be specific about which variables we want to join on through argument 'by'.

individual %>%
  dplyr::left_join(maptag, 
                   by = "individualID") %>% # this is where you specify the variable you want to match by
  assertr::assert(assertr::not_na, stemDistance, stemAzimuth, pointID) %>%
  View("default")

# However, on closer inspection, we’ve ended up with some odd new columns, uid.x and uid.y and eventID.x and eventID.y!
# That’s because those columns are also present in both our tables but we are not explicitly joining them. They are retained and each suffixed with .x & .y by default, to make them unique.
# So, what about these duplicate columns. Do we need them?
# With respect to eventID, we’re not really interested in the mapping eventIDs so we can just drop that column from maptag.

maptag <- select(maptag, -eventID)

individual %>%
  dplyr::left_join(maptag, 
                   by = "individualID") %>%
  assertr::assert(assertr::not_na, stemDistance, stemAzimuth, pointID) %>%
  View("default")

# On the other hand, "uid contains unique identifiers for each observation in their respective table and could be useful metadata to store, enabling us to trace the provenance of individual values to the original data. So rather than remove them, let’s retain both uid, one for each table.
# We can give more informative suffixes using argument suffix. In our case, I want the individual column to stay as uid and the maptag column to get the suffix _map.

individual %>%
  dplyr::left_join(maptag, 
                   by = "individualID",
                   suffix = c("", "_map")) %>%
  assertr::assert(assertr::not_na, stemDistance, stemAzimuth, pointID)


# Now let’s carry on and join the perplot data. First let’s read it in.

perplot <- readr::read_csv(fs::path(raw_data_path, "vst_perplotperyear.csv"))
names(perplot)

# Similarly to maptag, we want to exclude eventID and suffix the uid column. This time, however, we will be joining by plotID
# Let’s also move our validation test to the end and add the new columns we want to check to it, i.e. stemDistance, stemAzimuth, pointID.

perplot <- perplot %>% select(-eventID) # Exclude perplot's eventID column

individual %>%
  dplyr::left_join(maptag, 
                   by = "individualID", # what we did before (joined maptag data by individualID)
                   suffix = c("", "_map")) %>%
  dplyr::left_join(perplot, by = c("plotID"),  # our new join (joining perplot data by plotID)
                   suffix = c("", "_ppl")) %>%
  assertr::assert(assertr::not_na,  decimalLatitude, # check that no NAs are found in new tibble
                  decimalLongitude, plotID, stemDistance, stemAzimuth, pointID)

# Awesome!! It’s worked!
# Now that we are happy with our data we can use a new operator, the assignment pipe (%<>%).
# This allows us to both pipe an object forward into an expression and also update it with the resulting value.

individual %<>% # Here's the new operator
  dplyr::left_join(maptag, 
                   by = "individualID",
                   suffix = c("", "_map")) %>%
  dplyr::left_join(perplot, by = c("plotID"), 
                   suffix = c("", "_ppl")) %>%
  assertr::assert(assertr::not_na,  decimalLatitude,
                  decimalLongitude, plotID, stemDistance, stemAzimuth, pointID)


# We can now move on to geolocate our individuals with functions!

# A simple skeleton of a function:

name <- function(variables){

}

# Example:

add <- function(x,y) {
  x+y
}

x <- 4
y <- 2
add(x,y)

# It’s best to store functions in separate scripts in the R/ directory.
# We can use function usethis::use_r() to create scripts in R/. Let’s create a new one to start working on our function.

usethis::use_r("geolocate")

# Let’s add the code to source our function so it’s available during preprocessing:
source(here::here("R", "geolocate.R"))

# Not working... lost the thread at "Making new Variables" under "Functions" section

# Now we want to use data in individual to geolocate our individulas while at the same time creating new columns stemLat and stemLon.
# We also need to extract the appropriate coordinate for each column. We do that by using the $ subsetting operation after we call get_stem_location().

individual %>% dplyr::mutate(stemLat = get_stem_location(decimalLongitude, decimalLatitude, 
                                                         stemAzimuth, stemDistance)$lat, 
                             stemLon = get_stem_location(decimalLongitude, decimalLatitude, 
                                                         stemAzimuth, stemDistance)$lon)

# It works! We’re almost done with our data munging!
# Let's use the assignment pipe again now that we are happy.

individual %<>% dplyr::mutate(stemLat = get_stem_location(decimalLongitude, decimalLatitude, 
                                                         stemAzimuth, stemDistance)$lat, 
                             stemLon = get_stem_location(decimalLongitude, decimalLatitude, 
                                                         stemAzimuth, stemDistance)$lon)

# Error... Don't know why

# Saving analytical data

# Fist lets create a data directory

fs::dir_create("data")

# Then save the data:
individual %>%
    janitor::clean_names() %>% # turns all column names from camelCase to snake_case
    readr::write_csv(here::here("data", "individual.csv"))

