library(dataspice)
library(dplyr)

# We’ll start by creating the basic metadata .csv files in which to collect metadata related to our example dataset using function dataspice::create_spice().
# This creates a metadata folder in your project’s data folder (although you can specify a different directory if required) containing 4 .csv files in which to record your metadata.

create_spice()

# access.csv: record details about where your data can be accessed.
# attributes.csv: record details about the variables in your data.
# biblio.csv: record dataset level metadata like title, description, licence and spatial and temporal coverage.
# creators.csv: record creator details.

# ---------------------------------------------------------------

# creators.csv

# Let’s start with a quick and easy file to complete, the creators. We can open and edit the file using in an interactive shiny app using dataspice::edit_creators().

edit_creators()

# ---------------------------------------------------------------

# access.csv

# Before manually completing any details in the access.csv, we can use dataspice’s dedicated function prep_access() to extract relevant information from the data files themselves.

prep_access(data_name)

# Next, we can use function edit_access() to view access. The final details required, namely the URL at which each dataset can be downloaded from cannot be completed now so just leave that blank for now.
# Eventually it should link to a permanent identifier from which the published. data set can be downloaded from.
# We can also edit details such as the name field to something more informative if required.

edit_access()

# ---------------------------------------------------------------

# biblio.csv

# Before we start filling this table in, we can use some base R functions to extract some of the information we require. In particular we can use function range() to extract the temporal and spatial extents of our data from the columns containing temporal and spatial data.

# Get temporal extent.
# Although dates are stored as a text string, because they are in ISO format (YYYY-MM-DD), sorting them results in correct chronological ordering. If your temporal data is not in ISO format, consider converting them (see package lubridate)

range(individual$date) 

# Get geographical extent.
#The lat/lon coordinates are in decimal degrees which again are easy to sort or calculate the range in each dimension.

# South/North boundaries

range(individual$decimalLatitude)

# West/East boundaries

range(individual$decimalLongitude)

# Geographic description. 
# We’ll also need a geographic textual description. Let’s check the unique values in domain_id and use those to create a geographic description.

unique(individual$domainID)

# Now that we’ve got the values for our temporal and spatial extents and decided on the geographic description, we can complete the rest of the fields in the biblio.csv file using function dataspice::edit_biblio().

edit_biblio()


# ---------------------------------------------------------------

# attributes.csv

# Again, dataspice provides functionality to populate the attributes.csv by extracting the variable names from our data file using function dataspice::prep_attributes().
# The functions is vectorised and maps over each .csv file in our data/ folder.

prep_attributes()
edit_attributes()

# Now, we could manually complete the description and unitText fields,… or we can use a secret weapon, NEON_vst_variables.csv in our raw data!
# Let’s read it in and have a look:

variables <- readr::read_csv(here::here("data-raw", "wood-survey-data-master", 
                                        "NEON_vst_variables.csv"))


# ---------------------------------------------------------------

# Create metadata json-ld file
# Now that all our metadata files are complete, we can compile it all into a structured dataspice.json file in our data/metadata/ folder.

write_spice()

install.packages(c("jsonlite", "listviewer"))

jsonlite::read_json(here::here("data", "metadata", "dataspice.json")) %>% 
    listviewer::jsonedit()

# ---------------------------------------------------------------

# Build README site
# Finally, we can use the dataspice.json file we just created to produce an informative README web page to include with our dataset for humans to enjoy!

#We use function dataspice::build_site() which creates file index.html in the docs/ folder of your project (which it creates if it doesn’t already exist).

dataspice::build_site()
