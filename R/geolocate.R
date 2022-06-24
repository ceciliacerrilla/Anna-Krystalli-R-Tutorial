# Make the geolocate function

# Subset a single row from individual to test out the function
x <- individual[1,]

# Supply a vector of length two, containing the starting longitude and latitude to argument p. We pass stemAzimuth to argument b (for bearing) and stemDistance to argument d (for distance).
geosphere::destPoint(p = c(x$decimalLongitude, x$decimalLatitude), 
                     b = x$stemAzimuth, 
                     d = x$stemDistance)

# This looks like it’s working nicely. Let’s also check that it vectorises easily, i.e. that if we give it vectors of values instead of single ones that it works properly.

x <- individual[1:5,]

geosphere::destPoint(p = c(x$decimalLongitude, x$decimalLatitude), 
                     b = x$stemAzimuth, 
                     d = x$stemDistance)

# Returns: Error in .pointsToMatrix(p) : Wrong length for a vector, should be 2

geosphere::destPoint(p = cbind(x$decimalLongitude, x$decimalLatitude), # add cbind instead of c()
                     b = x$stemAzimuth, 
                     d = x$stemDistance)

get_stem_location <- function(decimalLongitude, decimalLatitude, stemAzimuth, stemDistance) { # variables
  geosphere::destPoint(p = cbind(decimalLongitude, decimalLatitude), # body of the function
                       b = stemAzimuth,
                       d = stemDistance) %>%
      tibble::as_tibble() # convert output into a tibbble, for better printing
}

# Test it out with vectors from individual

test <- get_stem_location(x$decimalLongitude, x$decimalLatitude, 
                  x$stemAzimuth, x$stemDistance)
test

# Our function seems to be working correctly but it’s good to incorporate checks, especially on our inputs and outputs. For example, if we supply a character vector to our function by mistake, our function won’t work. 
# This checks whether the the object we give it is numeric. If the check is not successful, it returns the object invisibly. If the check is not successful, it throws an error.

checkmate::assert_numeric(x$decimalLatitude)
checkmate::assert_numeric(x$uid)

# Error

# So we add a validation check for each argument in our function:

get_stem_location <- function(decimalLongitude, decimalLatitude, stemAzimuth, stemDistance){
    
    # validation checks
    checkmate::assert_numeric(decimalLongitude)
    checkmate::assert_numeric(decimalLatitude)
    checkmate::assert_numeric(stemAzimuth)
    checkmate::assert_numeric(stemDistance)
    
    
    geosphere::destPoint(p = cbind(decimalLongitude, decimalLatitude), 
                         b = stemAzimuth, d = stemDistance) %>%
        tibble::as_tibble()
}

# Let’s also add a check to our output. Let’s throw a warning if there are any NA values in our output.

# First we store our output so we can evaluate it.

get_stem_location <- function(decimalLongitude, decimalLatitude, stemAzimuth, stemDistance){
    # validation checks
    checkmate::assert_numeric(decimalLongitude)
    checkmate::assert_numeric(decimalLatitude)
    checkmate::assert_numeric(stemAzimuth)
    checkmate::assert_numeric(stemDistance)
    
    
    out <- geosphere::destPoint(p = cbind(decimalLongitude, decimalLatitude), 
                                b = stemAzimuth, d = stemDistance) %>%
        tibble::as_tibble()
}

# We can then check the whole tibble for NAs in one go. We get a 2 dimensional matrix of logical values.

is.na(test) %>% head()


# We can then wrap the output of that in any() which tests whether there are any TRUE values in a logical array.

any(is.na(test))

# Let's apply that to our function

get_stem_location <- function(decimalLongitude, decimalLatitude, stemAzimuth, stemDistance){
    # validation checks
    checkmate::assert_numeric(decimalLongitude)
    checkmate::assert_numeric(decimalLatitude)
    checkmate::assert_numeric(stemAzimuth)
    checkmate::assert_numeric(stemDistance)
    
    
    out <- geosphere::destPoint(p = cbind(decimalLongitude, decimalLatitude), 
                                b = stemAzimuth, d = stemDistance) %>%
        tibble::as_tibble()
    
    checkmate::assert_false(any(is.na(out)))
}

# Lastly, we need to return our actual output!

get_stem_location <- function(decimalLongitude, decimalLatitude, stemAzimuth, stemDistance){
    # validation checks
    checkmate::assert_numeric(decimalLongitude)
    checkmate::assert_numeric(decimalLatitude)
    checkmate::assert_numeric(stemAzimuth)
    checkmate::assert_numeric(stemDistance)
    
    
    out <- geosphere::destPoint(p = cbind(decimalLongitude, decimalLatitude), 
                                b = stemAzimuth, d = stemDistance) %>%
        tibble::as_tibble()
    
    checkmate::assert_false(any(is.na(out)))
    
    return(out)
}

# Let's test it again:

get_stem_location(x$decimalLongitude, x$decimalLatitude, 
                  x$stemAzimuth, x$stemDistance)

# Our function is now ready to be sourced into our last preprocessing stage, adding the new stemLat and stemLon columns


# FINAL FUNCTION 

# Function
get_stem_location <- function(decimalLongitude, decimalLatitude,
                              stemAzimuth, stemDistance) {
  # check inputs are correct type (numeric)
  checkmate::assert_numeric(decimalLatitude)
  checkmate::assert_numeric(decimalLongitude)
  checkmate::assert_numeric(stemAzimuth)
  checkmate::assert_numeric(stemDistance)
  
  
  out <- geosphere::destPoint(p = cbind(decimalLongitude, decimalLatitude),
                       b = stemAzimuth, d = stemDistance) %>%
    tibble::as_tibble()
  
  # check output for NAs
  checkmate::assert_false(any(is.na(out)))
  
  return(out)
  }
