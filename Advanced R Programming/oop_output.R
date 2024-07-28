# Read in the data
library(readr)
library(magrittr)
source("oop_code.R")

# Load data
data <- read_csv("MIE.csv")

# Create LongitudinalData object
x <- make_LD(data)
print(class(x))
print(x)

# Access data for non-existent subject (id = 10)
out <- subject(x, 10)
print(out)

# Access data for subject 14
out <- subject(x, 14)
print(out)

# Get a summary for subject 54
out <- subject(x, 54) %>% summary
print(out)

# Get a summary for subject 14
out <- subject(x, 14) %>% summary
print(out)

# Access data for subject 44, visit 0, room 'bedroom'
out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

# Show a summary of the pollutant values for the same subset
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

# Show a summary of the pollutant values for subject 44, visit 1, room 'living room'
out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)

