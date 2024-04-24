
# Meta --------------------------------------------------------------------

# Preliminaries -----------------------------------------------------------
kff.dat <- read_csv('data/input/KFF/KFF_medicaid_expansion_2019.csv')

# Clean KFF data -------------------------------------------------------

kff.final <- kff.dat %>%
  mutate(expanded = (`Expansion Status` == 'Adopted and Implemented'),
         Description = str_replace_all(Description,c("\n"='','"'='')))

kff.final$splitvar <- kff.final %>% select(Description) %>% as.data.frame() %>%
  separate(Description, sep=" ", into=c(NA, NA, NA, "date"))

kff.final <- kff.final %>%
  mutate(date_adopted = mdy(splitvar$date)) %>%
  select(State, expanded, date_adopted)

State_name <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
state_abbreviations <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

state_data <- data.frame(State_name = State_name, Abbreviation = state_abbreviations)

# Function to retrieve abbreviation for a given state name
get_state_abbreviation <- function(State_name) {
  abbreviation <- state_data$Abbreviation[state_data$State_name == State_name]
  if (length(abbreviation) == 0) {
    return("State not found")
  } else {
    return(abbreviation)
  }
}


kff.final <- kff.final %>%
  mutate(state_abb = sapply(State, get_state_abbreviation))


write_tsv(kff.final,'data/output/medicaid_expansion.txt')