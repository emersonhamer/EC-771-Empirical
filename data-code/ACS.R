
# Meta --------------------------------------------------------------------
# Preliminaries -----------------------------------------------------------
setwd('/Users/emersonhamer/Projects/EC-771-Empirical')

## see list of variable names and tables
# acs.lookup(endyear=2012, table.number="B27010", span=1)


# Retrieve ACS data -------------------------------------------------------

for (t in 2012:2019) {
  acs.data <- read_csv(paste0('data/input/ACS/ACSDT1Y',t,'.B27010-Data.csv'))
  acs.data <- acs.data %>%
    rename(State="NAME", all_18to34="B27010_018E",
           employer_18to34="B27010_020E",
           direct_18to34="B27010_021E",
           medicare_18to34="B27010_022E",
           medicaid_18to34="B27010_023E",
           tricare_18to34="B27010_024E",
           va_18to34="B27010_025E",
           none_18to34="B27010_033E",
           all_35to64="B27010_034E",
           employer_35to64="B27010_036E",
           direct_35to64="B27010_037E",
           medicare_35to64="B27010_038E",
           medicaid_35to64="B27010_039E",
           tricare_35to64="B27010_040E",
           va_35to64="B27010_041E",
           none_35to64="B27010_050E") %>%
    mutate(year=t)
    
  
  assign(paste0('insurance.',t),acs.data)
}

# Tidy --------------------------------------------------------------------

final.insurance <- rbind(insurance.2012, insurance.2013, insurance.2014, insurance.2015, insurance.2016,
                         insurance.2017, insurance.2018, insurance.2019)
final.insurance <- final.insurance %>%
 mutate(all_18to34 = str_replace_all(all_18to34, "[^\\d\\.]", ""),
         all_35to64 = str_replace_all(all_35to64, "[^\\d\\.]", ""), 
         employer_18to34 = str_replace_all(employer_18to34, "[^\\d\\.]", ""),
         employer_35to64 = str_replace_all(employer_35to64, "[^\\d\\.]", ""), 
         direct_18to34 = str_replace_all(direct_18to34, "[^\\d\\.]", ""),
         direct_35to64 = str_replace_all(direct_35to64, "[^\\d\\.]", ""), 
         medicare_18to34 = str_replace_all(medicare_18to34, "[^\\d\\.]", ""),
         medicare_35to64 = str_replace_all(medicare_35to64, "[^\\d\\.]", ""),          
         medicaid_18to34 = str_replace_all(medicaid_18to34, "[^\\d\\.]", ""),
         medicaid_35to64 = str_replace_all(medicaid_35to64, "[^\\d\\.]", ""), 
         none_18to34 = str_replace_all(none_18to34, "[^\\d\\.]", ""),
         none_35to64 = str_replace_all(none_35to64, "[^\\d\\.]", "")) %>%
  mutate(all_18to34 = as.numeric(all_18to34),
         all_35to64 = as.numeric(all_35to64), 
         employer_18to34 = as.numeric(employer_18to34), 
         employer_35to64 = as.numeric(employer_35to64),
         direct_18to34 = as.numeric(direct_18to34),
         direct_35to64 = as.numeric(direct_35to64), 
         medicare_18to34 = as.numeric(medicare_18to34), 
         medicare_35to64 = as.numeric(medicare_35to64), 
         medicaid_18to34 = as.numeric(medicaid_18to34), 
         medicaid_35to64 = as.numeric(medicaid_35to64),
         none_18to34 = as.numeric(none_18to34), 
         none_35to64 = as.numeric(none_35to64)) %>%
  mutate(adult_pop = all_18to34 + all_35to64,
         ins_employer = employer_18to34 + employer_35to64,
         ins_direct = direct_18to34 + direct_35to64,
         ins_medicare = medicare_18to34 + medicare_35to64,
         ins_medicaid = medicaid_18to34 + medicaid_35to64,
         uninsured = none_18to34 + none_35to64) %>%
  select(State, year, adult_pop, ins_employer, ins_direct, 
         ins_medicare, ins_medicaid, uninsured)

ins.dat <- as_tibble(final.insurance, rownames = "State")
write_tsv(final.insurance,'data/output/acs_insurance.txt')