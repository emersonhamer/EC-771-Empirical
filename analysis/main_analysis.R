

# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stargazer, 
               withr, fixest, modelsummary, did, xtable)
               
setwd('/Users/emersonhamer/Projects/EC-771-Empirical')


# import data
dataset_list <- c("HCRIS_Data", "medicaid_expansion", "pos_data_combined")
for (i in dataset_list){
  assign( paste0(i, ".df"),
          read.table(paste0("data/output/", i ,".txt"),
                     header = TRUE, fill = TRUE, 
                     colClasses=c("character"),
                     check.names = FALSE,
                     sep = '\t', quote = "")
                       )
}
#More cleaning

# Medicaid_expansion
medicaid_expansion.df <- medicaid_expansion.df %>%
    mutate(year = format(as.Date(medicaid_expansion.df$date_adopted), format="%Y")) %>%
    rename(year_expand = year, state = state_abb, expanded_ever = expanded) %>% 
    select(expanded_ever, state, year_expand)

# POS
pos_data_combined.df <- pos_data_combined.df %>%
  rename(provider_number = provider) %>%
  filter(category == "Hospital") %>%
  select(provider_number, state, own_type, year)%>%
  mutate(private = ifelse( own_type == "Non-profit Private" | own_type == "Profit" , 1, 0),
         non_profit_private = ifelse( own_type == "Non-profit Private" , 1, 0))

# HCRIS
HCRIS_Data.df <- HCRIS_Data.df %>%
  filter(year>=2003 & year <=2019) %>%
  select(provider_number, uncomp_care, tot_pat_rev, year)

  
# Merge

full.data <- HCRIS_Data.df %>%
  left_join(pos_data_combined.df,
            by = c("provider_number", "year")) %>%
  left_join(medicaid_expansion.df, 
            by = "state") %>%
  drop_na(uncomp_care, tot_pat_rev, expanded_ever) %>%
  
  transform(uncomp_care = as.numeric(uncomp_care), 
            tot_pat_rev = as.numeric(tot_pat_rev),
            year = as.numeric(year), 
            year_expand = as.numeric(year_expand),
            expanded_ever = as.integer(as.logical(expanded_ever))) %>%
  transform(uncomp_care = uncomp_care/1000000, 
            tot_pat_rev = tot_pat_rev/1000000, 
            year = as.numeric(year)) %>%
  mutate(expanded_t = ifelse(year < year_expand | is.na(year_expand), 0, 1))




trim.data <- full.data %>%
  group_by(year) %>%
  mutate(ptile_uncomp=ntile(uncomp_care,100)) %>%
  filter(ptile_uncomp>1 & ptile_uncomp<99)

# Q1

options(xtable.comment = FALSE)

#Total Revenue 
combined.rev.df <- trim.data %>%
  group_by(year) %>% 
  summarize(
    mean = mean(tot_pat_rev, na.rm = TRUE), 
    std_dev = sd(tot_pat_rev, na.rm = TRUE),
    min = min(tot_pat_rev, na.rm = TRUE),
    max = max(tot_pat_rev, na.rm = TRUE)
  )
# print(xtable(combined.rev.df, caption = "Total Revenue", label = "tab:rev", digits = 2), type = "latex")
# xt_tot_rev = xtable(combined.rev.df,
#              caption = "Summary statistics of total hospital revenus")
# names(xt_tot_rev) <- c('Year', 'Mean','SD','Min', 'Max')
# print(xt_tot_rev, include.rownames=FALSE)

# Rename columns
# colnames(combined.rev.df) <- c('Year', 'Mean','SD','Min', 'Max')
# Print table with knitr::kable
# knitr::kable(combined.rev.df, caption = "Summary statistics of total revenues")





#Uncompensated Care
combined.uncomp.df <- trim.data %>%
 #mutate(year = as.character(year)) %>%
  group_by(year) %>% 
  summarize(
    mean = mean(uncomp_care, na.rm = TRUE), 
    std_dev = sd(uncomp_care, na.rm = TRUE),
    min = min(uncomp_care, na.rm = TRUE),
    max = max(uncomp_care, na.rm = TRUE)
    )

# Rename columns
# # colnames(combined.uncomp.df) <- c('Year', 'Mean','SD','Min', 'Max')

# Print table with knitr::kable
# knitr::kable(combined.uncomp.df, caption = "Summary statistics of uncompensated care")

# print(xtable(combined.uncomp.df, caption = "Uncompensated Care", label = "tab:uncomp", digits = 2), type = "latex")


#Q2
# Load the necessary library
library(ggplot2)

# Calculate mean uncompensated care by year
mean_uncomp_care_by_year <- trim.data %>%
  filter(year >= 2003 & year <= 2019) %>%
  group_by(year) %>%
  summarize(mean_uncomp_care = mean(uncomp_care, na.rm = TRUE))

# Create the plot
full_mean_unc_graph <- ggplot(mean_uncomp_care_by_year, aes(x = year, y = mean_uncomp_care)) +
  geom_line() +
  labs(x = "Year", y = "Mean Uncompensated Care", 
       title = "Mean Hospital Uncompensated Care from 2003 to 2019")

# Save the plot
# saveRDS(mean_uncomp_care_by_year, "mean_uncomp_care_by_year.rds")


# Calculate mean uncompensated care by year and ownership type
mean_uncomp_care_by_year <- trim.data %>%
  filter(private ==1) %>%
  filter(year >= 2003 & year <= 2019) %>%
  group_by(year, non_profit_private) %>%
  mutate(non_profit_private = factor(non_profit_private)) %>%
  summarize(mean_uncomp_care = mean(uncomp_care, na.rm = TRUE))

# Create the plot
mean_unc_bg_graph <- ggplot(mean_uncomp_care_by_year, aes(x = year, y = mean_uncomp_care, color = non_profit_private)) +
  geom_line() +
  labs(x = "Year", y = "Mean Uncompensated Care", 
       title = "Mean Hospital Uncompensated Care from 2003 to 2019 by Ownership Type") +
  scale_color_discrete(name = "", labels=c("Non-profit Private","Profit Private")) +
  theme(legend.text = element_text(size = 8),  # make legend text smaller
        legend.position = "bottom")  # move legend to top

# Save the plot
#saveRDS(mean_uncomp_care_by_year, "mean_uncomp_care_by_year_ownership.rds")


#Q3
reg.data1 <- trim.data %>% 
  #mutate(uncomp_care=uncomp_care/1000) %>%
  mutate(treat=
           case_when(
             year>=year_expand & !is.na(year_expand) ~ 1,
             year<year_expand & !is.na(year_expand) ~0,
             is.na(year_expand) ~ 0
           )
         )

reg.data2 <- trim.data %>% 
  #mutate(uncomp_care=uncomp_care/1000) %>%
  filter(is.na(year_expand) | year_expand==2014) %>%
  mutate(post=(year>=2014),
         treat=post*expanded_ever)

reg.data3 <- trim.data %>% 
  #mutate(uncomp_care=uncomp_care/1000) %>%
  filter(is.na(year_expand) | year_expand==2015) %>%
  mutate(post=(year>=2015),
         treat=post*expanded_ever)

reg.data4 <- trim.data %>% 
  #mutate(uncomp_care=uncomp_care/1000) %>%
  filter(is.na(year_expand) | year_expand==2016) %>%
  mutate(post=(year>=2016),
         treat=post*expanded_ever)

dd.est1 <- feols(uncomp_care~treat | year + provider_number, data=reg.data1)
dd.est2 <- feols(uncomp_care~treat | year + provider_number, data=reg.data2)
dd.est3 <- feols(uncomp_care~treat | year + provider_number, data=reg.data3)
dd.est4 <- feols(uncomp_care~treat | year + provider_number, data=reg.data4)

#sum.fmt <- function(x) formatC(x, digits = 2, big.mark = ",", format = "f")
dd.summary <- msummary(list("Full Sample"=dd.est1, "Expand 2014"=dd.est2, 
                            "Expand 2015"=dd.est3, "Expand 2016"=dd.est4),
                       shape=term + statistic ~ model, 
                       gof_map=NA,
                       coef_omit='Intercept',
                       coef_rename=c("treat"="Expansion"),
                       fmt= sum.fmt,
                       vcov = ~provider_number,
                       caption="TWFE Estimates for Different Treatment Groups",
                       label="ddmodels") 



# Q4. 

## based on full data
event.data1 <- reg.data1 %>%
  mutate(event_time= case_when(
             !is.na(year_expand) ~ year-year_expand,
             is.na(year_expand) ~ -1
           )
         ) 

event.reg1 <- feols(uncomp_care ~ i(as.factor(event_time), expanded_ever, ref=-1) | year + provider_number, 
                   cluster=~provider_number, data=event.data1)


## based on 2014 treatment group only
event.reg2 <- feols(uncomp_care ~ i(as.factor(year), expanded_ever, ref=2013) | year + provider_number, 
                    cluster=~provider_number, data=reg.data2)

## based on 2014 treatment group only
event.reg2 <- feols(uncomp_care ~ i(as.factor(year), expanded_ever, ref=2013) | year + provider_number, 
                    cluster=~provider_number, data=reg.data2)


#Q5
sa.data <- event.data1 %>%
  mutate(year_expand = ifelse(expanded_ever==FALSE, 10000, year_expand),
         time_to_treat = ifelse(expanded_ever==FALSE, -1, year-year_expand),
         time_to_treat = ifelse(time_to_treat < -15, -15, time_to_treat))

sa.reg <- feols(uncomp_care ~ sunab(year_expand, time_to_treat) | year + provider_number,
                cluster=~provider_number, data=sa.data)
sa.est <- tidy(summary(sa.reg, agg=FALSE)) %>%
  filter(str_detect(term,"cohort::2014|cohort::2015|cohort::2016")) %>%
  mutate(term=str_replace(term,"time_to_treat::",""),
         term=str_replace(term,":cohort::",":")) %>%
  separate(term,c("period","cohort"),":") %>%
  mutate(period=as.numeric(period)) %>%
  select(period, cohort, estimate, p.value) %>%
  rename(p_value=p.value) 

# sa.est

  #Q6
#Event study plot
sa.event.plot <- iplot(sa.reg, xlab = "Time to Treatment", main="Sun and Abraham Event Study")



#Q7
library(scales)
install.packages("scales")

cs.data <- reg.data1 %>%
  mutate(year_expand=ifelse(is.na(year_expand), 0, year_expand)) %>%
  group_by(provider_number) %>%
  mutate(hospital_id=cur_group_id()) %>% 
  ungroup()


cs.mod <- att_gt(yname="uncomp_care", tname="year", idname="hospital_id",
                 gname="year_expand",
                 data=cs.data, panel=TRUE, est_method="dr",
                 allow_unbalanced_panel=TRUE)
cs.event <- aggte(cs.mod, type="dynamic")

coef.cs <- tidy(cs.event) %>%
  select(rel_year=event.time, estimate, ci_lower=conf.low, ci_upper=conf.high) %>%
  mutate(rel_year=as.numeric(rel_year))
coef.cs <- as_tibble(coef.cs)

cs.plot <- ggplot(coef.cs, aes(x=rel_year, y=estimate)) + 
  geom_point(size = 2)  +
  geom_linerange(aes(ymin = ci_lower, ymax = ci_upper)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  theme(legend.position="none") +
  scale_x_continuous(breaks = -15:5, minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL, label=comma) +
  labs(x = "Relative Time", y = "Estimated Effect, (millions)", color = NULL, title = NULL) +
  theme_bw()

  # cs.plot

# Save the plot
# saveRDS(cs.plot, "cs_plot.rds")


# Q8
library(did)
install.packages("did")

remotes::install_github ("asheshrambachan/HonestDiD", force = TRUE)
install.packages("devtools")
devtools::install_github("bcallaway11/BMisc", dependencies = TRUE)
library(HonestDiD)


source("/Users/emersonhamer/Projects/EC-771-Empirical/analysis/honest_did.R")


cs.hd <- att_gt(yname="uncomp_care", tname="year", idname="hospital_id",
                 gname="year_expand",
                 data=cs.data, panel=TRUE, est_method="dr",
                 allow_unbalanced_panel=TRUE,
                base_period="universal")
cs.hd.event <- aggte(cs.hd, type="dynamic", min_e=-10, max_e=5)

hd.cs <- honest_did(cs.hd.event, type="smoothness", Mvec=seq(from=0, to=2, by=.5))
hd.cs.graph <- createSensitivityPlot(hd.cs$robust_ci,
                                        hd.cs$orig_ci)

coef.cs.hd <- hd.cs$robust_ci %>% 
  bind_rows(hd.cs$orig_ci) %>%
  mutate(type=case_when(
    M==2 ~ "M = +2",
    M==1.5 ~ "M = +1.5",
    M==1 ~ "M = +1",
    M==.5 ~ "M = +.5",
    M==0 ~ "M = 0",
    is.na(M) ~ "Original"
  ))

cs.hd.plot <- ggplot(coef.cs.hd, aes(x=factor(type, 
                                              level=c('Original', 'M = 0', 'M = +.5', 'M = +1', 'M = +1.5', 'M = +2')))) + 
  geom_linerange(aes(ymin = lb, ymax = ub)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(legend.position="none") +
  scale_y_continuous(minor_breaks = NULL) +
  labs(x = "Violation in Parallel Trends", y = "Estimated Effect (at t=0)", color = NULL, title = NULL) +
  theme_bw()

save.image(file = 'analysis/did.RData')