library(tidyverse)
library(readxl)
library(ggplot2)
library(naniar)

data <- read_xls("breach_report.xls", 
                 col_names = c("entity_name", "state", "entity_type", "num_affected", "date", "breach_type", 
                               "breach_location", "assoc_present", "web_desc"), 
                 skip = 1)
data <- data %>% mutate_at(c("entity_name", "state", "entity_type", "breach_type", "breach_location", "assoc_present"), 
                            ~as.factor(.))
data$date <- as.Date(data$date, "%m/%d/%Y")
data$num_affected <- as.double(data$num_affected)

miss_var_summary(data)

data %>% select(web_desc) %>% filter(!is.na(web_desc)) %>% head()

web_desc <- data %>% select(web_desc)

data <- data %>% select(-web_desc) %>% drop_na()

summary(data)

data %>% arrange(desc(num_affected))

nlevels(data$breach_type)
nlevels(data$breach_location)

levels(data$breach_type)
head(levels(data$breach_location))

breach_type <- unique(str_split(data$breach_type, ","))
type_len <- length(breach_type)
type_unique <- c()

#breach_type is a list; this measures list length
for (i in 1:type_len) {
  inner_len <- length(breach_type[[i]]) #number of items in each list
  for (j in 1:inner_len) { #iterates over each item inside the list 
    val <- str_trim(breach_type[[i]][j]) 
    #if statement to insert unique values into type_unique
    if (val %in% loc_unique) {
      invisible()
    } else {
      type_unique <- rbind(type_unique, val)
    }
  }
}

type_unique

data_wide <- data
data_wide$hacking <- ifelse(str_detect(data$breach_type, "Hacking/IT Incident"), 1, 0)
data_wide$disposal <- ifelse(str_detect(data$breach_type, "Improper Disposal"), 1, 0)
data_wide$loss <- ifelse(str_detect(data$breach_type, "Loss"), 1, 0)
data_wide$theft <- ifelse(str_detect(data$breach_type, "Theft"), 1, 0)
data_wide$unauth <- ifelse(str_detect(data$breach_type, "Unauthorized Access/Disclosure"), 1, 0)
data_wide$type_other <- ifelse(str_detect(data$breach_type, "Other"), 1, 0)
data_wide$unknown <- ifelse(str_detect(data$breach_type, "Unknown"), 1, 0)

breach_loc <- unique(str_split(data$breach_location, ","))
loc_len <- length(breach_loc)
loc_unique <- c()

for (i in 1:loc_len) {
  inner_len <- length(breach_loc[[i]])
  for (j in 1:inner_len) {
    val <- breach_loc[[i]][j]
    if (val %in% loc_unique) {
      invisible()
    } else {
      loc_unique <- rbind(loc_unique, val)
    }
  }
}
loc_unique

loc_unique <- c()
for (i in 1:loc_len) {
  inner_len <- length(breach_loc[[i]])
  for (j in 1:inner_len) {
    val <- trimws(breach_loc[[i]][j])
    if (val %in% loc_unique) {
      invisible()
    } else {
      loc_unique <- rbind(loc_unique, val)
    }
  }
}

loc_unique

data_wide$paperfilm <- ifelse(str_detect(data$breach_location, "Paper/Films"), 1, 0)
data_wide$email <- ifelse(str_detect(data$breach_location, "Email"), 1, 0)
data_wide$network <- ifelse(str_detect(data$breach_location, "Network Server"), 1, 0)
data_wide$emr <- ifelse(str_detect(data$breach_location, "Electronic Medical Record"), 1, 0)
data_wide$laptop <- ifelse(str_detect(data$breach_location, "Laptop"), 1, 0)
data_wide$desktop <- ifelse(str_detect(data$breach_location, "Desktop Computer"), 1, 0)
data_wide$loc_other <- ifelse(str_detect(data$breach_location, "Other"), 1, 0)
data_wide$other_elec <- ifelse(str_detect(data$breach_location, "Other Portable Electronic Device"), 1, 0)

data_wide <- data_wide %>% select(-breach_type, -breach_location)

data_long <- data_wide %>% 
  pivot_longer(hacking:unknown, 
               names_to = "type") %>% 
  mutate(type_val = value, 
         type = as.factor(type)) %>% 
  select(-value)

data_long <- data_long %>% 
  pivot_longer(paperfilm:other_elec, 
               names_to = "loc") %>% 
  mutate(loc_val = value, 
         loc = as.factor(loc)) %>% 
  select(-value)

data_long <- data_long %>% filter(type_val == 1 & loc_val == 1) %>% select(-type_val, -loc_val)