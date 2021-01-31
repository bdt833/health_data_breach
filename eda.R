library(dplyr)
library(readxl)
library(lubridate)

data <- read_xls("breach_report.xls", 
                 col_names = c("entity_name", "state", "entity_type", "num_affected", "date", "breach_type", 
                               "breach_location", "assoc_present", "web_desc"), 
                 skip = 1)
data <- data %>% mutate_at(c("entity_name", "state", "entity_type", "num_affected", "breach_type", "breach_location", "assoc_present"), 
                            ~as.factor(.))
data$date <- as.Date(data$date, "%m/%d/%Y")