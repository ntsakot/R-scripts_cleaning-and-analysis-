pacman::p_load(rio, httr, tidyverse, janitor, here, zoo, ODBC, AMR, patchwork, scales, DBI, lubridate, dplyr, lubridate, rlang, ggplot2)

library(ggplot2)
library(ggthemes)
library(scales)
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)
library(rlang)
library(writexl)
library(scales)
library(EpiCurve)
library(ISOweek)
library(writexl)
library(gridExtra)
library(lubridate)
library(httr)
library(DBI)
library(odbc)

# Connect to Database
con <- *(
  *::*(),
  * = "*"
)

# Query Data
query1 <- "
***
"

flu <- **(*)

# Create Weekly Detection Dataset for 2026
weekly_detection <- flu %>%
  
  filter(year(as.Date(SAM_COL_DATE)) == 2026) %>%
  
  group_by(EPI_WEEK) %>%
  
  summarise(
    
    tested = n(),
    
    positive_any = sum(
      INFA_RESULT == "Positive" |
        INFB_RESULT == "Positive",
      na.rm = TRUE
    ),
    
    det_rate = ifelse(
      tested > 0,
      (positive_any / tested) * 100,
      NA_real_
    ),
    
    .groups = "drop"
  )

dataset <- weekly_detection

view(dataset)
