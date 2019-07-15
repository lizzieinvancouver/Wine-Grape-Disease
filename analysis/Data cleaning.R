####script to create light data for Ph.D. 
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
setwd("~/Documents/Ph.D/LI-COR Data (Manning Park)")

library(tidyverse)

#### uploads all the data in the LI-COR Data folder
tbl <-
  list.files(pattern = "*.xls") %>% 
  map_df(~read.delim(.))

### Removing unwanted rows from dataset
tbl<- tbl[tbl$Model.Name!="Serial Number",]
tbl<- tbl[tbl$Model.Name!="Time",]
tbl<- tbl[tbl$Model.Name!="Memo",]

### Rename column names
names(tbl)[names(tbl) == "Model.Name"] <- "Variable"
names(tbl)[names(tbl) == "LI.180"] <- "Value"

write_csv(tbl, "licordata.csv")

### add species and site data in excel

