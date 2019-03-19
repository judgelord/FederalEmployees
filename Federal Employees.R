library(tidyverse)
library(magrittr)
options(stringsAsFactors = FALSE)


# main data file (large, on dropbox)
data <- read.csv("FACTDATA_DEC2016.csv") 
names(data)



# names of things
data %<>% 
  left_join(read.csv("opm/DTagy.csv"), by = "AGYSUB") %>%
  left_join(read.csv("opm/DTtoa.csv"), by = "TOA") %>%
  left_join(read.csv("opm/DTedlvl.csv"), by = "EDLVL") %>%
  left_join(read.csv("opm/DTocc.csv"), by = "OCC") %>%
  left_join(read.csv("opm/DTloc.csv"), by = "LOC") # %>%
  filter(AGY == "TD") # DOT only
  
  
  
 