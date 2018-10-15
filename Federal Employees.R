library(tidyverse)
library(magrittr)
options(stringsAsFactors = FALSE)


# main data file
data <- read.csv("FACTDATA_DEC2016.csv") 
names(data)
# names of things
data %<>% 
  left_join(read.csv("DTagy.csv"), by = "AGYSUB") %>%
  left_join(read.csv("DTtoa.csv"), by = "TOA") %>%
  left_join(read.csv("DTedlvl.csv"), by = "EDLVL") %>%
  left_join(read.csv("DTocc.csv"), by = "OCC") %>%
  left_join(read.csv("DTloc.csv"), by = "LOC") # %>%
  filter(AGY == "TD") # DOT only

#########
# PLOTS #
#########

# all employees
ggplot(data) + geom_bar(aes(x=AGYSUBT)) + coord_flip() + 
  labs(x = "", y = "", title = "Total Employees")

# SES employees 
unique(data$TOAT)
ggplot(data %>% filter(grepl("Senior Executive Service|Administator|Schedule C", .$TOAT))) + 
  geom_bar(aes(x=AGYSUBT, fill = TOAT)) +
  coord_flip() + 
  labs(x = "", y = "", title = "SES Employees")

# by GS level (note FAA has lots of non-GS)
unique(data$PPGRD)
ggplot(data %>% filter(grepl("GS|EX|FJ|EV", PPGRD))) + 
  geom_bar(aes(x=AGYSUBT, fill = PPGRD)) +
  coord_flip() + 
  labs(x = "", y = "", title = "")

# by GS level (note FAA has a GS grade even for these employees)
unique(data$GSEGRD)
ggplot(data) + 
  geom_bar(aes(x=AGYSUBT, fill = GSEGRD)) +
  coord_flip() + 
  labs(x = "", y = "", title = "")

# by schedule 
unique(data$TOAT)
ggplot(data %>% filter(appointment %in% c("44-Excepted Service - Schedule C", 
                                  "50-Senior Executive Service - Career", 
                                  "55-Senior Executive Service - Non-Career", 
                                  "60-Senior Executive Service - Limited Term",
                                  "65-Senior Executive Service - Limited Emergency",
                                  "Administrator"), LOC == "11")) +
  geom_bar(aes(x=AGYSUBT, fill = appointment)) +
  coord_flip() + 
  labs(x = "", y = "", title = "Senior-Level DOT Employees in DC")  +
  theme(legend.position="bottom", legend.title = element_blank())


# WTF is up with FAA 
faa <- data %>% filter(
  AGYSUBT == "TD03-FEDERAL AVIATION ADMINISTRATION",
  #LOC == "11",
  grepl("EX|FJ|EV", PPGRD)
  #TOAT == "48-Excepted Service - Other"
  )  %>%
  group_by(PPGRD, OCCT) %>% 
  count()
unique(data$PPGRD)

ggplot(data %>% filter(
  LOC == "11",
  AGYSUBT == "TD03-FEDERAL AVIATION ADMINISTRATION",
  #TOAT == "48-Excepted Service - Other",
  grepl("EX|FJ|EV", PPGRD)
) ) + coord_flip() + 
  geom_bar(aes(OCCT, fill = PPGRD)) + 
  labs(x = "", y = "", title = "FAA \"Exempted Service\" non-Air Traffic Control Employees by pay grade")  +
  theme(legend.position="bottom", legend.title = element_blank())

# not sure this is a good idea
data %<>% 
  mutate(TOAT = ifelse(
    grepl("FJ-", PPGRD),
    "44-Excepted Service - Schedule C",
    TOAT)) %>%
  mutate(appointment = ifelse(
    grepl("EV-", PPGRD),
    "50-Senior Executive Service - Career",
    TOAT)) %>%
  mutate(TOAT = ifelse(
    grepl("EX-", PPGRD),
    "Administrator",
    TOAT)) 

data %<>% 
  mutate(
    political = ifelse(appointment %in% c("44-Excepted Service - Schedule C", 
                                   "55-Senior Executive Service - Non-Career", 
                                   "60-Senior Executive Service - Limited Term",
                                   "65-Senior Executive Service - Limited Emergency",
                                   "Administrator"), 
                       "Political", NA)) %>%
  mutate(
    political = ifelse(appointment %in% c("50-Senior Executive Service - Career"), 
                       "Carreer", political))

  1925

ggplot(data %>% filter(!is.na(political), LOC == "11")) +
  geom_bar(aes(x=AGYSUBT, fill = political)) +
  coord_flip() + 
  labs(x = "", y = "", title = "Senior-Level DOT Employees in DC")  +
  theme(legend.position="bottom", legend.title = element_blank())




write.csv(data, "FederalEmployees.csv")
