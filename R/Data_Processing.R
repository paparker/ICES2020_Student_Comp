library(readr)
library(dplyr)
library(tidyr)

## Read in data
sbo <- read_csv('Data/icesiv_contest.csv')


## Pivot the columns that are split by owner
pct <- sbo %>% 
  select(1:11) %>%
  pivot_longer(8:11, names_to="name", values_to="PCT") %>% 
  mutate(ID=1:n())

eth <- sbo %>% 
  select(12:15) %>%
  pivot_longer(1:4, names_to="name", values_to="ETH") %>% 
  mutate(ID=1:n())

race <- sbo %>% 
  select(16:19) %>%
  pivot_longer(1:4, names_to="name", values_to="RACE") %>% 
  mutate(ID=1:n())

sex <- sbo %>% 
  select(20:23) %>%
  pivot_longer(1:4, names_to="name", values_to="SEX") %>% 
  mutate(ID=1:n())

vet <- sbo %>% 
  select(24:27) %>%
  pivot_longer(1:4, names_to="name", values_to="VET") %>% 
  mutate(ID=1:n())

age <- sbo %>% 
  select(28:31) %>%
  pivot_longer(1:4, names_to="name", values_to="AGE") %>% 
  mutate(ID=1:n())

educ <- sbo %>% 
  select(32:35) %>%
  pivot_longer(1:4, names_to="name", values_to="EDUC") %>% 
  mutate(ID=1:n())

hours <- sbo %>% 
  select(36:39) %>%
  pivot_longer(1:4, names_to="name", values_to="HOURS") %>% 
  mutate(ID=1:n())

prminc <- sbo %>% 
  select(-c(1:39)) %>%
  pivot_longer(1:4, names_to="name", values_to="PRMINC") %>% 
  mutate(ID=1:n())

## Join data back together (i.e. one row per owner)
sboLong <- pct %>% 
  left_join(eth, by="ID") %>%
  left_join(race, by="ID") %>%
  left_join(sex, by="ID") %>%
  left_join(vet, by="ID") %>%
  left_join(age, by="ID") %>%
  left_join(educ, by="ID") %>%
  left_join(hours, by="ID") %>%
  left_join(prminc, by="ID")

## Remove rows without primary income data
sboLong <- sboLong %>% 
  filter(!is.na(PRMINC))

sboLong <- mutate(sboLong, SEX=recode(SEX, M="Male", F="Female")) %>% 
        	mutate(ETH=recode(ETH, N="Non-Hispanic", H="Hispanic")) %>%
        	mutate(RACE=recode(RACE, W="White", B="Black", 
        			   I="Amer. Indian and Alaska Native",
        			   A="Asian", P="Pacific Islander",
        			   S="Some other race")) %>%
        	mutate(VET=recode(VET, "1"="Yes", "2"="No")) %>%
        	mutate(SECTOR=recode(SECTOR,
        		     "11"="Agriculture, Forestry, Fishing and Hunting",
        		     "21"="Mining, Quarrying, and Oil and Gas Extraction",
        		     "22"="Utilities",
        		     "23"="Construction",
        		     "31"= "Manufacturing",
        		     "42"= "Wholesale Trade",
        		     "44"= "Retail Trade",
        		     "48"= "Transportation and Warehousing",
        		     "51"= "Information",
        		     "52"= "Finance and Insurance",
        		     "53"= "Real Estate and Rental and Leasing",
        		     "54"= "Professional, Scientific, and Technical Services",
        		     "55"= "Management of Companies and Enterprises",
        		     "56"= "Administrative and Support and Waste Management and Remediation Services",
        		     "61"= "Educational Services",
        		     "62"= "Health Care and Social Assistance",
        		     "71"= "Arts, Entertainment, and Recreation",
        		     "72"= "Accommodation and Food Services",
        		     "81"= "Other Services (except Public Administration)",
        		     "99"= "Nonclassifiable Establishments")) %>%
        	mutate(FRANCHISE=recode(FRANCHISE, "1"="Yes", "2"="No"))

## Save data
write_rds(sboLong, 'Data/tidy_sbo.rds')
