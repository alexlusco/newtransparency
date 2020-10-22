################################################
#load packages
################################################

library(here)
library(readr)
library(dplyr)
library(plyr)

################################################
#join data files, create column headers, subset
################################################

data1516 <- read_csv("data/raw/2015_2016.csv", col_names = FALSE)
data1617 <- read_csv("data/raw/2016_2017.csv", col_names = FALSE)
data1718 <- read_csv("data/raw/2017_2018.csv", col_names = FALSE)
data1819 <- read_csv("data/raw/2018_2019.csv", col_names = FALSE)

data1516 <- data1516[-c(1,2,3,4,5),]
data1617 <- data1617[-c(1,2,3,4,5),]
data1718 <- data1718[-c(1,2,3,4,5),]
data1819 <- data1819[-c(1,2,3,4,5),]

data1516 <- data1516 %>%
  mutate(fiscal_year = "2015-2016")
data1617 <- data1617 %>%
  mutate(fiscal_year = "2016-2017")
data1718 <- data1718 %>%
  mutate(fiscal_year = "2017-2018")
data1819 <- data1819 %>%
  mutate(fiscal_year = "2018-2019")

data_full <- rbind(data1516, data1617, data1718, data1819)

data_full_subset <- subset(data_full, select = c("X1",
                                                 "X2",
                                                 "X3",
                                                 "X4",
                                                 "X5",
                                                 "X6",
                                                 "X7",
                                                 "X8",
                                                 "X9",
                                                 "X10",
                                                 "X11",
                                                 "X12",
                                                 "X30",
                                                 "X39",
                                                 "X91",
                                                 "X94",
                                                 "X95",
                                                 "X96",
                                                 "X97",
                                                 "X98",
                                                 "X102",
                                                 "X103",
                                                 "X104",
                                                 "X105",
                                                 "X106",
                                                 "X107",
                                                 "X108",
                                                 "X109",
                                                 "X110",
                                                 "X111",
                                                 "X112",
                                                 "X113",
                                                 "X114",
                                                 "X115",
                                                 "X116",
                                                 "X117",
                                                 "X118",
                                                 "X119",
                                                 "X120",
                                                 "X121",
                                                 "X122",
                                                 "X123",
                                                 "X124",
                                                 "X125",
                                                 "X136",
                                                 "X137",
                                                 "X138",
                                                 "X139",
                                                 "X140",
                                                 "X141",
                                                 "X142",
                                                 "X143",
                                                 "fiscal_year",
                                                 "X742",
                                                 "X736"
                                                 ))

data_full_subset <- rename(data_full_subset, 
                           c("X1" = "agency_name",
                                 "X2" = "received_in_rp",
                                 "X3" = "outstanding_from_prev_rp",
                                 "X4" = "total_for_rp",
                                 "X5" = "closed_in_rp",
                                 "X6" = "carried_over_to_rp",
                                 "X7" = "media",
                                 "X8" = "academia",
                                 "X9" = "business",
                                 "X10" = "organization",
                                 "X11" = "public",
                                 "X12" = "declined_to_identify",
                                 "X30" = "total_1.15",
                                 "X39" = "total_16.30",
                                 "X91" = "abandoned_in_rp",
                                 "X94" = "13_1_a",
                                 "X95" = "13_1_b",
                                 "X96" = "13_1_c",
                                 "X97" = "13_1_d",
                                 "X98" = "13_1_e",
                                 "X102" = "15_1",
                                 "X103" = "15_1_ia",
                                 "X104" = "15_1_def",
                                 "X105" = "15_1_sa",
                                 "X106" = "16_1_ai",
                                 "X107" = "16_1_aii",
                                 "X108" = "16_1_aiii",
                                 "X109" = "16_1_b",
                                 "X110" = "16_1_c",
                                 "X111" = "16_1_d",
                                 "X112" = "16_2",
                                 "X113" = "16_2_a",
                                 "X114" = "16_2_b",
                                 "X115" = "16_2_c",
                                 "X116" = "16_3",
                                 "X117" = "16_1_1a",
                                 "X118" = "16_1_1b",
                                 "X119" = "16_1_1c",
                                 "X120" = "16_1_1d",
                                 "X121" = "16_2_1",
                                 "X122" = "16_3_",
                                 "X123" = "16_4_1a",
                                 "X124" = "16_4_1b",
                                 "X125" = "16_5",
                                 "X136" = "20_1a",
                                 "X137" = "20_1b",
                                 "X138" = "20_1_b1",
                                 "X139" = "20_1_3",
                                 "X140" = "20_1d",
                                 "X141" = "20_1_",
                                 "X142" = "20_2_",
                                 "X143" = "20_4_",
                                 "X742" = "people_years",
                                 "X736" = "total_costs"
                                 ))

write_csv(data_full_subset, "data/processed/full_data_subset_2015_2019.csv")

################################################
#add exemption columns together
################################################

data_full_subset <- read_csv("data/processed/full_data_subset_2015_2019.csv")

data_full_subset$s13exemp <- rowSums(cbind(data_full_subset["13_1_a"], 
                                           data_full_subset["13_1_b"], 
                                           data_full_subset["13_1_c"], 
                                           data_full_subset["13_1_e"]))
data_full_subset$s15exemp <- rowSums(cbind(data_full_subset["15_1"], 
                                           data_full_subset["15_1_ia"], 
                                           data_full_subset["15_1_def"], 
                                           data_full_subset["15_1_sa"]))
data_full_subset$s16exemp <- rowSums(cbind(data_full_subset["16_1_ai"], 
                                           data_full_subset["16_1_aii"], 
                                           data_full_subset["16_1_c"], 
                                           data_full_subset["16_1_d"], 
                                           data_full_subset["16_2"], 
                                           data_full_subset["16_2_a"],
                                           data_full_subset["16_2_b"], 
                                           data_full_subset["16_2_c"], 
                                           data_full_subset["16_3"], 
                                           data_full_subset["16_1_1a"], 
                                           data_full_subset["16_1_1b"], 
                                           data_full_subset["16_1_1c"],
                                           data_full_subset["16_1_1d"], 
                                           data_full_subset["16_2_1"], 
                                           data_full_subset["16_3_"], 
                                           data_full_subset["16_4_1a"], 
                                           data_full_subset["16_4_1b"], 
                                           data_full_subset["16_5"]))
data_full_subset$s20exemp <- rowSums(cbind(data_full_subset["20_1a"], 
                                           data_full_subset["20_1b"], 
                                           data_full_subset["20_1_b1"],
                                           data_full_subset["20_1_3"], 
                                           data_full_subset["20_1d"], 
                                           data_full_subset["20_1_"],
                                           data_full_subset["20_2_"], 
                                           data_full_subset["20_4_"]))

data_full_subset$statutorytimeline <- rowSums(cbind(data_full_subset["total 1-15"],
                                                    data_full_subset["total 16-30"]))

data_full_subset2 <- data_full_subset %>%
  select("agency_name",
         "received_in_rp",
         "outstanding_from_prev_rp",
         "total_for_rp",
         "closed_in_rp",
         "carried_over_to_rp",
         "media",
         "academia",
         "business",
         "organization",
         "public",
         "declined_to_identify",
         "abandoned_in_rp",
         "s13exemp",
         "s15exemp",
         "s16exemp",
         "s20exemp",
         "fiscal_year",
         "people_years",
         "total_costs")

write_csv(data_full_subset2, "data/processed/full_data_subset2_2015_2019.csv")

