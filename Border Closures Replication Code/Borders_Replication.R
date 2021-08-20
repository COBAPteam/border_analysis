# Install Packages ----------------------

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("plm",
              "DescTools",
              "PanelMatch",
              "ggplot2",
              "zoo",
              "rio",
              "tidyverse",
              "ggfortify",
              "stats",
              "astsa",
              "texreg",
              "forecast",
              "lubridate", 
              "urca",
              "data.table",
              "readxl",
              "foreign",
              "tscount",
              "reshape2",
              "nortest",
              "car",
              "patchwork",
              "scales",
              "readr")
ipak(packages)


# Load COBAP Datasets ----------------

# Set working directory if need using "setwd()"

COBAP_PL <- read_csv("Policy_list.csv")

COBAP <- read_csv("Time_series_7_26_21.csv")

COBAP_Island_Countries_List <- read_excel("Islands_list.xlsx", col_names = F)


# GLOBAL TRENDS ANALYSIS ----------------
# Data Management --------------
#Import data

ECON <- read_csv("data_with_sero_ifr_econ_pol.csv")
#Source: read_csv("https://raw.githubusercontent.com/TheEconomist/covid-19-the-economist-global-excess-deaths-model/main/source-data/data_with_sero_ifr_econ_pol.csv")


COBAP_PL <- COBAP_PL %>% 
  dplyr::select(ISO3, POLICY_TYPE:WORK_EXCEP) %>% 
  mutate(START_DATE = gsub("_", "-", START_DATE)) %>% 
  mutate(END_DATE = gsub("_", "-", END_DATE)) %>% 
  mutate(START_DATE = mdy(START_DATE)) %>% 
  mutate(END_DATE = mdy(END_DATE)) %>% 
  mutate(DURATION = END_DATE-START_DATE)

COBAP_PL$END_DATE[1218] = "2020-05-12"

#Get country list
country_list <- ECON %>% 
  dplyr::select(iso3c, new_cases) %>% 
  drop_na() %>% 
  dplyr::select(iso3c) %>% 
  unique()

#Create time series data set
start_date <- lubridate::mdy("01/30/2020")
end_date <- lubridate::mdy("12/31/2020")
date <- seq(start_date, end_date, by="days")

COBAP.daily <- as_tibble(country_list) %>% 
  slice(rep(1:n(), each = length(date))) %>% 
  cbind(date) %>% 
  dplyr::rename(iso3 = iso3c) %>% 
  left_join(COBAP_PL, by = c("iso3"="ISO3", "date"="START_DATE")) %>% 
  mutate(complete_new = if_else(POLICY_TYPE == "COMPLETE", 1, 0)) %>% 
  mutate(partial_new = if_else(POLICY_TYPE == "PARTIAL", 1, 0)) %>%
  mutate(borderclosure_new = if_else(POLICY_SUBTYPE == "BORDER_CLOSURE", 1, 0)) %>% 
  mutate(partial2_new = if_else(POLICY_TYPE == "PARTIAL" & POLICY_SUBTYPE != "VISA_BAN", 1, 0))


COBAP.daily$partial_new = dplyr::recode(COBAP.daily$partial_new, .missing = 0)
COBAP.daily$complete_new = dplyr::recode(COBAP.daily$complete_new, .missing = 0)

COBAP.daily <- COBAP.daily %>% 
  group_by(iso3) %>% 
  mutate(partial = purrr::accumulate(partial_new, `+`)) %>% 
  mutate(complete = purrr::accumulate(complete_new, `+`)) %>% 
  ungroup() %>% 
  mutate(air_c = 0) 

COBAPdata_list = split(COBAP.daily, COBAP.daily$iso3)

# Create policy count
for (i in seq_along(COBAPdata_list)) {
  for (j in seq_along(date)) {
    if (COBAPdata_list[[i]][j,32] ==1 && is.na(COBAPdata_list[[i]][j,5])==F){
      COBAPdata_list[[i]]$partial[COBAPdata_list[[i]]$date >= COBAPdata_list[[i]][j,5]$END_DATE] <- COBAPdata_list[[i]]$partial[COBAPdata_list[[i]]$date >= COBAPdata_list[[i]][j,5]$END_DATE]-1
    }else if (COBAPdata_list[[i]][j,30] ==1 && is.na(COBAPdata_list[[i]][j,5])==F){
      COBAPdata_list[[i]]$complete[COBAPdata_list[[i]]$date >= COBAPdata_list[[i]][j,5]$END_DATE] <- COBAPdata_list[[i]]$complete[COBAPdata_list[[i]]$date >= COBAPdata_list[[i]][j,5]$END_DATE]-1
    }else{
      COBAPdata_list[[i]]$complete <- COBAPdata_list[[i]]$complete  
    }
  }
  
}

# Create closure count 
for (i in seq_along(COBAPdata_list)){
  for (j in seq(nrow(COBAPdata_list[[i]]))) {
    if(is.na(COBAPdata_list[[i]][j,]$TARGETS_AIR) && 
       !is.na (COBAPdata_list[[i]][j,]$AIR_TYPE) && 
       COBAPdata_list[[i]][j,]$AIR_TYPE == "All"){
      COBAPdata_list[[i]][ which( COBAPdata_list[[i]]$date >= COBAPdata_list[[i]]$date[j]
                                  & COBAPdata_list[[i]]$date < COBAPdata_list[[i]]$END_DATE[j]) , ]$air_c <- length(unique(COBAP$ISO3))
    }else if(!is.na(COBAPdata_list[[i]][j,]$TARGETS_AIR) && 
             !is.na (COBAPdata_list[[i]][j,]$AIR_TYPE) && 
             COBAPdata_list[[i]][j,]$AIR_TYPE == "Specific" &&
             !grepl("irport", str_split(COBAPdata_list[[i]]$TARGETS_AIR[j], ",")[[1]])){
      COBAPdata_list[[i]][ which( COBAPdata_list[[i]]$date >= COBAPdata_list[[i]]$date[j]  & COBAPdata_list[[i]]$date < COBAPdata_list[[i]]$END_DATE[j]) , ]$air_c <- COBAPdata_list[[i]][ which( COBAPdata_list[[i]]$date >= COBAPdata_list[[i]]$date[j]  & COBAPdata_list[[i]]$date < COBAPdata_list[[i]]$END_DATE[j]) , ]$air_c + length(str_split(COBAPdata_list[[i]]$TARGETS_AIR[j], ", ")[[1]])
    }else{
      COBAPdata_list[[i]][j,]$air_c <- COBAPdata_list[[i]][j,]$air_c}}}

COBAP.daily <- merge_all(COBAPdata_list)

# Select relevant time frame from ECON

ECON2 <- ECON %>% 
  dplyr::select(iso3c, date, new_cases) %>% 
  mutate(date = ymd(date)) %>% 
  filter(date < "2021-01-01" && date >"2020-01-30")

#Join data sets

COBAP.daily.global <- COBAP.daily %>% 
  dplyr::select(iso3, date, complete, partial, complete_new, partial_new, AIR, AIR_TYPE, TARGETS_AIR, DURATION, air_c) %>% 
  left_join(ECON2, by = c("date"="date", "iso3"="iso3c")) %>% 
  group_by(date) %>% 
  summarise(
    global_complete = sum(complete, na.rm = T),
    global_borderclosures = sum(partial, na.rm = T),
    global_complete_new = sum(complete_new, na.rm = T),
    global_borderclosures_new = sum(partial_new, na.rm = T),
    global_airborder_c_sum = sum(air_c, na.rm = T),
    global_airborder_c_mean = mean(air_c, na.rm = T),
    global_new_cases = sum(new_cases, na.rm = T)
  ) 


COBAP.daily.global <- COBAP.daily.global %>% 
  mutate(day = wday(COBAP.daily.global$date)) %>% 
  mutate(week = week(COBAP.daily.global$date))


#Correction of outlier (Dec-10-2020: Source https://web.archive.org/web/20210618230847/https://ourworldindata.org/grapher/daily-covid-cases-deaths?country=~OWID_WRL)

COBAP.daily.global$global_new_cases<-replace(COBAP.daily.global$global_new_cases, 316, 674901)

# Generate Plot (Figure 1) --------------

#Plot
vizdf <- COBAP.daily.global %>% 
  select(date, global_complete, global_borderclosures)

vizdf <- melt(setDT(vizdf), id="date")

ggplot() + 
  geom_area(data = vizdf, aes(x=date, y=value, fill=variable))+
  geom_line(data = COBAP.daily.global, mapping = aes(x = date, y = global_new_cases/1438.793), color = "black")+
  scale_y_continuous(
    name ="Total Number of Policies (Border closures)",
    sec.axis = sec_axis(~.*1438.793,name="New COVID-19 Cases", labels = comma)
  )+
  theme_bw()+
  scale_fill_discrete(name = "Border closure type", labels = c("Complete closure", "Partial border closure"))+
  labs(x = "Date")



# PANEL MATCHING ANALYSIS ----------------
# Load Source Datasets --------------

JH <- read_csv("time_series_covid19_confirmed_global.csv")
# Source: read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
               
CGRT_School <- read_csv("c1_school_closing.csv")
# Source: read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c1_school_closing.csv")

CGRT_Workplace <- read_csv("c2_workplace_closing.csv")
# Source: read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c2_workplace_closing.csv")

CGRT_PublicEvents <- read_csv("c3_cancel_public_events.csv")
# Source: read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c3_cancel_public_events.csv")

CGRT_Gatherings <- read_csv("c4_restrictions_on_gatherings.csv")
# Source: read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c4_restrictions_on_gatherings.csv")

CGRT_Transport <- read_csv("c5_close_public_transport.csv")
# Source: read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c5_close_public_transport.csv")

CGRT_StayHome <- read_csv("c6_stay_at_home_requirements.csv")
# Source: read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c6_stay_at_home_requirements.csv")

CGRT_Movement <- read_csv("c7_movementrestrictions.csv")
# Source: read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/timeseries/c7_movementrestrictions.csv")


# Johns Hopkins Data Cleaning ----------------------
JH <- JH[ , -c(1,3,4)]

JH_country <-  aggregate(.~`Country/Region`, JH, sum)

JH_country$iso3c <- c("AFG", "ALB", "DZA", "AND", "AGO", "ATG", "ARG", "ARM", "AUS", "AUT", "AZE", "BHS", "BHR", "BGD", "BRB", "BLR", "BEL", "BLZ", "BEN", "BTN", "BOL", "BIH", "BWA", "BRA", "BRN", "BGR", "BFA", "MMR", "BDI", "CPV", "KHM", "CMR", "CAN", "CAF", "TCD", "CHL", "CHN", "COL", "COM", "COG", "COD", "CRI", "CIV", "HRV", "CUB", "CYP", "CZE", "DNK", "0", "DJI", "DMA", "DOM", "ECU", "EGY", "SLV", "GNQ", "ERI", "EST", "SWZ", "ETH", "FJI", "FIN", "FRA", "GAB", "GMB", "GEO", "DEU", "GHA", "GRC", "GRD", "GTM", "GIN", "GNB", "GUY", "HTI", "VAT", "HND", "HUN", "ISL", "IND", "IDN", "IRN", "IRQ", "IRL", "ISR", "ITA", "JAM", "JPN", "JOR", "KAZ", "KEN", "KIR", "KOR", "XKX", "KWT", "KGZ", "LAO", "LVA", "LBN", "LSO", "LBR", "LBY", "LIE", "LTU", "LUX", "MDG", "MWI", "MYS", "MDV", "MLI", "MLT", "MHL", "MRT", "MUS", "MEX", "FSM", "MDA", "MCO", "MNG", "MNE", "MAR", "MOZ", "0", "NAM", "NPL", "NLD", "NZL", "NIC", "NER", "NGA", "MKD", "NOR", "OMN", "PAK", "PLW", "PAN", "PNG", "PRY", "PER", "PHL", "POL", "PRT", "QAT", "ROU", "RUS", "RWA", "KNA", "LCA", "VCT", "WSM", "SMR", "STP", "SAU", "SEN", "SRB", "SYC", "SLE", "SGP", "SVK", "SVN", "SLB", "SOM", "ZAF", "SSD", "ESP", "LKA", "SDN", "0", "SUR", "SWE", "CHE", "SYR", "TWN", "TJK", "TZA", "THA", "TLS", "TGO", "TTO", "TUN", "TUR", "UGA", "UKR", "ARE", "GBR", "URY", "USA", "UZB", "VUT", "VEN", "VNM", "PSE", "YEM", "ZMB", "ZWE")

JH_long <- reshape::melt(JH_country, id.vars=c("iso3c", "Country/Region"))

JH_long <- JH_long[1:67275, ]

JH_long$time_count <- rep(22:366,each=195)

JH_long$week_time_count <- JH_long$time_count-4

JH_long$week_count <- ifelse(JH_long$week_time_count>0, round((JH_long$week_time_count/7)+0.499,0), NA)

JH_long <- JH_long[-which(JH_long$iso3c=="0"),]
JH_long$iso3c <- as.character(JH_long$iso3c)

JH_Cleaned <- aggregate(value ~ iso3c + week_count, JH_long, max)

rm(JH, JH_country, JH_long)

# Economist Data Cleaning  --------------------


ECON <- ECON[which(ECON$`C/T`=="c"), ]
ECON <- ECON[-c(28517:29128), ]

ECON$year <-as.numeric(substr(ECON$date, start=1, stop=4))
ECON$month <- as.numeric(substr(ECON$date, start=6, stop=7))
ECON$day <- as.numeric(substr(ECON$date, start=9, stop=10))
ECON <- ECON[-which(ECON$year==2021), ]

ECON$time_count <- ((ECON$year-2020)*366) + (ifelse(ECON$month==1, 0, ifelse(ECON$month==2, 31, ifelse(ECON$month==3, 60, ifelse(ECON$month==4, 91, ifelse(ECON$month==5, 121, ifelse(ECON$month==6, 152, ifelse(ECON$month==7, 182, ifelse(ECON$month==8, 213, ifelse(ECON$month==9, 244, ifelse(ECON$month==10, 274, ifelse(ECON$month==11, 305, 335)))))))))))) + ECON$day

ECON$week_time_count <- ECON$time_count-4

ECON$week_count <- ifelse(ECON$week_time_count>0, round((ECON$week_time_count/7)+0.499,0), NA)

ECON_E <- ECON[,c(2,4,6,8:29,124,129:142,149:163)]

# Combining variables into week aggregations - different variable transformation operation
  # Fixed: 1:11,26:29,41
  # Time Markers: 50:55
  # Mean: 12:16,19, 32,37,40
  # Sum:  17:31,34,36,39
  # Max:  30,33,35,38,42:49

ECON_E_fixed <- ECON_E[,c(1:11,26:29,41,55)]
ECON_E_mean <- ECON_E[,c(1,12,19,32,37,40,55)]
ECON_E_sum <- ECON_E[,c(1,13:18,20:25,31,34,36,39,55)]
ECON_E_max <- ECON_E[,c(1,30,33,35,38,42:49,55)]

ECON_E_fixed <- ECON_E_fixed[!duplicated(ECON_E_fixed),]

ECON_E_mean <- ECON_E_mean %>%
  group_by(iso3c, week_count) %>% 
  summarise_each(funs(mean))  

ECON_E_sum <- ECON_E_sum %>%
  group_by(iso3c, week_count) %>% 
  summarise_each(funs(sum))  

ECON_E_max <- ECON_E_max %>%
  group_by(iso3c, week_count) %>% 
  summarise_each(funs(max))

# Recombine week aggregations
ECON_Final <- cbind(ECON_E_mean, ECON_E_sum, ECON_E_max)
ECON_Final <- ECON_Final[, -c(8,9,26,27)]
colnames(ECON_Final)[1:2] <- c("iso3c", "week_count")
ECON_Cleaned <- merge(ECON_Final, ECON_E_fixed,by = c("iso3c", "week_count"))

rm(ECON, ECON_E, ECON_E_fixed, ECON_E_max, ECON_E_mean, ECON_E_sum, ECON_Final)

# CGR Data Cleaning  --------------------

CGRT_School <- CGRT_School[, -1]
CGRT_Workplace <- CGRT_Workplace[, -1]
CGRT_PublicEvents <- CGRT_PublicEvents[, -1]
CGRT_Gatherings <- CGRT_Gatherings[, -1]
CGRT_Transport <- CGRT_Transport[, -1]
CGRT_StayHome <- CGRT_StayHome[, -1]
CGRT_Movement <- CGRT_Movement[, -1]


# Convert variables to long form
CGRT_School <- reshape2::melt(CGRT_School, id.vars=c("country_code", "country_name"))
CGRT_Workplace <- reshape2::melt(CGRT_Workplace, id.vars=c("country_code", "country_name"))
CGRT_PublicEvents <- reshape2::melt(CGRT_PublicEvents, id.vars=c("country_code", "country_name"))
CGRT_Gatherings <- reshape2::melt(CGRT_Gatherings, id.vars=c("country_code", "country_name"))
CGRT_Transport <- reshape2::melt(CGRT_Transport, id.vars=c("country_code", "country_name"))
CGRT_StayHome <- reshape2::melt(CGRT_StayHome, id.vars=c("country_code", "country_name"))
CGRT_Movement <- reshape2::melt(CGRT_Movement, id.vars=c("country_code", "country_name"))

# Combine into single dataset
CGRT <- cbind(CGRT_School, CGRT_Workplace,  CGRT_PublicEvents, CGRT_Gatherings, CGRT_Transport, CGRT_StayHome, CGRT_Movement)

# Remove redundant columns
CGRT <- CGRT[ , -c(5:7,9:11,13:15,17:19,21:23,25:27)]
colnames(CGRT)[3:10] <- c("date", "school", "workplace", "publicevent", "gatherings", "transport", "stayhome", "movement")

# Create numeric date columns
CGRT$day <- as.numeric(substr(CGRT$date, start=1, stop=2))
CGRT$month <- substr(CGRT$date, start=3, stop=5)
CGRT$year <- as.numeric(substr(CGRT$date, start=6, stop=9))

CGRT$month <- ifelse(CGRT$month=="Jan", 1, ifelse(CGRT$month=="Feb", 2, ifelse(CGRT$month=="Mar", 3, ifelse(CGRT$month=="Apr", 4, ifelse(CGRT$month=="May", 5, ifelse(CGRT$month=="Jun", 6, ifelse(CGRT$month=="Jul", 7, ifelse(CGRT$month=="Aug", 8, ifelse(CGRT$month=="Sep", 9, ifelse(CGRT$month=="Oct", 10, ifelse(CGRT$month=="Nov", 11, ifelse(CGRT$month=="Dec", 12, NA))))))))))))

# Create day count variables
CGRT$time_count <- ((CGRT$year-2020)*366) + (ifelse(CGRT$month==1, 0, ifelse(CGRT$month==2, 31, ifelse(CGRT$month==3, 60, ifelse(CGRT$month==4, 91, ifelse(CGRT$month==5, 121, ifelse(CGRT$month==6, 152, ifelse(CGRT$month==7, 182, ifelse(CGRT$month==8, 213, ifelse(CGRT$month==9, 244, ifelse(CGRT$month==10, 274, ifelse(CGRT$month==11, 305, 335)))))))))))) + CGRT$day

# Adjust for start date
CGRT$week_time_count <- CGRT$time_count-4

# Create week counts
CGRT$week_count <- ifelse(CGRT$week_time_count>0, round((CGRT$week_time_count/7)+0.499,0), NA)

# Standardized variables between 0 and 1
CGRT$school_i <- CGRT$school/3
CGRT$workplace_i <- CGRT$workplace/3
CGRT$publicevent_i <- CGRT$publicevent/2
CGRT$gatherings_i <- CGRT$gatherings/4
CGRT$transport_i <- CGRT$transport/2
CGRT$stayhome_i <- CGRT$stayhome/3
CGRT$movement_i <- CGRT$movement/2

# Combine indicators into new Stringency Index
CGRT$Stringency_revised <- (CGRT$school_i + CGRT$workplace_i + CGRT$publicevent_i + CGRT$gatherings_i + CGRT$transport_i + CGRT$stayhome_i + CGRT$movement_i)/7


# Remove redundant observations
CGRT_E <- CGRT[-which(CGRT$year==2021),]
CGRT_E <- CGRT_E[, c(1,2,11:24)]

# Create weekly average statistics
CGRT_Cleaned <- CGRT_E %>%
  group_by(country_code, week_count) %>% 
  summarise_each(funs(mean))  


rm(CGRT, CGRT_E, CGRT_Gatherings, CGRT_Movement, CGRT_PublicEvents, CGRT_School, CGRT_StayHome, CGRT_Transport, CGRT_Workplace)


# Combining Datasets ---------------------

COBAP$week_count <- rep(seq(4,52,1),252)

colnames(COBAP)[1] <- "iso3c"
colnames(CGRT_Cleaned)[1] <- "iso3c"


DF_Full <- merge(COBAP, ECON_Cleaned[ ,c(1,2,13,39:47,50)], by=c("iso3c", "week_count"), all=T)

DF_Full <- merge(DF_Full, CGRT_Cleaned[, c(1,2,16)], by=c("iso3c", "week_count"), all=T)

DF_Full <- merge(DF_Full, JH_Cleaned, by=c("iso3c", "week_count"), all=T)

DF_Full <- DF_Full[which(is.na(DF_Full$week_count)==F), ]

DF_Full$date <- ifelse(DF_Full$week_count==1, "1_5_20", ifelse(DF_Full$week_count==2, "1_12_20", ifelse(DF_Full$week_count==3, "1_19_20", DF_Full$date)))

DF_Full$completenew <- ifelse(DF_Full$week_count<=3, 0, DF_Full$completenew)

DF_Full$partialnew <- ifelse(DF_Full$week_count<=3, 0, DF_Full$partialnew)

DF_Full$work_excepnew <- ifelse(DF_Full$week_count<=3, 0, DF_Full$work_excepnew)

DF_Full$specific_countrynew <- ifelse(DF_Full$week_count<=3, 0, DF_Full$specific_countrynew)

DF_Full$citizen_excepnew <- ifelse(DF_Full$week_count<=3, 0, DF_Full$citizen_excepnew)

DF_Full$essential_onlynew <- ifelse(DF_Full$week_count<=3, 0, DF_Full$essential_onlynew)

DF_Full$visa_bannew <- ifelse(DF_Full$week_count<=3, 0, DF_Full$visa_bannew)

DF_Full$citizenship_bannew <- ifelse(DF_Full$week_count<=3, 0, DF_Full$citizenship_bannew)

DF_Full$history_bannew <- ifelse(DF_Full$week_count<=3, 0, DF_Full$history_bannew)

DF_Full$border_closurenew <- ifelse(DF_Full$week_count<=3, 0, DF_Full$border_closurenew)


DF_Full$completenew <- ifelse(DF_Full$week_count==3 & DF_Full$iso3c=="PRK", 1, DF_Full$completenew)
DF_Full$citizen_excepnew <- ifelse(DF_Full$week_count==3 & DF_Full$iso3c=="PRK", 1, DF_Full$citizen_excepnew)



HDI <- aggregate(human_development_index ~ iso3c, DF_Full, mean)
DF_Full$human_development_index <- ifelse(is.na(DF_Full$human_development_index==T), HDI[match(DF_Full$iso3c, HDI$iso3c)  , 2],  DF_Full$human_development_index)
                                        
LDM <- aggregate(v2x_libdem ~ iso3c, DF_Full, mean)
DF_Full$v2x_libdem <- ifelse(is.na(DF_Full$v2x_libdem==T), LDM[match(DF_Full$iso3c, HDI$iso3c)  , 2],  DF_Full$v2x_libdem)

FEX <- aggregate(v2x_freexp_altinf ~ iso3c, DF_Full, mean)
DF_Full$v2x_freexp_altinf <- ifelse(is.na(DF_Full$v2x_freexp_altinf==T), FEX[match(DF_Full$iso3c, HDI$iso3c)  , 2],  DF_Full$v2x_freexp_altinf)

POP <- aggregate(pop.2020 ~ iso3c, DF_Full, mean)
DF_Full$pop.2020 <- ifelse(is.na(DF_Full$pop.2020==T), POP[match(DF_Full$iso3c, HDI$iso3c)  , 2],  DF_Full$pop.2020)

A70 <- aggregate(aged_70_older ~ iso3c, DF_Full, mean)
DF_Full$aged_70_older <- ifelse(is.na(DF_Full$aged_70_older==T), A70[match(DF_Full$iso3c, HDI$iso3c)  , 2],  DF_Full$aged_70_older)

LEX <- aggregate(life_expectancy ~ iso3c, DF_Full, mean)
DF_Full$life_expectancy <- ifelse(is.na(DF_Full$life_expectancy==T), LEX[match(DF_Full$iso3c, HDI$iso3c)  , 2],  DF_Full$life_expectancy)

GDP <- aggregate(log_gdp_ppp ~ iso3c, DF_Full, mean)
DF_Full$log_gdp_ppp <- ifelse(is.na(DF_Full$log_gdp_ppp==T), GDP[match(DF_Full$iso3c, HDI$iso3c)  , 2],  DF_Full$log_gdp_ppp)

ARE <- aggregate(area_ifr ~ iso3c, DF_Full, mean)
DF_Full$area_ifr <- ifelse(is.na(DF_Full$area_ifr==T), ARE[match(DF_Full$iso3c, HDI$iso3c)  , 2],  DF_Full$area_ifr)

AGE <- aggregate(median_age ~ iso3c, DF_Full, mean)
DF_Full$median_age <- ifelse(is.na(DF_Full$median_age==T), AGE[match(DF_Full$iso3c, HDI$iso3c)  , 2],  DF_Full$median_age)



nocases_early <- DF_Full[which(DF_Full$week_count==3 & DF_Full$value==0), ]$iso3c
zero2 <- intersect(which(DF_Full$week_count==2), which(DF_Full$iso3c %in% nocases_early==TRUE))
zero1 <- intersect(which(DF_Full$week_count==1), which(DF_Full$iso3c %in% nocases_early==TRUE))


DF_Full[zero1, 29] <- 0           
DF_Full[zero2, 29] <- 0            
                      



# Creating Testing Datasets -------------------


#which(duplicated(DF_Full[,1:2])==T)
DF_Full <- DF_Full[-which(duplicated(DF_Full[,1:2])==T), ]

# Convert to panel data frame format
DF_Full <- pdata.frame(DF_Full, index = c("iso3c", "week_count"))

DF_Full$value_1 <- plm::lag(DF_Full$value,1)
DF_Full$new_cases_UP <- DF_Full$value - DF_Full$value_1


DF_Full$new_cases_UP_100k <-  DF_Full$new_cases_UP/(DF_Full$pop.2020/100000)
DF_Full$new_cases_UP_100k_1 <- plm::lag(DF_Full$new_cases_UP_100k,1)
DF_Full$new_cases_UP_100k_change <- DF_Full$new_cases_UP_100k-DF_Full$new_cases_UP_100k_1

DF_Full$DV_Standardized <- DF_Full$new_cases_UP_100k_change/ (sd(DF_Full$new_cases_UP_100k_change, na.rm = T))

#sd(DF_Full$DV_Standardized, na.rm = T)
#mean(DF_Full$DV_Standardized, na.rm = T)


# Create Stringency Index Treatment - equals one if index increase by 0.25 or more in one week.  
DF_Full$Stringency_revised_1 <- plm::lag(DF_Full$Stringency_revised, 1)
DF_Full$stringency_revised_change <- DF_Full$Stringency_revised - DF_Full$Stringency_revised_1
DF_Full$Stringency_Treat <- ifelse(DF_Full$stringency_revised_change>=0.25, 1, 0)
table(DF_Full$Stringency_Treat)


# Change classes for analysis
DF_Full <- as.data.frame(DF_Full)
DF_Full$countryN <- as.numeric(as.factor(DF_Full$iso3c))
DF_Full$week_count <- as.integer(DF_Full$week_count)

# Convert COBAP new policies to dichotomous as some values are more than 1. 
DF_Full$partialnew_D <- ifelse(DF_Full$partialnew>=1, 1, 0)
DF_Full$competenew_D <- ifelse(DF_Full$completenew>=1, 1, 0)
DF_Full$work_excepnew_D <- ifelse(DF_Full$work_excepnew>=1, 1, 0)
DF_Full$specific_countrynew_D <- ifelse(DF_Full$specific_countrynew>=1, 1, 0)
DF_Full$citizen_excepnew_D <- ifelse(DF_Full$citizen_excepnew>=1, 1, 0)
DF_Full$essential_onlynew_D <- ifelse(DF_Full$essential_onlynew>=1, 1, 0)
DF_Full$visa_bannew_D <- ifelse(DF_Full$visa_bannew>=1, 1, 0)
DF_Full$citizenship_bannew_D <- ifelse(DF_Full$citizenship_bannew>=1, 1, 0)
DF_Full$history_bannew_D <- ifelse(DF_Full$history_bannew>=1, 1, 0)
DF_Full$border_closurenew_D <- ifelse(DF_Full$border_closurenew>=1, 1, 0)

# Create log versions of two highly unbalance variables
DF_Full$daily_tests_per_100k_LOG <- log(DF_Full$daily_tests_per_100k)
DF_Full$pop.2020_LOG <- log(DF_Full$pop.2020)
#hist(TEST$daily_tests_per_100k_LOG)


# Remove observations where with outcome variables missing
DF_Full <- DF_Full[-which(is.na(DF_Full$DV_Standardized)==T), ]


# Create two dataset for panel matching. One with only complete and partial variables, the other with all eight individual variables, remove other variables not being tested.
TEST_c_p <- DF_Full[ ,c(1,2,3,19:25,27,28,35,38,39,40,41,50,51)]

TEST_ind <- DF_Full[ ,c(1,2,3,19:25,27,28,35,38,39,42:51)]



# Test Border Closures (Figures 2-4) -------------

# KEY ARGUMENTS 
  # lag = number of previous weeks to analyze for matching
  # time.id = identification variable for time
  # unit.id = identification variable for units
  # treatment = treatment varible (0 or 1)
  # refinement.method = what type of match or weighting to use in the refinement process. 
  # covs.formula = which variables to use in the refinements process, the "independent variables". For those that are time-varying, specify that the algorithm should select on values during the lag period, three week prior to treatment.   # data = dataset to use
  # match.missing = True or False, asking whether matching can include observations with missing values. 
  # qoi = quantity of interest to compute, standard is average treatment effect on the treated
  # lead = time unit to compute for. Specified for weeks 0 through weeks five relative to treatment
  # outcome.var = outcome variable



# Domestic Lockdown Validity Test

# Create Panel Matches for Stringency Treatment using Mahalanobis
Stringency_m <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "Stringency_Treat", refinement.method = "mahalanobis", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(work_excepnew_D, 1:3)) + I(lag(specific_countrynew_D, 1:3)) + I(lag(citizen_excepnew_D, 1:3)) + I(lag(essential_onlynew_D, 1:3)) + I(lag(visa_bannew_D, 1:3)) + I(lag(citizenship_bannew_D, 1:3)) + I(lag(history_bannew_D, 1:3)) +I(lag(border_closurenew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_ind, match.missing = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")


# Create Panel Matches for Stringency Treatment using Propensity Score
Stringency_psm <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "Stringency_Treat", refinement.method = "ps.match", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(work_excepnew_D, 1:3)) + I(lag(specific_countrynew_D, 1:3)) + I(lag(citizen_excepnew_D, 1:3)) + I(lag(essential_onlynew_D, 1:3)) + I(lag(visa_bannew_D, 1:3)) + I(lag(citizenship_bannew_D, 1:3)) + I(lag(history_bannew_D, 1:3)) +I(lag(border_closurenew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_ind, match.missing = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")


# Calculate results using matched sets
Results_SI_m <- PanelEstimate(sets = Stringency_m, data = TEST_ind)
Results_SI_ps <- PanelEstimate(sets = Stringency_psm, data = TEST_ind)



plot(Results_SI_m)
summary(Citizenshipban_m$att)


# Generate Figure 2
plot_table_m <- as.data.frame(Results_SI_m$estimates)
colnames(plot_table_m) <- "estimate"
plot_table_m$high <- Results_SI_m$estimates + (Results_SI_m$standard.error*1.96)
plot_table_m$low <- Results_SI_m$estimates - (Results_SI_m$standard.error*1.96)
plot_table_m$Matching <- "Mahalanobis" 
plot_table_m$Week <- rownames(plot_table_m)
plot_table_ps <- as.data.frame(Results_SI_ps$estimates)
colnames(plot_table_ps) <- "estimate"
plot_table_ps$high <- Results_SI_ps$estimates + (Results_SI_ps$standard.error*1.96)
plot_table_ps$low <- Results_SI_ps$estimates - (Results_SI_ps$standard.error*1.96)
plot_table_ps$Matching <- "Propensity Score" 
plot_table_ps$Week <- rownames(plot_table_ps)
plot_table <- rbind(plot_table_m, plot_table_ps)


# Plot
pd = position_dodge(.3)   
ggplot(plot_table, aes(x = Week, y = estimate, color = Matching)) + geom_hline(yintercept = 0, color = "gray40", linetype="dashed") +
  geom_point(shape = 16, size  = 6, position = pd) +
  geom_errorbar(aes(ymin  = low, ymax  = high), width = 0, size  = 1.4, position = pd) +
  theme_bw() + theme(axis.title = element_text(face = "bold")) + ylab("New Cases Per Capita (SD)") + scale_color_manual(values= c("gray20", "gray60")) + theme(axis.text = element_text(size = 19), axis.title = element_text(size = 19), text = element_text(size = 19), plot.title = element_text(hjust = 0.5)) + ggtitle("Domestic Lockdown")  + scale_y_continuous(limits = c(-0.55, 0.55), breaks=c(-0.5,-0.25,0,0.25,0.5))









# Complete Closures Test

Complete_m <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "competenew_D", refinement.method = "mahalanobis", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(competenew_D, 1:3)) + I(lag(partialnew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_c_p, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Complete_psm <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "competenew_D", refinement.method = "ps.match", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(competenew_D, 1:3)) + I(lag(partialnew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_c_p, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Results_CC_m <- PanelEstimate(sets = Complete_m, data = TEST_c_p)
Results_CC_ps <- PanelEstimate(sets = Complete_psm, data = TEST_c_p)



# Generate Figure 3
plot_table_m <- as.data.frame(Results_CC_m$estimates)
colnames(plot_table_m) <- "estimate"
plot_table_m$high <- Results_CC_m$estimates + (Results_CC_m$standard.error*1.96)
plot_table_m$low <- Results_CC_m$estimates - (Results_CC_m$standard.error*1.96)
plot_table_m$Matching <- "Mahalanobis" 
plot_table_m$Week <- rownames(plot_table_m)
plot_table_ps <- as.data.frame(Results_CC_ps$estimates)
colnames(plot_table_ps) <- "estimate"
plot_table_ps$high <- Results_CC_ps$estimates + (Results_CC_ps$standard.error*1.96)
plot_table_ps$low <- Results_CC_ps$estimates - (Results_CC_ps$standard.error*1.96)
plot_table_ps$Matching <- "Propensity Score" 
plot_table_ps$Week <- rownames(plot_table_ps)
plot_table <- rbind(plot_table_m, plot_table_ps)


# Plot
pd = position_dodge(.3)   
ggplot(plot_table, aes(x = Week, y = estimate, color = Matching)) + geom_hline(yintercept = 0, color = "gray40", linetype="dashed") +
  geom_point(shape = 16, size  = 6, position = pd) +
  geom_errorbar(aes(ymin  = low, ymax  = high), width = 0, size  = 1.4, position = pd) +
  theme_bw() + theme(axis.title = element_text(face = "bold")) + ylab("New Cases Per Capita (SD)") + scale_color_manual(values= c("gray20", "gray60")) + theme(axis.text = element_text(size = 19), axis.title = element_text(size = 19), text = element_text(size = 19), plot.title = element_text(hjust = 0.5)) + ggtitle("Complete Border Closures")  + scale_y_continuous(limits = c(-0.55, 0.55), breaks=c(-0.5,-0.25,0,0.25,0.5))






# Partial Closures Test
Partial_m <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "partialnew_D", refinement.method = "mahalanobis", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(competenew_D, 1:3)) + I(lag(partialnew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_c_p, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Partial_psm <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "partialnew_D", refinement.method = "ps.match", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(competenew_D, 1:3)) + I(lag(partialnew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_c_p, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Results_PC_m <- PanelEstimate(sets = Partial_m, data = TEST_c_p)
Results_PC_ps <- PanelEstimate(sets = Partial_psm, data = TEST_c_p)


# Generate Figure 4
plot_table_m <- as.data.frame(Results_PC_m$estimates)
colnames(plot_table_m) <- "estimate"
plot_table_m$high <- Results_PC_m$estimates + (Results_PC_m$standard.error*1.96)
plot_table_m$low <- Results_PC_m$estimates - (Results_PC_m$standard.error*1.96)
plot_table_m$Matching <- "Mahalanobis" 
plot_table_m$Week <- rownames(plot_table_m)
plot_table_ps <- as.data.frame(Results_PC_ps$estimates)
colnames(plot_table_ps) <- "estimate"
plot_table_ps$high <- Results_PC_ps$estimates + (Results_PC_ps$standard.error*1.96)
plot_table_ps$low <- Results_PC_ps$estimates - (Results_PC_ps$standard.error*1.96)
plot_table_ps$Matching <- "Propensity Score" 
plot_table_ps$Week <- rownames(plot_table_ps)
plot_table <- rbind(plot_table_m, plot_table_ps)


# Plot
pd = position_dodge(.3)   
ggplot(plot_table, aes(x = Week, y = estimate, color = Matching)) + geom_hline(yintercept = 0, color = "gray40", linetype="dashed") +
  geom_point(shape = 16, size  = 6, position = pd) +
  geom_errorbar(aes(ymin  = low, ymax  = high), width = 0, size  = 1.4, position = pd) +
  theme_bw() + theme(axis.title = element_text(face = "bold")) + ylab("New Cases Per Capita (SD)") + scale_color_manual(values= c("gray20", "gray60")) + theme(axis.text = element_text(size = 19), axis.title = element_text(size = 19), text = element_text(size = 19), plot.title = element_text(hjust = 0.5)) + ggtitle("Partial Border Closures")  + scale_y_continuous(limits = c(-0.55, 0.55), breaks=c(-0.5,-0.25,0,0.25,0.5))



# Test Individual Closure Policies -----------------------


Work_exempt <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "work_excepnew_D", refinement.method = "ps.match", covs.formula = ~  human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(work_excepnew_D, 1:3)) + I(lag(specific_countrynew_D, 1:3)) + I(lag(citizen_excepnew_D, 1:3)) + I(lag(essential_onlynew_D, 1:3)) + I(lag(visa_bannew_D, 1:3)) + I(lag(citizenship_bannew_D, 1:3)) + I(lag(history_bannew_D, 1:3)) + I(lag(border_closurenew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_ind, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Specific_country <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "specific_countrynew_D", refinement.method = "ps.match", covs.formula = ~  human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(work_excepnew_D, 1:3)) + I(lag(specific_countrynew_D, 1:3)) + I(lag(citizen_excepnew_D, 1:3)) + I(lag(essential_onlynew_D, 1:3)) + I(lag(visa_bannew_D, 1:3)) + I(lag(citizenship_bannew_D, 1:3)) + I(lag(history_bannew_D, 1:3)) +I(lag(border_closurenew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_ind, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Citizen_exempt <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "citizen_excepnew_D", refinement.method = "ps.match", covs.formula = ~  human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(work_excepnew_D, 1:3)) + I(lag(specific_countrynew_D, 1:3)) + I(lag(citizen_excepnew_D, 1:3)) + I(lag(essential_onlynew_D, 1:3)) + I(lag(visa_bannew_D, 1:3)) + I(lag(citizenship_bannew_D, 1:3)) + I(lag(history_bannew_D, 1:3)) +I(lag(border_closurenew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_ind, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Essential_only <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "essential_onlynew_D", refinement.method = "ps.match", covs.formula = ~  human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(work_excepnew_D, 1:3)) + I(lag(specific_countrynew_D, 1:3)) + I(lag(citizen_excepnew_D, 1:3)) + I(lag(essential_onlynew_D, 1:3)) + I(lag(visa_bannew_D, 1:3)) + I(lag(citizenship_bannew_D, 1:3)) + I(lag(history_bannew_D, 1:3)) +I(lag(border_closurenew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_ind, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Visa_ban <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "visa_bannew_D", refinement.method = "ps.match", covs.formula = ~  human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(work_excepnew_D, 1:3)) + I(lag(specific_countrynew_D, 1:3)) + I(lag(citizen_excepnew_D, 1:3)) + I(lag(essential_onlynew_D, 1:3)) + I(lag(visa_bannew_D, 1:3)) + I(lag(citizenship_bannew_D, 1:3)) + I(lag(history_bannew_D, 1:3)) +I(lag(border_closurenew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_ind, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

History_ban <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "history_bannew_D", refinement.method = "ps.match", covs.formula = ~  human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(work_excepnew_D, 1:3)) + I(lag(specific_countrynew_D, 1:3)) + I(lag(citizen_excepnew_D, 1:3)) + I(lag(essential_onlynew_D, 1:3)) + I(lag(visa_bannew_D, 1:3)) + I(lag(citizenship_bannew_D, 1:3)) + I(lag(history_bannew_D, 1:3)) +I(lag(border_closurenew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_ind, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Citizenship_ban <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "citizenship_bannew_D", refinement.method = "ps.match", covs.formula = ~  human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(work_excepnew_D, 1:3)) + I(lag(specific_countrynew_D, 1:3)) + I(lag(citizen_excepnew_D, 1:3)) + I(lag(essential_onlynew_D, 1:3)) + I(lag(visa_bannew_D, 1:3)) + I(lag(citizenship_bannew_D, 1:3)) + I(lag(history_bannew_D, 1:3)) +I(lag(border_closurenew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_ind, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Border_close <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "border_closurenew_D", refinement.method = "ps.match", covs.formula = ~  human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(work_excepnew_D, 1:3)) + I(lag(specific_countrynew_D, 1:3)) + I(lag(citizen_excepnew_D, 1:3)) + I(lag(essential_onlynew_D, 1:3)) + I(lag(visa_bannew_D, 1:3)) + I(lag(citizenship_bannew_D, 1:3)) + I(lag(history_bannew_D, 1:3)) +I(lag(border_closurenew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_ind, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")


Results_WE <- PanelEstimate(sets = Work_exempt, data = TEST_ind)
Results_SC <- PanelEstimate(sets = Specific_country, data = TEST_ind)
Results_EO <- PanelEstimate(sets = Essential_only, data = TEST_ind)
Results_VB <- PanelEstimate(sets = Visa_ban, data = TEST_ind)
Results_HB <- PanelEstimate(sets = History_ban, data = TEST_ind)
Results_CB <- PanelEstimate(sets = Citizenship_ban, data = TEST_ind)
Results_BC <- PanelEstimate(sets = Border_close, data = TEST_ind)
Results_CE <- PanelEstimate(sets = Citizen_exempt, data = TEST_ind)

summary(Results_WE)



# Islands Only Test (Figures 5-7) --------------------------


# Restricting to only island countries
TEST_Islands <- subset(TEST_c_p, iso3c %in% COBAP_Island_Countries_List$...2)



# Matching for Complete Islands Only
Complete_IS_m <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "competenew_D", refinement.method = "mahalanobis", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(competenew_D, 1:3)) + I(lag(partialnew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_Islands, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Complete_IS_psm <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "competenew_D", refinement.method = "ps.match", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(competenew_D, 1:3)) + I(lag(partialnew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_Islands, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Results_IS_C_m <- PanelEstimate(sets = Complete_IS_m, data = TEST_Islands)
Results_IS_C_ps <- PanelEstimate(sets = Complete_IS_psm, data = TEST_Islands)



# Generate Figure 5
plot_table_cm <- as.data.frame(Results_IS_C_m$estimates)
colnames(plot_table_cm) <- "estimate"
plot_table_cm$high <- Results_IS_C_m$estimates + (Results_IS_C_m$standard.error*1.96)
plot_table_cm$low <- Results_IS_C_m$estimates - (Results_IS_C_m$standard.error*1.96)
plot_table_cm$Matching <- "Mahalanobis" 
plot_table_cm$Week <- rownames(plot_table_cm)

plot_table_cps <- as.data.frame(Results_IS_C_ps$estimates)
colnames(plot_table_cps) <- "estimate"
plot_table_cps$high <- Results_IS_C_ps$estimates + (Results_IS_C_ps$standard.error*1.96)
plot_table_cps$low <- Results_IS_C_ps$estimates - (Results_IS_C_ps$standard.error*1.96)
plot_table_cps$Matching <- "Propensity Score" 
plot_table_cps$Week <- rownames(plot_table_cps)
plot_table1 <- rbind(plot_table_cm, plot_table_cps)


# Plot
pd = position_dodge(.3)   
ggplot(plot_table1, aes(x = Week, y = estimate, color = Matching)) + geom_hline(yintercept = 0, color = "gray40", linetype="dashed") +
  geom_point(shape = 16, size  = 5, position = pd) +
  geom_errorbar(aes(ymin  = low, ymax  = high), width = 0, size  = 1.2, position = pd) +
  theme_bw() + theme(axis.title = element_text(face = "bold")) + ylab("New Cases Per Capita (SD)") + scale_color_manual(values= c("gray20", "gray60")) + theme(axis.text = element_text(size = 17), axis.title = element_text(size = 17), text = element_text(size = 17), plot.title = element_text(hjust = 0.5)) + ggtitle("Complete Closures - Islands") + scale_y_continuous(limits = c(-0.55, 0.55), breaks=c(-0.5,-0.25,0,0.25,0.5))




# Testing for Partial Closures Islands Only
Partial_IS_m <-  PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "partialnew_D", refinement.method = "mahalanobis", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(competenew_D, 1:3)) + I(lag(partialnew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_Islands, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Partial_IS_ps <-  PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "partialnew_D", refinement.method = "ps.match", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(competenew_D, 1:3)) + I(lag(partialnew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_Islands, match.missing = TRUE, verbose = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Results_IS_p_m <- PanelEstimate(sets = Partial_IS_m, data = TEST_Islands)
Results_IS_p_ps <- PanelEstimate(sets = Partial_IS_ps, data = TEST_Islands)


# Generate Figure 6
plot_table_pm <- as.data.frame(Results_IS_p_m$estimates)
colnames(plot_table_pm) <- "estimate"
plot_table_pm$high <- Results_IS_p_m$estimates + (Results_IS_p_m$standard.error*1.96)
plot_table_pm$low <- Results_IS_p_m$estimates - (Results_IS_p_m$standard.error*1.96)
plot_table_pm$Matching <- "Mahalanobis" 
plot_table_pm$Week <- rownames(plot_table_pm)

plot_table_pps <- as.data.frame(Results_IS_p_ps$estimates)
colnames(plot_table_pps) <- "estimate"
plot_table_pps$high <- Results_IS_p_ps$estimates + (Results_IS_p_ps$standard.error*1.96)
plot_table_pps$low <- Results_IS_p_ps$estimates - (Results_IS_p_ps$standard.error*1.96)
plot_table_pps$Matching <- "Propensity Score" 
plot_table_pps$Week <- rownames(plot_table_cps)
plot_table2 <- rbind(plot_table_pm, plot_table_pps)

# Plot
pd = position_dodge(.3)   
ggplot(plot_table2, aes(x = Week, y = estimate, color = Matching)) + geom_hline(yintercept = 0, color = "gray40", linetype="dashed") +
  geom_point(shape = 16, size  = 5, position = pd) +
  geom_errorbar(aes(ymin  = low, ymax  = high), width = 0, size  = 1.2, position = pd) +
  theme_bw() + theme(axis.title = element_text(face = "bold")) + ylab("New Cases Per Capita (SD)") + scale_color_manual(values= c("gray20", "gray60")) + theme(axis.text = element_text(size = 17), axis.title = element_text(size = 17), text = element_text(size = 17), plot.title = element_text(hjust = 0.5)) + ggtitle("Partial Closures - Islands") + scale_y_continuous(limits = c(-0.55, 0.55), breaks=c(-0.5,-0.25,0,0.25,0.5))


Partial_IS_ps$att
summary(Results_IS_p_ps)


# Domestic Lockdown Validity Check for Islands only

Stringency_IS_m <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "Stringency_Treat", refinement.method = "mahalanobis", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(competenew_D, 1:3)) + I(lag(partialnew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_Islands, match.missing = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized")

Stringency_IS_psm <- PanelMatch(lag = 3, time.id = "week_count", unit.id = "countryN", treatment = "Stringency_Treat", refinement.method = "ps.match", covs.formula = ~ human_development_index + v2x_libdem + v2x_freexp_altinf + pop.2020_LOG + aged_70_older + life_expectancy + log_gdp_ppp + area_ifr + I(lag(competenew_D, 1:3)) + I(lag(partialnew_D, 1:3)) + I(lag(Stringency_revised, 1:3)) + I(lag(daily_tests_per_100k_LOG, 1:3)) + I(lag(DV_Standardized, 1:3)), data = TEST_Islands, match.missing = TRUE, qoi = "att", lead = 0:5, outcome.var = "DV_Standardized", verbose = TRUE)

Results_IS_s_m <- PanelEstimate(sets = Stringency_IS_m, data = TEST_Islands)
Results_IS_s_ps <- PanelEstimate(sets = Stringency_IS_psm, data = TEST_Islands)


# Generate Figure 7 (Appendix)
plot_table_m <- as.data.frame(Results_IS_s_m$estimates)
colnames(plot_table_m) <- "estimate"
plot_table_m$high <- Results_IS_s_m$estimates + (Results_IS_s_m$standard.error*1.96)
plot_table_m$low <- Results_IS_s_m$estimates - (Results_IS_s_m$standard.error*1.96)
plot_table_m$Matching <- "Mahalanobis" 
plot_table_m$Week <- rownames(plot_table_m)
plot_table_ps <- as.data.frame(Results_IS_s_ps$estimates)
colnames(plot_table_ps) <- "estimate"
plot_table_ps$high <- Results_IS_s_ps$estimates + (Results_IS_s_ps$standard.error*1.96)
plot_table_ps$low <- Results_IS_s_ps$estimates - (Results_IS_s_ps$standard.error*1.96)
plot_table_ps$Matching <- "Propensity Score" 
plot_table_ps$Week <- rownames(plot_table_ps)
plot_table <- rbind(plot_table_m, plot_table_ps)

# Plot
pd = position_dodge(.3)   
ggplot(plot_table, aes(x = Week, y = estimate, color = Matching)) + geom_hline(yintercept = 0, color = "gray40", linetype="dashed") +
  geom_point(shape = 16, size  = 5, position = pd) +
  geom_errorbar(aes(ymin  = low, ymax  = high), width = 0, size  = 1.2, position = pd) +
  theme_bw() + theme(axis.title = element_text(face = "bold")) + ylab("New Cases Per Capita (SD)") + scale_color_manual(values= c("gray20", "gray60")) + theme(axis.text = element_text(size = 17), axis.title = element_text(size = 17), text = element_text(size = 17), plot.title = element_text(hjust = 0.5)) + ggtitle("Domestic Lockdown - Islands") + scale_y_continuous(limits = c(-0.55, 0.55), breaks=c(-0.5,-0.25,0,0.25,0.5))



# Additional Panel Match functions ---------------

DisplayTreatment(unit.id = "countryN", time.id = "week_count", legend.position= "none",  xlab = "Week", ylab = "Country Code", treatment = "border_closurenew_D", data = TEST2,  matched.set = Stringency$att,  show.set.only = TRUE)

get_covariate_balance(Stringency$att, data = TEST2, covariates = c("border_closurenew_D"), plot = T,  ylim = c(-1, 1))









