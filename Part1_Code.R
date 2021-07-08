library(tidyverse)

# Making covid19 confirmed data tidy
covid19_c_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
covid19_confirmed <- covid19_c_raw %>% subset(select=-c(Lat,Long))
covid19_confirmed <- covid19_confirmed %>% rename(Country= 'Country/Region')
covid19_confirmed_tidy <- covid19_confirmed %>% pivot_longer(-c(Country, `Province/State`), names_to = 'day', values_to = 'confirmed' )
covid19_c_tidy <- covid19_confirmed_tidy %>% group_by(Country, day) %>% summarise(confirmed = sum(confirmed))

# Making covid19 deaths data tidy
covid19_d_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
covid19_deaths <- covid19_d_raw %>% subset(select=-c(Lat,Long))
covid19_deaths <- covid19_deaths %>% rename(Country= 'Country/Region')
covid19_deaths_tidy <- covid19_deaths %>% pivot_longer(-c(Country, `Province/State`), names_to = 'day', values_to = 'deaths' )
covid19_d_tidy <- covid19_deaths_tidy %>% group_by(Country, day) %>% summarise(deaths = sum(deaths))

# Joining covid19 confirmed and deaths data
covid19_cd <- left_join(covid19_d_tidy, covid19_c_tidy, by= c("Country", "day"))
covid19_cd <- covid19_cd %>% mutate(Country = replace(Country, Country == "Korea, South", "South Korea"))
covid19_cd <- covid19_cd %>% mutate(Country = replace(Country, Country == "Burma", "Myanmar"))
covid19_cd <- covid19_cd %>% mutate(Country = replace(Country, Country == "Russia", "Russian Federation"))

# Edit data frame for hospital bed data so column names names and variables align
hospital_beds <- read_csv("C:/Users/17vin/Downloads/WHS6_102.csv")
hospital_beds_recent <- hospital_beds %>% group_by(Country) %>% summarise(Year=max(Year))
bed_data <- inner_join(hospital_beds_recent, hospital_beds)
bed_data <- bed_data %>% mutate(Country = replace(Country, Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))
bed_data <- bed_data %>% mutate(Country = replace(Country, Country == "Iran (Islamic Republic of)", "Iran"))
bed_data <- bed_data %>% mutate(Country = replace(Country, Country == "Republic of Korea", "South Korea"))
bed_data <- bed_data %>% mutate(Country = replace(Country, Country == "Bolivia (Plurinational State of)", "Bolivia"))
bed_data <- bed_data %>% mutate(Country = replace(Country, Country == "Viet Nam", "Vietnam"))

# join covid confirmed + deaths data with hospital bed data
covid19_cd_beds <- left_join(covid19_cd, bed_data, by= c("Country")) %>% subset(select=-Year) # %>% rename(beds= 'Hospital beds (per 10 000 population)')

# prepare demographic data
demographics <- read_csv("C:/Users/17vin/Downloads/demographics.csv")

# removed mortality rates
# kept population data 
demo <- demographics %>% subset(select=-c(2,3))
demo <- demo %>% rename(Country= 'Country Name')
demo <- demo %>% pivot_wider(names_from = `Series Code`, values_from= YR2015)
demo <- demo %>% subset(select=-c(11, 12, 13, 14))
demo <- demo %>% rename(POP.TOTL= 'SP.POP.TOTL') %>% mutate(POP.80UP = SP.POP.80UP.FE + SP.POP.80UP.MA) %>% mutate(POP.65UP = SP.POP.65UP.FE.IN + SP.POP.65UP.MA.IN) %>% mutate(POP.1564 = SP.POP.1564.FE.IN + SP.POP.1564.MA.IN) %>% mutate(POP.0014 = SP.POP.0014.FE.IN + SP.POP.0014.MA.IN)
demo <- demo %>% subset(select=-c(5:12))
demo <- demo %>% rename(URBAN.POP = 'SP.URB.TOTL') %>% rename('Life Expectancy at Birth' = 'SP.DYN.LE00.IN')
demo <- demo %>% mutate(Country = replace(Country, Country == "United Kingdom of Great Britain and Northern Ireland", "United Kingdom"))
demo <- demo %>% mutate(Country = replace(Country, Country == "Iran, Islamic Rep.", "Iran"))
demo <- demo %>% mutate(Country = replace(Country, Country == "Korea, Rep.", "South Korea"))
demo <- demo %>% mutate(Country = replace(Country, Country == "Bahamas, The", "Bahamas"))
demo <- demo %>% mutate(Country = replace(Country, Country == "Egypt, Arab Rep.", "Egypt"))
demo <- demo %>% mutate(Country = replace(Country, Country == "Czech Republic", "Czechia"))
demo <- demo %>% mutate(Country = replace(Country, Country == "Slovak Republic", "Slovakia"))

# join cleaned-up demographic data with covid19 confirmed + deaths + beds data
covid19_data <- left_join(covid19_cd_beds, demo, by= c("Country"))

# how i found countries outside of South Korea, Iran, United Kingdom to change the names
# only took a few countries that had large death numbers
# also included Vietnam for fun
test <- covid19_data[rowSums(is.na(covid19_data)) > 0, ]
view(test %>% group_by(Country) %>% summarise(sum(deaths)))

# remove rows with Missing Data
covid19_clean_data <- na.omit(covid19_data)


# linear modeling code

# deaths vs confirmed cases
model_confirmed <- lm(data=covid19_clean_data, formula=deaths~confirmed)
summary(model_confirmed)

# deaths vs urban population proportion
covid19_clean_data <- covid19_clean_data %>% mutate(URBAN.POP.PROP = URBAN.POP / POP.TOTL)
model_urb_pop_prop <- lm(data=covid19_clean_data, formula=deaths~URBAN.POP.PROP)
summary(model_urb_pop_prop)

# deaths vs beds + urban population proportion
model_bed_urb <- lm(data=covid19_clean_data, formula=deaths~`Hospital beds (per 10 000 population)`+ URBAN.POP.PROP)
summary(model_bed_urb)

# deaths vs proportion of confirmed to beds
model_bed_conf <- lm(data=covid19_clean_data, formula=deaths~(confirmed / `Hospital beds (per 10 000 population)`))
summary(model_bed_conf)

# deaths vs total population
model_totlpop <- lm(data=covid19_clean_data, formula=deaths~POP.TOTL)
summary(model_totlpop)

# deaths vs proportion of confirmed from total population
model_conf_tpop <- lm(data=covid19_clean_data, formula=deaths~(confirmed / POP.TOTL))
summary(model_conf_tpop)

# deaths vs proportion of beds to confirmed cases
model_bed_conf <- lm(data=covid19_clean_data, formula=deaths~(`Hospital beds (per 10 000 population)` / confirmed))
summary(model_bed_conf)
