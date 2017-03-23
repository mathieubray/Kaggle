library(readr)
library(plyr)
library(dplyr)
library(lubridate)
library(tm)

### Scrapers

url.male.names <- "http://names.mongabay.com/male_names_alpha.htm"
url.female.names <- "http://names.mongabay.com/female_names_alpha.htm"
url.surnames <- "http://names.mongabay.com/most_common_surnames.htm"

male.names <- url.male.names %>%
  read_html %>%
  html_nodes('#myTable') %>%
  html_table(header=T) %>%
  data.frame(stringsAsFactors=T)

female.names <- url.female.names %>%
  read_html %>%
  html_nodes('#myTable') %>%
  html_table(header=T) %>%
  data.frame(stringsAsFactors=T)

common.names <- data.frame(Name=unique(c(male.names$Name,female.names$Name)),stringsAsFactors=F)

write.csv(common.names,"Detroit/Common_Names.csv",row.names=F)

surnames <- url.surnames %>%
  read_html %>%
  html_nodes('#myTable') %>%
  html_table(header=T) %>%
  data.frame(stringsAsFactors=T) %>%
  select(Surname) %>%
  rename(Name=Surname)

write.csv(surnames,"Detroit/Surnames.csv",row.names=F)

##################################################################################################

### Load Data

train <- read_csv("Detroit/Blight_Train.csv")
test <- read_csv("Detroit/Blight_Test.csv")

common.names <- read.csv("Detroit/Common_Names.csv",header=T,stringsAsFactors=F) %>% select(Name)
surnames <- read.csv("Detroit/Surnames.csv",header=T,stringsAsFactors=F) %>% select(Name)
other.common.name.elements <- c("JR","SR","II","III")

### Collect Common Business Terms

list.of.violators <- c(train$violator_name,test$violator_name) %>%
  toupper %>%
  removePunctuation %>%
  removeNumbers %>%
  strsplit(split = " ") %>%
  unlist %>%
  trimws

business.terms <- (data.frame(term=list.of.violators,stringsAsFactors=F) %>%
  group_by(term) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  filter(!(term %in% common.names$Name), 
         !(term %in% surnames$Name),
         !(term %in% other.common.name.elements),
         nchar(term) >= 2) %>%
  top_n(200) %>%
  arrange(desc(n)))$term

# Collect Common Grouping Variables

common.dispositions <- (data.frame(disposition=c(train$disposition,test$disposition),stringsAsFactors=F) %>%
  group_by(disposition) %>%
  summarize(n = n()) %>%
  filter(!grepl(pattern="Not",x=disposition), n > 1000))$disposition

common.code.descriptions <- (data.frame(description=c(train$violation_description,test$violation_description),stringsAsFactors=F) %>%
  group_by(description) %>% 
  summarize(n = n()) %>% 
  filter(n > 1000))$description

# Collect Common Cities

replace.redundant.cities <- function(x) {
  
  x[x %in% c("DET","DET.")] <- "DETROIT"
  x[x %in% c("W. BLOOMFIELD","W BLOOMFIELD")] <- "WEST BLOOMFIELD"
  x[x %in% c("DEARBORN HGTS")] <- "DEARBORN HEIGHTS"
  x[x %in% c("STERLING HGTS")] <- "STERLING HEIGHTS"
  x[x %in% c("ST. CLAIR SHORES")] <- "ST CLAIR SHORES"
  
  return(x)
  
}

common.cities <- (data.frame(city=c(toupper(train$city),toupper(test$city)),stringsAsFactors=F) %>%
  mutate(city = replace.redundant.cities(city)) %>%
  group_by(city) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  top_n(50))$city


# Additional Functions for Cleaning Data

is.business <- function(violator){
  
  violator.tokens <- violator %>%
    toupper %>%
    removePunctuation %>%
    removeNumbers %>%
    strsplit(split = " ") %>%
    unlist %>%
    trimws
 
  business <- length(intersect(violator.tokens,business.terms))
  person <- length(intersect(violator.tokens,c(common.names,surnames,other.common.name.elements)))
  
  split.on.comma <- unlist(strsplit(removeNumbers(toupper(violator)), split = ", ", fixed=T))
  if (length(split.on.comma) == 2 | (length(split.on.comma) == 3 & (nchar(split.on.comma[3]) == 1 | split.on.comma[3] %in% other.common.name.elements))){
    person <- person + 0.5
  }
  
  if (business > person){
    return ("BUSINESS")
  } else if (business < person){
    return ("PERSON")
  } else {
    return ("NOT SURE")
  }
  
}

is.matching.address <- function(violation_street, violation_number, mailing_street, mailing_number){
  
  violation_street_clean = removePunctuation(toupper(violation_street))
  mailing_street_clean = removePunctuation(toupper(mailing_street))
  
  return(violation_street == mailing_street & violation_number == mailing_number)
}


# Clean data

train.clean <- train %>%
  dplyr::filter(
    !is.na(compliance), # Drop violators found not responsible
    country=="USA" # Focus on US (only US in test data)
  ) %>% 
  dplyr::select(
    -inspector_name, # Not informative
    -violation_zip_code, # All NAs
    -non_us_str_code, # Not relevant for US
    -country, # All US
    -admin_fee, # Not informative, same value for all US
    -state_fee, # Not informative, same value for all US
    -clean_up_cost, # Not informative, same value for all US
    -payment_amount, # Not included in test set
    -balance_due, # Not included in test set
    -payment_date, # Not included in test set
    -payment_status, # Not included in test set
    -collection_status, # Not included in test set
    -grafitti_status, # All NAs
    -compliance_detail # Not included in test set
  ) %>% 
  dplyr::mutate(
    disposition = ifelse(disposition %in% common.dispositions,disposition,"Other"),
    city = replace.redundant.cities(toupper(city)),
    city_name = ifelse(city %in% common.cities,city,"Other"),
    matching_address = is.matching.address(violation_street_name,violation_street_number,mailing_address_str_name,mailing_address_str_number),
    hearing_date_month = month(hearing_date,label=T,abbr=T),
    hearing_date_year = year(hearing_date),
    hearing_date_weekday = as.character(wday(hearing_date,label=T,abbr=T)),
    ticket_issued_date_month = month(ticket_issued_date,label=T,abbr=T),
    ticket_issued_date_year = year(ticket_issued_date),
    ticket_issued_date_weekday = as.character(wday(ticket_issued_date,label=T,abbr=T)),
    violation = ifelse(violation_description %in% common.code.descriptions,violation_description,"Other"),
    no_late_fee = late_fee == 0,
    has_discount = discount_amount != 0
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    business = is.business(violator_name),
    days_until_hearing = round(as.numeric(difftime(hearing_date,ticket_issued_date)))
  ) %>%
  dplyr::select(-violator_name,-violation_street_name,-violation_street_number,-mailing_address_str_name,-mailing_address_str_number,
                -city,-state,-zip_code,-hearing_date,-ticket_issued_date,-violation_code,-violation_description,-late_fee,-discount_amount)

test.clean <- test %>%
  dplyr::select(
    -inspector_name, # Not informative
    -violation_zip_code, # All NAs
    -non_us_str_code, # Not relevant for US
    -country, # All US
    -admin_fee, # Not informative, same value for all US
    -state_fee, # Not informative, same value for all US
    -clean_up_cost, # Not informative, same value for all US
    -grafitti_status # All NAs
  ) %>% 
  dplyr::mutate(
    disposition = ifelse(disposition %in% common.dispositions,disposition,"Other"),
    city = replace.redundant.cities(toupper(city)),
    city_name = ifelse(city %in% common.cities,city,"Other"),
    matching_address = is.matching.address(violation_street_name,violation_street_number,mailing_address_str_name,mailing_address_str_number),
    hearing_date_month = month(hearing_date,label=T,abbr=T),
    hearing_date_year = year(hearing_date),
    hearing_date_weekday = as.character(wday(hearing_date,label=T,abbr=T)),
    ticket_issued_date_month = month(ticket_issued_date,label=T,abbr=T),
    ticket_issued_date_year = year(ticket_issued_date),
    ticket_issued_date_weekday = as.character(wday(ticket_issued_date,label=T,abbr=T)),
    violation = ifelse(violation_description %in% common.code.descriptions,violation_description,"Other"),
    no_late_fee = late_fee == 0,
    has_discount = discount_amount != 0
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    business = is.business(violator_name),
    days_until_hearing = round(as.numeric(difftime(hearing_date,ticket_issued_date)))
  ) %>%
  dplyr::select(-violator_name,-violation_street_name,-violation_street_number,-mailing_address_str_name,-mailing_address_str_number,
                -city,-state,-zip_code,-hearing_date,-ticket_issued_date,-violation_code,-violation_description,-late_fee,-discount_amount)



write.csv(train.clean,"Detroit/Blight_Train_Clean.csv",row.names=F)
write.csv(test.clean,"Detroit/Blight_Test_Clean.csv",row.names=F)
