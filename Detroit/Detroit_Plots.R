library(readr)
library(plyr)
library(dplyr)
library(ggmap)
library(gganimate)

citation('ggmap')

# Fix data

addresses <- read_csv("Detroit/Addresses.csv")
latlon <- read_csv("Detroit/LatitudeLongitude.csv")
train_clean <- read_csv("Detroit/Blight_Train_Clean.csv")
test_clean <- read_csv("Detroit/Blight_Test_Clean.csv")

train_clean$set <- "Training"
test_clean$set <- "Testing"
test_clean$compliance <- NA

blight.data <- rbind(train_clean,test_clean) %>%
  left_join(addresses, by="ticket_id") %>%
  left_join(latlon, by="address") %>%
  filter(!is.na(lat),!is.na(lon)) %>%
  mutate(hearing_date_weekend = ifelse(ticket_issued_date_weekday %in% c("Sat","Sun"),"Weekend","Weekday"),
         ticket_issued_weekend = ifelse(ticket_issued_date_weekday %in% c("Sat","Sun"),"Weekend","Weekday"),
         hearing_date_weekday = factor(hearing_date_weekday, levels=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri","Sat")),
         hearing_date_month = factor(hearing_date_month, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")),
         ticket_issued_date_weekday = factor(ticket_issued_date_weekday, levels=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri","Sat")),
         ticket_issued_date_month = factor(ticket_issued_date_month, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))

# Set up maps

detroit <- get_map(location="detroit",zoom=11)

base.plot <- ggmap(detroit) +
  theme(text=element_text(size=12),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())



### Scatter Plots

# Weekday/Weekend vs. Agency

weekend.by.agency <- base.plot + 
  geom_point(aes(x = lon, y = lat, colour = agency_name), size = 0.1, data = blight.data) +
  facet_wrap(~ticket_issued_weekend) +
  guides(colour = guide_legend(title="Agency",
                               override.aes = list(size=3))) 

ggsave("Detroit/Plots/Agencies_by_Day_of_Week.png",weekend.by.agency)


# Business/Person vs. Compliance

blight.data.business <- blight.data %>%
  filter(!is.na(compliance), business != "NOT SURE")

compliance.by.violator <- base.plot + 
  geom_point(aes(x = lon, y = lat, colour = factor(compliance,labels=c("NonCompliant","Compliant"))),
             size = 0.1,
             data = blight.data.business) +
  facet_wrap(~business) +
  guides(colour = guide_legend(title="Compliance",
                               override.aes = list(size=3)))

ggsave("Detroit/Plots/Compliance_by_Violator.png",compliance.by.violator)



### Tile Plots

# Marginal Ticket and Compliance Distributions

binned.map.data <- blight.data %>%
  mutate(roundlat = as.factor(round(lat,2)),
         roundlon = as.factor(round(lon,2))) %>%
  group_by(roundlat,roundlon) %>%
  summarize(n = n(),
            pct = sum(compliance,na.rm=T)/length(!is.na(compliance))) %>%
  ungroup() %>%
  mutate(lat = as.numeric(as.character(roundlat)),
         lon = as.numeric(as.character(roundlon)))

ticket.plot <- base.plot +
  geom_tile(data = binned.map.data, aes(x = lon, y = lat, alpha = n), fill="purple")+
  ggtitle("Ticket Distribution")+
  labs(alpha="Tickets")

compliance.plot <- base.plot +
  geom_tile(data = filter(binned.map.data,n >= 10), aes(x = lon, y = lat, alpha = pct), fill="purple")+
  ggtitle("Compliance Distribution")+
  labs(alpha="Compliance")

ggsave("Detroit/Plots/Ticket_Distribution.png",ticket.plot)
ggsave("Detroit/Plots/Compliance_Distribution.png",compliance.plot)


# Distributions by Business/Residence

binned.map.data.business <- blight.data %>%
  mutate(roundlat = as.factor(round(lat,2)),
         roundlon = as.factor(round(lon,2))) %>%
  group_by(business,roundlat,roundlon) %>%
  summarize(n = n(),
            pct = sum(compliance,na.rm=T)/length(!is.na(compliance))) %>%
  ungroup() %>%
  mutate(lat = as.numeric(as.character(roundlat)),
         lon = as.numeric(as.character(roundlon))) %>%
  filter(business != "NOT SURE",n > 20) %>%
  mutate(business = factor(business))

levels(binned.map.data.business$business) <- c("Business", "Residential")

compliance.plot.business <- base.plot +
  geom_tile(data = binned.map.data.business, aes(x = lon, y = lat, alpha=pct), fill="purple") +
  facet_wrap(~business, ncol=1) +
  labs(alpha="Compliance") 

ggsave("Detroit/Plots/Compliance_by_Business.png",compliance.plot.business)


# Distributions by Matching Address

binned.map.data.address <- blight.data %>%
  mutate(roundlat = as.factor(round(lat,2)),
         roundlon = as.factor(round(lon,2))) %>%
  group_by(matching_address,roundlat,roundlon) %>%
  summarize(n = n(),
            pct = sum(compliance,na.rm=T)/length(!is.na(compliance))) %>%
  ungroup() %>%
  mutate(lat = as.numeric(as.character(roundlat)),
         lon = as.numeric(as.character(roundlon))) %>%
  filter(n > 20) %>%
  mutate(matching_address = factor(matching_address))

levels(binned.map.data.address$matching_address) <- c("Addresses Do Not Match","Addresses Match")

compliance.plot.address <- base.plot +
  geom_tile(data = binned.map.data.address, aes(x = lon, y = lat, alpha = pct), fill="purple") +
  facet_wrap(~matching_address, ncol=1) +
  labs(alpha="Compliance")

ggsave("Detroit/Plots/Compliance_by_Address.png",compliance.plot.address)



### GIFs

# Ticket Distribution by Day of Week

binned.map.data.by.weekday <- blight.data %>%
  mutate(roundlat = as.factor(round(lat,2)),
         roundlon = as.factor(round(lon,2))) %>%
  group_by(roundlat,roundlon,hearing_date_weekday) %>%
  summarize(n = n(),
            pct = sum(compliance,na.rm=T)/length(!is.na(compliance))) %>%
  ungroup() %>%
  mutate(lat = as.numeric(as.character(roundlat)),
         lon = as.numeric(as.character(roundlon)),
         weekday = factor(hearing_date_weekday, levels=c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri","Sat")))
  
ticket.weekday.gif <- base.plot +
  geom_tile(data = binned.map.data.by.weekday, aes(x = lon, y = lat, alpha = n, frame = weekday, cumulative=F), fill="red")+
  ggtitle("Ticket Distribution")+
  labs(alpha="Tickets")

gganimate(ticket.weekday.gif,"Detroit/Plots/Tickets_by_Weekday.gif")


# Ticket Distribution by Month, Split by Business

binned.map.data.by.month <- blight.data %>%
  mutate(roundlat = as.factor(round(lat,2)),
         roundlon = as.factor(round(lon,2))) %>%
  group_by(roundlat,roundlon,hearing_date_month,business) %>%
  summarize(n = n(),
            pct = sum(compliance,na.rm=T)/length(!is.na(compliance))) %>%
  ungroup() %>%
  mutate(lat = as.numeric(as.character(roundlat)),
         lon = as.numeric(as.character(roundlon)),
         month = factor(hearing_date_month, levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")))


ticket.month.gif <- base.plot +
  geom_tile(data = binned.map.data.by.month, aes(x = lon, y = lat, alpha = n, frame = month, cumulative=F), fill="red")+
  ggtitle("Ticket Distribution")+
  labs(alpha="Tickets")

gganimate(ticket.month.gif,"Detroit/Plots/Tickets_by_Month.gif") 

ticket.month.business.gif <- base.plot +
  geom_tile(data = filter(binned.map.data.by.month,business!="NOT SURE"), aes(x = lon, y = lat, alpha = n, frame = month, cumulative=F), fill="red")+
  facet_wrap(~business) +
  ggtitle("Ticket Distribution")+
  labs(alpha="Tickets")

gganimate(ticket.month.business.gif,"Detroit/Plots/Tickets_by_Month_Business.gif")

 



















