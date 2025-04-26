setwd("/home/alex/Pictures")

#dd <- read.table("hotel_booking_5000_rows.csv",header=T);

#dd <- read.table("credsco.csv",header=T, sep=";",na.strings="\"\"");
#dd <- read.table("credsco.csv",header=T, sep=";");

dd <- read.csv("hotel_booking_5000_rows.csv", stringsAsFactors=TRUE);

class(dd)
dim(dd)
n<-dim(dd)[1]
n
K<-dim(dd)[2]
K
names(dd)

library(dplyr)

month_mapping <- setNames(1:12, month.name)
dd <- dd %>%
  # mutación temporal para crear "arrival_date"
  mutate(
    arrival_date_year = as.character(arrival_date_year),
    arrival_date_month = as.character(month_mapping[as.character(arrival_date_month)]),
    arrival_date_day_of_month = as.character(arrival_date_day_of_month),
  )


# creación arrival_date, combinación 3 columnas
dd <- dd %>%
  mutate(
    arrival_date = as.Date(
      paste(arrival_date_year, arrival_date_month, arrival_date_day_of_month, sep = "-"),
      format = "%Y-%m-%d"
    )
  )

dd <- dd %>%
  mutate(
    hotel = as.factor(hotel),
    is_canceled = as.factor(is_canceled),
    meal = as.factor(meal),
    country = as.factor(country),
    market_segment = as.factor(market_segment),
    distribution_channel = as.factor(distribution_channel),
    is_repeated_guest = as.factor(is_repeated_guest),
    reserved_room_type = as.factor(reserved_room_type),
    assigned_room_type = as.factor(assigned_room_type),
    deposit_type = as.factor(deposit_type),
    customer_type = as.factor(customer_type),
    reservation_status = as.factor(reservation_status),
    company = as.factor(company),
    arrival_date_year = as.factor(arrival_date_year),
    arrival_date_month = as.factor(arrival_date_month),
    arrival_date_day_of_month = as.factor(arrival_date_day_of_month),
    arrival_date_week_number = as.factor(arrival_date_week_number)
  )

#select some variables
#DO NOT INCLUDE identifiers in the analysis!!!!
#actives<-c(1:5,7:11, 13:23, 26:32)
#actives<-c(1:5,7:11, 13:23, 26:32)
dd <- dd %>%
  select(-c(total_of_special_requests, previous_bookings_not_canceled, previous_cancellations, 
            reservation_status_date, deposit_type, company, agent, 
            distribution_channel, market_segment, required_car_parking_spaces, reserved_room_type, 
            assigned_room_type, is_repeated_guest, name, email, phone.number, credit_card, booking_changes,
            customer_type)
         )
#ddActives<-dd[,actives]

#Care! update K

K<-dim(dd)[2]

str(dd)

# Change variables' names

dd <- dd %>% rename(can = is_canceled)
dd <- dd %>% rename(lt = lead_time)
dd <- dd %>% rename(arr_y = arrival_date_year)
dd <- dd %>% rename(arr_m = arrival_date_month)
dd <- dd %>% rename(arr_wn  = arrival_date_week_number)
dd <- dd %>% rename(arr_dm = arrival_date_day_of_month)
dd <- dd %>% rename(s_wend_n = stays_in_weekend_nights)
dd <- dd %>% rename(s_wday_n  = stays_in_week_nights)
#dd <- dd %>% rename(mkt_s = market_segment)
#dd <- dd %>% rename(dst_ch = distribution_channel)
#dd <- dd %>% rename(prev_can = previous_cancellations)
#dd <- dd %>% rename(pb_nc = previous_bookings_not_canceled)
#dd <- dd %>% rename(res_rt = reserved_room_type)
#dd <- dd %>% rename(as_rt  = assigned_room_type)
#dd <- dd %>% rename(bk_ch = booking_changes)
#dd <- dd %>% rename(dp_t = deposit_type)
dd <- dd %>% rename(d_wl = days_in_waiting_list)
#dd <- dd %>% rename(c_type = customer_type)
#dd <- dd %>% rename(req_prk = required_car_parking_spaces)
#dd <- dd %>% rename(sp_req = total_of_special_requests)
dd <- dd %>% rename(res_s = reservation_status)
#dd <- dd %>% rename(reservation_status_date = reservation_status_date)
dd <- dd %>% rename(arr = arrival_date)

#write.table(dd, file = "BookingCleanPrueba.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

numeric_cols <- dd %>%
  select(where(is.numeric))

qualitative_cols <- dd %>%
  select(where(is.factor))


#seleccion some rows
#Dselection<-dd[Condition, ]

library(ggplot2)
library(GGally)

descriptiva<-function(X, nom){
  if (is.numeric(X) && class(X)!= "Date"){
    hist(X, main=paste("Histogram of", nom))
    boxplot(X, horizontal=TRUE, main=paste("Boxplot of",nom))
    print("Extended Summary Statistics")
    
    cat("Extended Summary Statistics:\n")
    print(summary(X))
    
    cat("Standard Deviation:\n")
    print(sd(X, na.rm = TRUE))
    
    cat("Variation Coefficient:\n")
    print(sd(X, na.rm = TRUE) / mean(X, na.rm = TRUE))
    
    #print(summary(X))
    #print(paste("sd: ", sd(X, na.rm=TRUE)))
    #print(paste("vc: ", sd(X, na.rm=TRUE)/mean(X, na.rm=TRUE)))
  }
  
}

dataset<-dd
actives<-c(1:K)

#Basic descriptive analysis for numerical variables
#(decide the maximum number of colors you can need in a graph based on your metadata file)

listOfColors<-rainbow(39)

par(ask=TRUE)

#Output graphs to a pdf
pdf("/home/alex/Downloads/histograms.pdf")
for(k in actives){
  print(paste("variable ", k, ":", names(dd)[k] ))
  descriptiva(dd[,k], names(dd)[k])
}
dev.off()  # Close PDF after all plots are generated
par(ask=FALSE)

# Replace instances in days_in_waiting_list that are > 300 with NA
class(dd)
#attach(dd)
# Directly reference the data frame column (d_wl)
dd$d_wl[dd$d_wl > 300] <- NA

# Now, you can check how many NA values you have
prueba <- dd$d_wl[is.na(dd$d_wl)]
length(prueba)

# Step 1: Identify numerical variables in the dataset
numeric_vars <- names(dd)[sapply(dd, is.numeric)]  # Select only numeric variables

# Step 2: Ensure "d_wl" (or the variable you want to impute) is in the selected variables
fullVariables <- unique(c("d_wl", numeric_vars))  # Add "d_wl" if not already included

# Step 3: Create the auxiliary matrix with selected numerical variables
aux <- dd[, fullVariables, drop = FALSE]  # Keep it as a data frame

# Step 4: Remove rows where any predictor (except "d_wl") has missing values
aux_clean <- aux[complete.cases(aux[, -which(names(aux) == "d_wl")]), ]

# divide in rows that had missing incomes or not on the target variable to be imputed
aux1 <- aux_clean[!is.na(aux_clean$d_wl),]
dim(aux1)
aux2 <- aux_clean[is.na(aux_clean$d_wl),]
dim(aux2)

#Find nns for aux2
#knn.ing = knn(aux1,aux2,d_wl[!is.na(d_wl)])

# Step 6: Apply KNN for imputation
library(class)  # Load the necessary package
knn.ing <- knn(
  train = aux1[, -which(names(aux1) == "d_wl")],  # Predictors only
  test  = aux2[, -which(names(aux2) == "d_wl")],  # Predictors only
  cl    = aux1$d_wl,  # Known values of d_wl
  k     = 10  # Choose an appropriate k
)

#CARE: neither aux1 nor aux2 can contain NAs


#CARE: knn.ing is generated as a factor. 
#Be sure to retrieve the correct values

days_in_waiting_listOriginal<-d_wl

# Step 7: Assign Imputed Values
dd$d_wl[is.na(dd$d_wl)] <- as.numeric(levels(knn.ing))[knn.ing]
 
# Step 8: Verify Results
summary(dd$d_wl)  # Check if the missing values were imputed properly



#saving the dataframe in an external file
write.table(dd, file = "BookingClean.csv", sep = ";", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

