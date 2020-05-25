# installing packages and reading excel file 
install.packages("readxl")
library(stringr)
library(lubridate)
library(tidyverse)
library(modelr)
library(sp)
library(leaflet)
library(geosphere)
library(knitr)
library(rpart)

read_excel("c:/Users/Parachute/Desktop/Database/Educations/Harvard_R\\anz_data.xlsx",sheet = "DSynth_Output_100c_3m_v3")
# examine the summary of the dataset
summary(anz_data)
str(anz_data)

#1.2 Cleaning and preparing the dataset

# change the format of date column
anz_data$date<- as.Date(anz_data$date,format = "%d/%m/%Y")

# the dateset only contain records for 91 days, one day is missing
DateRange <- seq(min(anz_data$date), max(anz_data$date), by = 1)
DateRange[!DateRange %in% anz_data$date] 
# 2018-08-16 transactions are missing

# derive weekday and hour data of each transaction
anz_data$extraction = as.character(anz_data$extraction)
anz_data$hour = hour(as.POSIXct(substr(anz_data$extraction,12,19),format="%H:%M:%S"))
anz_data$weekday = weekdays(anz_data$date)

# confirm the one -to -one link of account_id and customer_id
anz_data %>% select(account,customer_id) %>%
        unique() %>%
        nrow()

# split customer & merchant lat_long into individual columns for analysis 
anz_dataloc = anz_data[,c("long_lat","merchant_long_lat")]
anz_dataloc<- anz_dataloc %>% separate("long_lat", c("c_long", "c_lat"),sep=' ')
anz_dataloc<- anz_dataloc %>% separate("merchant_long_lat", c("m_long", "m_lat"),sep=' ')
anz_dataloc<- data.frame(sapply(anz_dataloc, as.numeric))
anz_data <- cbind(anz_data,anz_dataloc)

#basic insights

# classify based on payment status
auth <- sum(anz_data$status=="authorized")
auth
post <- sum(anz_data$status=="posted")
post
payment <- table(anz_data$status)
barplot(payment, main="Payment Status", col=c("darkblue","red"), horiz=TRUE, xlim=c(0,8000), legend = rownames(payment))

# classifying based on gender
male <- sum(anz_data$gender=="M")
male
female <- sum(anz_data$gender=="F")
female
gender <- table(anz_data$gender)
barplot(gender, main="Gender", col=c("darkblue","red"), horiz=TRUE, xlim=c(0,8000), legend = rownames(gender))

# filtering out purchase transactions only
# assuming purchase transactions must be associated with a merchant (have a merchant Id)
anz_data_temp <- anz_data %>% filter(merchant_id != '' )
# it turned out that is equivilent to excluding following categories of transactions
anz_data_csmp <- anz_data %>%filter(!(txn_description %in% c('PAY/SALARY',"INTER BANK", "PHONE BANK","PAYMENT")))
summary(anz_data_csmp)

# visualise the distribution of transaction amount
hist(anz_data_csmp$amount[!anz_data_csmp$amount %in% boxplot.stats(anz_data_csmp$amount)$out], #exclude outliers
     xlab= 'Transaction Amount', main = 'Histogram of purchase transaction amount')

hist(anz_data$amount[!anz_data$amount %in% boxplot.stats(anz_data$amount)$out], #exclude outliers
     xlab= 'Transaction Amount',main = 'Histogram of overall transaction amount')

# visualise the credit vs debit amount
debit <- which(clean_anzdata$movement == "debit")
avg_debit <- mean(clean_anzdata$amount[debit])
credit <- which(clean_anzdata$movement == "credit")
avg_credit<- mean(clean_anzdata$amount[credit])
avg_amount <- c(avg_debit, avg_credit )
barplot(avg_amount, main="Average debit vs credit", col=c("yellow","blue"), horiz=TRUE, xlim=c(0,2000))
legend("bottomright",
       c("avg debit","avg_credit"),
       fill = c("yellow","blue")
)
mean_transac <- mean(clean_anzdata$amount)
mean_transac

total_credit <- sum(clean_anzdata$amount[credit])
total_debit <- sum(clean_anzdata$amount[debit])
totals <- c(total_credit, total_debit )
barplot(totals, main="Total debit vs credit", col=c("orange","green"), horiz=TRUE, xlim=c(0,2000000))
legend("topright",
       c("total_credit","total_debit"),
       fill = c("orange","green")
)

#Visualise customers'average monthly transaction volume
anz_data2 <- anz_data %>%
        group_by(customer_id) %>%
        summarise(mon_avg_vol = round(n()/3,0))
hist(anz_data2$mon_avg_vol,
     xlab= 'Monthly transaction volume', ylab='No. of customers', main = "Histogram of customer
s' monthly transaction volume")

#segmenting the dataset into weeks 
day1 <- anz_data$date >= "2018-08-01"
day7 <- anz_data$date < "2018-08-08"
week1 <- day1 & day7
avg_wk1 <- mean(anz_data$amount[week1])
tot_wk1 <- sum(anz_data$amount[week2])

day2 <- anz_data$date >= "2018-08-08"
day8 <- anz_data$date < "2018-08-15"
week2 <- day2 & day8
avg_wk2 <- mean(anz_data$amount[week2])
tot_wk2 <- sum(anz_data$amount[week2])

day3 <- anz_data$date >= "2018-08-15"
day9 <- anz_data$date < "2018-08-22"
week3 <- day3 & day9
avg_wk3 <- mean(anz_data$amount[week3])
tot_wk3 <- sum(anz_data$amount[week3])

day4 <- anz_data$date >= "2018-08-22"
day10 <- anz_data$date < "2018-08-29"
week4 <- day4 & day10
avg_wk4 <- mean(anz_data$amount[week4])
tot_wk4 <- sum(anz_data$amount[week4])

day5 <- anz_data$date >= "2018-08-29"
day11 <- anz_data$date < "2018-09-04"
week5 <- day5 & day11
avg_wk5 <- mean(anz_data$amount[week5])
tot_wk5 <- sum(anz_data$amount[week5])

day6 <- anz_data$date >= "2018-09-04"
day12 <- anz_data$date < "2018-09-11"
week6 <- day6 & day12
avg_wk6 <- mean(anz_data$amount[week6])
tot_wk6 <- sum(anz_data$amount[week6])

day7 <- anz_data$date >= "2018-09-11"
day13 <- anz_data$date < "2018-09-18"
week7 <- day7 & day13
avg_wk7 <- mean(anz_data$amount[week7])
tot_wk7 <- sum(anz_data$amount[week7])

day8 <- anz_data$date >= "2018-09-18"
day14 <- anz_data$date < "2018-09-25"
week8 <- day8 & day14
avg_wk8 <- mean(anz_data$amount[week8])
tot_wk8 <- sum(anz_data$amount[week8])

day9 <- anz_data$date >= "2018-09-25"
day15 <- anz_data$date < "2018-10-02"
week9 <- day9 & day15
avg_wk9 <- mean(anz_data$amount[week9])
tot_wk9 <- sum(anz_data$amount[week9])

day10 <- anz_data$date >= "2018-10-02"
day16 <- anz_data$date < "2018-10-09"
week10 <- day10 & day16
avg_wk10 <- mean(anz_data$amount[week10])
tot_wk10 <- sum(anz_data$amount[week10])

day11 <- anz_data$date >= "2018-10-09"
day17 <- anz_data$date < "2018-10-16"
week11 <- day11 & day17
avg_wk11 <- mean(anz_data$amount[week11])
tot_wk12 <- sum(anz_data$amount[week12])

day12 <- anz_data$date >= "2018-10-16"
day18 <- anz_data$date < "2018-10-23"
week12 <- day12 & day18
avg_wk12 <- mean(anz_data$amount[week12])
tot_wk12 <- sum(anz_data$amount[week12])

day13 <- anz_data$date >= "2018-10-23"
day19 <- anz_data$date < "2018-11-01"
week13 <- day13 & day19
avg_wk13 <- mean(anz_data$amount[week13])
tot_wk13 <- sum(anz_data$amount[week13])
tot_wk13

week_avg <- c(avg_wk1, avg_wk2, avg_wk3, avg_wk4, avg_wk5, avg_wk6, avg_wk7,
              avg_wk8, avg_wk9, avg_wk10, avg_wk11, avg_wk12, avg_wk13 )
barplot(week_avg, main="Average transaction amount per week",xlab="Avg transaction amount", ylab="Weeks", 
        horiz=TRUE, col = c("darkblue", "red" ), xlim=c(0,250))

freq_wk1 <- sum(week1)
freq_wk2 <- sum(week2)
freq_wk3 <- sum(week3)
freq_wk4 <- sum(week4)
freq_wk5 <- sum(week5)
freq_wk6 <- sum(week6)
freq_wk7 <- sum(week7)
freq_wk8 <- sum(week8)
freq_wk9 <- sum(week9)
freq_wk10 <- sum(week10)
freq_wk11 <- sum(week11)
freq_wk12 <- sum(week12)
freq_wk13 <- sum(week13)

week_freq <- c(freq_wk1, freq_wk2, freq_wk3, freq_wk4, freq_wk5, freq_wk6, freq_wk7,
               freq_wk8, freq_wk9, freq_wk10, freq_wk11, freq_wk12, freq_wk13 )
barplot(week_freq, main="Transaction frequency per week",xlab="transaction frequency", ylab="Weeks", 
        horiz=TRUE, col = c("blue", "orange" ), xlim=c(0,1200))

# Visualise transaction volume over an average week.
anz_data3 <- anz_data %>%
        select(date,weekday) %>%
        group_by(date,weekday) %>%
        summarise(daily_avg_vol = n()) %>%
        group_by(weekday) %>%
        summarise(avg_vol=mean(daily_avg_vol,na.rm=TRUE ))
anz_data3$weekday <- factor(anz_data3$weekday, levels=c( "Monday","Tuesday","Wednesday",
                                             "Thursday","Friday","Saturday","Sunday"))
ggplot(anz_data3,aes(x=weekday, y=avg_vol)) +geom_point()+geom_line(aes(group = 1))+
        ggtitle('Average transaction volume by weekday') +
        labs(x='Weekday',y='Transaction volume')

# visualize transaction volume over an average day.
anz_data4 <- anz_data %>%
        select(date,hour) %>%
        group_by(date,hour) %>%
        summarize(trans_vol=n()) %>%
        group_by(hour) %>%
        summarize(trans_vol_per_hr = mean(trans_vol,na.rm=TRUE))
ggplot(anz_data4,aes(x=hour,y=trans_vol_per_hr))+geom_point()+geom_line(aes(group = 1))+
        ggtitle('Average transaction volume by hour') +
        labs(x='Hour',y='Transaction volume') + expand_limits( y = 0)


#further classification into debit and credit transactions per week 
d_amount1 <- week1 [debit]
c_amount1 <- week1 [credit]
debit1 <- sum(anz_data$amount[d_amount1])
credit1 <- sum(anz_data$amount[c_amount1])

d_amount2 <- week2 [debit]
c_amount2 <- week2 [credit]
debit2 <- sum(anz_data$amount[d_amount2])
credit2 <- sum(anz_data$amount[c_amount2])

d_amount3 <- week3 [debit]
c_amount3 <- week3 [credit]
debit3 <- sum(anz_data$amount[d_amount3])
credit3 <- sum(anz_data$amount[c_amount3])

d_amount4 <- week4 [debit]
c_amount4 <- week4 [credit]
debit4 <- sum(anz_data$amount[d_amount4])
credit4 <- sum(anz_data$amount[c_amount4])

d_amount5 <- week5 [debit]
c_amount5 <- week5 [credit]
debit5 <- sum(anz_data$amount[d_amount5])
credit5 <- sum(anz_data$amount[c_amount5])

d_amount6 <- week6 [debit]
c_amount6 <- week6 [credit]
debit6 <- sum(anz_data$amount[d_amount6])
credit6 <- sum(anz_data$amount[c_amount6])

d_amount7 <- week7 [debit]
c_amount7 <- week7 [credit]
debit7 <- sum(anz_data$amount[d_amount7])
credit7 <- sum(anz_data$amount[c_amount7])

d_amount8 <- week8 [debit]
c_amount8 <- week8 [credit]
debit8 <- sum(anz_data$amount[d_amount8])
credit8 <- sum(anz_data$amount[c_amount8])

d_amount9 <- week9 [debit]
c_amount9 <- week9 [credit]
debit9 <- sum(anz_data$amount[d_amount9])
credit9 <- sum(anz_data$amount[c_amount9])

d_amount10 <- week10 [debit]
c_amount10 <- week10[credit]
debit10 <- sum(anz_data$amount[d_amount10])
credit10 <- sum(anz_data$amount[c_amount10])

d_amount11 <- week11 [debit]
c_amount11 <- week11 [credit]
debit11 <- sum(anz_data$amount[d_amount11])
credit11 <- sum(anz_data$amount[c_amount11])


d_amount12 <- week12 [debit]
c_amount12 <- week12 [credit]
debit12 <- sum(anz_data$amount[d_amount12])
credit12 <- sum(anz_data$amount[c_amount12])

d_amount13 <- week13 [debit]
c_amount13 <- week13 [credit]
debit13 <- sum(anz_data$amount[d_amount13])
credit13 <- sum(anz_data$amount[c_amount13])


week_credit <- c(credit1, credit2, credit3, credit4, credit5, credit6, credit7,
                 credit8, credit9, credit10, credit11, credit12, credit13 )

week_debit <- c(debit1, debit2, debit3, debit4, debit5, debit6, debit7,
                debit8, debit9, debit10, debit11, debit12, debit13 )

barplot(week_credit, main="Credit transaction per week",xlab="credit amount", ylab="Weeks", 
        horiz=TRUE, col = c("lightblue", "orange" ), xlim=c(0,300000))

barplot(week_debit, main="Debit transaction per week",xlab="debit amount", ylab="Weeks", 
        horiz=TRUE, col = c("lightblue", "orange" ), xlim=c(0,300000))


#1.5 challenge: exploring location information

# segmenting by states
unique(anz_data$merchant_state)

anz_data5 <- anz_data%>%
        select(amount, merchant_state) %>%
        group_by(merchant_state)%>%
        summarise(avg_amount= n()) 


# State average amounts

QLD <- which(anz_data$merchant_state == "QLD")
NSW <- which(anz_data$merchant_state == "NSW")

VIC <- which(anz_data$merchant_state == "VIC")
WA <- which(anz_data$merchant_state == "WA")
SA <- which(anz_data$merchant_state == "SA")
NT <- which(anz_data$merchant_state == "NT")
TAS <- which(anz_data$merchant_state == "TAS")
ACT<- which(anz_data$merchant_state == "ACT")


QLD_amount <- sum(clean_anzdata$amount[QLD])
NSW_amount <- sum(clean_anzdata$amount[NSW])

VIC_amount <- sum(clean_anzdata$amount[VIC])
WA_amount <- sum(clean_anzdata$amount[WA])
SA_amount <- sum(clean_anzdata$amount[SA])
NT_amount <- sum(clean_anzdata$amount[NT])
TAS_amount <- sum(clean_anzdata$amount[TAS])
ACT_amount <- sum(clean_anzdata$amount[ACT])

state_transactions <- c(QLD_amount, NSW_amount, VIC_amount, WA_amount,
                        SA_amount, NT_amount, TAS_amount, ACT_amount)

barplot(state_transactions, main="State Transactions",xlab="Amount", ylab="Weeks", 
        horiz=TRUE, col = c("darkgreen", "orange", "pink", "lightblue", "red",
                            "darkblue", "lightgreen", "purple", "black" ), xlim=c(0,120000)) 
        legend("bottom",
       c("QLD", "NSW", "NAu", "VIC", "WA",
         "SA", "NT", "TAS", "ACT"),
       fill = c("darkgreen", "orange", "pink", "lightblue", "red",
                "darkblue", "lightgreen", "purple", "black" )
)

        

