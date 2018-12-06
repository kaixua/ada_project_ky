library(gdata)###reading in xls
library(WriteXLS)### write xlslibrary(gdata)###reading in xls
library(dplyr)
b1 <- read.xls("7.17.18 COPY.xls")
b1$state <- b1$In.what.US.State..Canadian.Province..or.Foreign.Country.do.you.permanently.reside.
b1$fir_mon <- b1$Have.you.ever.visited.Montana.before.
b1$have <- b1$How.many.nights.has.your.group.already.spent.in.Montana.since.you.most.recently.entered.the.state.
b1$will <- b1$How.many.additional.nights.is.your.group.planning.to.spend.on.this.trip.
for (i in 1:nrow(b1)){
  b1$night[i] <- b1$have[i] + b1$will[i]
}

abbr2state <- function(abbr){
  ab    <- tolower(c("AL",
                     "AK", "AZ", "KS", "UT", "CO", "CT",
                     "DE", "FL", "GA", "HI", "ID", "IL",
                     "IN", "IA", "AR", "KY", "LA", "ME",
                     "MD", "MA", "MI", "MN", "MS", "MO",
                     "MT", "NE", "NV", "NH", "NJ", "NM",
                     "NY", "NC", "ND", "OH", "OK", "OR",
                     "PA", "RI", "SC", "SD", "TN", "TX",
                     "CA", "VT", "VA", "WA", "WV", "WI",
                     "WY", "DC","Australia","British Columbia",
                     "BC","Alb","AB","MB","NB","NL","NT","NS",
                     "NU","ON","PE","QC","SK","YT"))
  st    <- c("Alabama",
             "Alaska", "Arizona", "Kansas",
             "Utah", "Colorado", "Connecticut",
             "Delaware", "Florida", "Georgia",
             "Hawaii", "Idaho", "Illinois",
             "Indiana", "Iowa", "Arkansas",
             "Kentucky", "Louisiana", "Maine",
             "Maryland", "Massachusetts", "Michigan",
             "Minnesota", "Mississippi", "Missouri",
             "Montana", "Nebraska", "Nevada",
             "New Hampshire", "New Jersey", "New Mexico",
             "New York", "North Carolina", "North Dakota",
             "Ohio", "Oklahoma", "Oregon",
             "Pennsylvania", "Rhode Island", "South Carolina",
             "South Dakota", "Tennessee", "Texas",
             "California", "Vermont", "Virginia",
             "Washington", "West Virginia", "Wisconsin",
             "Wyoming", "District of Columbia","Australia",
             "British Columbia, Canada","British Columbia, Canada",
             "Alberta, Canada","Alberta, Canada","Manitoba, Canada",
             "New Brunswick, Canada","Newfoundland and Labrador, Canada",
             "Northwest Territories, Canada","Nova Scotia, Canada",
             "Nunavut, Canada","Ontario, Canada","Prince Edward Island, Canada",
             "Quebec, Canada","Saskatchewan, Canada","Yukon, Canada")
  st[match(tolower(abbr), ab)]
}


for (j in 1:nrow(b1)){
  if (is.na(b1$state[j])){
    b1$statenew[j] <- "NULL"
  } else {
    b1$statenew[j] <- abbr2state(b1$state[j])
  }
}
for (i in 1:nrow(b1)){
  if (is.na(b1$statenew[i])){
    b1$statenew[i] = as.character(b1$In.what.US.State..Canadian.Province..or.Foreign.Country.do.you.permanently.reside.[i])
  }
}

b1$main_purpose <- b1$Of.these.purposes.you.just.mentioned..replied..yes..to...what.is.the.MAIN.purpose.for.you.being.IN.MONTANA.

output <- b1 %>% 
  select(statenew, main_purpose,fir_mon,night) %>% 
  collect
head(output)

state2region <- function(abbr){
  ab    <- tolower(c("Montana","Wyoming","Colorado","Utah","Idaho","Nevada",
                     "California","Washington","Oregon","Alaska","Hawaii",
                     "New Mexico","Arizona","Oklahoma","Texas","Ohio",
                     "Indiana","Illinois","Wisconsin","Michigan","Minnesota",
                     "Iowa","Missouri","Kansas","South Dakota","North Dakota",
                     "Nebraska","Kentucky","Virginia","West Virginia","Tennessee",
                     "North Carolina","South Carolina","Georgia","Florida","Arkansas",
                     "Alabama","Louisiana","Mississippi","Maine","Vermont",
                     "New Hampshire","New York","New Jersey","Connecticut",
                     "Rhode Island","Maryland","Delaware","Pennsylvania",
                     "Massachusetts"))
  st    <- c("West","West","West","West","West","West","West","West",
             "West","West","West","Southwest","Southwest","Southwest",
             "Southwest","Midwest","Midwest","Midwest","Midwest","Midwest",
             "Midwest","Midwest","Midwest","Midwest","Midwest","Midwest",
             "Midwest","Southeast","Southeast","Southeast","Southeast","Southeast",
             "Southeast","Southeast","Southeast","Southeast","Southeast",
             "Southeast","Southeast","Northeast","Northeast","Northeast","Northeast",
             "Northeast","Northeast","Northeast","Northeast","Northeast","Northeast",
             "Northeast")
  st[match(tolower(abbr), ab)]
}
output$region <- state2region(output$statenew)
output$region <- as.character(output$region)
output$main_purpose <- as.character(output$main_purpose)
output$statenew <- as.character(output$statenew)
chi_purpose <- output%>%
  select(region,main_purpose)%>%
  filter(main_purpose != "Shopping" & 
           main_purpose != "Other (medical/funeral/etc.)"&
           !is.na(region)&
           !is.na(main_purpose)&
           main_purpose != "")%>%
  collect

####chisq test
observed <- table(chi_purpose$region,chi_purpose$main_purpose)
chi_table <- data.frame(observed)
knitr::kable(chi_table)
chi <- chisq.test(x=chi_purpose$region,y=chi_purpose$main_purpose)
residuals(chi)
###anova
ano_nights <- output%>%
  select(region,night)%>%
  filter(!is.na(region))%>%
  collect
ano <- table(ano_nights$region,ano_nights$night)
data.frame(ano)
hist(ano_nights$night)
m1 <- aov(night~region,data=ano_nights)
summary(m1)
plot(predict(m1),resid(m1),pch=16)
hist(resid(m1),col="gray")
qqnorm(resid(m1),pch=16)
par(mar=c(5,7,4,1)+.1)
t1 = TukeyHSD(m1,"region")
t1
plot(t1,las = 1)


