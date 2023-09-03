library(tidyverse)
library(mdsr)
library(dplyr)

drug_use_by_age_data <- read.csv("~/R programming class/drug_use_by_age_data.csv")

Drug_use_by_age<-drug_use_by_age_data%>%
  mutate(
    Alcohol_Use=alcohol.use,
    Alcohol_Freq=alcohol.frequency,
    
    Marijuana_Use=marijuana.use,
    Marijuana_Freq=marijuana.frequency,
    
    Cocaine_Use=cocaine.use,
    Cocaine_Freq=as.numeric(cocaine.frequency),
    
    Heroin_Use=heroin.use,
    Heroin_Freq=as.numeric(heroin.frequency),
    
    Pain_Releiver_Use=pain.releiver.use,
    Pain_Releiver_Freq=pain.releiver.frequency,
    
    Oxycontin_Use=oxycontin.use,
    Oxycontin_Freq=as.numeric(oxycontin.frequency),
    
    Age=age
    
  )
  
Average_use_by_age_plot<-function(.data, y_var){
  ggplot(.data, aes(x = Age)) + 
    aes_string(y = y_var)+
    geom_jitter(alpha = 0.2)+
    geom_smooth() +
    labs(
      title = paste( y_var,"by Age ")
    )+
    theme_bw()
   }


c("Alcohol_Use", "Marijuana_Use", "Cocaine_Use", "Heroin_Use", "Pain_Releiver_Use","Oxycontin_Use")%>%
  map(Average_use_by_age_plot, .data = Drug_use_by_age)%>%
  patchwork::wrap_plots(ncol = 3)

######################################################################################

Freq_of__use_by_age_plot<-function(.data, y_var){
  ggplot(.data, aes(x = age)) +
    aes_string(y = y_var)+
    geom_jitter(alpha = 0.2)+
    geom_smooth() +
    scale_x_continuous(name = "Age", limits=c(10, 65))+
    labs(
      title = paste( y_var,"by Age "),
      #caption = "National Survey on Drug Use and Health from the Substance Abuse and Mental Health Data Archive"
  )
}

c("Alcohol_Freq", "Marijuana_Freq", "Cocaine_Freq", "Heroin_Freq", "Pain_Releiver_Freq","Oxycontin_Freq")%>%
  map(Average_use_by_age_plot, .data = Drug_use_by_age)%>%
  patchwork::wrap_plots(ncol = 3)

################################################################################################

CDC.Injury.Center.Drug.Overdose.Deaths <- read.csv("~/R programming class/CDC Injury Center Drug Overdose Deaths.csv", header=FALSE)

Drug_deaths_by_state<-CDC.Injury.Center.Drug.Overdose.Deaths


Drug_deaths_by_state<-Drug_deaths_by_state%>%
  select(V1, V2, V3, V17, V21)%>%
  filter(V1!="State")%>%
  mutate(
  State= V1,
  State_abbr=V2,
  Age_adjusted_death_rate_2019=V3,
  Poverty_rate= V17,
  Urban_pop_percentage= V21
  )%>%
  select( State,  State_abbr, Age_adjusted_death_rate_2019, Poverty_rate, Urban_pop_percentage )%>%
  mutate(Age_adjusted_death_rate_2019 = as.numeric(Age_adjusted_death_rate_2019),
         Poverty_rate = as.numeric(Poverty_rate),
         Urban_pop_percentage = as.numeric(Urban_pop_percentage)
         )

Drug_deaths_by_state

West_Virginia<-Drug_deaths_by_state%>%
  filter(State_abbr== "WV")

Mississippi<-Drug_deaths_by_state%>%
  filter(State_abbr== "MS")

D_C<-Drug_deaths_by_state%>%
  filter(State_abbr== "DC")

library(ggalt)

Death_rate_vs_Poverty_rate_plot<-ggplot(data= Drug_deaths_by_state, aes(y = Age_adjusted_death_rate_2019, x = Poverty_rate))+
  geom_text(aes(label = State_abbr),size=3)+
  scale_x_continuous(name = "Poverty Rate (Percent)", limits=c(7, 21))+
  scale_y_continuous(name = "Deaths per 100,000", limits=c(0, 55))+
  geom_smooth(method="lm", se= FALSE)+
  labs(
    title = "Drug Related Deaths per 100,000 Vs Poverty Rate by State in 2019",
    caption = "CDC Injury Center Drug Overdose Deaths" 
  )+
  theme_bw()+
  geom_encircle(aes(x=Poverty_rate, y=Age_adjusted_death_rate_2019), 
                data=West_Virginia, 
                color="red", 
                size=2)+
  geom_encircle(aes(x=Poverty_rate, y=Age_adjusted_death_rate_2019), 
                data=Mississippi, 
                color="green", 
                size=2)
  

Death_rate_vs_Poverty_rate_plot

cor.test(Drug_deaths_by_state$Poverty_rate, Drug_deaths_by_state$Age_adjusted_death_rate_2019)


Death_rate_vs_Urban_Pop_percentage_plot<-ggplot(data= Drug_deaths_by_state, aes(y = Age_adjusted_death_rate_2019, x = Urban_pop_percentage))+
  geom_text(aes(label = State_abbr),size=3)+
  scale_x_continuous(name = "Urban Population Proportion (Percent)", limits=c(35, 100))+
  scale_y_continuous(name = "Drug Related Deaths per 100,000", limits=c(0, 55))+
  geom_smooth(method="lm", se= FALSE)+
  labs(
    title = "Drug Related Deaths per 100,000 Vs Percentage of Population in Urban Setting",
    caption = "CDC Injury Center Drug Overdose Deaths"
  )+
  theme_bw()+
  geom_encircle(aes(x=Urban_pop_percentage, y=Age_adjusted_death_rate_2019), 
                          data=West_Virginia, 
                          color="red", 
                          size=2)+
  geom_encircle(aes(x=Urban_pop_percentage, y=Age_adjusted_death_rate_2019), 
                data=D_C, 
                color="green", 
                size=2)


Death_rate_vs_Urban_Pop_percentage_plot

cor.test(Drug_deaths_by_state$Urban_pop_percentage, Drug_deaths_by_state$Age_adjusted_death_rate_2019)

##########################################Alcohol and Drug use over time
alcohol_country=read.csv(file=file.choose())
View(alcohol_country)

alcohol_drug_country<-alcohol_country %>%
  rename(alcohol_use_disorder_percent = "Prevalence...Alcohol.use.disorders...Sex..Both...Age..Age.standardized..Percent.",
         drug_use_disorder_percent="Prevalence...Drug.use.disorders...Sex..Both...Age..Age.standardized..Percent.",
         population="Population..historical.estimates."
  ) %>%
  mutate(alcohol_disorder = (population*alcohol_use_disorder_percent),
         drug_disorder = (population*drug_use_disorder_percent))%>%
  filter(Entity == "United States" |Entity ==  "South Korea" |Entity ==  "China" |Entity ==  "United Kingdom") %>%
  select(Entity, Year,alcohol_disorder,drug_disorder)%>%
  filter(Year>1970)
alcohol_drug_country<-na.omit(alcohol_drug_country)

#alcohol for each country
us_alcohol_drug<-alcohol_drug_country%>%
  filter(Entity=="United States")%>%
  na.omit(.data)
alcohol_drug_country

china_alcohol_drug<-alcohol_drug_country%>%
  filter(Entity=="China")%>%
  na.omit(.data)
china_alcohol_drug

#Gdp per capita data for each country
us_gdp_capita=read.csv(file=file.choose())
us_gdp_capita<-us_gdp_capita%>%
  mutate(Year=lubridate::year(DATE),
         Entity = "United States",
         gdp_per_capita=A939RX0Q048SBEA)%>%
  select(Year,Entity,gdp_per_capita)
us_gdp_capita

china_gdp_capita=read.csv(file=file.choose())
china_gdp_capita<-china_gdp_capita%>%
  mutate(Year=lubridate::year(DATE),
         Entity = "China",
         gdp_per_capita=NYGDPPCAPKDCHN)%>%
  select(Year,Entity,gdp_per_capita)

#Combine all the datasets 
full_gdp_data<-rbind(korea_gdp_capita,china_gdp_capita,us_gdp_capita,uk_gdp_capita)
full_gdp_data

full_data<-full_join(full_gdp_data,alcohol_drug_country,c("Year"="Year","Entity"="Entity"))
full_data

#US alcohol and gdp correlation
us_gdp_capita_diff = diff(us_gdp_capita$gdp_per_capita)
us_alcohol_diff = diff(us_alcohol_drug$alcohol_disorder)
cor.test(us_alcohol_diff~us_gdp_capita_diff)
#US drug and gdp correlation
us_drug_diff = diff(us_alcohol_drug$drug_disorder)
cor.test(us_drug_diff~us_gdp_capita_diff)

#China alcohol and gdp correlation
china_gdp_capita_diff = diff(china_gdp_capita$gdp_per_capita)
china_alcohol_diff = diff(china_alcohol_drug$alcohol_disorder)
cor.test(china_alcohol_diff~china_gdp_capita_diff)
#China alcohol and gdp correlation
china_drug_diff = diff(china_alcohol_drug$drug_disorder)
cor.test(china_drug_diff~china_gdp_capita_diff)

g1<-ggplot(full_data,aes(y=alcohol_disorder, x = Year,color=Entity))+geom_point()+geom_line()+facet_wrap(~Entity)+scale_y_continuous()+ 
  labs( 
    y="Alcohol Disorder", 
    x="Year", 
    title="Alcohol Abuse Vs Time", 
    caption = "Source: Our World in Data (University of Oxford Database)")
g1
g2<-ggplot(full_data,aes(y=drug_disorder, x = Year,color=Entity))+geom_point()+geom_line()+facet_wrap(~Entity)+scale_y_continuous()+
  labs(
    y="Drug Disorder", 
    x="Year", 
    title="Drug Disorder vs Time", 
    caption = "Source: Our World in Data (University of Oxford Database)")

g2

 