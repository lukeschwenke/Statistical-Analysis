library(tidycensus)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(boot)
library(leaps)

# Q1 ----------------------------------------------------------------------



# vector of variables that we want to pull
vars <- c(totpop = 'DP05_0001E',   # Total population
          medage ='DP05_0018E',    # Median age (years)
          medhhinc = 'DP03_0062E', # Median household income 
          propbac = 'DP02_0065PE', # Population 25yrs or older w bachelor's degree
          propcov = 'DP03_0096PE', # Civilian non institutionalized population w health insurance coverage
          proppov = 'DP03_0128PE', # PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL All people
          proprent = 'DP04_0047PE') # Occupied housing units Renter-occupied percent

# use our api key
census_api_key("5b6ebd00d2a16dd1154bd4356355cbf7513e7e69")
df <- get_acs(geography = "tract", output = "wide", state = "IL", county = "Cook", 
              year = 2019, survey = "acs5", variables = vars, geometry = TRUE)

# drop margin of error cols
drop <- c('DP05_0001M','DP05_0018M','DP03_0062M','DP02_0065PM','DP03_0096PM','DP03_0128PM','DP04_0047PM')
df <- df[, !(colnames(df) %in% drop)]

# rename cols
df <- df %>% rename("geoid"="GEOID","name"="NAME")

# checking for na rows
df[rowSums(is.na(df)) > 0,]

# remove rows where values are na
nrow(df) # rows before na removal
df = na.omit(df)
nrow(df) # rows after na removal
model <- lm(propbac ~ medhhinc, data = df)
summary(model)


# Q2 ----------------------------------------------------------------------

no_geo <- df %>% select(-geoid, -geometry)
model2 <- lm(propbac ~ totpop + medage + medhhinc + propcov + proppov + proprent, data = df)

summary(model)
summary(model2)

anova(model, model2)

hist(model$residuals)


hist_df <- rbind(data.frame(Residuals = model$residuals, Model = 'Model 1'), 
                 data.frame(Residuals = model2$residuals, Model = 'Model 2'))

ggplot(hist_df, aes(x=Residuals, fill=Model)) +
    geom_histogram(alpha=0.6) +
    ggtitle(label = 'Model Residual Comparison', subtitle = 'Model 1 = meddhinc only | Model 2 = all variables') +
    ylab('Count')

ggplot(hist_df, aes(x=Residuals)) +
  geom_histogram(data=subset(hist_df, Model == 'Model 1'),fill = "green", alpha = 0.35) +
  geom_histogram(data=subset(hist_df, Model == 'Model 2'),fill = "blue", alpha = 0.35) +
  ggtitle(label = 'Model Residual Comparison', subtitle = 'Model 1 = meddhinc only | Model 2 = all variables') +
  ylab('Count') + scale_fill_manual(name = "Legend", 
                      breaks = c("green", "blue"),
                      values = c("Model 1", "Model 2"))

bptest(model)
bptest(model2)

ks.test(model)
ks.test(model2)

summary(model2)




# 6 -----------------------------------------------------------------------
vars <- c(totpop = "DP05_0001E", #total population
          propbac ="DP02_0065PE") # college degree acheievement levels - propbac

df <- get_acs(geography="tract",
              variables = vars, 
              year=2019,
              state = state.abb,output = "wide",
              geometry = TRUE) 

# Drop 2 margin of error variables
df <- df %>% select(c(-DP05_0001M, -DP02_0065PM)) 

# Get clean NON Cook County Data
df_NO_cook_clean <- df %>% filter(!str_detect(NAME, 'Cook County, Illinois')) %>% filter(!is.na(totpop) &
                                                                         !is.na(propbac) &
                                                                         totpop >= 100)
# Get clean Cook County Data
df_cook_clean <- df %>% filter(str_detect(NAME, 'Cook County, Illinois')) %>% filter(!is.na(totpop) &
                                                                      !is.na(propbac) &
                                                                      totpop >= 100)

#6b
#Calculate the national average for tract-level college degree attainment, using both an
#equal-weight average as well as weighting by population. For these calculations, exclude
#Cook County, IL.

equal_average_NO_cook <- mean(df_NO_cook_clean$propbac) #18.77547 - equal-weight average
weighted_average_NO_cook <- weighted.mean(df_NO_cook_clean$propbac, df_NO_cook_clean$totpop) #19.488

#6c
#Perform a hypothesis test of whether the tracts from Cook County could share the same
#equal-weighted average college degree attainment as the national average excluding Cook County.
#Treat that national average as a known constant, not a random variable.

equal_average_cook <- mean(df_cook_clean$collegeach)

t.test(df_cook_clean$totpop, equal_average_NO_cook)

df_cook_clean$national <- equal_average_NO_cook
t.test(x = df_cook_clean$propbac, df_cook_clean$national)


# 7 -----------------------------------------------------------------------

#Identify the tract containing the Gleacher Center and NBC Tower. 
#Note that the 2015-2019 ACS 5-year estimates use the 2010 Census tract boundaries. 
#Restore your regression from the previous week that predicted college degree attainment 
# as a function of all the predictors downloaded in the first group assignment.

vars <-   c(totpop = 'DP05_0001E', # Total population
            medage ='DP05_0018E', # Median age (years)
            medhhinc = 'DP03_0062E', # Median household income 
            propbac = 'DP02_0065PE', # Population 25yrs or older w bachelor's degree - COLLEGE ATTAINMENT
            propcov = 'DP03_0096PE', # Civilian non institutionalized population w health insurance coverage
            proppov = 'DP03_0128PE', # PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL All people
            proprent = 'DP04_0047PE') # Occupied housing units Renter-occupied percent

full_df <- get_acs(geography = "tract", output = "wide", state = "IL", 
                   county = "Cook", year = 2019, survey = "acs5", variables = vars, geometry = TRUE)
# drop margin of error cols
drop <- c('DP05_0001M','DP05_0018M','DP03_0062M','DP02_0065PM','DP03_0096PM','DP03_0128PM','DP04_0047PM')
full_df <- full_df[, !(colnames(full_df) %in% drop)]
# rename cols
full_df <- full_df %>% rename("geoid"="GEOID","name"="NAME")
full_df <- full_df %>% select(-geometry)

#	0814.03 as of 2022, GEO ID 17031081403
uctract <- full_df[full_df$geoid == "17031081403",]
uctract <- uctract[c("totpop","medage","medhhinc","propcov","proppov","proprent")]

# gleacher center and nbc tower lon and lat coords
locations <- data.frame(longitude = c(-87.622240,-87.621150), latitude = c(41.889640,41.890100))
# Trying to plot
ggplot(data = uctract) + 
  geom_sf() +
  geom_point(data = locations, aes(x = longitude, y = latitude), color="red") +
  labs(title = "Tract: 813.03, Geo ID: 17031081403",
       subtitle = "Gleacher Center and NBC Tower",
       caption = "Data: 2015-2019 5-year ACS, US Census Bureau, Cook County, IL",
       fill = "Percentage") + 
  theme_minimal()


gleach <- full_df %>% filter(geoid == '17031081403')
model_gleach <- lm(propbac ~ totpop + medage + medhhinc + propcov + proppov + proprent, data = full_df)

#a) What is the point estimate and 90% confidence interval for the predicted college 
#degree attainment in this tract? Is the true college degree attainment for this tract contained in that interval?

confint(model, parm='propbac', level=0.9)
confint(model_gleach)
vals <- c(full_df$propbac)

predict(model_gleach, interval='prediction', level=.90)

#b) How does this point estimate and interval differ if you re-calculate the regression, weighting by population?

model_pop_weight <- lm(propbac ~ totpop + medage + medhhinc + propcov + proppov + proprent, data=full_df, weights=totpop)
summary(model_pop_weight)

#c) Using all of the betas and their standard errors estimated by your (unweighted) regression, 
#simulate 10,000 sets of possible betas. Use those 10,000 sets of betas to calculate 10,000 predictions for the 
#Gleacher/NBC tract. Compare a 90% interval formed from these simulations to the 90% CI produced in (a) above.

#The estimate will be normally distributed around the unknown (True) parameter. 
#Resample w/ replacement Tracts, create 10,000 datasets. 
#Find the estimated parameter for each of these. 

model

betas <- full_df$
set.seed(55)
for(i in 1:10000) {
  name <- sprintf("dataframe_%d", i)
  samps[samp_col] <- sample(x=df$medhhinc, size=nrow(df), replace=TRUE)
  lm
}

for(i in 1:10000) {
  df_cook_clean
}

x <- modelr::resample(df_cook_clean, 1:10000)
as.data.frame(x)

resample(mtcars,1:10)

b <- resample_bootstrap(mtcars)
as.data.frame(b)

lm(mpg~wt, data=b)



frame <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c("intercept", 
                                                            "totpop", 
                                                            "medage",
                                                            "medhhinc",
                                                            "propcov",
                                                            "proppov",
                                                            "proprent"))
for(i in 1:10000){
save <- coef(lm(propbac ~ totpop + medage + medhhinc + propcov + proppov + proprent, data = resample_bootstrap(full_df)))
frame <- rbind(frame, save)
}

# RENAME COLUMNS
i = 1
for(name in c("intercept", "totpop", "medage", "medhhinc", "propcov"," proppov", "proprent")){
    names(frame)[i] = name
    i=i+1
}

predictions



# 9 -----------------------------------------------------------------------

vars <-   c(proppov = 'DP03_0128PE',  #Dependent Variable - PERCENTAGE OF FAMILIES AND PEOPLE WHOSE INCOME IN THE PAST 12 MONTHS IS BELOW THE POVERTY LEVEL All people
            propbac = 'DP02_0065PE',  # Population 25yrs or older w bachelor's degree - COLLEGE ATTAINMENT
            employ_status = 'DP03_0001PE', # employement status
            home_unit_50 = 'DP04_0081PE', # !Owner-occupied units!!Less than $50,000
            proprent = 'DP04_0047PE', # Occupied housing units Renter-occupied percent
            white = 'DP05_0037PE', # percent white
            work_from_home = 'DP03_0024PE', #works from home
            totpop = 'DP05_0001E', # Total population
            medage ='DP05_0018E', # Median age (years)
            propcov = 'DP03_0096PE', # Civilian non institutionalized population w health insurance coverage
            proprent = 'DP04_0047PE', # proportion that rent and not own 
            labor_force = 'DP03_0002PE') #percent working in labor force
  
#computer = 'DP02PR_0152PE', #home has computer
#language_not_eng = 'DP02PR_0113PE', #language other than english spoken at home
#not_us_citizen = 'DP02PR_0096PE') #non us citizen  
#avg_hh_size = 'DP02_0016PE',  #average household size 
#avg_fam_size = 'DP02_0017PE', #avg family size

pf <- get_acs(geography="tract",
              variables = vars, 
              year=2019,
              state = state.abb,
              output = "wide",
              geometry = TRUE) 

# drop margin of error cols
drop <- c('DP03_0128PM','DP02_0065PM','DP03_0001PM','DP04_0081PM','DP04_0081PM', 'DP04_0047PM', 'DP05_0037PM',
          'DP03_0024PM', 'DP05_0001M', 'DP05_0018M', 'DP02PR_0096PM', 'DP03_0096PM', 'DP03_0002PE', 'DP03_0002PM')
pf <- pf[, !(colnames(pf) %in% drop)]

# Percent that is NA
colMeans(is.na(pf))

# Remove NA data
pf = na.omit(pf)

# Generate baseline model for predicting Poverty
pf_base <- lm(proppov ~ propbac + employ_status + home_unit_50 + proprent + white + 
                        work_from_home + totpop + medage + propcov + labor_force,
                        data=pf)
summary(pf_base) #69.93% R2

reginfo <- leaps::regsubsets(x = pf[,c(-1:-3,-14)], y = c(pf[,3]))
#reginfo <- leaps::regsubsets(x = as.matrix(pf %>% select(-c(GEOID, NAME, proppov, geometry))), y = pf %>% select(proppov))

# FORWARD SELECTION
forward <- step(lm(proppov~1, pf), scope=formula(pf_base), direction='forward')
forward$anova
forward$coefficients

# BACKWARD ELIMINATION
backward <- step(pf_base, direction='backward', scope=formula(pf_base), trace=0)
backward$anova
backward$coefficients

# BOTH
both <- step(lm(proppov~1, pf), direction='both', scope=formula(pf_base), trace=0)
both$anova #AIC = 261,119.8
both$coefficients

backward

best_model <- both$coefficients


