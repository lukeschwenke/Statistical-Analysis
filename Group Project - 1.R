
library(tidyverse)
library(tidycensus)
library(ggpubr)
library(lmtest)
library(tidymodels)
library(boot)


census_key = '5b6ebd00d2a16dd1154bd4356355cbf7513e7e69'
#census_api_key(census_key, install = TRUE)

#---

vars <- c(totpop='DP05_0001E',   # Total population
          medage='DP05_0018E',   # Median age (years)
          medhhinc='DP03_0062E', # Median household income
          propbac='DP02_0065PE', # Population 25yrs or older w/ bachelor's degree
          propcov='DP03_0096PE', # Civilian non-institutionalized population w/ health insurance coverage
          proppov='DP03_0128PE', # Percent of families and people whose income in the past 12 month is below poverty
          proprent='DP04_0047PE')# Occupied housing units renter-occupied percent

# bring in tract-level data from the 2015-2019 American Community Survey 5-year estimates for Cook County, IL.
# Include the shapefile geometries, and format output as a ‘wide' table
# Note: survey = “acs5” should pull the 5 year data
df <- get_acs(geography = "tract", 
              output = "wide", 
              state = "IL", 
              county = "Cook", 
              year = 2019, 
              survey = "acs5", 
              variables = vars, 
              geometry = TRUE)

# rename columns and remove the margin of error (M) variables
df <- df %>% rename(geoid=GEOID, name=NAME) %>% select(-c('DP05_0001M',  
                                                          'DP05_0018M',
                                                          'DP03_0062M',
                                                          'DP02_0065PM',
                                                          'DP03_0096PM',
                                                          'DP03_0128PM',
                                                          'DP04_0047PM'))


# 4 - Cook Count Bachelor's Degrees Plot --------------------------------------

ggplot() + 
  geom_sf(data = df, mapping = aes(fill = propbac), show.legend = TRUE) +
  scale_fill_gradient2(
    low = "white", 
    mid = "orange", 
    high = "black", 
    midpoint = .02
  ) +
  ggtitle("Cook County Bacehlor's Degrees")


# 5 - Linear Model --------------------------------------------------------

model <- lm(df$propbac ~ df$medhhinc)
summary(model)

ggplot(df, aes(x = medhhinc, y = propbac)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle("Median Household Income vs. Bachelors Degree Rates") +
  xlab("Median Household Income (meddhinc)") + ylab("Bachelors Degree Rates (propbac)") 


# 6 - Normality,  Serial Correlation,  Heteroskedasticity -----------------

resid <- resid(model)

# ********** Normality **********

ggdensity(resid, 
          main = "Density plot of Residuals",
          xlab = "Residuals") # Density plot is not normal, skewed right

# QQ Plot
ggpubr::ggqqplot(resid, 
                 color='black', 
                 title='QQ-Plot: Normal Distribution vs. Linear Model Residuals',
                 xlab='Theoretical Normal Distribution',
                 ylab='Linear Model Residuals',
                 ggtheme = theme_classic()) 
#Indicates the residuals are not normally distributed (line)

# KS Test
ks.test(resid, pnorm, mean=mean(resid), sd=sd(resid)) #Indicates a loose fit of the normal distribution (1.0 is best)

# ********** Serial (Auto) Correlation **********

# Durbin-Watson Test 
dwtest(resid~1) # DW value at 1.8 indicates positive autocorrelation is present
# This means as median income goes down, so will the bachelors rates
# as median income goes up, so will the  bachelors rates

# ********** Heteroskedasticity **********

plot(fitted(model), resid)
abline(0,0)
# As the fitted values increase, errors decrease
# This suggests heteroskedasticity is present (the variances are not consistent)


# 7 - Data Simulation -----------------------------------------------------

# Generate 10,000 samples - Bootstrap
set.seed(111)
boot_income <- sample(x=df$medhhinc, 
                      size=10000,
                      replace=TRUE)

t <- boot::boot(df, df$medhhinc, 10000, sim="ordinary")

lm(boot_income~df$medhhinc)

# Generating sample data
samps <- data.frame(df$propbac) # initialize dataframe with a column of our baccalaureate attainment rates
samps['df.medhhinc'] <- df$medhhinc # add a column with our actual medhhinc data
# Generate 10,000 samples the same length as our data using bootstrap procedure
# Name each sample samp_n and add the column to the dataframe
for(i in 1:10000){
  samp_col <- sprintf('samp_%d', i)
  samps[samp_col] <- sample(x=df$medhhinc, size=nrow(df), replace=TRUE)
}

#Determine what proportion of the 10,000 samples show a stronger link between the
#(simulated) tract-level incomes and the (actual) tract-level baccalaureate attainment rates.

summary(model)
# 53.5% of the variation in bachelors rates could be explained by the median
#household income with our original dataset.

# Question is, what proportion of these 10,000 beat that rate?

options(scipen=999) #remove scientific notation

track <- data.frame(r_squared=NA, slope=NA)

for(i in 3:ncol(samps)){ # iterate starting on samps_1 to the end
y = samps$df.propbac # set constant y to be the bachelors rates
model_out <- lm(y~samps[,i]) # run a linear model for each sample
r_squared <- summary(model_out)$r.squared # get the r_squared for each sample
slope <- coef(model_out)[[2]] # get slope for each sample
track <- rbind(track, c(r_squared, slope)) # save the values
}

# create flag if it meets our original model's r-squared threshold
track$meets_R2_threshold <- ifelse(track$r_squared > summary(model)$r.squared, 1, 0)
track$meets_SLOPE_threshold <- ifelse(track$slope > coef(model)[[2]] , 1, 0)

# Remove first row (NA)
track = track[-1,]

# get percent of our simulated data that beats that threshold
sum(track$meets_R2_threshold) / 10000
sum(track$meets_SLOPE_threshold) / 10000

# add original values to double check
track$original_R2 <- summary(model)$r.squared
track$original_Slope <- coef(model)[[2]]


View(track)


# 8 - Correlation Distributions -------------------------------------------

hist(boot_income)
hist(df$medhhinc)

a <- ggplot(df, aes(x = medhhinc))

a + geom_density() +
  geom_vline(aes(xintercept = median(medhhinc)), 
             linetype = "dashed", size = 0.8)

a + geom_histogram(bins = 35, color = "black", fill = "gray")

plot()

# 10 ----------------------

summary(model)

diff_slope_values <- seq(-5, 5, by=0.007)
intercept <- coef(model)[[1]]

predicted_y <- intercept + tail(diff_slope_values, 1314) * head(df$medhhinc, 1314)

SSE <- sum((fitted(model) - predicted_y)^2)

df$manual_sse <- df$propbac - predicted_y

plot(tail(diff_slope_values,1319), df$manual_sse)

ggplot(df, aes(x = medhhinc, y = propbac)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle("Median Household Income vs. Bachelors Degree Rates") +
  xlab("Median Household Income (meddhinc)") + ylab("Bachelors Degree Rates (propbac)") 


for(i in 1:length(diff_slope_values)){
  slope = diff_slope_values[i]
  fitted_value = intercept + slope * df$medhhinc
  true_y = df$propbac
  SSE = (true_y - fitted_value)
}

plot(diff_slope_values,SSE**2)



# 12 ----------------------------------------------------------------------

names(df)

mini <- df %>% select(propbac, medhhinc) %>% arrange(medhhinc)
mini <- na.omit(mini)

mini$medhhinc[1:50] <- mini$medhhinc[1:50] + 10000
mini$medhhinc[nrow(mini)-50:nrow(mini)] <- mini$medhhinc[nrow(mini)-50:nrow(mini)] - 10000

tax_model <- lm(propbac ~ medhhinc, data=mini)
coef(tax_model)
coef(model)
mini$geometry <- NULL

# sort the income
num = nrow(df)
df_sorted <- df[order(df$medhhinc),]
df_sorted$medhhinc[1:50]<-df_sorted$medhhinc[1:50]+10000
df_sorted$medhhinc[num-50:num]<-df_sorted$medhhinc[num-50:num]-10000

model_tax <- lm(propbac ~ medhhinc, data = df_sorted)
summary(model_tax)
summary(model)

coef(model_tax)
coef(model)




