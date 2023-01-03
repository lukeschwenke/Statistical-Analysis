# Luke Schwenke
# Statistical Analysis Final Project
# December 2022

# Load Packages -----------------------------------------------------------
library(dplyr)
library(janitor)
library(stringr)
library(lubridate)
library(leaps)
library(Metrics)
library(lmtest)
setwd('/Users/lmschwenke/Desktop/Stats Final Project')

# Bring the data into R and adjust  --------------------------------------------

one <- read.csv('2016_brooklyn.csv')
two <- read.csv('2017_brooklyn.csv')
three <- read.csv('2018_brooklyn.csv')
four <- read.csv('2019_brooklyn.csv')
five <- read.csv('2020_brooklyn.csv')

column_names <- c('borough','neighborhood','bldclasscat','taxclasscurr','block',
                  'lot','easement','bldclasscurr','address','aptnum','zip',
                  'resunits','comunits','totunits','landsqft','grosssqft',
                  'yrbuilt','taxclasssale','bldclasssale','price','date') 

# Rename columns
colnames(one) <- column_names
colnames(two) <- column_names
colnames(three) <- column_names
colnames(four) <- column_names
colnames(five) <- column_names

# Add Dataset year column to reference later if needed
one$dataset_year <- '2015'
two$dataset_year <- '2016'
three$dataset_year <- '2017'
four$dataset_year <- '2018'
five$dataset_year <- '2019'


# 1.2 Join the data and make it usable for analysis  --------------------------

# Combine into 1 data frame before cleaning 
df <- rbind(one, two, three, four, five)

# Remove first 4 rows since they are not useful
df <- tail(df, -4)

# Set dataset_year to factor 
df$dataset_year <- as.factor(df$dataset_year)

############################ CLEAN - borough

# Column is not useful - drop
df$borough <- NULL

# Exploratory: Join to another dataset that can provide additional data
# On a particular neighborhood in Queens
# Note: this is not ultimately used in my final model
nyc <- read.csv('nyc_zip_borough_neighborhoods_pop.csv')
nyc$neighborhood <- toupper(nyc$neighborhood)
nyc$neighborhood <- str_replace_all(nyc$neighborhood, " ", "")
nyc <- nyc %>% filter(borough=='Queens')

# Group into higher level neighborhoods found in this dataset
#df$neighborhood_group <- ifelse(neighborhood %in% c('BATHBEACH', 'BAYRIDGE', ), 'SOUTHWESTQUEENS',
#                          ifelse())

nyc <- nyc %>% dplyr::select(neighborhood, population, density)
df <- left_join(df, nyc, by='neighborhood')

############################ CLEAN - population (exploratory)
df$population[is.na(df$population)] <- (df %>% dplyr::select(population) %>% 
                                              filter(!is.na(df$population)) %>% 
                                              summarise(med = median(population)))$med

############################ CLEAN - density (exploratory)
df$density[is.na(df$density)] <- (df %>% dplyr::select(density) %>% 
                                          filter(!is.na(df$density)) %>% 
                                          summarise(den = median(density)))$den

############################ CLEAN - neighborhood

# Set blank neighbordhoods to Other
df$neighborhood[df$neighborhood==""] <- "Other"

# Trim Whitespace
df$neighborhood <- trimws(df$neighborhood, which=c('both'))

# Convert to Factor
df$neighborhood <- as.factor(df$neighborhood)

# Removing 5 rows that don't make much sense
df <- df %>% filter(neighborhood != "NEIGHBORHOOD")

############################ CLEAN - bldclasscat
df$bldclasscat <- trimws(df$bldclasscat, which=c('both'))

# Create new column which is the cat number
df$bldclasscat_number <- substr(df$bldclasscat, 1, 2)

# Convert to Factor
df$bldclasscat <- as.factor(df$bldclasscat)

# Convert new variable to Factor
df$bldclasscat_number <- as.factor(df$bldclasscat_number)

############################ CLEAN - taxclasscurr
df$taxclasscurr <- trimws(df$taxclasscurr, which=c('both'))

# Impute blanks with the most common value of 1 (string)
df$taxclasscurr[df$taxclasscurr==""] <- "1"

# Convert to Factor
df$taxclasscurr <- as.factor(df$taxclasscurr)

############################ CLEAN - block 

# Convert to numeric and check range
df$block <- as.numeric(df$block)

# Convert to Factor
df$block <- as.factor(df$block)

############################ CLEAN - lot

# Convert to Factor
df$lot <- as.factor(df$lot)

############################ CLEAN - easement

unique(df$easement) # no valuable data, drop column

df$easement <- NULL

############################ CLEAN - bldclasscurr

df$bldclasscurr <- trimws(df$bldclasscurr, which=c('both'))

# Impute blank with most common
#df <- df %>% filter(bldclasscurr != "")
df$bldclasscurr[df$bldclasscurr==""]<-"R4"
df$bldclasscurr <- as.factor(df$bldclasscurr)

############################ CLEAN - address

df$address <- trimws(df$address, which=c('both'))

df$address <- as.factor(df$address)

# DROP address, will not provide any value
df$address <- NULL

############################ CLEAN - aptnum

df$aptnum <- trimws(df$aptnum, which=c('both'))
df$aptnum <- str_replace_all(df$aptnum, " ", "")

df$aptnum <- as.factor(df$aptnum)

############################ CLEAN - zip

df$zip <- str_replace_all(df$zip, " ", "")

df <- df %>% filter(zip != "")
df$zip[df$zip=="0"] <- "11235" # Impute 0 with most common zip code

df$zip <- as.factor(df$zip) # zip codes are categories (not purely numeric)

############################ CLEAN - resunits

df$resunits <- str_replace_all(df$resunits, " ", "")
df$resunits <- str_replace_all(df$resunits, ",", "")

# Impute '-' and blank with the most common, 1
df$resunits[df$resunits=="" | df$resunits =='-'] <-  "1"

# Ensure type is numeric
df$resunits <- as.numeric(df$resunits)

############################ CLEAN - comunits

df$comunits <- str_replace_all(df$comunits, " ", "")
df$comunits <- str_replace_all(df$comunits, ",", "")

# Impute blank or '-' commercial units with 0
df$comunits[df$comunits=="" | df$comunits =='-'] <-  "0"

# Ensure type is numeric
df$comunits <- as.numeric(df$comunits)

############################ CLEAN - totunits

df$totunits <- str_replace_all(df$totunits, " ", "")
df$totunits <- str_replace_all(df$totunits, ",", "")

# Impute blank or '-' total units with 1, the most common
df$totunits[df$totunits=="" | df$totunits =='-'] <-  "1"

# Ensure type is numeric
df$totunits <- as.numeric(df$totunits)

############################ CLEAN - landsqft

df$landsqft <- str_replace_all(df$landsqft, " ", "")
df$landsqft <- str_replace_all(df$landsqft, ",", "")

# Impute blank or '-' sqft with 0
df$landsqft[df$landsqft=="" | df$landsqft =='-'] <-  as.character(median(as.numeric(df$landsqft)))

# Convert to numeric
df$landsqft <- as.numeric(df$landsqft)

############################  CLEAN - grosssqft

df$grosssqft <- str_replace_all(df$grosssqft, " ", "")
df$grosssqft <- str_replace_all(df$grosssqft, ",", "")

# Impute blank or '-' sqft with 0
df$grosssqft[df$grosssqft=="" | df$grosssqft =='-'] <- as.character(median(as.numeric(df$grosssqft)))

# Convert to numeric
df$grosssqft <- as.numeric(df$grosssqft)

############################ CLEAN - yrbuilt

df$yrbuilt <- str_replace_all(df$yrbuilt, " ", "")
df$yrbuilt <- str_replace_all(df$yrbuilt, ",", "")

# Impute 0 or blank with most common years built
df$yrbuilt[df$yrbuilt==""] <-  "1920" #1 most common year
df$yrbuilt[df$yrbuilt=="0"] <- "1930" #2 most common year

#df <- df %>% filter(yrbuilt != "0" & yrbuilt != "")

df$yrbuilt <- as.numeric(df$yrbuilt)

############################ CLEAN - taxclasssale

df$taxclasssale <- str_replace_all(df$taxclasssale, " ", "")

df$taxclasssale <- as.factor(df$taxclasssale)

############################ CLEAN - bldclasssale

df$bldclasssale <- str_replace_all(df$bldclasssale, " ", "")

df$bldclasssale <- as.factor(df$bldclasssale)

############################ CLEAN - date

df$date <- str_replace_all(df$date, " ", "")

df$date <- lubridate::mdy(df$date)

############################ CLEAN - price

df$price <- str_replace_all(df$price, " ", "")
df$price <- str_replace_all(df$price, ",", "")

df$price <- gsub("[^0-9]","", df$price) # remove non-numeric characters

# Convert to numeric
df$price <- as.numeric(df$price)

# Impute NA price with median
#df$price[is.na(df$price)] <- median(df$price[!is.na(df$price)])

df <- df %>% filter(price>0) # Note: This drops a fair amount of records, but a
# price of 0 does not make sense in the context of our dataset

#Histogram plot, check price distribution
#hist(df$price[df$price<5000000], breaks=100)

janitor::tabyl(df$price)

#########################################################################
# 1.3 Filter the data and make transformations specific to this analysis
#########################################################################

df <- df %>% filter(substr(bldclasssale, 1, 1)=='A' | 
                    substr(bldclasssale, 1, 1)=='R') 

# 1 unit
df <- df %>% filter(resunits == 1 & totunits == 1) 

# sqft > 0
df <- df %>% filter(grosssqft > 0)

# price non missing
df <- df %>% filter(!is.na(price) | price != "")

# TOTAL RECORDS = 14,165

#######################################################################
######################### Feature Engineering #########################
#######################################################################

# 1800, 1900, 2000 century identifier
df$century <- as.factor(ifelse(as.numeric(df$yrbuilt) < 1900, 1,
                          ifelse(as.numeric(df$yrbuilt) < 2000, 2, 3)))

# Pre-Covid 19 Flag
df$flag_pre_covid <- ifelse(df$date < '2020-08-01', 1, 0)

# Proportion of the Gross SQFT that is Land
df$land_to_gross_ratio <- (df$landsqft / df$grosssqft)

# Records in each zip code
zip_group <- df %>% group_by(zip) %>% summarise(records_per_zip = n())
df <- left_join(df, zip_group, by='zip')

# Records in each neighborhood
neighborhood_group <- df %>% group_by(neighborhood) %>% summarise(records_per_neighborhood = n())
df <- left_join(df, neighborhood_group , by='neighborhood')

# Difference between Gross and Land SQFT
df$sqft_diff <- abs(df$grosssqft - df$landsqft)

# Sale Year
df$sale_year <- as.factor(lubridate::year(df$date))

# Sale Month
df$sale_month <- as.factor(lubridate::month(df$date))

# Sale Quarter (Season)
df$sale_quarter <- as.factor(lubridate::quarter(df$date))

# Mansion variable- MANSION TYPE OR TOWN HOUSE according to documentation
df$mansion <- ifelse(df$bldclasssale == "A7", 1, 0)

# Decade
df$decade <- as.factor(sub(".$", "0", df$yrbuilt))

# Expensive Neighborhood Flags --------------------
#1 NAVYYARD               2110500
#2 REDHOOK                1697500
#3 DOWNTOWNFULTONFERRY    1674000
#4 BOERUMHILL             1602500
#5 PROSPECTHEIGHTS        1535229

df$navyyard_flag <- ifelse(df$neighborhood=='NAVYYARD',1,0)
df$redhook_flag <- ifelse(df$neighborhood=='REDHOOK ',1,0)
df$downtonfultonferry_flag <- ifelse(df$neighborhood=='DOWNTOWNFULTONFERRY',1,0)
df$boerumhill_flag <- ifelse(df$neighborhood=='BOERUMHILL',1,0)
df$prospectheights_flag <- ifelse(df$neighborhood=='PROSPECTHEIGHTS',1,0)

# Cheaper Neighborhood Flags ---------------------
#1 EAST NEW YORK   380000
#2 BROWNSVILLE     390200
#3 SPRING CREEK    419666
df$eastnewyork_flag <- ifelse(df$neighborhood=='EASTNEWYORK',1,0)
df$brownsville_flag <- ifelse(df$neighborhood=='BROWNSVILLE',1,0)
df$springcreek_flag <- ifelse(df$neighborhood=='SPRINGCREEK',1,0)

# Expensive Block Flags
df$block_5459_flag <- ifelse(df$block==5459,1,0)
df$block_2392_flag <- ifelse(df$block==2392,1,0)
df$block_1096_flag <- ifelse(df$block==1096,1,0)

# Cheaper Block Flags
df$block_3451_flag <- ifelse(df$block==3451,1,0)
df$block_5661_flag <- ifelse(df$block==5661,1,0)
df$block_1641_flag <- ifelse(df$block==1641,1,0)

# Expensive Zip Codes
#df %>% dplyr::select(zip, price) %>% group_by(zip) %>% summarize(med=median(price)) %>% arrange(desc(med))
#1 11231 1391273 
#2 11217 1348590.
#3 11215 1325000 
#4 11201 1300000 
#5 11225 1240000 
df$zip_11231_flag <- ifelse(df$zip==11231,1,0)
df$zip_11217_flag <- ifelse(df$zip==11217,1,0)
df$zip_11215_flag <- ifelse(df$zip==11215,1,0)
df$zip_11201_flag <- ifelse(df$zip==11201,1,0)
df$zip_11225_flag <- ifelse(df$zip==11225,1,0)

# Apt Number Floor
#df$floor <- gsub("[^0-9.-]", "", as.character(df$aptnum))

# District - further grouping queens with specific neighborhoods since neighborhoods
# alone has too many degrees of freedom
df$neighborhood <- gsub("[^A-Z]","", df$neighborhood)
df$subdistrict <- df$neighborhood
df$subdistrict[which(df$subdistrict %in% c("GREENPOINT","WILLIAMSBURGEAST","WILLIAMSBURGNORTH","WILLIAMSBURGSOUTH","WILLIAMSBURGCENTRAL"))] <- "D1"
df$subdistrict[which(df$subdistrict %in% c("BOERUMHILL","BROOKLYNHEIGHTS","CLINTONHILL","DOWNTOWN-FULTONMALL","FORTGREENE","NAVYYARD","DOWNTOWNFULTONFERRY","DOWNTOWNMETROTECH","DOWNTOWNFULTONMALL"))] <- "D2"
df$subdistrict[which(df$subdistrict %in% c("BEDFORDSTUYVESANT"))] <- "D3"
df$subdistrict[which(df$subdistrict %in% c("BUSHWICK","WYCKOFFHEIGHTS"))] <- "D4"
df$subdistrict[which(df$subdistrict %in% c("CYPRESSHILLS","EASTNEWYORK","SPRINGCREEK"))]   <- "D5"
df$subdistrict[which(df$subdistrict %in% c("CARROLLGARDENS","GOWANUS","REDHOOK","COBBLEHILL","COBBLEHILLWEST","PARKSLOPE","PARKSLOPESOUTH"))] <- "D6"
df$subdistrict[which(df$subdistrict %in% c("SUNSETPARK","BUSHTERMINAL","WINDSORTERRACE"))] <- "D7"
df$subdistrict[which(df$subdistrict %in% c("PROSPECTHEIGHTS","CROWNHEIGHTS"))] <- "D8"
df$subdistrict[which(df$subdistrict %in% c("FLATBUSHLEFFERTSGARDEN"))] <- "D9"
df$subdistrict[which(df$subdistrict %in% c("BAYRIDGE","DYKERHEIGHTS"))] <- "D10"
df$subdistrict[which(df$subdistrict %in% c("BATHBEACH","BENSONHURST"))] <- "D11"
df$subdistrict[which(df$subdistrict %in% c("BOROUGHPARK","KENSINGTON","OCEANPARKWAYNORTH","OCEANPARKWAYSOUTH"))] <- "D12"
df$subdistrict[which(df$subdistrict %in% c("BRIGHTONBEACH","CONEYISLAND","SEAGATE"))] <- "D13"
df$subdistrict[which(df$subdistrict %in% c("MIDWOOD","FLATBUSHCENTRAL","FLATBUSHNORTH"))] <- "D14"
df$subdistrict[which(df$subdistrict %in% c("GRAVESEND","MADISON","MANHATTANBEACH","SHEEPSHEADBAY","GERRITSENBEACH"))] <- "D15"
df$subdistrict[which(df$subdistrict %in% c("BROWNSVILLE","OCEANHILL"))] <- "D16"
df$subdistrict[which(df$subdistrict %in% c("FLATBUSHEAST"))] <- "D17"
df$subdistrict[which(df$subdistrict %in% c("BERGENBEACH","CANARSIE","FLATLANDS","MARINEPARK","MILLBASIN","OLDMILLBASIN"))]   <- "D18"
df$subdistrict <- as.factor(df$subdistrict)

# Records in each subdistrict
subdistrict_group <- df %>% group_by(subdistrict) %>% summarise(records_per_subdistrict = n())
df <- left_join(df, subdistrict_group, by='subdistrict')

# Age of the condo/unit/home
df$age <- as.numeric(as.character(df$sale_y)) - as.numeric(as.character(df$yrbuilt))

# Binned age factor
df$agefactor <- as.factor(ifelse(as.numeric(df$age) < 30, 1,
                                         ifelse(as.numeric(df$age) < 60, 2, 
                                                ifelse(as.numeric(df$age) < 90, 3,
                                                       ifelse(as.numeric(df$age) < 120, 4, 5)))))

# Save a copy of the dataframe is it is now before aplying filters
prefil <- df

# ADDITIONAL FILTERS AND FEATURES ---------------------------------------------

# OUTLIER FILTERING

# Price less than $5.0 mil and greater than $250
df <- df %>% filter(price < 5000000)
df <- df %>% filter(price > 250)
#df$high_price_flag <- ifelse(df$price>3000000, 1, 0)

# Yrbuilt 1900 or more recent
#df <- df %>% filter(as.numeric(yrbuilt)>=1899)
df$old_house_flag <- ifelse(as.numeric(df$yrbuilt)>=1899, 0, 1)

# SET YRBUILT TO FACTOR
#df$yrbuilt <- as.factor(df$yrbuilt)

# Expensive Subdistrict Flags
#df$D9_subdistrict_flag <- ifelse(df$subdistrict=='D9')

# Expensive Block Flags
df$block_5459_flag <- ifelse(df$block==5459,1,0)
df$block_2392_flag <- ifelse(df$block==2392,1,0)
df$block_1096_flag <- ifelse(df$block==1096,1,0)

# Cheaper Block Flags
df$block_3451_flag <- ifelse(df$block==3451,1,0)
df$block_5661_flag <- ifelse(df$block==5661,1,0)
df$block_1641_flag <- ifelse(df$block==1641,1,0)

#######################################################################
######################### Linear Modeling #########################
#######################################################################

# ------------------- FINAL MODEL -------------------
# OBSERVATIONS USED: 13,551
# ADJUSTED R-SQUARED: 63.62%
# RMSE = $405,828.60
# DEGREES OF FREEDOM = 39

#df$q3_2020 <- ifelse(df$year_quarter=='2020_3',1,0) 
#df$q4_2020 <- ifelse(df$year_quarter=='2020_4',1,0) 

final <- lm(price ~ taxclasscurr +  
                    sqrt(landsqft) +
                    sqrt(grosssqft) +
                    date + 
                    land_to_gross_ratio +
                    records_per_zip +
                    records_per_neighborhood +
                    sqrt(sqft_diff) +
                    subdistrict + 
                    old_house_flag +
                    zip_11231_flag*grosssqft +
                    zip_11217_flag*grosssqft +
                    zip_11215_flag*grosssqft +
                    zip_11201_flag*grosssqft +
                    #q3_2020 + # Used only in Part 2 
                    #q4_2020 + # Used only in Part 2
                    age, 
                    data = df)

summary(final)
rmse(df$price, predict(final, df))

# Save file to submit to Canvas for Part 1 of Final
saveRDS(list(model=final, data=df), file='lukeschwenke.RDS') 


################################################################################
############### SCRATCH CODE ###################################################
################################################################################

# plot(final)

#df %>% group_by(subdistrict) %>% summarise(med = median(price)) %>% arrange(desc(med))

# # NOTE: TRIAL RUNS / EXPLORATION IS COMMENTED OUT
# 
# # Adj. R-squared = 53%
# m1 <- lm(price ~ grosssqft + landsqft + taxclasscurr + taxclasssale + date + 
#            century + flag_pre_covid + land_to_gross_ratio + records_per_zip +
#            population + density + sale_quarter + sale_year + sale_month, data = df)
# summary(m1)
# 
# # Adj. R-squared = 5%
# m2 <- lm(log(price) ~ grosssqft + landsqft + taxclasscurr +
#            yrbuilt + taxclasssale + date + century + flag_pre_covid +
#            land_to_gross_ratio + records_per_zip, data = df)
# summary(m2)
# 
# # Adj. R-squared = Max is 66.66%
# m3 <- leaps::regsubsets(price ~ grosssqft + landsqft + taxclasscurr +
#                           yrbuilt + taxclasssale + date + century + flag_pre_covid +
#                           land_to_gross_ratio + records_per_zip + neighborhood + bldclasscat +
#                           records_per_neighborhood + sqft_diff + density + population, 
#                         data=df, nvmax = 40, method = 'seqrep')
# summary(m3)$adjr2
# summary(m3)$rss
# coef(m3, 40)
# sqrt((summary(m3)$rss[41])/nrow(df)) #515,192.2
# 
# # Adj. R-squared = 40%
# m4 <- lm(price ~ log(grosssqft+1) + log(landsqft+1) + taxclasscurr +
#            yrbuilt + taxclasssale + date + century + flag_pre_covid +
#            land_to_gross_ratio + records_per_zip, data = df)
# summary(m4)
# 
# # Adj. R-squared = 63.8%
# m5 <- leaps::regsubsets(price ~ log(grosssqft+1) + log(landsqft+1) + taxclasscurr +
#                           yrbuilt + taxclasssale + date + century + flag_pre_covid +
#                           land_to_gross_ratio + records_per_zip + neighborhood + block +
#                           bldclasscat + zip, data=df, nvmax = 40, method = 'seqrep')
# summary(m5)$adjr2
# coef(m5, 41)
# sqrt((summary(m5)$rss[41])/nrow(df)) #536,605.1
# 
# # Adj. R-squared = 60%
# m6 <- lm(price~ grosssqft + landsqft + taxclasscurr +
#            yrbuilt + taxclasssale + date + century + flag_pre_covid +
#            land_to_gross_ratio + records_per_zip + neighborhood + bldclasscat +
#            records_per_neighborhood + sqft_diff, data=df)
# summary(m6)
# 
# # PCA
# m7 <- prcomp(~ landsqft+grosssqft, data = df)
# summary(m7)
# 
# # Adj. R2 = 39%
# m4 <- leaps::regsubsets(price ~ grosssqft + landsqft + taxclasscurr +
#                           yrbuilt + taxclasssale + date + century + flag_pre_covid +
#                           land_to_gross_ratio + records_per_zip, data = df)#, nvmax = 5)
# summary(m4)$adjr2
# # Land area may be 
# 
# # Forward - 64.5%
# b1 <- leaps::regsubsets(price ~ neighborhood + bldclasscat + taxclasscurr + bldclasscurr +
#                           zip + landsqft + grosssqft + yrbuilt + taxclasssale + bldclasssale +
#                           date + bldclasscat_number + century + 
#                           flag_pre_covid + land_to_gross_ratio + records_per_zip + 
#                           records_per_neighborhood + sqft_diff + sale_year + block + lot +
#                           sale_month + sale_quarter, data=df, nvmax = 40, method = 'forward')
# summary(b1)$adjr2
# coef(b1, 40)
# sqrt((summary(b1)$rss[40])/nrow(df))
# save_coef_1 <- coef(b1, 41)
# 
# # Check fuller model with Regsubsets
# b2 <- leaps::regsubsets(price ~ neighborhood + 
#                           bldclasscat + 
#                           bldclasssale +
#                           bldclasscurr +
#                           taxclasscurr + 
#                           taxclasssale +
#                           zip + 
#                           landsqft + 
#                           grosssqft +
#                           yrbuilt +  
#                           date + 
#                           population +
#                           density + 
#                           bldclasscat_number + 
#                           century + 
#                           flag_pre_covid + 
#                           land_to_gross_ratio + 
#                           records_per_zip + 
#                           records_per_neighborhood + 
#                           sqft_diff + 
#                           sale_year +
#                           sale_month + 
#                           sale_quarter + 
#                           subdistrict +
#                           age +
#                           agefactor +
#                           block + 
#                           lot, 
#                         data=df, nvmax = 40, method = 'subsetq')
# 
# summary(b2)$adjr2
# coef(b2, 41)
# sqrt((summary(b2)$rss[41])/nrow(df))
# 
# save_coef_2<- coef(b2, 41)
