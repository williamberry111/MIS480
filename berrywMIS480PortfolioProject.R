# File:    berrywMIS480PortfolioProject
# Capstone Project Option 2
# Improving Neonatal Mortality through Data Analytics


# 1.01 - Install Packages and initiate libraries############

install.packages("Rtools")
install.packages("readxl")
install.packages("ggplot2")
install.packages("car")

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(car)


# 1.02 - Read GHED_Data from local directory################
GHE_data <- read_excel("D:/code/GHED_data.xlsx")
# Select relevant GHED columns
GHE_data <- GHE_data[, c("country", "code", "income", "year", "che_gdp", "che_pc_usd", "che")]
# Rename GHED columns to match UNICEF Dataset
GHE_data <- GHE_data %>%
  rename(Year = year,
         ISO.Code = code)

GHE_data <- GHE_data[complete.cases(GHE_data), ]


# 1.03 - Read NMR_data from local directory#################
NMR_data <- read_excel("D:/code/Neonatal_Mortality_Rates_2022.xlsx", 
                       sheet = "NMR Country estimates", 
                       range = "A15:BV615")

# Remove asterisk from Uncertainty.Bounds
names(NMR_data)[names(NMR_data) == "Uncertainty.Bounds*"] <- "Uncertainty.Bounds"


# Remove the ".5" from yearly columns
colnames(NMR_data) <- gsub("\\.5$", "", colnames(NMR_data))

# Unpivot Neonatal Mortality Rate Data
NMR_data <- NMR_data %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  # Selects columns that are 4-digit years
    names_to = "Year",
    values_to = "NMR_Value"
  )

# Filter on complete cases only
NMR_data <- NMR_data[complete.cases(NMR_data), ]





# 1.04 - Read Neonatal Deaths Data##########################
NND_data <- read_excel("D:/code/Neonatal_Deaths_2022.xlsx", 
                       sheet = "Neonatal Country estimates", 
                       range = "A15:BU615")

# Remove asterisk from Uncertainty.Bounds
names(NND_data)[names(NND_data) == "Uncertainty.Bounds*"] <- "Uncertainty.Bounds"

# Remove the ".5" from yearly columns
colnames(NND_data) <- gsub("\\.5$", "", colnames(NND_data))

# Unpivot Neonatal Death Data
NND_data <- NND_data %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),  # Selects columns that are 4-digit years
    names_to = "Year",
    values_to = "NND_Value"
  )

# Filter on complete cases only
NND_data <- NND_data[complete.cases(NND_data), ]


# 1.05 - Join NND and NMR Data##############################
merged_data <- merge(NND_data, NMR_data, 
                     by = c("ISO.Code", "Country.Name", "Year", "Uncertainty.Bounds"),
                     suffixes = c("_NND", "_NMR"))


# 1.06 - Join GHED and Merged UNICEF Data###################
merged_data <- merge(GHE_data, merged_data, by = c("ISO.Code", "Year"), all = FALSE)


# 1.07 - Exclude GHED country column and reorder columns####
merged_data <- merged_data %>% 
  select(Country.Name, ISO.Code, income, Year, Uncertainty.Bounds, everything(), -country)

# Filter only Median data
median_data <- merged_data[merged_data$Uncertainty.Bounds == "Median", ]

# Remove any rows with missing values
median_data <- na.omit(median_data)




# 2 - Exploratory Data Analysis#############################


# 2.01 - Summary statistics#################################
summary(median_data$che); sd(median_data$che)
summary(median_data$che_gdp); sd(median_data$che_gdp)
summary(median_data$che_pc_usd); sd(median_data$che_pc_usd)
summary(median_data$NND_Value); sd(median_data$NND_Value)
summary(median_data$NMR_Value); sd(median_data$NMR_Value)

# 2.02 - Check for normality################################
shapiro.test(median_data$che)
shapiro.test(median_data$che_gdp)
shapiro.test(median_data$che_pc_usd)
shapiro.test(median_data$NND_Value)
shapiro.test(median_data$NMR_Value)

# 2.03 - Histograms#########################################
# GHED Data
# For Current Health Expenditure
hist(median_data$che, main="Distribution of Health Expenditure", xlab="Current Health Expenditure", prob=TRUE)
curve(dnorm(x, mean=mean(median_data$che), sd=sd(median_data$che)), 
      add=TRUE, col="red", lwd=2)

# For Health Expenditure as % of GDP
hist(median_data$che_gdp, main="Distribution of Health Expenditure", 
     xlab="Current Health Expenditure as % Gross Domestic Product (GDP)", prob=TRUE)
curve(dnorm(x, mean=mean(median_data$che_gdp), sd=sd(median_data$che_gdp)), 
      add=TRUE, col="red", lwd=2)

# For Health Expenditure USD per Capita
hist(median_data$che_pc_usd, main="Distribution of Health Expenditure", 
     xlab="Current Health Expenditure USD per Capita", prob=TRUE)
curve(dnorm(x, mean=mean(median_data$che_pc_usd), sd=sd(median_data$che_pc_usd)), 
      add=TRUE, col="red", lwd=2)

# For Neonatal Deaths
hist(median_data$NND_Value, main="Distribution of Neonatal Deaths", 
     xlab="Neonatal Deaths", prob=TRUE)
curve(dnorm(x, mean=mean(median_data$NND_Value), sd=sd(median_data$NND_Value)), 
      add=TRUE, col="red", lwd=2)

# For Neonatal Mortality Rates
hist(median_data$NMR_Value, main="Distribution of Neonatal Mortality Rates", 
     xlab="Neonatal Mortality Rates", prob=TRUE)
curve(dnorm(x, mean=mean(median_data$NMR_Value), sd=sd(median_data$NMR_Value)), 
      add=TRUE, col="red", lwd=2)

# 2.04 - Correlation Analysis###############################
cor(median_data[, sapply(median_data, is.numeric)], method = "spearman")


# 2.05 - Group income into numerical category value#########
median_data <- median_data %>%
  mutate(income_group = case_when(
    income == "High" ~ 3,
    income == "Upper-middle" ~ 2,
    income == "Lower-middle" ~ 1,
    income == "Low" ~ 0,
    TRUE ~ NA_real_  # Assign NA for any unmatched categories
  ))

# 2.06 - Split data by income group########################
# Split the data for the "High" income group
high_income_data <- subset(median_data, income_group == "3")

# Split the data for the "Upper-middle" income group
upper_middle_income_data <- subset(median_data, income_group == "2")

# Split the data for the "Low-middle" income group
lower_middle_income_data <- subset(median_data, income_group == "1")

# Split the data for the "Low" income group
low_income_data <- subset(median_data, income_group == "0")


# 2.07 - NMR Multiple Linear Regression Tests ##############
# NMR Multiple Linear Regression w/o income groups
NMRmodel_wo_income<-lm(median_data$NMR_Value ~  
                         median_data$che_gdp + median_data$che + 
                         median_data$che_pc_usd)
summary(NMRmodel_wo_income)

# NMR Multiple Linear Regression w/ income groups
NMRmodel_w_income<-lm(median_data$NMR_Value ~  median_data$che_gdp 
                      + median_data$che + median_data$che_pc_usd + median_data$income_group)
summary(NMRmodel_w_income)

# NMR Multiple Linear Regression for high income group
NMRmodel_high_income <- lm(NMR_Value ~ che_gdp + che + 
                             che_pc_usd, data = high_income_data)
summary(NMRmodel_high_income)

# NMR Multiple Linear Regression for upper-middle income group
NMRmodel_upper_middle_income <- lm(NMR_Value ~ che_gdp + che 
                                   + che_pc_usd, data = upper_middle_income_data)
summary(NMRmodel_upper_middle_income)

# NMR Multiple Linear Regression for lower-middle income group
NMRmodel_lower_middle_income <- lm(NMR_Value ~ che_gdp + che
                                   + che_pc_usd, data = lower_middle_income_data)
summary(NMRmodel_lower_middle_income)

# NMR Multiple Linear Regression for low income group
NMRmodel_low_income <- lm(NMR_Value ~ che_gdp + che + 
                            che_pc_usd, data = low_income_data)
summary(NMRmodel_low_income)

# 2.08 - NND Multiple Linear Regression Tests ##############
# NND Multiple Linear Regression w/o income groups
NNDmodel_wo_income<-lm(median_data$NND_Value ~
                         median_data$che_gdp + median_data$che + 
                         median_data$che_pc_usd)
summary(NNDmodel_wo_income)

# NND Multiple Linear Regression w/ income groups
NNDmodel_w_income<-lm(median_data$NND_Value ~  
                        median_data$che_gdp + median_data$che + 
                        median_data$che_pc_usd + median_data$income_group)
summary(NNDmodel_w_income)

# NND Multiple Linear Regression for high income group
NNDmodel_high_income <- lm(NND_Value ~ che_gdp + che + 
                             che_pc_usd, data = high_income_data)
summary(NNDmodel_high_income)

# NND Multiple Linear Regression for upper-middle income group
NNDmodel_upper_middle_income <- lm(NND_Value ~ che_gdp + 
                                     che + che_pc_usd, data = upper_middle_income_data)
summary(NNDmodel_upper_middle_income)

# NND Multiple Linear Regression for lower-middle income group
NNDmodel_lower_middle_income <- lm(NND_Value ~ che_gdp + che
                                   + che_pc_usd, data = lower_middle_income_data)
summary(NNDmodel_lower_middle_income)

# NND Multiple Linear Regression for low income group
NNDmodel_low_income <- lm(NND_Value ~ che_gdp + che + 
                            che_pc_usd, data = low_income_data)
summary(NNDmodel_low_income)


# 3.0 avPlot graphs#########################################


avPlots(NNDmodel_wo_income)
avPlots(NNDmodel_w_income)

avPlots(NNDmodel_high_income)
avPlots(NNDmodel_upper_middle_income)
avPlots(NNDmodel_lower_middle_income)
avPlots(NNDmodel_low_income)


avPlots(NMRmodel_wo_income)
avPlots(NMRmodel_w_income)

avPlots(NMRmodel_high_income)
avPlots(NMRmodel_upper_middle_income)
avPlots(NMRmodel_lower_middle_income)
avPlots(NMRmodel_low_income)