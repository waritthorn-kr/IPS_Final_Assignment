# Install Packages
install.packages("readxl")
install.packages("tidyr")
install.packages("dplyr")

# Load packages
library(readxl)
library(tidyr)
library(dplyr)

# Clean and preparing data
biomarkers_data <- read_excel("biomarkers.xlsx")
covariates_data <- read_excel("covariates.xlsx")

# Cleaning and Preparing Data for 'biomarkers.xlsx'

biomarkers_data <- separate(biomarkers_data, Biomarker, into = c("ID", "Time"), sep = "-")
biomarkers_data$Time <- gsub("weeks", "", biomarkers_data$Time)
biomarkers_data$Time <- gsub("months", "", biomarkers_data$Time)
biomarkers_data$Time <- as.numeric(biomarkers_data$Time)
biomarkers_data$ID <- as.numeric(biomarkers_data$ID)
biomarkers_data <- biomarkers_data %>% arrange(ID)
ids_with_incomplete_times <- biomarkers_data %>%
  group_by(ID) %>%
  summarise(Entries = n_distinct(Time)) %>%
  filter(Entries != 3) %>% pull(ID)
biomarkers_data <- biomarkers_data %>% filter(!(ID %in% ids_with_incomplete_times))

#Cleaning and Preparing Data for 'covariates.xlsx'
covariates_data <- covariates_data %>% arrange(PatientID)
covariates_data <- covariates_data %>%  filter(!rowSums(is.na(.)))

# Merging the Two Datasets
completed_data <- inner_join(biomarkers_data, covariates_data, by = c("ID" = "PatientID"))
completed_data <- completed_data %>% rename("Sex" = "Sex (1=male, 2=female)")
completed_data <- completed_data %>% rename("Smoker" = "Smoker (1=yes, 2=no)")
write.csv(completed_data, "completed_data.csv", row.names = FALSE)

# import completed_data
completed_data <- read.csv("completed_data.csv", header = TRUE)

# Test data for normal and draw histogram
time_0_data <- subset(completed_data, Time == 0)
biomarkers <- c("IL.8", "VEGF.A", "OPG", "TGF.beta.1", "IL.6", "CXCL9", "CXCL1", "IL.18", "CSF.1")
p_values <- numeric(length(biomarkers))
for (biomarker in biomarkers) {
  cat("Distribution assessment for", biomarker, "\n")
  data_male <- time_0_data[[biomarker]][time_0_data$Sex == 1]
  data_female <- time_0_data[[biomarker]][time_0_data$Sex == 2]
  hist(data_male, main = paste(biomarker, "Distribution in Males"), xlab = biomarker)
  hist(data_female, main = paste(biomarker, "Distribution in Females"), xlab = biomarker)
  shapiro_male <- shapiro.test(data_male)
  cat("Normality Test for Males - p-value:", shapiro_male$p.value, "\n")
  shapiro_female <- shapiro.test(data_female)
  cat("Normality Test for Females - p-value:", shapiro_female$p.value, "\n\n")
}

# Hypothesis test
time_0_data <- subset(completed_data, Time == 0)
biomarkers <- c("IL.8", "VEGF.A", "OPG", "TGF.beta.1", "IL.6", "CXCL9", "CXCL1", "IL.18", "CSF.1")
p_values <- list()
for (biomarker in biomarkers) {
  shapiro_male <- shapiro.test(time_0_data[[biomarker]][time_0_data$Sex == 1])
  shapiro_female <- shapiro.test(time_0_data[[biomarker]][time_0_data$Sex == 2])
  if (shapiro_male$p.value > 0.05 && shapiro_female$p.value > 0.05) {
    t_test <- t.test(time_0_data[[biomarker]] ~ time_0_data$Sex, var.equal = TRUE)
    p_values[[biomarker]] <- t_test$p.value
  } else {
    mw_test <- wilcox.test(time_0_data[[biomarker]] ~ time_0_data$Sex)
    p_values[[biomarker]] <- mw_test$p.value
  }
}
names(p_values) <- biomarkers
print(p_values)
p_values_adjusted <- p.adjust(unlist(p_values), method = "bonferroni")
print(p_values_adjusted)

# Regression Model
time_0_data <- subset(completed_data, Time == 0)
train_indices <- sample(1:nrow(time_0_data), size = 0.8 * nrow(time_0_data))
train_data <- time_0_data[train_indices, ]
test_data <- time_0_data[-train_indices, ]
model <- lm(Vas.12months ~ IL.8 + VEGF.A + OPG + TGF.beta.1 + IL.6 + CXCL9 + CXCL1 + IL.18 + CSF.1 + Age + as.factor(Sex) + as.factor(Smoker), data = train_data)
summary(model)
predictions <- predict(model, newdata = test_data)
comparison <- data.frame(ID = test_data$ID, Actual = test_data$Vas.12months, Predicted = predictions)
print(comparison)
rmse <- sqrt(mean((test_data$Vas.12months - predictions)^2))
print(rmse)