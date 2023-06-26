library(gbm)

# Read the dataset
df <- read.csv("D:\\data ANALYTICS AND SCIENCE\\ASSIGN\\project data\\car\\Car_data.csv")

#### DATA CLEANING ####

# Check for missing values
missing_values <- colSums(is.na(df))
missing_values <- missing_values[missing_values > 0]

# Drop rows with at least two missing values
df_cleaned <- df[rowSums(is.na(df)) < 2, ]

# Impute missing values in 'Market Category' column
df_cleaned$Market.Category <- ifelse(is.na(df_cleaned$Market.Category), "None", df_cleaned$Market.Category)

# Impute missing values in 'Number of Doors' column
df_cleaned$Number.of.Doors[is.na(df_cleaned$Number.of.Doors)] <- 2

# Impute missing values in 'Engine Cylinders' column
df_cleaned$Engine.Cylinders[is.na(df_cleaned$Engine.Cylinders)] <- 0

# Impute missing values in 'Engine HP' column with mean value
mean_engine_hp <- mean(df_cleaned$Engine.HP, na.rm = TRUE)
df_cleaned$Engine.HP[is.na(df_cleaned$Engine.HP)] <- mean_engine_hp

colSums(is.na(df_cleaned))

df_2=df_cleaned
colSums(is.na(df_2))



#gbm

# Convert categorical variables to factors
df_cleaned <- data.frame(lapply(df_cleaned, factor))

# Convert MSRP to numeric
df_cleaned$MSRP <- as.numeric(as.character(df_cleaned$MSRP))

# Split the data into train and test sets
set.seed(123)
train_index <- sample(1:nrow(df_cleaned), 0.8 * nrow(df_cleaned))
train <- df_cleaned[train_index, ]
test <- df_cleaned[-train_index, ]

# Create the formula for the GBM model
formula <- MSRP ~ .

# Train the GBM model
gbm_model <- gbm(formula = formula, data = train, n.trees = 100, interaction.depth = 3, shrinkage = 0.01, distribution = "gaussian")

# Make predictions on the test set
predictions <- predict(gbm_model, newdata = test, n.trees = 100)

# Calculate RMSE
rmse <- sqrt(mean((predictions - test$MSRP)^2))
print(paste("RMSE Score:", rmse))
