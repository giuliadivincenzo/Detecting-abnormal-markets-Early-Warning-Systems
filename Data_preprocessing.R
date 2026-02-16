# Load libraries
library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(GGally)
library(dplyr)
library(readxl)
library(ggplot2)

# File path the Excel file
file_path <- "C:/Users/Riccardo/Desktop/Fintech/Zenti_Business_Case_4/Dataset4_EWS.xlsx"

# Import data and metadata
data_df <- read_excel(file_path, sheet = "Markets")
metadata_df <- read_excel(file_path, sheet = "Metadata")

# Convert 'Data' column to Date format
data_df$Data <- as.Date(data_df$Data)

# General info
n_rows <- nrow(data_df)
n_cols <- ncol(data_df)
start_date <- min(data_df$Data, na.rm = TRUE)
end_date <- max(data_df$Data, na.rm = TRUE)
freq <- "W-TUE"  # Weekly Tuesday, as per your example
n_vars <- n_cols
n_anomalies <- sum(data_df$Y == 1, na.rm = TRUE)
pct_anomalies <- round(100 * n_anomalies / n_rows, 2)

# Print general info
cat("Data columns:", toString(colnames(data_df)), "\n")
cat("Data shape:", paste0("(", n_rows, ", ", n_cols, ")"), "\n")
cat("Total number of records:", n_rows, "\n")
cat("Time period: from", format(start_date, "%m/%d/%Y"), "to", format(end_date, "%m/%d/%Y"), "\n")
cat("Frequency:", freq, "\n")
cat("Number of variables:", n_vars, "\n")
cat("Number of anomalies:", n_anomalies, "(", pct_anomalies, "%)\n\n")

# Compute summary statistics (exclude 'Data' and 'Y')
summary_table <- data_df %>%
  select(-Data, -Y) %>%
  summarise(across(everything(), list(
    Mean = ~ mean(., na.rm = TRUE),
    StdDev = ~ sd(., na.rm = TRUE),
    Min = ~ min(., na.rm = TRUE),
    Max = ~ max(., na.rm = TRUE),
    Missing = ~ sum(is.na(.)),
    MissingPct = ~ round(100 * sum(is.na(.)) / n(), 2)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(), names_to = c("Ticker", ".value"), names_sep = "_")

colnames(metadata_df)

# Merge with metadata for ticker descriptions
summary_table <- summary_table %>%
  left_join(metadata_df, by = c("Ticker" = "Variable name")) %>%
  select(Ticker, Description, Mean, StdDev, Min, Max, Missing, MissingPct)

# Print the summary table
print(summary_table, n = Inf)


# Ensure Data is a Date object
data_df <- data_df %>%
  mutate(Data = as.Date(Data))

# Plot MXUS over time, with anomalies highlighted
ggplot(data_df, aes(x = Data, y = MXUS)) +
  geom_line(color = "steelblue", size = 0.8) +  # Normal line
  geom_point(data = filter(data_df, Y == 1), aes(x = Data, y = MXUS),
             color = "red", size = 2, alpha = 0.7) +  # Highlight anomalies
  labs(
    title = "MSCI USA (MXUS) Over Time with Anomalies Highlighted",
    x = "Date",
    y = "MXUS Value"
  ) +
  theme_minimal()

# Convert Y to factor for coloring
data_df <- data_df %>%
  mutate(Y = as.factor(Y))

# Select a subset of variables for plotting (too many will clutter the plot)
selected_vars <- c("MXUS", "MXEU", "MXJP", "MXCN", "MXIN", "MXBR", "Y")

# Drop rows with missing values in selected variables
plot_df <- data_df %>%
  select(all_of(selected_vars)) %>%
  na.omit()

# Create the scatterplot matrix
ggpairs(
  plot_df,
  columns = 1:(ncol(plot_df) - 1),  # exclude Y from axis but use for color
  aes(color = Y),
  upper = list(continuous = "points"),
  lower = list(continuous = "smooth"),
  diag = list(continuous = "densityDiag")
)








