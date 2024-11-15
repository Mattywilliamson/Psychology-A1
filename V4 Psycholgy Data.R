getwd()

# Load required libraries
library(dplyr)

# Create the data frame
cognitive_state_anxiety <- c(9,24,10,25,23,17,19,19,27,32,30,28,29,18,18,17,18,30,20,36,14,17,33,24,25,27,25,18,17,17,26,10,19,33,19,29,17,31,20)
somatic_state_anxiety <- c(11,23,27,20,28,17,20,22,27,23,19,26,26,15,11,14,25,24,19,33,15,21,26,19,24,12,22,11,15,16,26,12,17,28,27,26,14,22,15)
self_confidence <- c(33,17,19,29,28,31,22,23,15,13,20,23,12,30,30,12,16,20,26,14,27,20,20,21,27,19,17,25,29,32,27,23,24,22,28,21,17,10,26)
scat <- c(10,27,22,13,22,27,15,24,30,30,26,20,30,30,16,20,23,14,15,28,17,23,24,23,22,20,24,16,17,18,21,10,23,14,17,22,20,28,23)
gender <- factor(c("Male","Male","Male","Male","Female","Male","Male","Male","Male","Female","Male","Female","Female","Male","Male","Male","Female","Male","Male","Male","Male","Male","Female","Female","Female","Female","Male","Female","Male","Male","Female","Female","Female","Male","Female","Female","Female","Female","Female"))
sport <- factor(c("Team","Team","Team","Individual","Team","Individual","Individual","Team","Team","Team","Team","Individual","Individual","Team","Individual","Team","Team","Team","Team","Individual","Team","Team","Team","Team","Team","Individual","Individual","Individual","Individual","Individual","Team","Individual","Individual","Individual","Team","Individual","Individual","Individual","Individual"))
age <- c(22,20,21,26,20,21,22,21,21,20,21,20,22,23,22,20,21,20,20,20,42,20,22,23,22,20,22,22,20,20,21,20,21,21,20,20,20,22,22)

# Combine into a data frame
data <- data.frame(
  CognitiveStateAnxiety = cognitive_state_anxiety,
  SomaticStateAnxiety = somatic_state_anxiety,
  SelfConfidence = self_confidence,
  SCAT = scat,
  Gender = gender,
  Sport = sport,
  Age = age
)

# Calculate descriptive statistics
desc_stats_overall <- data.frame(
  Measure = c("CognitiveStateAnxiety", "SomaticStateAnxiety", "SelfConfidence", "SCAT"),
  Mean = c(mean(data$CognitiveStateAnxiety), 
           mean(data$SomaticStateAnxiety),
           mean(data$SelfConfidence),
           mean(data$SCAT)),
  SD = c(sd(data$CognitiveStateAnxiety),
         sd(data$SomaticStateAnxiety),
         sd(data$SelfConfidence),
         sd(data$SCAT)),
  N = rep(nrow(data), 4)
)

# Round the numeric columns to 2 decimal places
desc_stats_overall$Mean <- round(desc_stats_overall$Mean, 2)
desc_stats_overall$SD <- round(desc_stats_overall$SD, 2)

# Calculate correlation matrix
cor_matrix <- cor(data[,c("CognitiveStateAnxiety", "SomaticStateAnxiety", "SelfConfidence", "SCAT")])

# Print results
print("Descriptive Statistics Overall:")
print(desc_stats_overall)

print("\nCorrelation Matrix:")
print(round(cor_matrix, 2))

# Save the files (adjust the path as needed in RStudio)
write.csv(desc_stats_overall, "descriptive_stats.csv", row.names = FALSE)
write.csv(round(cor_matrix, 2), "correlation_matrix.csv", row.names = TRUE)

# Load required libraries
library(dplyr)
library(writexl)

# Create the data frame
cognitive_state_anxiety <- c(9,24,10,25,23,17,19,19,27,32,30,28,29,18,18,17,18,30,20,36,14,17,33,24,25,27,25,18,17,17,26,10,19,33,19,29,17,31,20)
somatic_state_anxiety <- c(11,23,27,20,28,17,20,22,27,23,19,26,26,15,11,14,25,24,19,33,15,21,26,19,24,12,22,11,15,16,26,12,17,28,27,26,14,22,15)
self_confidence <- c(33,17,19,29,28,31,22,23,15,13,20,23,12,30,30,12,16,20,26,14,27,20,20,21,27,19,17,25,29,32,27,23,24,22,28,21,17,10,26)
scat <- c(10,27,22,13,22,27,15,24,30,30,26,20,30,30,16,20,23,14,15,28,17,23,24,23,22,20,24,16,17,18,21,10,23,14,17,22,20,28,23)
gender <- factor(c("Male","Male","Male","Male","Female","Male","Male","Male","Male","Female","Male","Female","Female","Male","Male","Male","Female","Male","Male","Male","Male","Male","Female","Female","Female","Female","Male","Female","Male","Male","Female","Female","Female","Male","Female","Female","Female","Female","Female"))
sport <- factor(c("Team","Team","Team","Individual","Team","Individual","Individual","Team","Team","Team","Team","Individual","Individual","Team","Individual","Team","Team","Team","Team","Individual","Team","Team","Team","Team","Team","Individual","Individual","Individual","Individual","Individual","Team","Individual","Individual","Individual","Team","Individual","Individual","Individual","Individual"))
age <- c(22,20,21,26,20,21,22,21,21,20,21,20,22,23,22,20,21,20,20,20,42,20,22,23,22,20,22,22,20,20,21,20,21,21,20,20,20,22,22)

# Combine into a data frame
data <- data.frame(
  CognitiveStateAnxiety = cognitive_state_anxiety,
  SomaticStateAnxiety = somatic_state_anxiety,
  SelfConfidence = self_confidence,
  SCAT = scat,
  Gender = gender,
  Sport = sport,
  Age = age
)

# Calculate descriptive statistics
desc_stats_overall <- data.frame(
  Measure = c("CognitiveStateAnxiety", "SomaticStateAnxiety", "SelfConfidence", "SCAT"),
  Mean = c(mean(data$CognitiveStateAnxiety), 
           mean(data$SomaticStateAnxiety),
           mean(data$SelfConfidence),
           mean(data$SCAT)),
  SD = c(sd(data$CognitiveStateAnxiety),
         sd(data$SomaticStateAnxiety),
         sd(data$SelfConfidence),
         sd(data$SCAT)),
  N = rep(nrow(data), 4)
)

# Round the numeric columns to 2 decimal places
desc_stats_overall$Mean <- round(desc_stats_overall$Mean, 2)
desc_stats_overall$SD <- round(desc_stats_overall$SD, 2)

# Calculate correlation matrix
cor_matrix <- cor(data[,c("CognitiveStateAnxiety", "SomaticStateAnxiety", "SelfConfidence", "SCAT")])

# Create a list of data frames to save as separate sheets
excel_data <- list(
  "Descriptive Statistics" = desc_stats_overall,
  "Correlation Matrix" = as.data.frame(round(cor_matrix, 2))
)

# Set working directory (adjust path as needed)
setwd("/Users/matthewwilliamson/Library/CloudStorage/OneDrive-UniversityofSunderland-LIVE/Year 3/Semester 1/Psychology/Rstudio Data")

# Save to Excel file
writexl::write_xlsx(excel_data, "Psychology_Analysis_Results.xlsx")

# Install and load required packages
if (!requireNamespace("gt", quietly = TRUE)) install.packages("gt")
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("kableExtra", quietly = TRUE)) install.packages("kableExtra")

library(gt)
library(tidyverse)
library(kableExtra)

# Create the data frame
cognitive_state_anxiety <- c(9,24,10,25,23,17,19,19,27,32,30,28,29,18,18,17,18,30,20,36,14,17,33,24,25,27,25,18,17,17,26,10,19,33,19,29,17,31,20)
somatic_state_anxiety <- c(11,23,27,20,28,17,20,22,27,23,19,26,26,15,11,14,25,24,19,33,15,21,26,19,24,12,22,11,15,16,26,12,17,28,27,26,14,22,15)
self_confidence <- c(33,17,19,29,28,31,22,23,15,13,20,23,12,30,30,12,16,20,26,14,27,20,20,21,27,19,17,25,29,32,27,23,24,22,28,21,17,10,26)
scat <- c(10,27,22,13,22,27,15,24,30,30,26,20,30,30,16,20,23,14,15,28,17,23,24,23,22,20,24,16,17,18,21,10,23,14,17,22,20,28,23)
gender <- factor(c("Male","Male","Male","Male","Female","Male","Male","Male","Male","Female","Male","Female","Female","Male","Male","Male","Female","Male","Male","Male","Male","Male","Female","Female","Female","Female","Male","Female","Male","Male","Female","Female","Female","Male","Female","Female","Female","Female","Female"))
sport <- factor(c("Team","Team","Team","Individual","Team","Individual","Individual","Team","Team","Team","Team","Individual","Individual","Team","Individual","Team","Team","Team","Team","Individual","Team","Team","Team","Team","Team","Individual","Individual","Individual","Individual","Individual","Team","Individual","Individual","Individual","Team","Individual","Individual","Individual","Individual"))
age <- c(22,20,21,26,20,21,22,21,21,20,21,20,22,23,22,20,21,20,20,20,42,20,22,23,22,20,22,22,20,20,21,20,21,21,20,20,20,22,22)

data <- data.frame(
  CognitiveStateAnxiety = cognitive_state_anxiety,
  SomaticStateAnxiety = somatic_state_anxiety,
  SelfConfidence = self_confidence,
  SCAT = scat,
  Gender = gender,
  Sport = sport,
  Age = age
)

# 1. Descriptive Statistics
desc_stats <- data %>%
  select(CognitiveStateAnxiety, SomaticStateAnxiety, SelfConfidence, SCAT) %>%
  summarise(across(everything(), list(
    Mean = ~mean(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    N = ~n()
  ))) %>%
  pivot_longer(everything(), 
               names_to = c("Measure", "Statistic"), 
               names_pattern = "(.*)_(.*)",
               values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  mutate(across(c(Mean, SD), ~round(., 2)))

# 2. Correlation Matrix
cor_matrix <- cor(data[,c("CognitiveStateAnxiety", "SomaticStateAnxiety", "SelfConfidence", "SCAT")])
cor_matrix <- round(cor_matrix, 2)

# 3. T-tests for Gender differences
t_test_results <- data.frame(
  Measure = character(),
  t_value = numeric(),
  df = numeric(),
  p_value = numeric(),
  s_value = numeric(),
  stringsAsFactors = FALSE
)

for(measure in c("CognitiveStateAnxiety", "SomaticStateAnxiety", "SelfConfidence", "SCAT")) {
  t_test <- t.test(data[[measure]] ~ data$Gender)
  t_test_results <- rbind(t_test_results, data.frame(
    Measure = measure,
    t_value = t_test$statistic,
    df = t_test$parameter,
    p_value = t_test$p.value,
    s_value = -log2(t_test$p.value)
  ))
}

t_test_results <- t_test_results %>%
  mutate(across(where(is.numeric), ~round(., 3)))

# 4. ANOVAs for Sport differences
anova_results <- data.frame(
  Measure = character(),
  F_value = numeric(),
  df = numeric(),
  p_value = numeric(),
  s_value = numeric(),
  stringsAsFactors = FALSE
)

for(measure in c("CognitiveStateAnxiety", "SomaticStateAnxiety", "SelfConfidence", "SCAT")) {
  aov_result <- aov(data[[measure]] ~ Sport, data = data)
  anova_sum <- summary(aov_result)[[1]]
  anova_results <- rbind(anova_results, data.frame(
    Measure = measure,
    F_value = anova_sum$"F value"[1],
    df = anova_sum$Df[1],
    p_value = anova_sum$"Pr(>F)"[1],
    s_value = -log2(anova_sum$"Pr(>F)"[1])
  ))
}

anova_results <- anova_results %>%
  mutate(across(where(is.numeric), ~round(., 3)))

# Create formatted tables
# 1. Descriptive Statistics Table
desc_stats_table <- desc_stats %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics"
  ) %>%
  fmt_number(columns = c("Mean", "SD"), decimals = 2)

# 2. Correlation Matrix Table
cor_matrix_table <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  gt() %>%
  tab_header(
    title = "Correlation Matrix"
  ) %>%
  fmt_number(columns = -1, decimals = 2)

# 3. T-test Results Table
t_test_table <- t_test_results %>%
  gt() %>%
  tab_header(
    title = "Independent t-test Results (Gender Differences)"
  ) %>%
  fmt_number(columns = c("t_value", "df", "p_value", "s_value"), decimals = 3)

# 4. ANOVA Results Table
anova_table <- anova_results %>%
  gt() %>%
  tab_header(
    title = "ANOVA Results (Sport Differences)"
  ) %>%
  fmt_number(columns = c("F_value", "df", "p_value", "s_value"), decimals = 3)

# Print all tables
print("Descriptive Statistics:")
print(desc_stats_table)
print("\
Correlation Matrix:")
print(cor_matrix_table)
print("\
T-test Results:")
print(t_test_table)
print("\
ANOVA Results:")
print(anova_table)

# Check the current working directory
current_dir <- getwd()
print(paste("Current working directory is:", current_dir))

# Save the file locally
local_file_path <- file.path(current_dir, "Statistical_Analysis_Results.xlsx")
write_xlsx(sheets_list, local_file_path)

print(paste("Results have been saved locally to:", local_file_path))

# Instructions to move the file manually
cat("To move the file to your OneDrive folder, use the following command in your terminal or file explorer:\n")
cat("mv", shQuote(local_file_path), shQuote("/Users/matthewwilliamson/Library/CloudStorage/OneDrive-UniversityofSunderland-LIVE/Year 3/Semester 1/Psychology/Rstudio Data/"), "\n")

# Load required library
library(ggplot2)

# Create the plot
plot_CSA_SSA <- ggplot(data, aes(x=CognitiveStateAnxiety, y=SomaticStateAnxiety)) +
  geom_point(color='black', alpha=0.6) +
  geom_smooth(method='lm', color='black', se=TRUE) +
  labs(x='Cognitive State Anxiety', 
       y='Somatic State Anxiety') +
  theme_classic() +
  theme(plot.title = element_text(face="bold", size=12),
        axis.title = element_text(size=10),
        panel.grid = element_blank())

# Display the plot
print(plot_CSA_SSA)

# Optional: Save the plot
ggsave("CSA_SSA_correlation.png", plot_CSA_SSA, width=6, height=4, dpi=300)

# Load required library
library(ggplot2)

# Create the plot
plot_SC_SCAT <- ggplot(data, aes(x=SelfConfidence, y=SCAT)) +
  geom_point(color='black', alpha=0.6) +
  geom_smooth(method='lm', color='black', se=TRUE) +
  labs(x='Self-Confidence', 
       y='Sport Competition Anxiety Test Score') +
  theme_classic() +
  theme(plot.title = element_text(face="bold", size=12),
        axis.title = element_text(size=10),
        panel.grid = element_blank())

# Display the plot
print(plot_SC_SCAT)

# Optional: Save the plot
ggsave("SC_SCAT_correlation.png", plot_SC_SCAT, width=6, height=4, dpi=300)

# Load required library
library(ggplot2)

# Create the plot
plot_CSA_SC <- ggplot(data, aes(x=CognitiveStateAnxiety, y=SelfConfidence)) +
  geom_point(color='black', alpha=0.6) +
  geom_smooth(method='lm', color='black', se=TRUE) +
  labs(x='Cognitive State Anxiety', 
       y='Self-Confidence') +
  theme_classic() +
  theme(plot.title = element_text(face="bold", size=12),
        axis.title = element_text(size=10),
        panel.grid = element_blank())

# Display the plot
print(plot_CSA_SC)

# Optional: Save the plot
ggsave("CSA_SC_correlation.png", plot_CSA_SC, width=6, height=4, dpi=300)

# Load necessary libraries
library(ggplot2)
library(tidyr)

# Create the data frame
data <- data.frame(
  CognitiveStateAnxiety = c(9,24,10,25,23,17,19,19,27,32,30,28,29,18,18,17,18,30,20,36,14,17,33,24,25,27,25,18,17,17,26,10,19,33,19,29,17,31,20),
  SomaticStateAnxiety = c(11,23,27,20,28,17,20,22,27,23,19,26,26,15,11,14,25,24,19,33,15,21,26,19,24,12,22,11,15,16,26,12,17,28,27,26,14,22,15),
  SelfConfidence = c(33,17,19,29,28,31,22,23,15,13,20,23,12,30,30,12,16,20,26,14,27,20,20,21,27,19,17,25,29,32,27,23,24,22,28,21,17,10,26),
  SCAT = c(10,27,22,13,22,27,15,24,30,30,26,20,30,30,16,20,23,14,15,28,17,23,24,23,22,20,24,16,17,18,21,10,23,14,17,22,20,28,23),
  Gender = c(1,1,1,1,2,1,1,1,1,2,1,2,2,1,1,1,2,1,1,1,1,1,2,2,2,2,1,2,1,1,2,2,2,1,2,2,2,2,2)
)

# Convert Gender to a factor with labels
data$Gender <- factor(data$Gender, levels = c(1, 2), labels = c("Male", "Female"))

# Reshape the data for plotting
data_long <- pivot_longer(data, cols = -Gender, names_to = "Measure", values_to = "Score")

# Create the box plot
plot <- ggplot(data_long, aes(x = Measure, y = Score, fill = Gender)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.7, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
               position = position_dodge(width = 0.8),
               aes(group = Gender)) +
  scale_fill_manual(values = c("skyblue", "lightpink")) +
  labs(x = "Measure", y = "Score", fill = "Gender") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  scale_x_discrete(labels = c("Cognitive\nState Anxiety", 
                              "Somatic\nState Anxiety", 
                              "Self\nConfidence", 
                              "SCAT"))

# Save the plot as a PDF
ggsave("plot.pdf", plot = plot, width = 8, height = 6)

# Load necessary libraries
library(ggplot2)
library(tidyr)

# Create the data frame
data <- data.frame(
  CognitiveStateAnxiety = c(9,24,10,25,23,17,19,19,27,32,30,28,29,18,18,17,18,30,20,36,14,17,33,24,25,27,25,18,17,17,26,10,19,33,19,29,17,31,20),
  SomaticStateAnxiety = c(11,23,27,20,28,17,20,22,27,23,19,26,26,15,11,14,25,24,19,33,15,21,26,19,24,12,22,11,15,16,26,12,17,28,27,26,14,22,15),
  SelfConfidence = c(33,17,19,29,28,31,22,23,15,13,20,23,12,30,30,12,16,20,26,14,27,20,20,21,27,19,17,25,29,32,27,23,24,22,28,21,17,10,26),
  SCAT = c(10,27,22,13,22,27,15,24,30,30,26,20,30,30,16,20,23,14,15,28,17,23,24,23,22,20,24,16,17,18,21,10,23,14,17,22,20,28,23),
  Sport = c(1,1,2,1,2,2,1,1,1,1,2,2,1,2,1,1,1,1,2,1,1,1,1,1,2,2,2,2,2,1,2,2,2,1,2,2,2,2,2)
)

# Convert Sport to a factor with labels
data$Sport <- factor(data$Sport, levels = c(1, 2), labels = c("Team Sport", "Individual Sport"))

# Reshape the data for plotting
data_long <- pivot_longer(data, cols = -Sport, names_to = "Measure", values_to = "Score")

# Create the box plot
plot <- ggplot(data_long, aes(x = Measure, y = Score, fill = Sport)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.7, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4,
               position = position_dodge(width = 0.8),
               aes(group = Sport)) +
  scale_fill_manual(values = c("#0343DF", "#E50000")) +  # Changed to blue and red
  labs(x = "Measure", 
       y = "Score", 
       fill = "Sport Type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank()
  ) +
  scale_x_discrete(labels = c("Cognitive\nState Anxiety", 
                              "Somatic\nState Anxiety", 
                              "Self\nConfidence", 
                              "SCAT"))

# Display the plot
print(plot)

# Save the plot
ggsave("sport_type_comparison_blue_red.pdf", plot = plot, width = 10, height = 7)