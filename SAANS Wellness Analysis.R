library(dplyr)
library(ggplot2)

data <- read.csv("SAANS_Questionnaire.csv")
data$Sex <- as.factor(data$Sex)
data$`City/Town` <- as.factor(data$`City/Town`)

calculate_wellness_score <- function(df) {
  df <- df %>%
    rowwise() %>%
    mutate(
      Wellness_Score = sum(
        c_across(c(
          Physical, Nutritional, Emotional, `Career and Financial Wellness`,
          Intellectual, Environmental, Social, Cosmic
        )),
        na.rm = TRUE
      )
    )
  return(df)
}

data <- calculate_wellness_score(data)

cat("Performing Exploratory Data Analysis...\n")

wellness_dimensions <- c("Physical", "Nutritional", "Emotional", "Career and Financial Wellness",
                         "Intellectual", "Environmental", "Social", "Cosmic")

for (dimension in wellness_dimensions) {
  ggplot(data, aes_string(x = dimension)) +
    geom_bar(fill = "steelblue") +
    theme_minimal() +
    labs(title = paste("Bar Plot of", dimension), x = dimension, y = "Count") +
    ggsave(paste0(dimension, "_barplot.png"))
}

for (dimension in wellness_dimensions) {
  ggplot(data, aes_string(x = "Sex", y = dimension)) +
    geom_boxplot(fill = "lightcoral") +
    theme_minimal() +
    labs(title = paste("Box Plot of", dimension, "by Sex"), x = "Sex", y = dimension) +
    ggsave(paste0(dimension, "_boxplot.png"))
}

for (dimension in wellness_dimensions) {
  ggplot(data, aes_string(x = dimension)) +
    geom_histogram(binwidth = 5, fill = "seagreen") +
    theme_minimal() +
    labs(title = paste("Histogram of", dimension), x = dimension, y = "Frequency") +
    ggsave(paste0(dimension, "_histogram.png"))
}

data_intervention <- data %>% filter(Intervention == 1)

t_test_result <- t.test(Percentage ~ Sex, data = data_intervention)
cat("T-Test Results:\n")
print(t_test_result)

anova_result <- aov(Percentage ~ `City/Town`, data = data_intervention)
cat("\nANOVA Results:\n")
summary(anova_result)

write.csv(data, "SAANS_Questionnaire_with_Wellness_Score.csv", row.names = FALSE)

cat("\nAnalysis and EDA completed. The updated dataset has been saved as 'SAANS_Questionnaire_with_Wellness_Score.csv'.")
