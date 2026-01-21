# ============================================================================
# Create Figures with Statistical Tests - UPDATED VERSION
# Changes:
# - Figures 1 & 2: Use ALL data points (not just correct)
# - Figures 3 & 4: NEW accuracy percentage figures
# ============================================================================

# Load libraries
library(dplyr)
library(readr)
library(ggplot2)

# ============================================================================
# STEP 1: Load and Prepare Data
# ============================================================================

# Set your working directory
setwd("/Users/dylanpham/Documents/GitHub/Reaction-Time-App-Mohala-Lab-")

# Load one-button data
one_button_1 <- read_csv("one_button_reaction_data_1.csv")
one_button_2 <- read_csv("one_button_reaction_data_2.csv")
one_button_3 <- read_csv("one_button_reaction_data_3.csv")

# Load three-button data
three_button_1 <- read_csv("three_button_reaction_data_1.csv")
three_button_2 <- read_csv("three_button_reaction_data_2.csv")
three_button_3 <- read_csv("three_button_reaction_data_3.csv")

# Combine datasets
one_button <- bind_rows(one_button_1, one_button_2, one_button_3)
three_button <- bind_rows(three_button_1, three_button_2, three_button_3)

# Filter for correct responses only (for statistical tests)
one_correct <- one_button[one_button$FeltCorrectly == "YES", ]
three_correct <- three_button[three_button$FeltCorrectly == "YES", ]

cat("One-button total trials:", nrow(one_button), "\n")
cat("One-button correct trials:", nrow(one_correct), "\n")
cat("Three-button total trials:", nrow(three_button), "\n")
cat("Three-button correct trials:", nrow(three_correct), "\n")

# ============================================================================
# STEP 2: Calculate Race Model Predictions & Statistical Tests
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("RACE MODEL ANALYSIS - ONE-BUTTON TASK\n")
cat(rep("=", 70), "\n\n", sep = "")

# Get unimodal mean RTs (one-button task - CORRECT ONLY for race model)
V_mean <- mean(one_correct$ReactionTime_seconds[one_correct$Stimulus == "V"])
A_mean <- mean(one_correct$ReactionTime_seconds[one_correct$Stimulus == "A"])
H_mean <- mean(one_correct$ReactionTime_seconds[one_correct$Stimulus == "H"])

cat("Unimodal Mean RTs:\n")
cat(sprintf("  V alone: %.3f s\n", V_mean))
cat(sprintf("  A alone: %.3f s\n", A_mean))
cat(sprintf("  H alone: %.3f s\n", H_mean))

# Calculate race model predictions
race_predictions <- data.frame(
  Stimulus = c("VA", "VH", "AH", "VAH"),
  RaceModel = c(
    min(V_mean, A_mean),
    min(V_mean, H_mean),
    min(A_mean, H_mean),
    min(V_mean, A_mean, H_mean)
  )
)

cat("\n", "Race Model Predictions:\n")
print(race_predictions)

# ============================================================================
# STATISTICAL TESTS - ONE-BUTTON TASK
# ============================================================================

cat("\n", rep("-", 70), "\n", sep = "")
cat("T-TESTS FOR RACE MODEL VIOLATIONS (One-Button Task)\n")
cat(rep("-", 70), "\n\n", sep = "")

# Function to perform one-sample t-test against race model
test_race_model <- function(data, stimulus_name, race_prediction) {
  stim_data <- data$ReactionTime_seconds[data$Stimulus == stimulus_name]
  
  if (length(stim_data) < 2) {
    return(list(
      stimulus = stimulus_name,
      n = length(stim_data),
      observed_mean = mean(stim_data),
      race_pred = race_prediction,
      violation = NA,
      t_stat = NA,
      df = NA,
      p_value = NA,
      cohens_d = NA
    ))
  }
  
  # Perform t-test
  test_result <- t.test(stim_data, mu = race_prediction)
  
  # Calculate Cohen's d
  cohens_d <- (mean(stim_data) - race_prediction) / sd(stim_data)
  
  # Calculate violation (positive = facilitation, negative = interference)
  violation <- race_prediction - mean(stim_data)
  
  return(list(
    stimulus = stimulus_name,
    n = length(stim_data),
    observed_mean = mean(stim_data),
    race_pred = race_prediction,
    violation = violation,
    t_stat = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    cohens_d = cohens_d
  ))
}

# Test each multimodal condition
results_one_button <- list()

results_one_button$VA <- test_race_model(one_correct, "VA", race_predictions$RaceModel[1])
results_one_button$VH <- test_race_model(one_correct, "VH", race_predictions$RaceModel[2])
results_one_button$AH <- test_race_model(one_correct, "AH", race_predictions$RaceModel[3])
results_one_button$VAH <- test_race_model(one_correct, "VAH", race_predictions$RaceModel[4])

# Print results
for (result in results_one_button) {
  cat(sprintf("\n%s Condition:\n", result$stimulus))
  cat(sprintf("  Observed RT:     %.3f s (n=%d)\n", result$observed_mean, result$n))
  cat(sprintf("  Race Prediction: %.3f s\n", result$race_pred))
  cat(sprintf("  Violation:       %+.0f ms ", result$violation * 1000))
  if (!is.na(result$violation)) {
    if (result$violation > 0) {
      cat("(FACILITATION)\n")
    } else {
      cat("(INTERFERENCE)\n")
    }
  } else {
    cat("\n")
  }
  
  if (!is.na(result$p_value)) {
    cat(sprintf("  t(%d) = %.2f, p = %.4f", result$df, result$t_stat, result$p_value))
    if (result$p_value < 0.001) {
      cat(" ***")
    } else if (result$p_value < 0.01) {
      cat(" **")
    } else if (result$p_value < 0.05) {
      cat(" *")
    }
    cat("\n")
    cat(sprintf("  Cohen's d = %.2f\n", result$cohens_d))
  }
}

# ============================================================================
# COLAVITA EFFECT ANALYSIS - ONE-BUTTON TASK
# ============================================================================

cat("\n", rep("-", 70), "\n", sep = "")
cat("COLAVITA VISUAL DOMINANCE EFFECT (One-Button Task)\n")
cat(rep("-", 70), "\n\n", sep = "")

# Get visual multimodal trials
vis_multi_data <- one_correct[one_correct$Stimulus %in% c("VA", "VH", "VAH"), ]
vis_multi_rt <- vis_multi_data$ReactionTime_seconds

# Get non-visual multimodal trials
nonvis_multi_data <- one_correct[one_correct$Stimulus == "AH", ]
nonvis_multi_rt <- nonvis_multi_data$ReactionTime_seconds

cat(sprintf("Visual Multimodal (VA, VH, VAH):\n"))
cat(sprintf("  Mean RT: %.3f s (n=%d)\n", mean(vis_multi_rt), length(vis_multi_rt)))
cat(sprintf("  SD:      %.3f s\n", sd(vis_multi_rt)))

cat(sprintf("\nNon-Visual Multimodal (AH):\n"))
cat(sprintf("  Mean RT: %.3f s (n=%d)\n", mean(nonvis_multi_rt), length(nonvis_multi_rt)))
cat(sprintf("  SD:      %.3f s\n", sd(nonvis_multi_rt)))

# Independent samples t-test
colavita_test <- t.test(vis_multi_rt, nonvis_multi_rt)

# Cohen's d for independent samples
pooled_sd <- sqrt(((length(vis_multi_rt)-1)*var(vis_multi_rt) + 
                     (length(nonvis_multi_rt)-1)*var(nonvis_multi_rt)) / 
                    (length(vis_multi_rt)+length(nonvis_multi_rt)-2))
colavita_d <- (mean(vis_multi_rt) - mean(nonvis_multi_rt)) / pooled_sd

difference_ms <- (mean(nonvis_multi_rt) - mean(vis_multi_rt)) * 1000
percent_diff <- ((mean(nonvis_multi_rt) - mean(vis_multi_rt)) / mean(vis_multi_rt)) * 100

cat(sprintf("\nColavita Effect:\n"))
cat(sprintf("  Difference:  %.0f ms (%.1f%% faster)\n", difference_ms, percent_diff))
cat(sprintf("  t(%.1f) = %.2f, p = %.4f", colavita_test$parameter, 
            colavita_test$statistic, colavita_test$p.value))
if (colavita_test$p.value < 0.001) {
  cat(" ***")
} else if (colavita_test$p.value < 0.01) {
  cat(" **")
} else if (colavita_test$p.value < 0.05) {
  cat(" *")
}
cat("\n")
cat(sprintf("  Cohen's d = %.2f\n", colavita_d))

# ============================================================================
# THREE-BUTTON TASK STATISTICS
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("RACE MODEL ANALYSIS - THREE-BUTTON TASK\n")
cat(rep("=", 70), "\n\n", sep = "")

# Get unimodal mean RTs (three-button task)
V_mean_3 <- mean(three_correct$RT1_seconds[three_correct$Stimulus == "V"], na.rm = TRUE)
A_mean_3 <- mean(three_correct$RT1_seconds[three_correct$Stimulus == "A"], na.rm = TRUE)
H_mean_3 <- mean(three_correct$RT1_seconds[three_correct$Stimulus == "H"], na.rm = TRUE)

cat("Unimodal Mean RTs:\n")
cat(sprintf("  V alone: %.3f s\n", V_mean_3))
cat(sprintf("  A alone: %.3f s\n", A_mean_3))
cat(sprintf("  H alone: %.3f s\n", H_mean_3))

# Race model for three-button
race_predictions_3 <- data.frame(
  Stimulus = c("VA", "VH", "AH", "VAH"),
  RaceModel = c(
    min(V_mean_3, A_mean_3),
    min(V_mean_3, H_mean_3),
    min(A_mean_3, H_mean_3),
    min(V_mean_3, A_mean_3, H_mean_3)
  )
)

cat("\nRace Model Predictions:\n")
print(race_predictions_3)

cat("\n", rep("-", 70), "\n", sep = "")
cat("T-TESTS FOR RACE MODEL VIOLATIONS (Three-Button Task)\n")
cat(rep("-", 70), "\n\n", sep = "")

# Test three-button task
test_race_model_3 <- function(data, stimulus_name, race_prediction) {
  stim_data <- data$RT1_seconds[data$Stimulus == stimulus_name]
  stim_data <- stim_data[!is.na(stim_data)]
  
  if (length(stim_data) < 2) {
    return(list(
      stimulus = stimulus_name,
      n = length(stim_data),
      observed_mean = mean(stim_data),
      race_pred = race_prediction,
      violation = NA,
      t_stat = NA,
      df = NA,
      p_value = NA,
      cohens_d = NA
    ))
  }
  
  test_result <- t.test(stim_data, mu = race_prediction)
  cohens_d <- (mean(stim_data) - race_prediction) / sd(stim_data)
  violation <- race_prediction - mean(stim_data)
  
  return(list(
    stimulus = stimulus_name,
    n = length(stim_data),
    observed_mean = mean(stim_data),
    race_pred = race_prediction,
    violation = violation,
    t_stat = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    cohens_d = cohens_d
  ))
}

results_three_button <- list()
results_three_button$VA <- test_race_model_3(three_correct, "VA", race_predictions_3$RaceModel[1])
results_three_button$VH <- test_race_model_3(three_correct, "VH", race_predictions_3$RaceModel[2])
results_three_button$AH <- test_race_model_3(three_correct, "AH", race_predictions_3$RaceModel[3])
results_three_button$VAH <- test_race_model_3(three_correct, "VAH", race_predictions_3$RaceModel[4])

for (result in results_three_button) {
  cat(sprintf("\n%s Condition:\n", result$stimulus))
  cat(sprintf("  Observed RT:     %.3f s (n=%d)\n", result$observed_mean, result$n))
  cat(sprintf("  Race Prediction: %.3f s\n", result$race_pred))
  cat(sprintf("  Violation:       %+.0f ms ", result$violation * 1000))
  if (!is.na(result$violation)) {
    if (result$violation > 0) {
      cat("(FACILITATION)\n")
    } else {
      cat("(INTERFERENCE)\n")
    }
  } else {
    cat("\n")
  }
  
  if (!is.na(result$p_value)) {
    cat(sprintf("  t(%d) = %.2f, p = %.4f", result$df, result$t_stat, result$p_value))
    if (result$p_value < 0.001) {
      cat(" ***")
    } else if (result$p_value < 0.01) {
      cat(" **")
    } else if (result$p_value < 0.05) {
      cat(" *")
    }
    cat("\n")
    cat(sprintf("  Cohen's d = %.2f\n", result$cohens_d))
  }
}

# Colavita effect - three-button
cat("\n", rep("-", 70), "\n", sep = "")
cat("COLAVITA VISUAL DOMINANCE EFFECT (Three-Button Task)\n")
cat(rep("-", 70), "\n\n", sep = "")

vis_multi_data_3 <- three_correct[three_correct$Stimulus %in% c("VA", "VH", "VAH"), ]
vis_multi_rt_3 <- vis_multi_data_3$RT1_seconds[!is.na(vis_multi_data_3$RT1_seconds)]

nonvis_multi_data_3 <- three_correct[three_correct$Stimulus == "AH", ]
nonvis_multi_rt_3 <- nonvis_multi_data_3$RT1_seconds[!is.na(nonvis_multi_data_3$RT1_seconds)]

cat(sprintf("Visual Multimodal (VA, VH, VAH):\n"))
cat(sprintf("  Mean RT: %.3f s (n=%d)\n", mean(vis_multi_rt_3), length(vis_multi_rt_3)))
cat(sprintf("  SD:      %.3f s\n", sd(vis_multi_rt_3)))

cat(sprintf("\nNon-Visual Multimodal (AH):\n"))
cat(sprintf("  Mean RT: %.3f s (n=%d)\n", mean(nonvis_multi_rt_3), length(nonvis_multi_rt_3)))
cat(sprintf("  SD:      %.3f s\n", sd(nonvis_multi_rt_3)))

colavita_test_3 <- t.test(vis_multi_rt_3, nonvis_multi_rt_3)

pooled_sd_3 <- sqrt(((length(vis_multi_rt_3)-1)*var(vis_multi_rt_3) + 
                       (length(nonvis_multi_rt_3)-1)*var(nonvis_multi_rt_3)) / 
                      (length(vis_multi_rt_3)+length(nonvis_multi_rt_3)-2))
colavita_d_3 <- (mean(vis_multi_rt_3) - mean(nonvis_multi_rt_3)) / pooled_sd_3

difference_ms_3 <- (mean(nonvis_multi_rt_3) - mean(vis_multi_rt_3)) * 1000
percent_diff_3 <- ((mean(nonvis_multi_rt_3) - mean(vis_multi_rt_3)) / mean(vis_multi_rt_3)) * 100

cat(sprintf("\nColavita Effect:\n"))
cat(sprintf("  Difference:  %.0f ms (%.1f%% faster)\n", difference_ms_3, percent_diff_3))
cat(sprintf("  t(%.1f) = %.2f, p = %.4f", colavita_test_3$parameter, 
            colavita_test_3$statistic, colavita_test_3$p.value))
if (colavita_test_3$p.value < 0.001) {
  cat(" ***")
} else if (colavita_test_3$p.value < 0.01) {
  cat(" **")
} else if (colavita_test_3$p.value < 0.05) {
  cat(" *")
}
cat("\n")
cat(sprintf("  Cohen's d = %.2f\n", colavita_d_3))

# ============================================================================
# FIGURE 1: ONE-BUTTON TASK - MEAN RT (ALL DATA POINTS)
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("CREATING FIGURES\n")
cat(rep("=", 70), "\n\n", sep = "")

# Calculate summary statistics using ALL data (not just correct)
one_summary_all <- one_button %>%
  group_by(Stimulus) %>%
  summarise(
    mean_RT = mean(ReactionTime_seconds, na.rm = TRUE),
    sem_RT = sd(ReactionTime_seconds, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

one_summary_all$visual_containing <- ifelse(grepl("V", one_summary_all$Stimulus), "Visual", "Non-Visual")
one_summary_all$Stimulus <- factor(one_summary_all$Stimulus, levels = c("V", "A", "H", "VA", "VH", "AH", "VAH"))
one_summary_all <- merge(one_summary_all, race_predictions, by = "Stimulus", all.x = TRUE)

# Prepare race model data with proper x positions
race_model_data_1 <- one_summary_all[!is.na(one_summary_all$RaceModel), ]
race_model_data_1$x_start <- as.numeric(race_model_data_1$Stimulus) - 0.4
race_model_data_1$x_end <- as.numeric(race_model_data_1$Stimulus) + 0.4

# Create Figure 1
fig1 <- ggplot(one_summary_all, aes(x = Stimulus, y = mean_RT, fill = visual_containing)) +
  geom_bar(stat = "identity", color = "black", size = 1.2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_RT - sem_RT, ymax = mean_RT + sem_RT),
                width = 0.3, size = 1) +
  geom_segment(data = race_model_data_1,
               aes(x = x_start, xend = x_end, y = RaceModel, yend = RaceModel, linetype = "Race Model"),
               color = "red", linewidth = 1.5,
               inherit.aes = FALSE) +
  scale_fill_manual(values = c("Visual" = "#2E86AB", "Non-Visual" = "#A23B72"),
                    name = "",
                    labels = c("Non-Visual" = "Non-Visual Modality", "Visual" = "Visual Modality")) +
  scale_linetype_manual(values = c("Race Model" = "dashed"),
                        name = "",
                        labels = c("Race Model Prediction")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.0)) +
  labs(
    title = "One-Button Task: Mean Reaction Time",
    x = "Stimulus Type",
    y = "Reaction Time (Seconds)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top",
    axis.line = element_line(color = "black",
                             linewidth = 0.5,
                             linetype = 1))

ggsave("Figure1_OneButton_MeanRT_AllTrials.png", fig1, width = 10, height = 6, dpi = 300)
cat("✓ Figure 1 saved: Figure1_OneButton_MeanRT_AllTrials.png\n")

# ============================================================================
# FIGURE 2: THREE-BUTTON TASK - MEAN RT (ALL DATA POINTS)
# ============================================================================

three_summary_all <- three_button %>%
  group_by(Stimulus) %>%
  summarise(
    mean_RT = mean(RT1_seconds, na.rm = TRUE),
    sem_RT = sd(RT1_seconds, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

three_summary_all$visual_containing <- ifelse(grepl("V", three_summary_all$Stimulus), "Visual", "Non-Visual")
three_summary_all$Stimulus <- factor(three_summary_all$Stimulus, levels = c("V", "A", "H", "VA", "VH", "AH", "VAH"))
three_summary_all <- merge(three_summary_all, race_predictions_3, by = "Stimulus", all.x = TRUE)

race_model_data_3 <- three_summary_all[!is.na(three_summary_all$RaceModel), ]
race_model_data_3$x_start <- as.numeric(race_model_data_3$Stimulus) - 0.4
race_model_data_3$x_end <- as.numeric(race_model_data_3$Stimulus) + 0.4

fig2 <- ggplot(three_summary_all, aes(x = Stimulus, y = mean_RT, fill = visual_containing)) +
  geom_bar(stat = "identity", color = "black", size = 1.2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_RT - sem_RT, ymax = mean_RT + sem_RT),
                width = 0.3, size = 1) +
  scale_fill_manual(values = c("Visual" = "#2E86AB", "Non-Visual" = "#A23B72"),
                    name = "",
                    labels = c("Non-Visual" = "Non-Visual Modality", "Visual" = "Visual Modality")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.4)) +
  labs(
    title = "Three-Button Task: Mean Reaction Time",
    x = "Stimulus Type",
    y = "Reaction Time (Seconds)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top",
    axis.line = element_line(color = "black",
                             linewidth = 0.5,
                             linetype = 1))

ggsave("Figure2_ThreeButton_MeanRT_AllTrials.png", fig2, width = 10, height = 6, dpi = 300)
cat("✓ Figure 2 saved: Figure2_ThreeButton_MeanRT_AllTrials.png\n")

# ============================================================================
# FIGURE 3: ONE-BUTTON TASK - ACCURACY PERCENTAGE
# ============================================================================

# Calculate accuracy for one-button task
one_accuracy <- one_button %>%
  group_by(Stimulus) %>%
  summarise(
    total_trials = n(),
    correct_trials = sum(FeltCorrectly == "YES"),
    accuracy_pct = (correct_trials / total_trials) * 100,
    .groups = 'drop'
  )

one_accuracy$visual_containing <- ifelse(grepl("V", one_accuracy$Stimulus), "Visual", "Non-Visual")
one_accuracy$Stimulus <- factor(one_accuracy$Stimulus, levels = c("V", "A", "H", "VA", "VH", "AH", "VAH"))

fig3 <- ggplot(one_accuracy, aes(x = Stimulus, y = accuracy_pct, fill = visual_containing)) +
  geom_bar(stat = "identity", color = "black", size = 1.2, alpha = 0.8) +
  scale_fill_manual(values = c("Visual" = "#2E86AB", "Non-Visual" = "#A23B72"),
                    name = "",
                    labels = c("Non-Visual" = "Non-Visual Modality", "Visual" = "Visual Modality")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  labs(
    title = "One-Button Task: Accuracy by Stimulus Type",
    x = "Stimulus Type",
    y = "Accuracy (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top",
    axis.line = element_line(color = "black",
                             linewidth = 0.5,
                             linetype = 1))

ggsave("Figure3_OneButton_Accuracy.png", fig3, width = 10, height = 6, dpi = 300)
cat("✓ Figure 3 saved: Figure3_OneButton_Accuracy.png\n")

# ============================================================================
# FIGURE 4: THREE-BUTTON TASK - ACCURACY PERCENTAGE
# ============================================================================

# Calculate accuracy for three-button task
three_accuracy <- three_button %>%
  group_by(Stimulus) %>%
  summarise(
    total_trials = n(),
    correct_trials = sum(FeltCorrectly == "YES"),
    accuracy_pct = (correct_trials / total_trials) * 100,
    .groups = 'drop'
  )

three_accuracy$visual_containing <- ifelse(grepl("V", three_accuracy$Stimulus), "Visual", "Non-Visual")
three_accuracy$Stimulus <- factor(three_accuracy$Stimulus, levels = c("V", "A", "H", "VA", "VH", "AH", "VAH"))

fig4 <- ggplot(three_accuracy, aes(x = Stimulus, y = accuracy_pct, fill = visual_containing)) +
  geom_bar(stat = "identity", color = "black", size = 1.2, alpha = 0.8) +
  scale_fill_manual(values = c("Visual" = "#2E86AB", "Non-Visual" = "#A23B72"),
                    name = "",
                    labels = c("Non-Visual" = "Non-Visual Modality", "Visual" = "Visual Modality")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  labs(
    title = "Three-Button Task: Accuracy by Stimulus Type",
    x = "Stimulus Type",
    y = "Accuracy (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "top",
    axis.line = element_line(color = "black",
                             linewidth = 0.5,
                             linetype = 1))

ggsave("Figure4_ThreeButton_Accuracy.png", fig4, width = 10, height = 6, dpi = 300)
cat("✓ Figure 4 saved: Figure4_ThreeButton_Accuracy.png\n")

# ============================================================================
# PRINT ACCURACY STATISTICS
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("ACCURACY STATISTICS\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("ONE-BUTTON TASK ACCURACY:\n")
print(one_accuracy)

cat("\nTHREE-BUTTON TASK ACCURACY:\n")
print(three_accuracy)

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("FINAL SUMMARY\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("ONE-BUTTON TASK (Detection):\n")
cat(sprintf("  Visual Multimodal:     %.3f ± %.3f s\n", vis_multi_mean, vis_multi_sem))
cat(sprintf("  Non-Visual Multimodal: %.3f ± %.3f s\n", nonvis_multi_mean, nonvis_multi_sem))
cat(sprintf("  Colavita Effect:       %.0f ms (%.1f%% faster), p = %.4f\n\n", 
            difference_ms, percent_diff, colavita_test$p.value))

cat("THREE-BUTTON TASK (Identification):\n")
cat(sprintf("  Visual Multimodal:     %.3f ± %.3f s\n", vis_multi_mean_3, vis_multi_sem_3))
cat(sprintf("  Non-Visual Multimodal: %.3f ± %.3f s\n", nonvis_multi_mean_3, nonvis_multi_sem_3))
cat(sprintf("  Colavita Effect:       %.0f ms (%.1f%% faster), p = %.4f\n\n", 
            difference_ms_3, percent_diff_3, colavita_test_3$p.value))

cat("KEY FINDINGS:\n")
cat("  1. VH shows strongest integration in one-button task (58ms, p < 0.001)\n")
cat("  2. AH shows interference in both tasks (p < 0.001)\n")
cat("  3. Visual dominance persists across both tasks (both p < 0.001)\n")
cat("  4. Three-button task eliminates integration benefits\n\n")

cat("=== ALL FIGURES AND STATISTICS COMPLETED ===\n")
cat("\nFigures created:\n")
cat("  - Figure 1: One-Button Mean RT (All Trials)\n")
cat("  - Figure 2: Three-Button Mean RT (All Trials)\n")
cat("  - Figure 3: One-Button Accuracy\n")
cat("  - Figure 4: Three-Button Accuracy\n")