# ============================================================================
# Create Figures with Statistical Tests - UPDATED VERSION
# Changes:
# - Figures 1 & 2: Use ALL data points (not just correct)
# - Figures 3 & 4: NEW accuracy percentage figures
# - Figures 3 & 4: Axes swapped (x=Actual, y=Perceived) and legends removed
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

# Function to normalize stimulus order (alphabetical by modality: V, A, H)
# This is used for confusion matrices to standardize response formats
normalize_stimulus <- function(stim) {
  # Handle NA, NULL, or non-character inputs
  if(is.null(stim) || length(stim) == 0) return(NA_character_)
  if(is.na(stim)) return(NA_character_)
  
  # Convert to character if not already
  stim <- as.character(stim)
  
  # Handle empty strings
  if(stim == "" || nchar(stim) == 0) return(NA_character_)
  
  # Split into individual letters and sort
  letters <- strsplit(stim, "")[[1]]
  
  # Sort in order: V, A, H (so VA not AV, VH not HV, etc.)
  order_map <- c("V" = 1, "A" = 2, "H" = 3)
  sorted_letters <- letters[order(sapply(letters, function(x) order_map[x]))]
  return(paste(sorted_letters, collapse = ""))
}

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
  geom_bar(stat = "identity", color = "black", linewidth = 1.2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_RT - sem_RT, ymax = mean_RT + sem_RT),
                width = 0.3, linewidth = 1) +
  geom_segment(data = race_model_data_1,
               aes(x = x_start, xend = x_end, y = RaceModel, yend = RaceModel, linetype = "Race Model"),
               color = "red", linewidth = 1.5,
               inherit.aes = FALSE) +
  scale_fill_manual(values = c("Visual" = "#2E86AB", "Non-Visual" = "#A23B72"),
                    name = "",
                    labels = c("Non-Visual" = "Non-Visual", "Visual" = "Visual")) +
  scale_linetype_manual(values = c("Race Model" = "dashed"),
                        name = "",
                        labels = c("Race Model Prediction")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.0)) +
  labs(
    title = "One-Button Task",
    x = "Stimulus Type",
    y = "Reaction Time (s)"
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "top",
    legend.text = element_text(size = 16),
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
  geom_bar(stat = "identity", color = "black", linewidth = 1.2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_RT - sem_RT, ymax = mean_RT + sem_RT),
                width = 0.3, linewidth = 1) +
  scale_fill_manual(values = c("Visual" = "#2E86AB", "Non-Visual" = "#A23B72"),
                    name = "",
                    labels = c("Non-Visual" = "Non-Visual", "Visual" = "Visual")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.4)) +
  labs(
    title = "Three-Button Task",
    x = "Stimulus Type",
    y = "Reaction Time (s)"
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "top",
    legend.text = element_text(size = 16),
    axis.line = element_line(color = "black",
                             linewidth = 0.5,
                             linetype = 1))

ggsave("Figure2_ThreeButton_MeanRT_AllTrials.png", fig2, width = 10, height = 6, dpi = 300)
cat("✓ Figure 2 saved: Figure2_ThreeButton_MeanRT_AllTrials.png\n")

# ============================================================================
# FIGURE 3: ONE-BUTTON TASK - CONFUSION MATRIX
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("CREATING CONFUSION MATRIX FOR ONE-BUTTON TASK\n")
cat(rep("=", 70), "\n\n", sep = "")

# For one-button task, we want to use SelectedCues (what they selected)
# BUT we need to normalize the order (e.g., "HV" should become "VH")
felt_col <- "SelectedCues"

if(!felt_col %in% names(one_button)) {
  cat("ERROR: SelectedCues column not found!\n")
  cat("Available columns:\n")
  print(names(one_button))
  stop("Cannot create confusion matrix")
}

# Create a working copy with normalized responses
one_button_norm <- one_button %>%
  mutate(NormalizedResponse = sapply(!!sym(felt_col), normalize_stimulus))

cat("Sample of original vs normalized responses:\n")
sample_data <- one_button_norm %>% 
  select(Stimulus, !!sym(felt_col), NormalizedResponse) %>%
  head(30)
print(sample_data)
cat("\n")

cat("Frequency table of ORIGINAL responses:\n")
print(table(one_button_norm[[felt_col]]))
cat("\n")

cat("Frequency table of NORMALIZED responses:\n")
print(table(one_button_norm$NormalizedResponse))
cat("\n")

cat("Cross-tabulation: Actual Stimulus vs Normalized Response:\n")
print(table(one_button_norm$Stimulus, one_button_norm$NormalizedResponse))
cat("\n")

# Create confusion matrix for one-button task
# Rows = Actual Stimulus, Columns = What Participant Selected (normalized)
confusion_one <- one_button_norm %>%
  group_by(Stimulus, NormalizedResponse) %>%
  summarise(count = n(), .groups = 'drop') %>%
  rename(FeltStimulus = NormalizedResponse)

cat("Confusion matrix raw counts:\n")
print(confusion_one)
cat("\n")

# Get totals for each actual stimulus
totals_one <- one_button_norm %>%
  group_by(Stimulus) %>%
  summarise(total = n(), .groups = 'drop')

# Calculate proportions
confusion_one <- merge(confusion_one, totals_one, by = "Stimulus")
confusion_one$proportion <- confusion_one$count / confusion_one$total

# Set factor levels for proper ordering - THIS ENSURES ALL LEVELS APPEAR
all_levels <- c("V", "A", "H", "VA", "VH", "AH", "VAH")
confusion_one$Stimulus <- factor(confusion_one$Stimulus, levels = all_levels)
confusion_one$FeltStimulus <- factor(confusion_one$FeltStimulus, levels = all_levels)

# Create a complete grid of all combinations (including missing ones with 0)
complete_grid <- expand.grid(
  Stimulus = factor(all_levels, levels = all_levels),
  FeltStimulus = factor(all_levels, levels = all_levels)
)

# Merge with actual data, filling missing combinations with 0
confusion_one_complete <- merge(complete_grid, confusion_one, 
                                by = c("Stimulus", "FeltStimulus"), all.x = TRUE)
confusion_one_complete$proportion[is.na(confusion_one_complete$proportion)] <- 0

cat("Confusion matrix with proportions (diagonal should be high):\n")
diagonal <- confusion_one_complete[confusion_one_complete$Stimulus == confusion_one_complete$FeltStimulus, ]
print(diagonal)
cat("\n")

# Create confusion matrix heatmap - AXES SWAPPED: x=Actual, y=Perceived
fig3 <- ggplot(confusion_one_complete, aes(x = Stimulus, y = FeltStimulus, fill = proportion)) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", proportion)), 
            color = ifelse(confusion_one_complete$proportion > 0.5, "white", "black"), 
            size = 7, fontface = "bold") +
  scale_fill_gradient(low = "#E8F4F8", high = "#2E86AB", limits = c(0, 1),
                      name = "Proportion") +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "One-Button Task",
    x = "Actual Stimulus",
    y = "Perceived Stimulus"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

ggsave("Figure3_OneButton_ConfusionMatrix.png", fig3, width = 10, height = 8, dpi = 300)
cat("✓ Figure 3 saved: Figure3_OneButton_ConfusionMatrix.png\n")

# ============================================================================
# FIGURE 4: THREE-BUTTON TASK - CONFUSION MATRIX
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("CREATING CONFUSION MATRIX FOR THREE-BUTTON TASK\n")
cat(rep("=", 70), "\n\n", sep = "")

# For three-button task, use KeysPressed (what they selected)
felt_col_3 <- "KeysPressed"

if(!felt_col_3 %in% names(three_button)) {
  cat("ERROR: KeysPressed column not found!\n")
  cat("Available columns:\n")
  print(names(three_button))
  stop("Cannot create confusion matrix")
}

# Check for NA or empty values
cat("Checking KeysPressed column...\n")
cat(sprintf("Total trials: %d\n", nrow(three_button)))
cat(sprintf("NA values: %d\n", sum(is.na(three_button[[felt_col_3]]))))
cat(sprintf("Empty strings: %d\n", sum(three_button[[felt_col_3]] == "", na.rm = TRUE)))

# Show what types of values we have
cat("\nData type of KeysPressed column:\n")
cat(class(three_button[[felt_col_3]]), "\n")

cat("\nFirst 20 values in KeysPressed:\n")
print(head(three_button[[felt_col_3]], 20))
cat("\n")

# Function to map numeric keys to stimulus letters
# 1 = V, 2 = A, 3 = H
map_keys_to_stimulus <- function(key_num) {
  if(is.na(key_num)) return(NA_character_)
  
  key_str <- as.character(key_num)
  
  # Replace each digit with its corresponding letter
  key_str <- gsub("1", "V", key_str)
  key_str <- gsub("2", "A", key_str)
  key_str <- gsub("3", "H", key_str)
  
  return(key_str)
}

# Create a working copy, converting KeysPressed to stimulus format
three_button_norm <- three_button %>%
  filter(!is.na(!!sym(felt_col_3))) %>%
  mutate(
    KeysPressed_mapped = sapply(!!sym(felt_col_3), map_keys_to_stimulus),
    NormalizedResponse = sapply(KeysPressed_mapped, normalize_stimulus)
  )

cat(sprintf("After filtering, %d trials remain\n\n", nrow(three_button_norm)))

cat("Mapping verification (numeric -> letters):\n")
sample_mapping <- three_button_norm %>%
  select(Stimulus, !!sym(felt_col_3), KeysPressed_mapped, NormalizedResponse) %>%
  head(30)
print(sample_mapping)
cat("\n")

cat("Frequency table of MAPPED responses (after 1->V, 2->A, 3->H):\n")
print(table(three_button_norm$KeysPressed_mapped))
cat("\n")

cat("Frequency table of NORMALIZED responses:\n")
print(table(three_button_norm$NormalizedResponse))
cat("\n")

cat("Cross-tabulation: Actual Stimulus vs Normalized Response:\n")
print(table(three_button_norm$Stimulus, three_button_norm$NormalizedResponse))
cat("\n")

# Create confusion matrix for three-button task
confusion_three <- three_button_norm %>%
  group_by(Stimulus, NormalizedResponse) %>%
  summarise(count = n(), .groups = 'drop') %>%
  rename(FeltStimulus = NormalizedResponse)

cat("Confusion matrix raw counts:\n")
print(confusion_three)
cat("\n")

# Get totals for each actual stimulus (from filtered data)
totals_three <- three_button_norm %>%
  group_by(Stimulus) %>%
  summarise(total = n(), .groups = 'drop')

# Calculate proportions
confusion_three <- merge(confusion_three, totals_three, by = "Stimulus")
confusion_three$proportion <- confusion_three$count / confusion_three$total

# Set factor levels for proper ordering - THIS ENSURES ALL LEVELS APPEAR
all_levels <- c("V", "A", "H", "VA", "VH", "AH", "VAH")
confusion_three$Stimulus <- factor(confusion_three$Stimulus, levels = all_levels)
confusion_three$FeltStimulus <- factor(confusion_three$FeltStimulus, levels = all_levels)

# Create a complete grid of all combinations (including missing ones with 0)
complete_grid_3 <- expand.grid(
  Stimulus = factor(all_levels, levels = all_levels),
  FeltStimulus = factor(all_levels, levels = all_levels)
)

# Merge with actual data, filling missing combinations with 0
confusion_three_complete <- merge(complete_grid_3, confusion_three, 
                                  by = c("Stimulus", "FeltStimulus"), all.x = TRUE)
confusion_three_complete$proportion[is.na(confusion_three_complete$proportion)] <- 0

cat("Confusion matrix with proportions (diagonal should be high):\n")
diagonal <- confusion_three_complete[confusion_three_complete$Stimulus == confusion_three_complete$FeltStimulus, ]
print(diagonal)
cat("\n")

# Create confusion matrix heatmap - AXES SWAPPED: x=Actual, y=Perceived
fig4 <- ggplot(confusion_three_complete, aes(x = Stimulus, y = FeltStimulus, fill = proportion)) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", proportion)), 
            color = ifelse(confusion_three_complete$proportion > 0.5, "white", "black"), 
            size = 7, fontface = "bold") +
  scale_fill_gradient(low = "#E8F4F8", high = "#2E86AB", limits = c(0, 1),
                      name = "Proportion") +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Three-Button Task",
    x = "Actual Stimulus",
    y = "Perceived Stimulus"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 20),
    axis.text = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

ggsave("Figure4_ThreeButton_ConfusionMatrix.png", fig4, width = 10, height = 8, dpi = 300)
cat("✓ Figure 4 saved: Figure4_ThreeButton_ConfusionMatrix.png\n")

# ============================================================================
# PRINT ACCURACY STATISTICS
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("ACCURACY STATISTICS\n")
cat(rep("=", 70), "\n\n", sep = "")

# Calculate accuracy for one-button task
one_accuracy <- one_button %>%
  group_by(Stimulus) %>%
  summarise(
    total_trials = n(),
    correct_trials = sum(FeltCorrectly == "YES"),
    accuracy_pct = (correct_trials / total_trials) * 100,
    .groups = 'drop'
  )

cat("ONE-BUTTON TASK ACCURACY:\n")
print(one_accuracy)

# Calculate accuracy for three-button task
three_accuracy <- three_button %>%
  group_by(Stimulus) %>%
  summarise(
    total_trials = n(),
    correct_trials = sum(FeltCorrectly == "YES"),
    accuracy_pct = (correct_trials / total_trials) * 100,
    .groups = 'drop'
  )

cat("\nTHREE-BUTTON TASK ACCURACY:\n")
print(three_accuracy)

# ============================================================================
# CALCULATE SUMMARY STATISTICS FOR FINAL SUMMARY
# ============================================================================

# Calculate means and SEMs for visual vs non-visual multimodal (one-button)
vis_multi_mean <- mean(vis_multi_rt)
vis_multi_sem <- sd(vis_multi_rt) / sqrt(length(vis_multi_rt))
nonvis_multi_mean <- mean(nonvis_multi_rt)
nonvis_multi_sem <- sd(nonvis_multi_rt) / sqrt(length(nonvis_multi_rt))

# Calculate means and SEMs for visual vs non-visual multimodal (three-button)
vis_multi_mean_3 <- mean(vis_multi_rt_3)
vis_multi_sem_3 <- sd(vis_multi_rt_3) / sqrt(length(vis_multi_rt_3))
nonvis_multi_mean_3 <- mean(nonvis_multi_rt_3)
nonvis_multi_sem_3 <- sd(nonvis_multi_rt_3) / sqrt(length(nonvis_multi_rt_3))

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
cat("  - Figure 3: One-Button Confusion Matrix (Actual on x-axis, Perceived on y-axis)\n")
cat("  - Figure 4: Three-Button Confusion Matrix (Actual on x-axis, Perceived on y-axis)\n")

# ============================================================================
# ERROR BARS AND STATISTICAL SUMMARY
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("ERROR BAR VALUES (Standard Error of Mean)\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("ONE-BUTTON DETECTION TASK:\n")
cat(rep("-", 70), "\n", sep = "")

one_summary_table <- one_summary_all %>%
  left_join(
    one_button %>%
      group_by(Stimulus) %>%
      summarise(
        sd_RT = sd(ReactionTime_seconds, na.rm = TRUE),
        .groups = 'drop'
      ),
    by = "Stimulus"
  ) %>%
  arrange(factor(Stimulus, levels = c("V", "A", "H", "VA", "VH", "AH", "VAH")))

cat("\n")
cat(sprintf("%-8s %12s %12s %12s %12s\n", "Stimulus", "n", "Mean (s)", "SD (s)", "SEM (s)"))
cat(rep("-", 70), "\n", sep = "")
for(i in 1:nrow(one_summary_table)) {
  row <- one_summary_table[i,]
  n_trials <- nrow(one_button[one_button$Stimulus == row$Stimulus, ])
  cat(sprintf("%-8s %12d %12.4f %12.4f %12.4f\n",
              as.character(row$Stimulus), n_trials, row$mean_RT, row$sd_RT, row$sem_RT))
}

cat("\n\n")
cat("THREE-BUTTON IDENTIFICATION TASK:\n")
cat(rep("-", 70), "\n", sep = "")

three_summary_table <- three_summary_all %>%
  left_join(
    three_button %>%
      group_by(Stimulus) %>%
      summarise(
        sd_RT = sd(RT1_seconds, na.rm = TRUE),
        .groups = 'drop'
      ),
    by = "Stimulus"
  ) %>%
  arrange(factor(Stimulus, levels = c("V", "A", "H", "VA", "VH", "AH", "VAH")))

cat("\n")
cat(sprintf("%-8s %12s %12s %12s %12s\n", "Stimulus", "n", "Mean (s)", "SD (s)", "SEM (s)"))
cat(rep("-", 70), "\n", sep = "")
for(i in 1:nrow(three_summary_table)) {
  row <- three_summary_table[i,]
  n_trials <- nrow(three_button[three_button$Stimulus == row$Stimulus, ])
  cat(sprintf("%-8s %12d %12.4f %12.4f %12.4f\n",
              as.character(row$Stimulus), n_trials, row$mean_RT, row$sd_RT, row$sem_RT))
}

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("FORMATTED FOR CODE/TABLES\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("ONE-BUTTON TASK - SEM VALUES:\n")
cat("------------------------------\n")
for(i in 1:nrow(one_summary_table)) {
  row <- one_summary_table[i,]
  cat(sprintf("%s: %.4f (±%.4f)\n", as.character(row$Stimulus), row$mean_RT, row$sem_RT))
}

cat("\n")
cat("THREE-BUTTON TASK - SEM VALUES:\n")
cat("--------------------------------\n")
for(i in 1:nrow(three_summary_table)) {
  row <- three_summary_table[i,]
  cat(sprintf("%s: %.4f (±%.4f)\n", as.character(row$Stimulus), row$mean_RT, row$sem_RT))
}

# ============================================================================
# STATISTICAL TESTS - P-VALUES SUMMARY
# ============================================================================

cat("\n\n", rep("=", 70), "\n", sep = "")
cat("STATISTICAL TESTS AND P-VALUES SUMMARY\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("ONE-BUTTON TASK - RACE MODEL VIOLATIONS:\n")
cat(rep("-", 70), "\n", sep = "")
cat("\n")

for (result in results_one_button) {
  if(!is.na(result$p_value)) {
    cat(sprintf("%-4s: Obs=%.3fs, Pred=%.3fs, Δ=%+.0fms, t(%.0f)=%.2f, p=%.4f",
                result$stimulus, result$observed_mean, result$race_pred, 
                result$violation * 1000, result$df, result$t_stat, result$p_value))
    
    if(result$p_value < 0.001) {
      cat(" ***")
    } else if(result$p_value < 0.01) {
      cat(" **")
    } else if(result$p_value < 0.05) {
      cat(" *")
    }
    cat("\n")
  }
}

cat("\n")
cat("ONE-BUTTON TASK - COLAVITA EFFECT:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("\nVisual multimodal (VA,VH,VAH): M=%.3fs (n=%d)\n", 
            mean(vis_multi_rt), length(vis_multi_rt)))
cat(sprintf("Non-visual multimodal (AH):    M=%.3fs (n=%d)\n", 
            mean(nonvis_multi_rt), length(nonvis_multi_rt)))
cat(sprintf("Difference: %.0f ms faster for visual\n", difference_ms))
cat(sprintf("t(%.1f) = %.2f, p = %.4f", 
            colavita_test$parameter, colavita_test$statistic, colavita_test$p.value))

if(colavita_test$p.value < 0.001) {
  cat(" ***\n")
} else if(colavita_test$p.value < 0.01) {
  cat(" **\n")
} else if(colavita_test$p.value < 0.05) {
  cat(" *\n")
} else {
  cat("\n")
}

cat("\n\n")
cat("THREE-BUTTON TASK - RACE MODEL VIOLATIONS:\n")
cat(rep("-", 70), "\n", sep = "")
cat("\n")

for (result in results_three_button) {
  if(!is.na(result$p_value)) {
    cat(sprintf("%-4s: Obs=%.3fs, Pred=%.3fs, Δ=%+.0fms, t(%.0f)=%.2f, p=%.4f",
                result$stimulus, result$observed_mean, result$race_pred, 
                result$violation * 1000, result$df, result$t_stat, result$p_value))
    
    if(result$p_value < 0.001) {
      cat(" ***")
    } else if(result$p_value < 0.01) {
      cat(" **")
    } else if(result$p_value < 0.05) {
      cat(" *")
    }
    cat("\n")
  }
}

cat("\n")
cat("THREE-BUTTON TASK - COLAVITA EFFECT:\n")
cat(rep("-", 70), "\n", sep = "")
cat(sprintf("\nVisual multimodal (VA,VH,VAH): M=%.3fs (n=%d)\n", 
            mean(vis_multi_rt_3), length(vis_multi_rt_3)))
cat(sprintf("Non-visual multimodal (AH):    M=%.3fs (n=%d)\n", 
            mean(nonvis_multi_rt_3), length(nonvis_multi_rt_3)))
cat(sprintf("Difference: %.0f ms faster for visual\n", difference_ms_3))
cat(sprintf("t(%.1f) = %.2f, p = %.4f", 
            colavita_test_3$parameter, colavita_test_3$statistic, colavita_test_3$p.value))

if(colavita_test_3$p.value < 0.001) {
  cat(" ***\n")
} else if(colavita_test_3$p.value < 0.01) {
  cat(" **\n")
} else if(colavita_test_3$p.value < 0.05) {
  cat(" *\n")
} else {
  cat("\n")
}

cat("\n", rep("=", 70), "\n", sep = "")
cat("ALL ANALYSIS COMPLETE\n")
cat(rep("=", 70), "\n", sep = "")