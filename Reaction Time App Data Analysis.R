# ============================================================================
# Create Figures with Statistical Tests - SUBJECT-LEVEL ANALYSIS
# Major Changes:
# - ALL means and SEMs calculated from SUBJECT-LEVEL averages (n=3)
# - ALL t-tests use subject-level data
# - Figures 1 & 2: Show individual subject dots with DIFFERENTIAL SHADING BY SUBJECT
# - Each subject has a distinct color for easy identification
# - ERROR BARS NOW MORE VISIBLE (width=0.4, linewidth=1)
# - LARGER FONT SIZES for all figures (updated)
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
one_button_1 <- read_csv("one_button_reaction_data_1.csv") %>% mutate(Subject = "S1")
one_button_2 <- read_csv("one_button_reaction_data_2.csv") %>% mutate(Subject = "S2")
one_button_3 <- read_csv("one_button_reaction_data_3.csv") %>% mutate(Subject = "S3")

# Load three-button data
three_button_1 <- read_csv("three_button_reaction_data_1.csv") %>% mutate(Subject = "S1")
three_button_2 <- read_csv("three_button_reaction_data_2.csv") %>% mutate(Subject = "S2")
three_button_3 <- read_csv("three_button_reaction_data_3.csv") %>% mutate(Subject = "S3")

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
# STEP 2: Calculate SUBJECT-LEVEL means (one value per subject per condition)
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("CALCULATING SUBJECT-LEVEL MEANS\n")
cat(rep("=", 70), "\n\n", sep = "")

# ONE-BUTTON TASK: Subject-level means for ALL data
one_subject_means_all <- one_button %>%
  group_by(Subject, Stimulus) %>%
  summarise(
    mean_RT = mean(ReactionTime_seconds, na.rm = TRUE),
    .groups = 'drop'
  )

cat("ONE-BUTTON (ALL trials) - Subject-level means:\n")
print(one_subject_means_all)

# ONE-BUTTON TASK: Subject-level means for CORRECT data only
one_subject_means_correct <- one_correct %>%
  group_by(Subject, Stimulus) %>%
  summarise(
    mean_RT = mean(ReactionTime_seconds, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nONE-BUTTON (CORRECT only) - Subject-level means:\n")
print(one_subject_means_correct)

# THREE-BUTTON TASK: Subject-level means for ALL data
three_subject_means_all <- three_button %>%
  group_by(Subject, Stimulus) %>%
  summarise(
    mean_RT = mean(RT1_seconds, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nTHREE-BUTTON (ALL trials) - Subject-level means:\n")
print(three_subject_means_all)

# THREE-BUTTON TASK: Subject-level means for CORRECT data only
three_subject_means_correct <- three_correct %>%
  group_by(Subject, Stimulus) %>%
  summarise(
    mean_RT = mean(RT1_seconds, na.rm = TRUE),
    .groups = 'drop'
  )

cat("\nTHREE-BUTTON (CORRECT only) - Subject-level means:\n")
print(three_subject_means_correct)

# ============================================================================
# STEP 3: Calculate GRAND means and SEMs across subjects (n=3)
# ============================================================================

# Grand means for one-button (ALL data) - for figures
one_grand_summary_all <- one_subject_means_all %>%
  group_by(Stimulus) %>%
  summarise(
    n_subjects = n(),
    sd_RT = ifelse(n() >= 2, sd(mean_RT, na.rm = TRUE), 0),
    sem_RT = ifelse(n() >= 2, sd(mean_RT, na.rm = TRUE) / sqrt(n()), 0),
    mean_RT = mean(mean_RT, na.rm = TRUE),
    .groups = 'drop')

# Grand means for one-button (CORRECT data) - for statistics
one_grand_summary_correct <- one_subject_means_correct %>%
  group_by(Stimulus) %>%
  summarise(
    n_subjects = n(),
    mean_RT = mean(mean_RT, na.rm = TRUE),
    sd_RT = ifelse(n() >= 2, sd(mean_RT, na.rm = TRUE), 0),
    sem_RT = ifelse(n() >= 2, sd(mean_RT, na.rm = TRUE) / sqrt(n()), 0),
    .groups = 'drop'
  )

# Grand means for three-button (ALL data) - for figures
three_grand_summary_all <- three_subject_means_all %>%
  group_by(Stimulus) %>%
  summarise(
    n_subjects = n(),
    sd_RT = ifelse(n() >= 2, sd(mean_RT, na.rm = TRUE), 0),
    sem_RT = ifelse(n() >= 2, sd(mean_RT, na.rm = TRUE) / sqrt(n()), 0),
    mean_RT = mean(mean_RT, na.rm = TRUE),
    .groups = 'drop'
  )

# Grand means for three-button (CORRECT data) - for statistics
three_grand_summary_correct <- three_subject_means_correct %>%
  group_by(Stimulus) %>%
  summarise(
    n_subjects = n(),
    mean_RT = mean(mean_RT, na.rm = TRUE),
    sd_RT = ifelse(n() >= 2, sd(mean_RT, na.rm = TRUE), 0),
    sem_RT = ifelse(n() >= 2, sd(mean_RT, na.rm = TRUE) / sqrt(n()), 0),
    .groups = 'drop'
  )

cat("\n", rep("=", 70), "\n", sep = "")
cat("GRAND SUMMARY STATISTICS (calculated from subject means, n=3)\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("ONE-BUTTON (ALL trials):\n")
print(one_grand_summary_all)

cat("\nONE-BUTTON (CORRECT only):\n")
print(one_grand_summary_correct)

cat("\nTHREE-BUTTON (ALL trials):\n")
print(three_grand_summary_all)

cat("\nTHREE-BUTTON (CORRECT only):\n")
print(three_grand_summary_correct)

# ============================================================================
# STEP 4: Calculate Race Model Predictions using SUBJECT-LEVEL means
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("RACE MODEL ANALYSIS - ONE-BUTTON TASK (SUBJECT-LEVEL)\n")
cat(rep("=", 70), "\n\n", sep = "")

# Get unimodal subject-level means (using ALL trials now)
V_subjects <- one_subject_means_all %>% filter(Stimulus == "V") %>% pull(mean_RT)
A_subjects <- one_subject_means_all %>% filter(Stimulus == "A") %>% pull(mean_RT)
H_subjects <- one_subject_means_all %>% filter(Stimulus == "H") %>% pull(mean_RT)

# Calculate race model predictions for each subject
race_predictions_by_subject <- data.frame(
  Subject = c("S1", "S2", "S3"),
  V_mean = V_subjects,
  A_mean = A_subjects,
  H_mean = H_subjects,
  VA_race = pmin(V_subjects, A_subjects),
  VH_race = pmin(V_subjects, H_subjects),
  AH_race = pmin(A_subjects, H_subjects),
  VAH_race = pmin(V_subjects, A_subjects, H_subjects)
)

cat("Subject-level race model predictions:\n")
print(race_predictions_by_subject)

# Grand mean race predictions (average across subjects)
race_predictions <- data.frame(
  Stimulus = c("VA", "VH", "AH", "VAH"),
  RaceModel = c(
    mean(race_predictions_by_subject$VA_race),
    mean(race_predictions_by_subject$VH_race),
    mean(race_predictions_by_subject$AH_race),
    mean(race_predictions_by_subject$VAH_race)
  )
)

cat("\nGrand mean race model predictions:\n")
print(race_predictions)

# ============================================================================
# STEP 5: Statistical Tests using SUBJECT-LEVEL data (n=3)
# ============================================================================

cat("\n", rep("-", 70), "\n", sep = "")
cat("T-TESTS FOR RACE MODEL VIOLATIONS (One-Button Task, ALL trials, n=3 subjects)\n")
cat(rep("-", 70), "\n\n", sep = "")

# Function to perform t-test using subject-level data
test_race_model_subjects <- function(subject_means, stimulus_name, race_pred_vector) {
  # Get subject-level observed means for this condition
  obs_data <- subject_means %>% 
    filter(Stimulus == stimulus_name) %>% 
    pull(mean_RT)
  
  if (length(obs_data) < 2) {
    return(list(
      stimulus = stimulus_name,
      n = length(obs_data),
      observed_mean = mean(obs_data),
      race_pred = mean(race_pred_vector),
      violation = NA,
      t_stat = NA,
      df = NA,
      p_value = NA,
      cohens_d = NA
    ))
  }
  
  # Perform one-sample t-test against mean race prediction
  race_mean <- mean(race_pred_vector)
  test_result <- t.test(obs_data, mu = race_mean)
  
  # Calculate Cohen's d
  cohens_d <- (mean(obs_data) - race_mean) / sd(obs_data)
  
  # Calculate violation (positive = facilitation, negative = interference)
  violation <- race_mean - mean(obs_data)
  
  return(list(
    stimulus = stimulus_name,
    n = length(obs_data),
    observed_mean = mean(obs_data),
    race_pred = race_mean,
    violation = violation,
    t_stat = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    cohens_d = cohens_d
  ))
}

# Test each multimodal condition
results_one_button <- list()

results_one_button$VA <- test_race_model_subjects(
  one_subject_means_all, "VA", race_predictions_by_subject$VA_race)
results_one_button$VH <- test_race_model_subjects(
  one_subject_means_all, "VH", race_predictions_by_subject$VH_race)
results_one_button$AH <- test_race_model_subjects(
  one_subject_means_all, "AH", race_predictions_by_subject$AH_race)
results_one_button$VAH <- test_race_model_subjects(
  one_subject_means_all, "VAH", race_predictions_by_subject$VAH_race)

# Print results
for (result in results_one_button) {
  cat(sprintf("\n%s Condition:\n", result$stimulus))
  cat(sprintf("  Observed RT:     %.3f s (n=%d subjects)\n", result$observed_mean, result$n))
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
# COLAVITA EFFECT ANALYSIS - ONE-BUTTON TASK (SUBJECT-LEVEL)
# ============================================================================

cat("\n", rep("-", 70), "\n", sep = "")
cat("COLAVITA VISUAL DOMINANCE EFFECT (One-Button Task, ALL trials, n=3 subjects)\n")
cat(rep("-", 70), "\n\n", sep = "")

# Calculate subject-level means for visual multimodal (VA, VH, VAH) - using ALL trials
vis_multi_subject <- one_subject_means_all %>%
  filter(Stimulus %in% c("VA", "VH", "VAH")) %>%
  group_by(Subject) %>%
  summarise(mean_RT = mean(mean_RT), .groups = 'drop') %>%
  pull(mean_RT)

# Calculate subject-level means for non-visual multimodal (AH) - using ALL trials
nonvis_multi_subject <- one_subject_means_all %>%
  filter(Stimulus == "AH") %>%
  pull(mean_RT)

cat(sprintf("Visual Multimodal (VA, VH, VAH):\n"))
cat(sprintf("  Mean RT: %.3f s (n=%d subjects)\n", mean(vis_multi_subject), length(vis_multi_subject)))
cat(sprintf("  SD:      %.3f s\n", sd(vis_multi_subject)))

cat(sprintf("\nNon-Visual Multimodal (AH):\n"))
cat(sprintf("  Mean RT: %.3f s (n=%d subjects)\n", mean(nonvis_multi_subject), length(nonvis_multi_subject)))
cat(sprintf("  SD:      %.3f s\n", sd(nonvis_multi_subject)))

# Independent samples t-test
colavita_test <- t.test(vis_multi_subject, nonvis_multi_subject)

# Cohen's d for independent samples
pooled_sd <- sqrt(((length(vis_multi_subject)-1)*var(vis_multi_subject) + 
                     (length(nonvis_multi_subject)-1)*var(nonvis_multi_subject)) / 
                    (length(vis_multi_subject)+length(nonvis_multi_subject)-2))
colavita_d <- (mean(vis_multi_subject) - mean(nonvis_multi_subject)) / pooled_sd

difference_ms <- (mean(nonvis_multi_subject) - mean(vis_multi_subject)) * 1000
percent_diff <- ((mean(nonvis_multi_subject) - mean(vis_multi_subject)) / mean(vis_multi_subject)) * 100

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
# THREE-BUTTON TASK STATISTICS (SUBJECT-LEVEL)
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("RACE MODEL ANALYSIS - THREE-BUTTON TASK (SUBJECT-LEVEL)\n")
cat(rep("=", 70), "\n\n", sep = "")

# Get unimodal subject-level means (using ALL trials now)
V_subjects_3 <- three_subject_means_all %>% filter(Stimulus == "V") %>% pull(mean_RT)
A_subjects_3 <- three_subject_means_all %>% filter(Stimulus == "A") %>% pull(mean_RT)
H_subjects_3 <- three_subject_means_all %>% filter(Stimulus == "H") %>% pull(mean_RT)

# Calculate race model predictions for each subject
race_predictions_by_subject_3 <- data.frame(
  Subject = c("S1", "S2", "S3"),
  V_mean = V_subjects_3,
  A_mean = A_subjects_3,
  H_mean = H_subjects_3,
  VA_race = pmin(V_subjects_3, A_subjects_3),
  VH_race = pmin(V_subjects_3, H_subjects_3),
  AH_race = pmin(A_subjects_3, H_subjects_3),
  VAH_race = pmin(V_subjects_3, A_subjects_3, H_subjects_3)
)

cat("Subject-level race model predictions:\n")
print(race_predictions_by_subject_3)

# Grand mean race predictions
race_predictions_3 <- data.frame(
  Stimulus = c("VA", "VH", "AH", "VAH"),
  RaceModel = c(
    mean(race_predictions_by_subject_3$VA_race),
    mean(race_predictions_by_subject_3$VH_race),
    mean(race_predictions_by_subject_3$AH_race),
    mean(race_predictions_by_subject_3$VAH_race)
  )
)

cat("\nGrand mean race model predictions:\n")
print(race_predictions_3)

cat("\n", rep("-", 70), "\n", sep = "")
cat("T-TESTS FOR RACE MODEL VIOLATIONS (Three-Button Task, ALL trials, n=3 subjects)\n")
cat(rep("-", 70), "\n\n", sep = "")

# Test three-button task
results_three_button <- list()

results_three_button$VA <- test_race_model_subjects(
  three_subject_means_all, "VA", race_predictions_by_subject_3$VA_race)
results_three_button$VH <- test_race_model_subjects(
  three_subject_means_all, "VH", race_predictions_by_subject_3$VH_race)
results_three_button$AH <- test_race_model_subjects(
  three_subject_means_all, "AH", race_predictions_by_subject_3$AH_race)
results_three_button$VAH <- test_race_model_subjects(
  three_subject_means_all, "VAH", race_predictions_by_subject_3$VAH_race)

for (result in results_three_button) {
  cat(sprintf("\n%s Condition:\n", result$stimulus))
  cat(sprintf("  Observed RT:     %.3f s (n=%d subjects)\n", result$observed_mean, result$n))
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

# Colavita effect - three-button (SUBJECT-LEVEL)
cat("\n", rep("-", 70), "\n", sep = "")
cat("COLAVITA VISUAL DOMINANCE EFFECT (Three-Button Task, ALL trials, n=3 subjects)\n")
cat(rep("-", 70), "\n\n", sep = "")

# Calculate subject-level means for visual multimodal - using ALL trials
vis_multi_subject_3 <- three_subject_means_all %>%
  filter(Stimulus %in% c("VA", "VH", "VAH")) %>%
  group_by(Subject) %>%
  summarise(mean_RT = mean(mean_RT), .groups = 'drop') %>%
  pull(mean_RT)

# Calculate subject-level means for non-visual multimodal - using ALL trials
nonvis_multi_subject_3 <- three_subject_means_all %>%
  filter(Stimulus == "AH") %>%
  pull(mean_RT)

cat(sprintf("Visual Multimodal (VA, VH, VAH):\n"))
cat(sprintf("  Mean RT: %.3f s (n=%d subjects)\n", mean(vis_multi_subject_3), length(vis_multi_subject_3)))
cat(sprintf("  SD:      %.3f s\n", sd(vis_multi_subject_3)))

cat(sprintf("\nNon-Visual Multimodal (AH):\n"))
cat(sprintf("  Mean RT: %.3f s (n=%d subjects)\n", mean(nonvis_multi_subject_3), length(nonvis_multi_subject_3)))
cat(sprintf("  SD:      %.3f s\n", sd(nonvis_multi_subject_3)))

colavita_test_3 <- t.test(vis_multi_subject_3, nonvis_multi_subject_3)

pooled_sd_3 <- sqrt(((length(vis_multi_subject_3)-1)*var(vis_multi_subject_3) + 
                       (length(nonvis_multi_subject_3)-1)*var(nonvis_multi_subject_3)) / 
                      (length(vis_multi_subject_3)+length(nonvis_multi_subject_3)-2))
colavita_d_3 <- (mean(vis_multi_subject_3) - mean(nonvis_multi_subject_3)) / pooled_sd_3

difference_ms_3 <- (mean(nonvis_multi_subject_3) - mean(vis_multi_subject_3)) * 1000
percent_diff_3 <- ((mean(nonvis_multi_subject_3) - mean(vis_multi_subject_3)) / mean(vis_multi_subject_3)) * 100

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
# FIGURE 1: ONE-BUTTON TASK with SUBJECT-SPECIFIC COLORS - LARGER FONTS
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("CREATING FIGURES WITH SUBJECT-SPECIFIC COLORS AND LARGER FONTS\n")
cat(rep("=", 70), "\n\n", sep = "")

# Define subject colors - distinct and visually appealing
subject_colors <- c("S1" = "#08d8a2",   
                    "S2" = "#06a77d",   
                    "S3" = "#047658")   

# Prepare data for plotting
one_grand_summary_all$visual_containing <- ifelse(grepl("V", one_grand_summary_all$Stimulus), 
                                                  "Visual", "Non-Visual")
one_grand_summary_all$Stimulus <- factor(one_grand_summary_all$Stimulus, 
                                         levels = c("V", "A", "H", "VA", "VH", "AH", "VAH"))
one_grand_summary_all <- merge(one_grand_summary_all, race_predictions, by = "Stimulus", all.x = TRUE)

# Prepare individual subject data
one_subject_means_all$Stimulus <- factor(one_subject_means_all$Stimulus, 
                                         levels = c("V", "A", "H", "VA", "VH", "AH", "VAH"))

# Prepare race model data
race_model_data_1 <- one_grand_summary_all[!is.na(one_grand_summary_all$RaceModel), ]
race_model_data_1$x_start <- as.numeric(race_model_data_1$Stimulus) - 0.4
race_model_data_1$x_end <- as.numeric(race_model_data_1$Stimulus) + 0.4

# Create Figure 1 with SUBJECT-SPECIFIC COLORS and LARGER FONTS
fig1 <- ggplot(one_grand_summary_all, aes(x = Stimulus, y = mean_RT)) +
  # Individual subject points with DISTINCT COLORS
  geom_point(data = one_subject_means_all, 
             aes(x = Stimulus, y = mean_RT, color = Subject),
             size = 5, alpha = 0.8, show.legend = FALSE,
             position = position_jitter(width = 0.15, height = 0, seed = 42)) +
  # Race model predictions
  geom_segment(data = race_model_data_1,
               aes(x = x_start, xend = x_end, y = RaceModel, yend = RaceModel, 
                   linetype = "Race Model"),
               color = "red", linewidth = 2,
               inherit.aes = FALSE) +
  # Mean points (filled) - PLOTTED AFTER INDIVIDUAL POINTS
  geom_point(aes(fill = visual_containing), 
             size = 6, shape = 21, color = "black", stroke = 2) +
  # ERROR BARS - PLOTTED LAST SO THEY APPEAR ON TOP
  geom_errorbar(aes(ymin = mean_RT - sem_RT, ymax = mean_RT + sem_RT),
                width = 0.35, linewidth = 1.2, color = "black") +
  scale_color_manual(values = subject_colors,
                     name = "Subject",
                     labels = c("S1" = "S1", "S2" = "S2", "S3" = "S3")) +
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
  theme_classic(base_size = 24) +
  theme(
    plot.title = element_text(face = "bold", size = 32, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 28),
    axis.text = element_text(size = 24),
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    legend.position = "top",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 0.5, linetype = 1))

ggsave("Figure1_OneButton_MeanRT_SubjectColors.png", fig1, width = 12, height = 8, dpi = 300)
cat("✓ Figure 1 saved: Figure1_OneButton_MeanRT_SubjectColors.png\n")

# ============================================================================
# FIGURE 2: THREE-BUTTON TASK with SUBJECT-SPECIFIC COLORS - LARGER FONTS
# ============================================================================

# Prepare data for plotting
three_grand_summary_all$visual_containing <- ifelse(grepl("V", three_grand_summary_all$Stimulus), 
                                                    "Visual", "Non-Visual")
three_grand_summary_all$Stimulus <- factor(three_grand_summary_all$Stimulus, 
                                           levels = c("V", "A", "H", "VA", "VH", "AH", "VAH"))
three_grand_summary_all <- merge(three_grand_summary_all, race_predictions_3, by = "Stimulus", all.x = TRUE)

# Prepare individual subject data
three_subject_means_all$Stimulus <- factor(three_subject_means_all$Stimulus, 
                                           levels = c("V", "A", "H", "VA", "VH", "AH", "VAH"))

# Prepare race model data
race_model_data_3 <- three_grand_summary_all[!is.na(three_grand_summary_all$RaceModel), ]
race_model_data_3$x_start <- as.numeric(race_model_data_3$Stimulus) - 0.4
race_model_data_3$x_end <- as.numeric(race_model_data_3$Stimulus) + 0.4

# Create Figure 2 with SUBJECT-SPECIFIC COLORS and LARGER FONTS
fig2 <- ggplot(three_grand_summary_all, aes(x = Stimulus, y = mean_RT)) +
  # Individual subject points with DISTINCT COLORS
  geom_point(data = three_subject_means_all, 
             aes(x = Stimulus, y = mean_RT, color = Subject),
             size = 5, alpha = 0.8, show.legend = FALSE,
             position = position_jitter(width = 0.15, height = 0, seed = 42)) +
  # Mean points (filled) - PLOTTED AFTER INDIVIDUAL POINTS
  geom_point(aes(fill = visual_containing), 
             size = 6, shape = 21, color = "black", stroke = 2) +
  # ERROR BARS - PLOTTED LAST SO THEY APPEAR ON TOP
  geom_errorbar(aes(ymin = mean_RT - sem_RT, ymax = mean_RT + sem_RT),
                width = 0.35, linewidth = 1.2, color = "black") +
  scale_color_manual(values = subject_colors,
                     name = "Subject",
                     labels = c("S1" = "S1", "S2" = "S2", "S3" = "S3")) +
  scale_fill_manual(values = c("Visual" = "#2E86AB", "Non-Visual" = "#A23B72"),
                    name = "",
                    labels = c("Non-Visual" = "Non-Visual", "Visual" = "Visual")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.4)) +
  labs(
    title = "Three-Button Task",
    x = "Stimulus Type",
    y = "Reaction Time (s)"
  ) +
  theme_classic(base_size = 24) +
  theme(
    plot.title = element_text(face = "bold", size = 32, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 28),
    axis.text = element_text(size = 24),
    axis.text.x = element_text(size = 24),
    axis.text.y = element_text(size = 24),
    legend.position = "top",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22, face = "bold"),
    axis.line = element_line(color = "black", linewidth = 0.5, linetype = 1))

ggsave("Figure2_ThreeButton_MeanRT_SubjectColors.png", fig2, width = 12, height = 8, dpi = 300)
cat("✓ Figure 2 saved: Figure2_ThreeButton_MeanRT_SubjectColors.png\n")

# ============================================================================
# FIGURE 3 & 4: CONFUSION MATRICES - LARGER FONTS
# ============================================================================

# Function to normalize stimulus order
normalize_stimulus <- function(stim) {
  if(is.null(stim) || length(stim) == 0) return(NA_character_)
  if(is.na(stim)) return(NA_character_)
  stim <- as.character(stim)
  if(stim == "" || nchar(stim) == 0) return(NA_character_)
  letters <- strsplit(stim, "")[[1]]
  order_map <- c("V" = 1, "A" = 2, "H" = 3)
  sorted_letters <- letters[order(sapply(letters, function(x) order_map[x]))]
  return(paste(sorted_letters, collapse = ""))
}

# FIGURE 3: One-button confusion matrix (SUBJECT-LEVEL) - LARGER FONTS
cat("\n", rep("=", 70), "\n", sep = "")
cat("CREATING CONFUSION MATRIX FOR ONE-BUTTON TASK (SUBJECT-LEVEL)\n")
cat(rep("=", 70), "\n\n", sep = "")

felt_col <- "SelectedCues"
one_button_norm <- one_button %>%
  mutate(NormalizedResponse = sapply(!!sym(felt_col), normalize_stimulus))

# Calculate confusion matrix for EACH SUBJECT
confusion_one_by_subject <- one_button_norm %>%
  group_by(Subject, Stimulus, NormalizedResponse) %>%
  summarise(count = n(), .groups = 'drop') %>%
  rename(FeltStimulus = NormalizedResponse)

# Get totals for each subject and stimulus
totals_one_by_subject <- one_button_norm %>%
  group_by(Subject, Stimulus) %>%
  summarise(total = n(), .groups = 'drop')

# Calculate proportions for each subject
confusion_one_by_subject <- merge(confusion_one_by_subject, totals_one_by_subject, 
                                  by = c("Subject", "Stimulus"))
confusion_one_by_subject$proportion <- confusion_one_by_subject$count / confusion_one_by_subject$total

all_levels <- c("V", "A", "H", "VA", "VH", "AH", "VAH")
confusion_one_by_subject$Stimulus <- factor(confusion_one_by_subject$Stimulus, levels = all_levels)
confusion_one_by_subject$FeltStimulus <- factor(confusion_one_by_subject$FeltStimulus, levels = all_levels)

# Create complete grid for each subject
complete_grid_subjects <- expand.grid(
  Subject = unique(one_button$Subject),
  Stimulus = factor(all_levels, levels = all_levels),
  FeltStimulus = factor(all_levels, levels = all_levels)
)

# Merge with actual data, filling missing combinations with 0
confusion_one_complete_subjects <- merge(complete_grid_subjects, confusion_one_by_subject, 
                                         by = c("Subject", "Stimulus", "FeltStimulus"), all.x = TRUE)
confusion_one_complete_subjects$proportion[is.na(confusion_one_complete_subjects$proportion)] <- 0

cat("Subject-level confusion matrices (sample):\n")
print(head(confusion_one_complete_subjects, 20))

# Calculate GRAND MEAN proportions across subjects
confusion_one_grand <- confusion_one_complete_subjects %>%
  group_by(Stimulus, FeltStimulus) %>%
  summarise(
    mean_proportion = mean(proportion),
    sd_proportion = sd(proportion),
    sem_proportion = sd(proportion) / sqrt(n()),
    n_subjects = n(),
    .groups = 'drop'
  )

cat("\nGrand mean confusion matrix:\n")
print(confusion_one_grand)

fig3 <- ggplot(confusion_one_grand, aes(x = Stimulus, y = FeltStimulus, fill = mean_proportion)) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", mean_proportion)), 
            color = ifelse(confusion_one_grand$mean_proportion > 0.5, "white", "black"), 
            size = 9, fontface = "bold") +
  scale_fill_gradient(low = "#E8F4F8", high = "#2E86AB", limits = c(0, 1),
                      name = "Proportion") +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "One-Button Task",
    x = "Actual Stimulus",
    y = "Perceived Stimulus"
  ) +
  theme_minimal(base_size = 24) +
  theme(
    plot.title = element_text(face = "bold", size = 32, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 28),
    axis.text = element_text(size = 24),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

ggsave("Figure3_OneButton_ConfusionMatrix_SubjectLevel.png", fig3, width = 12, height = 10, dpi = 300)
cat("✓ Figure 3 saved: Figure3_OneButton_ConfusionMatrix_SubjectLevel.png\n")

# FIGURE 4: Three-button confusion matrix (SUBJECT-LEVEL) - LARGER FONTS
cat("\n", rep("=", 70), "\n", sep = "")
cat("CREATING CONFUSION MATRIX FOR THREE-BUTTON TASK (SUBJECT-LEVEL)\n")
cat(rep("=", 70), "\n\n", sep = "")

felt_col_3 <- "KeysPressed"

map_keys_to_stimulus <- function(key_num) {
  if(is.na(key_num)) return(NA_character_)
  key_str <- as.character(key_num)
  key_str <- gsub("1", "V", key_str)
  key_str <- gsub("2", "A", key_str)
  key_str <- gsub("3", "H", key_str)
  return(key_str)
}

three_button_norm <- three_button %>%
  filter(!is.na(!!sym(felt_col_3))) %>%
  mutate(
    KeysPressed_mapped = sapply(!!sym(felt_col_3), map_keys_to_stimulus),
    NormalizedResponse = sapply(KeysPressed_mapped, normalize_stimulus)
  )

# Calculate confusion matrix for EACH SUBJECT
confusion_three_by_subject <- three_button_norm %>%
  group_by(Subject, Stimulus, NormalizedResponse) %>%
  summarise(count = n(), .groups = 'drop') %>%
  rename(FeltStimulus = NormalizedResponse)

# Get totals for each subject and stimulus
totals_three_by_subject <- three_button_norm %>%
  group_by(Subject, Stimulus) %>%
  summarise(total = n(), .groups = 'drop')

# Calculate proportions for each subject
confusion_three_by_subject <- merge(confusion_three_by_subject, totals_three_by_subject, 
                                    by = c("Subject", "Stimulus"))
confusion_three_by_subject$proportion <- confusion_three_by_subject$count / confusion_three_by_subject$total

confusion_three_by_subject$Stimulus <- factor(confusion_three_by_subject$Stimulus, levels = all_levels)
confusion_three_by_subject$FeltStimulus <- factor(confusion_three_by_subject$FeltStimulus, levels = all_levels)

# Create complete grid for each subject
complete_grid_subjects_3 <- expand.grid(
  Subject = unique(three_button$Subject),
  Stimulus = factor(all_levels, levels = all_levels),
  FeltStimulus = factor(all_levels, levels = all_levels)
)

# Merge with actual data, filling missing combinations with 0
confusion_three_complete_subjects <- merge(complete_grid_subjects_3, confusion_three_by_subject, 
                                           by = c("Subject", "Stimulus", "FeltStimulus"), all.x = TRUE)
confusion_three_complete_subjects$proportion[is.na(confusion_three_complete_subjects$proportion)] <- 0

cat("Subject-level confusion matrices (sample):\n")
print(head(confusion_three_complete_subjects, 20))

# Calculate GRAND MEAN proportions across subjects
confusion_three_grand <- confusion_three_complete_subjects %>%
  group_by(Stimulus, FeltStimulus) %>%
  summarise(
    mean_proportion = mean(proportion),
    sd_proportion = sd(proportion),
    sem_proportion = sd(proportion) / sqrt(n()),
    n_subjects = n(),
    .groups = 'drop'
  )

cat("\nGrand mean confusion matrix:\n")
print(confusion_three_grand)

fig4 <- ggplot(confusion_three_grand, aes(x = Stimulus, y = FeltStimulus, fill = mean_proportion)) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", mean_proportion)), 
            color = ifelse(confusion_three_grand$mean_proportion > 0.5, "white", "black"), 
            size = 9, fontface = "bold") +
  scale_fill_gradient(low = "#E8F4F8", high = "#2E86AB", limits = c(0, 1),
                      name = "Proportion") +
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  labs(
    title = "Three-Button Task",
    x = "Actual Stimulus",
    y = "Perceived Stimulus"
  ) +
  theme_minimal(base_size = 24) +
  theme(
    plot.title = element_text(face = "bold", size = 32, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 28),
    axis.text = element_text(size = 24),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

ggsave("Figure4_ThreeButton_ConfusionMatrix_SubjectLevel.png", fig4, width = 12, height = 10, dpi = 300)
cat("✓ Figure 4 saved: Figure4_ThreeButton_ConfusionMatrix_SubjectLevel.png\n")

# ============================================================================
# ACCURACY STATISTICS (SUBJECT-LEVEL)
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("ACCURACY STATISTICS (SUBJECT-LEVEL, n=3)\n")
cat(rep("=", 70), "\n\n", sep = "")

# Calculate subject-level accuracy for one-button task
one_accuracy_subjects <- one_button %>%
  group_by(Subject, Stimulus) %>%
  summarise(
    total_trials = n(),
    correct_trials = sum(FeltCorrectly == "YES"),
    accuracy_pct = (correct_trials / total_trials) * 100,
    .groups = 'drop'
  )

cat("ONE-BUTTON TASK - Subject-level accuracy:\n")
print(one_accuracy_subjects)

# Calculate grand mean accuracy across subjects
one_accuracy_grand <- one_accuracy_subjects %>%
  group_by(Stimulus) %>%
  summarise(
    n_subjects = n(),
    mean_accuracy = mean(accuracy_pct),
    sd_accuracy = sd(accuracy_pct),
    sem_accuracy = sd(accuracy_pct) / sqrt(n()),
    .groups = 'drop'
  )

cat("\nONE-BUTTON TASK - Grand mean accuracy (across subjects):\n")
print(one_accuracy_grand)

# Calculate subject-level accuracy for three-button task
three_accuracy_subjects <- three_button %>%
  group_by(Subject, Stimulus) %>%
  summarise(
    total_trials = n(),
    correct_trials = sum(FeltCorrectly == "YES"),
    accuracy_pct = (correct_trials / total_trials) * 100,
    .groups = 'drop'
  )

cat("\nTHREE-BUTTON TASK - Subject-level accuracy:\n")
print(three_accuracy_subjects)

# Calculate grand mean accuracy across subjects
three_accuracy_grand <- three_accuracy_subjects %>%
  group_by(Stimulus) %>%
  summarise(
    n_subjects = n(),
    mean_accuracy = mean(accuracy_pct),
    sd_accuracy = sd(accuracy_pct),
    sem_accuracy = sd(accuracy_pct) / sqrt(n()),
    .groups = 'drop'
  )

cat("\nTHREE-BUTTON TASK - Grand mean accuracy (across subjects):\n")
print(three_accuracy_grand)

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n", rep("=", 70), "\n", sep = "")
cat("FINAL SUMMARY (SUBJECT-LEVEL ANALYSIS, n=3)\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("ONE-BUTTON TASK (Detection):\n")
cat(sprintf("  Visual Multimodal:     %.3f ± %.3f s\n", 
            mean(vis_multi_subject), sd(vis_multi_subject)/sqrt(length(vis_multi_subject))))
cat(sprintf("  Non-Visual Multimodal: %.3f ± %.3f s\n", 
            mean(nonvis_multi_subject), sd(nonvis_multi_subject)/sqrt(length(nonvis_multi_subject))))
cat(sprintf("  Colavita Effect:       %.0f ms (%.1f%% faster), p = %.4f\n\n", 
            difference_ms, percent_diff, colavita_test$p.value))

cat("THREE-BUTTON TASK (Identification):\n")
cat(sprintf("  Visual Multimodal:     %.3f ± %.3f s\n", 
            mean(vis_multi_subject_3), sd(vis_multi_subject_3)/sqrt(length(vis_multi_subject_3))))
cat(sprintf("  Non-Visual Multimodal: %.3f ± %.3f s\n", 
            mean(nonvis_multi_subject_3), sd(nonvis_multi_subject_3)/sqrt(length(nonvis_multi_subject_3))))
cat(sprintf("  Colavita Effect:       %.0f ms (%.1f%% faster), p = %.4f\n\n", 
            difference_ms_3, percent_diff_3, colavita_test_3$p.value))

cat("=== ALL ANALYSIS COMPLETED WITH SUBJECT-LEVEL STATISTICS ===\n")
cat("\nKey Changes in This Version:\n")
cat("  1. All means/SEMs calculated from subject averages (n=3)\n")
cat("  2. All t-tests use subject-level data from ALL TRIALS (not just correct)\n")
cat("  3. LARGER FONT SIZES for all figures:\n")
cat("     - Base font size increased from 18 to 24\n")
cat("     - Plot titles: 32pt (was 22pt)\n")
cat("     - Axis titles: 28pt (was 20pt)\n")
cat("     - Axis text: 24pt (was 18pt)\n")
cat("     - Legend text: 22pt (was 16pt)\n")
cat("     - Confusion matrix text: 9pt (was 7pt)\n")
cat("  4. Slightly larger point sizes and error bars\n")
cat("  5. Increased figure dimensions for better readability\n")
cat("  6. Subject-specific colors maintained\n")
cat("  7. All statistical analyses unchanged\n")