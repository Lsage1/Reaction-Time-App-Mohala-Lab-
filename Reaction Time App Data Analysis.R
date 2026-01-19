# ============================================================================
# Create Figures - CORRECTED VERSION (Race model lines properly centered)
# Works with: dplyr, readr, ggplot2 only
# ============================================================================

# Load only what we need (these should have installed successfully)
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

# Filter for correct responses only
one_correct <- one_button[one_button$FeltCorrectly == "YES", ]
three_correct <- three_button[three_button$FeltCorrectly == "YES", ]

cat("One-button correct trials:", nrow(one_correct), "\n")
cat("Three-button correct trials:", nrow(three_correct), "\n")

# ============================================================================
# STEP 2: Calculate Race Model Predictions
# ============================================================================

# Get unimodal mean RTs (one-button task)
V_mean <- mean(one_correct$ReactionTime_seconds[one_correct$Stimulus == "V"])
A_mean <- mean(one_correct$ReactionTime_seconds[one_correct$Stimulus == "A"])
H_mean <- mean(one_correct$ReactionTime_seconds[one_correct$Stimulus == "H"])

# Print them
cat("\n=== RACE MODEL PREDICTIONS (One-Button Task) ===\n")
cat(sprintf("V alone: %.3f s\n", V_mean))
cat(sprintf("A alone: %.3f s\n", A_mean))
cat(sprintf("H alone: %.3f s\n", H_mean))

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

print(race_predictions)

# Get unimodal mean RTs (three-button task)
V_mean_3 <- mean(three_correct$RT1_seconds[three_correct$Stimulus == "V"], na.rm = TRUE)
A_mean_3 <- mean(three_correct$RT1_seconds[three_correct$Stimulus == "A"], na.rm = TRUE)
H_mean_3 <- mean(three_correct$RT1_seconds[three_correct$Stimulus == "H"], na.rm = TRUE)

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

# ============================================================================
# FIGURE 1: One-Button Task - Mean RT by Stimulus Type
# ============================================================================

# Calculate summary statistics
one_summary <- one_correct %>%
  group_by(Stimulus) %>%
  summarise(
    mean_RT = mean(ReactionTime_seconds),
    sem_RT = sd(ReactionTime_seconds) / sqrt(n()),
    .groups = 'drop'
  )

# Add visual-containing indicator
one_summary$visual_containing <- ifelse(grepl("V", one_summary$Stimulus), "Visual", "Non-Visual")

# Order stimuli
one_summary$Stimulus <- factor(one_summary$Stimulus, levels = c("V", "A", "H", "VA", "VH", "AH", "VAH"))

# Add race model predictions
one_summary <- merge(one_summary, race_predictions, by = "Stimulus", all.x = TRUE)

# Prepare race model data with proper x positions
race_model_data_1 <- one_summary[!is.na(one_summary$RaceModel), ]
race_model_data_1$x_start <- as.numeric(race_model_data_1$Stimulus) - 0.4
race_model_data_1$x_end <- as.numeric(race_model_data_1$Stimulus) + 0.4

# Create Figure 1 with CORRECTED race model lines
fig1 <- ggplot(one_summary, aes(x = Stimulus, y = mean_RT, fill = visual_containing)) +
  geom_bar(stat = "identity", color = "black", size = 1.2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_RT - sem_RT, ymax = mean_RT + sem_RT),
                width = 0.3, size = 1) +
  # Add race model lines for multimodal conditions - PROPERLY CENTERED
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

# Save Figure 1
ggsave("Figure1_OneButton_MeanRT.png", fig1, width = 10, height = 6, dpi = 300)
cat("\n✓ Figure 1 saved: Figure1_OneButton_MeanRT.png\n")

# ============================================================================
# FIGURE 2: Three-Button Task - Mean RT by Stimulus Type
# ============================================================================

# Calculate summary statistics
three_summary <- three_correct %>%
  group_by(Stimulus) %>%
  summarise(
    mean_RT = mean(RT1_seconds, na.rm = TRUE),
    sem_RT = sd(RT1_seconds, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

three_summary$visual_containing <- ifelse(grepl("V", three_summary$Stimulus), "Visual", "Non-Visual")
three_summary$Stimulus <- factor(three_summary$Stimulus, levels = c("V", "A", "H", "VA", "VH", "AH", "VAH"))
three_summary <- merge(three_summary, race_predictions_3, by = "Stimulus", all.x = TRUE)

# Prepare race model data with proper x positions
race_model_data_3 <- three_summary[!is.na(three_summary$RaceModel), ]
race_model_data_3$x_start <- as.numeric(race_model_data_3$Stimulus) - 0.4
race_model_data_3$x_end <- as.numeric(race_model_data_3$Stimulus) + 0.4

# Create Figure 2 with CORRECTED race model lines
fig2 <- ggplot(three_summary, aes(x = Stimulus, y = mean_RT, fill = visual_containing)) +
  geom_bar(stat = "identity", color = "black", size = 1.2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_RT - sem_RT, ymax = mean_RT + sem_RT),
                width = 0.3, size = 1) +
  # Add race model lines for multimodal conditions - PROPERLY CENTERED
  geom_segment(data = race_model_data_3,
               aes(x = x_start, xend = x_end, y = RaceModel, yend = RaceModel, linetype = "Race Model"),
               color = "red", linewidth = 1.5,
               inherit.aes = FALSE) +
  scale_fill_manual(values = c("Visual" = "#2E86AB", "Non-Visual" = "#A23B72"),
                    name = "",
                    labels = c("Non-Visual" = "Non-Visual Modality", "Visual" = "Visual Modality")) +
  scale_linetype_manual(values = c("Race Model" = "dashed"),
                        name = "",
                        labels = c("Race Model Prediction")) +
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

# Save Figure 2
ggsave("Figure2_ThreeButton_MeanRT.png", fig2, width = 10, height = 6, dpi = 300)
cat("✓ Figure 2 saved: Figure2_ThreeButton_MeanRT.png\n")

# ============================================================================
# FIGURE 3: One-Button Task - Visual vs Non-Visual Comparison
# ============================================================================

# Calculate means for visual multimodal
vis_multi_data <- one_correct[one_correct$Stimulus %in% c("VA", "VH", "VAH"), ]
vis_multi_mean <- mean(vis_multi_data$ReactionTime_seconds)
vis_multi_sem <- sd(vis_multi_data$ReactionTime_seconds) / sqrt(nrow(vis_multi_data))

# Calculate means for non-visual multimodal
nonvis_multi_data <- one_correct[one_correct$Stimulus == "AH", ]
nonvis_multi_mean <- mean(nonvis_multi_data$ReactionTime_seconds)
nonvis_multi_sem <- sd(nonvis_multi_data$ReactionTime_seconds) / sqrt(nrow(nonvis_multi_data))

# Create comparison data
comparison_one <- data.frame(
  Category = factor(c("V", "A", "H", "Visual\nMultimodal", "Non-Visual\nMultimodal"),
                    levels = c("V", "A", "H", "Visual\nMultimodal", "Non-Visual\nMultimodal")),
  mean_RT = c(V_mean, A_mean, H_mean, vis_multi_mean, nonvis_multi_mean),
  sem_RT = c(
    sd(one_correct$ReactionTime_seconds[one_correct$Stimulus == "V"]) / sqrt(sum(one_correct$Stimulus == "V")),
    sd(one_correct$ReactionTime_seconds[one_correct$Stimulus == "A"]) / sqrt(sum(one_correct$Stimulus == "A")),
    sd(one_correct$ReactionTime_seconds[one_correct$Stimulus == "H"]) / sqrt(sum(one_correct$Stimulus == "H")),
    vis_multi_sem,
    nonvis_multi_sem
  ),
  Type = factor(c("V", "A", "H", "Visual-Multimodal", "Non-Visual-Multimodal"),
                levels = c("V", "A", "H", "Visual-Multimodal", "Non-Visual-Multimodal"))
)

# Create Figure 3
fig3 <- ggplot(comparison_one, aes(x = Category, y = mean_RT, fill = Type)) +
  geom_bar(stat = "identity", color = "black", size = 1.2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_RT - sem_RT, ymax = mean_RT + sem_RT),
                width = 0.3, size = 1) +
  scale_fill_manual(values = c("V" = "#4CAF50",
                               "A" = "#FF9800", 
                               "H" = "#9C27B0",
                               "Visual-Multimodal" = "#2E86AB",
                               "Non-Visual-Multimodal" = "#A23B72"),
                    labels = c("V" = "V",
                               "A" = "A",
                               "H" = "H",
                               "Visual-Multimodal" = "Visual Multimodal (VA, VH, VAH)",
                               "Non-Visual-Multimodal" = "Non-Visual Multimodal (AH)")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.9)) +
  labs(
    title = "One-Button Task: Colavita Visual Dominance Effect",
    x = "Stimulus Type",
    y = "Reaction Time (Seconds)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    axis.line = element_line(color = "black",
                             linewidth = 0.5,
                             linetype = 1))

# Save Figure 3
ggsave("Figure3_OneButton_Comparison.png", fig3, width = 12, height = 7, dpi = 300)
cat("✓ Figure 3 saved: Figure3_OneButton_Comparison.png\n")

# ============================================================================
# FIGURE 4: Three-Button Task - Visual vs Non-Visual Comparison
# ============================================================================

# Calculate means
vis_multi_data_3 <- three_correct[three_correct$Stimulus %in% c("VA", "VH", "VAH"), ]
vis_multi_mean_3 <- mean(vis_multi_data_3$RT1_seconds, na.rm = TRUE)
vis_multi_sem_3 <- sd(vis_multi_data_3$RT1_seconds, na.rm = TRUE) / sqrt(sum(!is.na(vis_multi_data_3$RT1_seconds)))

nonvis_multi_data_3 <- three_correct[three_correct$Stimulus == "AH", ]
nonvis_multi_mean_3 <- mean(nonvis_multi_data_3$RT1_seconds, na.rm = TRUE)
nonvis_multi_sem_3 <- sd(nonvis_multi_data_3$RT1_seconds, na.rm = TRUE) / sqrt(sum(!is.na(nonvis_multi_data_3$RT1_seconds)))

comparison_three <- data.frame(
  Category = factor(c("V", "A", "H", "Visual\nMultimodal", "Non-Visual\nMultimodal"),
                    levels = c("V", "A", "H", "Visual\nMultimodal", "Non-Visual\nMultimodal")),
  mean_RT = c(V_mean_3, A_mean_3, H_mean_3, vis_multi_mean_3, nonvis_multi_mean_3),
  sem_RT = c(
    sd(three_correct$RT1_seconds[three_correct$Stimulus == "V"], na.rm = TRUE) / sqrt(sum(three_correct$Stimulus == "V")),
    sd(three_correct$RT1_seconds[three_correct$Stimulus == "A"], na.rm = TRUE) / sqrt(sum(three_correct$Stimulus == "A")),
    sd(three_correct$RT1_seconds[three_correct$Stimulus == "H"], na.rm = TRUE) / sqrt(sum(three_correct$Stimulus == "H")),
    vis_multi_sem_3,
    nonvis_multi_sem_3
  ),
  Type = factor(c("V", "A", "H", "Visual-Multimodal", "Non-Visual-Multimodal"),
                levels = c("V", "A", "H", "Visual-Multimodal", "Non-Visual-Multimodal"))
)

pct_diff <- ((comparison_three$mean_RT[5] - comparison_three$mean_RT[4]) / comparison_three$mean_RT[4]) * 100

# Create Figure 4
fig4 <- ggplot(comparison_three, aes(x = Category, y = mean_RT, fill = Type)) +
  geom_bar(stat = "identity", color = "black", size = 1.2, alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_RT - sem_RT, ymax = mean_RT + sem_RT),
                width = 0.3, size = 1) +
  scale_fill_manual(values = c("V" = "#4CAF50",
                               "A" = "#FF9800", 
                               "H" = "#9C27B0",
                               "Visual-Multimodal" = "#2E86AB",
                               "Non-Visual-Multimodal" = "#A23B72"),
                    labels = c("V" = "V",
                               "A" = "A",
                               "H" = "H",
                               "Visual-Multimodal" = "Visual Multimodal (VA, VH, VAH)",
                               "Non-Visual-Multimodal" = "Non-Visual Multimodal (AH)")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5)) +
  labs(
    title = "Three-Button Task: Colavita Visual Dominance Effect",
    x = "Stimulus Type",
    y = "Reaction Time (Seconds)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    axis.line = element_line(color = "black",
                             linewidth = 0.5,
                             linetype = 1))

ggsave("Figure4_ThreeButton_Comparison.png", fig4, width = 12, height = 7, dpi = 300)
cat("✓ Figure 4 saved: Figure4_ThreeButton_Comparison.png\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n=== SUMMARY STATISTICS ===\n")
cat("\nONE-BUTTON TASK:\n")
cat(sprintf("  Visual Multimodal:     %.3f ± %.3f s\n", 
            comparison_one$mean_RT[4], comparison_one$sem_RT[4]))
cat(sprintf("  Non-Visual Multimodal: %.3f ± %.3f s\n",
            comparison_one$mean_RT[5], comparison_one$sem_RT[5]))
cat(sprintf("  Visual dominance: %.1f%% faster\n",
            ((comparison_one$mean_RT[5] - comparison_one$mean_RT[4]) / comparison_one$mean_RT[4]) * 100))

cat("\nTHREE-BUTTON TASK:\n")
cat(sprintf("  Visual Multimodal:     %.3f ± %.3f s\n",
            comparison_three$mean_RT[4], comparison_three$sem_RT[4]))
cat(sprintf("  Non-Visual Multimodal: %.3f ± %.3f s\n",
            comparison_three$mean_RT[5], comparison_three$sem_RT[5]))
cat(sprintf("  Visual dominance: %.1f%% faster\n", pct_diff))

cat("\n=== ALL FIGURES CREATED SUCCESSFULLY! ===\n")
cat("\nGenerated files:\n")
cat("  - Figure1_OneButton_MeanRT.png\n")
cat("  - Figure2_ThreeButton_MeanRT.png\n")
cat("  - Figure3_OneButton_Comparison.png\n")
cat("  - Figure4_ThreeButton_Comparison.png\n")