# Plot 2
trust_columns <- c("TRUST_NG_HMH", "TRUST_LG_HMH", "TRUST_CS_HMH", "TRUST_LE_HMH", "TRUST_CL_HMH", "TRUST_DAT_LIK")

cor_data <- oecd_data[, trust_columns]

cor_matrix <- cor(cor_data, use = "complete.obs") # ignore NA

cor_data_long <- as.data.frame(as.table(cor_matrix))
names(cor_data_long) <- c("variable1", "variable2", "correlation")

ggplot(cor_data_long, aes(variable1, variable2, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_distiller(palette = "Blues", direction = 1, name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(angle = 45, hjust = 1)) +
  coord_fixed()

# Plot 3
trust_columns <- c("TRUST_NG_HMH", "TRUST_LG_HMH", "TRUST_CS_HMH", "TRUST_LE_HMH", "TRUST_CL_HMH", "TRUST_DAT_LIK")

trust_data <- oecd_data[, trust_columns]

mean_trust <- sapply(trust_data, mean, na.rm = TRUE)

trust_plot_data <- data.frame(
  Trust_Category = names(mean_trust),
  Mean_Trust_Value = mean_trust
)
 # Bar chart
ggplot(trust_plot_data, aes(x = reorder(Trust_Category, -Mean_Trust_Value), y = Mean_Trust_Value, fill = Trust_Category)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    title = "Average Trust Levels Across Different Categories",
    x = "Trust Category",
    y = "Mean Trust Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Violin plots
# Assuming oecd_data is your data frame with relevant columns
trust_columns <- c("TRUST_NG_HMH", "TRUST_LG_HMH", "TRUST_CS_HMH", "TRUST_LE_HMH", "TRUST_CL_HMH")

# Select the relevant columns for the violin plot
trust_data <- oecd_data[, trust_columns]

# Reshape the data to long format for ggplot
trust_data_long <- tidyr::gather(trust_data, key = "Trust_Category", value = "Trust_Value")

# Plot the violin plot with professional adjustments
ggplot(trust_data_long, aes(x = Trust_Category, y = Trust_Value, fill = Trust_Category)) +
  geom_violin(trim = FALSE, scale = "width", width = 0.7) +
  labs(
    title = "Distribution of Trust Levels Across Different Categories",
    x = "Trust Category",
    y = "Trust Value"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 12)
  )

# Histogram of TRUST_PE_EXT_CON
ggplot(oecd_data, aes(x = TRUST_PE_EXT_CON)) +
  geom_histogram(binwidth = 10, fill = "deepskyblue3", color = "white", alpha = 0.7, show.legend = FALSE) +
  labs(
    title = "Distribution of Perceived Political Efficacy",
    x = "Political Efficacy",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Box plots
trust_columns <- c("TRUST_NG_HMH", "TRUST_LG_HMH", "TRUST_CS_HMH", "TRUST_LE_HMH", "TRUST_CL_HMH", "TRUST_DAT_LIK")

trust_data <- oecd_data[, trust_columns]

trust_data_long <- pivot_longer(trust_data, cols = colnames(trust_data), names_to = "gov_type", values_to = "percentage")

ggplot(trust_data_long, aes(x = gov_type, y = percentage, fill = gov_type)) +
  geom_boxplot(show.legend = FALSE) +
  labs(
    title = "Distribution of trust levels across government types",
    x = "Government type",
    y = "Percentage reporting high or moderately high trust"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))