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

ggplot(trust_plot_data, aes(x = reorder(Trust_Category, -Mean_Trust_Value), y = Mean_Trust_Value, fill = Trust_Category)) +
  geom_bar(stat = "identity", color = "white") +
  labs(
    title = "Average Trust Levels Across Different Categories",
    x = "Trust Category",
    y = "Mean Trust Value"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Dark2")