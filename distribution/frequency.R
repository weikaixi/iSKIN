########
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)



data1 <-read.csv("MW_frequency.csv",sep = ",",header=T)
fix(data1)
# Extract nHBA values for Training and Test sets
# Extract nHBA values for Training and Test sets
training_values <- data1[data1$PLATE == "Sensitizer",]$MW
test_values <- data1[data1$PLATE == "Non-sensitizer",]$MW

# Perform Wilcoxon Rank Sum test
test_result <- wilcox.test(training_values, test_values)

# Extract p-value and format it
p_val <- sprintf("p-value = %.4f", test_result$p.value)

# Calculate total counts for each group (Training and Test)
group_totals <- data1 %>% group_by(PLATE) %>% summarise(total = n())

# Join the totals to the main data
data1 <- merge(data1, group_totals, by = "PLATE")

# Calculate proportions for each bin within each group
data1_proportions <- data1 %>% 
  group_by(PLATE, MW) %>%
  tally() %>%
  left_join(group_totals, by = "PLATE") %>%
  mutate(proportion = n / total)
fix(data1_proportions)
# Plot with added p-value in legend and bar heights representing proportions within each group
bin_width <- 30  # Adjusted bin width

p <- ggplot(data1_proportions, aes(x = MW, fill = PLATE, y = proportion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 30), alpha = 0.4, width = bin_width) +
  coord_cartesian(xlim = c(0,1000)) +
  scale_x_continuous(breaks = seq(0, 1000, by =50)) +
  scale_fill_discrete(name = p_val) +  # Add p-value to legend title
  theme(panel.background = element_rect(color = "black", fill = NA, size = 0.7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.position = c(0.95, 0.95),  # Position the legend inside the plot area
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill="transparent", color="transparent"))  # Make legend background transparent

print(p)




pdf(file = "MW_count.pdf", width = 8, height = 7)
print(p)
dev.off()


###

data1 <-read.csv("LogP_frequency.csv",sep = ",",header=T)
#
fix(data1)
# Extract nHBA values for Training and Test sets
training_values <- data1[data1$PLATE == "Sensitizer",]$ALogP
test_values <- data1[data1$PLATE == "Non-sensitizer",]$ALogP

# Perform Wilcoxon Rank Sum test
test_result <- wilcox.test(training_values, test_values)

# Extract p-value and format it
p_val <- sprintf("p-value = %.4f", test_result$p.value)

# Calculate total counts for each group (Training and Test)
group_totals <- data1 %>% group_by(PLATE) %>% summarise(total = n())

# Join the totals to the main data
data1 <- merge(data1, group_totals, by = "PLATE")

# Calculate proportions for each bin within each group
data1_proportions <- data1 %>% 
  group_by(PLATE, ALogP) %>%
  tally() %>%
  left_join(group_totals, by = "PLATE") %>%
  mutate(proportion = n / total)
fix(data1_proportions)
# Plot with added p-value in legend and bar heights representing proportions within each group
bin_width <- 0.5  # Adjusted bin width

p <- ggplot(data1_proportions, aes(x = ALogP, fill = PLATE, y = proportion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), alpha = 0.4, width = bin_width) +
  coord_cartesian(xlim = c(-3,7)) +
  scale_x_continuous(breaks = seq(-3, 7, by = 1)) +
  scale_fill_discrete(name = p_val) +  # Add p-value to legend title
  theme(panel.background = element_rect(color = "black", fill = NA, size = 0.7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.position = c(0.95, 0.95),  # Position the legend inside the plot area
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill="transparent", color="transparent"))  # Make legend background transparent

print(p)




pdf(file = "LogP_count.pdf", width = 8, height = 7)
print(p)
dev.off()


###

data1 <-read.csv("TPSA_frequency.csv",sep = ",",header=T)
fix(data1)
training_values <- data1[data1$PLATE == "Sensitizer",]$TPSA
test_values <- data1[data1$PLATE == "Non-sensitizer",]$TPSA

# Perform Wilcoxon Rank Sum test
test_result <- wilcox.test(training_values, test_values)

# Extract p-value and format it
p_val <- sprintf("p-value = %.4f", test_result$p.value)

# Calculate total counts for each group (Training and Test)
group_totals <- data1 %>% group_by(PLATE) %>% summarise(total = n())

# Join the totals to the main data
data1 <- merge(data1, group_totals, by = "PLATE")

# Calculate proportions for each bin within each group
data1_proportions <- data1 %>% 
  group_by(PLATE, TPSA) %>%
  tally() %>%
  left_join(group_totals, by = "PLATE") %>%
  mutate(proportion = n / total)
fix(data1_proportions)
# Plot with added p-value in legend and bar heights representing proportions within each group
bin_width <- 10  # Adjusted bin width

p <- ggplot(data1_proportions, aes(x = TPSA, fill = PLATE, y = proportion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 10), alpha = 0.4, width = bin_width) +
  coord_cartesian(xlim = c(-1,600)) +
  scale_x_continuous(breaks = seq(0, 600, by = 30)) +
  scale_fill_discrete(name = p_val) +  # Add p-value to legend title
  theme(panel.background = element_rect(color = "black", fill = NA, size = 0.7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.position = c(0.95, 0.95),  # Position the legend inside the plot area
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill="transparent", color="transparent"))  # Make legend background transparent

print(p)


pdf(file = "TPSA_count.pdf", width = 8, height = 7)
print(p)
dev.off()


# Load necessary libraries
install.packages(c("ggplot2", "ggpubr"))
library(ggplot2)
library(ggpubr)

# Load necessary libraries
install.packages(c("ggplot2", "ggpubr"))
library(ggplot2)
library(ggpubr)

# Read the CSV file
data <- read.csv("updated_file_rawvalue.csv")

# Box plots for each column
cols <- c("Mw", "ALogP", "TPSA")
for (col in cols) {
  p <- ggplot(data, aes_string(x = "label", y = col)) + 
    geom_boxplot(outlier.shape = NA) +  # Hide outliers
    geom_jitter(aes_string(color = "label"), width = 0.2) + 
    theme(panel.background = element_rect(color = "black", fill = NA, size = 0.7),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_blank(),
          legend.position = c(0.95, 0.95),
          legend.justification = c("right", "top"),
          legend.background = element_rect(fill="transparent", color="transparent")) +
    scale_color_discrete(name = "Label")
  
  # Add p-value using Wilcoxon rank-sum test
  p <- p + stat_compare_means(method = "wilcox.test")
  pdf(file = paste0(col,".pdf"), width = 8, height = 7)
  print(p)
  dev.off()
}





