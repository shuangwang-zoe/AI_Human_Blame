# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(summarytools)
library(lme4)  # for mixed-effects models
library(lmerTest)  # for p-values in mixed models
library(emmeans)  # for post-hoc comparisons
library(effects)  # for visualizing effects
library(MASS)  # for model diagnostics
library(car)  # for Anova
library(tidyr)  # for data manipulation
library(scales)  # for percentage formatting
library(brms)  # for Bayesian models

# Read the Excel file
data <- read_excel("combinedData.xlsx")

# Basic data exploration
print("Data Structure:")
str(data)

print("\nSummary Statistics:")
print(summary(data))

# Check AI variable
print("\nChecking AI Variable:")
print("Unique values in AI:")
print(unique(data$AI))
print("Frequency of AI values:")
print(table(data$AI))

# Check NationCN variable
print("\nChecking NationCN Variable:")
print("Unique values in NationCN:")
print(unique(data$NationCN))
print("Frequency of NationCN values:")
print(table(data$NationCN))

# Check CorrectedFirstNewFactorCode
print("\nChecking CorrectedFirstNewFactorCode:")
print("Unique values in CorrectedFirstNewFactorCode:")
print(unique(data$CorrectedFirstNewFactorCode))
print("Frequency of CorrectedFirstNewFactorCode values:")
print(table(data$CorrectedFirstNewFactorCode))

# Check BehaviorCategory
print("\nChecking BehaviorCategory:")
print("Unique values in BehaviorCategory:")
print(unique(data$BehaviorCategory))
print("Frequency of BehaviorCategory values:")
print(table(data$BehaviorCategory))

# Check participant variable
print("\nChecking Participant Variable:")
print("Unique participants:")
print(length(unique(data$participant)))
print("Participant variable type:")
print(class(data$participant))

# Part 1: Descriptive Analysis
print("\nPart 1: Descriptive Analysis")
run_descriptive_analysis <- function(dataset) {
  print("\nAnalysis for Human Dataset:")
  
  # Filter for Human data only and remove NA
  human_data <- dataset[dataset$AI == "Human" & !is.na(dataset$CorrectedFirstNewFactorCode), ]
  
  # Check if dataset is empty
  if (nrow(human_data) == 0) {
    print("Warning: Human dataset is empty!")
    return(NULL)
  }
  
  # Basic information
  print("\nBasic Information:")
  print(paste("Number of observations:", nrow(human_data)))
  print(paste("Number of unique participants:", length(unique(human_data$participant))))
  
  # Ensure variables are properly formatted
  human_data$participant <- as.factor(human_data$participant)
  human_data$CorrectedFirstNewFactorCode <- as.factor(human_data$CorrectedFirstNewFactorCode)
  human_data$NationCN <- as.factor(human_data$NationCN)
  human_data$BehaviorCategory <- as.factor(human_data$BehaviorCategory)
  
  # Check levels of CorrectedFirstNewFactorCode
  print("\nLevels of CorrectedFirstNewFactorCode:")
  print(levels(human_data$CorrectedFirstNewFactorCode))
  
  # Check frequency of each level
  print("\nFrequency of CorrectedFirstNewFactorCode levels:")
  freq_table <- table(human_data$CorrectedFirstNewFactorCode)
  print(freq_table)
  
  # Keep only categories with at least 2 observations
  valid_categories <- names(freq_table[freq_table >= 2])
  human_data <- human_data[human_data$CorrectedFirstNewFactorCode %in% valid_categories, ]
  human_data$CorrectedFirstNewFactorCode <- droplevels(human_data$CorrectedFirstNewFactorCode)
  
  # Check remaining levels
  print("\nRemaining levels after removing categories with less than 2 observations:")
  print(levels(human_data$CorrectedFirstNewFactorCode))
  
  # Calculate percentages for all combinations
  percentages <- human_data %>%
    group_by(BehaviorCategory, NationCN, CorrectedFirstNewFactorCode) %>%
    summarise(count = n()) %>%
    group_by(BehaviorCategory, NationCN) %>%
    mutate(percentage = count / sum(count) * 100)
  
  print("\nPercentages by BehaviorCategory and Nation:")
  print(percentages)
  
  # Create contingency tables for each combination
  print("\nContingency Tables:")
  
  # For each BehaviorCategory
  for (category in levels(human_data$BehaviorCategory)) {
    print(paste("\nContingency Table for BehaviorCategory:", category))
    category_data <- human_data[human_data$BehaviorCategory == category, ]
    
    # Create 2xK table (NationCN x CorrectedFirstNewFactorCode)
    contingency_table <- table(category_data$NationCN, category_data$CorrectedFirstNewFactorCode)
    print(contingency_table)
    
    # Check if table is valid for chi-square test
    expected <- chisq.test(contingency_table)$expected
    if (any(expected < 5)) {
      print("Warning: Some expected frequencies are less than 5.")
      print("Using Monte Carlo simulation for chi-square test:")
      print(chisq.test(contingency_table, simulate.p.value = TRUE, B = 10000))
      
      # Calculate standardized residuals
      print("\nStandardized residuals:")
      residuals <- chisq.test(contingency_table)$residuals
      print(residuals)
      
      # Calculate relative risks
      print("\nRelative risks (NationCN=1 vs NationCN=0):")
      for (code in colnames(contingency_table)) {
        if (sum(contingency_table[, code]) > 0) {
          risk_ratio <- (contingency_table[2, code] / sum(contingency_table[2, ])) / 
                       (contingency_table[1, code] / sum(contingency_table[1, ]))
          print(paste(code, ":", round(risk_ratio, 3)))
        }
      }
    } else {
      print("\nChi-square test for NationCN vs CorrectedFirstNewFactorCode:")
      print(chisq.test(contingency_table))
      
      # Calculate standardized residuals
      print("\nStandardized residuals:")
      residuals <- chisq.test(contingency_table)$residuals
      print(residuals)
      
      # Calculate relative risks
      print("\nRelative risks (NationCN=1 vs NationCN=0):")
      for (code in colnames(contingency_table)) {
        if (sum(contingency_table[, code]) > 0) {
          risk_ratio <- (contingency_table[2, code] / sum(contingency_table[2, ])) / 
                       (contingency_table[1, code] / sum(contingency_table[1, ]))
          print(paste(code, ":", round(risk_ratio, 3)))
        }
      }
    }
    
    # Calculate Cramer's V for effect size
    chi2 <- chisq.test(contingency_table)$statistic
    n <- sum(contingency_table)
    k <- min(nrow(contingency_table), ncol(contingency_table))
    cramer_v <- sqrt(chi2 / (n * (k - 1)))
    print(paste("\nCramer's V effect size:", round(cramer_v, 3)))
  }
  
  # Create visualizations for each NationCN
  plots <- list()
  
  for (nation in levels(human_data$NationCN)) {
    nation_data <- percentages[percentages$NationCN == nation, ]
    
    # Sort data by percentage in descending order within each BehaviorCategory
    nation_data <- nation_data %>%
      group_by(BehaviorCategory) %>%
      arrange(desc(percentage), .by_group = TRUE) %>%
      mutate(order = row_number()) %>%
      ungroup()
    
    # Create ordered factor for CorrectedFirstNewFactorCode
    nation_data$CorrectedFirstNewFactorCode <- factor(
      nation_data$CorrectedFirstNewFactorCode,
      levels = unique(nation_data$CorrectedFirstNewFactorCode[order(nation_data$order)])
    )
    
    # Create a list to store individual plots for each BehaviorCategory
    behavior_plots <- list()
    
    # Create a separate plot for each BehaviorCategory
    for (category in unique(nation_data$BehaviorCategory)) {
      category_data <- nation_data[nation_data$BehaviorCategory == category, ]
      
      p <- ggplot(category_data, aes(x = CorrectedFirstNewFactorCode, 
                                    y = percentage, 
                                    fill = CorrectedFirstNewFactorCode)) +
        geom_bar(stat = "identity", width = 0.7) +
        scale_y_continuous(labels = percent_format(scale = 1)) +
        labs(title = paste("Behavior Category:", category),
             x = "CorrectedFirstNewFactorCode",
             y = "Percentage") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none",
              plot.title = element_text(size = 12, face = "bold"),
              plot.margin = unit(c(1, 1, 1, 1), "lines"))
      
      behavior_plots[[category]] <- p
    }
    
    # Combine all behavior category plots for this nation
    plots[[nation]] <- do.call(grid.arrange, c(behavior_plots, nrow = length(behavior_plots)))
  }
  
  # Arrange US and CN plots vertically
  grid_plot <- grid.arrange(plots[["0"]], plots[["1"]], nrow = 2, heights = c(1, 1))
  print(grid_plot)
  
  # Save the plot with more height to accommodate multiple rows
  ggsave("plot_human_categories.png", grid_plot, width = 16, height = 24)
  
  # Save descriptive statistics
  write.csv(percentages, "human_descriptive_stats.csv", row.names = FALSE)
  
  return(human_data)
}

# Part 2: Multilevel Analysis
print("\nPart 2: Multilevel Analysis")
run_multilevel_analysis <- function(human_data) {
  print("\nRunning Bayesian Multilevel Analysis:")
  
  # Fit the Bayesian multinomial logistic regression model
  brm_model <- brm(
    formula = CorrectedFirstNewFactorCode ~ BehaviorCategory * NationCN + (1 | participant),
    data = human_data,
    family = categorical(),
    prior = c(
      set_prior("normal(0, 2)", class = "b"),
      set_prior("normal(0, 2)", class = "sd")
    ),
    chains = 4,
    iter = 2000,
    warmup = 1000,
    cores = 4,
    control = list(adapt_delta = 0.95)
  )
  
  # Print model summary
  print(summary(brm_model))
  
  # Plot posterior distributions
  print(plot(brm_model))
  
  # Posterior predictive checks
  print(pp_check(brm_model))
  
  # Conditional effects
  print(conditional_effects(brm_model))
  
  # Save the model
  saveRDS(brm_model, "brm_model_human.rds")
  
  return(brm_model)
}

# Part 3: Three-way Analysis
print("\nPart 3: Three-way Analysis of Culture, AI, and Behavior Category Effects")
run_three_way_analysis <- function(dataset) {
  print("\nAnalyzing effects of Culture, AI, and Behavior Category on Factor Selection:")
  
  # Ensure variables are properly formatted
  dataset$participant <- as.factor(dataset$participant)
  dataset$CorrectedFirstNewFactorCode <- as.factor(dataset$CorrectedFirstNewFactorCode)
  dataset$NationCN <- as.factor(dataset$NationCN)
  dataset$AI <- as.factor(dataset$AI)
  dataset$BehaviorCategory <- as.factor(dataset$BehaviorCategory)
  
  # Create frequency table for each combination
  freq_table <- dataset %>%
    group_by(NationCN, AI, BehaviorCategory, CorrectedFirstNewFactorCode) %>%
    summarise(count = n()) %>%
    group_by(NationCN, AI, BehaviorCategory) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  print("\nFrequency Table:")
  print(freq_table)
  
  # Perform three-way ANOVA
  print("\nThree-way ANOVA Results:")
  anova_model <- aov(percentage ~ NationCN * AI * BehaviorCategory, data = freq_table)
  print(summary(anova_model))
  
  # Post-hoc tests for significant interactions
  print("\nPost-hoc Tests:")
  
  # Test for NationCN * AI interaction
  print("\nNationCN * AI Interaction:")
  interaction_test <- interaction(dataset$NationCN, dataset$AI)
  print(TukeyHSD(aov(percentage ~ interaction_test, data = freq_table)))
  
  # Test for NationCN * BehaviorCategory interaction
  print("\nNationCN * BehaviorCategory Interaction:")
  interaction_test <- interaction(dataset$NationCN, dataset$BehaviorCategory)
  print(TukeyHSD(aov(percentage ~ interaction_test, data = freq_table)))
  
  # Test for AI * BehaviorCategory interaction
  print("\nAI * BehaviorCategory Interaction:")
  interaction_test <- interaction(dataset$AI, dataset$BehaviorCategory)
  print(TukeyHSD(aov(percentage ~ interaction_test, data = freq_table)))
  
  # Visualize the interactions
  print("\nCreating Interaction Plots:")
  
  # NationCN * AI interaction
  p1 <- ggplot(freq_table, aes(x = NationCN, y = percentage, fill = AI)) +
    geom_boxplot() +
    facet_wrap(~BehaviorCategory) +
    labs(title = "NationCN * AI Interaction by Behavior Category",
         x = "NationCN",
         y = "Percentage",
         fill = "AI") +
    theme_minimal()
  
  # NationCN * BehaviorCategory interaction
  p2 <- ggplot(freq_table, aes(x = NationCN, y = percentage, fill = BehaviorCategory)) +
    geom_boxplot() +
    facet_wrap(~AI) +
    labs(title = "NationCN * BehaviorCategory Interaction by AI",
         x = "NationCN",
         y = "Percentage",
         fill = "Behavior Category") +
    theme_minimal()
  
  # AI * BehaviorCategory interaction
  p3 <- ggplot(freq_table, aes(x = AI, y = percentage, fill = BehaviorCategory)) +
    geom_boxplot() +
    facet_wrap(~NationCN) +
    labs(title = "AI * BehaviorCategory Interaction by NationCN",
         x = "AI",
         y = "Percentage",
         fill = "Behavior Category") +
    theme_minimal()
  
  # Arrange plots
  grid_plot <- grid.arrange(p1, p2, p3, nrow = 3)
  print(grid_plot)
  
  # Save the plots
  ggsave("three_way_interactions.png", grid_plot, width = 16, height = 20)
  
  # Calculate effect sizes
  print("\nEffect Sizes:")
  eta_squared <- summary(anova_model)[[1]]$`Sum Sq` / sum(summary(anova_model)[[1]]$`Sum Sq`)
  print(data.frame(
    Effect = rownames(summary(anova_model)[[1]]),
    Eta_Squared = round(eta_squared, 3)
  ))
  
  return(list(
    anova_model = anova_model,
    freq_table = freq_table
  ))
}

# Run both analyses
print("\nStarting Analysis Pipeline:")
human_data <- run_descriptive_analysis(data)
brm_model <- run_multilevel_analysis(human_data)

# Run the three-way analysis
three_way_results <- run_three_way_analysis(data)

# Save the workspace
save.image("analysis_results.RData") 