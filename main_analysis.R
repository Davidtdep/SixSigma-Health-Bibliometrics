###############################################################################
# 1. LIBRARIES
###############################################################################
# Short Description: Load all necessary packages.

library(dplyr)
library(tidyr)
library(readxl)
library(pheatmap)
library(ggplot2)
library(ggsci)
library(metafor)
library(wbstats)

# Optional small helper function to rename LMC -> LMIC and UMC -> UMIC in a dataframe
rename_LMC_UMC <- function(df, col_name = "IncomeGroup") {
  df[[col_name]] <- gsub("LMC", "LMIC", df[[col_name]])
  df[[col_name]] <- gsub("UMC", "UMIC", df[[col_name]])
  df
}


###############################################################################
# 2. INPUT OF INDICATORS FROM Global Observatory on Health R&D (no API)
###############################################################################
# Short Description:
#  - Reads CSV files for indicators that are not obtained via the World Bank API.
#  - Renames columns, pivots data by year, and converts income group values into abbreviations.

# Define the income groups of interest
income_groups <- c("HIC", "LIC", "LMC", "UMC")

# Read CSV file while keeping original column names
"Health researchers (in full-time equivalent), as a proportion of all researchers" <- read.csv(
  "~/Desktop/sixSigma/data/indicators/Health researchers (in full-time equivalent), as a proportion of all researchers.csv",
  header = TRUE, 
  stringsAsFactors = FALSE
)

"Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)" <- read.csv(
  "~/Desktop/sixSigma/data/indicators/Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT).csv",
  header = TRUE, 
  stringsAsFactors = FALSE
)

"Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country" <- read.csv(
  "~/Desktop/sixSigma/data/indicators/Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country.csv",
  header = TRUE, 
  stringsAsFactors = FALSE
)

# Rename the 'Income.Group' column to 'income_level_iso3c' for standardization
`Health researchers (in full-time equivalent), as a proportion of all researchers` <- 
  `Health researchers (in full-time equivalent), as a proportion of all researchers` %>%
  rename(income_level_iso3c = X)

`Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)` <-
  `Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)` %>%
  rename(income_level_iso3c = Income.Group)

`Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country` <-
  `Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country` %>%
  rename(income_level_iso3c = Income.Group)

# Function to replace values in income_level_iso3c with abbreviations
replace_income_levels <- function(df) {
  df %>%
    mutate(income_level_iso3c = case_when(
      income_level_iso3c == "Low income" ~ "LIC",
      income_level_iso3c == "Lower middle income" ~ "LMC",
      income_level_iso3c == "High income" ~ "HIC",
      income_level_iso3c == "Upper middle income" ~ "UMC",
      TRUE ~ income_level_iso3c  # Keep non-matching values
    ))
}

# Apply the function to each DataFrame
`Health researchers (in full-time equivalent), as a proportion of all researchers` <- 
  replace_income_levels(`Health researchers (in full-time equivalent), as a proportion of all researchers`)

`Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)` <- 
  replace_income_levels(`Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)`)

`Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country` <- 
  replace_income_levels(`Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country`)

# Change the DataFrame format (pivot the year columns and convert them to numeric)
`Health researchers (in full-time equivalent), as a proportion of all researchers` <- 
  `Health researchers (in full-time equivalent), as a proportion of all researchers` %>%
  pivot_longer(
    cols = starts_with("X"),   # Select columns that represent years
    names_to = "date",         # Name of the new column for years
    values_to = "value"        # Name of the new column for values
  ) %>%
  mutate(date = as.numeric(sub("X", "", date)))  # Remove "X" and convert to numeric

`Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)` <-
  `Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)` %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "date",
    values_to = "value"
  ) %>%
  mutate(date = as.numeric(sub("X", "", date)))

`Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country` <-
  `Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country` %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "date",
    values_to = "value"
  ) %>%
  mutate(date = as.numeric(sub("X", "", date)))


###############################################################################
# 3. INPUT THE DATASET
###############################################################################
# Short Description:
#  - Reads the main data file, converts selected columns to numeric,
#    and filters rows with non-missing income groups.

data <- read_excel(
  "~/Desktop/sixSigma/data/data.xlsx",
  col_types = "text"
)

data <- data %>%
  mutate_at(c(1, 5), as.numeric) %>%
  filter(!is.na(`IncomeGroup`))


###############################################################################
# 4. CREATING A SUMMARY DATASET (data_summary)
###############################################################################
# Short Description:
#  - Aggregates the main data by income group and year.
#  - Calculates total number of publications and mean H-index for each group/year.

data_summary <- data %>%
  group_by(`IncomeGroup`, `date`) %>%
  summarise(
    publications = n(),
    Hindex       = mean(`Hindex`, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  rename(
    income_level_iso3c = `IncomeGroup`,
    date               = `date`
  )

# Replace the names of the income groups for their abbreviations
data_summary <- replace_income_levels(data_summary)

# Create a list of DataFrames with indicator names as keys
indicator_dataframes <- list(
  "Health researchers (in full-time equivalent), as a proportion of all researchers" =
    `Health researchers (in full-time equivalent), as a proportion of all researchers`,
  "Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)" =
    `Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)`,
  "Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country" =
    `Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country`
)

# Merge the indicator values into data_summary
for (indicator_name in names(indicator_dataframes)) {
  # Extract the corresponding DataFrame
  indicator_data <- indicator_dataframes[[indicator_name]]
  
  # Ensure that the indicator column exists in data_summary
  if (!indicator_name %in% colnames(data_summary)) {
    data_summary[[indicator_name]] <- NA
  }
  
  # Match rows by income level and year, then add the value
  for (i in seq_len(nrow(indicator_data))) {
    match_row <- which(
      data_summary$income_level_iso3c == indicator_data$income_level_iso3c[i] &
        data_summary$date == indicator_data$date[i]
    )
    if (length(match_row) == 1) {
      data_summary[[indicator_name]][match_row] <- indicator_data$value[i]
    }
  }
  
  message(paste("Data added to data_summary for the indicator:", indicator_name))
}


###############################################################################
# 5. INPUT OF INDICATORS USING WORLD BANK API
###############################################################################
# Short Description:
#  - Uses the wbstats library to query the World Bank API for a list of indicators.
#  - Merges these indicators into data_summary, aggregated by income group and year.

# List of indicators to query
indicators <- c(
  "SH.DTH.NCOM.ZS", "SH.DTH.COMM.ZS", "SH.XPD.CHEX.GD.ZS", 
  "SP.DYN.CDRT.IN", "SH.MED.BEDS.ZS", "SH.DYN.NMRT",
  "SH.DYN.MORT",    "SP.DYN.IMRT.IN", "SP.DYN.AMRT.FE",
  "SP.DYN.AMRT.MA", "SH.XPD.OOPC.PC.CD", "SH.MED.PHYS.ZS",
  "SH.SGR.CRSK.ZS", "SH.SGR.IRSK.ZS",   "SH.MED.SAOP.P5",
  "BM.GSR.ROYL.CD", "BX.GSR.ROYL.CD",   "GB.XPD.RSDV.GD.ZS",
  "SP.POP.SCIE.RD.P6"
)

# Get the complete list of countries
countries_info <- wb_countries()

# Filter only the column with ISO code and income level
countries_income <- countries_info[
  countries_info$income_level_iso3c %in% income_groups,
  c("iso3c", "income_level_iso3c")
]

# Initialize a vector to store the indicator names
all_indicators <- c()

# Loop through each indicator to download, merge, and store
for (code in indicators) {
  # 1. Download data for the indicator
  indicator_data <- wb_data(indicator = code, return_wide = TRUE)
  
  # 2. Extract the descriptive "label" of the indicator
  indicator_name <- attr(indicator_data[[code]], "label")
  
  # 3. Merge with the country-income info
  indicator_data <- merge(indicator_data, countries_income, by = "iso3c")
  
  # 4. Rename the column corresponding to the indicator code to "value"
  colnames(indicator_data)[which(colnames(indicator_data) == code)] <- "value"
  
  # 5. Keep only relevant columns
  indicator_data <- indicator_data[, c("country", "date", "value", "last_updated", "income_level_iso3c")]
  
  # 6. Group by income_level_iso3c and date, then compute average value
  indicator_data <- aggregate(value ~ date + income_level_iso3c, 
                              data = indicator_data, 
                              FUN = mean, na.rm = TRUE)
  
  # 7. Reorder the columns to (income_level_iso3c, date, value)
  indicator_data <- indicator_data[, c("income_level_iso3c", "date", "value")]
  
  # 8. Add a new column in data_summary for this indicator
  if (!indicator_name %in% colnames(data_summary)) {
    data_summary[[indicator_name]] <- NA
  }
  
  # 9. Update the values in data_summary with the indicator data
  for (i in seq_len(nrow(indicator_data))) {
    match_row <- which(
      data_summary$income_level_iso3c == indicator_data$income_level_iso3c[i] &
        data_summary$date == indicator_data$date[i]
    )
    if (length(match_row) == 1) {
      data_summary[[indicator_name]][match_row] <- indicator_data$value[i]
    }
  }
  
  # 10. Append the indicator name to the global vector
  all_indicators <- c(all_indicators, indicator_name)
  
  # 11. Create an object in the environment with the name of the indicator label
  assign(indicator_name, indicator_data)
  
  # 12. (Optional) Remove temporary object to free memory
  rm(indicator_data)
  
  # Show a confirmation message
  message(paste("Indicator processed and added to data_summary:", indicator_name))
}

# Prepend the additional indicators from the CSV files to all_indicators
all_indicators <- c(
  "Health researchers (in full-time equivalent), as a proportion of all researchers",
  "Number of grants for biomedical research by funder, type of grant, duration and recipients (World RePORT)",
  "Official development assistance (ODA) for medical research and basic health sectors per capita, by recipient country",
  all_indicators
)

# Remove all DataFrames except data_summary, all_indicators, and replace_income_levels
rm(list = setdiff(ls(), c("data_summary", "all_indicators", "replace_income_levels", "rename_LMC_UMC")))


###############################################################################
# 6. DESCRIPTIVE ANALYSIS
###############################################################################
# Short Description:
#  - Simple example of descriptive analysis: line plot of publications over time,
#    by income group.

# Line plot
ggplot(data_summary, aes(x = date, y = publications, color = income_level_iso3c)) +
  geom_line(size = 0.5, lineend = "round") +
  labs(
    title = NULL,
    x = NULL,
    y = "Publications",
    color = NULL
  ) +
  scale_y_continuous(
    breaks = seq(0, max(data_summary$publications, na.rm = TRUE), by = 20),
    minor_breaks = NULL
  ) +
  scale_color_npg() +
  theme_minimal() +
  theme(
    axis.line.y = element_blank(),
    axis.line.x = element_line(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  )


###############################################################################
# 7. LINEAR MODELS
###############################################################################
# Short Description:
#  - Defines a function to run linear models for publications and H-index
#    against a set of dependent or independent indicators.

run_manual_lms <- function(data_summary, measure, all_indicators, income_group_var = "income_level_iso3c") {
  # Indices of dependent indicators (adjust as needed for your analysis)
  dependant_indices <- c(4, 5, 7, 8:13, 16, 17)
  dependant_indicators <- all_indicators[dependant_indices]
  independant_indicators <- all_indicators[-c(dependant_indices)]
  
  results_df <- data.frame(
    IncomeGroup = character(),
    DependentVariable = character(),
    IndependentVariable = character(),
    Coefficient = numeric(),
    StdError = numeric(),
    tValue = numeric(),
    PValue = numeric(),
    ResidualStdError = numeric(),
    DFResidual = numeric(),
    MultipleR2 = numeric(),
    AdjustedR2 = numeric(),
    FStatistic = numeric(),
    FStatisticPValue = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 1) (indicator_dep ~ measure)
  for (income_group in unique(data_summary[[income_group_var]])) {
    income_group_data <- data_summary[data_summary[[income_group_var]] == income_group, ]
    
    for (indicator in dependant_indicators) {
      valid_rows <- sum(!is.na(income_group_data[[indicator]]) & !is.na(income_group_data[[measure]]))
      if (valid_rows > 1) {
        formula_str <- paste0("`", indicator, "` ~ ", measure)
        model <- lm(as.formula(formula_str), data = income_group_data)
        s <- summary(model)
        
        coef_summary <- s$coefficients
        if (measure %in% rownames(coef_summary)) {
          Coefficient <- coef_summary[measure, "Estimate"]
          StdError    <- coef_summary[measure, "Std. Error"]
          tValue      <- coef_summary[measure, "t value"]
          PValue      <- coef_summary[measure, "Pr(>|t|)"]
        } else {
          Coefficient <- StdError <- tValue <- PValue <- NA
        }
        
        if (!is.null(s$fstatistic)) {
          f_statistic    <- s$fstatistic["value"]
          f_stat_p_value <- pf(f_statistic, s$fstatistic["numdf"], s$fstatistic["dendf"], lower.tail = FALSE)
        } else {
          f_statistic <- f_stat_p_value <- NA
        }
        
        results_df <- rbind(results_df, data.frame(
          IncomeGroup         = income_group,
          DependentVariable   = indicator,
          IndependentVariable = measure,
          Coefficient         = Coefficient,
          StdError            = StdError,
          tValue              = tValue,
          PValue              = PValue,
          ResidualStdError    = s$sigma,
          DFResidual          = s$df[2],
          MultipleR2          = s$r.squared,
          AdjustedR2          = s$adj.r.squared,
          FStatistic          = f_statistic,
          FStatisticPValue    = f_stat_p_value,
          stringsAsFactors    = FALSE
        ))
      }
    }
  }
  
  # 2) (measure ~ indicator_indep)
  for (income_group in unique(data_summary[[income_group_var]])) {
    income_group_data <- data_summary[data_summary[[income_group_var]] == income_group, ]
    
    for (indicator in independant_indicators) {
      valid_rows <- sum(!is.na(income_group_data[[indicator]]) & !is.na(income_group_data[[measure]]))
      if (valid_rows > 1) {
        formula_str <- paste0(measure, " ~ `", indicator, "`")
        model <- lm(as.formula(formula_str), data = income_group_data)
        s <- summary(model)
        
        coef_summary <- s$coefficients
        row_name <- paste0("`", indicator, "`")
        
        if (row_name %in% rownames(coef_summary)) {
          Coefficient <- coef_summary[row_name, "Estimate"]
          StdError    <- coef_summary[row_name, "Std. Error"]
          tValue      <- coef_summary[row_name, "t value"]
          PValue      <- coef_summary[row_name, "Pr(>|t|)"]
        } else {
          Coefficient <- StdError <- tValue <- PValue <- NA
        }
        
        if (!is.null(s$fstatistic)) {
          f_statistic    <- s$fstatistic["value"]
          f_stat_p_value <- pf(f_statistic, s$fstatistic["numdf"], s$fstatistic["dendf"], lower.tail = FALSE)
        } else {
          f_statistic <- f_stat_p_value <- NA
        }
        
        results_df <- rbind(results_df, data.frame(
          IncomeGroup         = income_group,
          DependentVariable   = measure,
          IndependentVariable = indicator,
          Coefficient         = Coefficient,
          StdError            = StdError,
          tValue              = tValue,
          PValue              = PValue,
          ResidualStdError    = s$sigma,
          DFResidual          = s$df[2],
          MultipleR2          = s$r.squared,
          AdjustedR2          = s$adj.r.squared,
          FStatistic          = f_statistic,
          FStatisticPValue    = f_stat_p_value,
          stringsAsFactors    = FALSE
        ))
      }
    }
  }
  
  results_df
}

# Run the function for each measure (publications and Hindex)
results_publications <- run_manual_lms(data_summary, "publications", all_indicators)
results_hindex       <- run_manual_lms(data_summary, "Hindex",       all_indicators)


###############################################################################
# 8. EXTRACT SIGNIFICANT RESULTS
###############################################################################
# Short Description:
#  - Filters the linear model results to retain only those below a certain p-value cutoff.

extract_significant_results <- function(results_df, p_cutoff = 0.05) {
  results_df %>%
    filter(PValue < p_cutoff) %>%
    select(IncomeGroup, DependentVariable, IndependentVariable, 
           Coefficient, StdError, tValue, PValue, ResidualStdError, 
           DFResidual, MultipleR2, AdjustedR2, FStatistic, FStatisticPValue)
}

significant_results_publications <- extract_significant_results(results_publications)
significant_results_hindex       <- extract_significant_results(results_hindex)

# Add row numbers (optional)
significant_results_publications$RowNumber <- seq_len(nrow(significant_results_publications))
significant_results_hindex$RowNumber       <- seq_len(nrow(significant_results_hindex))


###############################################################################
# 9. HEAT MAPS
###############################################################################
# Short Description:
#  - Creates heatmaps to visualize normalized regression coefficients.
#  - Uses hierarchical clustering and significance stars.

# Define indicators (add some placeholders first)
all_indicators <- c("citations", "publications", "Hindex", all_indicators)

specific_values <- c("CIT", "PUB", "HDX")  # Fixed abbreviations
specific_names  <- c("citations", "publications", "Hindex")

letters_vec <- LETTERS[1:(length(all_indicators) - length(specific_values))]
names(specific_values) <- specific_names
names(letters_vec) <- all_indicators[!(all_indicators %in% specific_names)]
equivalences <- c(specific_values, letters_vec)

# Optional helper functions for significance stars
assign_asterisks <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) "***"
  else if (p_value < 0.01) "**"
  else if (p_value < 0.05) "*"
  else ""
}

create_heatmap_data <- function(results, metric_name) {
  results <- results %>%
    mutate(IndependentAsterisk = ifelse(IndependentVariable != metric_name, "*", "")) %>%
    mutate(SignificanceAsterisk = case_when(
      PValue < 0.001 ~ "***",
      PValue < 0.01  ~ "**",
      PValue < 0.05  ~ "*",
      TRUE           ~ ""
    )) %>%
    group_by(DependentVariable, IndependentVariable) %>%
    mutate(CoefficientNormalized = (Coefficient - mean(Coefficient, na.rm = TRUE)) / 
             sd(Coefficient, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      DependentLetter = letters_vec[DependentVariable],
      IndependentLetter = letters_vec[IndependentVariable],
      RowLabel = ifelse(
        DependentVariable == metric_name,
        paste0(IndependentLetter, IndependentAsterisk),
        paste0(DependentLetter, IndependentAsterisk)
      ),
      CellLabel = ifelse(
        DependentVariable == metric_name,
        paste0(IndependentLetter, SignificanceAsterisk),
        paste0(DependentLetter, SignificanceAsterisk)
      )
    )
  
  heatmap_data <- results %>%
    select(IncomeGroup, RowLabel, CoefficientNormalized) %>%
    tidyr::spread(key = IncomeGroup, value = CoefficientNormalized)
  
  heatmap_matrix <- as.matrix(heatmap_data[, -1])
  rownames(heatmap_matrix) <- heatmap_data$RowLabel
  heatmap_matrix[is.na(heatmap_matrix)] <- 0
  
  significance_data <- results %>%
    select(IncomeGroup, RowLabel, SignificanceAsterisk) %>%
    tidyr::spread(key = IncomeGroup, value = SignificanceAsterisk)
  
  significance_matrix <- as.matrix(significance_data[, -1])
  rownames(significance_matrix) <- significance_data$RowLabel
  
  list(heatmap_matrix = heatmap_matrix, significance_matrix = significance_matrix)
}

create_custom_heatmap <- function(heatmap_matrix, significance_matrix, title) {
  pheatmap(
    heatmap_matrix,
    clustering_distance_rows = "manhattan",
    clustering_distance_cols = "manhattan",
    clustering_method = "ward.D2",
    show_rownames = TRUE,
    show_colnames = TRUE,
    display_numbers = significance_matrix,
    fontsize_number = 10,
    number_color = "black",
    main = title,
    border_color = NA,
    color = colorRampPalette(c("blue", "white", "red"))(50),
    breaks = seq(-1.5, 1.5, length.out = 51)
  )
}

# Example usage for 'publications'
data_publications <- create_heatmap_data(results_publications, "publications")

# Rename LMC -> LMIC, UMC -> UMIC for columns in heatmap
colnames(data_publications$heatmap_matrix) <- gsub("LMC", "LMIC", colnames(data_publications$heatmap_matrix))
colnames(data_publications$heatmap_matrix) <- gsub("UMC", "UMIC", colnames(data_publications$heatmap_matrix))

colnames(data_publications$significance_matrix) <- gsub("LMC", "LMIC", colnames(data_publications$significance_matrix))
colnames(data_publications$significance_matrix) <- gsub("UMC", "UMIC", colnames(data_publications$significance_matrix))

create_custom_heatmap(data_publications$heatmap_matrix, data_publications$significance_matrix, "Publications")

# Example usage for 'Hindex'
data_hindex <- create_heatmap_data(results_hindex, "Hindex")
colnames(data_hindex$heatmap_matrix) <- gsub("LMC", "LMIC", colnames(data_hindex$heatmap_matrix))
colnames(data_hindex$heatmap_matrix) <- gsub("UMC", "UMIC", colnames(data_hindex$heatmap_matrix))

colnames(data_hindex$significance_matrix) <- gsub("LMC", "LMIC", colnames(data_hindex$significance_matrix))
colnames(data_hindex$significance_matrix) <- gsub("UMC", "UMIC", colnames(data_hindex$significance_matrix))

create_custom_heatmap(data_hindex$heatmap_matrix, data_hindex$significance_matrix, "H-index")


###############################################################################
# 10. META-ANALYSIS
###############################################################################
# Short Description:
#  - Performs a random-effects meta-analysis on significant results from each income group,
#    then produces forest plots for normalized and unnormalized coefficients.

# Eliminate the first 3 bibliometric indicators
all_indicators_no_bibliometric <- all_indicators[-c(1:3)]

# Dependent indicators (adjust as necessary)
dependant_indices <- c(4, 5, 7, 8:13, 16, 17)
all_indicators_dependent <- all_indicators_no_bibliometric[dependant_indices]

# Independent indicators (exclude the first 3 and the dependent ones)
all_indicators_independent <- all_indicators_no_bibliometric[-c(dependant_indices)]

perform_meta_analysis_and_forest_plot <- function(
    results_df,
    measure_label,
    indicator_list,
    dep_or_indep = c("DependentVariable", "IndependentVariable"),
    normalized_title = "Forest Plot (Normalized)",
    unnormalized_title = "Forest Plot (Unnormalized)",
    min_studies = 2
) {
  dep_or_indep <- match.arg(dep_or_indep)
  
  meta_results <- data.frame(
    Indicator   = character(),
    Coefficient = numeric(),
    CI_Lower    = numeric(),
    CI_Upper    = numeric(),
    Weight      = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each indicator in the list
  for (indicator in indicator_list) {
    if (dep_or_indep == "DependentVariable") {
      subset_data <- results_df[results_df$DependentVariable == indicator, ]
    } else {
      subset_data <- results_df[results_df$IndependentVariable == indicator, ]
    }
    
    # Filter NA
    subset_data <- subset_data[!is.na(subset_data$Coefficient) & !is.na(subset_data$StdError), ]
    
    if (nrow(subset_data) >= min_studies) {
      res <- rma(yi = Coefficient, sei = StdError, data = subset_data, method = "REML")
      
      weight <- sum(1 / res$vi)
      
      meta_results <- rbind(meta_results, data.frame(
        Indicator   = indicator,
        Coefficient = res$b[1],
        CI_Lower    = res$ci.lb,
        CI_Upper    = res$ci.ub,
        Weight      = weight,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (!nrow(meta_results)) {
    message("No meta-analysis results for ", measure_label)
    return(NULL)
  }
  
  # Normalize coefficients (Z-score)
  meta_results_norm <- meta_results %>%
    mutate(
      Coefficient_Z = (Coefficient - mean(Coefficient)) / sd(Coefficient),
      CI_Lower_Z    = (CI_Lower   - mean(Coefficient)) / sd(Coefficient),
      CI_Upper_Z    = (CI_Upper   - mean(Coefficient)) / sd(Coefficient)
    )
  
  # Prepare a table with scientific format
  meta_results_table <- meta_results %>%
    mutate(
      `Coefficient (95% CI)` = paste0(
        round(Coefficient, 2), 
        " (", round(CI_Lower, 2), ", ", round(CI_Upper, 2), ")"
      ),
      Weight = round(Weight, 2)
    ) %>%
    select(Indicator, `Coefficient (95% CI)`, Weight)
  
  # Plot without normalization
  plot_original <- ggplot(meta_results, aes(x = Coefficient, y = Indicator)) +
    geom_point(aes(size = Weight), shape = 22, fill = alpha("black", 0.7)) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
    geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 0.8) +
    scale_size_continuous(range = c(3, 8)) +
    labs(
      title = paste(unnormalized_title, "-", measure_label),
      x     = "Coefficient (95% CI)",
      y     = "Indicators",
      size  = "Precision"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      axis.line.x      = element_blank(),
      axis.ticks       = element_blank()
    )
  
  # Plot with normalization
  plot_normalized <- ggplot(meta_results_norm, aes(x = Coefficient_Z, y = Indicator)) +
    geom_point(aes(size = Weight), shape = 22, fill = alpha("black", 0.7)) +
    geom_errorbarh(aes(xmin = CI_Lower_Z, xmax = CI_Upper_Z), height = 0.2) +
    geom_vline(xintercept = 0, color = "black", linetype = "solid", size = 0.8) +
    scale_size_continuous(range = c(3, 8)) +
    labs(
      title = paste(normalized_title, "-", measure_label),
      x     = "Z-Score Coefficient (95% CI)",
      y     = "Indicators",
      size  = "Precision"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      axis.line.x      = element_blank(),
      axis.ticks       = element_blank()
    )
  
  list(
    meta_results       = meta_results,
    meta_results_table = meta_results_table,
    plot_original      = plot_original,
    plot_normalized    = plot_normalized
  )
}


###############################################################################
# 11. EXAMPLES OF FOREST PLOTS
###############################################################################
# Short Description:
#  - Demonstrates meta-analysis usage for dependent and independent indicators
#    with either Publications or Hindex as measure.

# a) Publications - dependent indicators
res_pub_dep <- perform_meta_analysis_and_forest_plot(
  results_df     = results_publications,
  measure_label  = "Publications (Dep)",
  indicator_list = all_indicators_dependent,
  dep_or_indep   = "DependentVariable"
)

# b) H-index - dependent indicators
res_hdx_dep <- perform_meta_analysis_and_forest_plot(
  results_df     = results_hindex,
  measure_label  = "Hindex (Dep)",
  indicator_list = all_indicators_dependent,
  dep_or_indep   = "DependentVariable"
)

# Display results
res_hdx_dep$meta_results_table
res_pub_dep$plot_original
res_pub_dep$plot_normalized

# c) Publications - independent indicators
res_pub_ind <- perform_meta_analysis_and_forest_plot(
  results_df     = results_publications,
  measure_label  = "Publications (Ind)",
  indicator_list = all_indicators_independent,
  dep_or_indep   = "IndependentVariable"
)

# d) H-index - independent indicators
res_hdx_ind <- perform_meta_analysis_and_forest_plot(
  results_df     = results_hindex,
  measure_label  = "Hindex (Ind)",
  indicator_list = all_indicators_independent,
  dep_or_indep   = "IndependentVariable"
)

# Display results
res_hdx_ind$meta_results_table
res_pub_ind$plot_original
res_pub_ind$plot_normalized

# Example of exporting equivalences
# letters_vec_df <- data.frame(
#   Indicator = names(letters_vec),
#   Letter = as.character(letters_vec),
#   stringsAsFactors = FALSE
# )
# write.csv(letters_vec_df, "letters_vec_equivalences.csv", row.names = FALSE)


###############################################################################
# 12. MORTALITY RATE PER 1K PEOPLE: SUB-ANALYSIS
###############################################################################
# Short Description:
#  - Shows an example sub-analysis focusing on adult mortality rate by sex,
#    extracting specific rows from the results and plotting them.

# Add RowNumber to results_publications (in case it doesn't exist yet or was overwritten)
results_publications$RowNumber <- seq_len(nrow(results_publications))

# Extract the rows of interest
mortality_sex <- results_publications[results_publications$RowNumber %in% c(8, 9, 17, 18, 28, 29, 39, 40), ]

# Keep only certain columns
mortality_sex <- mortality_sex[, c(1, 2, 4, 5, 7)]

# Add a new 'sex' column based on DependentVariable
mortality_sex$sex <- ifelse(
  mortality_sex$DependentVariable == "Mortality rate, adult, female (per 1,000 female adults)", 
  "Female", 
  "Male"
)

# Add significance based on p-value
mortality_sex <- mortality_sex %>%
  mutate(Significance = case_when(
    PValue < 0.001 ~ "***",
    PValue < 0.01 ~ "**",
    PValue < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Rename LMC -> LMIC, UMC -> UMIC
mortality_sex <- rename_LMC_UMC(mortality_sex, "IncomeGroup")

# Reorder IncomeGroup in reverse order
mortality_sex$IncomeGroup <- factor(mortality_sex$IncomeGroup, levels = c("LIC", "LMIC", "UMIC", "HIC"))

# Calculate dynamic y-limits
y_min <- min(mortality_sex$Coefficient) - 0.3
y_max <- max(mortality_sex$Coefficient) + 0.3

average_coefficients <- mortality_sex %>%
  group_by(IncomeGroup) %>%
  summarize(AverageCoefficient = mean(Coefficient))

# Create the bar plot with a dashed line for average coefficients
ggplot(mortality_sex, aes(x = IncomeGroup, y = Coefficient, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(
    aes(ymin = Coefficient - StdError, ymax = Coefficient + StdError),
    position = position_dodge(width = 0.8), width = 0.2, color = "black", alpha = 0.6
  ) +
  geom_text(
    aes(label = Significance, y = Coefficient + ifelse(Coefficient > 0, StdError + 0.2, -StdError - 0.4)),
    position = position_dodge(width = 0.8), size = 4, vjust = 0.5
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  geom_line(
    data = average_coefficients,
    aes(x = IncomeGroup, y = AverageCoefficient, group = 1),
    inherit.aes = FALSE, color = "black", size = 0.5, alpha = 0.7,
    linetype = "dashed", lineend = "round"
  ) +
  labs(
    x = NULL,
    y = "Coefficient"
  ) +
  scale_y_continuous(breaks = c(-5, -3, -1, 0, 1, 3, 5)) +
  scale_fill_npg(name = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray80", size = 0.3),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length.y = unit(0, "cm"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    text = element_text(size = 10),
    legend.position = "top",
    plot.title = element_blank()
  )


###############################################################################
# 13. % OF GDP: SUB-ANALYSIS
###############################################################################
# Short Description:
#  - Similar sub-analysis focusing on percentage of GDP indicators,
#    extracting particular rows from results and plotting them.

results_publications$RowNumber <- seq_len(nrow(results_publications))  # Ensure row numbers exist

# Extract specific rows of interest
CHE <- results_publications[results_publications$RowNumber %in% c(46, 57, 67, 78), ]

# Keep only the relevant columns
CHE <- CHE[, c(1, 2, 4, 5, 7)]

# Add significance based on p-value
CHE <- CHE %>%
  mutate(Significance = case_when(
    PValue < 0.001 ~ "***",
    PValue < 0.01 ~ "**",
    PValue < 0.05 ~ "*",
    TRUE ~ ""
  ))

# Rename LMC -> LMIC, UMC -> UMIC
CHE <- rename_LMC_UMC(CHE, "IncomeGroup")

# Reorder the IncomeGroup in reverse order
CHE$IncomeGroup <- factor(CHE$IncomeGroup, levels = c("LIC", "LMIC", "UMIC", "HIC"))

# Calculate dynamic y-limits
y_min <- min(CHE$Coefficient, na.rm = TRUE) - 0.3
y_max <- max(CHE$Coefficient, na.rm = TRUE) + 0.3

# Replace one row as NA (as in original code)
CHE[2, 3] <- NA

average_coefficients <- CHE %>%
  group_by(IncomeGroup) %>%
  summarize(AverageCoefficient = mean(Coefficient, na.rm = TRUE))

ggplot(CHE, aes(x = IncomeGroup, y = Coefficient, fill = IncomeGroup)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.8) +
  geom_errorbar(
    aes(ymin = Coefficient - StdError, ymax = Coefficient + StdError),
    width = 0.2, color = "black", alpha = 0.6
  ) +
  geom_text(
    aes(label = Significance, 
        y = Coefficient + ifelse(Coefficient > 0, StdError + 0.2, -StdError - 0.4)),
    size = 4, vjust = 0.5
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  geom_line(
    data = average_coefficients,
    aes(x = IncomeGroup, y = AverageCoefficient, group = 1),
    inherit.aes = FALSE, color = "black", size = 0.5, alpha = 0.7,
    linetype = "dashed", lineend = "round"
  ) +
  labs(
    x = NULL,
    y = "Coefficient"
  ) +
  scale_y_continuous(
    limits = c(y_min, 15),
    breaks = seq(floor(y_min), 15, by = 2)
  ) +
  scale_fill_npg(name = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray80", size = 0.3),
    panel.grid.minor.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length.y = unit(0, "cm"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    text = element_text(size = 10),
    legend.position = "top",
    plot.title = element_blank()
  )
