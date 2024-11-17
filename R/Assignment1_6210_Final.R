### Assignment #1 ----

# Script name: Analysis of Marine Decapoda Species Accumulation in Canada

# Purpose of script: To examine the species accumulation for members of the Decapoda order in Canadian marine areas, and compare to the species accumulation of commercially and non-commercially valued species.

# Hypotheses:
# Decapoda species will be undersampled in all regions of Canada, following global trends.
# Sampling of Decapoda species of non-commercial value will be significantly less accumulated when compared to commercially valued species.

# Author: Jessica Finnie

# Date Created: 2024-09-23

# Email: finniej@uoguelph.ca

# Packages Required ----
# Uncomment and/or install as required
library(tidyverse)

library(mapview)

library(vegan)

# Acquiring Data and Initial Filtering ----
# Data for the Order Decapoda was pulled from BOLD using the code below, pre-filtered for Canadian only data sources on September 23 at 5:26PM.
# Data was then filtered to remove samples without a sampling Province/Territory, as this would be necessary for further analysis. The file was then written to disk. Only uncomment if needed.
# dfCanDecapoda <- read_tsv(file = "http://www.boldsystems.org/index.php/API_Public/combined?taxon=Decapoda&geo=Canada&format=tsv")
# dfDecapoda <- dfCanDecapoda[!is.na(dfCanDecapoda$province_state) & !is.na(dfCanDecapoda$bin_uri),]
# write_tsv(dfDecapoda, "Can_Decapoda_BOLD_data.tsv")

# _Retrieve the Data from File----
dfDecapoda <- read_tsv("C:/Users/Dell User/Downloads/Can_Decapoda_BOLD_data.tsv")

# _Filtering The Data ----
# The data must now be filtered and processed. Samples that are missing species identifications, latitude (and therefore longitude) and nucleotide sequences were removed. Nucleotide sequences are retained in case of future further analyses. The data frame was also manipulated to add a variable that labels each specimen according to commercial value. Commercial value of species was pulled from the Government of Canada's Department of Fisheries and Oceans website (see citations in Introduction).
dfFiltered <- dfDecapoda %>%
  filter(!is.na(nucleotides)) %>%
  filter(!is.na(species_name)) %>%
  filter(!is.na(lat)) %>%
  mutate(commercial_value = if_else(.$species_name == "Pandalus borealis" | .$species_name == "Pandalus montagui" | .$species_name == "Pandalus hypsinotus" | .$species_name == "Pandalus jordani" | .$species_name == "Pandalopsis dispar" | .$species_name == "Pandalus danae" | .$species_name == "Pandalus platyceros" | .$species_name == "Metacarcinus magister" | .$species_name == "Cancer magister" | .$species_name == "Cancer productus" | .$species_name == "Paralithodes camtschatic" | .$species_name == "Lithodes aequispinus" | .$species_name == "Homarus americanus" | .$species_name == "Chionoecetes opilio" | .$species_name == "Cancer borealis", "Commercial", "Non-Commercial"))

# In order to check the data for any errors, a few visual inspections are performed through the code below.
summary(dfFiltered)
names(dfFiltered)
table(dfFiltered$species_name)

#EDIT 1
# Defined a function called `summarize_data` that takes a dataset as input
summarize_data <- function(data) {
  summary_stats <- data %>%
    group_by(region) %>%
    summarize(
      Total_Samples = n(),
      Unique_Species = n_distinct(species_name),
      Avg_Samples_per_Species = mean(table(species_name))
    )

  # Created a bar plot using ggplot2 to visualize the total samples per region  
  print(summary_stats)
  ggplot(summary_stats, aes(x = region, y = Total_Samples)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Sample Distribution Across Regions",
         x = "Region", y = "Total Samples") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Call the function `summarize_data` and pass the filtered dataset (`dfFiltered`) as input
summarize_data(dfFiltered)


# Sub-setting the Data ----
# Data frames were then created for Atlantic and Pacific regions, in order to examine species distribution and accumulation by region. Latitude and longitude boundaries were determined using the Browser on https://www.marineregions.org/gazetteer.php?p=browser. Albertan samples were removed from the Atlantic region, as samples that were Barcoded from consumer-purchased samples were entered into the BOLD database, and should not be included in this analysis due to lack of specific region of collection.
dfPacific <- dfFiltered %>%
  filter(between(lat, 45, 60)) %>%
  filter(between(lon, -136, -120))

dfAtlantic <- dfFiltered %>%
  filter(between(lat, 40, 61)) %>%
  filter(between(lon, -69, -50)) %>%
  filter(province_state != "Alberta")

# Another round of inspections are performed on the new data frames to confirm filtering worked as intended
summary(dfPacific)
summary(dfAtlantic)

#EDIT 1 (Continued)
# Updated summarize_data Function ----
#Dynamic toggle

summarize_data <- function(data, plot_title, aggregate = TRUE, threshold = 10) {
  # Summarize the data
  summary_stats <- data %>%
    group_by(region) %>%
    summarize(
      Total_Samples = n(),
      Unique_Species = n_distinct(species_name),
      Avg_Samples_per_Species = round(mean(table(species_name)), 2)
    )
  
  # Aggregate minor regions if needed
  if (aggregate) {
    summary_stats <- summary_stats %>%
      mutate(region = if_else(Total_Samples < threshold, "Other", region)) %>%
      group_by(region) %>%
      summarize(
        Total_Samples = sum(Total_Samples),
        Unique_Species = sum(Unique_Species),
        Avg_Samples_per_Species = mean(Avg_Samples_per_Species)
      )
  }
  
  # Plot the data
  plot <- ggplot(summary_stats, aes(x = reorder(region, -Total_Samples), y = Total_Samples)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(
      title = plot_title,
      x = "Region",
      y = "Total Samples"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12)
    )
  
  # Print summary statistics and return the plot
  print(summary_stats)
  return(plot)
}

#For Atlantic region
# with aggregation
plot_atlantic <- summarize_data(dfAtlantic, "Sample Distribution for Atlantic Region")
print(plot_atlantic)
#without aggregation
plot_atlantic <- summarize_data(dfAtlantic, "Sample Distribution for Atlantic Region", aggregate = FALSE)
print(plot_atlantic)

#For Pacific region
# with aggregation
plot_pacific <- summarize_data(dfPacific, "Sample Distribution for Pacific Region")
print(plot_pacific)
#without aggregation
plot_pacific <- summarize_data(dfPacific, "Sample Distribution for Pacific Region", aggregate = FALSE)
print(plot_pacific)


# _Visualizing the Subsets ----
# In order to confirm the spread of the regional data frames, maps were generated for each region. Each map includes the spread of Families found in the order Decapoda, as well as the commercial value. Families were used to attain a general overview of diversity, as high numbers of Genera and Species crowded the map.
mapAtlCommercialVal <- mapview(dfAtlantic, xcol = "lon", ycol = "lat", zcol = "commercial_value", layer.name = "Commercial Value", crs = 4269, grid = FALSE, col.regions = list("red", "blue"))
mapAtlFamily <- mapview(dfAtlantic, xcol = "lon", ycol = "lat", zcol = "family_name", layer.name = "Family", crs = 4269, grid = FALSE)
mapPacCommercialVal <- mapview(dfPacific, xcol = "lon", ycol = "lat", zcol = "commercial_value", layer.name = "Commercial Value", crs = 4269, grid = FALSE, col.regions = list("red", "blue"))
mapPacFamily <- mapview(dfPacific, xcol = "lon", ycol = "lat", zcol = "family_name", layer.name = "Family", crs = 4269, grid = FALSE)



# install.packages(leaflet.extras2) if needed in order to view the maps.
install.packages("leaflet.extras2")
library(leaflet.extras2)

# Mapview was chose due to it's output of interactive HTML maps. These maps are best viewed on RStudio, or another interactive viewer. I have included just the pacific map as one of my figures, as it shows a greater amount of sampling sites.
mapAtlCommercialVal | mapAtlFamily
mapPacCommercialVal | mapPacFamily

# These maps suggest that there is some overlap between areas of high biodiversity and commercial activity. This may be due to sampling bias towards easily accessible areas, or a true measure of biodiversity.

# Analyzing the Data ----
# _Rarefaction Curves ----
# As a beginning point for my analysis, simple rarefaction curves were generated for Canada, as well as each coastal region. These rarefaction curves were created based on individual samples and BINs.
dfCanadaBINs <- dfFiltered %>%
  group_by(bin_uri) %>%
  count(bin_uri)
dfCanadaBINs <- pivot_wider(data = dfCanadaBINs, names_from = bin_uri, values_from = n)
x <- rarecurve(dfCanadaBINs, xlab = "Individuals Barcoded", ylab = "BIN Richness")

dfAtlanticBINS <- dfAtlantic %>%
  group_by(bin_uri) %>%
  count(bin_uri)
dfAtlanticBINS <- pivot_wider(data = dfAtlanticBINS, names_from = bin_uri, values_from = n)
y <- rarecurve(dfAtlanticBINS, xlab = "Individuals Barcoded", ylab = "BIN Richness")

dfPacificBINS <- dfPacific %>%
  group_by(bin_uri) %>%
  count(bin_uri)
dfPacificBINS <- pivot_wider(data = dfPacificBINS, names_from = bin_uri, values_from = n)
z <- rarecurve(dfPacificBINS, xlab = "Individuals Barcoded", ylab = "BIN Richness")

# Generated rarefaction curves show that all BINs are approaching sampling saturation. However, as rarefaction curves examine the data at the individual or BIN level, and I would like to examine the data by sampling regions to better understand current estimates of species richness, I will remove these dataframes and figures, and continue on to the creation of Species Accumulation Curves.
rm(dfCanadaBINs, dfAtlanticBINS, dfPacificBINS, x, y, z)

# _Regional Species Accumulation Curves ----
# Data is filtered and processed based on BINs and Regions, in order to create species accumulation curves for Canada and each coastal region. Each data frame is manipulated to accumulate BINs by Region, and to replace all missing BINs with a value of 0. I am creating these curves to investigate the hypothesis that Decapoda is undersampled in Canada, as well as each region of interest.

# Canadian Data Processing
dfBINs.CAN <- dfFiltered %>%
  group_by(region) %>%
  count(bin_uri)

dfBINs.CAN <- pivot_wider(data = dfBINs.CAN, names_from = bin_uri, values_from = n)

dfBINs.CAN <- dfBINs.CAN %>%
  filter(!is.na(region)) %>%
  remove_rownames() %>%
  column_to_rownames(var = "region")

dfBINs.CAN[is.na(dfBINs.CAN)] <- 0
AccumCurveCan <- specaccum(dfBINs.CAN)

# Atlantic Data Processing
dfBINs.ATL <- dfAtlantic %>%
  group_by(region) %>%
  count(bin_uri)

dfBINs.ATL <- pivot_wider(data = dfBINs.ATL, names_from = bin_uri, values_from = n)

dfBINs.ATL <- dfBINs.ATL %>%
  filter(!is.na(region)) %>%
  remove_rownames() %>%
  column_to_rownames(var = "region")

dfBINs.ATL[is.na(dfBINs.ATL)] <- 0
AccumCurveATL <- specaccum(dfBINs.ATL)

# Pacific Data Processing
dfBINs.PAC <- dfPacific %>%
  group_by(region) %>%
  count(bin_uri)

dfBINs.PAC <- pivot_wider(data = dfBINs.PAC, names_from = bin_uri, values_from = n)

dfBINs.PAC <- dfBINs.PAC %>%
  filter(!is.na(region)) %>%
  remove_rownames() %>%
  column_to_rownames(var = "region")

dfBINs.PAC[is.na(dfBINs.PAC)] <- 0
AccumCurvePAC <- specaccum(dfBINs.PAC)

# __Regional SA Plots ----
# Now, all accumulation curves are combined into one figure for ease of comparison.
plot(AccumCurveCan, xlab = "Regions Sampled", ylab = "BIN Richness", main = "Marine Decapoda Species Accumulation Curves in Canada", cex.main = 0.9)
plot(AccumCurveATL, add = TRUE, col = "red")
plot(AccumCurvePAC, add = TRUE, col = "blue")
legend(x = "bottomright", legend = c("All of Canada", "Atlantic Coast", "Pacific Coast"), fill = c("black", "red", "blue"), title = "Region of Canada")

# The figure indicates that the Atlantic region is approaching a full estimate of BIN richness more quickly than Canada as a whole, or the pacific region.

# _Commercial Species Accumulation Curves ----
# In order to investigate the association between commercial value and species richness, species accumulation curves are created below. The data is filtered similarly as above, with the addition of the filtering into commercial and non-commercial data sets.

# Commercial Data Processing
dfBINs.COMM <- dfFiltered %>%
  filter(commercial_value == "Commercial") %>%
  group_by(region) %>%
  count(bin_uri)

dfBINs.COMM <- pivot_wider(data = dfBINs.COMM, names_from = bin_uri, values_from = n)

dfBINs.COMM <- dfBINs.COMM %>%
  filter(!is.na(region)) %>%
  remove_rownames() %>%
  column_to_rownames(var = "region")

dfBINs.COMM[is.na(dfBINs.COMM)] <- 0
AccumCurveComm <- specaccum(dfBINs.COMM)

# Non-Commercial Data Processing
dfBINs.NONCOMM <- dfFiltered %>%
  filter(commercial_value != "Commercial") %>%
  group_by(region) %>%
  count(bin_uri)

dfBINs.NONCOMM <- pivot_wider(data = dfBINs.NONCOMM, names_from = bin_uri, values_from = n)

dfBINs.NONCOMM <- dfBINs.NONCOMM %>%
  filter(!is.na(region)) %>%
  remove_rownames() %>%
  column_to_rownames(var = "region")

dfBINs.NONCOMM[is.na(dfBINs.NONCOMM)] <- 0
AccumCurveNonComm <- specaccum(dfBINs.NONCOMM)

# __Commercial SA Plots ----
# The commercial and non-commercial accumulations are plotted. Non-commercial data is plotted first, due to the high sample size.
plot(AccumCurveNonComm, xlab = "Regions Sampled", ylab = "BIN Richness", main = "Marine Decapoda Species Accumulation Curves of Commercial & Non-Commercial Value in Canada", cex.main = 0.9)
plot(AccumCurveComm, add = TRUE, col = 2)
legend(x = "bottomright", legend = c("Non-Commercial Value", "Commercial Value"), fill = c("black", "red"), title = "Decapoda Samples")

# Spec Accumulation Curves suggest that BIN saturation is achieved for commercial species, but not non-commercial, as expected.
# Two main conclusion: more effort needs to be put into sampling in the Pacific region & of non-commercial species in general.



# EDIT 2 encapsulated function : compare accumulation curves (a change in edit one). Calculate Slopes for Comparison and visualize

# Load necessary library
library(ggplot2)

# Define the function
compare_accumulation_slopes <- function(accum_curve1, accum_curve2, label1 = "Group 1", label2 = "Group 2") {
  # Find common sampling points between the two curves
  common_sites <- intersect(accum_curve1$sites, accum_curve2$sites)
  
  # Check if there are common points to calculate slopes
  if (length(common_sites) > 1) {
    # Extract richness values at the common sampling points
    richness1 <- accum_curve1$richness[accum_curve1$sites %in% common_sites]
    richness2 <- accum_curve2$richness[accum_curve2$sites %in% common_sites]
    
    # Calculate slopes at these common points
    slope1 <- diff(richness1) / diff(common_sites)
    slope2 <- diff(richness2) / diff(common_sites)
    
    # Create a data frame to store slope comparisons at common sampling points
    slope_comparison <- data.frame(
      Common_Sites = rep(common_sites[-1], 2),
      Slope = c(slope1, slope2),
      Group = factor(c(rep(label1, length(slope1)), rep(label2, length(slope2))))
    )
    
    # Display the slope comparison table
    print(slope_comparison)
    
    # Visualize slopes for both groups at common sampling points
    ggplot(slope_comparison, aes(x = Common_Sites, y = Slope, color = Group)) +
      geom_line(size = 1) +
      labs(title = "Richness Accumulation Rate (Slope) Comparison",
           x = "Common Sampling Points",
           y = "Richness Accumulation Rate (Slope)") +
      scale_color_manual(values = c("blue", "red")) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(title = "Sample Type"))
    
  } else {
    print("Insufficient common sampling points to calculate slopes at the same sampling number.")
  }
}

# Example usage of the function
# Assuming AccumCurveNonComm and AccumCurveComm are the accumulation curves for two groups
compare_accumulation_slopes(AccumCurveNonComm, AccumCurveComm, label1 = "Non-Commercial", label2 = "Commercial")


#edit 3:Statistical Test: Comparing Slopes Between Groups

# Reshape slope_comparison to the required format
library(tidyr)

slope_comparison_long <- slope_comparison %>%
  pivot_longer(
    cols = c("Non_Comm_Slope", "Comm_Slope"),
    names_to = "Group",
    values_to = "Slope"
  ) %>%
  mutate(Group = if_else(Group == "Non_Comm_Slope", "Non-Commercial", "Commercial"))

# Inspect the new data structure
str(slope_comparison_long)

# Inspect group sizes
print(table(slope_comparison_long$Group))

# Split data into groups
group1_slopes <- slope_comparison_long$Slope[slope_comparison_long$Group == "Non-Commercial"]
group2_slopes <- slope_comparison_long$Slope[slope_comparison_long$Group == "Commercial"]

# Check group sizes
if (length(group1_slopes) < 3 || length(group2_slopes) < 3) {
  cat("Insufficient data for normality tests. Proceeding with non-parametric Mann-Whitney U test.\n")
  wilcox_test_result <- wilcox.test(group1_slopes, group2_slopes)
  print("Mann-Whitney U Test Result (small group sizes):")
  print(wilcox_test_result)
} else {
  # Perform Shapiro-Wilk test for normality
  shapiro_group1 <- shapiro.test(group1_slopes)
  shapiro_group2 <- shapiro.test(group2_slopes)
  
  print("Shapiro-Wilk Test for Normality:")
  print(shapiro_group1)
  print(shapiro_group2)
  
  # Perform t-test or Mann-Whitney U test based on normality
  if (shapiro_group1$p.value > 0.05 && shapiro_group2$p.value > 0.05) {
    t_test_result <- t.test(group1_slopes, group2_slopes)
    print("T-Test Result:")
    print(t_test_result)
  } else {
    wilcox_test_result <- wilcox.test(group1_slopes, group2_slopes)
    print("Mann-Whitney U Test Result:")
    print(wilcox_test_result)
  }
}

