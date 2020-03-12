##########################
### R script by Ivan Prates (ivanprates.org).
### Smithsonian National Museum of Natural History, Washington, DC, USA.
### November 2019.

### The goals of this R script are:
### 1. To select variables for environmental analyses;
### 2. To perform PCA analyses on the environmental analyses;
### 3. To make scatter plots with the first PC axes;
### 4. To make violin plots with the first PC axes.

## ## PART 1: Getting ready:

## Install packages:
#install.packages("cowplot")
#install.packages("dplyr")
#install.packages("factoextra")
#install.packages("ggplot2")
#install.packages("magrittr")
#install.packages("psych")
#install.packages("reshape2")

## Load packages:
library(cowplot)
library(dplyr)
library(factoextra)
library(ggplot2)
library(magrittr)
library(psych)
library(reshape2)

## Setting working directory:
path = "~/Dropbox/Science/MYPAPERS_ongoing/2020_fuscoauratus/2020_gh_fuscoauratus/environmental_analyses/"
#path = "C:/Users/RhinellaX/Dropbox/Science/MYPAPERS_ongoing/2020_fuscoauratus/2020_gh_fuscoauratus/environmental_analyses/"
setwd(path)

## Importing environmental data extracted form GIS layers:
read_data = read.csv(file = "fusco_environmental_data_2019-10.csv", header = TRUE)
environ_data = read_data[, c(2, 7:27)] # Selecting only environmental data and dewlap groups.

# Change column names:
colnames(environ_data) = c("dewlap_group", 
                           "Precipitation of driest quarter", # Bio 17
                           "Precipitation of wettest quarter", # Bio 16
                           "Precipitation of driest month", # Bio 14
                           "Precipitation of wettest month", # Bio 13
                           "Annual precipitation", # Bio 12
                           "Mean temp. of coldest quarter", # Bio 11
                           "Mean temp. of warmest quarter", # Bio 10
                           "Min temp. of coldest month", # Bio 06
                           "Max temp. of warmest month", # Bio 05
                           "Annual mean temperature", # Bio 01
                           "Climatic moisture index",
                           "Mean annual cloud cover",
                           "Elevation",
                           "Slope",
                           "Topographic ruggedness index",
                           "Topographic roughness",
                           "Shrub cover",
                           "Flooded forest cover",
                           "Herbaceous vegetation cover",
                           "Deciduous broadleaf forest cover",
                           "Evergreen broadleaf forest cover")

## Take a look at environ_data:
#View(environ_data)
dim(environ_data)

## Checking some min and max values:
## Annual mean temperature:
max(environ_data$`Annual mean temperature`)
min(environ_data$`Annual mean temperature`)
max(environ_data$`Annual mean temperature`)/min(environ_data$`Annual mean temperature`)

## Annual precipitation:
max(environ_data$`Annual precipitation`)
min(environ_data$`Annual precipitation`)
max(environ_data$`Annual precipitation`)/min(environ_data$`Annual precipitation`)

## Elevation:
max(environ_data$Elevation)
min(environ_data$Elevation)
max(environ_data$Elevation)/min(environ_data$Elevation)

## Vegetation cover:
max(environ_data$`Evergreen broadleaf forest cover`)
min(environ_data$`Evergreen broadleaf forest cover`)
max(environ_data$`Evergreen broadleaf forest cover`)/min(environ_data$`Evergreen broadleaf forest cover`)

## PART 2: Plotting variable ranges for each A. fuscoauratus dewlap.

## Melting dataset, as required by ggplot to plot several variables side by side:
melt_to_plot = melt(environ_data, id.vars = "dewlap_group")

## Plottin the count data. Creating new plot:
var_violin_plot = ggplot(as.data.frame(melt_to_plot), aes(x = dewlap_group, y = value, fill = dewlap_group)) +     
  
  ## Defining plot type (violin):
  ## Bars side by side. Trim = TRUE will truncate plot above and below the observed value range.
  geom_violin(alpha = 0.7, color = "gray40", scale = "area", trim = TRUE)  +
  
  ## Adding individual points on top of plot:
  #geom_point(color = "gray40") + # Coincident points on top of each other.
  geom_dotplot(color = "gray40", binaxis = 'y', stackdir = 'center', dotsize = 1.2, fill = "gray40") + # Spread coincident dots side by side.
  
  ## Changing plot basic theme:
  theme_bw() +
  
  ## Changing text parameters:
  theme(
    #legend.position = c(0.85, 0.17), # Setting position of legend on chart.
    legend.position = "none", # Setting position of legend on chart.
    text = element_text(size = 20), # Changing font size on entire plot to change that of the title of the y axis.
    axis.text.y = element_text(size = 16), # Changing font size on y axis.
    axis.title.y = element_blank(), # Removing the title of the y axis.
    axis.title.x = element_blank(), # Removing the title of the x axis.
    axis.text.x = element_blank(), # Getting text on x axis.
    axis.ticks.x = element_blank(), # Getting rid of ticks on x axis.
    panel.grid.major.x = element_blank(), # Getting rid of all grid lines on x axis.
    panel.grid.minor = element_blank() # Getting rid of minor grid lines on y axis.
    ) +
  
  ## Adjusting scale color and labels:
  scale_fill_manual(values = c("#c5beab", "#ec95b4", "#f4ee32")) +

  facet_wrap(~variable, scales = "free_y") #+
  
  ## Save plot in pdf format:
  save_plot(plot = var_violin_plot, base_height = 10, base_width = 20,
            filename = paste0(path, "dewlap_by_environment_violin_plot_variables.pdf"))
  
## PART 3: PCA analyses

## Test for correlations between selected variables:
corr.env = cov2cor(cov(environ_data[, -1]))
correlation_results = abs(corr.env) >= 0.7

## Saving results of correlation test:
write.csv(x = correlation_results, file = paste0(path, "environmental_variables_correlation_results.csv"))

## Selecting data to use:
environ_selected = environ_data %>% select( 
                        "Precipitation of wettest quarter", # Bio 16
                        "Precipitation of wettest month", # Bio 13
                        "Annual precipitation", # Bio 12
                        "Mean temp. of warmest quarter", # Bio 10
                        "Max temp. of warmest month", # Bio 05
                        "Annual mean temperature", # Bio 01
                        "Climatic moisture index",
                        "Mean annual cloud cover",
                        "Elevation",
                        "Slope",
                        "Topographic ruggedness index",
                        "Topographic roughness",
                        "Shrub cover",
                        "Flooded forest cover",
                        "Herbaceous vegetation cover",
                        "Deciduous broadleaf forest cover",
                        "Evergreen broadleaf forest cover"
                        )

## Take a look at the environ_all data:
#View(environ_selected)
dim(environ_selected)

## Implement PCA on a selection of more easily interpretable variables:
pca_selected = prcomp(x = environ_selected, retx = TRUE, center = TRUE, scale. = TRUE)

## Check PCA results:
pca_selected$rotation[1:10, 1:3] ## Loadings.
pca_selected$x[1:10, 1:3] ## PC scores.
View(summary(pca_selected$rotation))

## Saving PCA loadings:
write.csv(x = pca_selected$rotation, file = paste0(path, "environmental_variables_PCA_loadings.csv"))

## To check the correlations between the principal components' scores and the initial data:
head(t(pca_selected$rotation)*pca_selected$sdev)

## Converting pc score table to data frame:
pca_data_selected = as.data.frame(pca_selected$x)

## Adding dewlap info to PCA score table:
pca_data_selected$dewlap_group = environ_data$dewlap_group

## Visualizing the amount of variation captured by each PC axis:
fviz_eig(pca_selected)

## Visualizing variable correlation in PC space, and saving plot:
pca_var_plot = fviz_pca_var(pca_selected,  
               axes = c(1, 2), col.var = "contrib", # Color by contributions to the PC
               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), ## Color pallete for contributions.
               repel = TRUE)     # Avoid text overlapping
  
  ## Save plot in pdf format:
  save_plot(plot = pca_var_plot, base_height = 7, base_width = 7,
          filename = paste0(path, "dewlap_by_environment_PCA_variable_contributions.pdf"))

## PART 4: Running ANOVAs:

## Selecting data to use:
pca_data = pca_data_selected[ , 1:3]

## Add dewlap info back:
pca_data$dewlap_group = environ_data$dewlap_group # Adding dewlap info to PCA score table.

## Run analysis of variance:

## First, creating a function to be used with different PC axes:
PC.anova = function(var_anova)
{  
  anova = aov(pca_data[, var_anova] ~ dewlap_group, data = pca_data)

  ## Summary of the analysis:
  summary(anova)

  ## Testing homogeneity of variances:
  #plot(anova, 1)

  ## Testing normality:
  #plot(anova, 2)

  ## Poshoc comparisons using Tukey's HSD test:
  ## To be run if the ANOVA is significant.
  TukeyHSD(anova)
  
  }

## Run ANOVA function for each PC axis and check for outliers:
PC.anova(var_anova = "PC1")
PC.anova(var_anova = "PC2")
PC.anova(var_anova = "PC3")

## Removing outliers:
pca_data = pca_data[-c(10, 19, 25, 29), ] ## From Q-Q Plots.

## Run ANOVA function for each PC axis, now without outliers:
PC.anova(var_anova = "PC1")
PC.anova(var_anova = "PC2")
PC.anova(var_anova = "PC3")

## PART 5: PCA plots:

## First Writing a function for pairwise PC plots:
plot.2.pcs <- function(var1, var2)
{
  ## Plot with ggplot: Removing outliers:  
  pca_plot = ggplot(data = pca_data) + 
  
  ## Plotting ellipses around the points corresponding to each dewlap:
  stat_ellipse(aes(x = pca_data[, var1], y = pca_data[, var2], color = dewlap_group), level = 0.95, size = 5) +
  
  ## Plot type: Scatterplot:
  geom_point(aes(x = pca_data[, var1], y = pca_data[, var2], fill = dewlap_group), size = 15, shape = 21, stroke = 2) +
  
  ## Line color of ellipses:
  scale_color_manual(values = c("#c5beab", "#ec95b4", "#f4ee32"), guide = "none") +
    
  ## Color of points:
  scale_fill_manual(values = c("#c5beab", "#ec95b4", "#f4ee32"), guide = "none") +
  
  ## Setting general theme:
  theme_light(base_size = 70) +
  
  ## Adjusting labels:
  #labs(title = paste0(var1, " vs. ", var2)) +
  labs(y = var2, x = var1) +
  
  ## Other ggplot parameters:
  theme(
    panel.border = element_rect(size = 7, colour = "gray30"), ## Changing thickness of lines around plot.
    plot.title = element_text(hjust = 0.5, size = 70, margin = margin(t = 0, r = 0, b = 20, l = 0)), ## Centralizing plot title.
    axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0)), ## Changing title of the x axis.
    axis.title.y = element_text(margin = margin(t = 0, r = 3, b = 0, l = 0)), ## Changing title of the y axis.
    axis.text.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0), size = 70, color = "black"), ## Font size and distance of axis values from plot.
    axis.text.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0), size = 70, color = "black"), ## Font size and distance of axis values from plot.
    axis.ticks = element_line(size = 5, color = "gray30"), ## Making axis thicks thicker.
    axis.ticks.length = unit(.75, "cm"), ## Making axis ticks longer.
    panel.grid = element_blank()) ## Removing background grid.

  ## Save plot in pdf format:
  save_plot(plot = pca_plot, base_height = 15, base_width = 20,
            filename = paste0(path, "dewlap_by_environment_PCA_plot_", var1, "_vs_", var2, "_no_outs.pdf"))
  
  ## Check plot:
  pca_plot
  
} ## End of function.

## Apply function to combinations of PCs:
plot.2.pcs(var1 = "PC1", var2 = "PC2") ## PC1 vs. PC2.
plot.2.pcs(var1 = "PC1", var2 = "PC3") ## PC1 vs. PC3.
plot.2.pcs(var1 = "PC2", var2 = "PC3") ## PC2 vs. PC3.

## PART 6: Plotting PC ranges for each A. fuscoauratus dewlap.

## Melting dataset, as required by ggplot to plot several variables side by side:
melted_data_plot = melt(pca_data, id.vars = "dewlap_group")

## Plottin the count data. Creating new plot:
violin_pca = ggplot(data = as.data.frame(melted_data_plot)) +
  aes(x = dewlap_group, y = value, fill = dewlap_group) +     
  
  ## Defining plot type (violin):
  ## Bars side by side. Trim = TRUE will truncate plot above and below the observed value range.
  geom_violin(alpha = 0.7, color = "gray40", scale = "area", trim = FALSE, size = 4)  +
  
  ## Adding individual points on top of plot:
  #geom_point(color = "gray40") + # Coincident points on top of each other.
  geom_dotplot(color = "gray40", binaxis = 'y', stackdir = 'center', dotsize = 1.2, fill = "gray40") + # Spread coincident dots side by side.
  
    ## Adjusting labels:
  labs(y = "PC scores") +
  
  ## Changing plot basic theme:
  theme_light() +
  
  ## Changing text parameters:
  theme(
    panel.border = element_rect(size = 7, colour = "gray30"), ## Changing thickness of lines around plot.
    legend.position = "none", # Setting position of legend on chart.
    text = element_text(size = 70), # Changing font size on entire plot to change that of the title of the y axis.
    axis.text.y = element_text(size = 70, margin = margin(t = 0, r = 20, b = 0, l = 20), color = "black"), # Changing font size on y axis, margins.
    #axis.title.y = element_blank(), # Removing the title of the y axis.
    axis.title.x = element_blank(), # Removing the title of the x axis.
    axis.text.x = element_text(size = 70, margin = margin(t = 20, r = 0, b = 0, l = 0), color = "black"),
    #axis.text.x = element_blank(), # Getting rid of text on x axis.
    #axis.ticks.x = element_blank(), # Getting rid of ticks on x axis.
    axis.ticks.y = element_line(size = 5, color = "gray30"), ## Making axis thicks thicker.
    panel.grid.major = element_blank(), # Getting rid of all grid lines on x and y axis.
    panel.grid.minor = element_blank(), # Getting rid of minor grid lines on x and y axis.
    axis.ticks.length = unit(.75, "cm"), ## Making axis ticks longer.
    axis.ticks = element_line(size = 5, color = "gray30") ## Making axis thicks thicker.
    ) +
  
  ## Adjusting scale color and labels:
  scale_fill_manual(values = c("#c5beab", "#ec95b4", "#f4ee32")) +
                    
  ## Plotting plots side-by-side using a facet:
  facet_wrap(~variable, scales = "free_y") +
  
  ## Change the aspect of plot titles in facet:
  theme(
    strip.background = element_blank(), ## Removing boxes behind titles.
    strip.text.x = element_text(size = 70, color = "black", ## Changing title size, color, margins.
                                margin = margin(t = 20, r = 0, b = 20, l = 0)))

## Check plot:
violin_pca
  
## Save plot in pdf format:
save_plot(plot = violin_pca, base_height = 15, base_width = 60, limitsize = FALSE,
            filename = paste0(path, "dewlap_by_environment_violin_plot_PCs_no_outs.pdf")) 
            
## All done!
