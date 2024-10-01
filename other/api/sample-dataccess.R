
#Install and load required packages
install_and_load <- function(packages, repos = "http://cran.us.r-project.org") {
  # Install any packages that are not yet installed
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    install.packages(new_packages, repos = repos)
  }
  
  # Load all specified packages
  invisible(sapply(packages, function(pkg) {
    suppressMessages(suppressWarnings(require(pkg, character.only = TRUE)))
  }))
}

# Define the required packages
required_packages <- c("httr", "jsonlite", "ggplot2", "dplyr", "tidyr", "plotly")

# Install and load the required packages
install_and_load(required_packages)


#define api url
#usecase data
api_url<-"https://32d7-143-233-242-132.ngrok-free.app/use-case"
#usecase kpi data
api_url_kpi<-"https://32d7-143-233-242-132.ngrok-free.app/kpi"

response<- httr::GET(api_url, query = list(eia_code = "USC001", key= "specifykey" ))
data_USC001<- content(response, as= "text" )
data_USC001<-fromJSON(data_USC001)

#kpi
response_kpi<- httr::GET(api_url_kpi, query = list(eia_code = "USC007",kpi= "yield" , key= "specifykey" ))
kpi_USC007<- content(response_kpi, as= "text" )
kpi_USC007<-fromJSON(kpi_USC007)


head(kpi_USC007)

#### Sample Visualizations

## General View
them3 <- theme(panel.background = element_rect(fill = "white"),
               plot.background = element_rect(fill = "white",
                                              color = NA), panel.grid.major = element_blank(),
               plot.title = element_text(color = "#CD7F32",
                                         size = 13), strip.text.x = element_text(size = 10,
                                                                                 color = "black", face = "bold"),
               axis.text = element_text(color = "black",
                                        face = "bold", size = 10), axis.title.x = element_text(margin = unit(c(5,
                                                                                                               0, 0, 0), "mm")), axis.title = element_text(color = "black",
                                                                                                                                                           size = 12), legend.title = element_blank(),
               legend.text = element_text(size = 12),
               legend.background = element_rect(fill = "white"),
               panel.border = element_blank(), axis.line.x = element_blank(),
               axis.line.y = element_blank())


#data by crop...ui filter options
kpi_USC007_wheat<-kpi_USC007[kpi_USC007$crop=="wheat",]

# Reshape data to wide format
data_filtered <- kpi_USC007_wheat %>%
  select(adm1,trial_id , treatment, yield, landscape_position) %>%
  filter(treatment %in% c("EXN", "LSFR")) %>%
  pivot_wider(names_from = treatment, values_from = yield) 
data_filtered$yield_diff<-data_filtered$LSFR-data_filtered$EXN

# bar plot
ggplot(kpi_USC007_wheat, aes(x = adm1, y = yield, fill = treatment)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(), color = "black") +
  theme_minimal() +
  labs(x = "ADM1", y = "Average Yield", title = "Average Yield by Location and Treatment") +
  scale_fill_manual(values = c("EXN" = "#004080",
                               "LSFR" = "#4caf50",
                               "RSREC" = "#c26e60")) +
  them3


#pie/sunburst
library(plotly)
x <- (data_filtered[! is.na(data_filtered$yield_diff),] )$yield_diff
xi <- x[x<0]
xj <- x[x>0]
pos <- (length(xj)/length(x))*100
neg <- (length(xi)/length(x))*100
ds <- data.frame(labels = c("Yield Difference <br> (LSFR yield - EXN yield)",
                            "Positive change", "Negative change"),
                 values = c(NA, pos, neg))
# Plot the data
plot_ly(data = ds,
        labels = ~labels,
        values = ~values,
        parents = c("", "Yield Difference <br> (LSFR yield - EXN yield)",
                    "Yield Difference <br> (LSFR yield - EXN yield)"),
        type = "sunburst",
        branchvalues = 'total',
        textinfo = "label+percent entry",
        hoverinfo = "text",
        hovertext = paste("% of farmers experiencing<br>",
                          tolower(ds$labels), "from LSFR")) 



##Detailed view
#scatter plot

# Create the scatter plot
ggplot(data_filtered, aes(x = EXN, y = LSFR)) +
  geom_point(aes(color=landscape_position,shape=landscape_position),size = 3) +
  facet_wrap(~ adm1) +
  geom_abline(slope = 1, intercept = 0, size = 0.5, colour = "grey") +
  labs(title = "Scatter Plot of EXN Yield vs. LSFR Yield",
       x = "Yield of EXN",
       y = "Yield of LSFR") +
  them3






