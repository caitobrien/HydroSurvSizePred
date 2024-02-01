set.seed(100)

extrafont::loadfonts(device="win")
library(ggplot2)
library(scales)
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
options(knitr.table.format = "html")
library(viridis)
library(tidyr)


DATA <-
  read.csv(
    "~/R Scripts/SalmonLength-Pred-Surv/LWG2AdultGrowth.csv",
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )
# nrows = 600000)

# ----- SET DATA FILTERS & ADJUSTMENTS ------

# Filter fish data to those between 55-165mm
DATA <- DATA[DATA$mark_length %in% (55:165),]

# Dates need to be converted to POSIXct format
DATA$lwg_last <- strptime(DATA$lwg_last, format = "%Y-%m-%d")
DATA$lwg_last <- as.POSIXct(DATA$lwg_last)

DATA$bon_last <- strptime(DATA$bon_last, format = "%Y-%m-%d")
DATA$bon_last <- as.POSIXct(DATA$bon_last)

# Smolt migration season
DATA$lwg_DOY <-
  as.numeric(format(strptime(DATA$lwg_last, "%Y-%m-%d"), "%j"))
DATA <- DATA[DATA$lwg_DOY %in% (80:160),]

DATA$bon_DOY <-
  as.numeric(format(strptime(DATA$bon_last, "%Y-%m-%d"), "%j"))
DATA <- DATA[DATA$bon_DOY %in% (80:160),]

# Add year column
DATA$year <- format(DATA$lwg_last, format = "%Y")

# Order data by year chronologically
DATA <-
  DATA[order(DATA$year), ]                   # Order data by date


# ------ FISH FREQUENCY VS. YEAR --------------------------------------------- #

# Plot the histogram
# Oldest Date To Newest
##---------------------
# limits = c(as.POSIXct("1988-08-02"),
#            as.POSIXct("2040-06-24")) )

# [1*] However, x-axis dates disappear due to data size
# Revised to second latest date -> 2021
# & Revised data breaks from 3 months -> 1 year
# [2*] Graphing a overlay graph bt LWG & BON displays
# no major differences - too much overlap

start_date = as.POSIXct("1988-04-20")
end_date = as.POSIXct("2022-07-19")

allYearPlot <- ggplot(DATA, aes(lwg_last, ..count..)) +
  geom_histogram(bins=30) +
  theme_bw() +
  ggtitle("Smolts vs. Year") +
  theme(plot.title = element_text(hjust = 0.5)) + # Used to center main title, o.w. would be default left aligned
  xlab("Year") +
  ylab("Count") +
  scale_x_datetime(
    breaks = date_breaks("1 year"),
    labels = date_format("%Y"),
    limits = c(start_date,  # 1988-04-20
               end_date)
  )


# ----- FACET HISTOGRAM WRAP ------------------------------------------------- #

# Create the facet histogram graph

FACET_DATA <-
  DATA %>%
  group_by(year) %>%
  ungroup()

# lwg_display = T
# bon_display = T
# ```{r fig.height = 12, fig.width = 5}
# Find out how to change histogram color for LWG
all_year_facet <- function(lwg_display,
                           bon_display,
                           north_pike_threshold,
                           avian_predator_threshold) {
  
  
  print(paste0("lwg ", lwg_display))
  print(paste0("bon ", bon_display))
  print(paste0("pike ", north_pike_threshold))
  print(paste0("pred ", avian_predator_threshold))
  
 
  ggplot(data = FACET_DATA) +
    
    {
      if (lwg_display)
        geom_histogram(aes(x = mark_length, y = ..count..,
                           fill = "LWG"),
                       alpha = 0.3, bins=30) 
        
      
    } +
    {
      if (bon_display)
        geom_histogram(aes(x = bon_length,
                           fill = "BON"),
                       alpha = 0.3, bins=30)
    } +
    
    scale_fill_manual(name = "Location", values = c("BON" = "#F8766D", "LWG" = "#00BFC4")) +
    #geom_density(col="red") + 
    geom_vline(xintercept = 125,
               linetype = "dotted",
               color = "blue")  +
    
    # {if (north_pike_threshold)
    #     geom_vline(xintercept = 125,
    #              linetype = "dotted",
    #              color = "blue")
    # } +                                 #, size=1.5) +
    
    geom_vline(xintercept = avian_predator_threshold,
               linetype = "dotted",
               color = "red") +
    
  ggtitle("Facet Graph With LWG & BON") +
    xlab("Fork Length") +
    ylab("Number of Smolts") +
    # facet_wrap( ~ fct_rev(year), scales = "free", ncol = 4) # scales: plot fits its own values
    facet_grid( rows = vars(fct_rev(year))) # scales: plot fits its own values
 
  }
all_year_facet(T, T, F, 150)
# ```


#-----  MONTH FACET GRAPH -----------------------------------------------------#
months_list = c(
  'January',
  'February',
  'March',
  'April',
  'May',
  'June',
  'July',
  'August',
  'September',
  'October',
  'November',
  'December'
)

# Convert to date data type and add year
DATA$lwg_last <- as.Date(DATA$lwg_last)
DATA$bon_last <- as.Date(DATA$bon_last)

GV_YEAR = 1995
TEMP_DATA = DATA %>% filter(DATA$year %in% GV_YEAR)
TEMP_DATA$lwg_last <- as.Date(TEMP_DATA$lwg_last)
TEMP_DATA$lwg_month <-  format(TEMP_DATA$lwg_last, "%B")
TEMP_DATA$bon_month <-  format(TEMP_DATA$bon_last, "%B")


TEMP_DATA$lwg_month <- as.character(TEMP_DATA$lwg_month)

TEMP_DATA$lwg_month = factor(TEMP_DATA$lwg_month,
                             levels = months_list)

TEMP_DATA$bon_month = factor(TEMP_DATA$bon_month,
                             levels = months_list)

NEW_DATA <-
  select(TEMP_DATA, lwg_month, bon_month, lwg_length, bon_length)
MONTH_DATA <- NEW_DATA |>
  pivot_longer(everything(),
               names_sep = "_",
               names_to = c("type", ".value"))


# Location Filtering - For Shiny
LWG = "lwg"
BON = "bon"

month_facet <- function(lwg_display, bon_display, 
                        north_pike_threshold, avian_predator_threshold)  
  {
    if (lwg_display & bon_display){
      filter_location = c(LWG, BON)
    } else if (lwg_display){
      filter_location = c(LWG)
    } else if (bon_display){
      filter_location = c(BON)
    }
    
    FILTERED_DATA = filter(MONTH_DATA, type %in% filter_location)
    
    manual_color = data.frame(type=c("lwg", "bon"), col=c("#00BFC4", "#F8766D"))
    manual_color <- filter(manual_color, type %in% filter_location)
    
    
    monthPlot <- ggplot(FILTERED_DATA, aes(x = length, fill = type)) +
      geom_histogram(alpha = 0.6,
                     binwidth = 5,
                     position = "identity", 
                     bins=30) +
      scale_fill_manual(name = "Location", values = manual_color$col) +
      #scale_fill_viridis(discrete = TRUE) +
      #scale_color_viridis(discrete = TRUE) +
      theme_ipsum() +
      theme(
        legend.position = "right",
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      geom_vline(xintercept = 125,
                 linetype = "dotted",
                 color = "blue")  + 
      
      
      # {if (north_pike_threshold) 
      #   geom_vline(xintercept = 125,
      #              linetype = "dotted",
      #              color = "blue")
      # } +        
      
      # geom_vline(xintercept = avian_predator_threshold,
      #            linetype = "dotted",
      #            color = "red") +
      
      ggtitle(paste0("By Month Facet Graph - ", GV_YEAR)) +
      xlab("Fork Length") +
      ylab("Number of Smolts") +
      #scale_fill_discrete(name = "Location") +
      facet_wrap( ~ month)
    
    monthPlot
}

# Boxplot testing 
# par(mar=c(0, 3.1, 1.1, 2.1))
# boxplot(my_variable , horizontal=TRUE , ylim=c(-10,20), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
# par(mar=c(4, 3.1, 1.1, 2.1))
# hist(my_variable , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab="value of the variable", xlim=c(-10,20))

# Plot Month Histograms
# Q: Reverse LWG & BON legend labels?

monthPlot <- ggplot(MONTH_DATA, aes(x = length, fill = type)) +
  geom_histogram(alpha = 0.6,
                 binwidth = 5,
                 position = "identity", 
                 bins=30) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "right",
    panel.spacing = unit(2, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  ggtitle(paste0("By Month Facet Graph - ", GV_YEAR)) +
  xlab("Fork Length") +
  ylab("Number of Smolts") +
  scale_fill_discrete(name = "Location") +
  facet_wrap( ~ month)
monthPlot
# x <-rnorm(200)
# monthPlot + lines(MONTH_DATA$length, col = 4, lwd = 2)

#----- HALF MONTH FACET GRAPH ------------------------------------------------ #
halfMonths_list = c(
  'Early March',
  'Late March',
  'Early April',
  'Late April',
  'Early May',
  'Late May',
  'Early June',
  'Late June',
  'Early July',
  'Late July',
  'Early August',
  'Late August'
)

#TEMP_DATA <- subset (TEMP_DATA, select = -day)
TEMP_DATA$lwg_day <- format(TEMP_DATA$lwg_last, format = "%d")
TEMP_DATA$bon_day <- format(TEMP_DATA$bon_last, format = "%d")

TEMP_DATA$lwg_halfMonth <-ifelse(TEMP_DATA$lwg_day %in% seq(1, 15),  
                                (paste0("Early ",TEMP_DATA$lwg_month)),  
                                (paste0("Late ",TEMP_DATA$lwg_month)))

TEMP_DATA$bon_halfMonth <-ifelse(TEMP_DATA$bon_day %in% seq(1, 15),  
                                (paste0("Early ",TEMP_DATA$bon_month)),  
                                (paste0("Late ",TEMP_DATA$bon_month)))

TEMP_DATA$lwg_halfMonth = factor(
  TEMP_DATA$lwg_halfMonth,
  levels = halfMonths_list
)

TEMP_DATA$bon_halfMonth = factor(
  TEMP_DATA$bon_halfMonth,
  levels = halfMonths_list
)

NEW_DATA <- select(TEMP_DATA,lwg_halfMonth, bon_halfMonth, lwg_length, bon_length)
HALFMONTH_DATA <- NEW_DATA |>
  pivot_longer(everything(), names_sep = "_", names_to = c("type", ".value"))

# Plot Half Month (~2 weeks) Histogram 

half_month_facet <- function(lwg_display, bon_display, 
                        north_pike_threshold, avian_predator_threshold)  
{
  if (lwg_display & bon_display){
    filter_location = c(LWG, BON)
  } else if (lwg_display){
    filter_location = c(LWG)
  } else if (bon_display){
    filter_location = c(BON)
  }    
  
  ggplot(HALFMONTH_DATA, aes(x = length, fill = type)) +
      geom_histogram(alpha = 0.6, binwidth = 5, position = "identity", bins=30) +
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis(discrete = TRUE) +
      theme_ipsum() +
      theme(
        legend.position = "right",
        panel.spacing = unit(2, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      
      ggtitle(paste0("By Half Months Facet Graph - ", GV_YEAR)) +
      xlab("Fork Length") +
      ylab("Number of Smolts") +
      
      scale_fill_discrete(name = "Location", labels = c("BON", "LWG")) +
      facet_wrap(~halfMonth)
}

ggplot(HALFMONTH_DATA, aes(x = length, fill = type)) +
  geom_histogram(alpha = 0.6, binwidth = 5, position = "identity", bins=30) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position = "right",
    panel.spacing = unit(2, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  
  ggtitle(paste0("By Half Months Facet Graph - ", GV_YEAR)) +
  xlab("Fork Length") +
  ylab("Number of Smolts") +
  
  scale_fill_discrete(name = "Location", labels = c("BON", "LWG")) +
  facet_wrap(~halfMonth)



















