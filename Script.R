# 1 Descargar #### 

url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url = url, destfile = "data/monitoring_data.zip")
# Descomprimir 
unzip(zipfile = "data/monitoring_data.zip", exdir = "data/monitoring_data", overwrite = TRUE)

# 2 Cargar #### 

if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(dplyr, tidyverse, ggplot2, gtsummary, knitr, kableExtra) 

activity_data <- read.csv("data/monitoring_data/activity.csv")

# Pregunta 1 ####
# What is mean total number of steps taken per day?
## Datos
totales <- activity_data %>% 
  group_by(date) %>% 
  summarise(Totales = sum(steps), .groups = 'drop') %>%
  ungroup()

## Grafico
ggplot(totales, aes(x = Totales)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +   
  labs(title = "Histogram of Total Steps per Day",
       x = "Total Steps",
       y = "Frequency") +
  theme_minimal()

## Media y Promedio
summary_table_totales <- totales %>%
  tbl_summary(
    include = Totales,  
    statistic = list(all_continuous() ~ "{mean} ({median})"),   
    digits = list(Totales ~ 2)   
  ) %>%
  modify_header(label = "**Variable**", stat_0 = "**Summary**") %>%   
  bold_labels() 

summary_table_totales

# Pregunta 2 ####
# What is the average daily activity pattern?

## Datos 
activity_pattern <- activity_data %>% 
  na.omit() %>% 
  group_by(interval) %>% 
  summarise(mean = mean(steps))

ggplot(activity_pattern, aes(x = interval, y = mean)) +
  geom_line(color = "steelblue", size = 1) +  
  geom_point(color = "darkorange", size = 2, alpha = 0.6) +  
  labs(
    title = "Average Daily Activity Pattern", 
    subtitle = "Mean number of steps taken per 5-minute interval",  
    x = "5-Minute Interval",  
    y = "Average Number of Steps"  
  ) +
  theme_minimal() +  
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),   
    plot.subtitle = element_text(size = 12, hjust = 0.5),   
    axis.title.x = element_text(size = 12, face = "bold"),   
    axis.title.y = element_text(size = 12, face = "bold"),   
    axis.text.x = element_text(angle = 45, hjust = 1)   
  ) +
  scale_x_continuous(breaks = seq(0, 2400, by = 200)) +   
  scale_y_continuous(breaks = seq(0, max(activity_pattern$mean, na.rm = TRUE), by = 25)) +   
  annotate("text", x = 800, y = max(activity_pattern$mean, na.rm = TRUE) * 0.9, 
           label = "Peak Activity", color = "red", size = 5)   
 

## Datos
max_mean_row <- activity_pattern %>%
  filter(mean == max(mean, na.rm = TRUE))

# Create a summary table
summary_table <- activity_pattern %>%
  tbl_summary(
    include = mean,  
    statistic = list(all_continuous() ~ "{max}"),   
    digits = list(mean ~ 2) 
  ) %>%
  modify_header(label = "**Variable**", stat_0 = "**Summary**") %>%   
  bold_labels() 

# Add a row for the interval corresponding to the maximum mean value
summary_table <- summary_table %>%
  modify_table_body(
    ~ .x %>%
      add_row(
        variable = "Interval with Max Mean",
        label = "Interval",
        stat_0 = as.character(max_mean_row$interval)  # Convert interval to character
      )
  )

# Print the summary table
summary_table

# Print the summary table
summary_table

# Pregunta 3 ####
## Imputing missing values

na_activity <- activity_data%>% 
  is.na() %>% 
  as.data.frame() %>% 
  summarise(na_steps = sum(steps), 
            na_dates = sum(date), 
            na_interval = sum(interval))

na_activity %>%
  kable(
    caption = "Summary of Missing Values (NA) in Activity Data",  # Add a caption
    col.names = c("NA in Steps", "NA in Dates", "NA in Intervals"),  # Rename columns
    align = "c"  # Center-align the table content
  ) %>%
  kable_styling(  # Add styling to the table
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    position = "center"
  )  

## Valores de NA
activity_data_filled <- activity_data %>%
  left_join(activity_pattern, by = "interval") %>%   
  mutate(steps = ifelse(is.na(steps), mean, steps)) %>% 
  select(-mean)  # Eliminar la columna temporal "mean"

## totales na corregido
totales_na <- activity_data_filled %>% 
  group_by(date) %>% 
  summarise(Totales = sum(steps), .groups = 'drop') %>%
  ungroup()


## Grafico
ggplot(totales_na, aes(x = Totales)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +   
  labs(title = "Histogram of Total Steps per Day",
       x = "Total Steps",
       y = "Frequency") +
  theme_minimal()

## Media y Promedio
summary_table_na <- totales_na %>%
  tbl_summary(
    include = Totales,  
    statistic = list(all_continuous() ~ "{mean} ({median})"),   
    digits = list(Totales ~ 2)   
  ) %>%
  modify_header(label = "**Variable**", stat_0 = "**Summary**") %>%   
  bold_labels() 

summary_table_na

# Pregunta 4 ####
## Are there differences in activity patterns between weekdays and weekends?

## Datos
dias <- activity_data_filled %>%
  mutate(Type = factor(
    case_when(
      wday(date) %in% c(1, 7) ~ "Weekend",   
      TRUE ~ "Weekday" 
    ),
    levels = c("Weekday", "Weekend") 
  ))

## Grafico

dias %>%
  group_by(interval, Type) %>%
  summarise(mi = mean(steps, na.rm = TRUE)) %>%  
  ggplot(aes(x = interval, y = mi, color = Type)) +  
  geom_line(linewidth = 1) +  
  labs(
    x = "Interval",
    y = "Average Number of Steps",
    title = "Average Steps per Interval by Day Type",
    color = "Day Type"   
  ) +
  facet_grid(Type ~ ., scales = "free_y") + 
  theme_minimal() +  
  theme(
    strip.text = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"  
  )
