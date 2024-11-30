README
================

``` r
knitr::opts_chunk$set(echo = TRUE,warning = FALSE, message = FALSE)
```

Download that from GitHub Repo: <https://github.com/lopezbec/Hexad-12_Spanish_CHI2025>

``` r
# Define the GitHub raw CSV URL
github_url <- "https://raw.githubusercontent.com/lopezbec/Hexad-12_Spanish_CHI2025/refs/heads/main/Hexad_12_Spanish_data.csv"

# Define a local file path to save the downloaded file
local_file <- tempfile(fileext = ".csv")

# Download the file
download.file(github_url, local_file)

# Load the CSV into a data frame
data <- read.csv(local_file,row.names = 1)

#remove 1st non-value columns


# Display the first few rows of the data frame
head(data)
```

    ##    philanthropist socializer freeSpirit achiever player disruptor A2 A4 D3 D4
    ## 4              14         14         13       14      7         7  7  7  6  1
    ## 5              14         14         14       14     14         2  7  7  1  1
    ## 6              13         10         11        9     13        10  3  6  4  6
    ## 7              13          2         14        9      2         5  5  4  4  1
    ## 9              13          6         13       13      6         7  6  7  6  1
    ## 12             14         14         14       14     14        10  7  7  5  5
    ##    F1 F3 P1 P4 R2 R4 S2 S4 Completion.Time Age Gender
    ## 4   6  7  7  7  5  2  7  7          85.868  75   <NA>
    ## 5   7  7  7  7  7  7  7  7          75.567  NA   <NA>
    ## 6   5  6  7  6  7  6  6  4          42.281  NA   <NA>
    ## 7   7  7  7  6  1  1  1  1          66.028  69 female
    ## 9   6  7  7  6  1  5  1  5          91.748  72 female
    ## 12  7  7  7  7  7  7  7  7          41.451  65 female
    ##                                 Country                        Education
    ## 4                                                                       
    ## 5                                                                       
    ## 6                                                                       
    ## 7                             Argentina Technical or vocational training
    ## 9  Venezuela (República Bolivariana de)              Professional degree
    ## 12                            Argentina                                 
    ##    Freq.Games
    ## 4           1
    ## 5          NA
    ## 6          NA
    ## 7           7
    ## 9           7
    ## 12         NA

## General Statistics

#### Number of Participants

Each row represents a data point from a single participant. Total number of participants

``` r
print(nrow(data))
```

    ## [1] 866

#### Completion Time

``` r
# Ensure Completion.Time is numeric
data$Completion.Time <- as.numeric(data$Completion.Time)

# Compute and create summary table
summary_table <- data.frame(
  Statistic = c("Average", "Median", "Min", "Max", "Standard Deviation", "Non-Empty Rows"),
  Value = c(
   round(mean(data$Completion.Time, na.rm = TRUE),2),
    round(median(data$Completion.Time, na.rm = TRUE),2),
    round(min(data$Completion.Time, na.rm = TRUE),2),
    round(max(data$Completion.Time, na.rm = TRUE),2),
    round(sd(data$Completion.Time, na.rm = TRUE),2),
    round(sum(!is.na(data$Completion.Time)),0)
  )
)

# Display the table
kable(summary_table, caption = "Summary Statistics for Completion Time")
```

| Statistic          |  Value |
|:-------------------|-------:|
| Average            |  69.33 |
| Median             |  64.27 |
| Min                |  30.38 |
| Max                | 143.52 |
| Standard Deviation |  23.52 |
| Non-Empty Rows     | 866.00 |

Summary Statistics for Completion Time

#### Age

``` r
# Ensure Completion.Time is numeric
data$Age <- as.numeric(data$Age)

# Compute and create summary table
summary_table <- data.frame(
  Statistic = c("Average", "Median", "Min", "Max", "Standard Deviation", "Non-Empty Rows"),
  Value = c(
   round(mean(data$Age, na.rm = TRUE),2),
    round(median(data$Age, na.rm = TRUE),2),
    round(min(data$Age, na.rm = TRUE),2),
    round(max(data$Age, na.rm = TRUE),2),
    round(sd(data$Age, na.rm = TRUE),2),
    round(sum(!is.na(data$Age)),0)
  )
)

# Display the table
kable(summary_table, caption = "Summary Statistics for Age")
```

| Statistic          |  Value |
|:-------------------|-------:|
| Average            |  37.89 |
| Median             |  32.00 |
| Min                |  18.00 |
| Max                |  87.00 |
| Standard Deviation |  17.25 |
| Non-Empty Rows     | 737.00 |

Summary Statistics for Age

#### Gender

``` r
# Encode Gender as a factor with specific levels
gender_labels <- c("female", "male", "other")
data$Gender <- factor(data$Gender, levels = gender_labels)

# Compute summary statistics for Gender
gender_counts <- table(data$Gender)
total_count <- sum(gender_counts)
gender_percentages <- round((gender_counts / total_count) * 100, 2)

# Create a summary table
gender_summary_table <- data.frame(
  Gender = c(names(gender_counts), "Total"),
  Count = c(as.numeric(gender_counts), total_count),
  Percentage = c(paste0(gender_percentages, "%"), "100%")
)

# Display the table
kable(gender_summary_table, caption = "Summary of Gender Distribution with Totals")
```

| Gender | Count | Percentage |
|:-------|------:|:-----------|
| female |   369 | 58.76%     |
| male   |   256 | 40.76%     |
| other  |     3 | 0.48%      |
| Total  |   628 | 100%       |

Summary of Gender Distribution with Totals

#### Education Level

``` r
# Encode Education Level as a factor with specific levels
educationlevel_labels <- c(
  "High School diploma",
  "Technical or vocational training",
  "Associate's degree",
  "Bachelor's degree",
  "Master's degree",
  "Professional degree",
  "Doctorate"
)

data$Education <- factor(data$Education, levels = educationlevel_labels)

# Compute counts and percentages
education_counts <- table(data$Education)
total_count <- sum(education_counts)
education_percentages <- round((education_counts / total_count) * 100, 2)

# Create a summary table
education_summary_table <- data.frame(
  EducationLevel = c(names(education_counts), "Total"),
  Count = c(as.numeric(education_counts), total_count),
  Percentage = c(paste0(education_percentages, "%"), "100%")
)

# Display the summary table
kable(education_summary_table, caption = "Summary of Education Level Distribution with Totals")
```

| EducationLevel                   | Count | Percentage |
|:---------------------------------|------:|:-----------|
| High School diploma              |   307 | 43.48%     |
| Technical or vocational training |   117 | 16.57%     |
| Associate’s degree               |    13 | 1.84%      |
| Bachelor’s degree                |   168 | 23.8%      |
| Master’s degree                  |    20 | 2.83%      |
| Professional degree              |    78 | 11.05%     |
| Doctorate                        |     3 | 0.42%      |
| Total                            |   706 | 100%       |

Summary of Education Level Distribution with Totals

#### Playing Days

``` r
# Ensure Completion.Time is numeric
data$Freq.Games <- as.numeric(data$Freq.Games)

# Compute and create summary table
summary_table <- data.frame(
  Statistic = c("Average", "Median", "Min", "Max", "Standard Deviation", "Non-Empty Rows"),
  Value = c(
   round(mean(data$Freq.Games, na.rm = TRUE),2),
    round(median(data$Freq.Games, na.rm = TRUE),2),
    round(min(data$Freq.Games, na.rm = TRUE),2),
    round(max(data$Freq.Games, na.rm = TRUE),2),
    round(sd(data$Freq.Games, na.rm = TRUE),2),
    round(sum(!is.na(data$Freq.Games)),0)
  )
)

# Display the table
kable(summary_table, caption = "Summary Statistics for Frequency of Play")
```

| Statistic          |  Value |
|:-------------------|-------:|
| Average            |   2.70 |
| Median             |   2.00 |
| Min                |   1.00 |
| Max                |   7.00 |
| Standard Deviation |   2.06 |
| Non-Empty Rows     | 648.00 |

Summary Statistics for Frequency of Play

#### Country of Origin

``` r
# Filter the data to exclude empty Country values
filtered_data <- data$Country[data$Country != ""]

# Compute counts and percentages
country_counts <- sort(table(filtered_data), decreasing = TRUE)
total_count <- sum(country_counts)
country_percentages <- round((country_counts / total_count) * 100, 2)

# Create a summary table with "Total" as the first row
summary_table <- data.frame(
  Country = c("Total", names(country_counts)),
  Count = c(total_count, as.numeric(country_counts)),
  Percentage = c("100%", paste0(country_percentages, "%"))
)

# Display the summary table
kable(summary_table, caption = "Summary of Country Distribution with Totals at the Top")
```

| Country                              | Count | Percentage |
|:-------------------------------------|------:|:-----------|
| Total                                |   704 | 100%       |
| Venezuela (República Bolivariana de) |   255 | 36.22%     |
| Argentina                            |   121 | 17.19%     |
| Bolivia (Estado Plurinacional de)    |   115 | 16.34%     |
| Colombia                             |    53 | 7.53%      |
| Nicaragua                            |    51 | 7.24%      |
| El Salvador                          |    21 | 2.98%      |
| Honduras                             |    21 | 2.98%      |
| Guatemala                            |    15 | 2.13%      |
| Perú                                 |     9 | 1.28%      |
| Ecuador                              |     8 | 1.14%      |
| Uruguay                              |     7 | 0.99%      |
| Chile                                |     6 | 0.85%      |
| República Dominicana                 |     5 | 0.71%      |
| México                               |     4 | 0.57%      |
| Paraguay                             |     4 | 0.57%      |
| Costa Rica                           |     2 | 0.28%      |
| Afganistán                           |     1 | 0.14%      |
| Argelia                              |     1 | 0.14%      |
| Australia                            |     1 | 0.14%      |
| Belice                               |     1 | 0.14%      |
| Brasil                               |     1 | 0.14%      |
| España                               |     1 | 0.14%      |
| Estados Unidos de América            |     1 | 0.14%      |

Summary of Country Distribution with Totals at the Top

#### Hexad type Distribution

``` r
# Subset data to include the first six columns
subdata <- data[, c(1:6)]

# Specify the columns of interest
columns_of_interest <- c("Philanthropist", "Socializer", "Free Spirit", "Achiever", "Player", "Disruptor")

# Rename columns
colnames(subdata) <- columns_of_interest

# Calculate summary statistics and round to 2 decimal places
summary_stats <- data.frame(
  Column = columns_of_interest,
  Mean = round(sapply(subdata, mean, na.rm = TRUE), 2),
  Median = round(sapply(subdata, median, na.rm = TRUE), 2),
  Min = round(sapply(subdata, min, na.rm = TRUE), 2),
  Max = round(sapply(subdata, max, na.rm = TRUE), 2),
  SD = round(sapply(subdata, sd, na.rm = TRUE), 2)
)

summary_stats <- summary_stats[order(-summary_stats$Mean), -1]

# Display the summary statistics table
kable(summary_stats, caption = "Summary Statistics for Hexad Types")
```

|                |  Mean | Median | Min | Max |   SD |
|:---------------|------:|-------:|----:|----:|-----:|
| Free Spirit    | 12.76 |     14 |   2 |  14 | 2.06 |
| Achiever       | 12.72 |     13 |   2 |  14 | 1.71 |
| Philanthropist | 12.38 |     13 |   2 |  14 | 2.33 |
| Player         | 11.84 |     13 |   2 |  14 | 2.54 |
| Socializer     | 10.96 |     12 |   2 |  14 | 3.10 |
| Disruptor      |  7.98 |      8 |   2 |  14 | 3.78 |

Summary Statistics for Hexad Types

## Hexad Player type Distributions Analysis By Country (top 10)

#### Distribution by Country (Top 10)

``` r
#Shorten Countries Name for easy of tables
data[data$Country=="Bolivia (Estado Plurinacional de)", 'Country' ]<-"Bolivia"
data[data$Country=="Venezuela (República Bolivariana de)", 'Country' ]<-"Venezuela"
data[data$Country==" Estados Unidos de América", 'Country' ]<-"USA"
data$Country<-as.factor(data$Country)

# Filter out rows where 'Country' is missing
hexad_data_clean <- data %>% filter(Country != "")

# Identify the top 5, top 10, and top 20 most common countries
top_countries <- hexad_data_clean %>%
  count(Country, sort = TRUE)

top_10_countries <- top_countries %>% slice(1:10) %>% pull(Country)

# Filter data for these top countries
hexad_top10 <- hexad_data_clean %>% filter(Country %in% top_10_countries)
```

``` r
# Calculate and round summary statistics by Country for each player type
hexad_summary <- hexad_top10 %>%
  group_by(Country) %>%
  summarise(
    sample_size = n(),
    philanthropist_mean = round(mean(philanthropist, na.rm = TRUE), 2),
    philanthropist_median = round(median(philanthropist, na.rm = TRUE), 2),
    philanthropist_min = round(min(philanthropist, na.rm = TRUE), 2),
    philanthropist_max = round(max(philanthropist, na.rm = TRUE), 2),
    philanthropist_sd = round(sd(philanthropist, na.rm = TRUE), 2),
    
    socializer_mean = round(mean(socializer, na.rm = TRUE), 2),
    socializer_median = round(median(socializer, na.rm = TRUE), 2),
    socializer_min = round(min(socializer, na.rm = TRUE), 2),
    socializer_max = round(max(socializer, na.rm = TRUE), 2),
    socializer_sd = round(sd(socializer, na.rm = TRUE), 2),
    
    freeSpirit_mean = round(mean(freeSpirit, na.rm = TRUE), 2),
    freeSpirit_median = round(median(freeSpirit, na.rm = TRUE), 2),
    freeSpirit_min = round(min(freeSpirit, na.rm = TRUE), 2),
    freeSpirit_max = round(max(freeSpirit, na.rm = TRUE), 2),
    freeSpirit_sd = round(sd(freeSpirit, na.rm = TRUE), 2),
    
    achiever_mean = round(mean(achiever, na.rm = TRUE), 2),
    achiever_median = round(median(achiever, na.rm = TRUE), 2),
    achiever_min = round(min(achiever, na.rm = TRUE), 2),
    achiever_max = round(max(achiever, na.rm = TRUE), 2),
    achiever_sd = round(sd(achiever, na.rm = TRUE), 2),
    
    player_mean = round(mean(player, na.rm = TRUE), 2),
    player_median = round(median(player, na.rm = TRUE), 2),
    player_min = round(min(player, na.rm = TRUE), 2),
    player_max = round(max(player, na.rm = TRUE), 2),
    player_sd = round(sd(player, na.rm = TRUE), 2),
    
    disruptor_mean = round(mean(disruptor, na.rm = TRUE), 2),
    disruptor_median = round(median(disruptor, na.rm = TRUE), 2),
    disruptor_min = round(min(disruptor, na.rm = TRUE), 2),
    disruptor_max = round(max(disruptor, na.rm = TRUE), 2),
    disruptor_sd = round(sd(disruptor, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(sample_size)) # Sort by sample size

# Transpose the table
hexad_summary_t <- hexad_summary %>%
  pivot_longer(cols = -Country, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = Country, values_from = Value)

# Generate an RMarkdown table
hexad_summary_t %>%
  kbl(caption = "Transposed Summary Statistics for Hexad Player Types by Country") %>%
  kable_styling(full_width = TRUE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="margin-left: auto; margin-right: auto;">
<caption>
Transposed Summary Statistics for Hexad Player Types by Country
</caption>
<thead>
<tr>
<th style="text-align:left;">
Statistic
</th>
<th style="text-align:right;">
Venezuela
</th>
<th style="text-align:right;">
Argentina
</th>
<th style="text-align:right;">
Bolivia
</th>
<th style="text-align:right;">
Colombia
</th>
<th style="text-align:right;">
Nicaragua
</th>
<th style="text-align:right;">
El Salvador
</th>
<th style="text-align:right;">
Honduras
</th>
<th style="text-align:right;">
Guatemala
</th>
<th style="text-align:right;">
Perú
</th>
<th style="text-align:right;">
Ecuador
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
sample_size
</td>
<td style="text-align:right;">
255.00
</td>
<td style="text-align:right;">
121.00
</td>
<td style="text-align:right;">
115.00
</td>
<td style="text-align:right;">
53.00
</td>
<td style="text-align:right;">
51.00
</td>
<td style="text-align:right;">
21.00
</td>
<td style="text-align:right;">
21.00
</td>
<td style="text-align:right;">
15.00
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
8.00
</td>
</tr>
<tr>
<td style="text-align:left;">
philanthropist_mean
</td>
<td style="text-align:right;">
12.64
</td>
<td style="text-align:right;">
12.40
</td>
<td style="text-align:right;">
12.16
</td>
<td style="text-align:right;">
12.92
</td>
<td style="text-align:right;">
12.16
</td>
<td style="text-align:right;">
12.57
</td>
<td style="text-align:right;">
12.86
</td>
<td style="text-align:right;">
12.07
</td>
<td style="text-align:right;">
12.33
</td>
<td style="text-align:right;">
11.25
</td>
</tr>
<tr>
<td style="text-align:left;">
philanthropist_median
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
</tr>
<tr>
<td style="text-align:left;">
philanthropist_min
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
5.00
</td>
<td style="text-align:right;">
10.00
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
10.00
</td>
<td style="text-align:right;">
2.00
</td>
</tr>
<tr>
<td style="text-align:left;">
philanthropist_max
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
</tr>
<tr>
<td style="text-align:left;">
philanthropist_sd
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.21
</td>
<td style="text-align:right;">
1.98
</td>
<td style="text-align:right;">
1.81
</td>
<td style="text-align:right;">
2.39
</td>
<td style="text-align:right;">
2.25
</td>
<td style="text-align:right;">
1.31
</td>
<td style="text-align:right;">
1.71
</td>
<td style="text-align:right;">
1.80
</td>
<td style="text-align:right;">
4.23
</td>
</tr>
<tr>
<td style="text-align:left;">
socializer_mean
</td>
<td style="text-align:right;">
11.40
</td>
<td style="text-align:right;">
10.50
</td>
<td style="text-align:right;">
11.18
</td>
<td style="text-align:right;">
11.47
</td>
<td style="text-align:right;">
10.31
</td>
<td style="text-align:right;">
11.62
</td>
<td style="text-align:right;">
11.29
</td>
<td style="text-align:right;">
10.33
</td>
<td style="text-align:right;">
11.56
</td>
<td style="text-align:right;">
11.50
</td>
</tr>
<tr>
<td style="text-align:left;">
socializer_median
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
11.00
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
11.00
</td>
<td style="text-align:right;">
11.00
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
11.00
</td>
<td style="text-align:right;">
11.00
</td>
<td style="text-align:right;">
13.00
</td>
</tr>
<tr>
<td style="text-align:left;">
socializer_min
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
4.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
4.00
</td>
</tr>
<tr>
<td style="text-align:left;">
socializer_max
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
</tr>
<tr>
<td style="text-align:left;">
socializer_sd
</td>
<td style="text-align:right;">
2.77
</td>
<td style="text-align:right;">
3.03
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
2.87
</td>
<td style="text-align:right;">
3.37
</td>
<td style="text-align:right;">
2.42
</td>
<td style="text-align:right;">
3.08
</td>
<td style="text-align:right;">
2.89
</td>
<td style="text-align:right;">
2.19
</td>
<td style="text-align:right;">
3.55
</td>
</tr>
<tr>
<td style="text-align:left;">
freeSpirit_mean
</td>
<td style="text-align:right;">
13.02
</td>
<td style="text-align:right;">
12.76
</td>
<td style="text-align:right;">
12.85
</td>
<td style="text-align:right;">
13.04
</td>
<td style="text-align:right;">
13.12
</td>
<td style="text-align:right;">
12.90
</td>
<td style="text-align:right;">
12.90
</td>
<td style="text-align:right;">
13.20
</td>
<td style="text-align:right;">
13.67
</td>
<td style="text-align:right;">
11.88
</td>
</tr>
<tr>
<td style="text-align:left;">
freeSpirit_median
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
</tr>
<tr>
<td style="text-align:left;">
freeSpirit_min
</td>
<td style="text-align:right;">
4.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
4.00
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
10.00
</td>
<td style="text-align:right;">
10.00
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
3.00
</td>
</tr>
<tr>
<td style="text-align:left;">
freeSpirit_max
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
</tr>
<tr>
<td style="text-align:left;">
freeSpirit_sd
</td>
<td style="text-align:right;">
1.74
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
1.86
</td>
<td style="text-align:right;">
1.70
</td>
<td style="text-align:right;">
1.68
</td>
<td style="text-align:right;">
1.73
</td>
<td style="text-align:right;">
1.22
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
0.71
</td>
<td style="text-align:right;">
3.87
</td>
</tr>
<tr>
<td style="text-align:left;">
achiever_mean
</td>
<td style="text-align:right;">
12.72
</td>
<td style="text-align:right;">
12.52
</td>
<td style="text-align:right;">
12.49
</td>
<td style="text-align:right;">
13.32
</td>
<td style="text-align:right;">
13.25
</td>
<td style="text-align:right;">
13.10
</td>
<td style="text-align:right;">
12.29
</td>
<td style="text-align:right;">
12.67
</td>
<td style="text-align:right;">
12.78
</td>
<td style="text-align:right;">
12.75
</td>
</tr>
<tr>
<td style="text-align:left;">
achiever_median
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
14.00
</td>
</tr>
<tr>
<td style="text-align:left;">
achiever_min
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
10.00
</td>
<td style="text-align:right;">
10.00
</td>
<td style="text-align:right;">
10.00
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
11.00
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
8.00
</td>
</tr>
<tr>
<td style="text-align:left;">
achiever_max
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
</tr>
<tr>
<td style="text-align:left;">
achiever_sd
</td>
<td style="text-align:right;">
1.78
</td>
<td style="text-align:right;">
1.81
</td>
<td style="text-align:right;">
1.76
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
1.11
</td>
<td style="text-align:right;">
1.26
</td>
<td style="text-align:right;">
1.68
</td>
<td style="text-align:right;">
1.11
</td>
<td style="text-align:right;">
1.64
</td>
<td style="text-align:right;">
2.19
</td>
</tr>
<tr>
<td style="text-align:left;">
player_mean
</td>
<td style="text-align:right;">
12.16
</td>
<td style="text-align:right;">
11.52
</td>
<td style="text-align:right;">
12.25
</td>
<td style="text-align:right;">
12.02
</td>
<td style="text-align:right;">
11.88
</td>
<td style="text-align:right;">
11.48
</td>
<td style="text-align:right;">
11.86
</td>
<td style="text-align:right;">
12.47
</td>
<td style="text-align:right;">
12.78
</td>
<td style="text-align:right;">
10.88
</td>
</tr>
<tr>
<td style="text-align:left;">
player_median
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
12.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
12.00
</td>
</tr>
<tr>
<td style="text-align:left;">
player_min
</td>
<td style="text-align:right;">
3.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
4.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
6.00
</td>
<td style="text-align:right;">
5.00
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
10.00
</td>
<td style="text-align:right;">
2.00
</td>
</tr>
<tr>
<td style="text-align:left;">
player_max
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
</tr>
<tr>
<td style="text-align:left;">
player_sd
</td>
<td style="text-align:right;">
2.31
</td>
<td style="text-align:right;">
2.46
</td>
<td style="text-align:right;">
2.15
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
2.44
</td>
<td style="text-align:right;">
2.86
</td>
<td style="text-align:right;">
2.06
</td>
<td style="text-align:right;">
1.51
</td>
<td style="text-align:right;">
1.48
</td>
<td style="text-align:right;">
3.91
</td>
</tr>
<tr>
<td style="text-align:left;">
disruptor_mean
</td>
<td style="text-align:right;">
7.49
</td>
<td style="text-align:right;">
8.39
</td>
<td style="text-align:right;">
8.32
</td>
<td style="text-align:right;">
8.21
</td>
<td style="text-align:right;">
7.59
</td>
<td style="text-align:right;">
7.38
</td>
<td style="text-align:right;">
7.90
</td>
<td style="text-align:right;">
9.73
</td>
<td style="text-align:right;">
8.22
</td>
<td style="text-align:right;">
8.38
</td>
</tr>
<tr>
<td style="text-align:left;">
disruptor_median
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
7.00
</td>
<td style="text-align:right;">
8.00
</td>
<td style="text-align:right;">
11.00
</td>
<td style="text-align:right;">
9.00
</td>
<td style="text-align:right;">
9.00
</td>
</tr>
<tr>
<td style="text-align:left;">
disruptor_min
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
2.00
</td>
<td style="text-align:right;">
5.00
</td>
</tr>
<tr>
<td style="text-align:left;">
disruptor_max
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
14.00
</td>
<td style="text-align:right;">
13.00
</td>
<td style="text-align:right;">
11.00
</td>
</tr>
<tr>
<td style="text-align:left;">
disruptor_sd
</td>
<td style="text-align:right;">
3.72
</td>
<td style="text-align:right;">
3.75
</td>
<td style="text-align:right;">
3.38
</td>
<td style="text-align:right;">
3.92
</td>
<td style="text-align:right;">
3.99
</td>
<td style="text-align:right;">
4.26
</td>
<td style="text-align:right;">
3.55
</td>
<td style="text-align:right;">
3.61
</td>
<td style="text-align:right;">
3.80
</td>
<td style="text-align:right;">
2.45
</td>
</tr>
</tbody>
</table>

#### Normality tests

``` r
# Initialize an empty data frame to store the results
shapiro_results <- data.frame(
  Row = c("Philanthropist", "Socializer", "Free Spirit", "Achiever", "Player", "Disruptor"),
  Shapiro_Wilk = numeric(6),
  P_value = numeric(6),
  stringsAsFactors = FALSE
)

# List of player types
player_types <- c("philanthropist", "socializer", "freeSpirit", "achiever", "player", "disruptor")

# Loop through each player type and perform the Shapiro-Wilk test
for (i in seq_along(player_types)) {
  type <- player_types[i]
  
  # Perform ANOVA to get residuals
  anova_5 <- aov(as.formula(paste(type, "~ Country")), data = hexad_top10)
  
  # Perform Shapiro-Wilk Test
  shapiro_test <- shapiro.test(residuals(anova_5))
  
  # Save results
  shapiro_results$Shapiro_Wilk[i] <- round(shapiro_test$statistic, 2)
  shapiro_results$P_value[i] <- shapiro_test$p.value
}

# Rename columns to match desired output
colnames(shapiro_results) <- c("Row", "Shapiro-Wilk", "p-value")

# Create and display the RMarkdown table
shapiro_results %>%
  kbl(caption = "Shapiro-Wilk Normality Test for Player Types by Country") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Shapiro-Wilk Normality Test for Player Types by Country
</caption>
<thead>
<tr>
<th style="text-align:left;">
Row
</th>
<th style="text-align:right;">
Shapiro-Wilk
</th>
<th style="text-align:right;">
p-value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Philanthropist
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Socializer
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Free Spirit
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Achiever
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Player
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Disruptor
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

#### Non-parametric ANOVA tests

``` r
# Initialize an empty data frame to store the results
results <- data.frame(
  Row = c("Philanthropist", "Socializer", "Free Spirit", "Achiever", "Player", "Disruptor"),
  Kruskal_Wallis_Chi_Sq = numeric(6),
  Degrees_of_Freedom = numeric(6),
  p_value = numeric(6),
  stringsAsFactors = FALSE
)

# List of player types
player_types <- c("philanthropist", "socializer", "freeSpirit", "achiever", "player", "disruptor")

# Loop through each player type and perform the Kruskal-Wallis test
for (i in seq_along(player_types)) {
  type <- player_types[i]
  
  # Perform Kruskal-Wallis Test
  kw_test <- kruskal.test(as.formula(paste(type, "~ Country")), data = hexad_top10)
  
  # Save results
  results$Kruskal_Wallis_Chi_Sq[i] <- round(kw_test$statistic, 2)
  results$Degrees_of_Freedom[i] <- kw_test$parameter
  results$p_value[i] <- round(kw_test$p.value, 3)
}

# Rename columns to match desired output
colnames(results) <- c(
  "Row",
  "Kruskal-Wallis χ^2",
  "Degrees of Freedom",
  "p-value"
)

# Create and display the RMarkdown table
results %>%
  kbl(caption = "Kruskal-Wallis Test Results for Player Types by Country") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Kruskal-Wallis Test Results for Player Types by Country
</caption>
<thead>
<tr>
<th style="text-align:left;">
Row
</th>
<th style="text-align:right;">
Kruskal-Wallis χ^2
</th>
<th style="text-align:right;">
Degrees of Freedom
</th>
<th style="text-align:right;">
p-value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Philanthropist
</td>
<td style="text-align:right;">
16.17
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.063
</td>
</tr>
<tr>
<td style="text-align:left;">
Socializer
</td>
<td style="text-align:right;">
15.83
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.070
</td>
</tr>
<tr>
<td style="text-align:left;">
Free Spirit
</td>
<td style="text-align:right;">
6.53
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.686
</td>
</tr>
<tr>
<td style="text-align:left;">
Achiever
</td>
<td style="text-align:right;">
21.76
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.010
</td>
</tr>
<tr>
<td style="text-align:left;">
Player
</td>
<td style="text-align:right;">
13.42
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.145
</td>
</tr>
<tr>
<td style="text-align:left;">
Disruptor
</td>
<td style="text-align:right;">
11.68
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.232
</td>
</tr>
</tbody>
</table>

#### Dunn test for multiple comparison for Achiever

``` r
# Perform Dunn Test with Bonferroni adjustment
posthoc_test <- dunnTest(achiever ~ Country, data = hexad_top10, method = "bonferroni")

# Extract and round results
posthoc_results <- posthoc_test$res %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) # Round all numeric columns to 3 decimal places

# Format results as a nice RMarkdown table
posthoc_results %>%
  kbl(caption = "Post Hoc Dunn Test Results for Achiever by Country (Bonferroni Adjustment)") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Post Hoc Dunn Test Results for Achiever by Country (Bonferroni Adjustment)
</caption>
<thead>
<tr>
<th style="text-align:left;">
Comparison
</th>
<th style="text-align:right;">
Z
</th>
<th style="text-align:right;">
P.unadj
</th>
<th style="text-align:right;">
P.adj
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Argentina - Bolivia
</td>
<td style="text-align:right;">
0.374
</td>
<td style="text-align:right;">
0.708
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Argentina - Colombia
</td>
<td style="text-align:right;">
-2.775
</td>
<td style="text-align:right;">
0.006
</td>
<td style="text-align:right;">
0.248
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia - Colombia
</td>
<td style="text-align:right;">
-3.047
</td>
<td style="text-align:right;">
0.002
</td>
<td style="text-align:right;">
0.104
</td>
</tr>
<tr>
<td style="text-align:left;">
Argentina - Ecuador
</td>
<td style="text-align:right;">
-0.936
</td>
<td style="text-align:right;">
0.349
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia - Ecuador
</td>
<td style="text-align:right;">
-1.068
</td>
<td style="text-align:right;">
0.286
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia - Ecuador
</td>
<td style="text-align:right;">
0.304
</td>
<td style="text-align:right;">
0.761
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Argentina - El Salvador
</td>
<td style="text-align:right;">
-1.472
</td>
<td style="text-align:right;">
0.141
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia - El Salvador
</td>
<td style="text-align:right;">
-1.672
</td>
<td style="text-align:right;">
0.095
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia - El Salvador
</td>
<td style="text-align:right;">
0.423
</td>
<td style="text-align:right;">
0.672
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Ecuador - El Salvador
</td>
<td style="text-align:right;">
-0.015
</td>
<td style="text-align:right;">
0.988
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Argentina - Guatemala
</td>
<td style="text-align:right;">
0.385
</td>
<td style="text-align:right;">
0.700
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia - Guatemala
</td>
<td style="text-align:right;">
0.206
</td>
<td style="text-align:right;">
0.837
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia - Guatemala
</td>
<td style="text-align:right;">
1.923
</td>
<td style="text-align:right;">
0.054
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Ecuador - Guatemala
</td>
<td style="text-align:right;">
1.021
</td>
<td style="text-align:right;">
0.307
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
El Salvador - Guatemala
</td>
<td style="text-align:right;">
1.341
</td>
<td style="text-align:right;">
0.180
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Argentina - Honduras
</td>
<td style="text-align:right;">
0.893
</td>
<td style="text-align:right;">
0.372
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia - Honduras
</td>
<td style="text-align:right;">
0.684
</td>
<td style="text-align:right;">
0.494
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia - Honduras
</td>
<td style="text-align:right;">
2.592
</td>
<td style="text-align:right;">
0.010
</td>
<td style="text-align:right;">
0.430
</td>
</tr>
<tr>
<td style="text-align:left;">
Ecuador - Honduras
</td>
<td style="text-align:right;">
1.331
</td>
<td style="text-align:right;">
0.183
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
El Salvador - Honduras
</td>
<td style="text-align:right;">
1.812
</td>
<td style="text-align:right;">
0.070
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Guatemala - Honduras
</td>
<td style="text-align:right;">
0.313
</td>
<td style="text-align:right;">
0.754
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Argentina - Nicaragua
</td>
<td style="text-align:right;">
-2.731
</td>
<td style="text-align:right;">
0.006
</td>
<td style="text-align:right;">
0.284
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia - Nicaragua
</td>
<td style="text-align:right;">
-2.999
</td>
<td style="text-align:right;">
0.003
</td>
<td style="text-align:right;">
0.122
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia - Nicaragua
</td>
<td style="text-align:right;">
0.006
</td>
<td style="text-align:right;">
0.995
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Ecuador - Nicaragua
</td>
<td style="text-align:right;">
-0.300
</td>
<td style="text-align:right;">
0.764
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
El Salvador - Nicaragua
</td>
<td style="text-align:right;">
-0.416
</td>
<td style="text-align:right;">
0.677
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Guatemala - Nicaragua
</td>
<td style="text-align:right;">
-1.911
</td>
<td style="text-align:right;">
0.056
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Honduras - Nicaragua
</td>
<td style="text-align:right;">
-2.573
</td>
<td style="text-align:right;">
0.010
</td>
<td style="text-align:right;">
0.454
</td>
</tr>
<tr>
<td style="text-align:left;">
Argentina - Perú
</td>
<td style="text-align:right;">
-0.431
</td>
<td style="text-align:right;">
0.666
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia - Perú
</td>
<td style="text-align:right;">
-0.571
</td>
<td style="text-align:right;">
0.568
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia - Perú
</td>
<td style="text-align:right;">
0.855
</td>
<td style="text-align:right;">
0.393
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Ecuador - Perú
</td>
<td style="text-align:right;">
0.397
</td>
<td style="text-align:right;">
0.692
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
El Salvador - Perú
</td>
<td style="text-align:right;">
0.499
</td>
<td style="text-align:right;">
0.618
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Guatemala - Perú
</td>
<td style="text-align:right;">
-0.603
</td>
<td style="text-align:right;">
0.546
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Honduras - Perú
</td>
<td style="text-align:right;">
-0.904
</td>
<td style="text-align:right;">
0.366
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Nicaragua - Perú
</td>
<td style="text-align:right;">
0.849
</td>
<td style="text-align:right;">
0.396
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Argentina - Venezuela
</td>
<td style="text-align:right;">
-1.502
</td>
<td style="text-align:right;">
0.133
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Bolivia - Venezuela
</td>
<td style="text-align:right;">
-1.910
</td>
<td style="text-align:right;">
0.056
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Colombia - Venezuela
</td>
<td style="text-align:right;">
1.930
</td>
<td style="text-align:right;">
0.054
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Ecuador - Venezuela
</td>
<td style="text-align:right;">
0.490
</td>
<td style="text-align:right;">
0.624
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
El Salvador - Venezuela
</td>
<td style="text-align:right;">
0.802
</td>
<td style="text-align:right;">
0.422
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Guatemala - Venezuela
</td>
<td style="text-align:right;">
-1.021
</td>
<td style="text-align:right;">
0.307
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Honduras - Venezuela
</td>
<td style="text-align:right;">
-1.660
</td>
<td style="text-align:right;">
0.097
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Nicaragua - Venezuela
</td>
<td style="text-align:right;">
1.891
</td>
<td style="text-align:right;">
0.059
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Perú - Venezuela
</td>
<td style="text-align:right;">
-0.049
</td>
<td style="text-align:right;">
0.961
</td>
<td style="text-align:right;">
1.000
</td>
</tr>
</tbody>
</table>

## Confirmatory Factor Analysis

#### Mardia’s multivariate normality test

``` r
# Perform Mardia's test for multivariate normality
mardia_test <- mvn(data = data[, 7:18], mvnTest = "mardia")
mardia_test_N<-mardia_test$multivariateNormality
# Extract and prepare the results for the table
mardia_results <- data.frame(
  Test = c("Skewness", "Kurtosis"),
  Statistic = c(
    (mardia_test_N[1,2]),
    (mardia_test_N[2,2])
    
  ),
  P_Value = c(
  (mardia_test_N[1,3]),
    (mardia_test_N[2,3])
    
  )
)

# Create and display the RMarkdown table
mardia_results %>%
  kbl(caption = "Mardia's Test for Multivariate Normality") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Mardia’s Test for Multivariate Normality
</caption>
<thead>
<tr>
<th style="text-align:left;">
Test
</th>
<th style="text-align:left;">
Statistic
</th>
<th style="text-align:left;">
P_Value
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Skewness
</td>
<td style="text-align:left;">
6261.88714901932
</td>
<td style="text-align:left;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Kurtosis
</td>
<td style="text-align:left;">
69.6038292856511
</td>
<td style="text-align:left;">
0
</td>
</tr>
</tbody>
</table>

#### Weighted Least Squares Mean and Variance Adjusted CFA

``` r
# Specify the CFA model
cfa_model <- '
  Achiever =~ A2 + A4
  Disruptor =~ D3 + D4
  FreeSpirit =~ F1 + F3
  Philanthropist =~ P1 + P4
  Player =~ R2 + R4
  Socializer =~ S2 + S4
'

# Fit the CFA model
fit <- cfa(cfa_model, data = data,std.lv=TRUE, auto.efa = TRUE, estimator = "WLSMV")
 #
# Global Fit Metrics
results<-summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


results$pe[1:12, c(1:8,10)]
```

    ##               lhs op rhs exo       est         se         z pvalue   std.all
    ## 1        Achiever =~  A2   0 0.6757448 0.06695227 10.092933      0 0.6068167
    ## 2        Achiever =~  A4   0 0.6698247 0.06889788  9.721992      0 0.7584539
    ## 3       Disruptor =~  D3   0 1.4024810 0.13330145 10.521124      0 0.6606243
    ## 4       Disruptor =~  D4   0 1.8427001 0.16662118 11.059219      0 0.8598936
    ## 5      FreeSpirit =~  F1   0 0.9008680 0.06143618 14.663476      0 0.7890522
    ## 6      FreeSpirit =~  F3   0 0.8681472 0.07012414 12.380148      0 0.7441112
    ## 7  Philanthropist =~  P1   0 1.0393800 0.07140467 14.556191      0 0.8603602
    ## 8  Philanthropist =~  P4   0 0.9872884 0.06365573 15.509812      0 0.7153118
    ## 9          Player =~  R2   0 1.0456529 0.07921433 13.200300      0 0.7407792
    ## 10         Player =~  R4   0 1.0383291 0.07389367 14.051666      0 0.6882407
    ## 11     Socializer =~  S2   0 1.4011580 0.06345829 22.079983      0 0.8247651
    ## 12     Socializer =~  S4   0 1.3718032 0.06426109 21.347337      0 0.8116533

### CFA metrics with confident interval

``` r
reliability_metrics <- semTools::reliability(fit)
CR<-reliability_metrics[2,]
AVE<-reliability_metrics[5,]
  

  # Extract relevant fit measures
  fit_measures_final <- fitMeasures(fit,standardized = TRUE, rsquare = TRUE, c("chisq.scaled", "srmr", "tli.scaled", "cfi.scaled", "rmsea.scaled"))

  fit_measures_final<-c(fit_measures_final, CR, AVE)
  

# Function to take a bootstrap sample and perform CFA, returning CR, Omega, and AVE
bootstrap_cfa_cr_omega_ave <- function(data, cfa_model) {
  # Generate a bootstrap sample (with replacement)
  bootstrap_sample <- data[sample(1:nrow(data), replace = TRUE), ]
  

# Fit the CFA model
fit <- cfa(cfa_model, data = bootstrap_sample,std.lv=TRUE, auto.efa = TRUE, estimator = "WLSMV")

 # Calculate reliability metrics (including Composite Reliability)
reliability_metrics <- semTools::reliability(fit)
  CR<-reliability_metrics[2,]
 AVE<-reliability_metrics[5,]
  

  # Extract relevant fit measures
  fit_measures <- fitMeasures(fit, c("chisq.scaled", "srmr", "tli.scaled", "cfi.scaled", "rmsea.scaled"))

  fit_measures<-c(fit_measures, CR, AVE)
 
  return(fit_measures)
}

# Perform bootstrap sampling
set.seed(123)  # For reproducibility
num_bootstrap <- 10000  # Number of bootstrap samples8u-p0

# Initialize matrix to store CR, Omega, and AVE
fit_metrics <- matrix(NA, nrow = num_bootstrap, ncol = 17)
# List of columns to calculate confidence intervals for
columns_to_ci <- c("chisq.scaled", "srmr", "tli.scaled", "cfi.scaled", "rmsea.scaled", 
                   "CR.Achiever", "CR.Disruptor", "CR.FreeSpirit", "CR.Philanthropist", 
                   "CR.Player", "CR.Socializer", 
                   "AVE.Achiever", "AVE.Disruptor", "AVE.FreeSpirit", 
                   "AVE.Philanthropist", "AVE.Player", "AVE.Socializer")
colnames(fit_metrics) <- columns_to_ci

# Bootstrap loop
for (i in 1:num_bootstrap) {
  fit_metrics[i, ] <- bootstrap_cfa_cr_omega_ave(data, cfa_model)
}


  names(fit_measures_final) <- columns_to_ci


# Initialize an empty data frame to store the results
results_table <- data.frame(
  Metric = columns_to_ci,
  Estimate = numeric(length(columns_to_ci)),
  CI_Lower = numeric(length(columns_to_ci)),
  CI_Upper = numeric(length(columns_to_ci)),
  stringsAsFactors = FALSE
)

# Populate the results table with confidence intervals and estimates
for (i in seq_along(columns_to_ci)) {
  col <- columns_to_ci[i]
  ci <- quantile(fit_metrics[, col], probs = c(0.025, 0.975))
  results_table$Estimate[i] <- round(fit_measures_final[col], 3)  # Add the estimate and round
  results_table$CI_Lower[i] <- round(ci[1], 3)  # Round lower CI
  results_table$CI_Upper[i] <- round(ci[2], 3)  # Round upper CI
}

# Correctly print the table in a nicely formatted way
results_table %>%
  kbl(caption = "CFA  metrics with confident interval", format = "markdown") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
CFA metrics with confident interval
</caption>
<thead>
<tr>
<th style="text-align:left;">
Metric
</th>
<th style="text-align:right;">
Estimate
</th>
<th style="text-align:right;">
CI_Lower
</th>
<th style="text-align:right;">
CI_Upper
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
chisq.scaled
</td>
<td style="text-align:right;">
77.320
</td>
<td style="text-align:right;">
69.545
</td>
<td style="text-align:right;">
141.811
</td>
</tr>
<tr>
<td style="text-align:left;">
srmr
</td>
<td style="text-align:right;">
0.026
</td>
<td style="text-align:right;">
0.024
</td>
<td style="text-align:right;">
0.037
</td>
</tr>
<tr>
<td style="text-align:left;">
tli.scaled
</td>
<td style="text-align:right;">
0.914
</td>
<td style="text-align:right;">
0.766
</td>
<td style="text-align:right;">
0.933
</td>
</tr>
<tr>
<td style="text-align:left;">
cfi.scaled
</td>
<td style="text-align:right;">
0.949
</td>
<td style="text-align:right;">
0.862
</td>
<td style="text-align:right;">
0.960
</td>
</tr>
<tr>
<td style="text-align:left;">
rmsea.scaled
</td>
<td style="text-align:right;">
0.034
</td>
<td style="text-align:right;">
0.030
</td>
<td style="text-align:right;">
0.055
</td>
</tr>
<tr>
<td style="text-align:left;">
CR.Achiever
</td>
<td style="text-align:right;">
0.619
</td>
<td style="text-align:right;">
0.524
</td>
<td style="text-align:right;">
0.698
</td>
</tr>
<tr>
<td style="text-align:left;">
CR.Disruptor
</td>
<td style="text-align:right;">
0.738
</td>
<td style="text-align:right;">
0.693
</td>
<td style="text-align:right;">
0.817
</td>
</tr>
<tr>
<td style="text-align:left;">
CR.FreeSpirit
</td>
<td style="text-align:right;">
0.740
</td>
<td style="text-align:right;">
0.665
</td>
<td style="text-align:right;">
0.800
</td>
</tr>
<tr>
<td style="text-align:left;">
CR.Philanthropist
</td>
<td style="text-align:right;">
0.758
</td>
<td style="text-align:right;">
0.693
</td>
<td style="text-align:right;">
0.813
</td>
</tr>
<tr>
<td style="text-align:left;">
CR.Player
</td>
<td style="text-align:right;">
0.674
</td>
<td style="text-align:right;">
0.611
</td>
<td style="text-align:right;">
0.731
</td>
</tr>
<tr>
<td style="text-align:left;">
CR.Socializer
</td>
<td style="text-align:right;">
0.802
</td>
<td style="text-align:right;">
0.759
</td>
<td style="text-align:right;">
0.841
</td>
</tr>
<tr>
<td style="text-align:left;">
AVE.Achiever
</td>
<td style="text-align:right;">
0.448
</td>
<td style="text-align:right;">
0.356
</td>
<td style="text-align:right;">
0.537
</td>
</tr>
<tr>
<td style="text-align:left;">
AVE.Disruptor
</td>
<td style="text-align:right;">
0.589
</td>
<td style="text-align:right;">
0.532
</td>
<td style="text-align:right;">
0.711
</td>
</tr>
<tr>
<td style="text-align:left;">
AVE.FreeSpirit
</td>
<td style="text-align:right;">
0.587
</td>
<td style="text-align:right;">
0.499
</td>
<td style="text-align:right;">
0.667
</td>
</tr>
<tr>
<td style="text-align:left;">
AVE.Philanthropist
</td>
<td style="text-align:right;">
0.611
</td>
<td style="text-align:right;">
0.531
</td>
<td style="text-align:right;">
0.685
</td>
</tr>
<tr>
<td style="text-align:left;">
AVE.Player
</td>
<td style="text-align:right;">
0.509
</td>
<td style="text-align:right;">
0.441
</td>
<td style="text-align:right;">
0.576
</td>
</tr>
<tr>
<td style="text-align:left;">
AVE.Socializer
</td>
<td style="text-align:right;">
0.670
</td>
<td style="text-align:right;">
0.612
</td>
<td style="text-align:right;">
0.725
</td>
</tr>
</tbody>
</table>

#### CFA Standardized residuals

``` r
# Local Fit: Residuals  Standardized residuals
standardized_residuals <- residuals(fit, type = "standardized")
print(standardized_residuals)
```

    ## $type
    ## [1] "standardized"
    ## 
    ## $cov
    ##        A2     A4     D3     D4     F1     F3     P1     P4     R2     R4     S2
    ## A2  0.000                                                                      
    ## A4  0.000  0.000                                                               
    ## D3  1.440 -0.100  0.000                                                        
    ## D4  0.150 -1.113  0.000  0.000                                                 
    ## F1 -1.056 -1.045  1.244  0.983  0.000                                          
    ## F3  1.468  1.046  0.704 -2.826  0.000  0.000                                   
    ## P1 -0.298  2.628 -0.015  2.289 -0.533  2.587  0.000                            
    ## P4 -2.295  0.974 -0.812 -1.642 -2.289  0.816  0.000  0.000                     
    ## R2 -0.560 -0.977 -1.361  0.210 -0.554 -2.281  1.277 -0.235  0.000              
    ## R4 -0.921  2.708 -0.840  1.795  3.620 -0.836  1.709 -2.554  0.000  0.000       
    ## S2  1.983 -1.844  0.525 -0.167 -0.639 -0.631 -1.929  0.929  2.901 -0.927  0.000
    ## S4  1.439 -1.547  0.067 -0.337  1.376 -0.226 -0.921  1.306  0.851 -2.877  0.000
    ##        S4
    ## A2       
    ## A4       
    ## D3       
    ## D4       
    ## F1       
    ## F3       
    ## P1       
    ## P4       
    ## R2       
    ## R4       
    ## S2       
    ## S4  0.000

#### CFA Correlation residuals

``` r
#Correlation residuals
residuals(fit, type = "cor")
```

    ## $type
    ## [1] "cor.bollen"
    ## 
    ## $cov
    ##        A2     A4     D3     D4     F1     F3     P1     P4     R2     R4     S2
    ## A2  0.000                                                                      
    ## A4  0.000  0.000                                                               
    ## D3  0.035 -0.002  0.000                                                        
    ## D4  0.003 -0.021  0.000  0.000                                                 
    ## F1 -0.021 -0.022  0.024  0.018  0.000                                          
    ## F3  0.031  0.023  0.015 -0.057  0.000  0.000                                   
    ## P1 -0.006  0.057  0.000  0.039 -0.010  0.051  0.000                            
    ## P4 -0.041  0.019 -0.016 -0.031 -0.037  0.014  0.000  0.000                     
    ## R2 -0.011 -0.019 -0.031  0.004 -0.009 -0.041  0.025 -0.004  0.000              
    ## R4 -0.018  0.056 -0.019  0.038  0.078 -0.017  0.027 -0.042  0.000  0.000       
    ## S2  0.040 -0.030  0.009 -0.003 -0.009 -0.010 -0.029  0.015  0.057 -0.016  0.000
    ## S4  0.025 -0.024  0.001 -0.005  0.020 -0.003 -0.014  0.025  0.017 -0.054  0.000
    ##        S4
    ## A2       
    ## A4       
    ## D3       
    ## D4       
    ## F1       
    ## F3       
    ## P1       
    ## P4       
    ## R2       
    ## R4       
    ## S2       
    ## S4  0.000

## Internal Reliability

``` r
# Define pairs of columns for which to calculate Cronbach's alpha and Spearman-Brown reliability
pairs <- list(
  c("P1", "P4"),
   c("S2", "S4"),
  c("A2", "A4"),
    c("R2", "R4"),
  c("D3", "D4"),
   c("F1", "F3")
   )



num_bootstrap <- 10000
# Define function for Cronbach's alpha calculation
cronbach_alpha_fn <- function(data, indices, item1, item2) {
  sample_data <- data[indices, c(item1, item2)]
  alpha <- psych::alpha(sample_data)$total$raw_alpha
  return(alpha)
}

# Define function for Spearman-Brown calculation
spearman_brown_fn <- function(data, indices, item1, item2) {
  sample_data <- data[indices, ]
  correlation <- cor(sample_data[[item1]], sample_data[[item2]], method = "pearson")
  spearman_brown <- 2 * correlation / (1 + correlation)
  return(spearman_brown)
}

# Initialize a data frame to store results
results <- data.frame(Pair = character(), Alpha_Estimate = numeric(), Alpha_CI_Lower = numeric(), Alpha_CI_Upper = numeric(),
                      Spearman_Brown_Estimate = numeric(), SB_CI_Lower = numeric(), SB_CI_Upper = numeric())

# Loop through each pair and calculate Cronbach's alpha and Spearman-Brown with confidence intervals
for (pair in pairs) {
  item1 <- pair[1]
  item2 <- pair[2]
  
  # Bootstrapping for Cronbach's alpha
  alpha_boot <- boot(data = data, statistic = cronbach_alpha_fn, R = num_bootstrap, item1 = item1, item2 = item2)
  alpha_estimate <- alpha_boot$t0
  alpha_ci <- boot.ci(alpha_boot, type = "perc")$percent[4:5]
  
  # Bootstrapping for Spearman-Brown
  sb_boot <- boot(data = data, statistic = spearman_brown_fn, R = num_bootstrap, item1 = item1, item2 = item2)
  sb_estimate <- sb_boot$t0
  sb_ci <- boot.ci(sb_boot, type = "perc")$percent[4:5]
  
  # Append results to the data frame
  results <- rbind(results, data.frame(
    Pair = paste(item1, item2, sep = "_"),
    Alpha_Estimate = round(alpha_estimate,2),
    Alpha_CI_Lower = round(alpha_ci[1],2),
    Alpha_CI_Upper = round(alpha_ci[2],2),
    Spearman_Brown_Estimate = round(sb_estimate,2),
    SB_CI_Lower = round(sb_ci[1],2),
    SB_CI_Upper = round(sb_ci[2],2)
  ))
}

# Define row names
row_names <- c("Philanthropist", "Socializer", "Achiever", "Player", "Disruptor", "Free Spirit")

# Add row names to the results data frame
results <- cbind(Row = row_names, results)

# Display the results in an RMarkdown-friendly table
results %>%
  kbl(caption = "Reliability Analysis: Cronbach's Alpha and Spearman-Brown Results") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Reliability Analysis: Cronbach’s Alpha and Spearman-Brown Results
</caption>
<thead>
<tr>
<th style="text-align:left;">
Row
</th>
<th style="text-align:left;">
Pair
</th>
<th style="text-align:right;">
Alpha_Estimate
</th>
<th style="text-align:right;">
Alpha_CI_Lower
</th>
<th style="text-align:right;">
Alpha_CI_Upper
</th>
<th style="text-align:right;">
Spearman_Brown_Estimate
</th>
<th style="text-align:right;">
SB_CI_Lower
</th>
<th style="text-align:right;">
SB_CI_Upper
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Philanthropist
</td>
<td style="text-align:left;">
P1_P4
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.69
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.81
</td>
</tr>
<tr>
<td style="text-align:left;">
Socializer
</td>
<td style="text-align:left;">
S2_S4
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.84
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.84
</td>
</tr>
<tr>
<td style="text-align:left;">
Achiever
</td>
<td style="text-align:left;">
A2_A4
</td>
<td style="text-align:right;">
0.62
</td>
<td style="text-align:right;">
0.52
</td>
<td style="text-align:right;">
0.70
</td>
<td style="text-align:right;">
0.63
</td>
<td style="text-align:right;">
0.54
</td>
<td style="text-align:right;">
0.71
</td>
</tr>
<tr>
<td style="text-align:left;">
Player
</td>
<td style="text-align:left;">
R2_R4
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.73
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.61
</td>
<td style="text-align:right;">
0.73
</td>
</tr>
<tr>
<td style="text-align:left;">
Disruptor
</td>
<td style="text-align:left;">
D3_D4
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.76
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.68
</td>
<td style="text-align:right;">
0.76
</td>
</tr>
<tr>
<td style="text-align:left;">
Free Spirit
</td>
<td style="text-align:left;">
F1_F3
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.80
</td>
<td style="text-align:right;">
0.74
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.80
</td>
</tr>
</tbody>
</table>

## Correlaiton metric Kendals Tau

``` r
# Reorganize columns in the desired order
columns <- c("philanthropist", "socializer", "achiever", "player", "disruptor", "freeSpirit")
data_subset <- data[columns]

# Initialize matrices for correlations and p-values
n <- length(columns)
cor_matrix <- matrix("", n, n, dimnames = list(columns, columns))
p_matrix <- matrix(NA, n, n, dimnames = list(columns, columns))

# Calculate Kendall Tau correlations and p-values
for (i in 1:n) {
  for (j in i:n) {
    if (i == j) {
      cor_matrix[i, j] <- "1.000"
    } else {
      test <- cor.test(data_subset[[i]], data_subset[[j]], method = "kendall", use = "pairwise.complete.obs")
      correlation <- round(test$estimate, 3)
      p_value <- round(test$p.value, 3)
      significance <- ifelse(p_value < 0.001, "***", 
                      ifelse(p_value < 0.01, "**", 
                      ifelse(p_value < 0.05, "*", "")))
      
      cor_matrix[i, j] <- ""
      cor_matrix[j, i] <- paste0(correlation, significance)
    }
  }
}

# Convert the correlation matrix to a data frame for display
cor_matrix_df <- as.data.frame(cor_matrix, row.names = columns)
rownames(cor_matrix_df) <- c("Philanthropic", "Socializer", "Achiever", "Player", "Disruptor", "Free Spirit")
colnames(cor_matrix_df) <- c("Philanthropic", "Socializer", "Achiever", "Player", "Disruptor", "Free Spirit")

# Display the correlation matrix as a nicely formatted RMarkdown table
cor_matrix_df %>%
  kbl(
    caption = "Lower Triangle of Kendall Tau Correlation Matrix with Significance. Significance Legend: *** p < 0.001, ** p < 0.01, * p < 0.05"
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive"))
```

<table class="table table-striped table-hover table-condensed table-responsive" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>
Lower Triangle of Kendall Tau Correlation Matrix with Significance. Significance Legend: \*\*\* p \< 0.001, \*\* p \< 0.01, \* p \< 0.05
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
Philanthropic
</th>
<th style="text-align:left;">
Socializer
</th>
<th style="text-align:left;">
Achiever
</th>
<th style="text-align:left;">
Player
</th>
<th style="text-align:left;">
Disruptor
</th>
<th style="text-align:left;">
Free Spirit
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Philanthropic
</td>
<td style="text-align:left;">
1.000
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Socializer
</td>
<td style="text-align:left;">
0.397\*\*\*
</td>
<td style="text-align:left;">
1.000
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Achiever
</td>
<td style="text-align:left;">
0.221\*\*\*
</td>
<td style="text-align:left;">
0.194\*\*\*
</td>
<td style="text-align:left;">
1.000
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Player
</td>
<td style="text-align:left;">
0.171\*\*\*
</td>
<td style="text-align:left;">
0.218\*\*\*
</td>
<td style="text-align:left;">
0.261\*\*\*
</td>
<td style="text-align:left;">
1.000
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Disruptor
</td>
<td style="text-align:left;">
-0.091\*\*
</td>
<td style="text-align:left;">
-0.122\*\*\*
</td>
<td style="text-align:left;">
0.022
</td>
<td style="text-align:left;">
0.036
</td>
<td style="text-align:left;">
1.000
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Free Spirit
</td>
<td style="text-align:left;">
0.29\*\*\*
</td>
<td style="text-align:left;">
0.201\*\*\*
</td>
<td style="text-align:left;">
0.341\*\*\*
</td>
<td style="text-align:left;">
0.237\*\*\*
</td>
<td style="text-align:left;">
0.053\*
</td>
<td style="text-align:left;">
1.000
</td>
</tr>
</tbody>
</table>
