# cumulativeConfidence Function
Function for assessing impact and presence of chance between AB test variant measures. Outputs a cumulative time series of each variant mean and confidence interval. Can also assess non randomly assigned variants.
<br>
<br>

### Languages and Tools
<div>
  <img src="https://github.com/devicons/devicon/blob/master/icons/r/r-original.svg" title = "r" alt = "r" width = "60" height = "60"/>&nbsp;
  <img src="https://github.com/devicons/devicon/blob/master/icons/rstudio/rstudio-original.svg" title = "RStudio" alt = "RStudio" width = "60" height = "60"/>&nbsp;
</div>

### Packages
<div>
  <img src="https://github.com/tidyverse/dplyr/raw/main/man/figures/logo.png" height = "100" style = "max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/rlang/raw/main/man/figures/logo.png" height = "100" style = "max-width: 100%;"/>&nbsp;
  <img src="https://github.com/tidyverse/ggplot2/raw/main/man/figures/logo.png" height = "100" style = "max-width: 100%;"/>&nbsp;
</div>
<br>
<br>

### Arguments
| Argument | Description |
| --- | --- |
| `.data` | A data frame, tibble, named matrix, or data.table that must be in machine readable long format. |
| `date` | Date column, which must be in the ISO 8601 format, or convertible to the format. Internally the function will attempt to cast the date to YYYY-MM-DD. |
| `variants` | The column/variable indicating whether the observation, or row, contains information for the A or B variant. |
| `metric` | The success metric that the means of will be used to estimate impact and presence of chance. |
| `z_value` | The Z value to determine which confidence level will be used. Defaulted to 1.39 to estimate at the 95% confidence level. |

# Function
```r
cumulativeConfidence <- function(.data, date, variants, metric, z_value = 1.39) {
  require(dplyr)
  require(rlang)

data_table <- .data %>%
  mutate(date = as.Date( {{ date }} )  ) %>%
  arrange( {{ variants }}, date) %>%
  group_by( {{ variants }} ) %>%
  mutate(
    observations = 1,
    cumulative_mean = cummean( {{ metric }} ),
    cumulative_observations = cumsum(observations)
  ) %>%
  group_by( {{variants }}, date) %>%
  mutate(
    cumulative_mean = last(cumulative_mean),
    cumulative_observations = max(cumulative_observations),
    cumulative_squared_errors = cumsum(as.numeric( {{ metric }} - cumulative_mean)^2)
  ) %>%
  group_by( {{variants }}, date) %>%
  summarise(
    cumulative_ci_lower = max(cumulative_mean) - (z_value * (sqrt(max(cumulative_squared_errors) / max(cumulative_observations)) / sqrt(max(cumulative_observations))) ),
    cumulative_mean = max(cumulative_mean),
    cumulative_ci_upper = max(cumulative_mean) + (z_value * (sqrt(max(cumulative_squared_errors) / max(cumulative_observations)) / sqrt(max(cumulative_observations))) )
  )

data_table %>% 
  group_by( {{ variants }} ) %>% 
  filter(date == max(date)) %>%
  print()

data_table %>% 
  ggplot2::ggplot(
    aes(
      date,
      cumulative_mean,
      group = {{ variants }},
      col = {{ variants }}
    )
  ) +
  geom_line(alpha = 0.5) +
  geom_errorbar(
    aes(
      ymin = cumulative_ci_lower,
      ymax = cumulative_ci_upper
    ),
    width = 0.15
  ) +
  xlab("Date") +
  ylab(paste0(as_label(enquo(metric)), ' mean')) +
  ggtitle(paste0('Cumulative Mean ', as_label(enquo(metric)), ': 83.4% Confidence Intervals')) +
  viridis::scale_color_viridis(
    option = "D",
    discrete = TRUE,
    begin = 0,
    end = 0.6
  ) +
  theme(
    panel.background = element_rect(fill = 'grey94'),
    text = element_text(family = extrafont::fonts()[fonts() == 'Segoe UI'])
  )

}
```
<br>
<br>

Example output
```r
#   Prototype date       cumulative_ci_lower cumulative_mean cumulative_ci_upper
#   <chr>     <date>                   <dbl>           <dbl>               <dbl>
# 1 proto_A   2018-07-29                17.3            17.4                17.4
# 2 proto_B   2018-07-29                12.5            12.6                12.6
```
![cumulative confidence intervals white](https://user-images.githubusercontent.com/25012294/161422002-65eff502-1a39-4bea-b726-02535c43c11d.png)



