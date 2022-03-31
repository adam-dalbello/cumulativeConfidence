# cumulativeConfidence
Function for assessing impact and presence of chance between AB test variant measures.

```r
cumulativeConfidence <- function(.data, date, variants, metric, z_value = 1.39) {
  require(dplyr)
  require(rlang)

.data %>%
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
  ) %>%
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
  theme(panel.background = element_blank())

}
```
