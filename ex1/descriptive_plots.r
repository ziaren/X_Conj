
library(tidyverse)

data <- read.csv('ex1_cleaned.csv')
glimpse(data)
colnames(data)

library(dplyr)

data <- data %>%
  mutate(
    PROMPT = ifelse(PROMPT == "CONJ", "AND", PROMPT)
  )

# keep only target questions, drop filler questions

data_target <- data %>%
  filter(PROMPT %in% c("AND", "ALL"))

data_target <- data_target %>%
  mutate(
    PROMPT  = factor(PROMPT, levels = c("AND", "ALL")),
    DISPLAY = factor(DISPLAY, levels = c(0, 2, 4)),
    Response = factor(Response)
  )

# overall distribution

ggplot(data_target, aes(x = Response)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Overall Response Distribution")

# proportion plot by prompt

ggplot(data_target, aes(x = PROMPT, fill = Response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "Proportion",
    title = "Response Distribution by PROMPT"
  ) +
  theme_minimal()

# proportion plot by display

ggplot(data_target, aes(x = DISPLAY, fill = Response)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "Proportion",
    title = "Response Distribution by DISPLAY"
  ) +
  theme_minimal()

# full interaction plot

ggplot(data_target, aes(x = DISPLAY, fill = Response)) +
  geom_bar(position = "fill") +
  facet_wrap(~ PROMPT) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "Proportion",
    title = "Response by DISPLAY and PROMPT"
  ) +
  theme_minimal()
