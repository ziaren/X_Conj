library(tidyverse)

# import data, basic count

data <- read.csv('ex1_cleaned.csv')

data <- data %>%
  mutate(
    PROMPT = str_trim(PROMPT),
    PROMPT = recode(PROMPT, "CONJ" = "AND")
  )

data_target <- data %>%
  filter(PROMPT %in% c("AND", "ALL"))

write_csv(data_target, "data_target.csv")

d <- read_csv("data_target.csv") %>%
  mutate(
    # Ordered response: False < Neither < True
    Response_ord = ordered(
      Response,
      levels = c(
        "Completely false",
        "Neither completely true nor completely false",
        "Completely true"
      )
    ),
    PROMPT  = factor(PROMPT, levels = c("ALL", "AND")),
    DISPLAY = factor(DISPLAY, levels = c(0, 2, 4)),
    participant_id = factor(participant_id),
    item_id = factor(QUESTION)   # 60 items total; each participant saw 30
  )

# categorical mixed effect model

## data prep, "completely false" as baseline

library(dplyr)
library(brms)

d2 <- d %>%
  mutate(
    Response_cat = factor(
      Response,
      levels = c(
        "Completely false",
        "Neither completely true nor completely false",
        "Completely true"
      )
    ),
    PROMPT  = factor(PROMPT, levels = c("ALL","AND")),
    DISPLAY = factor(DISPLAY, levels = c("0","2","4")),
    participant_id = factor(participant_id),
    Group = factor(Group)
  )

## fit the model

priors_cat <- c(
  prior(normal(0, 2), class = "b", dpar = "muCompletelytrue"),
  prior(normal(0, 2), class = "b", dpar = "muNeithercompletelytruenorcompletelyfalse"),
  prior(exponential(1), class = "sd", dpar = "muCompletelytrue"),
  prior(exponential(1), class = "sd", dpar = "muNeithercompletelytruenorcompletelyfalse")
)

m_cat <- brm(
  Response_cat ~ PROMPT * DISPLAY + (1 | participant_id) + (1 | Group),
  data = d2,
  family = categorical(link = "logit"),
  prior = priors_cat,
  chains = 4, cores = 4,
  iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.995, max_treedepth = 15)
)

summary(m_cat)

## predicted probability

newdat <- expand.grid(
  PROMPT  = factor(c("ALL","AND"), levels = c("ALL","AND")),
  DISPLAY = factor(c("0","2","4"), levels = c("0","2","4"))
)

ep_cat <- posterior_epred(m_cat, newdata = newdat, re_formula = NA)

summ_cat <- apply(ep_cat, c(2,3), function(x)
  c(mean = mean(x),
    l95  = unname(quantile(x, 0.025)),
    u95  = unname(quantile(x, 0.975)))
)
dimnames(summ_cat)[[1]] <- c("mean","l95","u95")
cat_names <- dimnames(ep_cat)[[3]]

out_cat <- do.call(rbind, lapply(seq_len(nrow(newdat)), function(i){
  data.frame(
    PROMPT   = newdat$PROMPT[i],
    DISPLAY  = newdat$DISPLAY[i],
    category = cat_names,
    mean = summ_cat["mean", i, ],
    l95  = summ_cat["l95",  i, ],
    u95  = summ_cat["u95",  i, ]
  )
}))
out_cat
