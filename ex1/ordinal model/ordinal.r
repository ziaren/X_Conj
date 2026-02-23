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

data_table <- data_target %>%
  mutate(
    PROMPT  = factor(PROMPT),
    DISPLAY = factor(DISPLAY),
    Response = factor(Response)
  )

summary_counts <- data_table %>%
  count(PROMPT, DISPLAY, Response) %>%
  complete(PROMPT, DISPLAY, Response, fill = list(n = 0))

summary_counts

# ordinal mixed model

library(readr)
library(dplyr)
library(brms)

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

priors <- c(
  prior(normal(0, 1.5), class = "b"),         # regularize fixed effects
  prior(exponential(1), class = "sd")         # random effect SDs
)

m_ord <- brm(
  Response_ord ~ PROMPT * DISPLAY + (1 | participant_id) + (1 | Group),
  data = d,
  family = cumulative(link = "logit"),
  prior = priors,
  chains = 4, cores = 4,
  iter = 4000, warmup = 2000,
  control = list(adapt_delta = 0.995, max_treedepth = 15)
)

summary(m_ord)


fit_brms <- m_ord
newdat <- expand.grid(
  PROMPT = factor(c("ALL","AND"), levels = c("ALL","AND")),
  DISPLAY = factor(c("0","2","4"), levels = c("0","2","4"))
)
ep <- posterior_epred(fit_brms, newdata = newdat, re_formula = NA)
dim(ep)

cat_names <- dimnames(summ)[[3]]

out <- do.call(rbind, lapply(seq_len(nrow(newdat)), function(i){
  data.frame(
    PROMPT   = newdat$PROMPT[i],
    DISPLAY  = newdat$DISPLAY[i],
    category = cat_names,
    mean = summ["mean", i, ],
    l95  = summ["l95.2.5%",  i, ],
    u95  = summ["u95.97.5%", i, ]
  )
}))

out

## CI

k_neither <- which(dimnames(ep)[[3]] ==
                     "Neither completely true nor completely false")

### indices: (ALL,0) (AND,0) (ALL,2) (AND,2) (ALL,4) (AND,4)
diff_neither_disp2 <- ep[, 4, k_neither] - ep[, 3, k_neither]

c(mean = mean(diff_neither_disp2),
  l95  = quantile(diff_neither_disp2, 0.025),
  u95  = quantile(diff_neither_disp2, 0.975))

k_false <- which(dimnames(ep)[[3]] == "Completely false")
diff_false_disp2 <- ep[, 4, k_false] - ep[, 3, k_false]
c(mean = mean(diff_false_disp2),
  l95  = quantile(diff_false_disp2, 0.025),
  u95  = quantile(diff_false_disp2, 0.975))

k_true <- which(dimnames(ep)[[3]] == "Completely true")

### ALL: row 5 (ALL,4) - row 1 (ALL,0)
diff_true_all_4v0 <- ep[, 5, k_true] - ep[, 1, k_true]
### AND: row 6 (AND,4) - row 2 (AND,0)
diff_true_and_4v0 <- ep[, 6, k_true] - ep[, 2, k_true]

rbind(
  ALL = c(mean=mean(diff_true_all_4v0),
          l95=quantile(diff_true_all_4v0,0.025),
          u95=quantile(diff_true_all_4v0,0.975)),
  AND = c(mean=mean(diff_true_and_4v0),
          l95=quantile(diff_true_and_4v0,0.025),
          u95=quantile(diff_true_and_4v0,0.975))
)

## posterior predictive check

pp_check(fit_brms, type="bars", nsamples=200)
