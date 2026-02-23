library(tidyverse)

# import data, basic count

data <- read.csv('ex2_cleaned.csv')

data <- data %>%
  mutate(
    PROMPT = str_trim(PROMPT),
    PROMPT = recode(PROMPT, "CONJ" = "AND")
  )

data_target <- data %>%
  filter(PROMPT %in% c("AND"))

write_csv(data_target, "data_target_ex2.csv")

data_table <- data_target %>%
  mutate(
    MONOTONICITY  = factor(MONOTONICITY),
    DISPLAY = factor(DISPLAY),
    Response = factor(Response)
  )

summary_counts <- data_table %>%
  count(MONOTONICITY, DISPLAY, Response) %>%
  complete(MONOTONICITY, DISPLAY, Response, fill = list(n = 0))

summary_counts

# ordinal mixed model

library(readr)
library(dplyr)
library(brms)

d <- read_csv("data_target_ex2.csv") %>%
  mutate(
    Response_ord = ordered(Response,
                           levels = c("Completely false",
                                      "Neither completely true nor completely false",
                                      "Completely true")),
    MONOTONICITY = factor(MONOTONICITY, levels=c("NEG","POS")),
    DISPLAY = factor(DISPLAY, levels=c("0","2","4")),
    participant_id = factor(participant_id),
    Group = factor(Group)
  )

priors_ord <- c(
  prior(normal(0, 1.5), class="b"),
  prior(exponential(1), class="sd")
)

m_ord_mono <- brm(
  Response_ord ~ MONOTONICITY * DISPLAY + (1|participant_id) + (1|Group),
  data = d,
  family = cumulative(link="logit"),
  prior = priors_ord,
  chains=4, cores=4, iter=4000, warmup=2000,
  control=list(adapt_delta=0.995, max_treedepth=15)
)

summary(m_ord_mono)

newdat <- expand.grid(
  MONOTONICITY = factor(c("NEG","POS"), levels=c("NEG","POS")),
  DISPLAY = factor(c("0","2","4"), levels=c("0","2","4"))
)

ep <- posterior_epred(m_ord_mono, newdata=newdat, re_formula=NA)

summ <- apply(ep, c(2,3), function(x)
  c(mean=mean(x),
    l95=unname(quantile(x,0.025)),
    u95=unname(quantile(x,0.975)))
)
dimnames(summ)[[1]] <- c("mean","l95","u95")
cat_names <- dimnames(ep)[[3]]

out <- do.call(rbind, lapply(seq_len(nrow(newdat)), function(i){
  data.frame(
    MONOTONICITY=newdat$MONOTONICITY[i],
    DISPLAY=newdat$DISPLAY[i],
    category=cat_names,
    mean=summ["mean",i,],
    l95=summ["l95",i,],
    u95=summ["u95",i,]
  )
}))
out
