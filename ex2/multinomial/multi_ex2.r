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

# categorical mixed effect model

## data prep, "completely false" as baseline

library(dplyr)
library(brms)

d <- d %>%
  mutate(
    Response_cat = factor(Response,
                          levels=c("Completely false",
                                   "Neither completely true nor completely false",
                                   "Completely true")),
    MONOTONICITY = factor(MONOTONICITY, levels=c("NEG","POS")),
    DISPLAY = factor(DISPLAY, levels=c("0","2","4"))
  )

get_prior(
  Response_cat ~ MONOTONICITY * DISPLAY + (1|participant_id) + (1|Group),
  data=d, family=categorical()
)
# then set priors using the returned dpar names, same trick as before

m_cat_mono <- brm(
  Response_cat ~ MONOTONICITY * DISPLAY + (1|participant_id) + (1|Group),
  data = d,
  family = categorical(link="logit"),
  chains=4, cores=4, iter=4000, warmup=2000,
  control=list(adapt_delta=0.995, max_treedepth=15)
)

summary(m_cat_mono)

newdat <- expand.grid(
  MONOTONICITY = factor(c("NEG","POS"), levels=c("NEG","POS")),
  DISPLAY = factor(c("0","2","4"), levels=c("0","2","4"))
)

ep_cat_mono <- posterior_epred(m_cat_mono, newdata=newdat, re_formula=NA)

summ <- apply(ep_cat_mono, c(2,3), function(x)
  c(mean=mean(x),
    l95=unname(quantile(x,0.025)),
    u95=unname(quantile(x,0.975)))
)
dimnames(summ)[[1]] <- c("mean","l95","u95")
cat_names <- dimnames(ep_cat_mono)[[3]]

out_cat_mono <- do.call(rbind, lapply(seq_len(nrow(newdat)), function(i){
  data.frame(
    MONOTONICITY=newdat$MONOTONICITY[i],
    DISPLAY=newdat$DISPLAY[i],
    category=cat_names,
    mean=summ["mean",i,],
    l95=summ["l95",i,],
    u95=summ["u95",i,]
  )
}))
out_cat_mono

k_true <- which(dimnames(ep_cat_mono)[[3]] == "Completely true")
k_false <- which(dimnames(ep_cat_mono)[[3]] == "Completely false")
k_neither <- which(dimnames(ep_cat_mono)[[3]] == "Neither completely true nor completely false")

# Row order: (NEG,0) (POS,0) (NEG,2) (POS,2) (NEG,4) (POS,4)

# NEG vs POS at DISPLAY=0 on True
diff_true_disp0 <- ep_cat_mono[, 1, k_true] - ep_cat_mono[, 2, k_true]

# POS vs NEG at DISPLAY=4 on True
diff_true_disp4 <- ep_cat_mono[, 6, k_true] - ep_cat_mono[, 5, k_true]

# NEG vs POS at DISPLAY=2 on Neither
diff_neither_disp2 <- ep_cat_mono[, 3, k_neither] - ep_cat_mono[, 4, k_neither]

rbind(
  True_NEGminusPOS_at0 = c(mean=mean(diff_true_disp0),
                           l95=quantile(diff_true_disp0,0.025),
                           u95=quantile(diff_true_disp0,0.975)),
  True_POSminusNEG_at4 = c(mean=mean(diff_true_disp4),
                           l95=quantile(diff_true_disp4,0.025),
                           u95=quantile(diff_true_disp4,0.975)),
  Neither_NEGminusPOS_at2 = c(mean=mean(diff_neither_disp2),
                              l95=quantile(diff_neither_disp2,0.025),
                              u95=quantile(diff_neither_disp2,0.975))
)
