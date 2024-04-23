#### By Grant Mitchell
#### Code last updated on 4/20

#### Packages

library(tidyverse)
library(haven)
library(foreign)
library(ggthemes)
library(vdemdata)
library(lme4)
library(readxl)
library(rstanarm)
library(Matrix)
library(zoo)
library(plm)
library(reghelper)
#### functions

neg_binom <- function(bi_var) {
  counts <- numeric(length(bi_var))
  current_count <- 0
  for (i in seq_along(bi_var)) {
    if (bi_var[i] == 0) {
      current_count <- current_count + 1  
      counts[i] <- current_count
    } else {
      current_count <- 0
    }
  }
  
  return(counts)
}


impute <- function(df, var) {
  previous_value <- NA
  for (i in 1:nrow(df)) {
    if (is.na(df[i, var])) {
      df[i, var] <- previous_value
    } else {
      previous_value <- df[i, var]
    }
  }
  return(df)
}

#### Clean data

data <- vdem %>%
  mutate(turnover_test = ifelse(v2elturnhog == 0, 0, 1), COWcode = as.factor(COWcode), election = ifelse(is.na(v2eltype_0), 0, 1)) %>%
  replace_na(replace = list(turnover_test = 0, v2elturnhog = 0)) %>%
  mutate(v2elturnhog = as.factor(v2elturnhog), fix = as.factor(country_name)) %>%
  select(turnover_test, v2x_api, v2x_libdem, election, COWcode, v2elturnhog, year, fix, v2ellostsl, v2elsuffrage, e_p_polity, v2expathhg, v2ex_elechog, v2ex_elechos, v2exhoshog, ifs = country_text_id, v2expathhg, v2expathhs, v2x_freexp_altinf, v2xel_frefair) %>%
  group_by(COWcode) %>%
  mutate(last_elect = log(neg_binom(election) + 1), var_elect = var(last_elect), exec_chose = ifelse(v2exhoshog == 1, v2expathhs, v2expathhg-1), parli = ifelse(exec_chose == 6, 1, 0))


lexical <- read_excel("C:/Users/grantam/Downloads/LIED_6.5.xlsx") %>%
  rename(COWcode = cow) %>%
  mutate(COWcode = as.factor(COWcode))

data <- left_join(data, lexical, by = c("year", "COWcode")) %>%
  mutate(lexical_index = (lexical_index/6), lexical_index_plus = (lexical_index_plus/7))

data <- cbind(data, 
              datawizard::demean(data, 
                                 select = c("last_elect","v2ellostsl", "executive_elections", "legislative_elections", "multi-party_legislative_elections", "turnover_event", "competitive_elections", "v2elsuffrage", "e_p_polity", "political_liberties", "parli", "v2exhoshog", "v2x_freexp_altinf", "v2xel_frefair"),
                                 group = "COWcode"))

data <- data %>%
  ungroup() %>%
  mutate(across(
    starts_with(c(
      "turnover_event_within", 
      "legislative_elections_within", 
      "executive_elections_within", 
      "multi.party_legislative_elections_within", 
      "last_elect_within", 
      "competitive_elections_within",
      "v2elsuffrage_within",
      "political_liberties_within",
      "turnover_event_between", 
      "legislative_elections_between", 
      "executive_elections_between", 
      "multi.party_legislative_elections_between",
      "last_elect_between",
      "competitive_elections_between",
      "v2elsuffrage_between",
      "political_liberties_between",
      "parli_between",
      "parli_within",
      "v2exhoshog_within",
      "v2exhoshog_between",
      "v2x_freexp_altinf_within",
      "v2x_freexp_altinf_between",
      "v2xel_frefair_within",
      "v2xel_frefair_between"
    )),
    ~ (.-mean(., na.rm = TRUE)) / (2*(sd(., na.rm = TRUE)))
  ))

#### Exploratory viz

ggplot(data = data) +
  geom_smooth(aes(x = year, y = v2x_api), color = "grey40", linewidth = 2, se = F) +
  geom_smooth(aes(x = year, y = lexical_index_plus), color = "black", linewidth = 2, se = F) +
  labs(title = "Lexical Index vs. V-Dem Polyarchy Index", x = "Year", y = "Average Democracy in the World") +
  theme_bw() + 
  theme(plot.title = element_text(size = 25), axis.title = element_text(size = 20), axis.text = element_text(size = 20)) +
  annotate("text", label = "Lexical Index", x = 1975, y = .7, size = 7, color = "black") +
  annotate("text", label = "Polyarchy Index", x = 2000, y = .2, size = 7, color = "grey40")


#### Model

##### Standard

m0 <- lmer(v2x_api ~ 
             turnover_event + 
             legislative_elections + 
             executive_elections + 
             `multi-party_legislative_elections` + 
             last_elect + 
             competitive_elections +
             v2elsuffrage +
             political_liberties +
             (1|COWcode),
           data = data)

summary(m0)

ICC(m0)


##### Standard with Between effects

m1 <- lmer(v2x_api ~ 
             turnover_event_within + 
             legislative_elections_within + 
             executive_elections_within + 
             multi.party_legislative_elections_within + 
             last_elect_within + 
             competitive_elections_within +
             turnover_event_between + 
             legislative_elections_between + 
             executive_elections_between + 
             multi.party_legislative_elections_between + 
             last_elect_between + 
             competitive_elections_between +
             v2elsuffrage_between +
             v2elsuffrage_within +
             political_liberties_within +
             political_liberties_between +
             (1|COWcode),
           data = data)

summary(m1)

ICC(m1)

##### Add Presidentialism with Between effects

m2 <- lmer(v2x_api ~ 
             turnover_event_within + 
             legislative_elections_within + 
             executive_elections_within + 
             multi.party_legislative_elections_within + 
             last_elect_within + 
             competitive_elections_within +
             turnover_event_between + 
             legislative_elections_between + 
             executive_elections_between + 
             multi.party_legislative_elections_between + 
             last_elect_between + 
             competitive_elections_between +
             v2elsuffrage_between +
             v2elsuffrage_within +
             political_liberties_within +
             political_liberties_between +
             v2exhoshog_within +
             v2exhoshog_between +
             parli_between +
             parli_within +
             (1|COWcode),
           data = data)

summary(m2)

ICC(m2)

##### Add Presidentialism and media freedom with Between effects

m3 <- lmer(v2x_api ~ 
             turnover_event_within + 
             legislative_elections_within + 
             executive_elections_within + 
             multi.party_legislative_elections_within + 
             last_elect_within + 
             v2xel_frefair_within +
             turnover_event_between + 
             legislative_elections_between + 
             executive_elections_between + 
             multi.party_legislative_elections_between + 
             last_elect_between + 
             v2xel_frefair_between +
             v2elsuffrage_between +
             v2elsuffrage_within +
             v2exhoshog_within +
             v2exhoshog_between +
             parli_between +
             parli_within +
             v2x_freexp_altinf_within +
             v2x_freexp_altinf_between +
             (1|COWcode),
           data = data)

summary(m3)

ICC(m3)


anova(m0, m1)

names <- c("Intercept", "Turnover Within", "Legislative Elections Within", "Executive Elections Within", "Multi-Party Elections Within", "Years Since Election Within", "Uncertainty Within", "Turnover Between", "Legislative Elections Between", "Executive Elections Between", "Multi-Party Elections Between", "Years Since Election Between", "Uncertainty Between", "Suffrage Between", "Suffrage Within", "Political Liberties Within", "Political Liberties Between")

coef.plot <- summary(m1)[10]
coef.plot2 <- coef.plot[["coefficients"]]
coef.plot2 <- as.data.frame(cbind(coef.plot2, names)) %>%
  rename(stde = `Std. Error`) %>%
  mutate(stde = as.double(stde), mean = as.double(Estimate), sig = as.double(`t value`))

viz_data <- coef.plot2 %>%
  mutate(lower = mean - (1.96 * stde), upper = mean + (1.96 * stde), sig_factor = ifelse(abs(sig) >= 1.96, T, F)) %>%
  filter(names != "Intercept") %>%
  mutate(names = reorder(names, -mean))


ggplot(data = viz_data) +
  geom_linerange(aes(xmin = lower, xmax = upper, y = names, color = sig_factor), linewidth = 2) +
  geom_point(aes(x = mean, y = names, color = sig_factor, shape = sig_factor), size = 7) +
  geom_vline(xintercept = 0, color = "darkblue") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "Coeffecients of Objective Measures", y = "",x = "Effect Size", caption = "All vairables are mean centered and set to a standard scale.") + 
  scale_color_manual(values = c("darkblue", "darkgreen")) +
  theme(plot.title = element_text(size = 30), axis.title = element_text(size = 20), axis.text = element_text(size = 20), plot.caption = element_text(size = 15))

#### Stan

options(mc.cores = parallel::detectCores())

stan1 <- stan_lmer(v2x_api ~ 
                     turnover_event_within + 
                     legislative_elections_within + 
                     executive_elections_within + 
                     multi.party_legislative_elections_within + 
                     last_elect_within + 
                     competitive_elections_within +
                     turnover_event_between + 
                     legislative_elections_between + 
                     executive_elections_between + 
                     multi.party_legislative_elections_between + 
                     last_elect_between + 
                     competitive_elections_between +
                     v2elsuffrage_between +
                     v2elsuffrage_within +
                     political_liberties_within +
                     political_liberties_between +
                     (1|COWcode),
                   data = data, algorithm = "sampling", cores = 12)

sims <- as.matrix(stan1)
# draws for overall mean
mu_a_sims <- as.matrix(sims[, 1])
# draws for 85 counties county-level random effect
u_sims <- sims[, 16:211]
# draws for 85 countys' varying intercepts               
a_sims <- as.numeric(mu_a_sims) + u_sims          

# Compute mean, SD, median, and 95% credible interval of varying intercepts
# Posterior mean and SD of each alpha
a_mean <- apply(a_sims, 2, mean)
a_sd <- apply(a_sims, 2, sd)
a_quant <- apply(a_sims, 2, quantile, 
                 probs = c(0.025, 0.50, 0.975))
a_quant <- data.frame(t(a_quant))
names(a_quant) <- c("Q2.5", "Q50", "Q97.5")
# Combine summary statistics of posterior simulation draws
a_df <- data.frame(a_mean, a_sd, a_quant)

# Sort dataframe containing an estimated alpha's mean and sd for every county
a_df <- a_df[order(a_df$a_mean), ]
a_df$a_rank <- c(1 : dim(a_df)[1])
a_df <- a_df %>%
  mutate(sig = ifelse((Q2.5 > mean(a_df$a_mean) & Q97.5 > mean(a_df$a_mean) | Q2.5 < mean(a_df$a_mean) & Q97.5 < mean(a_df$a_mean)), T, F))

# a vector of country rank 

# Caterpillar plot
# Plot county-level alphas's posterior mean and 95% credible interval
ggplot(data = a_df, 
       aes(x = a_rank, 
           y = a_mean, color = sig)) +
  geom_pointrange(aes(ymin = Q2.5, 
                      ymax = Q97.5,
                      shape = sig), size = 1, linewidth = 2) + 
  geom_hline(yintercept = mean(a_df$a_mean), 
             size = 0.5, 
             col = "darkblue") + 
  scale_x_continuous(breaks = seq(from = 0, 
                                  to = 196, 
                                  by = 10)) + 
  scale_color_manual(values = c("darkblue", "darkgreen")) +
  theme_bw() +
  labs(y = "Country-Intercept", x = "Rank", title = "Country Intercepts with 95% CI") +
  annotate("text", label = "Fixed Intercept", x = 185, y= mean(a_df$a_mean)-.03, size = 10, color = "darkblue") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 30), axis.title = element_text(size = 20), axis.text = element_text(size = 20), plot.caption = element_text(size = 15))


#### Model


options(mc.cores = parallel::detectCores())

stan2 <- stan_lmer(v2x_api ~ 
                     turnover_event_within + 
                     legislative_elections_within + 
                     executive_elections_within + 
                     multi.party_legislative_elections_within + 
                     last_elect_within + 
                     competitive_elections_within +
                     turnover_event_between + 
                     legislative_elections_between + 
                     executive_elections_between + 
                     multi.party_legislative_elections_between + 
                     last_elect_between + 
                     competitive_elections_between +
                     v2elsuffrage_between +
                     v2elsuffrage_within +
                     political_liberties_within +
                     political_liberties_between +
                     v2exhoshog_within +
                     v2exhoshog_between +
                     parli_between +
                     parli_within +
                     (1|COWcode),
                   data = data, algorithm = "sampling", cores = 12)

sims <- as.matrix(stan2)
# draws for overall mean
mu_a_sims <- as.matrix(sims[, 1])
# draws for 85 counties county-level random effect
u_sims <- sims[, 16:211]
# draws for 85 countys' varying intercepts               
a_sims <- as.numeric(mu_a_sims) + u_sims          

# Compute mean, SD, median, and 95% credible interval of varying intercepts
# Posterior mean and SD of each alpha
a_mean <- apply(a_sims, 2, mean)
a_sd <- apply(a_sims, 2, sd)
a_quant <- apply(a_sims, 2, quantile, 
                 probs = c(0.025, 0.50, 0.975))
a_quant <- data.frame(t(a_quant))
names(a_quant) <- c("Q2.5", "Q50", "Q97.5")
# Combine summary statistics of posterior simulation draws
a_df <- data.frame(a_mean, a_sd, a_quant)

# Sort dataframe containing an estimated alpha's mean and sd for every county
a_df <- a_df[order(a_df$a_mean), ]
a_df$a_rank <- c(1 : dim(a_df)[1])
a_df <- a_df %>%
  mutate(sig = ifelse((Q2.5 > mean(a_df$a_mean) & Q97.5 > mean(a_df$a_mean) | Q2.5 < mean(a_df$a_mean) & Q97.5 < mean(a_df$a_mean)), T, F))

# a vector of country rank 

# Caterpillar plot
# Plot county-level alphas's posterior mean and 95% credible interval
ggplot(data = a_df, 
       aes(x = a_rank, 
           y = a_mean, color = sig)) +
  geom_pointrange(aes(ymin = Q2.5, 
                      ymax = Q97.5,
                      shape = sig), size = 1, linewidth = 2) + 
  geom_hline(yintercept = mean(a_df$a_mean), 
             size = 0.5, 
             col = "darkblue") + 
  scale_x_continuous(breaks = seq(from = 0, 
                                  to = 196, 
                                  by = 10)) + 
  scale_color_manual(values = c("darkblue", "darkgreen")) +
  theme_bw() +
  labs(y = "Country-Intercept", x = "Rank", title = "Country Intercepts with 95% CI") +
  annotate("text", label = "Fixed Intercept", x = 185, y= 0.24, size = 10, color = "darkblue") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 30), axis.title = element_text(size = 20), axis.text = element_text(size = 20), plot.caption = element_text(size = 15))


#### Robustness 3



options(mc.cores = parallel::detectCores())

stan3 <- stan_lmer(v2x_api ~ 
                     turnover_event_within + 
                     legislative_elections_within + 
                     executive_elections_within + 
                     multi.party_legislative_elections_within + 
                     last_elect_within + 
                     v2xel_frefair_within +
                     turnover_event_between + 
                     legislative_elections_between + 
                     executive_elections_between + 
                     multi.party_legislative_elections_between + 
                     last_elect_between + 
                     v2xel_frefair_between +
                     v2elsuffrage_between +
                     v2elsuffrage_within +
                     v2exhoshog_within +
                     v2exhoshog_between +
                     parli_between +
                     parli_within +
                     v2x_freexp_altinf_within +
                     v2x_freexp_altinf_between +
                     (1|COWcode),
                   data = data, algorithm = "sampling", cores = 12)

sims <- as.matrix(stan2)
# draws for overall mean
mu_a_sims <- as.matrix(sims[, 1])
# draws for 85 counties county-level random effect
u_sims <- sims[, 16:211]
# draws for 85 countys' varying intercepts               
a_sims <- as.numeric(mu_a_sims) + u_sims          

# Compute mean, SD, median, and 95% credible interval of varying intercepts
# Posterior mean and SD of each alpha
a_mean <- apply(a_sims, 2, mean)
a_sd <- apply(a_sims, 2, sd)
a_quant <- apply(a_sims, 2, quantile, 
                 probs = c(0.025, 0.50, 0.975))
a_quant <- data.frame(t(a_quant))
names(a_quant) <- c("Q2.5", "Q50", "Q97.5")
# Combine summary statistics of posterior simulation draws
a_df <- data.frame(a_mean, a_sd, a_quant)

# Sort dataframe containing an estimated alpha's mean and sd for every county
a_df <- a_df[order(a_df$a_mean), ]
a_df$a_rank <- c(1 : dim(a_df)[1])
a_df <- a_df %>%
  mutate(sig = ifelse((Q2.5 > mean(a_df$a_mean) & Q97.5 > mean(a_df$a_mean) | Q2.5 < mean(a_df$a_mean) & Q97.5 < mean(a_df$a_mean)), T, F))

# a vector of country rank 

# Caterpillar plot
# Plot county-level alphas's posterior mean and 95% credible interval
ggplot(data = a_df, 
       aes(x = a_rank, 
           y = a_mean, color = sig)) +
  geom_pointrange(aes(ymin = Q2.5, 
                      ymax = Q97.5,
                      shape = sig), size = 1, linewidth = 2) + 
  geom_hline(yintercept = mean(a_df$a_mean), 
             size = 0.5, 
             col = "darkblue") + 
  scale_x_continuous(breaks = seq(from = 0, 
                                  to = 196, 
                                  by = 10)) + 
  scale_color_manual(values = c("darkblue", "darkgreen")) +
  theme_bw() +
  labs(y = "Country-Intercept", x = "Rank", title = "Country Intercepts with 95% CI") +
  annotate("text", label = "Fixed Intercept", x = 185, y= 0.24, size = 10, color = "darkblue") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(size = 30), axis.title = element_text(size = 20), axis.text = element_text(size = 20), plot.caption = element_text(size = 15))