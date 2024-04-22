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
  mutate(turnover_test = ifelse(v2elturnhog == 0, 0, 1), COWcode = as.factor(COWcode), election = ifelse(is.na(v2eltype_0), 0, 1), seatshare = ifelse(is.na(v2ellostsl), 100, v2ellostsl)) %>%
  replace_na(replace = list(turnover_test = 0, v2elturnhog = 0)) %>%
  mutate(v2elturnhog = as.factor(v2elturnhog), fix = as.factor(country_name)) %>%
  select(turnover_test, v2x_polyarchy, v2x_libdem, election, COWcode, v2elturnhog, year, fix, seatshare, v2elsuffrage, e_p_polity) %>%
  group_by(COWcode) %>%
  mutate(last_elect = log(neg_binom(election) + 1), var_elect = var(last_elect))

lexical <- read_excel("C:/Users/ochoc/Downloads/LIED_6.5.xlsx") %>%
  rename(COWcode = cow) %>%
  mutate(COWcode = as.factor(COWcode))

data <- left_join(data, lexical, by = c("year", "COWcode")) %>%
  mutate(lexical_index = (lexical_index/6), lexical_index_plus = (lexical_index_plus/7))

parties <- vparty %>%
  group_by(COWcode, year) %>%
  summarize(number_of_parties = n()) %>%
  mutate(COWcode = as.factor(COWcode))

data <- left_join(data, 
                  parties, 
                  by = c("year", "COWcode"))

data <- cbind(data, 
              datawizard::demean(data, 
                                 select = c("last_elect","seatshare", "executive_elections", "legislative_elections", "multi-party_legislative_elections", "turnover_event", "number_of_parties", "competitive_elections", "v2elsuffrage", "e_p_polity", "political_liberties"),
                                 group = "COWcode"))

data <- data %>%
  ungroup() %>%
  mutate(across(
    starts_with(c(
      "turnover_event_within", 
      "legislative_elections_within", 
      "executive_elections_within", 
      "multi.party_legislative_elections_within", 
      "seatshare_within", 
      "last_elect_within", 
      "competitive_elections_within",
      "v2elsuffrage_within",
      "political_liberties_within",
      "turnover_event_between", 
      "legislative_elections_between", 
      "executive_elections_between", 
      "multi.party_legislative_elections_between", 
      "seatshare_between", 
      "last_elect_between",
      "competitive_elections_between",
      "v2elsuffrage_between",
      "political_liberties_between"
    )),
    ~ (.-mean(., na.rm = TRUE)) / (2*(sd(., na.rm = TRUE)))
  ))

#### Exploratory viz

ggplot(data = data) +
  geom_smooth(aes(x = year, y = v2x_polyarchy), color = "grey40", linewidth = 2, se = F) +
  geom_smooth(aes(x = year, y = lexical_index_plus), color = "black", linewidth = 2, se = F) +
  labs(title = "Lexical Index vs. V-Dem Polyarchy Index", x = "Year", y = "Average Democracy in the World") +
  theme_bw() + 
  theme(plot.title = element_text(size = 25), axis.title = element_text(size = 20), axis.text = element_text(size = 20)) +
  annotate("text", label = "Lexical Index", x = 1975, y = .7, size = 7, color = "black") +
  annotate("text", label = "Polyarchy Index", x = 2000, y = .2, size = 7, color = "grey40")


#### Model



m1 <- lmer(v2x_polyarchy ~ 
             turnover_event_within + 
             legislative_elections_within + 
             executive_elections_within + 
             multi.party_legislative_elections_within + 
             seatshare_within + 
             last_elect_within + 
             competitive_elections_within +
             turnover_event_between + 
             legislative_elections_between + 
             executive_elections_between + 
             multi.party_legislative_elections_between + 
             seatshare_between + 
             last_elect_between + 
             competitive_elections_between +
             v2elsuffrage_between +
             v2elsuffrage_within +
             political_liberties_within +
             political_liberties_between +
             (1|COWcode),
           data = data)

m2 <- lmer(v2x_polyarchy ~ 
             turnover_event + 
             legislative_elections + 
             executive_elections +  `multi-party_legislative_elections` + 
             seatshare + 
             last_elect + 
             competitive_elections +
             v2elsuffrage +
             political_liberties +
             (1|COWcode),
           data = data)

anova(m1, m2)

names <- c("Intercept", "Turnover Within", "Legislative Elections Within", "Executive Elections Within", "Multi-Party Elections Within", "Seatshare Within", "Years Since Election Within", "Uncertainty Within", "Turnover Between", "Legislative Elections Between", "Executive Elections Between", "Multi-Party Elections Between", "Seatshare Between", "Years Since Election Between", "Uncertainty Between", "Suffrage Between", "Suffrage Within", "Political Liberties Within", "Political Liberties Between")

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

options(mc.cores = 6)

stan1 <- stan_lmer(v2x_polyarchy ~ 
                     turnover_event_within + 
                     legislative_elections_within + 
                     executive_elections_within + 
                     multi.party_legislative_elections_within + 
                     seatshare_within + 
                     last_elect_within + 
                     competitive_elections_within +
                     turnover_event_between + 
                     legislative_elections_between + 
                     executive_elections_between + 
                     multi.party_legislative_elections_between + 
                     seatshare_between + 
                     last_elect_between + 
                     competitive_elections_between +
                     (1|COWcode),
                   data = data, algorithm = "sampling")

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
  mutate(sig = ifelse((Q2.5 > 0.2602782 & Q97.5 > 0.2602782 | Q2.5 < 0.2602782 & Q97.5 < 0.2602782), T, F))

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

View(data)
