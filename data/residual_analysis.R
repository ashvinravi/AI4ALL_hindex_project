library(dplyr)
library(data.table)

library(ggplot2)
library(ggpubr)
library(patchwork)

d = read.csv("gscholar_profiles.csv")

h_index_upper_limit = mean(d$h_index) + 3*sd(d$h_index)
citation_number_upper_limit = mean(d$citation_number) + 3*sd(d$citation_number)

d = d %>%
  filter(h_index <= h_index_upper_limit) %>%
  filter(citation_number <= citation_number_upper_limit) %>%
  mutate(log_h_index = log(h_index, base = 10)) %>%
  mutate(log_citation_number = log(citation_number, base = 10))

plt1 = ggplot(data = d, aes(x = citation_number, y = h_index)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ poly(x,3)") + 
  theme_pubr()

l1 = lm(data = d, formula = h_index ~ citation_number)

summary(l1)

predict(l1, data.frame(citation_number = 0))

residual_df = data.frame(
  residuals = l1$residuals,
  citation_number = d$citation_number
)

plt2 = ggplot(data = residual_df, aes(x = citation_number, y = residuals)) +
  geom_point() + 
  geom_hline(aes(yintercept = 0), size = 2, color = "gray") + 
  theme_pubr()

plt1 + plt2


l2 = lm(data = d, formula = h_index ~ poly(citation_number, 2))
summary(l2)

residual_df = data.frame(
  residuals = l2$residuals,
  citation_number = d$citation_number
)

plt3 = ggplot(data = residual_df, aes(x = citation_number, y = residuals)) +
  geom_point() + 
  geom_hline(aes(yintercept = 0), size = 2, color = "gray") + 
  theme_pubr()

plt1 + plt3
predict(l2, data.frame(citation_number = 0))
l2


l3 = lm(data = d, formula = h_index ~ poly(citation_number, 3))
summary(l3)

predict(l3, data.frame(citation_number = 0))

residual_df = data.frame(
  residuals = l3$residuals,
  citation_number = d$citation_number
)

plt4 = ggplot(data = residual_df, aes(x = citation_number, y = residuals)) +
  geom_point() + 
  geom_hline(aes(yintercept = 0), size = 2, color = "gray") + 
  theme_pubr()


lm(data = d, formula = h_index ~ poly(citation_number,9)) %>%
  summary()

plt5 = ggplot(data = d, aes(x = citation_number, y = h_index)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ poly(x,3)") + 
  theme_pubr(); plt5

l4 = lm(data = d, formula = log_h_index ~ log_citation_number)
summary(l4)

plt6 = ggplot(data = d, aes(x = log_citation_number, y = log_h_index)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = "y ~ x") + 
  theme_pubr(); plt6

residual_df = data.frame(
  residuals = l4$residuals,
  citation_number = d$citation_number
)

plt7 = ggplot(data = residual_df, aes(x = citation_number, y = residuals)) +
  geom_point() + 
  geom_hline(aes(yintercept = 0), size = 2, color = "gray") + 
  theme_pubr()

plt4 + plt7


l5 = lm(data = d, formula = log_h_index ~ poly(log_citation_number,3))
summary(l5)


residual_df = data.frame(
  residuals = l5$residuals,
  citation_number = d$citation_number
)

plt8 = ggplot(data = residual_df, aes(x = citation_number, y = residuals)) +
  geom_point() + 
  geom_hline(aes(yintercept = 0), size = 2, color = "gray") + 
  theme_pubr()

d = d %>%
  mutate(prediction_residuals = l3$residuals)

write.csv(d, file = "gscholar_profiles_with_residuals.csv", 
          quote = FALSE, row.names = FALSE)
