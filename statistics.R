library(broom)
breach_lm <- lm(log10_num ~ state + entity_type + assoc_present + type + loc,
                data = data_long)
breach_eval <- breach_lm %>% tidy() %>% 
  mutate_if(is.numeric, round, 6) %>% 
  filter(p.value < 0.05)

breach_eval
tail(breach_eval)

breach_augment <- breach_lm %>% augment()
breach_augment %>% ggplot(aes(.fitted, .resid)) + 
  geom_point() +
  theme_publcean()
breach_augment %>% ggplot(aes(sample = .resid)) + 
  geom_qq() + 
  geom_qq_line() + 
  theme_pubclean()

breach_lm %>% glance()

breach_lm2 <- lm(log10_num ~ state + entity_type + assoc_present*type + assoc_present*loc + type*loc,
                 data = data_long)

anova(breach_lm, breach_lm2)

library(ranger)
breach_rf <- ranger(type ~ state + entity_type + assoc_present + log10_num + loc,
                    data = data_long,
                    importance = "impurity",
                    seed = 1)

importance(breach_rf)

library(yardstick)
(rf_conf <- breach_rf$confusion.matrix)

accuracy(rf_conf)
precision(rf_conf)