library(ggplot2)
library(GGally)

data_long <- data_long %>% mutate(log10_num = log10(num_affected))

data_long %>% select(log10_num, assoc_present, type, loc) %>% ggpairs()

num_hist <- data_long %>% ggplot(aes(log10_num)) + geom_histogram(bins = 15) + theme_pubr() + ggtitle("log10_num histogram")
assoc_num <- data_long %>% ggplot(aes(assoc_present, log10_num)) + geom_boxplot() + theme_pubclean() + ggtitle("log10_num vs assoc_present boxplot")

ggarrange(num_hist, assoc_num)

type_num <- data_long %>% ggplot(aes(type, log10_num)) + geom_boxplot() + theme_pubclean() + ggtitle("log10_num vs breach_type boxplot")
loc_num <- data_long %>% ggplot(aes(loc, log10_num)) + geom_boxplot() + theme_pubclean() + ggtitle("log10_num vs breach_location boxplot")

ggarrange(type_num, loc_num, ncol = 1)

assoc_plot <- data_long %>% ggplot(aes(assoc_present)) + geom_bar() + theme_pubclean() + ggtitle("Number of cases with/without associate present")
type_plot <- data_long %>% ggplot(aes(type)) + geom_bar() + theme_pubclean() + ggtitle("Number of cases based on breach type")
loc_plot <- data_long %>% ggplot(aes(loc)) + geom_bar() + theme_pubclean() + ggtitle("Number of cases based on breach location")
ggarrange(assoc_plot, type_plot, loc_plot, ncol = 1)