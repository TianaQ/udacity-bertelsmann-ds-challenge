mat_mort_data <- read.csv('maternal_mortality_ratio_per_100000_live_births.csv')
wb_income_groups <- read.csv('WB_income.csv')
regions <- read.csv('list-of-countries-etc.csv')

# since zeros come from missing data, they can be substituted by NA and omitted
# for reasons of omission see full Python project on Github https://goo.gl/2miTfm
mat_mort_data[mat_mort_data == 0] <- NA

# subsetting
# maternal mortality data for 1990 and 2010 years
mat_mort <- na.omit(mat_mort_data[c("country", 'X1990', 'X2010')])
mat_mort$country <- as.character(mat_mort$country)

# world regions, geo column to merge with income groups
reg_sub <- regions[c('geo', 'name', 'eight_regions')]
reg_sub <- data.frame(lapply(reg_sub, as.character), stringsAsFactors=FALSE)

#world bank income classification of countries for 1990 and 2010
wb_sub <- wb_income_groups[c('code', 'X1990', 'X2010')]
wb_sub <- data.frame(lapply(wb_sub, as.character), stringsAsFactors=FALSE)
wb_sub$code <- tolower(wb_sub$code)

# joining
library(dplyr)
mat_mort <- left_join(mat_mort, reg_sub, by = c("country" = "name"))
mat_mort <- left_join(mat_mort, wb_sub, by = c("geo" = "code"))

# renaming columns
names(mat_mort) <- c('country', 'mm_1990', 'mm_2010', 'code', 'eight_regions', 'income_1990', 'income_2010')

mat_mort$eight_regions <- as.factor(mat_mort$eight_regions) 
mat_mort$income_1990 <- as.factor(mat_mort$income_1990)
mat_mort$income_2010 <- as.factor(mat_mort$income_2010)
head(mat_mort)

str(mat_mort)

library(ggplot2)

ggplot(aes(x=mm_1990), data = mat_mort) +
  geom_histogram(binwidth = 50, color = 'black', fill='#aaaaff') +
  scale_x_continuous(limits = c(0, 2800), breaks = seq(0, 2500, 100)) +
  ggtitle('Maternal Mortality in 1990') +
  xlab('Maternal mortality ratios') +
  ylab('Number of countries') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('mm_1990.png')

ggplot(aes(x=mm_1990), data = mat_mort) +
  geom_histogram(color = 'black', fill='#aaaaff') +
  scale_x_log10() +
  ggtitle('Maternal Mortality in 1990') +
  xlab('Maternal mortality ratios, log10 scale') +
  ylab('Number of countries') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('mm_1990_log_scale.png')

ggplot(aes(x=mm_2010), data = mat_mort) +
  geom_histogram(binwidth = 50, color = 'black', fill='#aaaaff') +
  scale_x_continuous(limits = c(0, 2800), breaks = seq(0, 2500, 100)) +
  ggtitle('Maternal Mortality in 2010') +
  xlab('Maternal mortality ratios') +
  ylab('Number of countries') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('mm_2010.png')

ggplot(aes(x=mm_2010), data = mat_mort) +
  geom_histogram(color = 'black', fill='#aaaaff') +
  scale_x_log10() +
  ggtitle('Maternal Mortality in 2010') +
  xlab('Maternal mortality ratios, log10 scale') +
  ylab('Number of countries') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('mm_2010_log_scale.png')

ggplot(aes(x=mm_1990), data = mat_mort) +
  geom_histogram(binwidth = 50, color = 'black', fill='#aaaaff') +
  scale_x_continuous(limits = c(0, 2800), breaks = seq(0, 2500, 100)) +
  ggtitle('Maternal mortality in 1990 by income groups') +
  xlab('Maternal mortality ratios') +
  ylab('Number of countries') +
  facet_wrap(~income_1990) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('mm_by_ig_1990.png')

ggplot(aes(x=mm_2010), data = mat_mort) +
  geom_histogram(binwidth = 50, color = 'black', fill='#aaaaff') +
  scale_x_continuous(limits = c(0, 2800), breaks = seq(0, 2500, 100)) +
  ggtitle('Maternal mortality in 2010 by income groups of 1990') +
  xlab('Maternal mortality ratios') +
  ylab('Number of countries') +
  facet_wrap(~income_1990) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('mm_2010_by_ig_1990.png')

ggplot(data = mat_mort) +
  geom_boxplot(aes(x = eight_regions, y = mm_1990)) +
  ggtitle('Maternal mortality in 1990 by regions') +
  xlab('Regions') +
  ylab('Maternal mortality ratios') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('mm_1990_by_reg.png')

ggplot(data = mat_mort) +
  geom_boxplot(aes(x = eight_regions, y = mm_2010)) +
  ggtitle('Maternal mortality in 2010 by regions') +
  xlab('Regions') +
  ylab('Maternal mortality ratios') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('mm_2010_by_reg.png')

##############
total_fert <- read.csv('children_per_woman_total_fertility.csv')

total_fert <- total_fert[c("country", 'X1990', 'X2010')]
names(total_fert) <- c('country', 'tf_1990', 'tf_2010')

mat_mort <- left_join(mat_mort, total_fert, by = c("country" = "country"))

ggplot(aes(tf_1990, mm_1990), data = mat_mort) + 
  geom_point() +
  ggtitle('Maternal mortality by Total fertility in 1990') +
  xlab('Total fertility rate') +
  ylab('Maternal mortality ratios') +
  geom_smooth(method = 'lm')

ggplot(aes(tf_2010, mm_2010), data = mat_mort) + 
  geom_point() +
  ggtitle('Maternal mortality by Total fertility in 2010') +
  xlab('Total fertility rate') +
  ylab('Maternal mortality ratios') +
  geom_smooth(method = 'lm')

library(GGally)
theme_set(theme_minimal(12))

ggplot(aes(mm_2010, tf_2010), data = mat_mort) +
  geom_point(aes(color = eight_regions), alpha = 0.7) +
  scale_color_brewer(type = 'div') +
  xlab('Maternal mortality ratio') +
  ylab('Total fertility rate') + 
  ggtitle('Maternal mortality and total fertility in 2010 by regions')

