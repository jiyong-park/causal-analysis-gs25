##############################################################################
## [인과추론 연습문제] GS25 를 둘러싼 남혐 논란과 매출 변화에 대한 인과적 분석
## 작성자: UNCG 경영대학 박지용 (jiyong.park@uncg.edu)
##############################################################################



### 0. Loading packages

# install.packages("tidyverse")
# install.packages("tidysynth")
# install.packages("CausalImpact")
# install.packages("ggpubr")
library(tidyverse)
library(tidysynth)
library(CausalImpact)
library(ggpubr)




### 1. Loading data

sales_data <- read.csv("데이터정리.csv")

sales_GS25 <- read.csv("데이터정리.csv") %>% 
  filter(company=="GS25")




### 2. Time-series causal analysis

pre.period = c(1, 13)
post.period = c(14, 14)

TS_data <- zoo(cbind(sales_GS25$sale, sales_GS25$year, sales_GS25$quarter, sales_GS25$profit.margin), 1:14)

impact <- CausalImpact(TS_data, pre.period, post.period)

plot(impact)




### 3. Comparison between GS25 and CU
sales_trend1 <- sales_data %>% filter(company=="GS25" | company=="CU") %>% 
  ggplot(aes(x=time, y=sale, col=factor(company))) +
  geom_line() +
  labs(y = "Quarterly Sales", color="Company") +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom")

sales_trend2 <- sales_data %>% filter(company=="GS25" | company=="CU") %>%
  group_by(company) %>%
  mutate(avg_sale = mean(sale)) %>% 
  mutate(demeaned_sale = sale-avg_sale) %>% 
  ggplot(aes(x=time, y=demeaned_sale, col=factor(company))) +
  geom_line() + 
  labs(y = "Quarterly Sales (Mean-Centering)", color="Company") +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom")


profit_trend1 <- sales_data %>% filter(company=="GS25" | company=="CU") %>% 
  ggplot(aes(x=time, y=profit, col=factor(company))) +
  geom_line() +
  labs(y = "Quarterly Profit", color="Company") +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom")

profit_trend2 <- sales_data %>% filter(company=="GS25" | company=="CU") %>%
  group_by(company) %>%
  mutate(avg_profit = mean(profit)) %>% 
  mutate(demeaned_profit = profit-avg_profit) %>% 
  ggplot(aes(x=time, y=demeaned_profit, col=factor(company))) +
  geom_line() + 
  labs(y = "Quarterly Profit (Mean-Centering)", color="Company") +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom")


margin_trend1 <- sales_data %>% filter(company=="GS25" | company=="CU") %>% 
  ggplot(aes(x=time, y=profit.margin, col=factor(company))) +
  geom_line() +
  labs(y = "Quarterly Margin", color="Company") +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom")

margin_trend2 <- sales_data %>% filter(company=="GS25" | company=="CU") %>%
  group_by(company) %>%
  mutate(avg_margin = mean(profit.margin)) %>% 
  mutate(demeaned_margin = profit.margin-avg_margin) %>% 
  ggplot(aes(x=time, y=demeaned_margin, col=factor(company))) +
  geom_line() + 
  labs(y = "Quarterly Profit Margin (Mean-Centering)", color="Company") +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom")


ggarrange(sales_trend1, profit_trend1, margin_trend1, sales_trend2, profit_trend2, margin_trend2, ncol=3, nrow=2)




### 4. Constructing a synthetic control

treatment_company = "GS25"
#treatment_company = "CU"
#treatment_company = "SevenEleven"


# initial the synthetic control object
synthetic_setup_sales <- sales_data %>%
  synthetic_control(outcome = sale, unit = company, time = time, i_unit = treatment_company, i_time = 13)

synthetic_setup_profits <- sales_data %>%
  synthetic_control(outcome = profit, unit = company, time = time, i_unit = treatment_company, i_time = 13)

synthetic_setup_margins <- sales_data %>%
  synthetic_control(outcome = profit.margin, unit = company, time = time, i_unit = treatment_company, i_time = 13)


# Generate the aggregate predictors used to fit the weights
# Predictors: lagged variables during 2018-2020

predictor_lagged_sales <- synthetic_setup_sales %>% 
  generate_predictor(time_window = 1, sale_2018Q1 = sale) %>%
  generate_predictor(time_window = 2, sale_2018Q2 = sale) %>% 
  generate_predictor(time_window = 3, sale_2018Q3 = sale) %>%
  generate_predictor(time_window = 4, sale_2018Q4 = sale) %>%
  generate_predictor(time_window = 5, sale_2019Q1 = sale) %>%
  generate_predictor(time_window = 6, sale_2019Q2 = sale) %>%
  generate_predictor(time_window = 7, sale_2019Q3 = sale) %>%
  generate_predictor(time_window = 8, sale_2019Q4 = sale) %>%
  generate_predictor(time_window = 9, sale_2020Q1 = sale) %>%
  generate_predictor(time_window = 10, sale_2020Q2 = sale) %>%
  generate_predictor(time_window = 11, sale_2020Q3 = sale) %>%
  generate_predictor(time_window = 12, sale_2020Q4 = sale)

predictor_lagged_profits <- synthetic_setup_profits %>% 
  generate_predictor(time_window = 1, profit_2018Q1 = profit) %>%
  generate_predictor(time_window = 2, profit_2018Q2 = profit) %>% 
  generate_predictor(time_window = 3, profit_2018Q3 = profit) %>%
  generate_predictor(time_window = 4, profit_2018Q4 = profit) %>%
  generate_predictor(time_window = 5, profit_2019Q1 = profit) %>%
  generate_predictor(time_window = 6, profit_2019Q2 = profit) %>%
  generate_predictor(time_window = 7, profit_2019Q3 = profit) %>%
  generate_predictor(time_window = 8, profit_2019Q4 = profit) %>%
  generate_predictor(time_window = 9, profit_2020Q1 = profit) %>%
  generate_predictor(time_window = 10, profit_2020Q2 = profit) %>%
  generate_predictor(time_window = 11, profit_2020Q3 = profit) %>%
  generate_predictor(time_window = 12, profit_2020Q4 = profit)

predictor_lagged_margins <- synthetic_setup_margins %>% 
  generate_predictor(time_window = 1, profit.margin_2018Q1 = profit.margin) %>%
  generate_predictor(time_window = 2, profit.margin_2018Q2 = profit.margin) %>% 
  generate_predictor(time_window = 3, profit.margin_2018Q3 = profit.margin) %>%
  generate_predictor(time_window = 4, profit.margin_2018Q4 = profit.margin) %>%
  generate_predictor(time_window = 5, profit.margin_2019Q1 = profit.margin) %>%
  generate_predictor(time_window = 6, profit.margin_2019Q2 = profit.margin) %>%
  generate_predictor(time_window = 7, profit.margin_2019Q3 = profit.margin) %>%
  generate_predictor(time_window = 8, profit.margin_2019Q4 = profit.margin) %>%
  generate_predictor(time_window = 9, profit.margin_2020Q1 = profit.margin) %>%
  generate_predictor(time_window = 10, profit.margin_2020Q2 = profit.margin) %>%
  generate_predictor(time_window = 11, profit.margin_2020Q3 = profit.margin) %>%
  generate_predictor(time_window = 12, profit.margin_2020Q4 = profit.margin)


# Generate the fitted weights for the synthetic control (optimizer: ipop or LowRankQP)
synthetic_weight_sales <- predictor_lagged_sales %>% 
  generate_weights(optimization_window = 1:12, quadopt = "ipop")

synthetic_weight_profits <- predictor_lagged_profits %>% 
  generate_weights(optimization_window = 1:12, quadopt = "ipop")

synthetic_weight_margins <- predictor_lagged_margins %>% 
  generate_weights(optimization_window = 1:12, quadopt = "ipop")


# Generate the synthetic control
synthetic_output_sales <- synthetic_weight_sales %>% 
  generate_control()

synthetic_output_profits <- synthetic_weight_profits %>% 
  generate_control()

synthetic_output_margins <- synthetic_weight_margins %>% 
  generate_control()




### 5. Results of the synthetic control

# Present the weight of control units to construct the synthetic control
weight_figure1 <- synthetic_output_sales %>% plot_weights()
synthetic_output_sales %>% grab_unit_weights()

weight_figure2 <- synthetic_output_profits %>% plot_weights()
synthetic_output_profits %>% grab_unit_weights()

weight_figure3 <- synthetic_output_margins %>% plot_weights()
synthetic_output_margins %>% grab_unit_weights()


ggarrange(weight_figure1, weight_figure2, weight_figure3, ncol=3)


# Present the trends of treatment unit and synthetic control

synthetic_output_sales %>% grab_synthetic_control()
synthetic_output_profit %>% grab_synthetic_control()
synthetic_output_margins %>% grab_synthetic_control()


company_text = paste(treatment_company, "and", "Synthetic", treatment_company)

synth_plot_sales <- synthetic_output_sales %>% plot_trends() +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom") +
  labs(title=paste("Sales Trend of", company_text), caption ="")

diff_plot_sales <- synthetic_output_sales %>% plot_differences() +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom") +
  labs(title=paste("Sales Difference between", company_text), caption ="")


synth_plot_profits <- synthetic_output_profits %>% plot_trends() +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom") +
  labs(title=paste("Profit Trend of", company_text), caption ="")

diff_plot_profits <- synthetic_output_profits %>% plot_differences() +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom") +
  labs(title=paste("Profit Difference between", company_text), caption ="")


synth_plot_margins <- synthetic_output_margins %>% plot_trends() +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom") +
  labs(title=paste("Profit Margin Trend of", company_text), caption ="")

diff_plot_margins <- synthetic_output_margins %>% plot_differences() +
  scale_x_continuous("", labels = c("2018 1Q", "2018 2Q", "2018 3Q", "2018 4Q", "2019 1Q", "2019 2Q", "2019 3Q", "2019 4Q", 
                                    "2020 1Q", "2020 2Q", "2020 3Q", "2020 4Q", "2021 1Q", "2021 2Q"), breaks = 1:14) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom") +
  labs(title=paste("Profit Margin Difference between", company_text), caption ="")


ggarrange(synth_plot_sales, synth_plot_profits, synth_plot_margins, diff_plot_sales, diff_plot_profits, diff_plot_margins, ncol=3, nrow=2)



