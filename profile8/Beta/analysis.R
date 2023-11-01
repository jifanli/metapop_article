library(tidyverse)
library(metapoppkg)
profile_Beta_be <- rbind(readRDS("out_3/profile_Beta_be.rds"),readRDS("out_3/profile_Beta_be2.rds"))
profile_Beta_af <- rbind(readRDS("out_3/profile_Beta_af.rds"),readRDS("out_3/profile_Beta_af2.rds"))
saveRDS(profile_Beta_be,"profile_Beta_be_full.rds")
saveRDS(profile_Beta_af,"profile_Beta_af_full.rds")

prof_res_be <- profile_Beta_be %>%
  group_by(Beta_be1) %>%
  summarize(logLik = max(logLik))

mcap_results_be <- pomp::mcap(prof_res_be$logLik, prof_res_be$Beta_be1)

Beta_be <- ggplot() +
  geom_point(data = prof_res_be, aes(x = Beta_be1, y = logLik)) +
  geom_line(data = mcap_results_be$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results_be$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_be$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_be$mle, col = 'blue') +
  labs(x = "Beta_be", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("Beta_be.png", plot=Beta_be, width=10, height=7, dpi=300)

prof_res_af <- profile_Beta_af %>%
  group_by(Beta_af1) %>%
  summarize(logLik = max(logLik))

mcap_results_af <- pomp::mcap(prof_res_af$logLik, prof_res_af$Beta_af1)

Beta_af <- ggplot() +
  geom_point(data = prof_res_af, aes(x = Beta_af1, y = logLik)) +
  geom_line(data = mcap_results_af$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results_af$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_af$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_af$mle, col = 'blue') +
  labs(x = "Beta_af", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("Beta_af.png", plot=Beta_af, width=10, height=7, dpi=300)

