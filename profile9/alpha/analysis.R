library(tidyverse)
library(metapoppkg)
profile_alpha_be <- rbind(readRDS("out_3/profile_alpha_be.rds"),readRDS("out_3/profile_alpha_be2.rds"))
profile_alpha_af <- rbind(readRDS("out_3/profile_alpha_af.rds"),readRDS("out_3/profile_alpha_af2.rds"))
saveRDS(profile_alpha_be,"profile_alpha_be_full.rds")
saveRDS(profile_alpha_af,"profile_alpha_af_full.rds")

prof_res_be <- profile_alpha_be %>%
  group_by(alpha_be1) %>%
  summarize(logLik = max(logLik))

mcap_results_be <- pomp::mcap(prof_res_be$logLik, prof_res_be$alpha_be1)

alpha_be <- ggplot() +
  geom_point(data = prof_res_be, aes(x = alpha_be1, y = logLik)) +
  geom_line(data = mcap_results_be$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results_be$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_be$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_be$mle, col = 'blue') +
  labs(x = "alpha_be", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("alpha_be.png", plot=alpha_be, width=10, height=7, dpi=300)


prof_res_af <- profile_alpha_af %>%
  group_by(alpha_af1) %>%
  summarize(logLik = max(logLik))

mcap_results_af <- pomp::mcap(prof_res_af$logLik, prof_res_af$alpha_af1)

alpha_af <- ggplot() +
  geom_point(data = prof_res_af, aes(x = alpha_af1, y = logLik)) +
  geom_line(data = mcap_results_af$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results_af$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_af$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_af$mle, col = 'blue') +
  labs(x = "alpha_af", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("alpha_af.png", plot=alpha_af, width=10, height=7, dpi=300)
