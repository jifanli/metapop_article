library(tidyverse)
library(metapoppkg)
profile_mu_be <- rbind(readRDS("out_3/profile_mu_be.rds"),readRDS("out_3/profile_mu_be2.rds"))
profile_mu_af <- rbind(readRDS("out_3/profile_mu_af.rds"),readRDS("out_3/profile_mu_af2.rds"))
saveRDS(profile_mu_be,"profile_mu_be_full.rds")
saveRDS(profile_mu_af,"profile_mu_af_full.rds")

prof_res_be <- profile_mu_be %>%
  group_by(mu_be1) %>%
  summarize(logLik = max(logLik))

mcap_results_be <- pomp::mcap(prof_res_be$logLik, prof_res_be$mu_be1)

mu_be <- ggplot() +
  geom_point(data = prof_res_be, aes(x = mu_be1, y = logLik)) +
  geom_line(data = mcap_results_be$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results_be$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_be$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_be$mle, col = 'blue') +
  labs(x = "mu_be", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("mu_be.png", plot=mu_be, width=10, height=7, dpi=300)

prof_res_af <- profile_mu_af %>%
  group_by(mu_af1) %>%
  summarize(logLik = max(logLik))

mcap_results_af <- pomp::mcap(prof_res_af$logLik, prof_res_af$mu_af1)

mu_af <- ggplot() +
  geom_point(data = prof_res_af, aes(x = mu_af1, y = logLik)) +
  geom_line(data = mcap_results_af$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results_af$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_af$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_af$mle, col = 'blue') +
  labs(x = "mu_af", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("mu_af.png", plot=mu_af, width=10, height=7, dpi=300)
