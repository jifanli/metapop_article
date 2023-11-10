library(tidyverse)
library(metapoppkg)
profile_Z_be <- rbind(readRDS("out_3/profile_Z_be.rds"),readRDS("out_3/profile_Z_be2.rds"))
profile_Z_af <- rbind(readRDS("out_3/profile_Z_af.rds"),readRDS("out_3/profile_Z_af2.rds"))
saveRDS(profile_Z_be,"profile_Z_be_full.rds")
saveRDS(profile_Z_af,"profile_Z_af_full.rds")

prof_res_be <- profile_Z_be %>%
  group_by(Z_be1) %>%
  summarize(logLik = max(logLik))

mcap_results_be <- pomp::mcap(prof_res_be$logLik, prof_res_be$Z_be1)

Z_be <- ggplot() +
  geom_point(data = prof_res_be, aes(x = Z_be1, y = logLik)) +
  geom_line(data = mcap_results_be$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results_be$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_be$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_be$mle, col = 'blue') +
  labs(x = "Z_be", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("Z_be.png", plot=Z_be, width=10, height=7, dpi=300)

prof_res_af <- profile_Z_af %>%
  group_by(Z_af1) %>%
  summarize(logLik = max(logLik))

mcap_results_af <- pomp::mcap(prof_res_af$logLik, prof_res_af$Z_af1)

Z_af <- ggplot() +
  geom_point(data = prof_res_af, aes(x = Z_af1, y = logLik)) +
  geom_line(data = mcap_results_af$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results_af$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_af$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_af$mle, col = 'blue') +
  labs(x = "Z_af", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("Z_af.png", plot=Z_af, width=10, height=7, dpi=300)

