library(tidyverse)
library(metapoppkg)
profile_R0_be <- rbind(readRDS("out_3/profile_R0_be.rds"),readRDS("out_3/profile_R0_be2.rds"))
profile_R0_af <- rbind(readRDS("out_3/profile_R0_af.rds"),readRDS("out_3/profile_R0_af2.rds"))
saveRDS(profile_R0_be,"profile_R0_be_full.rds")
saveRDS(profile_R0_af,"profile_R0_af_full.rds")

prof_res_be <- profile_R0_be %>%
  group_by(R0_be1) %>%
  summarize(logLik = max(logLik))

mcap_results_be <- pomp::mcap(prof_res_be$logLik, prof_res_be$R0_be1)

R0_be <- ggplot() +
  geom_point(data = prof_res_be, aes(x = R0_be1, y = logLik)) +
  geom_line(data = mcap_results_be$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results_be$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_be$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_be$mle, col = 'blue') +
  labs(x = "R0_be", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("R0_be.png", plot=R0_be, width=10, height=7, dpi=300)

prof_res_af <- profile_R0_af %>%
  group_by(R0_af1) %>%
  summarize(logLik = max(logLik))

mcap_results_af <- pomp::mcap(prof_res_af$logLik, prof_res_af$R0_af1)

R0_af <- ggplot() +
  geom_point(data = prof_res_af, aes(x = R0_af1, y = logLik)) +
  geom_line(data = mcap_results_af$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results_af$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_af$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results_af$mle, col = 'blue') +
  labs(x = "R0_af", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("R0_af.png", plot=R0_af, width=10, height=7, dpi=300)

