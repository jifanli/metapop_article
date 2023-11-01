library(tidyverse)
library(metapoppkg)
profile_tau <- rbind(readRDS("out_3/profile_tau.rds"),readRDS("out_3/profile_tau2.rds"))
saveRDS(profile_tau,"profile_tau_full.rds")

prof_res <- profile_tau %>%
  group_by(tau1) %>%
  summarize(logLik = max(logLik))

mcap_results <- pomp::mcap(prof_res$logLik, prof_res$tau1)

tau <- ggplot() +
  geom_point(data = prof_res, aes(x = tau1, y = logLik)) +
  geom_line(data = mcap_results$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$mle, col = 'blue') +
  labs(x = "tau", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("tau.png", plot=tau, width=10, height=7, dpi=300)
