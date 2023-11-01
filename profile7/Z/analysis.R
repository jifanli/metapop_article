library(tidyverse)
library(metapoppkg)
profile_Z <- rbind(readRDS("out_3/profile_Z.rds"),readRDS("out_3/profile_Z2.rds"))
saveRDS(profile_Z,"profile_Z_full.rds")

prof_res <- profile_Z %>%
  group_by(Z1) %>%
  summarize(logLik = max(logLik))

mcap_results <- pomp::mcap(prof_res$logLik, prof_res$Z1)

Z <- ggplot() +
  geom_point(data = prof_res, aes(x = Z1, y = logLik)) +
  geom_line(data = mcap_results$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$mle, col = 'blue') +
  labs(x = "Z", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("Z.png", plot=Z, width=10, height=7, dpi=300)
