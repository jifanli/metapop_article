library(tidyverse)
library(metapoppkg)
profile_sigma_SE <- rbind(readRDS("out_3/profile_sigma_SE.rds"),readRDS("out_3/profile_sigma_SE2.rds"))
saveRDS(profile_sigma_SE,"profile_sigma_SE_full.rds")

prof_res <- profile_sigma_SE %>%
  group_by(sigma_SE1) %>%
  summarize(logLik = max(logLik))

mcap_results <- pomp::mcap(prof_res$logLik, prof_res$sigma_SE1)

sigma_SE <- ggplot() +
  geom_point(data = prof_res, aes(x = sigma_SE1, y = logLik)) +
  geom_line(data = mcap_results$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$mle, col = 'blue') +
  labs(x = "sigma_SE", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("sigma_SE.png", plot=sigma_SE, width=10, height=7, dpi=300)
