library(tidyverse)
library(metapoppkg)
profile_theta <- rbind(readRDS("out_3/profile_theta.rds"),readRDS("out_3/profile_theta2.rds"))
saveRDS(profile_theta,"profile_theta_full.rds")

prof_res <- profile_theta %>%
  group_by(theta1) %>%
  summarize(logLik = max(logLik))

mcap_results <- pomp::mcap(prof_res$logLik, prof_res$theta1)

theta <- ggplot() +
  geom_point(data = prof_res, aes(x = theta1, y = logLik)) +
  geom_line(data = mcap_results$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$mle, col = 'blue') +
  labs(x = "theta", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("theta.png", plot=theta, width=10, height=7, dpi=300)
