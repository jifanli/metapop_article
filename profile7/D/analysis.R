library(tidyverse)
library(metapoppkg)
profile_D <- rbind(readRDS("out_3/profile_D.rds"),readRDS("out_3/profile_D2.rds"))
saveRDS(profile_D,"profile_D_full.rds")

prof_res <- profile_D %>%
  group_by(D1) %>%
  summarize(logLik = max(logLik))

mcap_results <- pomp::mcap(prof_res$logLik, prof_res$D1)

D <- ggplot() +
  geom_point(data = prof_res, aes(x = D1, y = logLik)) +
  geom_line(data = mcap_results$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$mle, col = 'blue') +
  labs(x = "D", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("D.png", plot=D, width=10, height=7, dpi=300)

