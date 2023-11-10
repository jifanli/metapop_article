library(tidyverse)
library(metapoppkg)
profile_A_0 <- rbind(readRDS("out_3/profile_A_0.rds"),readRDS("out_3/profile_A_02.rds"))
saveRDS(profile_A_0,"profile_A_0_full.rds")

prof_res <- profile_A_0 %>%
  group_by(A_01) %>%
  summarize(logLik = max(logLik))

mcap_results <- pomp::mcap(prof_res$logLik, prof_res$A_01)

A_0 <- ggplot() +
  geom_point(data = prof_res, aes(x = A_01, y = logLik)) +
  geom_line(data = mcap_results$fit, aes(x = parameter, y = smoothed), col = 'blue') +
  geom_vline(xintercept = mcap_results$ci[1], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$ci[2], linetype = 'dashed') +
  geom_vline(xintercept = mcap_results$mle, col = 'blue') +
  labs(x = "A_0", y = 'Log Likelihood') +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 10))

ggsave("A_0.png", plot=A_0, width=10, height=7, dpi=300)