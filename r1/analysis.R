
library(tidyverse)
library(patchwork)

i <- 2

U <-switch(i,5,373)

# pulls results from code-c.R

out_dir <- paste0("r1/out_c_",i,"/")
load(file=paste0(out_dir,"filters.rda"))
load(file=paste0(out_dir,"ar.rda"))
rownames(bc3m) <- rownames(bc3sd)
data_c <- data.frame(
  day = rep(1:30, each = U),
  residual = as.numeric(bc3m - t(arc3)),
  city = rep(rownames(bc3m),30)
)

# pulls results from code-uc.R
out_dir <- paste0("r1/out_uc_",i,"/")
load(file=paste0(out_dir,"filters.rda"))
load(file=paste0(out_dir,"ar.rda"))
rownames(bc3m) <- rownames(bc3sd)
data_uc <- data.frame(
  day = rep(1:30, each = U),
  residual = as.numeric(bc3m - t(arc3)),
  city = rep(rownames(bc3m),30)
)

anomaly_range <- range(c(data_c$residual,data_uc$residual)) 

anomaly_c <- ggplot(data_c, aes(x = day, y = residual)) +
  labs(
    title= expression(B.~Model~M[6]),  
    x = "Day", # 1 is Jan 10
    y = "block conditional log likelihood residual"
  ) +
  ylim(anomaly_range) +
  geom_point(data = subset(data_c, city != "Wuhan"), aes(color = city), size = 2) +
  geom_point(data = subset(data_c, city == "Wuhan"), aes(color = city), shape = 15, size = 3,color="red") +
  geom_point(data = subset(data_c, city == "Huangshi"), aes(color = city), shape = 17, size = 3,color="green") +  
  scale_color_manual(values = c("red" = "red", "green" = "green")) +
   theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),
     axis.title.y=element_blank())




##### repeat with unconstrained fit, re-using some object names


anomaly_uc <- ggplot(data_uc, aes(x = day, y = residual)) +
  labs(
    title= expression(A.~Model~M[5]),  
    x = "Day",
    y = "Block conditional log-likelihood anomaly"
  ) +
  ylim(anomaly_range) +
  geom_point(data = subset(data_uc, city != "Wuhan"), aes(color = city), size = 2) +
  geom_point(data = subset(data_uc, city == "Wuhan"), aes(color = city), shape = 15, size = 3,color="red") +
  geom_point(data = subset(data_uc, city == "Huangshi"), aes(color = city), shape = 17, size = 3,color="green") +  
  scale_color_manual(values = c("red" = "red", "green" = "green"))

anomaly_uc + anomaly_c