
# pulls results from code-uc.R

i <- 2

U <-switch(i,5,373)

out_dir <- paste0("out_c_",i,"/")


load(file=paste0(out_dir,"filters.rda"))

load(file=paste0(out_dir,"ar.rda"))

rownames(bc3m) <- rownames(bc3sd)

cr3 <- city_resid3 <- apply(bc3m-t(arc3),1,sum)
round(head(sort(cr3,decreasing=F),10),1)

round(head(sort(cr3,decreasing=F),10),1)
       #  Wuhan       Xinyang         Linyi      Chaoyang      Shenyang 
       #  -43.5          -9.0          -5.9          -4.9          -4.3 
       # Taiwan         Jilin Daxing.anling    Mudanjiang        Ledong 
       #   -4.2          -3.6          -3.4          -3.4          -3.3

# Create a data frame with the required data
colored <- c("Wuhan","Huangshi")

data_c <- data.frame(
  day = rep(1:30, each = U),
  residual = as.numeric(bc3m - t(arc3)),
  city = rep(rownames(bc3m),30)
)

library(ggplot2)

# Create the ggplot
anomaly_c <- ggplot(data_c, aes(x = day, y = residual)) +
#  geom_blank() +  # Set up an empty plot to define the axes and labels
  labs(
    x = "day (1 is January 10)",
    y = "block conditional log likelihood residual"
  ) +
  geom_point(data = subset(data_c, city != "Wuhan"), aes(color = city), size = 2) +
  geom_point(data = subset(data_c, city == "Wuhan"), aes(color = city), shape = 15, size = 3,color="red") +
  geom_point(data = subset(data_c, city == "Huangshi"), aes(color = city), shape = 17, size = 3,color="green") +  
  scale_color_manual(values = c("red" = "red", "green" = "green"))


