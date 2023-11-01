
# source('code-uc.R')
# pulls results from code-uc.R

i <- 2

U <-switch(i,5,373)

out_dir <- paste0("out_uc_",i,"/")


load(file=paste0(out_dir,"filters.rda"))

load(file=paste0(out_dir,"ar.rda"))

rownames(bc3m) <- rownames(bc3sd)

if(0){
pdf("residual_time_plot_uc.pdf")
colored <- c("Wuhan","Huangshi")
plot(y=bc3m-t(arc3),x=rep(1:30,each=U),ty="n",xlab="day (1 is January 10)",
  ylab="block conditional log likelihood residual")
points(y=(bc3m-t(arc3))[!rownames(bc3m)%in%colored,],x=rep(1:30,each=U-length(colored)))
points(y=(bc3m-t(arc3))[colored[1],],x=1:30,col="red",pch=15)
points(y=(bc3m-t(arc3))[colored[2],],x=1:30,col="green",pch=17)
dev.off()
}

cr3 <- city_resid3 <- apply(bc3m-t(arc3),1,sum)
round(head(sort(cr3,decreasing=F),10),1)
     # Xinyang         Linyi      Chaoyang        Taiwan      Shenyang 
     #     -9.0          -6.1          -5.0          -4.7          -3.9 
     #    Jilin     Sanmenxia        Ledong Daxing.anling    Mudanjiang 
     #     -3.8          -3.7          -3.7          -3.3          -3.2 

incidence[,"Shenyang"]
incidence[,"Xinyang"]

# Load the ggplot2 library
library(ggplot2)

# Create a data frame with the required data
colored <- c("Wuhan","Huangshi")

data_uc <- data.frame(
  day = rep(1:30, each = U),
  residual = as.numeric(bc3m - t(arc3)),
  city = rep(rownames(bc3m),30)
)

# Create the ggplot
anomaly_uc <- ggplot(data_uc, aes(x = day, y = residual)) +
#  geom_blank() +  # Set up an empty plot to define the axes and labels
  labs(
    x = "day (1 is January 10)",
    y = "block conditional log likelihood residual"
  ) +
  geom_point(data = subset(data_uc, city != "Wuhan"), aes(color = city), size = 2) +
  geom_point(data = subset(data_uc, city == "Wuhan"), aes(color = city), shape = 15, size = 3,color="red") +
  geom_point(data = subset(data_uc, city == "Huangshi"), aes(color = city), shape = 17, size = 3,color="green") +  
  scale_color_manual(values = c("red" = "red", "green" = "green"))
