

source('code-uc.R')


plot(y=bc3sd+1,x=rep(1:30,each=U),log="y")
points(y=bc3sd[1,]+1,x=1:30,col="red",pch=5)
points(y=bc3sd[42,]+1,x=1:30,col="blue",pch=6)


plot(y=bc3m,x=rep(1:30,each=U))

plot(y=bc3m-t(arc3),x=rep(1:30,each=U),ty="n",xlab="day (1 is January 10)",
  ylab="block conditional log likelihood residual")
points(y=(bc3m-t(arc3))[-c(1,42),],x=rep(1:30,each=U-2))
points(y=(bc3m-t(arc3))[1,],x=1:30,col="red",pch=5)
points(y=(bc3m-t(arc3))[42,],x=1:30,col="blue",pch=6)


which((bc3m-t(arc3))< -10, arr.ind=T)
         name date
Huangshi    7   16

which((bc3m-t(arc3))< -5, arr.ind=T)
             name date
Shenzhen       16   11
Zhuhai         42   11
Huangshi        7   16
Ezhou          10   17
Ezhou          10   21
Shuangyashan   83   28



dat["Huangshi",]
#  0   0   0   0   0   0   0   0   0   0   0   0   0   0   0  31   5  17  33
#  27  55  41  43  82  71 104  57  69  68  57

# > which(colnames(mobility)=="Zhuhai")
# [1] 204
# > which(colnames(mobility)=="Wuhan")
# [1] 170
# mobility["Wuhan1",mobility["Wuhan1",]>0]

wuhan_out <- mobility[(1:373)*14-13,170]
wuhan_out[wuhan_out>0]

which((bc3m-t(arc3))< -10, arr.ind=T)


# Zhuhai has population 2.439e6 according to Wikipedia, and 1.68e6 according to li et al
# population[population[,1]=="Zhuhai",]
#  [1,] "Zhuhai" "1890000"    
# 3 cases on day 11 is unusual, but surprisingly the bigger outlier is the 1 case on day 12.
# the 3 cases may suck out the cases from Cb leaving none to transition to C the next day.

cr3 <- city_resid3 <- apply(bc3m-t(arc3),1,sum)

hist(cr3)
# shows that Zhuhai is an outlier

head(sort(cr3,decreasing=F))
#   Shenyang    Xinyang   Chaoyang     Taiwan      Jilin      Linyi 
# -12.543387 -10.814522  -6.412513  -5.159168  -4.992238  -4.991958 

# m1@unit_names[143]
# "Shenyang"

# > (bc3m-t(arc3))[143,]
#  [1]  0.243511818  0.240475705  0.228763454  0.205997700  0.170484410
#  [6]  0.119012451  0.048042494 -0.037039417 -0.133407453 -0.241472399
# [11] -0.353045237 -0.462418837  0.102415592 -0.218950448 -2.404235965
# [16] -1.947963785 -2.637169280 -1.312193632 -2.313350454 -1.253791366
# [21] -1.161105810  0.004844126 -1.040638853 -0.124846457 -0.765264539
# [26] -0.917299650  0.101585001 -0.265806613  1.310250665 -0.142747761

wuhan_out[wuhan_out>0]
      # Beijing1      Shenyang1       Yichang1     Xiangyang1        Jinmen1 
      #    25288          11062          10863           8091          11934 
      # Xiaogan1 Jinzhou.Hubei1     Huanggang1      Xianning1       Suizhou1 
      #    17792           9690          15664           7652          11304 
      #   Enshi1         Sanya1     Chongqing1       Chengdu1       Kunming1 
      #     8241           8965          12012           9797          11177 

incidence[,"Shenyang"]
# 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 2 1 2 0 1 1 2 0 1 0 0 1 0 2 1

# Likely, this is just a large city, well connected to Wuhan according to the model,
# which has surprisingly few cases under the circumstances.


