# pfilter1 does not give an error, whereas pfilter2 does.
# The only difference between them is that the name "list" has been changed to "li23_list" (or any name other than "list").
library(metapoppkg)
li23_mod <- li23(U = 2, for_ibpf = F)

list <- vector("list", length = 1)
list[[1]] <- li23_mod
li23_list <- vector("list", length = 1)
li23_list[[1]] <- li23_mod

pfilter1 <- stew(file="pfilter_test1.rda",seed=53285,{
  pfilter(li23_list[[1]],Np=2) -> pfilter_out
})

pfilter2 <- stew(file="pfilter_test2.rda",seed=53286,{
  pfilter(list[[1]],Np=2) -> pfilter_out
})

