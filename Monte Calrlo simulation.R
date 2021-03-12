# Monte Carlo simulation example
B<-10000
result <-replicate(B,{
  celtic_wins <- sample(c(0,1),7,replace=TRUE, prob=c(0.6,0.4))
  sum(celtic_wins)>=1
})


mean(result)

  