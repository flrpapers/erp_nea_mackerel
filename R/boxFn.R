#define the summary function
fn<-function(x) {
  r=quantile(x, probs=c(0.05, 0.33, 0.5, 0.66, 0.66))
  names(r)=c("ymin", "lower", "middle", "upper", "ymax")
  r}

# sample data
d=data.frame(x=gl(2,50), y=rnorm(100))

# do it
p=ggplot(d, aes(x, y))+ 
  stat_summary(fun.data=fn, geom="boxplot")

# example with outliers
# define outlier as you want    
o <-function(x) {
  subset(x, x < quantile(x)[2] | quantile(x)[4] < x)}

filterLims<-function(x){
  
  l <- boxplot.stats(x)$stats[1]
  u <- boxplot.stats(x)$stats[5]
  
  for (i in 1:length(x))
    x[i]=ifelse(x[i]>l & x[i]<u, x[i], NA)
  
  return(x)}
