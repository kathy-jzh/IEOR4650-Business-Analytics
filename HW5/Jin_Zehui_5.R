setwd("/Users/kathy/Desktop/Columbia University/2019Spring/Business Analytics/HW5")

# Prob1 *****************************************************************************

library(xlsx)
col_index= c(1:7)
online_sale = read.xlsx(file = "home_and_kitchen_BOPS_data.xlsx", sheetIndex=1, colIndex=col_index)
bm_sale = read.xlsx(file = "home_and_kitchen_BOPS_data.xlsx", sheetIndex=3, colIndex=col_index)

# (a) B&M sales percent change
bm_percent_change = function(sales_data, affected){
  df = data.frame("Store_ID" = NA, "before_sale" = NA, "after_sale" = NA, "sales_change" = NA, "Affected" = NA)
  store_list = unique(sales_data[,1])
  for (store in store_list) {
    store_data  = sales_data[which(sales_data$id..store. == store),]
    before_sales = sum(store_data[which(store_data$after==0),]$sales)
    after_sales = sum(store_data[which(store_data$after==1),]$sales)
    df = rbind(df,list(store,before_sales,after_sales, (after_sales-before_sales)/before_sales, affected))
  }
  df = na.omit(df)
  return(df)
}
bm_sale_usa_change = bm_percent_change(bm_sale[which(bm_sale$usa==1),],affected = 1)
bm_sale_can_change = bm_percent_change(bm_sale[which(bm_sale$usa==0),],affected = 0)
mean(bm_sale_usa_change$sales_change)
mean(bm_sale_can_change$sales_change)

# (b) linear regression
df1 = rbind(bm_sale_usa_change, bm_sale_can_change)
df1 = df1[order(df1[,1]),]
attach(df1)
linReg1 = lm(sales_change~Affected)
summary(linReg1)
detach(df1)

# (c) DMA sales percent change
dma_percent_change = function(sales_data, affected){
  df = data.frame("Store_ID" = NA, "before_sale" = NA, "after_sale" = NA, "sales_change" = NA, "Affected" = NA)
  store_list = unique(sales_data[,1])
  for (store in store_list) {
    store_data  = sales_data[which(sales_data$id..DMA. == store),]
    before_sales = sum(store_data[which(store_data$after==0),]$sales)
    after_sales = sum(store_data[which(store_data$after==1),]$sales)
    df = rbind(df,list(store,before_sales,after_sales, (after_sales-before_sales)/before_sales, affected))
  }
  df = na.omit(df)
  return(df)
}
online_sale_close_change = dma_percent_change(online_sale[which(online_sale$close==1),],affected = 1)
online_sale_notclose_change = dma_percent_change(online_sale[which(online_sale$close==0),],affected = 0)
mean(online_sale_close_change$sales_change)

# (d) 
df2 = rbind(online_sale_close_change, online_sale_notclose_change)
df2 = df2[order(df2[,1]),]
attach(df2)
linReg2 = lm(sales_change~Affected)
summary(linReg2)
detach(df2)

# (e)
dma_percent_change_13m = function(sales_data, affected){
  df = data.frame("Store_ID" = NA, "before_sale" = NA, "after_sale" = NA, "sales_change" = NA, "Affected" = NA)
  store_list = unique(sales_data[,1])
  for (store in store_list) {
    store_data  = sales_data[which(sales_data$id..DMA. == store),][c(14:39),]
    before_sales = sum(store_data[which(store_data$after==0),]$sales)
    after_sales = sum(store_data[which(store_data$after==1),]$sales)
    df = rbind(df,list(store,before_sales,after_sales, (after_sales-before_sales)/before_sales, affected))
  }
  df = na.omit(df)
  return(df)
}
online_sale_close_change_13m = dma_percent_change_13m(online_sale[which(online_sale$close==1),],affected = 1)
online_sale_notclose_change_13m = dma_percent_change_13m(online_sale[which(online_sale$close==0),],affected = 0)

df3 = rbind(online_sale_close_change_13m, online_sale_notclose_change_13m)
df3 = df3[order(df3[,1]),]
attach(df3)
linReg3 = lm(sales_change~Affected)
summary(linReg3)
detach(df3)



# Prob2 *****************************************************************************

emaildata = read.csv("emails.csv",header=FALSE)

# (a)
## Pure Explore
Pure_Explore = function(emaildata){
  count = 0
  regret = rep(0, nrow(emaildata))
  for (i in c(1:nrow(emaildata))) {
    pos = sample(1:5, 1, replace=T)
    selected =  emaildata[i,][pos]
    if (selected == 1) {count = count + 1}
    regret[i] = 0.21*i - count
  }
  return(regret)
}

regret_Explore = Pure_Explore(emaildata)
plot(regret_Explore, type = "l", xlab = "iteration", ylab = "Cumulative Regret", col = "red")

## Pure Exploitation
Pure_Exploit = function(emaildata){
  count = 0
  regret = rep(0, nrow(emaildata))
  occur_time = rep(1, 5)
  pick_time = rep(1, 5)
  for (i in c(1:nrow(emaildata))) {
    pos = which.max(occur_time/pick_time)
    pick_time[pos] = pick_time[pos]+1
    selected =  emaildata[i,][pos]
    if (selected == 1) {
      count = count + 1
      occur_time[pos] = occur_time[pos]+1
    }
    regret[i] = 0.21*i - count
  }
  return(regret)
}
regret_Exploit =  Pure_Exploit(emaildata)
lines(regret_Exploit, type = "l", col = "blue")

## Explore then Exploit
Explore_Exploit = function(emaildata, T0){
  count = 0
  regret = rep(0, nrow(emaildata))
  occur_time = rep(1, 5)
  pick_time = rep(1, 5)
  for (i in c(1:nrow(emaildata))) {
    pos = ifelse(i<T0, sample(1:5, 1, replace=T), which.max(occur_time/pick_time))
    pick_time[pos] = pick_time[pos]+1
    selected =  emaildata[i,][pos]
    if (selected == 1) {
      count = count + 1
      occur_time[pos] = occur_time[pos]+1
    }
    regret[i] = 0.21*i - count
  }
  return(regret)
}
regret_Explore_Exploit =  Explore_Exploit(emaildata,100)
lines(regret_Explore_Exploit, type = "l", col = "purple")

## Epsilon Greedy
Epsion_Greedy = function(emaildata){
  count = 0
  regret = rep(0, nrow(emaildata))
  occur_time = rep(1, 5)
  pick_time = rep(1, 5)
  for (i in c(1:nrow(emaildata))) {
    epsilon = 1/i
    pos = ifelse(runif(1)<epsilon, sample(1:5, 1, replace=T), which.max(occur_time/pick_time))
    pick_time[pos] = pick_time[pos]+1
    selected =  emaildata[i,][pos]
    if (selected == 1) {
      count = count + 1
      occur_time[pos] = occur_time[pos]+1
    }
    regret[i] = 0.21*i - count
  }
  return(regret)
}
regret_EG =  Epsion_Greedy(emaildata)
lines(regret_EG, type = "l", col = "green")

## UCB
UCB = function(emaildata, alpha){
  count = 0
  regret = rep(0, nrow(emaildata))
  occur_time = rep(1, 5)
  pick_time = rep(1, 5)
  for (i in c(1:nrow(emaildata))) {
    pos = which.max(occur_time/pick_time+alpha*sqrt(log(i)/pick_time))
    pick_time[pos] = pick_time[pos]+1
    selected =  emaildata[i,][pos]
    if (selected == 1) {
      count = count + 1
      occur_time[pos] = occur_time[pos]+1
    }
    regret[i] = 0.21*i - count
  }
  return(list(regret, pick_time))
}
regret_UCB =  UCB(emaildata, 0.75)[[1]]
lines(regret_UCB, type = "l", col = "orange")

legend(1, 350, legend=c("Pure Explore", "Pure Exploitation", "Explore then Exploit", "Epsilon Greedy", "UCB"),
       col=c("red", "blue", "purple", "green", "orange"), lty=1:2, cex=0.8)

# (b)
dist1 = (UCB(emaildata[1:100,], 0.75)[[2]] - 1)/100
dist2 = (UCB(emaildata[(nrow(emaildata)-99):nrow(emaildata),], 0.75)[[2]] - 1)/100
dist1; dist2
