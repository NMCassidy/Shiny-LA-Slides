HiLoTen <- function(x, number = 10){
    top_ten<-head(x, n = number)
    bot_ten<-tail(x, n = number)
    top_bot_df<-rbind(top_ten, bot_ten)
}