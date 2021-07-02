# install the required packages first
require(jsonlite)
require(httr)
require(data.table)

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32737302":2.4,"32939029":2.4,"4066298":2.4,"48740784":2.4,"6676673":2.4, "7061886":2.4, "73318567":2.4, "85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://46.101.163.177'

u_name = "Group16"
p_word = "gBYJop18sJJOwsvk"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)

predictions=unique(data[,list(product_content_id)])
predictions[,forecast:=2.3]

send_submission(predictions, token, url=subm_url, submit_now=F)

# this part is main code


library(caTools)
library(xts)
library(zoo)
library(forecast)

data
summary(data)

data[, is.discount_days:= 0]
rawdata <- read.csv("/Users/tolgaerdogan/Desktop/ProjectRawData.csv", header=TRUE, sep = ";")

rawdata$event_date <- as.Date(rawdata$event_date , format = "%d.%m.%Y")

alldata<-rbind(rawdata,data)
alldata<-as.data.table(alldata)

alldata$is.discount_days[alldata$event_date=="2021-03-08"] <- 1
alldata$is.discount_days[alldata$event_date=="2021-03-09"] <- 1
alldata$is.discount_days[alldata$event_date=="2021-03-10"] <- 1
alldata$is.discount_days[alldata$event_date=="2021-03-11"] <- 1
alldata$is.discount_days[alldata$event_date=="2021-03-12"] <- 1

## Some of discount days are set in ProjectRawData.csv

str(alldata)

n<-400


dates<- seq(as.Date("2020-05-25"), length = n, by = "days")
dates


testation_dates<-seq(as.Date("2021-03-01"), length = n-280, by = "days")
testation_dates

mont<-subset(alldata, alldata$product_content_id==48740784)
summary(mont)


plot(dates,mont$sold_count)
mont_xts<-xts(mont,order.by=dates)

mont_train_xts<-mont_xts[index(mont_xts)<"2021-03-01"]
mont_test_xts<-mont_xts[index(mont_xts)>="2021-03-01"]


mont_xreg1<-cbind(mont[1:280,"favored_count"],
                  mont[1:280,"basket_count"])

mont_xreg2<-cbind(mont[281:n,"favored_count"],
                  mont[281:n,"basket_count"])

auto.arima(as.numeric(mont_train_xts$sold_count))


arima_mont<-Arima(as.numeric(mont_train_xts$sold_count),xreg=as.matrix(mont_xreg1),order=c(3,1,3))

summary(arima_mont)

forecast_arima_mont<-forecast(arima_mont,xreg=as.matrix(mont_xreg2))

forecast_arima_mont_xts<-xts(forecast_arima_mont$mean,order.by=testation_dates)

plot(dates,mont$sold_count,type='l')
lines(testation_dates,forecast_arima_mont_xts,col="blue")

mont_lm_train<-mont[1:280,]
mont_lm_test<-mont[281:n,]


mont_lm_model<-lm(sold_count~favored_count+basket_count,data=mont_lm_train)

predict_mont_lm<-predict(mont_lm_model,mont_lm_test)

predict_mont_lm

predict_mont_lm_xts<- xts(predict_mont_lm,order.by=testation_dates)


plot(dates,mont$sold_count)
lines(testation_dates,predict_mont_lm_xts, col = "blue")

summary(mont_lm_model)

mont_favored_xreg<-as.numeric(mont_xts$favored_count)
mont_favored_model<-auto.arima(mont_favored_xreg)
mont_favored_forecast<-forecast(mont_favored_model,h=2)

mont_basket_xreg<-as.numeric(mont_xts$basket_count)
mont_basket_model<-auto.arima(mont_basket_xreg)
mont_basket_forecast<-forecast(mont_basket_model,h=2)

mont_submission_xreg1<-cbind(mont[1:n,"favored_count"],
                             mont[1:n,"basket_count"])
mont_submission_xreg2<-data.table("favored_count"=mont_favored_forecast$mean,
                                  "basket_count"=mont_basket_forecast$mean)

mont_submission_xts<-as.numeric(mont_xts$sold_count)

mont_arimasub<-Arima(mont_submission_xts,
                                   xreg=as.matrix(mont_submission_xreg1),
                                   order=c(3,1,3))

mont_submission_forecast_arima<-forecast(mont_arimasub,
                                         xreg=as.matrix(mont_submission_xreg2))

mont_lm_submission<-lm(sold_count~favored_count+basket_count,data=mont)

pred_mont_lm_submodel<-predict(mont_lm_submission,mont_submission_xreg2)


mont_submission_arima<-0.5*(mont_submission_forecast_arima$mean[2]+pred_mont_lm_submodel[2])
mont_submission_arima

oralb<-subset(alldata, alldata$product_content_id==32939029)

oralb


plot(dates,oralb$sold_count)


oralb_xts<-xts(oralb,order.by=oralb$event_date)


oralb_train_xts<-oralb_xts[index(oralb_xts)<"2021-03-01"]

oralb_test_xts<-oralb_xts[index(oralb_xts)>="2021-03-01"]

oralb_xreg1<-cbind( oralb[1:280,"visit_count"],
                         oralb[1:280,"favored_count"],
                         oralb[1:280,"basket_count"])

oralb_xreg2<-cbind( oralb[281:n,"visit_count"],
                         oralb[281:n,"favored_count"],
                         oralb[281:n,"basket_count"])
auto.arima(as.numeric(oralb_train_xts$sold_count))

oralb_arima_model<-Arima(as.numeric(oralb_train_xts$sold_count),
                              xreg=as.matrix(oralb_xreg1),
                              order=c(2,1,1))

AIC(oralb_arima_model)

oralb_forecast_arima<-forecast(oralb_arima_model,xreg=as.matrix(oralb_xreg2))

oralb_forecast_arima_xts<-xts(oralb_forecast_arima$mean,order.by=testation_dates)


plot(dates,oralb$sold_count, type="l")
lines(testation_dates,oralb_forecast_arima_xts,col="blue")


oralb_ARIMA_MAPE<-100*mean(abs((as.numeric(oralb_forecast_arima_xts)-as.numeric(oralb_test_xts$sold_count))/as.numeric(oralb_test_xts$sold_count)))
oralb_ARIMA_MAPE

oralb_lm_train<-oralb[1:280,]
oralb_lm_test<-oralb[281:n,]

oralb_lm_model<-lm(sold_count~visit_count+favored_count+basket_count,data=oralb_lm_train)

pred_oralb_lm_model<-predict(oralb_lm_model,oralb_lm_test)

pred_oralb_lm_model_xts<-xts(pred_oralb_lm_model,order.by = testation_dates)

plot(pred_oralb_lm_model_xts)
lines(oralb_test_xts$sold_count)

plot(dates,oralb$sold_count)
lines(testation_dates,pred_oralb_lm_model_xts,col="blue")

oralb_reg_MAPE<-100*mean(abs((pred_oralb_lm_model_xts-as.numeric(oralb_test_xts$sold_count))/as.numeric(oralb_test_xts$sold_count)))
oralb_reg_MAPE

oralb_visit_xreg<-as.numeric(oralb_xts$visit_count)
oralb_visit_model<-auto.arima(oralb_visit_xreg)
oralb_visit_forecast<-forecast(oralb_visit_model,h=2)

oralb_favored_xreg<-as.numeric(oralb_xts$favored_count)
oralb_favored_model<-auto.arima(oralb_favored_xreg)
oralb_favored_forecast<-forecast(oralb_favored_model,h=2)

oralb_basket_xreg<-as.numeric(oralb_xts$basket_count)
oralb_basket_model<-auto.arima(oralb_basket_xreg)
oralb_basket_forecast<-forecast(oralb_basket_model,h=2)

oralb_submission_xreg1<-cbind(oralb[1:n,"visit_count"],
                                   oralb[1:n,"favored_count"],
                                   oralb[1:n,"basket_count"])
oralb_submission_xreg2<-data.table("visit_count"=oralb_visit_forecast$mean,
                                        "favored_count"=oralb_favored_forecast$mean,
                                        "basket_count"=oralb_basket_forecast$mean)

oralb_submission_xts<-as.numeric(oralb_xts$sold_count)

oralb_arimasub<-Arima(oralb_submission_xts,
                                         xreg=as.matrix(oralb_submission_xreg1),
                                         order=c(2,1,2))

oralb_submission_forecast_arima<-forecast(oralb_arimasub,
                                               xreg=as.matrix(oralb_submission_xreg2))


oralb_lm_submission<-lm(sold_count~visit_count+favored_count+basket_count,data=oralb)

pred_oralb_lm_submodel<-predict(oralb_lm_submission,oralb_submission_xreg2)


oralb_submission_arima<-0.5*(oralb_submission_forecast_arima$mean[2]+pred_oralb_lm_submodel[2])

oralb_submission_arima


bikini1<-subset(alldata, alldata$product_content_id==73318567)

plot(dates,bikini1$sold_count)

bikini1_xts<-xts(bikini1,order.by=dates)

plot(testation_dates, bikini1_xts$sold_count)


bikini1_train_xts<-bikini1_xts[index(bikini1_xts)<"2021-03-01"]

bikini1_test_xts<-bikini1_xts[index(bikini1_xts)>="2021-03-01"]


bikini1_xreg1<-cbind( bikini1[1:280,"visit_count"],
                      bikini1[1:280,"favored_count"],
                      bikini1[1:280,"basket_count"])

bikini1_xreg2<-cbind( bikini1[281:n,"visit_count"],
                      bikini1[281:n,"favored_count"],
                      bikini1[281:n,"basket_count"])

auto.arima(as.numeric(bikini1_train_xts$sold_count))

bikini1_arima_model<-Arima(as.numeric(bikini1_train_xts$sold_count),
                           xreg=as.matrix(bikini1_xreg1),
                           order=c(2,1,2))

AIC(bikini1_arima_model)

bikini1_forecast_arima<-forecast(bikini1_arima_model,xreg=as.matrix(bikini1_xreg2))

bikini1_forecast_arima_xts<-xts(bikini1_forecast_arima$mean,order.by=testation_dates)


plot(dates,bikini1$sold_count, type="l")
lines(testation_dates,bikini1_forecast_arima_xts,col="blue")


bikini1_ARIMA_MAPE<-100*mean(abs((as.numeric(bikini1_forecast_arima_xts)-as.numeric(bikini1_test_xts$sold_count))/as.numeric(bikini1_test_xts$sold_count)))
bikini1_ARIMA_MAPE


bikini1_lm_train<-bikini1[1:280,]
bikini1_lm_test<-bikini1[281:n,]

bikini1_lm_model<-lm(sold_count~visit_count+favored_count+basket_count,data=bikini1_lm_train)

pred_bikini1_lm_model<-predict(bikini1_lm_model,bikini1_lm_test)

pred_bikini1_lm_model_xts<-xts(pred_bikini1_lm_model,order.by = testation_dates)

plot(pred_bikini1_lm_model_xts)
lines(bikini1_test_xts$sold_count)

bikini1_reg_MAPE<-100*mean(abs((pred_bikini1_lm_model_xts-as.numeric(bikini1_test_xts$sold_count))/as.numeric(bikini1_test_xts$sold_count)))
bikini1_reg_MAPE


bikini1_visit_xreg<-as.numeric(bikini1_xts$visit_count)
bikini1_visit_model<-auto.arima(bikini1_visit_xreg)
bikini1_visit_forecast<-forecast(bikini1_visit_model,h=2)

bikini1_favored_xreg<-as.numeric(bikini1_xts$favored_count)
bikini1_favored_model<-auto.arima(bikini1_favored_xreg)
bikini1_favored_forecast<-forecast(bikini1_favored_model,h=2)

bikini1_basket_xreg<-as.numeric(bikini1_xts$basket_count)
bikini1_basket_model<-auto.arima(bikini1_basket_xreg)
bikini1_basket_forecast<-forecast(bikini1_basket_model,h=2)



bikini1_submission_xreg1<-cbind( bikini1[1:n,"visit_count"],
                                 bikini1[1:n,"favored_count"],
                                 bikini1[1:n,"basket_count"])

bikini1_submission_xreg2<-data.table("visit_count"=bikini1_visit_forecast$mean,
                                     "favored_count"=bikini1_favored_forecast$mean,
                                     "basket_count"=bikini1_basket_forecast$mean)



bikini1_submission_xts<-as.numeric(bikini1_xts$sold_count)

bikini1_arimasub<-Arima(bikini1_submission_xts,
                                      xreg=as.matrix(bikini1_submission_xreg1),
                                      order=c(0,1,0))

bikini1_submission_forecast_arima<-forecast(bikini1_arimasub,
                                            xreg=as.matrix(bikini1_submission_xreg2))



bikini1_lm_submission<-lm(sold_count~visit_count+favored_count+basket_count,data=bikini1)

pred_bikini1_lm_submodel<-predict(bikini1_lm_submission,bikini1_submission_xreg2)


bikini1_submission_arima<-0.5*(bikini1_submission_forecast_arima$mean[2]+pred_bikini1_lm_submodel[2])

bikini1_submission_arima


bikini2<-subset(alldata, alldata$product_content_id==32737302)

bikini2

plot(dates,bikini2$sold_count)


bikini2_xts<-xts(bikini2,order.by=dates)


bikini2_train_xts<-bikini2_xts[index(bikini2_xts)<"2021-03-01"]

bikini2_test_xts<-bikini2_xts[index(bikini2_xts)>="2021-03-01"]

bikini2_xreg1<-cbind( bikini2[1:280,"visit_count"],
                      bikini2[1:280,"favored_count"],
                      bikini2[1:280,"basket_count"])

bikini2_xreg2<-cbind( bikini2[281:n,"visit_count"],
                      bikini2[281:n,"favored_count"],
                      bikini2[281:n,"basket_count"])
auto.arima(as.numeric(bikini2_train_xts$sold_count))

bikini2_arima_model<-Arima(as.numeric(bikini2_train_xts$sold_count),
                           xreg=as.matrix(bikini2_xreg1),
                           order=c(5,2,1))

AIC(bikini2_arima_model)

bikini2_forecast_arima<-forecast(bikini2_arima_model,xreg=as.matrix(bikini2_xreg2))

bikini2_forecast_arima_xts<-xts(bikini2_forecast_arima$mean,order.by=testation_dates)



plot(dates,bikini2$sold_count, type="l")
lines(testation_dates,bikini2_forecast_arima_xts,col="blue")


bikini2_ARIMA_MAPE<-100*mean(abs((as.numeric(bikini2_forecast_arima_xts)-as.numeric(bikini2_test_xts$sold_count))/as.numeric(bikini2_test_xts$sold_count)))
bikini2_ARIMA_MAPE


bikini2_lm_train<-bikini2[1:280,]
bikini2_lm_test<-bikini2[281:n,]


bikini2_lm_model<-lm(sold_count~visit_count+favored_count+basket_count,data=bikini2_lm_train)

pred_bikini2_lm_model<-predict(bikini2_lm_model,bikini2_lm_test)


pred_bikini2_lm_model_xts<-xts(pred_bikini2_lm_model,order.by = testation_dates)

plot(pred_bikini2_lm_model_xts)
lines(bikini2_test_xts$sold_count,col="blue")
lines(bikini2_forecast_arima_xts, col="red")

plot(dates,bikini2$sold_count)
lines(testation_dates,pred_bikini2_lm_model_xts,col="blue")

bikini2_reg_MAPE<-100*mean(abs((pred_bikini2_lm_model_xts-as.numeric(bikini2_test_xts$sold_count))/as.numeric(bikini2_test_xts$sold_count)))
bikini2_reg_MAPE



bikini2_visit_xreg<-as.numeric(bikini2_xts$visit_count)
bikini2_visit_model<-auto.arima(bikini2_visit_xreg)
bikini2_visit_forecast<-forecast(bikini2_visit_model,h=2)

bikini2_favored_xreg<-as.numeric(bikini2_xts$favored_count)
bikini2_favored_model<-auto.arima(bikini2_favored_xreg)
bikini2_favored_forecast<-forecast(bikini2_favored_model,h=2)

bikini2_basket_xreg<-as.numeric(bikini2_xts$basket_count)
bikini2_basket_model<-auto.arima(bikini2_basket_xreg)
bikini2_basket_forecast<-forecast(bikini2_basket_model,h=2)




bikini2_submission_xreg1<-cbind(bikini2[1:n,"visit_count"],
                                bikini2[1:n,"favored_count"],
                                bikini2[1:n,"basket_count"])
bikini2_submission_xreg2<-data.table("visit_count"=bikini2_visit_forecast$mean,
                                     "favored_count"=bikini2_favored_forecast$mean,
                                     "basket_count"=bikini2_basket_forecast$mean)

bikini2_submission_xts<-as.numeric(bikini2_xts$sold_count)

bikini2_arimasub<-Arima(bikini2_submission_xts,
                                      xreg=as.matrix(bikini2_submission_xreg1),
                                      order=c(5,2,1))

bikini2_submission_forecast_arima<-forecast(bikini2_arimasub,
                                            xreg=as.matrix(bikini2_submission_xreg2))


bikini2_lm_submission<-lm(sold_count~visit_count+favored_count+basket_count,data=bikini2)

pred_bikini2_lm_submodel<-predict(bikini2_lm_submission,bikini2_submission_xreg2)

bikini2_submission_arima<-0.5*(bikini2_submission_forecast_arima$mean[2]+pred_bikini2_lm_submodel[2])

bikini2_submission_arima



tayt<-subset(alldata, alldata$product_content_id==31515569)

tayt

plot(dates,tayt$sold_count)

tayt_xts<-xts(tayt,order.by=dates)

tayt_train_xts<-tayt_xts[index(tayt_xts)<"2021-03-01"]

tayt_test_xts<-tayt_xts[index(tayt_xts)>="2021-03-01"]


tayt_xreg1<-cbind( tayt[1:280,"visit_count"],
                   tayt[1:280,"is.discount_days"],
                   tayt[1:280,"favored_count"],
                   tayt[1:280,"basket_count"])

tayt_xreg2<-cbind( tayt[281:n,"visit_count"],
                   tayt[281:n,"is.discount_days"],
                   tayt[281:n,"favored_count"],
                   tayt[281:n,"basket_count"])
auto.arima(as.numeric(tayt_train_xts$sold_count))

str(tayt_xreg1)
tayt_arima_model<-Arima(as.numeric(tayt_train_xts$sold_count),
                        xreg=as.matrix(tayt_xreg1),
                        order=c(4,1,1))
AIC(tayt_arima_model)

tayt_forecast_arima<-forecast(tayt_arima_model,xreg=as.matrix(tayt_xreg2))

tayt_forecast_arima_xts<-xts(tayt_forecast_arima$mean,order.by=testation_dates)



plot(dates,tayt$sold_count, type="l")
lines(testation_dates,tayt_forecast_arima_xts,col="blue")


tayt_ARIMA_MAPE<-100*mean(abs((as.numeric(tayt_forecast_arima_xts)-as.numeric(tayt_test_xts$sold_count))/as.numeric(tayt_test_xts$sold_count)))
tayt_ARIMA_MAPE



tayt_lm_train<-tayt[1:280,]
tayt_lm_test<-tayt[281:n,]


tayt_lm_model<-lm(sold_count~visit_count+is.discount_days+favored_count+basket_count,data=tayt_lm_train)

summary(tayt_lm_model)

pred_tayt_lm_model<-predict(tayt_lm_model,tayt_lm_test)


pred_tayt_lm_model_xts<-xts(pred_tayt_lm_model,order.by = testation_dates)

plot(pred_tayt_lm_model_xts, col="blue")
lines(tayt_test_xts$sold_count,col="black")
lines(tayt_forecast_arima_xts, col="red")

plot(dates,tayt$sold_count)
lines(testation_dates,pred_tayt_lm_model_xts,col="blue")

tayt_reg_MAPE<-100*mean(abs((pred_tayt_lm_model_xts-as.numeric(tayt_test_xts$sold_count))/as.numeric(tayt_test_xts$sold_count)))
tayt_reg_MAPE



tayt_visit_xreg<-as.numeric(tayt_xts$visit_count)
tayt_visit_model<-auto.arima(tayt_visit_xreg)
tayt_visit_forecast<-forecast(tayt_visit_model,h=2)

tayt_favored_xreg<-as.numeric(tayt_xts$favored_count)
tayt_favored_model<-auto.arima(tayt_favored_xreg)
tayt_favored_forecast<-forecast(tayt_favored_model,h=2)

tayt_basket_xreg<-as.numeric(tayt_xts$basket_count)
tayt_basket_model<-auto.arima(tayt_basket_xreg)
tayt_basket_forecast<-forecast(tayt_basket_model,h=2)



tayt_submission_xreg1<-cbind(tayt[1:n,"visit_count"],
                             tayt[1:n,"is.discount_days"],
                             tayt[1:n,"favored_count"],
                             tayt[1:n,"basket_count"])
tayt_submission_xreg2<-data.table("visit_count"=tayt_visit_forecast$mean,
                                  "is.discount_days"=c(0,0),
                                  "favored_count"=tayt_favored_forecast$mean,
                                  "basket_count"=tayt_basket_forecast$mean)

tayt_submission_xts<-as.numeric(tayt_xts$sold_count)

tayt_arimasub<-Arima(tayt_submission_xts,
                                   xreg=as.matrix(tayt_submission_xreg1),
                                   order=c(4,1,1))

tayt_submission_forecast_arima<-forecast(tayt_arimasub,
                                         xreg=as.matrix(tayt_submission_xreg2))


tayt_lm_submission<-lm(sold_count~visit_count+is.discount_days+favored_count+basket_count,data=tayt)

pred_tayt_lm_submodel<-predict(tayt_lm_submission,tayt_submission_xreg2)

tayt_submission_arima<-0.5*(tayt_submission_forecast_arima$mean[2]+pred_tayt_lm_submodel[2])

tayt_submission_arima


earphone<-subset(alldata, alldata$product_content_id==6676673)

earphone

plot(dates,earphone$sold_count)

earphone_xts<-xts(earphone,order.by=dates)

earphone_train_xts<-earphone_xts[index(earphone_xts)<"2021-03-01"]

earphone_test_xts<-earphone_xts[index(earphone_xts)>="2021-03-01"]


earphone_xreg1<-cbind( earphone[1:280,"visit_count"],
                       earphone[1:280,"favored_count"],
                       earphone[1:280,"basket_count"])

earphone_xreg2<-cbind( earphone[281:n,"visit_count"],
                       earphone[281:n,"favored_count"],
                       earphone[281:n,"basket_count"])
auto.arima(as.numeric(earphone_train_xts$sold_count))

earphone_arima_model<-Arima(as.numeric(earphone_train_xts$sold_count),
                            xreg=as.matrix(earphone_xreg1),
                            order=c(3,1,1))
AIC(earphone_arima_model)

earphone_forecast_arima<-forecast(earphone_arima_model,xreg=as.matrix(earphone_xreg2))

earphone_forecast_arima_xts<-xts(earphone_forecast_arima$mean,order.by=testation_dates)


plot(dates,earphone$sold_count, type="l")
lines(testation_dates,earphone_forecast_arima_xts,col="blue")


earphone_ARIMA_MAPE<-100*mean(abs((as.numeric(earphone_forecast_arima_xts)-as.numeric(earphone_test_xts$sold_count))/as.numeric(earphone_test_xts$sold_count)))
earphone_ARIMA_MAPE


earphone_lm_train<-earphone[1:280,]
earphone_lm_test<-earphone[281:n,]


earphone_lm_model<-lm(sold_count~visit_count+favored_count+basket_count,data=earphone_lm_train)

pred_earphone_lm_model<-predict(earphone_lm_model,earphone_lm_test)


pred_earphone_lm_model_xts<-xts(pred_earphone_lm_model,order.by = testation_dates)

plot(pred_earphone_lm_model_xts, col="blue")
lines(earphone_test_xts$sold_count,col="black")
lines(earphone_forecast_arima_xts, col="red")

plot(dates,earphone$sold_count, type="l")
lines(testation_dates,pred_earphone_lm_model_xts,col="blue")

earphone_reg_MAPE<-100*mean(abs((pred_earphone_lm_model_xts-as.numeric(earphone_test_xts$sold_count))/as.numeric(earphone_test_xts$sold_count)))
earphone_reg_MAPE

earphone_visit_xreg<-as.numeric(earphone_xts$visit_count)
earphone_visit_model<-auto.arima(earphone_visit_xreg)
earphone_visit_forecast<-forecast(earphone_visit_model,h=2)

earphone_favored_xreg<-as.numeric(earphone_xts$favored_count)
earphone_favored_model<-auto.arima(earphone_favored_xreg)
earphone_favored_forecast<-forecast(earphone_favored_model,h=2)

earphone_basket_xreg<-as.numeric(earphone_xts$basket_count)
earphone_basket_model<-auto.arima(earphone_basket_xreg)
earphone_basket_forecast<-forecast(earphone_basket_model,h=2)


earphone_submission_xreg1<-cbind(earphone[1:n,"visit_count"],
                                 earphone[1:n,"favored_count"],
                                 earphone[1:n,"basket_count"])
earphone_submission_xreg2<-data.table("visit_count"=earphone_visit_forecast$mean,
                                      "favored_count"=earphone_favored_forecast$mean,
                                      "basket_count"=earphone_basket_forecast$mean)

earphone_submission_xts<-as.numeric(earphone_xts$sold_count)

earphone_arimasub<-Arima(earphone_submission_xts,
                                       xreg=as.matrix(earphone_submission_xreg1),
                                       order=c(3,1,1))

earphone_submission_forecast_arima<-forecast(earphone_arimasub,
                                             xreg=as.matrix(earphone_submission_xreg2))


earphone_lm_submission<-lm(sold_count~visit_count+favored_count+basket_count,data=earphone)

pred_earphone_lm_submodel<-predict(earphone_lm_submission,earphone_submission_xreg2)

earphone_submission_arima<-0.5*(earphone_submission_forecast_arima$mean[2]+pred_earphone_lm_submodel[2])

earphone_submission_arima


vacuum<-subset(alldata, alldata$product_content_id==7061886)

vacuum

plot(dates,vacuum$sold_count)

vacuum_xts<-xts(vacuum,order.by=dates)

vacuum_train_xts<-vacuum_xts[index(vacuum_xts)<"2021-03-01"]

vacuum_test_xts<-vacuum_xts[index(vacuum_xts)>="2021-03-01"]


vacuum_xreg1<-cbind( vacuum[1:280,"visit_count"],
                            vacuum[1:280,"favored_count"],
                            vacuum[1:280,"basket_count"])

vacuum_xreg2<-cbind( vacuum[281:n,"visit_count"],
                            vacuum[281:n,"favored_count"],
                            vacuum[281:n,"basket_count"])
auto.arima(as.numeric(vacuum_train_xts$sold_count))

vacuum_arima_model<-Arima(as.numeric(vacuum_train_xts$sold_count),
                                 xreg=as.matrix(vacuum_xreg1),
                                 order=c(1,0,3))
AIC(vacuum_arima_model)

vacuum_forecast_arima<-forecast(vacuum_arima_model,xreg=as.matrix(vacuum_xreg2))

vacuum_forecast_arima_xts<-xts(vacuum_forecast_arima$mean,order.by=testation_dates)


plot(dates,vacuum$sold_count, type="l")
lines(testation_dates,vacuum_forecast_arima_xts,col="blue")


vacuum_ARIMA_MAPE<-100*mean(abs((as.numeric(vacuum_forecast_arima_xts)-as.numeric(vacuum_test_xts$sold_count))/as.numeric(vacuum_test_xts$sold_count)))
vacuum_ARIMA_MAPE


vacuum_lm_train<-vacuum[1:280,]
vacuum_lm_test<-vacuum[281:n,]

vacuum_lm_model<-lm(sold_count~visit_count+favored_count+basket_count,data=vacuum_lm_train)
summary(vacuum_lm_model)

pred_vacuum_lm_model<-predict(vacuum_lm_model,vacuum_lm_test)

pred_vacuum_lm_model_xts<-xts(pred_vacuum_lm_model,order.by = testation_dates)

plot(pred_vacuum_lm_model_xts, col="blue")
lines(vacuum_test_xts$sold_count,col="black")
lines(vacuum_forecast_arima_xts, col="red")

plot(dates,vacuum$sold_count, type="l")
lines(testation_dates,pred_vacuum_lm_model_xts,col="blue")

vacuum_reg_MAPE<-100*mean(abs((pred_vacuum_lm_model_xts-as.numeric(vacuum_test_xts$sold_count))/as.numeric(vacuum_test_xts$sold_count)))
vacuum_reg_MAPE

vacuum_visit_xreg<-as.numeric(vacuum_xts$visit_count)
vacuum_visit_model<-auto.arima(vacuum_visit_xreg)
vacuum_visit_forecast<-forecast(vacuum_visit_model,h=2)

vacuum_favored_xreg<-as.numeric(vacuum_xts$favored_count)
vacuum_favored_model<-auto.arima(vacuum_favored_xreg)
vacuum_favored_forecast<-forecast(vacuum_favored_model,h=2)

vacuum_basket_xreg<-as.numeric(vacuum_xts$basket_count)
vacuum_basket_model<-auto.arima(vacuum_basket_xreg)
vacuum_basket_forecast<-forecast(vacuum_basket_model,h=2)


vacuum_submission_xreg1<-cbind(vacuum[1:n,"visit_count"],
                                      vacuum[1:n,"favored_count"],
                                      vacuum[1:n,"basket_count"])
vacuum_submission_xreg2<-data.table("visit_count"=vacuum_visit_forecast$mean,
                                           "favored_count"=vacuum_favored_forecast$mean,
                                           "basket_count"=vacuum_basket_forecast$mean)

vacuum_submission_xts<-as.numeric(vacuum_xts$sold_count)

vacuum_arimasub<-Arima(vacuum_submission_xts,
                                            xreg=as.matrix(vacuum_submission_xreg1),
                                            order=c(1,0,3))

vacuum_submission_forecast_arima<-forecast(vacuum_arimasub,
                                                  xreg=as.matrix(vacuum_submission_xreg2))


vacuum_lm_submission<-lm(sold_count~visit_count+favored_count+basket_count,data=vacuum)

pred_vacuum_lm_submodel<-predict(vacuum_lm_submission,vacuum_submission_xreg2)

vacuum_submission_arima<-0.5*(vacuum_submission_forecast_arima$mean[2]+pred_vacuum_lm_submodel[2])

vacuum_submission_arima


facialcleanser<-subset(alldata, alldata$product_content_id==85004)

facialcleanser

plot(dates,facialcleanser$sold_count)

facialcleanser_xts<-xts(facialcleanser,order.by=dates)

facialcleanser_train_xts<-facialcleanser_xts[index(facialcleanser_xts)<"2021-03-01"]

facialcleanser_test_xts<-facialcleanser_xts[index(facialcleanser_xts)>="2021-03-01"]


facialcleanser_xreg1<-cbind( facialcleanser[1:280,"visit_count"],
                             facialcleanser[1:280,"favored_count"],
                             facialcleanser[1:280,"basket_count"])

facialcleanser_xreg2<-cbind( facialcleanser[281:n,"visit_count"],
                             facialcleanser[281:n,"favored_count"],
                             facialcleanser[281:n,"basket_count"])
auto.arima(as.numeric(facialcleanser_train_xts$sold_count))

facialcleanser_arima_model<-Arima(as.numeric(facialcleanser_train_xts$sold_count),
                                  xreg=as.matrix(facialcleanser_xreg1),
                                  order=c(0,1,0))
AIC(facialcleanser_arima_model)

facialcleanser_forecast_arima<-forecast(facialcleanser_arima_model,xreg=as.matrix(facialcleanser_xreg2))

facialcleanser_forecast_arima_xts<-xts(facialcleanser_forecast_arima$mean,order.by=testation_dates)


plot(dates,facialcleanser$sold_count, type="l")
lines(testation_dates,facialcleanser_forecast_arima_xts,col="blue")


facialcleanser_ARIMA_MAPE<-100*mean(abs((as.numeric(facialcleanser_forecast_arima_xts)-as.numeric(facialcleanser_test_xts$sold_count))/as.numeric(facialcleanser_test_xts$sold_count)))
facialcleanser_ARIMA_MAPE


facialcleanser_lm_train<-facialcleanser[1:280,]
facialcleanser_lm_test<-facialcleanser[281:n,]

facialcleanser_lm_model<-lm(sold_count~visit_count+is.discount_days+favored_count+basket_count,data=facialcleanser_lm_train)

pred_facialcleanser_lm_model<-predict(facialcleanser_lm_model,facialcleanser_lm_test)

pred_facialcleanser_lm_model_xts<-xts(pred_facialcleanser_lm_model,order.by = testation_dates)

plot(pred_facialcleanser_lm_model_xts, col="blue")
lines(facialcleanser_test_xts$sold_count,col="black")
lines(facialcleanser_forecast_arima_xts, col="red")

plot(dates,facialcleanser$sold_count, type="l")
lines(testation_dates,pred_facialcleanser_lm_model_xts,col="blue")

facialcleanser_reg_MAPE<-100*mean(abs((pred_facialcleanser_lm_model_xts-as.numeric(facialcleanser_test_xts$sold_count))/as.numeric(facialcleanser_test_xts$sold_count)))
facialcleanser_reg_MAPE


facialcleanser_visit_xreg<-as.numeric(facialcleanser_xts$visit_count)
facialcleanser_visit_model<-auto.arima(facialcleanser_visit_xreg)
facialcleanser_visit_forecast<-forecast(facialcleanser_visit_model,h=2)

facialcleanser_favored_xreg<-as.numeric(facialcleanser_xts$favored_count)
facialcleanser_favored_model<-auto.arima(facialcleanser_favored_xreg)
facialcleanser_favored_forecast<-forecast(facialcleanser_favored_model,h=2)

facialcleanser_basket_xreg<-as.numeric(facialcleanser_xts$basket_count)
facialcleanser_basket_model<-auto.arima(facialcleanser_basket_xreg)
facialcleanser_basket_forecast<-forecast(facialcleanser_basket_model,h=2)



facialcleanser_submission_xreg1<-cbind(facialcleanser[1:n,"visit_count"],
                                       facialcleanser[1:n,"is.discount_days"],
                                       facialcleanser[1:n,"favored_count"],
                                       facialcleanser[1:n,"basket_count"])
facialcleanser_submission_xreg2<-data.table("visit_count"=facialcleanser_visit_forecast$mean,
                                            "is.discount_days"=c(0,0),
                                            "favored_count"=facialcleanser_favored_forecast$mean,
                                            "basket_count"=facialcleanser_basket_forecast$mean)

facialcleanser_submission_xts<-as.numeric(facialcleanser_xts$sold_count)

facialcleanser_arimasub<-Arima(facialcleanser_submission_xts,
                                             xreg=as.matrix(facialcleanser_submission_xreg1),
                                             order=c(0,1,0))

facialcleanser_submission_forecast_arima<-forecast(facialcleanser_arimasub,
                                                   xreg=as.matrix(facialcleanser_submission_xreg2))


facialcleanser_lm_submission<-lm(sold_count~visit_count+favored_count+basket_count,data=facialcleanser)

pred_facialcleanser_lm_submodel<-predict(facialcleanser_lm_submission,facialcleanser_submission_xreg2)

facialcleanser_submission_arima<-0.5*(facialcleanser_submission_forecast_arima$mean[2]+pred_facialcleanser_lm_submodel[2])

facialcleanser_submission_arima


babywipe<-subset(alldata, alldata$product_content_id==4066298)

babywipe


plot(dates,babywipe$sold_count)

babywipe_xts<-xts(babywipe,order.by=dates)

babywipe_train_xts<-babywipe_xts[index(babywipe_xts)<"2021-03-01"]

babywipe_test_xts<-babywipe_xts[index(babywipe_xts)>="2021-03-01"]


babywipe_xreg1<-cbind( babywipe[1:280,"favored_count"],
                      babywipe[1:280,"basket_count"])

babywipe_xreg2<-cbind( babywipe[281:n,"favored_count"],
                      babywipe[281:n,"basket_count"])
auto.arima(as.numeric(babywipe_train_xts$sold_count))

babywipe_arima_model<-Arima(as.numeric(babywipe_train_xts$sold_count),
                           xreg=as.matrix(babywipe_xreg1),
                           order=c(3,0,2))
AIC(babywipe_arima_model)

babywipe_forecast_arima<-forecast(babywipe_arima_model,xreg=as.matrix(babywipe_xreg2))

babywipe_forecast_arima_xts<-xts(babywipe_forecast_arima$mean,order.by=testation_dates)


plot(dates,babywipe$sold_count, type="l")
lines(testation_dates,babywipe_forecast_arima_xts,col="blue")


babywipe_ARIMA_MAPE<-100*mean(abs((as.numeric(babywipe_forecast_arima_xts)-as.numeric(babywipe_test_xts$sold_count))/as.numeric(babywipe_test_xts$sold_count)))
babywipe_ARIMA_MAPE

babywipe_lm_train<-babywipe[1:280,]
babywipe_lm_test<-babywipe[281:n,]

babywipe_lm_model<-lm(sold_count~price+ty_visits+favored_count+basket_count,data=babywipe_lm_train)

summary(babywipe_lm_model)

pred_babywipe_lm_model<-predict(babywipe_lm_model,babywipe_lm_test)

pred_babywipe_lm_model_xts<-xts(pred_babywipe_lm_model,order.by = testation_dates)

plot(pred_babywipe_lm_model_xts, col="blue")
lines(babywipe_test_xts$sold_count,col="black")
lines(babywipe_forecast_arima_xts, col="red")

plot(dates,babywipe$sold_count, type="l")
lines(testation_dates,pred_babywipe_lm_model_xts,col="blue")

babywipe_reg_MAPE<-100*mean(abs((pred_babywipe_lm_model_xts-as.numeric(babywipe_test_xts$sold_count))/as.numeric(babywipe_test_xts$sold_count)))
babywipe_reg_MAPE

babywipe_price_xreg<-as.numeric(babywipe_xts$price)
babywipe_price_model<-auto.arima(babywipe_price_xreg)
babywipe_price_forecast<-forecast(babywipe_price_model,h=2)

babywipe_favored_xreg<-as.numeric(babywipe_xts$favored_count)
babywipe_favored_model<-auto.arima(babywipe_favored_xreg)
babywipe_favored_forecast<-forecast(babywipe_favored_model,h=2)

babywipe_basket_xreg<-as.numeric(babywipe_xts$basket_count)
babywipe_basket_model<-auto.arima(babywipe_basket_xreg)
babywipe_basket_forecast<-forecast(babywipe_basket_model,h=2)


babywipe_submission_xreg1<-cbind(babywipe[1:n,"price"],
                                babywipe[1:n,"favored_count"],
                                babywipe[1:n,"basket_count"])
babywipe_submission_xreg2<-data.table("price"=babywipe_price_forecast$mean,
                                     "favored_count"=babywipe_favored_forecast$mean,
                                     "basket_count"=babywipe_basket_forecast$mean)

babywipe_submission_xts<-as.numeric(babywipe_xts$sold_count)

babywipe_arimasub<-Arima(babywipe_submission_xts,
                                      xreg=as.matrix(babywipe_submission_xreg1),
                                      order=c(2,0,1))

babywipe_submission_forecast_arima<-forecast(babywipe_arimasub,
                                            xreg=as.matrix(babywipe_submission_xreg2))


babywipe_lm_submission<-lm(sold_count~price+favored_count+basket_count,data=babywipe)

pred_babywipe_lm_submodel<-predict(babywipe_lm_submission,babywipe_submission_xreg2)

babywipe_submission_arima<-0.5*(babywipe_submission_forecast_arima$mean[2]+pred_babywipe_lm_submodel[2])

babywipe_submission_arima

submission_list<-list(mont_submission_arima,bikini1_submission_arima,bikini2_submission_arima,tayt_submission_arima,
                      earphone_submission_arima,vacuum_submission_arima,facialcleanser_submission_arima,babywipe_submission_arima,oralb_submission_arima)
submission_list


submission_productlist<-list(48740784,73318567,32737302,31515569,6676673,7061886,85004,4066298,32939029)


submission_df<-data.table("product_content_id"=submission_productlist, "forecast"=submission_list)

submission_df$forecast<-as.numeric(submission_df$forecast)

submission_df

send_submission(submission_df, token, url=subm_url, submit_now=T)

