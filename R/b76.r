#' @import ggplot2
#' @import quantmod

#' @export
plotImpliedVol <- function(df, date, mode="B") {

  results = getImpliedVol(df, date, mode)
  title = "Implied Volatility of Put & Call options against Strike"
  if(mode == "P") {
    results = results[which(results$type == "P"),]
    title = "Implied Volatility of Put option against Strike"
  } else if(mode == "C") {
    results = results[which(results$type == "C"),]
    title = "Implied Volatility of Call option against Strike"
  }

  if(mode != "CP") {
    ggplot(data=results, aes(x=strike,y=implied_volatility,colour=time_to_expiry,group=time_to_expiry)) +
      geom_line() +
      geom_point() +
      labs(title = title, x = "Strike", y = "Implied Volatility") +
      theme(legend.position="right",
            legend.key.size=unit(1.2, "cm"),
            text = element_text(color = "#444444"),
            panel.background = element_rect(fill = '#444B5A'),
            panel.grid.minor = element_line(color = '#586174'),
            panel.grid.major = element_line(color = '#586174'),
            plot.title = element_text(size = 12),
            axis.title = element_text(size = 14, color = '#555555')) +
      scale_colour_gradient(name="Time To Expiry",low="red",high="white")
  } else {
    title = "Implied Volatility of Call vs Put option against Strike"
    ggplot(data=results, aes(x=strike,y=implied_volatility,colour=type)) +
      geom_line() +
      geom_point() +
      labs(title = title, x = "Strike", y = "Implied Volatility") +
      theme(legend.position="right",
            legend.key.size=unit(1.2, "cm"),
            text = element_text(color = "#444444"),
            panel.background = element_rect(fill = '#444B5A'),
            panel.grid.minor = element_line(color = '#586174'),
            panel.grid.major = element_line(color = '#586174'),
            plot.title = element_text(size = 12),
            axis.title = element_text(size = 14, color = '#555555'))
  }
}

#' @export
getImpliedVol <- function(df, date, mode="B"){
  options("getSymbols.warning4.0"=FALSE)

  list_of_v = vector(length=nrow(df))
  list_of_strike = vector(length=nrow(df))
  list_of_type = vector(length=nrow(df))
  list_of_time_to_expiry = vector(length=nrow(df))
  list_of_optionPrice = vector(length=nrow(df))
  list_of_futurePrice = vector(length=nrow(df))

  # risk free rate based on specified date of 3month US T-bill
  getSymbols("DGS3MO", src = "FRED")
  r = as.data.frame(DGS3MO)[date, 1] / 100

  # for call option
  y_call <- function(v,fp,x,r,t,c) {
    d1 = (log(fp/x) + ((v**2)/2)*t) / (v*(t**0.5))
    d2 = d1 - v*(t**0.5)
    return (exp(-r*t) * (fp*pnorm(d1,0,1) - x*pnorm(d2,0,1)) - c)
  }

  # for put option
  y_put <- function(v,fp,x,r,t,p) {
    d1 = (log(fp/x) + ((v**2)/2)*t) / (v*(t**0.5))
    d2 = d1 - v*(t**0.5)
    return (exp(-r*t) * (x*pnorm(-d2,0,1) - fp*pnorm(-d1,0,1)) - p)
  }

  # get volatility v for each option
  for(i in 1:nrow(df)) {
    row = df[i,]
    fp = row$futurePrice
    x = row$strike
    t = row$time_to_expiry
    type = as.character(row$type)

    if(type == "C") {
      c = row$optionPrice
      implied_vol = uniroot(f=y_call,interval=c(0, 1),tol=0.0001,fp=fp,x=x,r=r,t=t,c=c)$root
      list_of_optionPrice[i] = c
    } else {
      p = row$optionPrice
      implied_vol = uniroot(f=y_put,interval=c(0, 1),tol=0.0001,fp=fp,x=x,r=r,t=t,p=p)$root
      list_of_optionPrice[i] = p
    }

    # return volatility and strike
    list_of_v[i] = round(implied_vol, 3)
    list_of_strike[i] = x
    list_of_type[i] = type
    list_of_time_to_expiry[i] = round(t, 3)
    list_of_futurePrice[i] = round(fp, 3)
  }

  results = data.frame(strike=list_of_strike, type=list_of_type, optionPrice=list_of_optionPrice, futurePrice=list_of_futurePrice, time_to_expiry=list_of_time_to_expiry, implied_volatility=list_of_v)

  if(mode == "P") {
    return (results[which(results$type == "P"),])
  } else if(mode == "C") {
    return (results[which(results$type == "C"),])
  }
  return (results)
}
