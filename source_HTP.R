## functions

RGR_interval = function(w1, w2, t1, t2){
  if(t2 < t1){  # if user selected t2 smaller than t1, then swap the data
    tmp = t2
    t2 = t1
    t1 = tmp
    
    tmp = w2
    w2 = w1
    w1 = tmp
  }
  
  res = (log(w2) - log(w1))/(t2-t1)
  return(res)
}

D_integral = function(w1, w2, t1, t2){
  if(t2 < t1){  # if user selected t2 smaller than t1, then swap the data
    tmp = t2
    t2 = t1
    t1 = tmp
    
    tmp = w2
    w2 = w1
    w1 = tmp
  }
  
  res = (t2-t1)*(w2+w1)/2
  return(res)
}

RGR_integral = function(w1, w2, d){
  res = (w2-w1)/d
  return(res)
}

plot.fitted.log = function(K, N0, r, t, color){
  A = K/(1+(((K - N0)/N0) * exp(-r*(t))))
  points(t, A, col = color, type="l", lwd = 2)
}

RGR_functional = function(K, N0, r, t){
  A= (K-N0)/N0
  f.prime= (K*A*r*exp(-r*t))/(1+A*exp(-r*t))^2
  f= K / (1 + ((K - N0) / N0) * exp(-r * t))
  return(f.prime/f)
}

