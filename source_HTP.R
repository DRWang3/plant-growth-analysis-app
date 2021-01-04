
## definitions for growth traits

var.names = c("PlantAge",
"SideAverageCenterOfMassDistance",
"SideAverageCenterOfMassX",
"SideAverageCenterOfMassY",
"SideAverageConvexHull",
"SideAverageHeight",
"SideAverageHue",
"SideAverageRoundness",
"SideAverageSaturation",
'SideAverageSurface',
"SideAverageValue",
"TopAverageHue",
"TopAverageSaturation",
"TopAverageValue",
"TopCenterOfMassDistance",
"TopCenterOfMassX",
"TopCenterOfMassY",
"TopConvexHull",
"TopPlantSurface",
"TopRoundness",
"TopSurface")

descriptions= c("Age in days after sowing",
"Distance between center of mass (cyan circle in Figure 1) and center of pot (mm)",
"X-coordinate of center of mass in image (mm)",
"Y-coordinate of center of mass in image (mm)",
"Area defined by the blue outline (smallest length that can circumvent plant) (mm^2)",
"Defined from bottom of lime green box in Figure 1 (mm)",
"Hue in HSI color space; see color wheel 0 to 360",
"How round plant is in computer vision language; calculation based on ratio of grey area and a square occupying the same perimeter",
"Saturation in HSI color space; how much white is the color 0 to 100",
"Region occupied by grey pixels in Figure 1 (mm^2)",
"Intensity in HSI color space; values up to 256",
"Hue in HSI color space; see color wheel 0 to 360",
"Saturation in HSI color space; how much white is the color 0 to 100",
"Intensity in HSI color space; values up to 256",
"Distance between center of mass (cyan circle in Figure 2) and center of pot (mm)",
"X-coordinate of center of mass in image (mm)",
"Y-coordinate of center of mass in image (mm)",
"Area defined by the blue outline (smallest length that can circumvent plant) (mm^2)",
"Region occupied by grey pixels in Figure 2 (mm^2)",
"How round plant is in computer vision language; calculation based on ratio of grey area and a square occupying the same perimeter",
"Region occupied by grey pixels in Figure 2 (mm^2)")

defDict = cbind(var.names, descriptions)
defDict = as.data.frame(defDict)

names(defDict)=c("var.name", "description")

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

plot.fitted.log = function(K, N0, r, t, color, line.width){
  A = K/(1+(((K - N0)/N0) * exp(-r*(t))))
  points(t, A, col = color, type="l", lwd = line.width)
}

RGR_functional = function(K, N0, r, t){
  A= (K-N0)/N0
  f.prime= (K*A*r*exp(-r*t))/(1+A*exp(-r*t))^2
  f= K / (1 + ((K - N0) / N0) * exp(-r * t))
  return(f.prime/f)
}

