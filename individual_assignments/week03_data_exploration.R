#load the here() package to help find files easily
require(here)
here()
#read in and make a dataframe of the habitat data
dat_habitat <- data.frame(read.csv(here("data/hab.sta.csv")))
#check it out
str(dat_habitat)

#make some histograms of terrain variables
hist(dat_habitat$elev,
     main = "Histogram of Elevation",
     xlab = "Elevation")
hist(dat_habitat$slope,
     main = "Histogram of Slope",
     xlab = "Slope")
hist(dat_habitat$aspect,
     main = "Histogram of Aspect",
     xlab = "Aspect")

#make some scatterplots of these variables
#plot basal area on the y axis 
plot(dat_habitat$elev, dat_habitat$ba.tot,
     main = "Elevation v Basal Area",
     xlab = "Elevation",
     ylab = "Basal Area")
plot(dat_habitat$slope, dat_habitat$ba.tot,
     main = "Slope v Basal Area",
     xlab = "Slope",
     ylab = "Basal Area")
plot(dat_habitat$aspect, dat_habitat$ba.tot,
     main = "Aspect v Basal Area",
     xlab = "Aspect",
     ylab = "Basal Area")

#add some linear functions to the scatterplot code
#multi step proccess

#this code finds the x value for a linear function
#when you give it a known (x1,y1) coordinate and the slope
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

#find the data centerpoint (repeat for each plot)
#so you can give the line_point_slope function a coordinate
data_center_x = mean(dat_habitat$elev)
data_center_y = mean(dat_habitat$ba.tot)
c(data_center_x, data_center_y)

#make the plot
plot(dat_habitat$elev, dat_habitat$ba.tot,
     main = "Elevation v Basal Area",
     xlab = "Elevation",
     ylab = "Basal Area")
#add the line; mess around with what slope to use
#to visually estimate a good fit
curve(line_point_slope(x, 
                  data_center_x, 
                  data_center_y,
                  -0.05), 
        add = TRUE,
        col = "light blue")

#next centerpoint
data_center_x = mean(dat_habitat$slope)
data_center_y = mean(dat_habitat$ba.tot)
c(data_center_x, data_center_y)
#next plot
plot(dat_habitat$slope, dat_habitat$ba.tot,
     main = "Slope v Basal Area",
     xlab = "Slope",
     ylab = "Basal Area")
curve(line_point_slope(x, 
                  data_center_x, 
                  data_center_y,
                  0), 
        add = TRUE,
        col = "light pink")

#last centerpoint
data_center_x = mean(dat_habitat$aspect)
data_center_y = mean(dat_habitat$ba.tot)
c(data_center_x, data_center_y)
#last plot
plot(dat_habitat$aspect, dat_habitat$ba.tot,
     main = "Aspect v Basal Area",
     xlab = "Aspect",
     ylab = "Basal Area")
curve(line_point_slope(x, 
                  data_center_x, 
                  data_center_y,
                  0.02), 
        add = TRUE,
        col = "yellow")



