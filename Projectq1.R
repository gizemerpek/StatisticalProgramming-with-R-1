data1<- read.table("C:/Users/ceren/OneDrive/Belgeler/istatikselprogramlama/DatasetNA.txt",header = TRUE)
my_function<-function(data, main_title = "", x_label = "", y_label = "", x_limits = NULL, y_limits = NULL, colors = NULL, horizontal = FALSE) {
  
  var_data <- factor(data)
  var_data <- var_data[!is.na(var_data)]
  x_axis_label <- x_label
  y_axis_label <- y_label
  
  if (horizontal) {
    plot(var_data, main = main_title, xlab = x_axis_label, ylab = y_axis_label, col = colors, horiz = TRUE, xlim = x_limits, ylim = y_limits)
  } else {
    plot(var_data, main = main_title, xlab = x_axis_label, ylab = y_axis_label, col = colors, xlim = x_limits, ylim = y_limits)
  }
}


#For seperate page

par(mfrow = c(1, 1))
my_function(data1$Group,main="Group Table",x_label ="Groups",y_label="Values",x_limits =NULL,y_limits =NULL,colors="red")
my_function(data1$Gender,main="Gender Table",x_label ="Genders", y_label="Values",x_limits =NULL,y_limits =NULL,colors="red") 

#For same page

par(mfcol=c(1,2))
my_function(data1$Group,main="Group Table",x_label ="Groups",y_label="Values",x_limits =NULL,y_limits =NULL,colors="red")
my_function(data1$Gender,main="Gender Table",x_label ="Genders", y_label="Values",x_limits =NULL,y_limits =NULL,colors="red") 
#For same page
par(mfrow = c(2, 1))
my_function(data1$Group,main="Group Table",x_label ="Groups",y_label="Values",x_limits =NULL,y_limits =NULL,colors="red")
my_function(data1$Gender,main="Gender Table",x_label ="Genders", y_label="Values",x_limits =NULL,y_limits =NULL,colors="red")

