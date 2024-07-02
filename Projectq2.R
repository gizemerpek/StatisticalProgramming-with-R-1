data1<- read.table("C:/Users/ceren/OneDrive/Belgeler/istatikselprogramlama/DatasetNA.txt",header = TRUE)
my_function<-function(data,vars, main_title = "", x_label = "", y_label = "", x_limit = NULL, y_limit = NULL, colors = NULL,bins=NULL) {
  
  x_axis_label <- x_label
  y_axis_label <- y_label
  
  
  for(var in vars){ 
    var_data <-data[[var]]
    var_data <- as.numeric(gsub(",",".",var_data)) 
    var_data <- var_data[!is.na(var_data)]
  }
  
  
  var_table=cut(var_data, bins)                                                     
  freq_table=transform(table(var_table))                                  
  transform(freq_table, Rel_Freq=prop.table(Freq),  Cum_Freq=cumsum(Freq))
  
  plot(freq_table, main = main_title, xlab = x_axis_label, ylab = y_axis_label, col = colors, xlim =x_limit, ylim = y_limit,bins=bins,colors=colors,lwd=2)
  
  
}
#For seperate page
par(mfrow = c(1, 1))
my_function(data1,"Var1",main_title = "Histogram for Var 1", x_label = "variable", y_label = "value",bins=4)   
my_function(data1,"Var2",main_title = "Histogram for Var 2", x_label = "variable", y_label = "value",bins=4)


#For same page

par(mfcol=c(1,2))
my_function(data1,"Var1",main_title = "Histogram for Var 1", x_label = "variable", y_label = "value",bins=4)   
my_function(data1,"Var2",main_title = "Histogram for Var 2", x_label = "variable", y_label = "value",bins=4)

