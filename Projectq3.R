# Custom boxplot function without using boxplot function
my_boxplot <- function(data, main_title = "Boxplot", x_label = "X Axis", y_label = "Y Axis", colors = NULL) {
  
  par(mar = c(3, 3, 2, 2) + 0.1)
  
  #cleaning na values and calculating quarties and outliers
  cleaned_data <- na.omit(data)
  quartiles <- quantile(cleaned_data, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  outliers <- cleaned_data[cleaned_data < quantile(cleaned_data, 0.25, na.rm = TRUE) - 1.5 * IQR(cleaned_data, na.rm = TRUE)] | cleaned_data[cleaned_data > quantile(cleaned_data, 0.75, na.rm = TRUE) + 1.5 * IQR(cleaned_data, na.rm = TRUE)]
  
  # Creating the boxplot
  plot(1, type = "n", main = main_title, xlab = x_label, ylab = y_label, ylim = range(data, na.rm = TRUE))
  
  # Draw box
  polygon(c(0.8, 1.2, 1.2, 0.8), c(quartiles[1], quartiles[1], quartiles[3], quartiles[3]), border = "black", col = colors)
  
  # Draw median line
  abline(h = quartiles[2], col = "red")
  
  # Draw whiskers
  segments(1, quantile(data, 0.25, na.rm = TRUE), 1, min(data[data > quantile(data, 0.25, na.rm = TRUE) - 1.5 * IQR(data, na.rm = TRUE)], na.rm = TRUE))
  segments(1, quantile(data, 0.75, na.rm = TRUE), 1, max(data[data < quantile(data, 0.75, na.rm = TRUE) + 1.5 * IQR(data, na.rm = TRUE)], na.rm = TRUE))
  
  # Draw outliers
  points(rep(1, length(outliers)), outliers, pch = 19, col = "red")
  
  # Add legend
  legend("topright", legend = c("Box", "Median", "Whiskers", "Outliers"), fill = c("lightblue", "red", "black", "red"))
}

data1 <- read.table("C:/Users/ceren/OneDrive/Belgeler/istatikselprogramlama/DatasetNA.txt", header = TRUE)

# variables boxplots
par(mfrow = c(1, 1))  # Set layout for multiple plots

my_boxplot(data1$Var1,  main_title = "Boxplot for Var1", x_label = "Variable 1", y_label = "Values", colors = "lightblue")
my_boxplot(data1$Var2,  main_title = "Boxplot for Var2", x_label = "Variable 2", y_label = "Values", colors = "lightblue")
my_boxplot(data1$Var3,  main_title = "Boxplot for Var3", x_label = "Variable 3", y_label = "Values", colors = "lightblue")
my_boxplot(data1$Var4,  main_title = "Boxplot for Var4", x_label = "Variable 4", y_label = "Values", colors = "lightblue")
my_boxplot(data1$Var5,  main_title = "Boxplot for Var5", x_label = "Variable 5", y_label = "Values", colors = "lightblue")
my_boxplot(data1$Var6,  main_title = "Boxplot for Var6", x_label = "Variable 6", y_label = "Values", colors = "lightblue")
my_boxplot(data1$Var7,  main_title = "Boxplot for Var7", x_label = "Variable 7", y_label = "Values", colors = "lightblue")
my_boxplot(data1$Var8,  main_title = "Boxplot for Var8", x_label = "Variable 8", y_label = "Values", colors = "lightblue")
 
# c. more than one boxplot could be plotted together
par(mfcol=c(1,2))
my_boxplot(data1$Var3,main_title = "Var3", x_label = "Variables", y_label = "values",colors = "lightpink")   
my_boxplot(data1$Var5,main_title = "Var5", x_label = "Variables", y_label = "values",colors = "lightgreen")


# d. boxplot of variable 1 based on gender
data1$Gender <- as.factor(data1$Gender)

par(mfrow = c(1, 1))

my_boxplot(data1$Var1, 
           main_title = "boxplot of V1 based on Gender",
           x_label = "Gender",
           y_label = "Var1 Values",
           colors = "lightpink")


