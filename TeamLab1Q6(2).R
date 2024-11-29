###Finds the File and converts it to a dataframe
d <- read.table('window_breakage_data.csv',sep = ',',header = T)
# Converts the data to type data.frame
d <- as.data.frame(d)
head(d)

### (c) Identify where and how many records are missing for each variable
colSums(is.na(d))
lapply(d, function(x) which(is.na(x)))

### (d) Create a 9x9 window of 8 histograms using par(mfrow=) discussed in class
### Should be for window size, glass thickness, window color, ambient temp, cut speed
### Edge deletion rate, spacer distance, silicon viscosity

# Set the drawing window to 9x9, showing 8 histograms
par(mfrow = c(3, 3))
d <- data.frame(d)
# Plot a histogram of each numerical feature
hist(d$Window.Size, main = "Window Size", xlab = "Window Size", col = "lightblue")
hist(d$Glass.thickness, main = "Glass Thickness", xlab = "Glass Thickness", col = "lightblue")
hist(d$Window.color, main = "Window Color", xlab = "Window Color", col = "lightblue")
hist(d$`Ambient.Temp`, main = "Ambient Temp", xlab = "Ambient Temp", col = "lightblue")
hist(d$`Cut.speed`, main = "Cut Speed", xlab = "Cut Speed", col = "lightblue")
hist(d$`Edge.Deletion.rate`, main = "Edge Deletion Rate", xlab = "Edge Deletion Rate", col = "lightblue")
hist(d$`Spacer.Distance`, main = "Spacer Distance", xlab = "Spacer Distance", col = "lightblue")
hist(d$`Silicon.Viscosity`, main = "Silicon Viscosity", xlab = "Silicon Viscosity", col = "lightblue")
par(mfrow = c(1, 1))


### (e) Create a correlation matrix of the variables in part (d)
dd <- d[,c('Window.Size', 'Glass.thickness', 'Window.color', 'Ambient.Temp', 'Cut.speed', 
           'Edge.Deletion.rate', 'Spacer.Distance', 'Silicon.Viscosity')]
cor_matrix <- cor(dd, use = "complete.obs")
print(cor_matrix)

### (f) Discuss what you observe from d and e. Which input variables do you think
### "Drive" window breakage?

### (g) Create scatter plots of the target variable breakage rate vs these numeric features:
### window size, glass thickness, window color, ambient temp, cut speed, edge deletion rate
### spacer distance, silicon viscosity (you may or may not use par(mfrow=))

# Set the drawing window layout to 2x4
par(mfrow = c(2, 4))

plot(d$`Window.Size`, d$`Breakage.Rate`, main = "Breakage Rate vs Window Size", xlab = "Window Size", ylab = "Breakage Rate", col = "blue", pch = 19)
plot(d$`Glass.thickness`, d$`Breakage.Rate`, main = "Breakage Rate vs Glass Thickness", xlab = "Glass Thickness", ylab = "Breakage Rate", col = "blue", pch = 19)
plot(d$`Window.color`, d$`Breakage.Rate`, main = "Breakage Rate vs Window Color", xlab = "Window Color", ylab = "Breakage Rate", col = "blue", pch = 19)
plot(d$`Ambient.Temp`, d$`Breakage.Rate`, main = "Breakage Rate vs Ambient Temp", xlab = "Ambient Temp", ylab = "Breakage Rate", col = "blue", pch = 19)
plot(d$`Cut.speed`, d$`Breakage.Rate`, main = "Breakage Rate vs Cut Speed", xlab = "Cut Speed", ylab = "Breakage Rate", col = "blue", pch = 19)
plot(d$`Edge.Deletion.rate`, d$`Breakage.Rate`, main = "Breakage Rate vs Edge Deletion Rate", xlab = "Edge Deletion Rate", ylab = "Breakage Rate", col = "blue", pch = 19)
plot(d$`Spacer.Distance`, d$`Breakage.Rate`, main = "Breakage Rate vs Spacer Distance", xlab = "Spacer Distance", ylab = "Breakage Rate", col = "blue", pch = 19)
plot(d$`Silicon.Viscosity`, d$`Breakage.Rate`, main = "Breakage Rate vs Silicon Viscosity", xlab = "Silicon Viscosity", ylab = "Breakage Rate", col = "blue", pch = 19)

par(mfrow = c(1, 1))


### (h) Create cross-tabulation tables of the target variable pass/fail vs
### window type, glass supplier and glass supplier location

# Create crosstabs for Pass/Fail and Window Type
table(d$Pass.Fail, d$`Window.Type`)


# Create crosstabs for Pass/Fail and Glass Supplier
table(d$Pass.Fail, d$`Glass.Supplier`)

# Create a crosstab for Pass/Fail and Glass Supplier Location
table(d$Pass.Fail, d$`Glass.Supplier.Location`)



### (i) Create normalized views of the target variable pass/fail vs:
### window size, glass thickness, window color, ambient temp, cut speed, edge deletion rate
### and spacer. What do these graphics suggest?

# Create a standardized boxplot
par(mfrow = c(2, 4))

boxplot(d$`Window.Size` ~ d$Pass.Fail, main = "Window Size", xlab = "Pass/Fail", ylab = "Window Size", col = "lightblue")
boxplot(d$`Glass.thickness` ~ d$Pass.Fail, main = "Glass Thickness", xlab = "Pass/Fail", ylab = "Glass Thickness", col = "lightblue")
boxplot(d$`Window.color` ~ d$Pass.Fail, main = "Window Color", xlab = "Pass/Fail", ylab = "Window Color", col = "lightblue")
boxplot(d$`Ambient.Temp` ~ d$Pass.Fail, main = "Ambient Temp", xlab = "Pass/Fail", ylab = "Ambient Temp", col = "lightblue")
boxplot(d$`Cut.speed` ~ d$Pass.Fail, main = "Cut Speed", xlab = "Pass/Fail", ylab = "Cut Speed", col = "lightblue")
boxplot(d$`Edge.Deletion.rate` ~ d$Pass.Fail, main = "Edge Deletion Rate", xlab = "Pass/Fail", ylab = "Edge Deletion Rate", col = "lightblue")
boxplot(d$`Spacer.Distance` ~ d$Pass.Fail, main = "Spacer Distance", xlab = "Pass/Fail", ylab = "Spacer Distance", col = "lightblue")
boxplot(d$`Silicon.Viscosity` ~ d$Pass.Fail, main = "Silicon Viscosity", xlab = "Pass/Fail", ylab = "Silicon Viscosity", col = "lightblue")

par(mfrow = c(1, 1))


