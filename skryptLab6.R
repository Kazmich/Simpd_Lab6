library("neuralnet")

#Going to create a neural network to perform prediction
#Type ?neuralnet for more information on the neuralnet library

#Generate training data
#And store them as a dataframe
traininginput <- as.data.frame(matrix(c(7 , 1000 ,
                                        6 , 1300 ,
                                        5 , 1000 ,
                                        6 , 1200 ,
                                        7 , 1200 ,
                                        6 , 1000 ,
                                        9 , 1400 ,
                                        7 , 1200 ,
                                        6 , 1200 ,
                                        6.5 , 1200 ,), nrow=10, ncol=2))
trainingoutput <- c(1199, 1399, 799, 1599, 1249, 1249, 3199, 1899, 1079, 1599)

#Column bind the data into one variable
trainingdata <- cbind(traininginput, trainingoutput)

# Create Vector of Column Max and Min Values
maxs <- apply(trainingdata[,], 2, max)
mins <- apply(trainingdata[,], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled.trainingdata <- as.data.frame(scale(trainingdata[,], center=mins, scale=maxs-mins))
trainingdata <- scaled.trainingdata

# Check out results
print(head(trainingdata, 10))

colnames(trainingdata) <- c("capacity", "speed", "Price") 
print(trainingdata)

#Train the neural network
#Going to have C(6, 5, 3) hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net.price <- neuralnet(Price~wyswietlacz+pojemnosc+RAM, trainingdata, hidden=c(6, 5, 3), threshold=0.001)
print(net.price)

#Plot the neural network
plot(net.price)

#Test the neural network on some training data
testdata <- as.data.frame(matrix(c(7, 1000,
                                   5, 900,
                                   6, 1200), nrow=3, ncol=2))
scaled.testdata <- as.data.frame(scale(testdata[,], center=mins[1:2], scale=maxs[1:2]-mins[1:2]))
net.results <- compute(net.price, scaled.testdata) #Run them through the neural network

#Lets see what properties net.price has
ls(net.results)

#Lets see the results
print(net.results$net.result)
