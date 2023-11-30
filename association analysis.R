library(arules)
library(arulesViz)

# Load data as transaction
myData <- read.transactions("/Users/rhaeagrxing/Downloads/Data_File_Association.csv", format='basket', sep=',')
inspect(myData[1:5])  # View the first 5 transactions

# Generate and plot item frequencies
itemFreq <- itemFrequency(myData)  # Calculate item appearance rate
itemFrequencyPlot(myData, topN=20) # Plot top 20 items

# Summary of transactions
summary(myData)  # Get a sense of our transaction data

# Generating rules with Apriori algorithm
rules <- apriori(myData, parameter=list(minlen=2, supp=0.02, conf=0.3))
srules <- sort(rules, by='lift', decreasing=TRUE)
inspect(srules[1:10])  # Inspect top 10 rules

# More detailed summary of rules
summary(srules)

# Visualize rules using various methods
plot(srules, method='graph', control=list(type='items'))
plot(srules, method='scatterplot', measure=c('support', 'confidence'), shading='lift')
plot(srules, method='grouped', control=list(type='items'))

# Explore the distribution of support, confidence and lift
quality(srules) %>%
  as.data.frame() %>%
  ggplot(aes(x=support, y=confidence, color=lift)) +
  geom_point() + theme_minimal() + ggtitle("Rule Quality Scatterplot")

# Reducing the number of rules for more insights
reducedRules <- head(SORT(srules, by='lift'), 20)  # Focus on top 20 rules
inspect(reducedRules)

# Saving the rules
write(srules, file="/Users/rhaeagrxing/Downloads/Data_File_Association_Update.csv", sep=',', quote=FALSE)

