



















































# Load the lavaan package
install.packages("lavaan")
library(lavaan)
# Load the built-in dataset HolzingerSwineford1939
data("HolzingerSwineford1939")
# Define the CFA model
model = 'visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9
x1 ~~ x4
x2 ~~ x5
x3 ~~ x6
x4 ~~ x7
x5 ~~ x8
x6 ~~ x9'
# Fit the CFA model to the data
fit <- cfa(model, data = HolzingerSwineford1939)
# Summarize the results
summary(fit)
# Visualize the results (optional)
inspect(fit, "std.lv")
install.packages("semPlot")
library(semPlot)
# Plot the standardized factor loadings
semPaths(fit, "std", layout = "tree2")
semPaths(fit,whatLabels="est",style="lisrel",main="cfa diagram")
