# A completely randomized experiment: Formulation of a Portland cement mortar
#   Problem: Does addition of a polymer latex emulsion during mixing affect tension bond
#            strength of Portland cement mortar?
#   We have one factor (mortar formulation) with two levels (treatments):
#     A: modified mortar (latex added)
#     B: unmodified mortar (no latex added)
#     n1 = 10 batches of modified mortar are prepared and tested
#     n2 = 10 batches of unmodified mortar are prepared and tested
#     Experimental units: batches of mortar
#     Randomization: Randomly arrange the order in which the 20 batches are tested.
#                    The order is completely randomized.
#     Observed response: measured tension bond strength (kilogram-force/centimeter2)

## Data input
### Download the dataset file: mortar.txt from the Week 3 instructions page on Canvas
## Set the working directory to the data file location
mortar = read.table(file = "mortar.txt",header = TRUE)

####################
## Informal analysis
####################
### Dotplot
dotchart(as.matrix(mortar))

### Histogram
par(mfrow=c(2,1))
hist(mortar$Modified.Mortar,probability = T)
hist(mortar$Unmodified.Mortar,probability = T)

### Boxplot
par(mfrow=c(1,1))
boxplot(x = mortar)

### Summary statistics
mean(mortar$Modified.Mortar)
sd(mortar$Modified.Mortar)
var(mortar$Modified.Mortar)

##################
## Formal analysis
##################
### Hypothesis testing
## mu1: Modified mortar group mean; mu1: Unmodified mortar group mean
## whether there is a difference between the two treatment means: 
## Ha: mu1 doesn't equal mu2
t.test(x = mortar$Modified.Mortar, y = mortar$Unmodified.Mortar, 
       alternative = "two.sided", var.equal = T, conf.level = 0.95)

## whether the modified mortar group has a lower mean compared to the unmodified mortar group
## Ha: mu1 < mu2
t.test(x = mortar$Modified.Mortar, y = mortar$Unmodified.Mortar, 
       alternative = "less", var.equal = T)

### 99% Confidence interval
t.test(x = mortar$Modified.Mortar, y = mortar$Unmodified.Mortar, 
       alternative = "two.sided", var.equal = T, conf.level = 0.99)

