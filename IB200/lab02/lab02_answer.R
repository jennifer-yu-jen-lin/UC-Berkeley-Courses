####################
##     LAB-02     ##
##  Jennifer Lin  ##
####################

# set the working directory
setwd("~/Desktop/IB200/20200129_Lab2")

# import packages
## library(ape) # Error: library ape after installing it
install.packages("ape")
library(ape)

# import the tree 
trees <- read.nexus("Amblygnathus.nex")
class(trees) # class: more information about the object
ls(trees) # ls: what the object contains

# plot the tree
## plot(trees) # Error: Notice that this goes through and plots all of the trees. For now, focus on just the first tree:
plot(trees[1])
?plot.phylo # ?: get more information on the command
            # shows you what additional arguments the function
plot(trees[1], font = 2, edge.lty = 4,
     edge.width = 10, tip.color = c("red", "green"))

# make my own ugly (or pretty) tree and save it as a PDF file
pdf("ugly r tree.pdf")
plot(trees[1], font = 2, edge.lty = 4,
     edge.width = 2, tip.color = c("blue", "green"))
dev.off()