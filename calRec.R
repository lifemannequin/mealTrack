dat=read.csv("food_composition.csv",header=TRUE)
meal=read.csv("recipe.csv")

indeces<-numeric(length(meal[,1]))

for (i in 1:length(meal[,1])){

indeces[i]=match(meal[i,1],dat[,1])

}

macros=dat[indeces,2:5]
macros=macros*(meal[,2]/100.0)

meal2<-cbind(meal,macros)
print(meal2)
meal3<-c(sum(meal2$protein),sum(meal2$fat),sum(meal2$carbohydrates),sum(meal2$calories))
print(meal3/5.39)

