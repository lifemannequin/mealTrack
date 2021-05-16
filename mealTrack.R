dat=read.csv("food_composition.csv",header=TRUE)
meal=read.csv("meal.csv")
mealdiary<-matrix(,nrow=0,ncol=4)
colnames(mealdiary)=c("protein","fat","carbs","cal")
day<-matrix(,nrow=0,ncol=4)
d=as.numeric(levels(factor(meal[,1])))
dind<-numeric(3*length(d))
indeces<-numeric(length(meal[,1]))

for (i in 1:length(meal[,1])){

indeces[i]=match(meal[i,3],dat[,1])

}

macros=dat[indeces,2:5]
macros=macros*(meal[,4]/100.0)

meal2<-cbind(meal,macros)

#print(meal2)
mealcal<-function(meal,protein,fat,carb,cal,m){
a<-ifelse(meal==m,1,0)
c(as.numeric(sum(protein*a)),as.numeric(sum(fat*a)),as.numeric(sum(carb*a)),as.numeric(sum(cal*a)))
}

mtype<-c("b","l","d","br")
m=1
for(j in 1:length(d)){
dind=which(meal[,1]==d[j])

for(i in 1:length(mtype)){

a=mealcal(meal2$meal[dind],meal2$protein[dind],meal2$fat[dind],meal2$carbohydrates[dind],meal2$calories[dind],mtype[i])

mealdiary<-rbind(mealdiary,a)



}

days=c(sum(mealdiary[,1]),sum(mealdiary[,2]),sum(mealdiary[,3]),sum(mealdiary[,4]))

day<-rbind(day,days)
}
dateS=d
print(mealdiary)
day[2,]=day[2,]-day[1,]
for (i in 3:length(day[,1])){
remain<-colSums(day[1:(i-1),])
day[i,]=day[i,]-remain

}
day<-cbind(d,day)
mealdiary2<-cbind(dateS,mealdiary)
colnames(day)<-c("date","Protein","Fat","Carbs","Calories")
print(mealdiary2)

write.csv(meal2,file="MealDiary.csv",row.names = FALSE)
write.csv(day,file="MealDdaily.csv",row.names = FALSE)
print('done')


