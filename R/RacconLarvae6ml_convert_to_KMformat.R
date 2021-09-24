library(readxl)
Formatfast6ml <- read_excel("C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RacoonOlyLarvalSurvival/Formatfast6ml.xlsx")

rows<-sum(Formatfast6ml$Total)
rows
#2075


#putting all dataset together
#total larvae repeated the number of times it displays
col1<-Formatfast6ml$Total
Total1<-rep(col1,col1)
Total1
length(Total1) 

col2<-Formatfast6ml$Date
Total2<-rep(col2,col1)
Total2
length(Total2) 

col3<-Formatfast6ml$Number
Total3<-rep(col3,col1)
Total3
length(Total3) 

col4<-Formatfast6ml$Jar
Total4<-rep(col4,col1)
Total4

col5<-Formatfast6ml$Treatment
Total5<-rep(col5,col1)
Total5

col6<-Formatfast6ml$Loctaion
Total6<-rep(col6,col1)
Total6

col7<-Formatfast6ml$day
Total7<-rep(col7,col1)
Total7

df1<-data.frame(Total=Total1, Date=Total2, Number=Total3, Jar=Total4, Treatment=Total5, Location=Total6, day=Total7)
View(df1)

write.csv(df1, "C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RacoonOlyLarvalSurvival/df1.csv")


#create and repeat number `dead`alive with "1"
colAlive1=Formatfast6ml$Alive
colAlive1
Alive1<-rep(colAlive1,colAlive1)
Alive1
length(Alive1)
Alive1<-replace(Alive1, 1:1128, 1)

#number matched to the alive
colAlive2=Formatfast6ml$Number
colAlive2
Alive2<-rep(colAlive2,colAlive1)
Alive2
length(Alive2)



df2<-data.frame(alive=Alive1,Number=Alive2)
df2
write.csv(df2, "C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RacoonOlyLarvalSurvival/df2.csv")


#create and repeat number dead with "1"
colDead1=Formatfast6ml$Dead
colDead1
Dead1<-rep(colDead1,colDead1)
Dead1<-replace(Dead1, 1:947, 0)
Dead1

#number matched to the dead
colDead2=Formatfast6ml$Number
colDead2
Dead2<-rep(colDead2,colDead1)
Dead2
length(Dead2)

df3<-data.frame(dead=Dead1,Number=Dead2)

write.csv(df3, "C:/Users/Lindsay/Dropbox/Raccoon/larvae/oyster/larvae survival stats/GitHub/RacoonOlyLarvalSurvival/df3.csv")


#both add up to 2075, which is total larvae counted for CI
Ldead<-length(Dead1)
Lalive<-length(Alive1)
Ldead+Lalive
length(Total1)



df <- data.frame(Total=Total1 ,dead=c(Alive1, Dead1), day=row1$day, Treatment=row1$Treatment,
                 Jar=row1$Jar, Location=row1$Loctaion)
View(df)
