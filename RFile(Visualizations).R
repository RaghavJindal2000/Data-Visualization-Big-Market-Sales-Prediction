install.packages("caret")
install.packages("corrplot")
#install.packages("xgboost")
#install.packages("cowplot")
install.packages("magritrr")
install.packages("ggplot2")
library(data.table) # used for reading and manipulation of data 
library(dplyr) # used for data manipulation and joining 
library(ggplot2) # used for plotting 
library(caret) # used for modeling 
library(corrplot) # used for making correlation plot 
library(xgboost) # used for building XGBoost model 
library(cowplot) # used for combining multiple plots 

train<-read.csv("C:/Users/Naman/Desktop/DV_Dataset/train_v9rqX0R.csv")
test<-read.csv("C:/Users/Naman/Desktop/DV_Dataset/test_AbJTz2l.csv")

dim(train)
dim(test)

names(train)
names(test)

str(train)
str(test)

combi <- rbind(train[1, 11], test)
combi <- rbind(test, train[, names(test)])
combi
# Code for combining training and testing dataset this is helpful if we want to make any change in dataset then we don't have to change in test and training dataset, it saves a lot of time.

dim(combi)

ggplot(train)+geom_histogram(aes(Item_Outlet_Sales),binwidth=100,fill="darkgreen")+xlab("Item_Outlet_Sales")

p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue") 
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")
plot_grid(p1, p2, p3, nrow = 1)

ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count=n())) + geom_bar(aes(Item_Fat_Content,Count), stat="identity", fill="coral")

combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"  
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count=n())) + geom_bar(aes(Item_Fat_Content,Count), stat="identity", fill="coral")

p4=ggplot(combi %>% group_by(Item_Type) %>% summarise(Count=n())) +geom_bar(aes(Item_Type,Count), stat="identity", fill="coral")+xlab("")+geom_label(aes(Item_Type,Count,label=Count),vjust=0.5)+theme(axis.text.x=element_text(angle=45,hjust=1))+ggtitle("Item_Type")
p4

p5=ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count=n())) + geom_bar(aes(Outlet_Identifier,Count), stat="identity", fill="coral")+geom_label(aes(Outlet_Identifier,Count,label=Count),vjust=0.5)+theme(axis.text.x=element_text(angle=45,hjust=1))
p5

p6=ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count=n()))+ geom_bar(aes(Outlet_Establishment_Year,Count), stat="identity", fill="coral")+geom_label(aes(Outlet_Establishment_Year,Count,label=Count),vjust=0.5)+theme(axis.text.x=element_text(angle=45,hjust=1))
p6

p7=ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count=n())) + geom_bar(aes(Outlet_Type,Count), stat="identity", fill="coral")+geom_label(aes(Outlet_Type,Count,label=Count),vjust=0.5)+theme(axis.text.x=element_text(angle=45,hjust=1))
p7

p8 = ggplot(train) + geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +theme(axis.title = element_text(size = 8.5))
p8

sm = sum(is.na(combi$Item_Weight))
nonNullVal = nrow(combi) - sm
missing_index = which(is.na(combi$Item_Weight))
for(i in missing_index){
  item = combi$Item_Identifier[i]
  combi$Item_Weight[i] = sum(combi$Item_Weight, na.rm = T)/nonNullVal
}

ggplot(combi) + geom_histogram(aes(Item_Visibility), bins=100)

zero_index = which(combi$Item_Visibility == 0) 
for(i in zero_index){ item = combi$Item_Identifier[i]
combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], 
                                na.rm = T)
}

corMatrix<-cor(combi[1:nrow(train),][sapply(combi[1:nrow(train),],is.numeric)])
corMatrix

corrplot::corrplot(corMatrix, method="number", type="upper")
corrplot::corrplot(corMatrix, method="number", type="upper", order="hclust")
corrplot::corrplot(corMatrix, method="number", type="upper")
corrplot::corrplot(corMatrix, method="number", type="upper", order="hclust")


##########
ggplot(train[1:nrow(train),],aes(Item_Visibility, Item_Outlet_Sales))+geom_point(size=2.5,
                                                                                 aes(colour=factor(Outlet_Identifier)))+theme(axis.text.x=element_text(angle = 70,vjust = 0.5,color = "black")) + 
  xlab("Item Visibility")+ylab("Item Outlet Sales")+ggtitle("Item Sales Vs Item Visibility")


ggplot(train[1:nrow(train),],aes(x=Item_Type,y=Item_Outlet_Sales,fill=Outlet_Type))+geom_boxplot()+theme(axis.title.x = 
                                                                                                           element_text(angle=70,vjust=0.5,color = "black"))+xlab("Item Type")+ylab("Sales")+ggtitle("Sales vs Item type")

##########

ggplot(train,aes(Item_Type,Item_Outlet_Sales/Item_MRP,fill=Outlet_Type))+geom_bar(stat = "identity",position = 
                                                                                    position_dodge())+theme(axis.title.x = element_text(angle=70,vjust = 0.5,color = "black"))+xlab("Item Type")+ylab("Item Outlet Sales")+ggtitle("Item Sales vs Item type")


##########

ggplot(train[1:nrow(train),],aes(Item_Visibility,Item_Outlet_Sales))+geom_point(size=2.5,
                                                                                                   aes(colour=factor(Outlet_Identifier)))+theme(axis.text.x=element_text(angle = 70,vjust = 0.5,color = "black"))+
  xlab("Item Visibility")+ylab("Item Outlet Sales")+ggtitle("Item Sales Vs Item Visibility")

##########
ggplot(train[1:nrow(train),],aes(x=Item_Type,y=Item_Outlet_Sales,fill=Outlet_Type))+geom_boxplot()+
  theme(axis.text.x=element_text(angle = 70,vjust = 0.5,color = "black"))+xlab("Item type")+ylab(" Sales")+
  ggtitle("Sales Vs Item Visibility")
##########

ggplot(train,aes(Item_Type,Item_Outlet_Sales/Item_MRP,fill=Outlet_Type))+ geom_bar(stat = "identity",position = position_dodge())+
  theme(axis.title.x = element_text(angle=70,vjust = 0.5,color = "black"))+xlab("Item Type")+
  ylab("Item Outlet Sales")+ggtitle("Item Sales vs Item type")