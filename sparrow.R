sp <-read.table("sparrows.txt", header=TRUE)
sp 
attach(sp)
str(sp)
summary(sp)
names(sp)

summary(Length)
detach(sp)


##if dataframe not attached you need to specify the dataframe with the variable
summary(sp$Length)