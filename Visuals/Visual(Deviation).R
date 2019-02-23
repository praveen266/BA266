####2. DEVIATION   
#Comparison of variation in values between small number of items (or categories) with respect to a fixed reference

##2.1 Diverging bars:
#In order to make sure you get diverging bars instead of just bars, make sure, your categorical variable has 2 categories that changes values at a certain threshold of the continuous variable. In below example, the mpg from mtcars dataset is normalised by computing the z score.

str(mtcars)
View(mtcars)
mtcars$car_name= rownames(mtcars) #Create new column for car_name
mtcars$mpg_z= round((mtcars$mpg- mean(mtcars$mpg))/sd(mtcars$mpg),2)  #compute normalized mpg
mtcars$mpg_z

# above/below flag
mtcars_1= mtcars[mtcars$mpg_z,]
mtcars_1= mtcars[order(mtcars$mpg_z),] # in order # sort

##########################################
mtcars_3= mtcars_3[,-13]
k=rownames(mtcars)
View(mtcars_3)
mtcars_3= cbind(mtcars_3,mtcars$mpg_z)
order(mtcars_3)
sort(mtcars_3$`mtcars$mpg_z`)
##########################################

ggplot(mtcars_1, aes(car_name, mpg_z,label= mpg_z))+
  geom_bar(stat= "identity", width = 0.5)+
  labs(title = "Diverging bars",
       subtitle = "Normalized mileage from mtcars"
  )

ggplot(mtcars_1, aes(car_name, mpg_z,label= mpg_z))+
  geom_bar(stat= "identity",width = 0.5)+
  labs(title = "Diverging bars",
       subtitle = "Normalized mileage from mtcars"
  )+
  coord_flip()
##to add color
mtcars_1$mpg_type= ifelse(mtcars_1$mpg_z<0, "below", "above")
View(mtcars_1)
ggplot(mtcars_1, aes(car_name, mpg_z,label= mpg_z))+
  geom_bar(aes(fill= mpg_type),stat= "identity",width = 0.5)+
  coord_flip()
## to sort
mtcars_1$car_name= factor(mtcars_1$car_name, levels = mtcars_1$car_name) # convert to factor tpo retain sorted order in plot
ggplot(mtcars_1, aes(car_name, mpg_z,label= mpg_z))+
  geom_bar(aes(fill= mpg_type),stat= "identity",width = 0.5)+
  coord_flip()
##2.2 Diverging lollipop chart
#Lollipop chart conveys the same information as bar chart and diverging bar. Except that it looks more modern.

ggplot(mtcars_1, aes(car_name, mpg_z,label= mpg_z))+
  geom_point()+
  coord_flip()

ggplot(mtcars_1, aes(car_name, mpg_z,label= mpg_z))+
  geom_point(stat = "identity",fill= "black",size= 6)+
  coord_flip()

ggplot(mtcars_1, aes(car_name, mpg_z,label= mpg_z))+
  geom_point(stat = "identity",fill= "black",size= 6)+
  geom_text(col= "white", size= 2)+
  ylim(-2.5,2.5)+
  geom_segment(aes(y=0,
                   x= car_name,
                   yend= mpg_z,
                   xend= car_name),
               col= "black")+
  coord_flip()

##2.3 Diverging dot plot:
#Dot plot conveys similar information. The principles are same as what we saw in Diverging bars, except that only point are used.

ggplot(mtcars_1, aes(car_name, mpg_z,label= mpg_z))+
  geom_point(stat = "identity",fill= "black",size= 6)+
  geom_text(col= "white", size= 2)+
  scale_color_manual(name= "mileage",
                     labels= c("Above Average", "Below Average"),
                     values = c("above"="#00ba38", "below"="3f8766d"))+
  
  ##2.4 Area chart:
  #Area charts are typically used to visualize how a particular metric (such as % returns from a stock) performed compared to a certain baseline. Other types of %returns or %change data are also commonly used.