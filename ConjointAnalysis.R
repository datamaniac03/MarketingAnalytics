#Scott Jacobs
#Marketing Notes

#create dataframe
pref_grid <- expand.grid(Pulp = c(0,1), Organic = c(0,1), Brand = c(0,1), Price = c(5,10)) #create df fr all combos

#add preference
pref_grid$Preference <- sample(1:16)#randomly assigning preference
pref_grid$RPreference <- rev(pref_grid$Preference) #rev, reverse

#no intercept
linreg1 <- lm(RPreference ~ .-1, data = pref_grid[,-5])

#intercept
linreg2 <- lm(RPreference ~ ., data = pref_grid[,-5])

#part worths
pw1 <- 10*abs(linreg2$coefficients[5])
pw2 <- 5*abs(linreg2$coefficients[5])
price_pw <- pw1-pw2

partworth <- linreg2$coefficients
partworth$Price <- pw
partworth <- as.data.frame(partworth)
#Importance
importance <- partworth/sum(partworth)
