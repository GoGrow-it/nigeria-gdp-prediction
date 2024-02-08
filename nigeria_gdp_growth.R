################################################################################
###############################################################################

#© 2024 - GoGrow.it All Rights are Reserved

#interesting in using the computations? Write an email at general@gogrow.it

#data source: statista.com




#Uncomment & Install if the required packages are not present

#install.packages("extrafont")
#install.packages(ggplot2)

################################################################################
################################################################################



# Required Libraries
library("ggplot2")
library("extrafont")

# Data
years <- c(2019, 2021, 2022, 2023)
transactions_usd <- c(36.29, 452.01, 610.94, 946.95)
data <- data.frame(years, transactions_usd)

# Linear Regression Model
model <- lm(transactions_usd ~ years, data = data)

# Predictions for 2024, 2025, and 2026
new_years <- data.frame(years = c(2024, 2025, 2026))
predictions <- predict(model, newdata = new_years)

# Plot --V1
ggplot(data, aes(x = years, y = transactions_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_point(data = new_years, aes(y = predictions), color = "red") + theme_minimal()+
  
  ggtitle("Credit/Debit Card Transactions in Nigeria") +
  xlab("Year") + ylab("Transactions (in billion USD)") + labs(
    caption = "Red points are predicted values for 2024, 2025, and 2026")  +
  theme(plot.title=element_text(hjust = 0.5, family = "Rockwell"), 
        axis.title.x=element_text(color = "blue", size = 15, family="Rockwell"),
        axis.title.y=element_text(color = "blue", size = 15, family="Rockwell"),
        legend.title = element_text(size=13),
        legend.text=element_text(size=10, family="Rockwell" ))  

# Print Predictions
print(paste("Predicted transactions for 2024: ", predictions[1], "billion USD"))
print(paste("Predicted transactions for 2025: ", predictions[2], "billion USD"))
print(paste("Predicted transactions for 2026: ", predictions[3], "billion USD"))


# PLOT --V2
ggplot(data, aes(x = years, y = transactions_usd)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_point(data = new_years, aes(y = predictions), color = "red") +
  annotate("text", x = new_years$years, y = predictions, label = round(predictions, 2), hjust = -0.1, vjust = -1) +
  labs(title = "Credit/Debit Card Transactions in Nigeria",
       x = "Year",
       y = "Transactions (in billion USD)",
       caption = "Red points and labels are predicted values for 2024, 2025, and 2026")  + theme_minimal()+
  
  ggtitle("Credit/Debit Card Transactions in Nigeria") +
  xlab("Year") + ylab("Transactions (in billion USD)") + labs(
    caption = "Red points are predicted values for 2024, 2025, and 2026")  +
  theme(plot.title=element_text(hjust = 0.5, family = "Rockwell"), 
        axis.title.x=element_text(color = "blue", size = 15, family="Rockwell"),
        axis.title.y=element_text(color = "blue", size = 15, family="Rockwell"),
        legend.title = element_text(size=13),
        legend.text=element_text(size=10, family="Rockwell" )) 

