#  SALES DATA ANALYSIS - R PROJECT


# STEP 1: LOAD PACKAGES
library(dplyr) # used for functions like group_by(), summarise(), arrange(), and filter()
library(ggplot2) # used to create graphs


# STEP 2: IMPORT DATA
df <- read.csv("/Users/rishujain/Downloads/Sales.csv")
head(df)
summary(df)


# STEP 3: DATA CLEANING
df$Date             <- as.Date(df$Date)
df$Product_Category <- as.character(df$Product_Category)
df$Country          <- as.character(df$Country)
df$Customer_Gender  <- as.character(df$Customer_Gender)
df$Age_Group        <- as.character(df$Age_Group)


# STEP 4: ANALYSIS

# Total Revenue and Profit
sum(df$Revenue)
sum(df$Profit)

# Revenue by Category
df %>% group_by(Product_Category) %>%
  summarise(Revenue = sum(Revenue)) %>%
  arrange((Revenue))

# Revenue by Country
df %>% group_by(Country) %>%
  summarise(Revenue = sum(Revenue)) %>%
  arrange((Revenue))

# Revenue by Year
df %>% group_by(Year) %>%
  summarise(Revenue = sum(Revenue), Profit = sum(Profit))

# Revenue by Age Group
df %>% group_by(Age_Group) %>%
  summarise(Revenue = sum(Revenue)) %>%
  arrange((Revenue))

# Revenue by Gender
df %>% group_by(Customer_Gender) %>%
  summarise(Revenue = sum(Revenue))

# Top 5 Products
df %>% group_by(Product) %>%
  summarise(Revenue = sum(Revenue)) %>%
  arrange((Revenue)) %>%
  head(5)


# STEP 5: VISUALIZATIONS

# Chart 1 - Revenue by Category
#set up the canvas and axes 
ggplot(df, aes(x = Product_Category, y = Revenue, fill = Product_Category)) +  #set up the canvas and axes 
  geom_bar(stat = "identity") +  #draw the bars 
  labs(title = "Revenue by Category") +  #add titles and labels
  theme_minimal()

# Chart 2 - Revenue by Country
ggplot(df, aes(x = Country, y = Revenue, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Revenue by Country") +
  theme_minimal()

# Chart 3 - Yearly Revenue Trend
yearly <- df %>% group_by(Year) %>% summarise(Revenue = sum(Revenue))
ggplot(yearly, aes(x = Year, y = Revenue)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 3) +
  labs(title = "Yearly Revenue Trend") +
  theme_minimal()

# Chart 4 - Revenue by Age Group
ggplot(df, aes(x = Age_Group, y = Revenue, fill = Age_Group)) +
  geom_bar(stat = "identity") +
  labs(title = "Revenue by Age Group") +
  theme_minimal()

# Chart 5 - Revenue by Gender
ggplot(df, aes(x = Customer_Gender, y = Revenue, fill = Customer_Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Revenue by Gender") +
  theme_minimal()


# STEP 6: INSIGHTS
# 1. Bikes generate the most revenue (~72% of total)
# 2. USA is the top country by sales
# 3. Adults (35-64) spend the most
# 4. Revenue increases steadily from 2011 to 2016
# 5. Male and Female customers contribute almost equally


# STEP 7: RECOMMENDATIONS
# 1. Focus marketing on Adults (35-64) - biggest buyers
# 2. Expand in Australia - strong growth potential
# 3. Promote Accessories alongside Bikes
# 4. Run campaigns in Q4 - peak sales season
# 5. Introduce affordable products for younger customers

# END

