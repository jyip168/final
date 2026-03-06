install.packages("ggplot2")
install.packages("vctrs")
install.packages("rlang")
install.packages("cli")
install.packages("dplyr")
install.packages("tidyr")
install.packages("broom")
library(ggplot2)
library(vctrs)
library(dplyr)
library(tidyr)
library(broom)

data <- read.csv("foods.csv")

# data <- data %>% filter(store == "Target")

# print(head(data))
# print(summary(data))

nutrient_cols <- c("Protein", "Total.Fat", "Carbohydrate", "Sugars..total", "Fiber..total.dietary", "Calcium", "Iron", "Sodium", "Vitamin.C", "Cholesterol", "Fatty.acids..total.saturated", "Total.Vitamin.A")

cor_by_category <- data %>%
  group_by(harmonized.single.category) %>%
  summarise(
    across(
      all_of(nutrient_cols),
      ~ cor(price, ., use = "complete.obs"),
      .names = "corr_price_{.col}"
    )
  )

# print(cor_by_category, n = Inf, width = Inf)

cor_long <- cor_by_category %>%
  pivot_longer(
    cols = starts_with("corr_"),
    names_to = "Nutrient",
    names_prefix = "corr_", 
    values_to = "Correlation"
  )

cor_heatmap <- ggplot(cor_long, aes(x = Nutrient, y = harmonized.single.category, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation between Price and Nutrients by Food Category",
      x = "Nutrient",
      y = "Food Category",
      fill = "Correlation")

# print(cor_heatmap)




get_slope <- function(df, nutrient) {
  fit <- lm(as.formula(paste("price ~", nutrient)), data = df)
  tidy(fit) %>% filter (term == nutrient) %>% select (estimate)
}

slopes_by_category <- data %>%
  group_by(harmonized.single.category) %>%
  summarise(across(
    all_of(nutrient_cols),
    ~ get_slope(cur_data(), cur_column())$estimate,
    .names = "slope_{.col}"
  ))

slopes_long <- slopes_by_category %>%
  pivot_longer(
    cols = starts_with("slope_"),
    names_to = "Nutrient",
    names_prefix = "slope_",
    values_to = "Slope"
  )

regr_heatmap <- ggplot(slopes_long, aes(x = Nutrient, y = harmonized.single.category, fill = Slope)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Slope, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Regression Slopes of Price on Nutrients by Food Category",
      x = "Nutrient",
      y = "Food Category",
      fill = "Slope")

# print(regr_heatmap)







selected_category = "pudding-jello"
selected_nutrient = "Total.Fat"

category_data <- data %>%
  filter(harmonized.single.category == selected_category)

scatterplot <- ggplot(category_data, aes_string(x = selected_nutrient, y = "price")) +
  geom_point(color = "darkgreen", size = 3, alpha = 0.6) +
  #geom_smooth(method = "lm", se = TRUE, color = "blue") +
  #coord_cartesian(xlim = c(0, 25), ylim=c(0,10)) +
  theme_minimal() +
  labs(
    title = paste("Scatterplot of", selected_nutrient, "vs Price for", selected_category),
    x = paste("Quantity of", selected_nutrient),
    y = "Price"
  )
print(scatterplot)