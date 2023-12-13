df <- read.csv("cleaned_data2.csv")
library(dplyr)
library(ggplot2)

columnIndex1 <- which(names(df) == "Game.Feature.Single.player")
columnIndex2 <- dim(df)[2]
col_names <- colnames(df)

## Features的图
## Select the data with price<= 100的游戏和前5000的data build model due to hardware limitation，this model can be extend to more data to get a more accuracy result.
df1 <- 
  df%>%
  filter(Original.Price >= 0L & Original.Price <= 100L)
df1 <- df1[1:5000, ]



output_path <- "/Users/chenxuanxiong/Desktop/Computation Assignment/MA678-midterm-proj/plots/"
dir.create(output_path, showWarnings = FALSE)


for (col_index in columnIndex1:columnIndex2) {
  price_means <- df1 %>%
    group_by(as.factor(df1[, col_index])) %>% 
    summarise(price_mean = mean(Original.Price))
    price_means <- as.data.frame(price_means)
  

    p <- ggplot(df1) + 
      geom_density(aes(x = Original.Price, color = as.factor(df1[, col_index]))) +
      geom_vline(data = price_means,
                 aes(xintercept = price_means[,2], color = price_means[,1]),
                 linetype = 'dashed')+
      scale_color_discrete(name = "Feature status") + 
      labs(x = "Price",y = "Density", title = col_names[col_index] )
    print(p)
    filename <- paste(output_path, gsub(" ", "_", col_names[col_index]), ".png", sep="")
    ggsave(filename, p, width = 6, height = 4, units = "in")
}

## ANOVA for processer和别的
## We can not do Anova for processor since its memory usage is too high and it can not even run on SCC.
##So we just remove this variable
model_Processor <- aov(Original.Price ~ Processor, data = df1)
summary(model_Processor)

model_Memory <- aov(Original.Price ~ Memory, data = df1)
summary(model_Memory)

model_Graphics_Series <- aov(Original.Price ~ Graphics_Series, data = df1)
summary(model_Graphics_Series)

## Fit general linear regression model

## Null model : distribution of price
p1 <- ggplot(df1) + 
  geom_density(aes(x = Original.Price)) +
  labs(x = "Price",y = "Density", title = "Distribution of Price" )
print(p1)

df2 = df1[,c(3,columnIndex3:columnIndex4)]
df2_clean = na.omit(df2, cols = selected_variables)

null_model <- lm(log(Original.Price+1.01) ~ 1, data = df2_clean) 
summary(null_model)

##因为我们variable太多了，所以我们需要找到主导地位的variables，并且最大限度的删除多余的variables，我们使用forward selection找到主要的variavles
## forward selection
columnIndex3 <- which(names(df1) == "Processor")
columnIndex4 <- length(column_names)


df2 = df1[,c(3,columnIndex3:columnIndex4)]


forward_selection <- function(data, response, max_vars = NULL) {
  selected_vars <- c()
  remaining_vars <- setdiff(names(data), response)
  formula <- as.formula(paste(response, "~ 1"))
  
  while (length(selected_vars) < length(remaining_vars) && 
         (is.null(max_vars) || length(selected_vars) < max_vars)) {
    p_values <- numeric(length(remaining_vars))
    
    for (i in seq_along(remaining_vars)) {
      candidate_vars <- c(selected_vars, remaining_vars[i])
      candidate_formula <- as.formula(paste(response, "~", paste(candidate_vars, collapse = " + ")))
      model <- lm(candidate_formula, data = data)
      p_values[i] <- summary(model)$coefficients[2, 4]
    }
    
    best_variable <- remaining_vars[which.min(p_values)]
    selected_vars <- c(selected_vars, best_variable)
    remaining_vars <- setdiff(remaining_vars, best_variable)
  }
  
  return(selected_vars)
}

# Perform forward selection
selected_variables <- forward_selection(df2, response = "Original.Price")

# Display the selected variables
print(selected_variables)
library(lme4)

complete_pooling_model <- glm(log(Original.Price+1.01) ~ Game.Feature.Single.player + 
                                Game.Feature..Shared.Split.Screen.PvP + 
                                Game.Feature..Tracked.Controller.Support +
                                Game.Feature..Steam.Trading.Cards + 
                                Game.Feature..Remote.Play.on.TV +
                                Game.Feature..MMO + Game.Feature..Remote.Play.on.Tablet + 
                                Game.Feature..VR.Supported +
                                Game.Feature..SteamVR.Collectibles + 
                                Game.Feature..Shared.Split.Screen.Co.op +
                                Game.Feature..Captions.available + 
                                Game.Feature..Steam.Workshop + 
                                Game.Feature..Valve.Anti.Cheat.enabled +
                                Game.Feature..LAN.PvP +
                                Game.Feature..Includes.Source.SDK + 
                                Game.Feature..Cross.Platform.Multiplayer ,
                                family = Gamma(),data = df2_clean)

summary(complete_pooling_model)



partial_pooling_model <- lmer(log(Original.Price+1.01) ~ Game.Feature.Single.player + 
                                Game.Feature..Shared.Split.Screen.PvP + 
                                Game.Feature..Tracked.Controller.Support +
                                Game.Feature..Steam.Trading.Cards + 
                                Game.Feature..Remote.Play.on.TV +
                                Game.Feature..MMO + 
                                Game.Feature..Remote.Play.on.Tablet + 
                                Game.Feature..VR.Supported +
                                Game.Feature..SteamVR.Collectibles + 
                                Game.Feature..Shared.Split.Screen.Co.op +
                                Game.Feature..Captions.available  + 
                                Game.Feature..Steam.Workshop + 
                                Game.Feature..Valve.Anti.Cheat.enabled + 
                                Game.Feature..LAN.PvP +
                                Game.Feature..Includes.Source.SDK + 
                                Game.Feature..Cross.Platform.Multiplayer +
                                (1 | Memory),data = df2_clean)
summary(partial_pooling_model)

AIC(partial_pooling_model)


group <- df2_clean$Memory
## No pooling



no_pooling_model = lmer(log(Original.Price+1.01) ~ Game.Feature.Single.player + 
                          Game.Feature..Shared.Split.Screen.PvP + Game.Feature..Tracked.Controller.Support +
                          Game.Feature..Steam.Trading.Cards + Game.Feature..Remote.Play.on.TV +
                          Game.Feature..MMO + Game.Feature..Remote.Play.on.Tablet + Game.Feature..VR.Supported +
                          Game.Feature..SteamVR.Collectibles + Game.Feature..Shared.Split.Screen.Co.op +
                          Game.Feature..Captions.available  + Game.Feature..Steam.Workshop + 
                          Game.Feature..Valve.Anti.Cheat.enabled + Game.Feature..LAN.PvP +
                          Game.Feature..Includes.Source.SDK + Game.Feature..Cross.Platform.Multiplayer + 
                          (1 + Game.Feature..LAN.Co.op |Memory), data = df2_clean)

summary(no_pooling_model)


AIC(no_pooling_model)


cor_matrix <- cor(model.matrix(no_pooling_model))
vif_values <- car::vif(no_pooling_model)

#display(model4)
coefplot(no_pooling_model,title = "Fig 12.Coefficient plot for model 4")




anova_result <- anova(null_model, model1)
summary(anova_result)


AIC_no_pooling_model <- AIC(no_pooling_model)
AIC_partial_pooling_model <- AIC(partial_pooling_model)
AIC_complete_pooling_model <- AIC(complete_pooling_model)

BIC_no_pooling_model <- BIC(no_pooling_model)
BIC_partial_pooling_model <- BIC(partial_pooling_model)
BIC_complete_pooling_model <- BIC(complete_pooling_model)

AIC_info <- data.frame(AIC_no_pooling_model,AIC_partial_pooling_model,AIC_complete_pooling_model)
BIC_info <- data.frame(BIC_no_pooling_model,BIC_partial_pooling_model,BIC_complete_pooling_model)

## Model accuracy check
df3 <- 
  df%>%
  filter(Original.Price >= 0L & Original.Price <= 100L) %>%
  subset(select = selected_variables)
df3 <- cbind(Original.Price = y_observed, df3)

df3_clean = na.omit(df3, cols = selected_variables)
test_set <- df3_clean[5001:6000, ]

y_observed <- df%>%
  filter(Original.Price >= 0L & Original.Price <= 100L) %>%
  subset(select = "Original.Price")
y_observed <- test_set$Original.Price



predictions <- predict(complete_pooling_model, newdata = test_set, type = "response")+coef(null_model)[1]


# mean_values <- mean(predictions, na.rm = TRUE)
# 
# replace_na_with_zero <- function(x) {
#   x[is.na(x)] <- mean_values
#   return(x)
# }
# predictions <- lapply(predictions, replace_na_with_zero)


y_observed <- as.numeric(y_observed)
predictions <- as.numeric(predictions) + coef(null_model)[1]
res <- y_observed - predictions
se <- sqrt(sum(res^2) / (length(res) - 2))
lower_ci <- predictions - qt(0.9, df = length(res) - 2) * se
upper_ci <- predictions + qt(0.9, df = length(res) - 2) * se

x <- seq(1:1000)
plot_data <- data.frame(x, predictions,y_observed, res,lower_ci,upper_ci)



p3 <- ggplot(data = plot_data, aes(x = x, y = predictions)) +
  labs(x = "game index", y = "price" , title = "Price prediction plot")+
  geom_line(data = plot_data, aes(y = y_observed), color = "orange", linewidth = 1) +
  geom_line(data = plot_data, aes(y = predictions), color = "purple", linewidth = 1)
p3

p4<-residual_plot <- ggplot(plot_data, aes(x = x, y = res)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot", x = "Game index", y = "Residuals")
p4

mse <- mean((y_observed - predictions)^2)








