library(ggplot2)
library(patchwork)

set.seed(10)
diamonds2 <- diamonds[sample(nrow(diamonds), size = 400), ]

theme_set(theme_bw())

#Question 1
p1 = ggplot(diamonds2, aes(carat, price))
p1 + geom_point()

#Question 2
p2 = p1 + 
  geom_point(color = "blue")


#Question 3
p3 = p1 + 
  geom_point(aes(col = cut)) + 
  geom_rug(alpha = 0.1) + 
  labs(col='Cut quality') + 
  theme(legend.position = "top")


#Question 4
p4 = p1 + 
  geom_point(aes(col = depth > 62.5, shape = depth > 62.5)) + 
  labs(col = 'depth > 62', shape = 'depth > 62') 


#Question 5
p5 = p1 + 
  geom_point(aes(col = cut, shape = depth > 62.5)) + 
  labs(shape = 'depth > 62') 


#Question 6
p6 = p5 +
  labs(x = "Number of carats",
       y = "Price in US dollars",
       title = "Diamonds data: price versus carat.",
       subtitle = "Color by cut and shape by depth") 


#Question 7
p7 = p4 +
  geom_smooth(method = "lm", se=FALSE, aes(color = depth > 62.5))


#Question 8
p8 = p4 +
  geom_smooth(method = "lm", se=FALSE, aes(linetype = depth > 62), col = "grey")


#question 9
p9 = p8 +
  geom_point(size= 0.8, color = "grey")


#Question 10
p10 = p4 +
  geom_smooth(method = "lm", se = F, color = "grey")



#Question 11
p11 = p7 +
  geom_smooth(method = "lm", se = F, color = "grey", linetype = 3)


#Question 12
p12 = ggplot(diamonds2, aes(log10(carat), log10(price))) + 
  geom_point(aes(col = depth > 62.5, shape = depth > 62.5)) + 
  labs(col = 'depth > 62', shape = 'depth > 62') +
  geom_smooth(method = "lm", se=FALSE, aes(color = depth > 62.5)) +
  geom_smooth(method = "lm", se = F, color = "grey", linetype = 3)


#question 13
p12 = ggplot(diamonds2, aes(log10(carat), log10(price))) + 
  geom_point(aes(col = depth > 62.5, shape = depth > 62.5)) + 
  labs(col = 'depth > 62', shape = 'depth > 62') +
  geom_smooth(method = "lm", se = F, color = "grey", linetype = 3) +
  geom_smooth(method = "lm", se = F, aes(color = depth > 62.5))


#Question 13
p13 = p10 + coord_cartesian(
  xlim = c(0.3,1),
  ylim = c(1000,10000),
  expand = TRUE,
  default = FALSE,
  clip = "on"
)


#Question 14
p14 = p13 + lims(x = c(0.3,1), y = c(1000, 10000))


#Question 15
p15 = p1  + geom_point() +
  geom_smooth(formula = y~x, se = F) +
  geom_function(fun = \(x) exp(x), xlim) # a voir



#Qestion 16
p16 = ggplot(diamonds2, aes(x = cut), stat = "count") + geom_bar(alpha=0.5, aes(y = ..count.. / sum(..count..))) + labs(y = "prop")


#Question 17

test = cut(diamonds2$price, breaks = 1040:3300)
test
diamonds2$priceC = 0
diamonds2[diamonds2$price<=1040,]$priceC = 1
diamonds2[diamonds2$price>1040 & diamonds2$price<= 3300,]$priceC = 2
diamonds2[diamonds2$price> 3300,]$priceC = 3
diamonds2$priceC = as.factor(diamonds2$priceC)


p17 = ggplot(diamonds2, aes(x = cut, fill = priceC , color = priceC)) +
  geom_bar(position = "fill")


# gauche

ggplot(diamonds2, aes(x = priceC, y= ..prop.., group = cut)) + 
  geom_bar(alpha=0.5) + 
  facet_grid(cols = vars(cut)) +
  geom_text(aes(label = scales::percent(..prop.., accuracy = 1, suffix = ""),
                 y= ..prop.. ), stat= "count", vjust = 4)

# question 18

ggplot(diamonds2, aes(x=log(price))) + 
  geom_histogram(aes(y= ..density..), boundary = 5.5, binwidth = 0.5, color = "white", fill = "grey") + 
  stat_function(fun = dnorm, xlim = c(5.5, 10), args = list(mean = mean(log(diamonds2$price)), sd = sd(log(diamonds2$price)))) +
  ggtitle("Histogram of the logarithm of price
together with the adjusted Normal density (black curve)")
  
# question 19

ggplot(diamonds2, aes(x=log(price))) + 
  stat_ecdf(color = "red3") + 
  stat_function(fun = pnorm, args = list(mean = mean(log(diamonds2$price)), sd = sd(log(diamonds2$price))))

# question 20

ggplot(diamonds2, aes(x = price, y = cut)) + 
  geom_boxplot() + 
  scale_y_discrete(labels = c("Fair \n(n=10)", "Good \n(n=43)", "Very Good \n(n=93)", "Premium \n(n=96)", "Ideal \n(n=158)"))

# question 21

ggplot(diamonds2, aes(y = price, x = cut)) + 
  geom_violin(color = NA, fill = "lightgray", alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.2, varwidth = TRUE, outlier.shape = NA, fill = NA) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.2)
  
#question 22

P1 = ggplot(diamonds2, aes(y = price, x = cut, color = cut)) + 
  geom_boxplot(width = 0.2, varwidth = TRUE, outlier.shape = NA, fill = NA, size = 1) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.2)


P2 = ggplot(diamonds2, aes(x = price, color = cut, fill = cut)) + 
  geom_density(alpha = 0.1, size = 1)
P2

P3 = ggplot(diamonds2, aes(sample = price, color = cut)) + 
  facet_grid(cols = vars(cut)) +
  geom_qq() +
  geom_qq_line()


((P1 + guides(colour = "none")) + P2) / (P3 + guides(colour = "none")) + plot_layout(guides = "collect")
