## Question 1

Question1 <- read.csv('Question1.csv')

#H1 : True Mean of paying bill is less than 22 days

t.test(Question1$Payment, alternative = 'less', mu = 22, conf.level = 0.99)

# p -value = 0.1752

## Question 2

Question2 <- read.csv('Question2.csv')

#H0 : Republican will have less than or equal to 0.50 votes
#H1 : Republican will win i.e. Prop greater than 0.50

num_dem <- length(which(Question2$Votes == 2))
total_num <- length(Question2$Votes)

prop.test(num_dem, total_num, alternative = 'greater', p = 0.5)

# Reject null hypothesis 
# p-value = 0.04
# alpha = 0.1

## Question 3

Question3 <- read.csv('Question3.csv')

#H0 : Town's average is equal to national average of $150
#H1 : Town's average is not equal to national average of $150

t.test(Question3$Weekly_food_expense, alternative = 'two.sided', mu = 150)

t.test(Question3$Weekly_food_expense, alternative = 'two.sided', mu = 150, conf.level = 0.90)

t.test(Question3$Weekly_food_expense, alternative = 'two.sided', mu = 150, conf.level = 0.99)

## Part b

sd_3 = sd(Question3$Weekly_food_expense)
n_3 = nrow(Question3$Weekly_food_expense)

q3_b_1_t_upper_end = qt(0.995, df = 99)
x_bar_1_ue = (q3_b_1_t_upper_end * (sd_3/10)) + 150
q3_b_1_t_lower_end = qt(0.005, df = 99)
x_bar_1_le = (q3_b_1_t_lower_end * (sd_3/10)) + 150
              
q3_b_10_t_upper_end = qt(0.95, df = 99)
x_bar_10_ue = (q3_b_10_t_upper_end * (sd_3/10)) + 150
q3_b_10_t_lower_end = qt(0.05, df = 99)
x_bar_10_le = (q3_b_10_t_lower_end * (sd_3/10)) + 150

 ## Question 4

Question4 <- read.csv("Question4.csv")

#H0 : Variance is equal to 250
#H1 : Variance is not to equal to 250

varTest(Question4$Demand, alternative = "two.sided", sigma.squared = 250)

#Fail to reject H0 i.e. cannot reject that Variance is equal to 250

## Question 5

## Part a

z_5 = qnorm(0.05)
p_5 = 0.14
n = 1500
denomi_5 = sqrt(p_5*(1 - p_5)/n)
p_hat = (z_5 * denomi_5) + p_5

## Part b

z_5_b = (p_hat - 0.134)/(sqrt(0.134*(1 - 0.134)/n))

pnorm(z_5_b)

## Question 7

## Part a

p_convinced = 0.42

p_convicted = p_convinced^12

## Part b
print('Type I error')

## Part c

prob_fail_check = 1 - (0.99^12)

prob_fails = pbinom(11, 12, 0.99)

## Part d
print('Type II error')
