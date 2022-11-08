# load libraries
library(tidyverse)
library(scales)
library(readxl)

#Set theme

theme_set(theme_classic())

# load data
qualtrics_data <- read_excel("qualtrics-data.xlsx")



# restructure data

qualtrics_data$residents_n <- as.numeric(qualtrics_data$residents)
qualtrics_data$residents_f <- as.factor(qualtrics_data$residents)

# removed dollar signs and columns

gsub("[$,]","",qualtrics_data$gfi_2022_expected)
qualtrics_data$gfi_2022_expected <- as.numeric(qualtrics_data$gfi_2022_expected)

# food security

table(qualtrics_data$food_security)

qualtrics_data$food_security <- 
  recode_factor(qualtrics_data$food_security, 
         "There is always enough food in my home to satisfy the needs of those in the household" = "Always Enough",
         "There is mostly enough food in my home to satisfy the needs of those in the household" = "Mostly Enough",
         "There is rarely enough food in my home to satisfy the needs of those in the household" = "Rarely Enough",
         "There is never enough food in my home to satisfy the needs of those in the household" = "Never Enough")

qualtrics_data %>% 
  drop_na(food_security) %>% 
  ggplot(aes(food_security, fill = food_security)) +
    geom_bar() +
    scale_fill_brewer(palette = "Dark2") +
    theme_classic() +
    theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
    labs(title = "There is _______ food in my home to satisfy the needs of those in my household",
       x = "",
       y = "")

ggsave("food_security.png")
  

# residents

qualtrics_data %>% 
  filter(!is.na(residents_f)) %>% 
  group_by(residents_f) %>% 
  count()

qualtrics_data %>% 
  drop_na(residents_f, gfi_2022_expected) %>% 
  ggplot(qualtrics_data, mapping = aes(residents_f, gfi_2022_expected)) +
    geom_boxplot() + 
    theme_classic() + 
    scale_y_continuous(labels = comma) +
    theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5)) +
    labs(title = "Mean Income by Household Size",
       x = "",
       y = "")
ggsave("income_by_housesize.png")

# income

qualtrics_data %>% 
  filter(Finished == "True") %>% 
  drop_na(gfi_2022_expected) %>% 
  ggplot(qualtrics_data, mapping = aes(Finished, gfi_2022_expected)) +
  geom_boxplot() + 
  theme_classic() + 
  scale_y_continuous(labels = comma) + 
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank()) +
  labs(title = "Mean Income",
       x = "",
       y = "") 
ggsave("income.png")


# Under 18

# Replace NA

qualtrics_data$under18count <- as.numeric(qualtrics_data$under18count)

# Convert "One" to 1

qualtrics_data$under18count[qualtrics_data$under18count == "One"] <- 1

# Convert to numeric

qualtrics_data$under18count[is.na(qualtrics_data$under18count)] <- 0

# Run histogram

ggplot(qualtrics_data, aes(under18count)) + 
  geom_histogram(color = "black",
                 fill = "gray") +
  labs(x = "Number of Dependents < 18 Years Old",
       y = "")

ggsave("under18count.png")

qualtrics_data %>% 
  group_by(under18count) %>% 
  count() 

# housing

table(qualtrics_data$housing)

qualtrics_data$housing <- 
  recode_factor(qualtrics_data$housing, 
                "I own my house or apartment" = "Own",
                "I rent my own house or apartment" = "Rent",
                "Other" = "Other")

qualtrics_data %>% 
  drop_na(housing) %>% 
  ggplot(aes(housing, fill = housing)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "",
       x = "",
       y = "")

ggsave("housing.png")

