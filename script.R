# load libraries
library(tidyverse)
library(scales)
library(readxl)
library(ggthemes)


# load data
colnames <- as.character(read_excel("dec-qualtrics-data.xlsx", n_max = 1, col_names = FALSE))
qualtrics_data <- read_excel("dec-qualtrics-data.xlsx", skip = 2, col_names = colnames) %>% 
  filter(Status == "IP Address")

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
    theme_fivethirtyeight() +
    theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
    labs(title = "There is _______ food in my home to satisfy \n the needs of those in my household",
       x = "",
       y = "")


food_security_count <- qualtrics_data %>% 
  filter(!is.na(food_security)) %>% 
  group_by(food_security) %>% 
  count()

table(qualtrics_data$food_security)/sum(table(qualtrics_data$food_security))*100



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
       x = "Number of Residents",
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
  theme_classic() +
  labs(x = "Number of Dependents < 18 Years Old",
       y = "")

ggsave("under18count.png")

qualtrics_data %>% 
  group_by(under18count) %>% 
  count() 

# Bar chart

qualtrics_data %>% 
  drop_na(under18) %>% 
  ggplot(aes(under18, fill = under18)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "",
       x = "",
       y = "")
ggsave("under18.png")

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

# Single Parent



qualtrics_data$singleparent_f <- as.factor(qualtrics_data$singleparent)
qualtrics_data$singleparent_f <- recode(qualtrics_data$singleparent_f,
                                        "yes" = "Yes",
                                        "NO" = "No",
                                        "no" = "No",
                                        "O" = "No",
                                        "Widow" = "NA",
                                        "N/a" = "NA",
                                        "N/A" = "NA",
                                        "Na" = "NA",
                                        "1" = "NA",
                                        "Just me  childen are all married" = "NA",
                                        "My son moved in after , I lost my husband to covid." = "NA")
qualtrics_data$singleparent_f <- na_if(qualtrics_data$singleparent_f,"NA")

qualtrics_data %>% 
  drop_na(singleparent_f) %>% 
  ggplot(aes(singleparent_f, fill = singleparent_f)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "",
       x = "",
       y = "")
ggsave("singleparent.png")

# Over 65




qualtrics_data$over65count_f <- recode(qualtrics_data$over65count,
                                        "0" = "NA",
                                        "myself" = "1",
                                        "One" = "1",
                                        "two" = "2")

qualtrics_data$over65count_f <- na_if(qualtrics_data$over65count_f,"NA")

                                        
qualtrics_data$over65count_n <- as.numeric(qualtrics_data$over65count_f)


# Run histogram

qualtrics_data %>% 
  drop_na(over65) %>% 
  ggplot(aes(over65, fill = over65)) +
  geom_bar() +
  scale_fill_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "",
       x = "",
       y = "")
ggsave("over65.png")


qualtrics_data_combined <- qualtrics_data %>% 
  mutate(gfi = coalesce(gfi1, gfi2, gfi3, gfi4, gfi5, gfi6, gfi7, gfi8)) 


# Employment Affected by COVID

table(qualtrics_data$employ_affect_covid)

# Temp Reduction in Hours

table(qualtrics_data$temp_reduction_hours)


# Permanent Reduction in Hours

table(qualtrics_data$perm_reduction_hours)


# Temporary Out of Work

table(qualtrics_data$temp_out_of_work)



# Permanent Out of Work

table(qualtrics_data$perm_out_of_work)

# Work Remotely

table(qualtrics_data$work_remotely)

# How Found Work

table(qualtrics_data$how_found_work)

# Require Child Care

table(qualtrics_data$require_child_care)

# Used Community Services

table(qualtrics_data$used_community_nonprofit_services)

table(qualtrics_data$how_found_work,
      qualtrics_data$perm_out_of_work)
