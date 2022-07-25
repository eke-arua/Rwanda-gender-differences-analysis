library(here)
library(dplyr)
library(nlme)
library(ggplot2)
library(stringr)
library(gt)
library(gtsummary)

data <- read.csv(here("Marc data updated.csv"))

#Format the data
data <- data %>% mutate(STEM = str_to_upper(STEM), STEM = str_trim(STEM, side = "both"))
data <- data %>% mutate(across(where(is.character), as.factor)) #changes characters to factors
data$Gender <- recode_factor(data$Gender, F = "Female", M = "Male")
data$FamillyCategory <- recode_factor(data$FamillyCategory, L = "Low income",
                                      M = "Middle income", H = "High income")

#Check for missing data
missing <- tibble(variables = names(data), number_missing = NA)
for (i in 1:ncol(data)){
  missing$number_missing[i] = sum(is.na(data[i]))
}#No missing data


#Exploratory data analysis (table)
t1 <- data %>% select(Gender, Age, Status, FamillyCategory, Sponsorship,
                      Accommodation, S6_SchoolAttended, DegreeClass,
                      SelfOrientation, CumAvGrade) %>%
  tbl_summary(
    by = Gender,
    missing = "no",
    statistic = list(
      Age ~ "{mean} ({sd})" ,
      Status ~ "{n} ({p}%)",
      FamillyCategory ~ "{n} ({p}%)",
      Sponsorship ~ "{n} ({p}%)",
      Accommodation ~ "{n} ({p}%)",
      S6_SchoolAttended ~ "{n} ({p}%)",
      DegreeClass ~ "{n} ({p}%)",
      SelfOrientation ~ "{n} ({p}%)",
      CumAvGrade ~ "{mean} ({sd})"),
    label = list(
      Age = "Age",
      Status = "Marital status",
      FamillyCategory = "Family income status",
      Sponsorship = "Sponsorship",
      Accommodation = "Accommodation",
      S6_SchoolAttended = "Highschool type",
      DegreeClass = "Degree Class",
      SelfOrientation = "Self Orientation",
      CumAvGrade = "Final Grade"),
    digits = list(
      Age ~ c(1, 1),
      CumAvGrade ~ c(1, 1)),
    type = list(
      Accommodation ~ "categorical",
      SelfOrientation ~ "categorical"),
    sort = list(
      Accommodation ~ "alphanumeric",
      SelfOrientation ~ "alphanumeric"
    )
  )
(t2 <- add_overall(t1, last = TRUE))

#Plot the outcome variable by Sex

#Plot 1: A comparison of the GPA distribution by Gender
ggplot(data) +
  geom_histogram(aes(CumAvGrade), bins = 40) +
  geom_vline(xintercept = mean(data$CumAvGrade)) +
  facet_wrap(vars(Gender))

#Plot 2: A comparison of GPA By Gender
ggplot(data, aes(x = Gender, y = CumAvGrade, colour = Gender)) +
  geom_boxplot() #  Not much difference in performance by Gender

#Plot 3: Same comparison stratified by Sponsorship
ggplot(data, aes(x = Gender, y = CumAvGrade, colour = Gender)) +
  geom_boxplot() +
  facet_wrap(vars(Sponsorship)) #We can see that overall, self-sponsored females perform better than self-sponsored males

#Plot 4: By income group
ggplot(data, aes(x = Gender, y = CumAvGrade, colour = Gender)) +
  geom_boxplot() +
  facet_wrap(vars(FamillyCategory)) +
  theme_bw()#We can see a difference between gender performances in high and middle income groups. Non in low income.

ggplot(data, aes(x = Gender, y = CumAvGrade, colour = Gender)) +
  geom_boxplot() +
  facet_wrap(vars(Accommodation)) #We can see a difference between gender performances in high and middle income groups. Non in low income.

ggplot(data, aes(x = Gender, y = CumAvGrade, colour = Gender)) +
  geom_boxplot() +
  facet_wrap(vars(PrincipalPasses)) #We can see a difference between gender performances in high and middle income groups. Non in low income.

ggplot(data, aes(x = Gender, y = CumAvGrade, colour = Gender)) +
  geom_boxplot() +
  facet_wrap(vars(SelfOrientation)) #We can see a difference between gender performances in those who have self orientation

ggplot(data, aes(x = Gender, y = CumAvGrade, colour = Gender)) +
  geom_boxplot() +
  facet_wrap(vars(S6_SchoolAttended)) #We can see a difference between gender performances in those who went to private and public schools


ggplot(data, aes(x = Gender, y = CumAvGrade, colour = Gender)) +
  geom_boxplot() +
  facet_wrap(vars(School), nrow = 4)

ggplot(data, aes(x = Age, y = CumAvGrade, colour = Gender)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") # This shows the relationship between age and Grade by Gender. We fit a linear regression line between them
