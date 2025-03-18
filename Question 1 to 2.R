
library(tidyverse)

graduate_survey <- read.csv("C:/Users/reati/Downloads/graduate_survey.csv")
  View(graduate_survey)
  
# Create a new data frame 'data' from graduate_survey
data <- graduate_survey

# Question 1a: Select relevant columns
selected_cols <- c("Campus", "StudyField", "Branch", "Role", "EduLevel", "ProgLang", 
                   "Databases", "Platform", "WebFramework", "Industry", "AISearch", "AITool")

data_clean <- data %>%
  select(all_of(selected_cols))

# Question 1b: Handle missing values
data_clean <- data_clean %>%
  mutate(across(c(ProgLang, Databases, Platform, WebFramework, AISearch, AITool),
                ~ifelse(is.na(.), "None", .)))

# Question 1c: Standardize campus names
data_clean <- data_clean %>%
  mutate(Campus = str_replace(Campus, "Campus", ""))

# Question 1d: Subset to top 5 campuses
top_campuses <- data_clean %>%
  count(Campus) %>%
  top_n(5, n) %>%
  pull(Campus)

data_clean <- data_clean %>%
  filter(Campus %in% top_campuses)

## 2i: Tool Popularity Analysis (Redone)
# Programming Languages
prog_lang_summary <- data_clean %>%
  count(ProgLang) %>%
  arrange(desc(n)) %>%
  top_n(5)

ggplot(prog_lang_summary, aes(x = reorder(ProgLang, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top Programming Languages", x = "Language", y = "Count") +
  coord_flip()

# Databases
db_summary <- data_clean %>%
  count(Databases) %>%
  arrange(desc(n)) %>%
  top_n(5)

ggplot(db_summary, aes(x = reorder(Databases, n), y = n)) +
  geom_bar(stat = "identity", fill = "#2ca25f") +
  labs(title = "Top Databases", x = "Database", y = "Count") +
  coord_flip()

# Platforms
platform_summary <- data_clean %>%
  count(Platform) %>%
  arrange(desc(n)) %>%
  top_n(5)

ggplot(platform_summary, aes(x = reorder(Platform, n), y = n)) +
  geom_bar(stat = "identity", fill = "#ff5733") +
  labs(title = "Top Platforms", x = "Platform", y = "Count") +
  coord_flip()

# Web Frameworks
web_framework_summary <- data_clean %>%
  count(WebFramework) %>%
  arrange(desc(n)) %>%
  top_n(5)

ggplot(web_framework_summary, aes(x = reorder(WebFramework, n), y = n)) +
  geom_bar(stat = "identity", fill = "#f4b400") +
  labs(title = "Top Web Frameworks", x = "Web Framework", y = "Count") +
  coord_flip()


#AI SEARCH
ai_search_summary <- data_clean %>%
  count(AISearch) %>%  # Use correct column name without underscore
  arrange(desc(n)) %>%
  top_n(5)

ggplot(ai_search_summary, aes(x = reorder(AISearch, n), y = n)) +
  geom_bar(stat = "identity", fill = "#6a3d9a") +
  labs(title = "Top AI Search Tools", x = "AI Search", y = "Count") +
  coord_flip()

# AI Tools
ai_tools_summary <- data_clean %>%
  count(AI_Tools) %>%
  arrange(desc(n)) %>%
  top_n(5)

ggplot(ai_tools_summary, aes(x = reorder(AI_Tools, n), y = n)) +
  geom_bar(stat = "identity", fill = "#e31a1c") +
  labs(title = "Top AI Tools", x = "AI Tools", y = "Count") +
  coord_flip()



## 2ii: Industry Analysis By Study Field
industry_by_field <- data_clean %>%
  count(StudyField, Industry) %>%
  group_by(StudyField) %>%
  top_n(5, n)

ggplot(industry_by_field, aes(x = reorder(Industry, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity") +
  facet_wrap(~StudyField, scales = "free") +
  coord_flip() +
  labs(title = "Industries by Study Field")

# Overall
industry_overall <- data_clean %>%
  count(Industry) %>%
  arrange(desc(n))



## 2iii: Job Role Analysis (By Study Field)

jobrole_by_field <- data_clean %>%
  count(StudyField, Role) %>%  # Changed to 'Role'
  group_by(StudyField) %>%
  top_n(5, n)

ggplot(jobrole_by_field, aes(x = reorder(Role, n), y = n, fill = StudyField)) +
  geom_bar(stat = "identity") +
  facet_wrap(~StudyField, scales = "free") +
  coord_flip() +
  labs(title = "Job Roles by Study Field", x = "Job Role", y = "Count")



# Question 2iii (By overall job roles analysis)

jobrole_overall <- data_clean %>%
  count(Role) %>%  # Changed to 'Role'
  arrange(desc(n))

# Optional: Plot the overall job roles
ggplot(jobrole_overall, aes(x = reorder(Role, n), y = n)) +
  geom_bar(stat = "identity", fill = "#33a02c") +
  coord_flip() +
  labs(title = "Overall Job Roles", x = "Job Role", y = "Count")




## 2iv: Employment Rate Analysis (By Study Field)

employment_rate <- data_clean %>%
  mutate(Employed = ifelse(str_detect(Employment, "Employed"), "Yes", "No")) %>%
  group_by(StudyField, Employed) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Percentage = n / sum(n) * 100)

ggplot(employment_rate, aes(x = StudyField, y = Percentage, fill = Employed)) +
  geom_col() +
  labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Percentage (%)")















