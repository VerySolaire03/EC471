library(tidyverse)
library(dplyr)

hdi_data <- hdr_data %>%
  filter(indicatorCode == "hdi") %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))

gnipc_data <- hdr_data %>%
  filter(indicatorCode == "gnipc") %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))

eys_data <- hdr_data %>%
  filter(indicatorCode == "eys") %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))

le_data <- hdr_data %>%
  filter(indicatorCode == "le") %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))

mys_data <- hdr_data %>%
  filter(indicatorCode == "mys") %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))

hdi_rank <- hdr_data %>%
  filter(indicatorCode == "hdi_rank")

ggplot(hdi_data, aes(x = year, y = value, color = countryIsoCode, group = countryIsoCode)) +
  geom_line() +
  labs(title = "HDI Over Time", x = "Year", y = "Human Development Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(gnipc_data, aes(x = year, y = value, color = countryIsoCode, group = countryIsoCode)) +
  geom_line() +
  labs(title = "GNIPC Over Time", x = "Year", y = "Gross National Income Per Capita") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(eys_data, aes(x = year, y = value, color = countryIsoCode, group = countryIsoCode)) +
  geom_line() +
  labs(title = "EYS Over Time", x = "Year", y = "Expected Years of Schooling") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(le_data, aes(x = year, y = value, color = countryIsoCode, group = countryIsoCode)) +
  geom_line() +
  labs(title = "Life Expectancy Over Time", x = "Year", y = "Life Expectancy at Birth") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(mys_data, aes(x = year, y = value, color = countryIsoCode, group = countryIsoCode)) +
  geom_line() +
  labs(title = "MYS Over Time", x = "Year", y = "Mean Years of Schooling") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


gini_data <- API_SI_POV_GINI_DS2_en_excel_v2_211 %>%
  filter(country_code %in% c("ARG", "BOL", "ECU", "PER", "VEN"))

data_long <- pivot_longer(gini_data, cols = starts_with("19") | starts_with("20"),
                          names_to = "year", values_to = "gini_value")
data_long$year <- as.numeric(data_long$year)
data_long <- data_long %>%
  filter(year >= 1980)
data_long$gini_coeff <- data_long$gini_value/100

ggplot(data_long, aes(x = year, y = gini_coeff, color = country_code, group = country_code)) +
  geom_line() +
  labs(title = "Gini Coefficient Over Time", x = "Year", y = "Gini Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


growth_data <- API_NY_GDP_MKTP_KD_ZG_DS2_en_excel_v2_73467 %>%
  filter(country_code %in% c("ARG", "BOL", "ECU", "PER", "VEN"))
growth_data <- pivot_longer(growth_data, cols = starts_with("19") | starts_with("20"),
                            names_to = "year", values_to = "growth_rate")
growth_data$year <- as.numeric(growth_data$year)
growth_data <- growth_data %>%
  filter(year >= 1980)

growth_wash <- growth_data %>%
  filter(country_code %in% c("ARG", "VEN"))
growth_data_buen.vivir <- growth_data %>%
  filter(country_code %in% c("BOL", "ECU"))
growth_wash1 <- growth_wash %>%
  filter(year <= 2000)
ggplot(growth_wash1, aes(x = year, y = growth_rate, color = country_code, group = country_code)) +
  geom_line() +
  labs(title = "Growth Rate Over Time", x = "Year", y = "GDP Growth Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(growth_data, aes(x = year, y = growth_rate, color = country_code, group = country_code)) +
  geom_line() +
  labs(title = "Growth Rate Over Time", x = "Year", y = "GDP Growth Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(growth_data_buen.vivir, aes(x = year, y = growth_rate, color = country_code, group = country_code)) +
  geom_line() +
  labs(title = "Growth Rate Over Time", x = "Year", y = "GDP Growth Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


phdi_data <- hdr_data_2 %>%
  filter(indicatorCode == "phdi") %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))

ihdi_data <- hdr_data_2 %>%
  filter(indicatorCode == "ihdi") %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))


ggplot(phdi_data, aes(x = year, y = value, color = countryIsoCode, group = countryIsoCode)) +
  geom_line() +
  labs(title = "PHDI Over Time", x = "Year", y = "Planetary-Pressures Adjusted HDI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(ihdi_data, aes(x = year, y = value, color = countryIsoCode, group = countryIsoCode)) +
  geom_line() +
  labs(title = "IHDI Over Time", x = "Year", y = "Inequality Adjusted HDI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


rank_diff <- hdr_data_2 %>%
  filter(indicatorCode == "rankdiff_hdi_phdi") %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))

diff_hdi_phdi <- hdr_data_2 %>%
  filter(indicatorCode == "diff_hdi_phdi") %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))

ggplot(diff_hdi_phdi, aes(x = year, y = value, color = countryIsoCode, group = countryIsoCode)) +
  geom_line() +
  labs(title = "Difference between HDI and PHDI Over Time", x = "Year", y = "Difference as percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


gdp_pc <- API_NY_GDP_PCAP_CD_DS2_en_excel_v2_193_2 %>%
  filter(country_code %in% c("ARG", "BOL", "ECU", "PER", "VEN"))
gdppc_data <- pivot_longer(gdp_pc, cols = starts_with("19") | starts_with("20"),
                            names_to = "year", values_to = "gdppc")
gdppc_data$year <- as.numeric(gdppc_data$year)
gdppc_data <- gdppc_data %>%
  filter(year >= 1980)

ggplot(gdppc_data, aes(x = year, y = gdppc, color = country_code, group = country_code)) +
  geom_line() +
  labs(title = "GDP Per Capita Over Time", x = "Year", y = "GDP Per Capita") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


hdi_general <- hdr_data %>%
  filter(indicatorCode == "hdi")
phdi_general <- hdr_data_2 %>%
  filter(indicatorCode == "phdi")

data_ranked <- hdi_general %>%
  group_by(year) %>%                 
  mutate(rank = rank(-value, ties.method = "first")) %>%  
  arrange(year, rank) 

data_ranked3 <- data_ranked %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))
data_ranked4 <- data_ranked2 %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))

combined_data <- data_ranked3 %>%
  inner_join(data_ranked4, by = c("countryIsoCode", "year")) %>% # Join datasets on shared keys
  group_by(countryIsoCode, year)
combined_data$difference <- combined_data$rank.x - combined_data$rank.y
combined_data$diff.in.value <- combined_data$value.x - combined_data$value.y
ggplot(combined_data, aes(x = year, y = difference, color = countryIsoCode, group = countryIsoCode)) +
  geom_line() +
  labs(title = "Rank Difference between HDI and OHDI", x = "Year", y = "Difference") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot(combined_data, aes(x = year, y = diff.in.value, color = countryIsoCode, group = countryIsoCode)) +
  geom_line() +
  labs(title = "Value difference between HDI and PHDI", x = "Year", y = "Difference") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

data_ranked2 <- phdi_general %>%
  group_by(year) %>%
  mutate(rank = rank(-value, ties.method = "first")) %>%
  arrange(year, rank)

merged_df <- merge(data_ranked, data_ranked2, by = "countryIsoCode")
merged_df <- merged_df %>%
  filter(countryIsoCode %in% c("ARG", "BOL", "ECU", "PER","VEN"))