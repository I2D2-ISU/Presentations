library(tidyverse)

# Low Birth Weight --------------------------------------------------------

low_birth_weight <- read_csv("data/low-birth-weight.csv", lazy = FALSE)

lbw_eci <- tibble::tribble(
             ~year,   ~eci,
             2000L,   "6.13",
             2001L,   "6.42",
             2002L,   "6.64",
             2003L,   "6.61",
             2004L,   "7.01",
             2005L,   "7.19",
             2006L,   "6.95",
             2007L,   "6.84",
             2008L,   "6.65",
             2009L,   "6.74",
             2010L,   "6.98",
             2011L,   "6.46",
             2012L,   "6.61",
             2013L,   "6.56",
             2014L,   "6.76",
             2015L,   "6.77",
             2016L,   "6.76",
             2017L,   "6.58",
             2018L,   "6.91",
             2019L,   "6.77",
             2020L,   "6.96"
             )


low_birth_weight %>%
  filter(fips == 19) %>%
  inner_join(lbw_eci) %>%
  mutate(idd = round(lbw_percent*100, 2)) %>%
  select(year, eci, idd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)/100) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.2f", value*100)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "Low Birth Weight",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)
  
ggsave("images/IDD-vs-ECI-2022/low_birth_weight.png", height = 5, width = 9)


# Immunized Children ------------------------------------------------------

immunization <- read_csv("data/child-immunization.csv", lazy = FALSE)

imm_eci <- tibble::tribble(
             ~year,   ~eci,
             2011L,  "64.0",
             2012L,  "71.0",
             2013L,  "71.0",
             2014L,  "69.0",
             2015L,  "67.0",
             2016L,  "69.0",
             2017L,  "70.0",
             2018L,  "74.0",
             2019L,  "74.9",
             2020L, "72.40"
             )


immunization %>%
  filter(fips == 19, population_source == "IRIS") %>%
  inner_join(imm_eci) %>%
  mutate(idd = round(immunized_percent*100, 2)) %>%
  select(year, eci, idd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)/100) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.1f", value*100)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "Immunized Children",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/immunization.png", height = 5, width = 9)
  



# Dental Services ---------------------------------------------------------

dental_service <- read_csv("data/child-dental-service.csv", lazy = FALSE)

dnt_eci <- tibble::tribble(
            ~year,   ~count, ~total,  ~eci,
             2014L,  50813,  108074, "47.0",
             2015L,  52861,  109307, "48.4",
             2016L,  53025,  110602, "47.9",
             2017L,  54277,  110576, "49.1",
             2018L,  53962,  108150, "49.9",
             2019L,  53585,  107400, "49.9",
             2020L,  39609,  103889, "38.1"
             )


dental_service %>%
  filter(fips == 19) %>%
  inner_join(dnt_eci) %>%
  mutate(idd = round(service_received_percent*100, 2)) %>%
  select(year, eci, idd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)/100) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.1f", value*100)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "Dental Services",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/dental_service.png", height = 5, width = 9)



# Infant Mortality --------------------------------------------------------

infant_mortality <- read_csv("data/infant-mortality.csv", lazy = FALSE)

infant_mortality %>%
  filter(fips == 19) %>%
  select(year, idd = infant_mortality_rate, eci = year_range) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)/100) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.2f", value*100)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "Infant Mortality",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/infant_mortality.png", height = 5, width = 9)



# Early Literacy Skills ---------------------------------------------------

k_assessment <- read_csv("data/k-assessment.csv", lazy = FALSE)

kas_eci <- tibble::tribble(
             ~year, ~total, ~count,
             2015L,  37615,  24340,
             2016L,  37567,  25409,
             2017L,  37880,  26056,
             2018L,  38592,  23708,
             2019L,  38562,  27683,
             2020L,  36112,  23965
             ) %>%
  mutate(eci = count/total*100)


k_assessment %>%
  filter(fips == 19) %>%
  inner_join(kas_eci) %>%
  mutate(idd = round(percent_met_benchmark*100, 2)) %>%
  select(year, eci, idd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)/100) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.1f", value*100)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "Early Literacy Skills",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/k_assessment.png", height = 5, width = 9)



# Educational Attainment of Mothers ---------------------------------------

maternal_education <- read_csv("data/maternal-education.csv", lazy = FALSE)

# maternal_education %>%
#   filter(fips == 19, marital_status == "Both") %>%
#   mutate(edu = recode(education, 
#                       "Less than High School Graduate" = "Less",
#                       "High School Graduate" = "High",
#                       "Some College or Associate's Degree" = "Some",
#                       "Graduate or Professional Degree" = "Graduate",
#                       "Bachelor's Degree" = "BA")) %>%
#   select(year, edu, count) %>%
#   spread(edu, count) %>%
#   mutate(Indicator = "Educational Attainment of Mothers") %>%
#   select(Indicator, 
#          Year = year,
#          Less, High, Some, BA, Graduate) %>%
#   
#   inner_join(kas_eci) %>%
#   mutate(idd = round(percent_met_benchmark*100, 2)) %>%
#   select(year, eci, idd) %>%
#   gather(key, value, -year) %>%
#   filter(year > 2010) %>%
#   mutate(year = as.factor(year),
#          key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
#          value = as.numeric(value)/100) %>%
#   ggplot(aes(x = year, y = value, fill = key)) +
#   geom_bar(stat = "identity", position = position_dodge(), col = "black") +
#   geom_text(aes(label = sprintf("%2.1f", value*100)), position = position_dodge(width = 0.9), vjust = -0.25) +
#   labs(x = NULL,
#        y = NULL,
#        title = "Early Literacy Skills",
#        fill = NULL) +
#   scale_fill_brewer(palette = 15) +
#   scale_y_continuous(labels = scales::percent) +
#   ggthemes::theme_hc() +
#   theme(axis.ticks = element_blank(),
#         axis.text = element_text(size = 12)) +
#   coord_cartesian(expand = T)



# Child Abuse -------------------------------------------------------------

child_abuse <- read_csv("data/child-abuse-age.csv")

cha_eci <- tibble::tribble(
             ~year,  ~eci,
             2011L,  24.6,
             2012L,  24.3,
             2013L, 24.82,
             2014L, 14.96,
             2015L, 16.88,
             2016L, 18.77,
             2017L, 21.97,
             2018L, 22.39,
             2019L, 21.96,
             2020L, 20.56
             )


child_abuse %>%
  filter(fips == 19, age == "under_6") %>%
  inner_join(cha_eci) %>%
  mutate(idd = round(child_abuse_per_1000, 2)) %>%
  select(year, eci, idd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.2f", value)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "Child Abuse",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/child_abuse.png", height = 5, width = 9)




# Teen Births -------------------------------------------------------------

teen_birth <- read_csv("data/teen-birth.csv", lazy = FALSE)

tnb_eci <- tibble::tribble(
             ~year, ~eci,
             2011L,  7.1,
             2012L,  6.5,
             2013L,  5.9,
             2014L,  5.2,
             2015L,  4.1,
             2016L,  4.6,
             2017L,  4.4,
             2018L,  4.3,
             2019L,  3.9,
             2020L,  3.9
             )


teen_birth  %>%
  filter(fips == 19) %>%
  inner_join(tnb_eci) %>%
  mutate(idd = round(live_birth_rate, 2)) %>%
  select(year, eci, idd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI - Percentage", "IDD - Rate per 100,000")),
         value = as.numeric(value)/100) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.1f", value*100)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "Teen Births",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/teen_birth.png", height = 5, width = 9)




# Domestic Violence -------------------------------------------------------

domestic_violence_rate <- read_csv("data/domestic-violence-rate-IUCR.csv", lazy = FALSE)

dvr_eci <-tibble::tribble(
            ~year, ~eci_count,  ~eci,
            2009L, 6341L,   211,
            2010L, 7218L, 264.9,
            2011L, 6823L, 250.3,
            2012L, 6636L, 249.1,
            2013L, 6680L, 230.7,
            2014L, 6473L, 225.6,
            2015L, 6408L, 210.3,
            2016L, 6431L, 203.6,
            2017L, 6238L, 196.9,
            2018L, 5762L, 181.3,
            2019L, 5897L, 186.7,
            2020L, 6177L, 194.4
            )


domestic_violence_rate %>%
  filter(fips == 19) %>%
  inner_join(dvr_eci) %>%
  mutate(idd = round(domestic_violence_rate, 2)) %>%
  select(year, eci, idd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.1f", value)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "Domestic Violence Rate",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/domestic_violence_rate.png", height = 5, width = 9)




# Unemployment Rate -------------------------------------------------------

unemployment <- read_csv("data/unemployment-annual.csv", lazy = FALSE)

ump_eci <- tibble::tribble(
             ~year, ~eci,
             2010L,  6.1,
             2011L,  5.6,
             2012L,  5.1,
             2013L,  4.7,
             2014L,  4.2,
             2015L,  3.7,
             2016L,  3.6,
             2017L,  3.1,
             2018L,  2.6,
             2019L,  2.8,
             2020L,  5.3
             )


unemployment %>%
  filter(fips == 19) %>%
  inner_join(ump_eci) %>%
  mutate(idd = round(unemployment_rate*100, 2)) %>%
  select(year, eci, idd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)/100) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.1f", value*100)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "Unemployment Rate",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/unemployment.png", height = 5, width = 9)




# Children in Poverty -----------------------------------------------------

child_poverty <- read_csv("data/child-poverty.csv", lazy = FALSE)

chp_eci <- tibble::tribble(
             ~year,  ~eci,
             2010L, "19",
             2011L, "19",
             2012L, "20",
             2013L, "17",
             2014L, "16",
             2015L, "16",
             2016L, "18",
             2017L, "15",
             2018L, "15",
             2019L, "16"
             )


child_poverty %>%
  filter(fips == 19, race_ethnicity == "All") %>%
  inner_join(chp_eci) %>%
  mutate(idd = round(population_under_6_below_poverty_percent * 100, 1)) %>%
  select(year, eci, idd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)/100) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.1f", value*100)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "Children in Poverty",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/child_poverty.png", height = 5, width = 9)




# Children with Health Coverage by Income Level ---------------------------

child_health_coverage <- read_csv("data/child-health-coverage.csv")

# child_health_coverage %>%
#   filter(fips == 19) %>%
#   mutate(income = 
#            recode(income,
#                   "All Incomes" = "All",
#                   "<= 200% of Poverty" = "FPL_200")) %>%
#   select(year, income, insured, insured_percent) %>%
#   transmute(Indicator = "Health Coverage",
#             Year = year,
#             Income = income,
#             Count = insured,
#             Percent = sprintf("%2.1f%%", round(insured_percent * 100, 1))) %>%
#   pivot_wider(names_from = Income, values_from = c(Count, Percent)) 


# All People in Poverty ---------------------------------------------------

all_people_poverty <- read_csv("data/child-poverty.csv", lazy = FALSE)

all_people_poverty %>%
  filter(fips == 19, race_ethnicity == "All") %>%
  mutate(idd = round(population_total_below_poverty_percent * 100, 1),
         eci = NA) %>%
  select(year, eci, idd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)/100) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.1f", value*100)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "All People in Poverty",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/all_people_poverty.png", height = 5, width = 9)




# Availability of Child Care  ---------------------------------------------

childcare_availability <- read_csv("data/childcare-availability.csv", lazy = FALSE)

cca_eci <- tibble::tribble(
             ~year,   ~lice,   ~reg,
             2011L,   89776,  49908,
             2012L,   90982,  46296,
             2013L,   90567,  41784,
             2014L,   95868,  37984,
             2015L,  107566,  35184,
             2016L,  116208,  34116,
             2017L,  118551,  30716,
             2018L,  124398,  28688,
             2019L,  128880,  27764,
             2020L,  130287,  26796,
             2021L,  132173,  25704
             )

  

childcare_availability %>%
  filter(fips == 19) %>%
  select(year = year,
         licensed = childcare_centers_preschools,
         registerd = child_development_homes) %>%
  inner_join(cca_eci) %>%
  select(year, eci = lice, idd = licensed) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.numeric(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_area(alpha = 0.35, position = "identity") +
  geom_line(aes(col =  I("black")), size = 1, alpha = 0.7, show.legend = FALSE) +
  labs(x = NULL,
       y = NULL,
       title = "Availability of Child Care (Licensed)",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::label_comma(), breaks = seq(0, 160000, 20000)) +
  scale_x_continuous(n.breaks = 15) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/childcare_availability_licensed.png", height = 5, width = 9)



childcare_availability %>%
  filter(fips == 19) %>%
  select(year = year,
         licensed = childcare_centers_preschools,
         registerd = child_development_homes) %>%
  inner_join(cca_eci) %>%
  select(year, eci = reg, idd = registerd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.numeric(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_area(alpha = 0.35, position = "identity") +
  geom_line(aes(col =  I("black")), size = 1, alpha = 0.7, show.legend = FALSE) +
  labs(x = NULL,
       y = NULL,
       title = "Availability of Child Care (Registered)",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::label_comma(), breaks = seq(0, 60000, 10000)) +
  scale_x_continuous(n.breaks = 15) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/childcare_availability_registerd.png", height = 5, width = 9)




# Working Parents ---------------------------------------------------------

working_parents <- read_csv("data/working-parents-under-6.csv", lazy = FALSE)

wrk_eci <- tibble::tribble(
             ~year,  ~eci,
             2010L, 0.76,
             2011L, 0.74,
             2012L, 0.74,
             2013L, 0.77,
             2014L, 0.76,
             2015L, 0.75,
             2016L, 0.75,
             2017L, 0.75,
             2018L, 0.75,
             2019L, 0.75
             )


working_parents %>%
  filter(fips == 19) %>%
  inner_join(wrk_eci) %>%
  mutate(idd = round(parents_working_all_percent, 3)) %>%
  select(year, eci, idd) %>%
  gather(key, value, -year) %>%
  filter(year > 2010) %>%
  mutate(year = as.factor(year),
         key = factor(key, levels = c("eci", "idd"), labels = c("ECI", "IDD")),
         value = as.numeric(value)) %>%
  ggplot(aes(x = year, y = value, fill = key)) +
  geom_bar(stat = "identity", position = position_dodge(), col = "black") +
  geom_text(aes(label = sprintf("%2.1f", value*100)), position = position_dodge(width = 0.9), vjust = -0.25) +
  labs(x = NULL,
       y = NULL,
       title = "Working Parents",
       fill = NULL) +
  scale_fill_brewer(palette = 15) +
  scale_y_continuous(labels = scales::percent) +
  ggthemes::theme_hc() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 12)) +
  coord_cartesian(expand = T)

ggsave("images/IDD-vs-ECI-2022/working_parents.png", height = 5, width = 9)




