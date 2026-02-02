########------- PACKAGES -------########

library(ggplot2)
library(ggalluvial)
library(alluvial)
library(readr)
library(genderizeR)
#library(gender)
library(DemografixeR)
library(dplyr)
library(tidyr)
library(readxl)
library(wordcloud2)
library(tidytext)
library(broom)
library(purrr)

#######---------- DATA ----------########

setwd("C:/Users/sbiwp1/OneDrive - Cardiff University/NoWPaS review paper")

NoWPaS_df<- read_excel("NoWPaS talk characterisation.v2.xlsx", 
                                              sheet = "Sheet1")

#rename columns
colnames(NoWPaS_df)[1] <- "AbstractID"
colnames(NoWPaS_df)[7] <- "Gender"
colnames(NoWPaS_df)[9] <- "Academia"
colnames(NoWPaS_df)[24] <- "Trait"

#remove keynotes
NoWPaS_df <- NoWPaS_df %>%
  filter(Keynote == "N")

#abstract number per year
abstracts_per_year <- NoWPaS_df %>%
  distinct(AbstractID, Year) %>%
  count(Year, name = "N_abstracts")
plot(abstracts_per_year, type="l")
abline(lm(N_abstracts ~ Year, data = abstracts_per_year), col="red")
summary(lm(N_abstracts ~ Year, data = abstracts_per_year))



#######--------- GENDER ---------########

# Names <- NoWPaS_df[, c("Speaker","Year")]
# #Names <-unique(Names)
# Names$First_name <- sub(" .*", "", Names$Speaker)

NoWPaS_df$First_name <- sub(" .*", "", NoWPaS_df$Speaker)

# givenNames = findGivenNames(NoWPaS_df$First_name, ssl.verifypeer = FALSE,apikey ="feee483c4b42823dd446bdab34e2a0b4")
# givenNames <- givenNames %>%
#   rename(
#     First_name = gender,
#     gender = probability,
#     probability = count,
#     count = name
#   )
# givenNames$name<-as.numeric(givenNames$name)
# write.csv(givenNames,"givenNames.csv")

givenNames <- read_csv("givenNames.csv")
givenNames$...1<-NULL
givenNames<-unique(givenNames)

# gender <- gender(NoWPaS_df$First_name,
#                          method = "ssa",
#                          years = c(2005, 2012))

NoWPaS_df$First_name <- tolower(NoWPaS_df$First_name)
NoWPaS_df$First_name <- sub("-.*", "", NoWPaS_df$First_name)

NoWPaS_df<- merge(givenNames, NoWPaS_df, by="First_name",all.y = TRUE)

NoWPaS_df <- NoWPaS_df %>%
  mutate(gender = case_when(
    grepl("caliyena", First_name, ignore.case = TRUE) ~ "female",
    grepl("joshka",  First_name, ignore.case = TRUE) ~ "male",
    TRUE ~ gender  # keep existing values
  ))


ggplot(NoWPaS_df, aes(x = Year, fill = gender)) +
  geom_bar(position="fill") +
  theme_minimal()

sex_ratio_df <- NoWPaS_df %>%
  filter(!is.na(gender)) %>%           # ignore NAs
  group_by(Year) %>%
  summarise(
    males   = sum(gender == "male"),
    females = sum(gender == "female"),
    sex_ratio = males / females        # male : female ratio
  )

ggplot(sex_ratio_df, aes(x = Year, y = sex_ratio)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # horizontal line at 0
  labs(
    x = "Year",
    y = "Gender ratio (male:female)"
  ) +
  scale_x_continuous(breaks = sex_ratio_df$Year) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),  # x-axis text
    axis.text.y = element_text(size = 14),  # y-axis text
    axis.title.x = element_text(size = 16), # x-axis label
    axis.title.y = element_text(size = 16), # y-axis label
    plot.title = element_text(size = 18, face = "bold") # plot title if added
  )

## Amaia : based on database Gender column
sex_ratio <- NoWPaS_df %>%
  filter(Gender %in% c("M", "F")) %>%       # keep only valid cases
  group_by(Year) %>%
  summarise(
    nM = sum(Gender == "M"),
    nF = sum(Gender == "F"),
    ratio = nM / nF
  )
ggplot(sex_ratio, aes(x = Year, y = ratio)) +
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    x = "Year",
    y = "Male : Female ratio"
  ) +
  theme_minimal()


########--------- DISCIPLINES ---------########

discipline_long <- NoWPaS_df %>%
  separate_rows(Discipline_decider, sep = ";\\s*") %>%
  mutate(Discipline_decider = trimws(Discipline_decider)) %>% 
  filter(!is.na(Discipline_decider) & Discipline_decider != "NA")
table(discipline_long$Discipline_decider)

discipline_long$Discipline_decider[which(discipline_long$Discipline_decider == "Genetics (PopGen)")] <- "Genetics"
discipline_long$Discipline_decider <- factor(discipline_long$Discipline_decider)
levels(discipline_long$Discipline_decider) <- c("Behavioural ecology", "Disease and parasitology", "Ecology", 
                                                "Ecotoxicology", "Evolutionary biology", "Genetics and genomics", 
                                                "Habitat use", "Hydro-geomorphology", "Movement ecology", "Physiology")
sort(table(discipline_long$Discipline_decider) * 100 / nrow(discipline_long))

#stacked barplot
ggplot(discipline_long, aes(x = Year, fill = Discipline_decider)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "Proportion",
    fill = "Discipline"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")

#lines (relative proportion)
discipline_long %>%
  group_by(Year, Discipline_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Discipline_decider, fill = list(n = 0)) %>%
  group_by(Year) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = Year, y = prop, color = Discipline_decider)) +
  geom_point(size = 1, show.legend = F) +
  geom_line(size = 1, alpha=0.5, show.legend = F) +
  geom_smooth(aes(fill = Discipline_decider), se = T, linewidth = 1, show.legend = F)+
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "Proportion",
    color = "Discipline"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~ Discipline_decider, scales = "fixed")


#absolute numbers
discipline_long %>%
  group_by(Year, Discipline_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Discipline_decider, fill = list(n = 0)) %>%
  ggplot(aes(x = Year, y = n, color = Discipline_decider)) +
  geom_point(size = 1, show.legend = F) +
  geom_line(size = 1, alpha = 0.5, show.legend = F) +
  geom_smooth(aes(fill = Discipline_decider), se = T, linewidth = 1, show.legend = F)+
  labs(
    y = "Number of studies",
    color = "Discipline"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~ Discipline_decider, scales = "fixed")

#stats
#cannot do multinomial model for proportions because disciplines are not exclusive
#so glm binomial for each discipline separately: what is the probability of each discipline over time?
discipline_ts <- discipline_long %>%
  group_by(Year, Discipline_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Discipline_decider, fill = list(n = 0)) %>%
  group_by(Year) %>%
  mutate(N = sum(n)) %>%
  ungroup()
discipline_ts$Year_scaled <- scale(discipline_ts$Year, center = TRUE, scale = TRUE)

discipline_ts <- discipline_ts %>%
  left_join(abstracts_per_year, by = "Year") #need the number of abstracts as the total max number of occurence of the discipline

glm_results <- discipline_ts %>%
  group_by(Discipline_decider) %>%
  group_modify(~ {
    
    m <- glm(
      cbind(n, N_abstracts - n) ~ Year_scaled,
      family = quasibinomial(),   # safer default
      data = .x
    )
    
    tidy(m) %>%
      filter(term == "Year_scaled") %>%
      mutate(
        dispersion = deviance(m) / df.residual(m),
        n_years = n_distinct(.x$Year)
      )
  }) %>%
  ungroup()
glm_results #Estimates are log-odds change per 1 SD increase in year
glm_results %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

#or rather using counts
counts_ts <- discipline_long %>%
  group_by(Year, Discipline_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Discipline_decider, fill = list(n = 0))

trend_pois <- counts_ts %>%
  group_by(Discipline_decider) %>%
  do({
    m <- glm(n ~ scale(Year), family = poisson, data = .)
    tidy(m)
  }) %>%
  filter(term == "scale(Year)")
trend_pois <- trend_pois %>%
  ungroup() %>%
  mutate(
    p_adj = p.adjust(p.value, method = "BH"),
    direction = case_when(
      estimate > 0 & p_adj < 0.05 ~ "Increasing",
      estimate < 0 & p_adj < 0.05 ~ "Decreasing",
      TRUE ~ "Stable"
    )
  )
trend_pois

check_dispersion <- counts_ts %>%
  group_by(Discipline_decider) %>%
  do({
    m <- glm(n ~ scale(Year), family = poisson, data = .)
    disp <- sum(residuals(m, type = "pearson")^2) / df.residual(m)
    data.frame(dispersion = disp)
  })
check_dispersion

########---------- DRIVERS ---------########
driver_long <- NoWPaS_df %>%
  separate_rows(Driver_decider, sep = ";\\s*") %>%
  mutate(Driver_decider = trimws(Driver_decider)) %>% 
  filter(!is.na(Driver_decider) & Driver_decider != "NA")
sort(table(driver_long$Driver_decider))/ nrow(driver_long) * 100

#stacked barplot
ggplot(driver_long, aes(x = Year, fill = Driver_decider)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "Proportion",
    fill = "Driver"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")

#lines (relative proportion)
driver_long %>%
  group_by(Year, Driver_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Driver_decider, fill = list(n = 0)) %>%
  group_by(Year) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = Year, y = prop, color = Driver_decider)) +
  geom_point(size = 1, show.legend = F) +
  geom_line(size = 1, alpha=0.5, show.legend = F) +
  geom_smooth(aes(fill = Driver_decider), se = T, linewidth = 1, show.legend = F)+
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "Proportion",
    color = "Driver"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~ Driver_decider, scales = "fixed")

#absolute numbers
driver_long %>% 
  group_by(Year, Driver_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Driver_decider, fill = list(n = 0)) %>%
  ggplot(aes(x = Year, y = n, color = Driver_decider)) +
  geom_point(size = 1, show.legend = F) +
  geom_line(size = 1, alpha = 0.5, show.legend = F) +
  geom_smooth(aes(fill = Driver_decider), se = T, linewidth = 1, show.legend = F)+
  labs(
    y = "Number of studies",
    color = "Driver"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~ Driver_decider, scales = "fixed")


#stats
#cannot do multinomial model for proportions because disciplines are not exclusive
#so glm binomial for each discipline separately: what is the probability of each discipline over time?
drivers_ts <- driver_long %>%
  group_by(Year, Driver_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Driver_decider, fill = list(n = 0)) %>%
  group_by(Year) %>%
  mutate(N = sum(n)) %>%
  ungroup()
drivers_ts$Year_scaled <- scale(drivers_ts$Year, center = TRUE, scale = TRUE)

drivers_ts <- drivers_ts %>%
  left_join(abstracts_per_year, by = "Year") #need the number of abstracts as the total max number of occurence of the discipline

glm_results <- drivers_ts %>%
  group_by(Driver_decider) %>%
  group_modify(~ {
    
    m <- glm(
      cbind(n, N_abstracts - n) ~ Year_scaled,
      family = quasibinomial(),   # safer default
      data = .x
    )
    
    tidy(m) %>%
      filter(term == "Year_scaled") %>%
      mutate(
        dispersion = deviance(m) / df.residual(m),
        n_years = n_distinct(.x$Year)
      )
  }) %>%
  ungroup()
glm_results #Estimates are log-odds change per 1 SD increase in year
glm_results %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

#stats using counts
counts_ts <- driver_long %>%
  group_by(Year, Driver_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Driver_decider, fill = list(n = 0))

trend_pois <- counts_ts %>%
  group_by(Driver_decider) %>%
  do({
    m <- glm(n ~ scale(Year), family = poisson, data = .)
    tidy(m)
  }) %>%
  filter(term == "scale(Year)")
trend_pois <- trend_pois %>%
  ungroup() %>%
  mutate(
    p_adj = p.adjust(p.value, method = "BH"),
    direction = case_when(
      estimate > 0 & p_adj < 0.05 ~ "Increasing",
      estimate < 0 & p_adj < 0.05 ~ "Decreasing",
      TRUE ~ "Stable"
    )
  )
trend_pois

check_dispersion <- counts_ts %>%
  group_by(Driver_decider) %>%
  do({
    m <- glm(n ~ scale(Year), family = poisson, data = .)
    disp <- sum(residuals(m, type = "pearson")^2) / df.residual(m)
    data.frame(dispersion = disp)
  })
check_dispersion

########---------- METHODS ---------########

broad_method_long <- NoWPaS_df %>%
  separate_rows(Method_decider_broad, sep = ";") %>%
  mutate(Method_decider_broad = trimws(Method_decider_broad))%>% 
  filter(!is.na(Method_decider_broad) & Method_decider_broad != "NA")
sort(table(broad_method_long$Method_decider_broad)) / nrow(broad_method_long) * 100

#stacked barplot
ggplot(broad_method_long, aes(x = Year, fill = Method_decider_broad)) +
  geom_bar(position="fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "Proportion",
    fill = "Method"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")

#lines (relative proportion)
broad_method_long %>% 
  group_by(Year, Method_decider_broad) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Method_decider_broad, fill = list(n = 0)) %>%
  group_by(Year) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = Year, y = prop, color = Method_decider_broad)) +
  geom_point(size = 1, show.legend = F) +
  geom_line(size = 1, alpha=0.5, show.legend = F) +
  geom_smooth(aes(fill = Method_decider_broad), se = T, linewidth = 1, show.legend = F)+
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    y = "Proportion",
    color = "Method"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~ Method_decider_broad, scales = "fixed")

#absolute numbers
broad_method_long %>% 
  group_by(Year, Method_decider_broad) %>%
  summarise(n = n(), .groups= "drop") %>%
  complete(Year, Method_decider_broad, fill = list(n = 0)) %>%
  ggplot(aes(x = Year, y = n, color = Method_decider_broad)) +
  geom_point(size = 1, show.legend = F) +
  geom_line(size = 1, alpha=0.5, show.legend = F) +
  geom_smooth(aes(fill = Method_decider_broad), se = T, linewidth = 1, show.legend = F)+
  labs(
    y = "Number of studies",
    color = "Method"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~ Method_decider_broad, scales = "fixed")


#stats
#cannot do multinomial model for proportions because disciplines are not exclusive
#so glm binomial for each discipline separately: what is the probability of each discipline over time?
method_ts <- broad_method_long %>%
  group_by(Year, Method_decider_broad) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Method_decider_broad, fill = list(n = 0)) %>%
  group_by(Year) %>%
  mutate(N = sum(n)) %>%
  ungroup()
method_ts$Year_scaled <- scale(method_ts$Year, center = TRUE, scale = TRUE)

method_ts <- method_ts %>%
  left_join(abstracts_per_year, by = "Year") #need the number of abstracts as the total max number of occurence of the discipline

glm_results <- method_ts %>%
  group_by(Method_decider_broad) %>%
  group_modify(~ {
    
    m <- glm(
      cbind(n, N_abstracts - n) ~ Year_scaled,
      family = quasibinomial(),   # safer default
      data = .x
    )
    
    tidy(m) %>%
      filter(term == "Year_scaled") %>%
      mutate(
        dispersion = deviance(m) / df.residual(m),
        n_years = n_distinct(.x$Year)
      )
  }) %>%
  ungroup()
glm_results #Estimates are log-odds change per 1 SD increase in year
glm_results %>%
  mutate(p_adj = p.adjust(p.value, method = "BH"))

#stats using counts
counts_ts <- broad_method_long %>%
  group_by(Year, Method_decider_broad) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Method_decider_broad, fill = list(n = 0))
trend_pois <- counts_ts %>%
  group_by(Method_decider_broad) %>%
  do({
    m <- glm(n ~ scale(Year), family = poisson, data = .)
    tidy(m)
  }) %>%
  filter(term == "scale(Year)")
trend_pois <- trend_pois %>%
  ungroup() %>%
  mutate(
    p_adj = p.adjust(p.value, method = "BH"),
    direction = case_when(
      estimate > 0 & p_adj < 0.05 ~ "Increasing",
      estimate < 0 & p_adj < 0.05 ~ "Decreasing",
      TRUE ~ "Stable"
    )
  )
trend_pois

########---------- TRAITS ---------########
traits_long <- NoWPaS_df %>%
  separate_rows(Trait, sep = ";\\s*") %>%
  mutate(Trait = trimws(Trait))%>% 
  filter(!is.na(Trait) & Trait != "NA")
table(traits_long$Trait)

traits_long$Trait[which(traits_long$Trait %in% c("Behaviour (aggression)",
                                                "Behaviour (boldness)",
                                                "Behaviour (competition)",
                                                "Behaviour (exploration, boldness, aggression)",
                                                "Behaviour (exploration)",
                                                "Behaviour (feeding)",
                                                "Behaviour (spawning)"))] <- "Growth"

traits_long$Trait[which(traits_long$Trait == "Dispersal")] <- "Straying"
traits_long$Trait[which(traits_long$Trait %in% c("Habitat niche", "Habitat use (spawning)"))] <- "Habitat use"
traits_long$Trait[which(traits_long$Trait %in% c("Microbiome (gill)", "Microbiome (gut)"))] <- "Microbiome"
traits_long$Trait[which(traits_long$Trait %in% c("Timing (emergence)", "Timing (hatching)", "Timing (migration)"))] <- "Timing"

traits_freq <- traits_long %>%
  count(Trait, sort = TRUE)
wordcloud2(data = traits_freq, 
           #figPath = "Salmo.png",
           size = 1,
           backgroundColor = "white")



########---------- SANKY PLOT ---------########
species_country <- NoWPaS_df
species_country <- species_country %>%
  separate_rows(Focus_species, sep = ";") %>%
  separate_rows(Country_of_study_or_author, sep = ";") %>%
  separate_rows(Habitat, sep = ";") %>%
  separate_rows(Tank_field_decider, sep = ";") %>%
  mutate(across(c(Focus_species, Country_of_study_or_author), trimws))
  
species_country$Focus_species <- species_country$Focus_species %>%
  tolower() %>%                      # make lower case
  sub("\\(.*", "", .) %>%            # remove "(" and all after
  gsub("etc\\.", "", .) %>%          # remove "etc."
  gsub("\\bna\\b", "", .)%>%            # remove "(" and all after
  gsub("any species using bypass", "", .) %>% 
  gsub("salmonids", "", .) %>%       
  trimws() 

species_country$Focus_species <-
  paste0(toupper(substr(species_country$Focus_species, 1, 1)),
         substr(species_country$Focus_species, 2, nchar(species_country$Focus_species)))

species_country <- species_country %>%
  filter(trimws(Focus_species) != "")

species_country$Freq<-1

species_country <- species_country %>%
  group_by(Focus_species) %>%
  mutate(n = n()) %>%                    # count occurrences
  ungroup() %>%
  mutate(Focus_species = ifelse(n < 6, "Other", Focus_species)) %>%
  select(-n) 

table(species_country$Focus_species)

species_country$Country_of_study_or_author <- species_country$Country_of_study_or_author %>%
  #tolower() %>%                      # make lower case
  sub("\\(.*", "", .) %>%            # remove "(" and all after
  gsub("any species using bypass", "", .) %>% 
  gsub("salmonids", "", .) %>%       
  trimws()

species_country <- species_country %>%
  group_by(Country_of_study_or_author) %>%
  mutate(n = n()) %>%                    # count occurrences
  ungroup() %>%
  mutate(Country_of_study_or_author = ifelse(n < 6, "Other", Country_of_study_or_author)) %>%
  select(-n) 

species_country$Habitat <- species_country$Habitat %>%
  #tolower() %>%                      # make lower case
  sub("\\(.*", "", .) %>%            # remove "(" and all after
  gsub("Coast", "Marine", .) %>% 
  gsub("Fjord", "Marine", .) %>%       
  trimws()

species_country <- species_country %>%
  group_by(Habitat) %>%
  mutate(n = n()) %>%                    # count occurrences
  ungroup() %>%
  mutate(Habitat = ifelse(n < 6, "Other", Habitat)) %>%
  select(-n) 

species_country$Tank_field_decider <- species_country$Tank_field_decider %>%
  sub("\\(.*", "", .) %>%            # remove "(" and all after
  gsub("tank", "Tank", .) %>% 
  trimws()

species_country$Focus_species <- species_country$Focus_species %>%
  factor() %>%
  forcats::fct_infreq() %>%      # order by frequency (most → least)
  forcats::fct_drop()

species_country$Country_of_study_or_author <- species_country$Country_of_study_or_author %>%
  factor() %>%
  forcats::fct_infreq() %>%      # order by frequency
  forcats::fct_drop()

species_country$Habitat <- species_country$Habitat %>%
  factor() %>%
  forcats::fct_infreq() %>%      # order by frequency (most → least)
  forcats::fct_drop()

species_country$Tank_field_decider <- species_country$Tank_field_decider %>%
  factor() %>%
  forcats::fct_infreq() %>%      # order by frequency (most → least)
  forcats::fct_drop()

NoWPaS_sanky<-ggplot(species_country,
       aes(y = Freq, axis1 =Country_of_study_or_author,axis2=Focus_species , axis3=Tank_field_decider)) +
  scale_x_discrete(limits = c("Country","Species","Study type")) +
  geom_flow(aes(fill = Habitat),curve_type = "quintic") +
  geom_stratum(colour="grey")+
  scale_fill_manual(values = c(
    "River" = "skyblue",
    "Marine" = "darkblue",
    "NA" = "white",
    "Lake" = "darkcyan",
    "Other" = "grey84",
    "Estuary" = "goldenrod4"
  ))+
  geom_text(stat = "stratum", aes(label = after_stat(stratum),lineheight = 0.6),size=4) +
  theme(panel.background = element_blank())+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(vjust = 15),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank())+
  theme(
    axis.text.x = element_text(size = 16), 
    legend.text = element_text(size = 16),                                   
    legend.title = element_text(size = 18)                                     
  )

ggsave("NoWPaS_sanky.png", NoWPaS_sanky, width = 14, height = 18, dpi = 300)




