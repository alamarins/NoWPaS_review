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

setwd("C:/Users/sbiwp1/OneDrive - Cardiff University/NoWPaS review paper")

NoWPaS_df<- read_excel("NoWPaS talk characterisation.v2.xlsx", 
                                              sheet = "Sheet1")
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

broad_method <- NoWPaS_df
broad_method<- broad_method %>%
  separate_rows(Method_decider_broad, sep = ";") %>%
  mutate(Method_decider_broad = trimws(Method_decider_broad))%>% 
  filter(!is.na(Method_decider_broad) & Method_decider_broad != "NA")
broad_method$Freq<-1

broad_method$Method_decider_broad <- tolower(broad_method$Method_decider_broad)

ggplot(broad_method, aes(x = Year, fill = Method_decider_broad)) +
  geom_bar(position="fill") +
  scale_fill_brewer(palette = "Paired")+
  labs(
    x = "Year",
    y = "Count",
    fill = "Method",
  ) +
  theme_minimal()

broad_method$Driver_decider <- sub(";.*", "", broad_method$Driver_decider)

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




