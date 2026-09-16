########------- GENERAL INFO -------########
# NoWPaS conference abstract characterisation - analysis script
#
# REPRODUCIBILITY NOTES:
# - This script assumes it lives at the project root, alongside:
#     NoWPaS_talk_characterisation.xlsx
#     figures/            
#     results_models/     
# - A fixed random seed is set before the one place this script uses
#   randomness (resolving ties when picking a speaker's earliest talk).
# R ENVIRONMENT:
# R version 4.6.1 (2026-06-24)
# here_1.0.2         ggpubr_1.0.0       gratia_0.11.2      emmeans_2.0.4      DHARMa_0.4.7      
# boot_1.3-32        mgcv_1.9-4         nlme_3.1-169       janitor_2.2.1      vegan_2.7-3       
# permute_0.9-10     lubridate_1.9.5    tibble_3.3.1       tidyverse_2.0.0    ggSankeyGrad_0.0.1
# lme4_2.0-1         Matrix_1.7-5       forcats_1.0.1      stringr_1.6.0      purrr_1.2.2       
# broom_1.0.12       tidytext_0.4.3     wordcloud2_0.2.1   readxl_1.4.5       tidyr_1.3.2       
# dplyr_1.2.1        readr_2.2.0        alluvial_0.1-2     ggalluvial_0.12.6  ggplot2_4.0.3   

set.seed(123)

########------- PACKAGES -------########

required_pkgs <- c(
  "ggplot2", "ggalluvial", "alluvial", "readr", "dplyr", "tidyr",
  "readxl", "tidytext", "broom", "purrr", "stringr",
  "forcats", "lme4", "tidyverse", "vegan", "janitor", "mgcv", "boot",
  "DHARMa", "emmeans", "gratia", "ggpubr", "here"
)
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ggSankeyGrad is GitHub-only.
if (!requireNamespace("ggSankeyGrad", quietly = TRUE)) {
  devtools::install_github("ssp3nc3r/ggSankeyGrad", ref = "master")
}
library(ggSankeyGrad)

#######---------- PROJECT PATHS ----------#######
data_path    <- here::here("NoWPaS_talk_characterisation.xlsx")
figures_dir  <- here::here("figures")
results_dir  <- here::here("results_models")

#######---------- FUNCTIONS ----------#######
#GLM metric time
fit_glm_trend_time <- function(data, group_var, interaction_var = NULL) {
  rhs <- if (is.null(interaction_var)) "Year_scaled" else paste0("Year_scaled * ", interaction_var)
  fmla <- as.formula(paste0("cbind(n, N_abstracts - n) ~ ", rhs))
  
  data %>%
    group_by(.data[[group_var]]) %>%
    group_modify(~ {
      m <- glm(fmla, family = quasibinomial(), data = .x)
      out <- broom::tidy(m)
      out <- if (is.null(interaction_var)) {
        out %>% filter(term == "Year_scaled")
      } else {
        out %>% filter(term != "(Intercept)")
      }
      out %>%
        mutate(
          dispersion = deviance(m) / df.residual(m),
          n_years = dplyr::n_distinct(.x$Year)
        )
    }) %>%
    ungroup() %>%
    mutate(p_adj = p.adjust(p.value, method = "BH")) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
}

#significance stars in plots
p_to_stars <- function(p) {
  cut(p, breaks = c(-Inf, 0.001, 0.01, 0.051, Inf),
      labels = c("***", "**", "*", ""), right = TRUE)
}

make_stars_df <- function(results_df, group_var, plot_data, y_var,
                          x_pos = NULL, y_pos = NULL) {
  sig <- results_df %>%
    mutate(stars = as.character(p_to_stars(p_adj))) %>%
    filter(stars != "")
  if (nrow(sig) == 0) return(NULL)
  
  if (is.null(x_pos)) x_pos <- max(plot_data$Year, na.rm = TRUE)
  
  sig %>%
    select(all_of(group_var), stars) %>%
    mutate(x = x_pos) %>%
    mutate(y = y_pos) %>%
    rename(label = stars)
}

integer_breaks <- function(x) {
  b <- pretty(x)
  b <- unique(round(b))
  b[b>0]
}

plot_method_bubbles <- function(method) {
  ggplot(
    method_year_complete %>% filter(Broad_method == method, n>0),
    aes(x = Year,
        y = label,
        size = n)
  ) +
    geom_point(alpha = 0.85, colour="black") +
    scale_size_area(name="Number of studies",
                    breaks=integer_breaks) +
    scale_x_continuous(
      breaks = main_ticks,
      minor_breaks = all_years,
    ) +
    scale_y_discrete(labels = \(x) stringr::str_wrap(x, width = 42)) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 9),
      axis.ticks.y = element_line(),
      axis.ticks.length = unit(0.2, "cm")
    )+
    labs(
      title = method,
      x = "Year",
      y = NULL
    )
}

test_trend <- function(data, group_var) {
  data %>%
    group_by(.data[[group_var]]) %>%
    do({
      mod <- lm(Mean_note ~ Year, data = .)
      slope <- broom::tidy(mod) %>% filter(term == "Year")
      data.frame(
        category = unique(.[[group_var]]),
        slope = slope$estimate,
        p_value = slope$p.value,
        r2 = summary(mod)$r.squared,
        n = nrow(.)
      )
    }) %>%
    ungroup() %>%
    mutate(p_adj = p.adjust(p_value, method = "BH"))
}

#functions for heatmaps co-occurence
# MULTI-RESPONSE FUNCTION: the function that will be used to create the long format for every category
expand_multiresponse <- function(df,var_name){
  df %>%
    select(AbstractID,all_of(var_name)) %>%
    filter(!is.na(.data[[var_name]])) %>%
    separate_rows(!!sym(var_name),sep=";") %>%
    transmute(AbstractID,Variable=var_name,Response=str_trim(.data[[var_name]]))
}

# CO-OCCURRENCE FUNCTION
compute_cooc <- function(df,var1,var2){
  df %>%
    filter(Variable%in%c(var1,var2)) %>%
    distinct(AbstractID,Variable,Response) %>%
    group_by(AbstractID) %>%
    summarise(v1=list(Response[Variable==var1]),
              v2=list(Response[Variable==var2]),
              .groups="drop") %>%
    filter(lengths(v1)>0,lengths(v2)>0) %>%
    unnest(v1) %>%
    unnest(v2) %>%
    count(v1,v2) %>%
    rename(Var1=v1,Var2=v2,n=n)
}

# STANDARDIZED RESIDUALS (CHI²): calculation of the residuals
compute_residuals <- function(cooc){
  tab <- cooc %>%
    pivot_wider(names_from=Var2,values_from=n,values_fill=0) %>%
    column_to_rownames("Var1") %>%
    as.matrix()
  
  if(nrow(tab)<2||ncol(tab)<2||sum(tab)==0) return(NULL)
  
  chisq.test(tab)$stdres %>%
    as.data.frame() %>%
    rownames_to_column("Var1") %>%
    pivot_longer(-Var1,names_to="Var2",values_to="Residual")
}

# PLOT FUNCTION: a generic plot function for creating every plot (every possible combination)
plot_resid <- function(df,var1,var2){
  lim <- max(abs(df$Residual),na.rm=TRUE)
  
  ggplot(df,aes(Var2,Var1,fill=Residual))+
    geom_tile(color="white")+
    scale_fill_gradient2(low="#2166AC",mid="white",high="#B2182B",midpoint=0,limits=c(-lim,lim))+
    theme_bw()+
    theme(
      axis.text.x=element_text(angle=45,hjust=1,color="black", size =10),
      axis.text.y=element_text(color="black", size =10),
      axis.title=element_text(color="black"),
      axis.ticks=element_line(color="black"),
      strip.background=element_rect(fill="white",color="black"),
      strip.text=element_text(color="black"),
      panel.border=element_rect(color="black",fill=NA),
      panel.grid=element_blank()
    )+
    labs(title=NULL,x=NULL,y=NULL,fill="Std residual")
}

# LEXICAL LINEAR MODELS
fit_models <- function(data, response_var) {
  data %>%
    group_by(discipline_decider) %>%
    group_modify(~{
      mod <- lm(as.formula(paste(response_var, "~ year")), data = .x)
      tibble(metric = response_var,
             slope = coef(mod)[2],
             p_value = summary(mod)$coefficients[2,4],
             r2 = summary(mod)$r.squared,
             AIC = AIC(mod))
    })
}

#######---------- DATA ----------########

NoWPaS_df <- read_excel(data_path, sheet = "Main", na = "NA")

#rename columns
colnames(NoWPaS_df)[1] <- "AbstractID"
colnames(NoWPaS_df)[3] <- "Metadata_1"
colnames(NoWPaS_df)[4] <- "Metadata_2"
colnames(NoWPaS_df)[5] <- "Metadata_3"
colnames(NoWPaS_df)[11] <- "Academia"
colnames(NoWPaS_df)[14] <- "Species_full"
colnames(NoWPaS_df)[15] <- "Species_grouped"
colnames(NoWPaS_df)[27] <- "Trait"

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

#renaming disciplines in column Discipline_decider
NoWPaS_df$Discipline_decider <- gsub("Genetics \\(PopGen\\)", "Genetics", NoWPaS_df$Discipline_decider)
NoWPaS_df$Discipline_decider <- gsub("Genetics", "Genetics and genomics", NoWPaS_df$Discipline_decider)
NoWPaS_df$Discipline_decider <- gsub("Hydro-Geomorphology", "Hydro-geomorphology", NoWPaS_df$Discipline_decider)
NoWPaS_df$Discipline_decider <- gsub("Disease and Parasitology", "Disease and parasitology", NoWPaS_df$Discipline_decider)

# Rename charr to char
NoWPaS_df <- NoWPaS_df %>%
  mutate(Species_full = gsub("charr", "char", Species_full, ignore.case = TRUE)) %>%
  mutate(Species_grouped = gsub("charr", "char", Species_grouped, ignore.case = TRUE))

#######--------- COMMUNITY ---------########
#gender previously determined based on the R package genderizeR

sex_ratio_df <- NoWPaS_df %>%
  filter(!is.na(Gender)) %>%
  group_by(Year) %>%
  summarise(males   = sum(Gender == "male"),females = sum(Gender == "female"),
            sex_ratio = males / females)

gender_plot <- ggplot(sex_ratio_df, aes(x = Year, y = sex_ratio)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # horizontal line at 0
  labs(x = "Year",y = "Gender ratio (male:female)") +
  scale_x_continuous(breaks = sex_ratio_df$Year) +
  scale_x_reverse(breaks = sex_ratio_df$Year)+
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 14),  # x-axis text
    axis.text.y = element_text(size = 14),  # y-axis text
    axis.title.x = element_text(size = 16), # x-axis label
    axis.title.y = element_text(size = 16), # y-axis label
    plot.title = element_text(size = 18, face = "bold") # plot title if added
  )

ggsave(file.path(figures_dir, "gender_time.png"), plot = gender_plot, width = 8, height = 4, units = "in",dpi=500)

########---------- ACADEMIA RETENTION ---------########
academia_retain <- NoWPaS_df
academia_retain$`Theo(1)_app(5)_1`<-as.numeric(academia_retain$`Theo(1)_app(5)_1`)
academia_retain$`Theo(1)_app(5)_2`<-as.numeric(academia_retain$`Theo(1)_app(5)_2`)
academia_retain$`Theo(1)_app(5)_3`<-as.numeric(academia_retain$`Theo(1)_app(5)_3`)

cols <- c("Theo(1)_app(5)_1",
          "Theo(1)_app(5)_2",
          "Theo(1)_app(5)_3")

academia_retain$Theo_app_mean <- rowMeans(
  academia_retain[, cols],
  na.rm = TRUE
)
academia_retain$Theo_app_mean[is.nan(academia_retain$Theo_app_mean)] <- NA
academia_retain$Theo_app_mean <- round(academia_retain$Theo_app_mean, 0)

academia_retain <- academia_retain %>%
  select(Host_country, Gender, Speaker_ID, Year, Country_for_study_map, Academia,
         Driver_decider,Theo_app_mean) %>%
  extract(Academia,into = c("Academia", "Academia_detail"),regex = "^\\s*([^\\(]+)\\s*(?:\\(([^\\)]+)\\))?",
          remove = FALSE) %>%
  mutate(Academia_detail = ifelse(Academia == "Y", "academia",Academia_detail)) %>%
  filter(!is.na(Academia)) %>%
  mutate(Academia = trimws(Academia)) %>%
  mutate(Academia_binary = case_when(
    Academia == "Y" ~ 1,
    Academia == "N" ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(years_since_2025 = 2025 - Year)

academia_retain <- academia_retain %>%
  group_by(Speaker_ID) %>%
  filter(Year == min(Year)) %>%  # keep earliest year
  slice_sample(n = 1) %>%        # if multiple rows in that year, pick one at random
  ungroup()

freq_data <- academia_retain %>%
  count(years_since_2025, Academia_binary)
academia_retain$Gender<-as.factor(academia_retain$Gender)

model_gam <- gam(
  Academia_binary ~ 
    s(years_since_2025),
  data = academia_retain,
  family = binomial,
  method = "REML"
)
anova(model_gam)

academia_retain_country_reduc <- academia_retain %>%
  filter(Country_for_study_map %in% c(
    "Canada", "Finland", "France", "Iceland", "Ireland", "Norway", "Sweden", "United Kingdom"
  )) %>%
  mutate(
    Gender = as.factor(Gender),
    Country_for_study_map = as.factor(Country_for_study_map)
  )

model_gam_gender_country <- gam(
  Academia_binary ~ 
    s(years_since_2025)+
    s(years_since_2025, by = Gender) +
    s(years_since_2025, by = Country_for_study_map),
  data = academia_retain_country_reduc,
  family = binomial,
  method = "REML"
)
anova(model_gam_gender_country)

newdat <- data.frame(
  years_since_2025 =
    seq(min(academia_retain_country_reduc$years_since_2025),
        max(academia_retain_country_reduc$years_since_2025),
        length.out = 200)
)
newdat$pred <- predict(model_gam, newdata = newdat, type = "response")

academia_retain_plot_GAM <- ggplot() +
  geom_point(data = freq_data,aes(x = years_since_2025, y = Academia_binary, size = n),alpha = 0.6) +  
  geom_line(data=newdat, aes(years_since_2025, pred),linewidth = 1.2) +
  ylim(0, 1) +
  labs(x = "Years since the meeting",y = "Probability still in academia")+
  theme_bw() +
  theme(
    strip.text = element_text(size = 14),
    axis.text = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
    axis.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14))

ggsave(file.path(figures_dir, "academia_retain_plot_GAM.png"), plot = academia_retain_plot_GAM, width = 8, height = 4, units = "in",dpi=500)

########--------- RETENTION TYPES ---------########
academia_retain <- academia_retain %>%
  mutate(
    Academia_detail = ifelse(Academia_detail == "", "unknown", Academia_detail)) %>%
  mutate(Academia_detail = ifelse(Academia == "Y", "academia", Academia_detail))

detail_map <- c(
  "industry - Vattenfall" = "industry", "agency analyst" = "government",
  "Atlantic salmon federation" = "NGO/non-profit", "programming" = "industry",
  "environmental agency" = "government", "research institute NIBIO" = "government",
  "environmental specialist" = "industry", "advicer in fisheries" = "government",
  "environmental institution" = "NGO/non-profit", "manager" = "government",
  "company" = "industry"
)
for (pat in names(detail_map)) {
  academia_retain$Academia_detail[grepl(pat, academia_retain$Academia_detail)] <- detail_map[[pat]]
}

academia_retain <- academia_retain %>%
  mutate(
    Theo_app_mean = as.factor(Theo_app_mean),
    Academia_detail = as.factor(Academia_detail)
  )

academia_retain$Academia_detail <- factor(
  academia_retain$Academia_detail,
  levels = c(
    "academia",
    "government",
    "NGO/non-profit",
    "consulting",
    "industry",
    "unknown"
  ),
  labels = c(
    "Academia",
    "Government",
    "NGO/non-profit",
    "Consulting",
    "Industry",
    "Unknown"
  )
)

academic_retention_sanky<-
  ggplot(academia_retain, aes(axis1 = Theo_app_mean, axis2 = Academia_detail, y = 1)) +
  geom_alluvium(aes(fill = Theo_app_mean), alpha = 0.7,width = 0.3) +
  geom_stratum(fill = "white", color = "grey",width = 0.3 ) +
  geom_text(stat = "stratum",aes(label = after_stat(stratum)),size = 6) +
  scale_x_discrete(limits = c("Theo_app_mean", "Academia_detail"), expand = c(0.15, 0.05)) +
  scale_fill_manual(values = c("#56B4E9", "#009E73", "#F0E442", "#E69F00", "#D55E00"))+
  labs(x = NULL, y = "Count") +
  theme_bw()+
  theme(plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.title.y = element_blank(),  # removes y-axis title
        axis.text.x  = element_blank(),  # removes y-axis numbers
        axis.text.y  = element_blank(),  # removes y-axis numbers
        axis.ticks.y = element_blank())+theme(legend.position = "none")

ggsave(file.path(figures_dir, "academic_retention_sanky.png"), plot = academic_retention_sanky, width = 8, height = 11, units = "in",dpi=500)

#######--------- TANK/FIELD ---------########
Tank_field <- NoWPaS_df
Tank_field$Discipline_decider<-as.factor(Tank_field$Discipline_decider)
Tank_field$Tank_field_decider <- gsub("In silico","Tank",Tank_field$Tank_field_decider)
Tank_field$Tank_field_decider <- gsub("Tank; Tank","Tank",Tank_field$Tank_field_decider)
Tank_field <- Tank_field[!grepl("Literature", Tank_field$Tank_field_decider), ]
Tank_field <- Tank_field[!grepl("NA", Tank_field$Tank_field_decider), ]

Tank_field <- Tank_field %>%
  mutate(
    Tank_field_decider = ifelse(
      Tank_field_decider == "Field; Tank",
      "Field;Tank",
      Tank_field_decider
    )
  ) %>%
  separate_rows(Tank_field_decider, sep = ";") %>%
  mutate(Tank_field_decider = trimws(Tank_field_decider))

Tank_field$Tank_field_decider <- recode(
  as.character(Tank_field$Tank_field_decider),
  "Tank" = "1",
  "Field" = "0"
)

prop.table(table(Tank_field$Tank_field_decider))

########---------- SPECIES ---------########

# Data preparation
# There are two columns for species - FULL and GROUPED. 
# FULL maintains the name of the species
# GROUPED groups species (e.g., beaver and otter become Mammals) except for salmonids.
# !! IMPORTANT NOTE: in the original file Species_FULL (of _GROUPED) doesn't have _; add _ beteween Species and FULL (or GROUPED)  !!

# Separate Species_full
species_full <- NoWPaS_df %>%
  separate_rows(Species_full, sep = ";\\s") %>%  # split at semicolon
  mutate(Species_full = trimws(Species_full)) # remove extra spaces

# Separate Species_grouped
species_grouped <- NoWPaS_df %>%
  separate_rows(Species_grouped, sep = ";\\s") %>%
  mutate(Species_grouped = trimws(Species_grouped))

# Counts for species in full - These lines allow counting the number of time X appears in the data
full_counts <- species_full %>%
  group_by(Species_full) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

# Counts for species in group - These lines allow counting the number of time X appears in the data
grouped_counts <- species_grouped %>%
  group_by(Species_grouped) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count)) 

grouped_counts

# BUBBLE PLOT TEMPORAL
# Preparation - Merge groups
species_grouped <- species_grouped %>%
  mutate(Species_grouped = case_when(
    Species_grouped %in% c("Freshwater fish", 
                           "Freshwater fish (species not specified)") ~ "Freshwater fish",
    Species_grouped %in% c("Parasite", 
                           "Parasite (not specified)") ~ "Parasite",
    TRUE ~ Species_grouped
  ))

# Prepare classes
salmo <- c("Atlantic salmon", "Atlantic salmon (landlocked)", "Brown trout", "Sea trout", "Marble trout") # Salmo
salvelinus<- c("Arctic char", "Arctic char (anadromous)", "Brook char", "Brook char (anadromous)", "Dolly Varden", "Lake char", "Lake trout") # Salvelinus
oncorhynchus <- c("Rainbow trout", "Coho salmon", "Chinook salmon", "Pink salmon") # Oncorhynchus               
other_salmonids <- c("Atlantic whitefish", "European whitefish", "Whitefish (species not specified)", "European grayling", "Salmonid (species not specified)") # Other salmonids
fish <- c("Freshwater fish", "Saltwater fish") # Define fish class
parasite <- c("Parasite") # Define parasite class
other_species <- c("Freshwater organism", "Aquatic vegetation", "Terrestrial vegetation", "Gut microbiome", "Mammal") # Define other class

# Species order - This defines how species will be ordered in the plot, based on the classes defined earlier
species_order <- c(salmo, salvelinus, oncorhynchus, other_salmonids, fish, parasite, other_species) # Define class order

# Count species/group by year and sort by species_order
year_counts <- species_grouped %>%
  group_by(Year, Species_grouped) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Species_grouped = factor(Species_grouped, levels = species_order)) %>%
  filter(!is.na(Species_grouped)) %>%
  arrange(Species_grouped, Year)

# Plot       
all_years <- seq(min(year_counts$Year), max(year_counts$Year), 1)
main_ticks <- seq(2005, 2025, 5)  # only show every 5 years
species_time <- ggplot(
  year_counts,
  aes(x = Year,
      y = Species_grouped,
      size = n)
) +
  geom_point(alpha = 0.85, colour="black") +
  scale_size_area(name="Number of studies") +
  scale_x_continuous(
    breaks = main_ticks,
    minor_breaks = all_years,
  ) +
  scale_y_discrete(labels = \(x) stringr::str_wrap(x, width = 50), limits=rev) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(0.2, "cm")
  )+
  labs(
    x = "Year",
    y = NULL
  )

ggsave(file.path(figures_dir, "species_time.svg"), plot = species_time, width = 8, height = 7, units = "in",dpi=500)

########---------- LIFE STAGES ---------########

# Separate lifestages
lifestage <- NoWPaS_df %>%
  separate_rows(Lifestage, sep = ";\\s*") %>%
  mutate(Lifestage = trimws(Lifestage))

# Counts for lifestages
counts <- lifestage %>%
  group_by(Lifestage) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))
counts

# BUBBLE PLOT TEMPORAL
# Preparation - Class order
lf_order <- c("Egg", "Alevin", "Fry", "Parr", "Juvenile", "Smolt", "Post-smolt", "Adult", "Kelt", "Carcass") # Define lifestage order

# Count species/group by year and sort by lf_order
year_counts <- lifestage %>%
  group_by(Year, Lifestage) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Lifestage = factor(Lifestage, levels = lf_order)) %>%
  filter(!is.na(Lifestage)) %>%
  arrange(Lifestage, Year) 

# Plot    
all_years <- seq(min(year_counts$Year), max(year_counts$Year), 1)
main_ticks <- seq(2005, 2025, 5)  # only show every 5 years
lifestage_time <- ggplot(
  year_counts,
  aes(x = Year,
      y = Lifestage,
      size = n)
) +
  geom_point(alpha = 0.85, colour="black") +
  scale_size_area(name="Number of studies") +
  scale_x_continuous(
    breaks = main_ticks,
    minor_breaks = all_years,
  ) +
  scale_y_discrete(labels = \(x) stringr::str_wrap(x, width = 50), limits=rev) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(0.2, "cm")
  )+
  labs(
    x = "Year",
    y = NULL
  )

ggsave(file.path(figures_dir, "lifestage_time.svg"), plot = lifestage_time, width = 7, height = 5, units = "in",dpi=500)

########---------- HABITATS ---------########
# Separate Habitat
habitat <- NoWPaS_df %>%
  separate_rows(Habitat, sep = ";\\s*") %>%
  mutate(Habitat = trimws(Habitat))

# Counts for habitats
counts <- habitat %>%
  group_by(Habitat) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))
counts

# BUBBLE PLOT TEMPORAL
# Preparation - Class order
habitat_order <- c("Watershed", "River", "River (frozen)", "Lake", "Estuary", "Coast", "Fjord", "Marine", "Farm", "Hatchery")

# Count species/group by year and sort by species_order
year_counts <- habitat %>%
  group_by(Year, Habitat) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Habitat = factor(Habitat, levels = habitat_order)) %>%
  filter(!is.na(Habitat)) %>%
  arrange(Habitat, Year)   

# Plot      
all_years <- seq(min(year_counts$Year), max(year_counts$Year), 1)
main_ticks <- seq(2005, 2025, 5)  # only show every 5 years
habitat_time <- ggplot(
  year_counts,
  aes(x = Year,
      y = Habitat,
      size = n)
) +
  geom_point(alpha = 0.85, colour="black") +
  scale_size_area(name="Number of studies") +
  scale_x_continuous(
    breaks = main_ticks,
    minor_breaks = all_years,
  ) +
  scale_y_discrete(labels = \(x) stringr::str_wrap(x, width = 50), limits=rev) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    axis.ticks.y = element_line(),
    axis.ticks.length = unit(0.2, "cm")
  )+
  labs(
    x = "Year",
    y = NULL
  )

ggsave(file.path(figures_dir, "habitat_time.png"), plot = habitat_time, width = 7, height = 4, units = "in",dpi=500)

########--------- DISCIPLINES ---------########
#Separate disciplines
discipline <- NoWPaS_df %>%
  separate_rows(Discipline_decider, sep = ";\\s*") %>%
  mutate(Discipline_decider = trimws(Discipline_decider)) %>% 
  filter(!is.na(Discipline_decider) & Discipline_decider != "NA")
table(discipline$Discipline_decider)
discipline$Discipline_decider <- factor(discipline$Discipline_decider)
sort(table(discipline$Discipline_decider) * 100 / nrow(discipline))

#drop Ecotoxicology because too rare
discipline <- discipline %>%
  filter(Discipline_decider != "Ecotoxicology")
#drop the level
discipline$Discipline_decider <- droplevels(discipline$Discipline_decider)

#STATS TEMPORAL OCCURRENCE
discipline_ts <- discipline %>%
  group_by(Year, Discipline_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Discipline_decider, fill = list(n = 0)) %>%
  group_by(Year) %>%
  mutate(N = sum(n)) %>%
  ungroup() %>%
  mutate(Year_scaled = scale(Year, center = TRUE, scale = TRUE)) %>%
  left_join(abstracts_per_year, by = "Year") #need the number of abstracts as the total max number of occurence of the discipline

adjusted <- fit_glm_trend_time(discipline_ts, "Discipline_decider")
write.csv(adjusted, file.path(results_dir, "glm_disciplines_time.csv"), row.names = FALSE)

#PLOT TEMPORAL OCCURRENCE
discipline_plot_data <- discipline %>%
  group_by(Year, Discipline_decider) %>% summarise(n = n(), .groups = "drop") %>%
  complete(Year, Discipline_decider, fill = list(n = 0))
#significance
stars_df <- make_stars_df(adjusted, "Discipline_decider", discipline_plot_data, "n", 2024, 14)

#plot
discipline_time <- ggplot(discipline_plot_data, aes(x = Year, y = n)) +
  geom_point(size = 1, show.legend = F) +
  geom_line(size = 1, alpha = 0.5, show.legend = F) +
  geom_smooth(method="lm", se = T, linewidth = 1,
              color="black",show.legend = F)+
  labs(
    y = "Number of studies",
  ) +
  theme_bw() +
  facet_wrap(~ Discipline_decider, 
             scales = "fixed")+
  geom_text(
  data = stars_df,
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  size = 6,
  hjust = 1.2,
  vjust = 1.2
) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(colour = "black", fill = "white")
  )

#DISCIPLINES BY SPECIES (MAIN SPECIES)
discipline_species <- discipline %>%
  mutate(
    species_list = str_split(Species_grouped, "\\s*;\\s*"),
    species_list = map(species_list, str_trim),
    is_salmon = map_lgl(species_list, ~ any(str_detect(.x, regex("^Atlantic salmon$", ignore_case = TRUE)))), #not including landlocked
    is_trout  = map_lgl(species_list, ~ any(str_detect(.x, regex("Brown trout|Sea trout", ignore_case = TRUE))))
  ) %>%
  filter(is_salmon | is_trout) %>%
  pivot_longer(
    cols = c(is_salmon, is_trout),
    names_to = "species_flag",
    values_to = "keep"
  ) %>%
  filter(keep) %>%
  mutate(
    Main_species = recode(species_flag,
                          "is_salmon" = "Atlantic salmon",
                          "is_trout"  = "Brown trout + Sea trout"
    )
  ) %>%
  select(-species_list, -species_flag, -keep)

discipline_ts_species <- discipline_species %>%
  group_by(Year, Main_species, Discipline_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Main_species, Discipline_decider, fill = list(n = 0))

discipline_ts_species$Discipline_decider <- recode(discipline_ts_species$Discipline_decider,
                                        "Hydro-geomorphology" = "Hydro-geo-\nmorphology")
discipline_time_species <- ggplot(discipline_ts_species,
       aes(x = Year, y = n)) +
  geom_point(size = 1, show.legend = FALSE) +
  geom_line(size = 1, alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method="lm", se = T, linewidth = 1, show.legend = FALSE, color="black") +
  facet_grid(
    Discipline_decider ~ Main_species,
    scales = "fixed",
    labeller = labeller(Discipline_decider = label_wrap_gen(width = 15))
  ) +
  labs(
    y = "Number of studies",
    x = "Year"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 8, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(colour = "black", fill = "white")
  )
ggsave(file.path("figures", "disciplines_time_species.png"), plot = discipline_time_species, width = 5, height = 8, units = "in", dpi = 500)

#stats species interaction
discipline_ts_species_stats <- discipline_ts_species %>%
group_by(Year) %>%
  mutate(N = sum(n)) %>%
  ungroup() %>%
  mutate(Year_scaled = scale(Year, center = TRUE, scale = TRUE)) %>%
  left_join(abstracts_per_year, by = "Year") #need the number of abstracts as the total max number of occurence of the discipline

adjusted <- fit_glm_trend_time(discipline_ts_species_stats, "Discipline_decider", interaction_var = "Main_species")
write.csv(adjusted, file.path(results_dir, "glm_disciplines_time_species.csv"), row.names = FALSE)

########---------- METHODS (BROAD) ---------########
broad_method <- NoWPaS_df %>%
  separate_rows(Method_decider_broad, sep = ";") %>%
  mutate(Method_decider_broad = trimws(Method_decider_broad))%>% 
  filter(!is.na(Method_decider_broad) & Method_decider_broad != "NA")
sort(table(broad_method$Method_decider_broad)) / nrow(broad_method) * 100

#drop Literature review because too rare
broad_method <- broad_method %>%
  filter(Method_decider_broad != "Literature review")

#STATS METHODS OCCURRENCE
method_ts <- broad_method %>%
  group_by(Year, Method_decider_broad) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Method_decider_broad, fill = list(n = 0)) %>%
  group_by(Year) %>%
  mutate(N = sum(n)) %>%
  ungroup() %>%
  mutate(Year_scaled = scale(Year, center = TRUE, scale = TRUE)) %>%
  left_join(abstracts_per_year, by = "Year") #need the number of abstracts as the total max number of occurence of the discipline

adjusted <- fit_glm_trend_time(method_ts, "Method_decider_broad")
write.csv(adjusted, file.path(results_dir, "glm_methods_time.csv"), row.names = FALSE)

#PLOT TEMPORAL OCCURRENCE
method_plot_data <- broad_method %>%
  group_by(Year, Method_decider_broad) %>% 
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Method_decider_broad, fill = list(n = 0))

stars_df <- make_stars_df(adjusted, "Method_decider_broad", method_plot_data, "n", 2024, 18)

method_time <- ggplot(method_plot_data, aes(x = Year, y = n)) +
  geom_point(size = 1, show.legend = F) +
  geom_line(size = 1, alpha = 0.5, show.legend = F) +
  geom_smooth(method="lm", se = T, linewidth = 1,
              color="black",show.legend = F)+
  labs(
    y = "Number of studies",
  ) +
  theme_bw() +
  facet_wrap(~ Method_decider_broad, 
             scales = "fixed")+
  geom_text(
    data = stars_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 6,
    hjust = 1.2,
    vjust = 1.2
  ) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(colour = "black", fill = "white")
  )  

#assemble two plots discpilne time left (a) and method time right (b)
discipline_method_time <- ggarrange(discipline_time, method_time, ncol = 2, nrow = 1, labels = c("(a)", "(b)"), font.label = list(size = 16, face = "plain"))
ggsave(file.path(figures_dir, "disciplines_method_time.png"), plot = discipline_method_time, width = 15, height = 6, units = "in", dpi = 500)

#METHODS BY SPECIES (MAIN SPECIES)
method_species <- broad_method %>%
  mutate(
    species_list = str_split(Species_grouped, "\\s*;\\s*"),
    species_list = map(species_list, str_trim),
    is_salmon = map_lgl(species_list, ~ any(str_detect(.x, regex("^Atlantic salmon$", ignore_case = TRUE)))), #not including landlocked
    is_trout  = map_lgl(species_list, ~ any(str_detect(.x, regex("Brown trout|Sea trout", ignore_case = TRUE))))
  ) %>%
  filter(is_salmon | is_trout) %>%
  pivot_longer(
    cols = c(is_salmon, is_trout),
    names_to = "species_flag",
    values_to = "keep"
  ) %>%
  filter(keep) %>%
  mutate(
    Main_species = recode(species_flag,
                          "is_salmon" = "Atlantic salmon",
                          "is_trout"  = "Brown trout + Sea trout"
    )
  ) %>%
  select(-species_list, -species_flag, -keep)

method_ts_species <- method_species %>%
  group_by(Year, Main_species, Method_decider_broad) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Main_species, Method_decider_broad, fill = list(n = 0))

method_time_species <- ggplot(method_ts_species,
       aes(x = Year, y = n)) +
  geom_point(size = 1, show.legend = FALSE) +
  geom_line(size=1, alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method="lm", se = T, linewidth = 1, show.legend = FALSE, color="black") +
  #facet_grid(Method_decider_broad ~ Main_species, scales = "fixed") +
  facet_grid(
    Method_decider_broad ~ Main_species,
    scales = "fixed",
    labeller = labeller(Method_decider_broad = label_wrap_gen(width = 15))
  ) +
  labs(
    y = "Number of studies",
    x = "Year"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 8, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(colour = "black", fill = "white")
  )
ggsave(file.path(figures_dir, "method_time_species.png"), plot = method_time_species, width = 5, height = 8, units = "in", dpi = 500)

#stats species interaction
method_ts_species_stats <- method_ts_species %>%
  group_by(Year) %>%
  mutate(N = sum(n)) %>%
  ungroup() %>%
  mutate(Year_scaled = scale(Year, center = TRUE, scale = TRUE)) %>%
  left_join(abstracts_per_year, by = "Year") #need the number of abstracts as the total max number of occurence of the discipline

adjusted <- fit_glm_trend_time(method_ts_species_stats, "Method_decider_broad", interaction_var = "Main_species")
write.csv(adjusted, file.path(results_dir, "glm_methods_time_species.csv"), row.names = FALSE)

########---------- METHODS (DETAILED) ---------########

# Preparation
method <- NoWPaS_df %>%
  separate_rows(Method_decider, sep = ";") %>%
  mutate(Method_decider = trimws(Method_decider))%>% 
  filter(!is.na(Method_decider) & Method_decider != "NA")

table(method$Method_decider)
#regrouping
method_2 <- method %>%
  mutate(Method_std = case_when(
    Method_decider %in% c("NGS") ~ "Next-generation sequencing",
    Method_decider %in% c("Transcriptomics", "RNA-seq", "Gene expression analysis (RNA sequencing)", "RADSeq", "ddRADseq") ~ "RNA sequencing",
    Method_decider %in% c("Gene expression analysis (microarray)", "Gene expression analysis (RT-qPCR)", "mRNA expression (Nanostring)") ~ "Targeted gene expression",
    Method_decider %in% c("Gene expression analysis") ~ "Gene expression analysis",
    Method_decider %in% c("DNA methylation analysis", "Epigenetic methylation", "Reduced representation bisulfite sequencing (DNA methylation)", "Reduced Representation Bisulfite Sequencing (DNA methylation)") ~ "Methylation analysis",
    Method_decider %in% c("Genetic analysis (neutral markers)", "Genetic analysis (qPCR)", "DNA damage") ~ "Genetic analysis",
    Method_decider %in% c("Parentage analysis", "Pedigree analysis") ~ "Parentage/pedigree analysis",
    Method_decider %in% c("Microbiome genomics", "Shotgun metagenomic sequencing") ~ "Metagenomics",
    Method_decider %in% c("epigenomics") ~ "Epigenomics",
    Method_decider %in% c("Sonar", "Split-beam echosounder", "Acoustic sonar", "Adaptive Resolution Imaging Sonar") ~ "Sonar",
    Method_decider %in% c("Stomach content (Pike)", "Stomach content") ~ "Diet analysis",
    Method_decider %in% c("Habitat characterization") ~ "Habitat modelling, assessment, and classification",
    Method_decider %in% c("In-situ hydraulics", "Velocimetry") ~ "Hydraulics measurements",
    Method_decider %in% c("Citizen science") ~ "Catch reports",
    Method_decider %in% c("Mark-recapture") ~ "Capture-marking-recapture",
    Method_decider %in% c("Infection rate") ~ "Parasite screening",
    Method_decider %in% c("Data Storage Tagging") ~ "Data Storage Tag",
    Method_decider %in% c("PIT tagging") ~ "PIT tag",
    Method_decider %in% c("Tagging") ~ "Tag",
    Method_decider %in% c("Flume experiment") ~ "Channel experiment",
    TRUE ~ Method_decider
  ))

method_lookup <- tribble(
  ~Method_std, ~Broad_method,
  #Field survey and monitoring
  "Acoustic telemetry", "Field survey and monitoring",
  "Telemetry", "Field survey and monitoring",
  "Radio telemetry", "Field survey and monitoring",
  "Sonar", "Field survey and monitoring",
  "Carlin tag", "Field survey and monitoring",
  "Coded wire tag", "Field survey and monitoring",
  "PIT tag", "Field survey and monitoring",
  "Tag", "Field survey and monitoring",
  "Data Storage Tag", "Field survey and monitoring",
  "Capture-marking-recapture", "Field survey and monitoring",
  "Electrofishing", "Field survey and monitoring",
  "Video monitoring", "Field survey and monitoring",
  "Snorkeling", "Field survey and monitoring",
  "Catch reports", "Field survey and monitoring",
  "Catching traps", "Field survey and monitoring",
  "Morphology", "Field survey and monitoring",
  "Morphometrics", "Field survey and monitoring",
  "Parasite screening", "Field survey and monitoring",
  "Scalimetry", "Field survey and monitoring",
  "Diet analysis", "Field survey and monitoring",
  "Sclerochronology (otolith)", "Field survey and monitoring",
  #Molecular approach
  "Gene expression analysis", "Molecular approach",
  "Targeted gene expression", "Molecular approach",
  "Methylation analysis", "Molecular approach",
  "Epigenomics", "Molecular approach",
  "RNA sequencing", "Molecular approach",
  "Next-generation sequencing", "Molecular approach",
  "Genetic analysis", "Molecular approach",
  "Microsatellites", "Molecular approach",
  "SNPs", "Molecular approach",
  "GWAS", "Molecular approach",
  "Parentage/pedigree analysis", "Molecular approach",
  "Metabarcoding", "Molecular approach",
  "eDNA", "Molecular approach",
  "Allozymes", "Molecular approach",
  "CRISPR gene editing", "Molecular approach",
  "Fluorescence in-situ hybridisation", "Molecular approach",
  "Genomic offset", "Molecular approach",
  "Genomics", "Molecular approach",
  "MHC", "Molecular approach",
  "mRNA transfection", "Molecular approach",
  "Proteomics", "Molecular approach",
  "Stem cells", "Molecular approach",
  "Telomere analysis", "Molecular approach",
  "Vaccine development", "Molecular approach",
  "Gene regulatory analysis", "Molecular approach",
  "Metagenomics", "Molecular approach",
  "Whole-genome sequencing", "Molecular approach",
#Physiological analysis
  "Histology", "Physiological analysis",
  "Histopathology", "Physiological analysis",
  "Immunohistochemistry", "Physiological analysis",
  "Respirometry", "Physiological analysis",
  "Lipid analysis", "Physiological analysis",
  "Metabolomics", "Physiological analysis",
  "Physiological analysis", "Physiological analysis",
  "Hormone analysis", "Physiological analysis",
  "Blood immunology", "Physiological analysis",
  "Plasma analysis", "Physiological analysis",
  "Body composition", "Physiological analysis",
  "Cardiac measurements", "Physiological analysis",
  "Electrocardiography", "Physiological analysis",
  "Electromyogram radio transmitter", "Physiological analysis",
  "Color analysis", "Physiological analysis",
  "Cortisol analysis", "Physiological analysis",
  "Egg analysis", "Physiological analysis",
  "Sperm analysis", "Physiological analysis",
  "Ovarian fluid viscosity measurement", "Physiological analysis",
  "Gamete quality analysis", "Physiological analysis",
  "ELISA", "Physiological analysis",
  "Osmotic biomarkers", "Physiological analysis",
  "Oxidative stress analysis", "Physiological analysis",
  "Chromatography", "Physiological analysis",
  "Pollutant measurements", "Physiological analysis",
#Experimental approach
  "Field experiment", "Experimental approach",
  "Lab experiment", "Experimental approach",
  "Common-garden experiment", "Experimental approach",
  "Reciprocal transplant experiment", "Experimental approach",
  "Behavioural experiment", "Experimental approach",
  "Cell culture", "Experimental approach",
  "Channel experiment", "Experimental approach",
  "Enrichment experiment", "Experimental approach",
  "Hormone experiment", "Experimental approach",
  "Parasite experiment", "Experimental approach",
  "Swimming trials", "Experimental approach",
  #Habitat characterization
  "Habitat modelling, assessment, and classification", "Habitat characterization",
  "Hydraulic modelling", "Habitat characterization",
  "Remote sensing", "Habitat characterization",
  "Hydraulics measurements", "Habitat characterization",
  "Climate models", "Habitat characterization",
  "General circulation models", "Habitat characterization",
  "Hydroacoustics", "Habitat characterization",
  "Pressure sensors", "Habitat characterization",
  "Redd mapping", "Habitat characterization",
#Biological modelling
  "Population modelling", "Biological modelling",
  "Growth modelling", "Biological modelling",
  "State-space model", "Biological modelling",
  "Simulation model", "Biological modelling",
  "Machine learning", "Biological modelling",
  "Bioenergetic modelling", "Biological modelling",
  "Disease network modelling", "Biological modelling",
  "Dynamic Energy Budget Modelling", "Biological modelling",
  "Particle tracking model", "Biological modelling",
  #Stable isotopes and chemistry
  "Eye lens chemistry", "Stable isotopes and archival tags",
  "Otolith chemistry", "Stable isotopes and archival tags",
  "Scale chemistry", "Stable isotopes and archival tags",
  "Stable isotopes", "Stable isotopes and archival tags"
)

method_summary <- method_2 %>%
  left_join(method_lookup, by = "Method_std")

#PLOT TEMPORAL OCCURRENCE
method_counts <- method_summary %>%
  count(Year, Broad_method, Method_std, name="n")

method_year_complete <- method_summary %>%
  distinct(Broad_method, Method_std) %>%
  crossing(
    Year = sort(unique(method_summary$Year))
  ) %>%
  left_join(
    method_counts,
    by = c("Year", "Broad_method", "Method_std")
  ) %>%
  mutate(
    n = tidyr::replace_na(n, 0)
  )

#total counts per method and % for labels
method_totals <- method_year_complete %>%
  group_by(Broad_method, Method_std) %>%
  summarise(total_n = sum(n), .groups = "drop") %>%
  group_by(Broad_method) %>%
  mutate(percent = total_n / sum(total_n) * 100) %>%
  ungroup()
#labels
method_year_complete <- method_year_complete %>%
  left_join(method_totals, by = c("Broad_method", "Method_std")) %>%
  mutate(label = paste0(Method_std, " (", round(percent, 1), "%)"))

#first appearance years
first_appearance <- method_year_complete %>%
  group_by(Broad_method, Method_std) %>%
  summarise(first_year = min(Year[n > 0], na.rm = TRUE), .groups = "drop")

#bubble plots
method_year_complete <- method_year_complete %>%
  left_join(first_appearance, by = c("Broad_method", "Method_std")) %>%
  mutate(
    label = reorder(label, first_year),
    label = forcats::fct_rev(label))

#plots
molecular_time <- plot_method_bubbles("Molecular approach")
ggsave(file.path(figures_dir, "molecular_time.png"), plot = molecular_time, width = 9, height = 6, units = "in", dpi = 500)

bio_time <- plot_method_bubbles("Biological modelling")
exp_time <- plot_method_bubbles("Experimental approach")
field_time <- plot_method_bubbles("Field survey and monitoring")
habitat_char_time <- plot_method_bubbles("Habitat characterization")
physio_time <- plot_method_bubbles("Physiological analysis")
isotope_time <- plot_method_bubbles("Stable isotopes and archival tags")
all_time <- ggarrange(field_time, physio_time, habitat_char_time, exp_time, bio_time, isotope_time, ncol = 2, nrow = 3, labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), font.label = list(size = 16, face = "plain"))
ggsave(file.path(figures_dir, "field_physio_hab_exp_model_stable_time.png"), plot = all_time, width = 15, height = 13, units = "in", dpi = 500)

########---------- THEO/APPLIED ---------########
#cleaning
NoWPaS_df <- NoWPaS_df %>%
  filter(!is.na(`Theo(1)_app(5)_1`),
         !is.na(`Theo(1)_app(5)_2`),
         !is.na(`Theo(1)_app(5)_3`)) %>%
  mutate(across(32:34, as.numeric)) %>%
  mutate(
    Mean_theo_app = rowMeans(select(., 32:34), na.rm = TRUE),
    SD_theo_app   = apply(select(., 32:34), 1, sd, na.rm = TRUE),
    Year = as.integer(Year)
  ) %>%
  filter(!is.na(Mean_theo_app), !is.na(SD_theo_app), !is.na(Year))

#long format
reviewer_map <- c(
  "Theo(1)_app(5)_1" = 1,
  "Theo(1)_app(5)_2" = 2,
  "Theo(1)_app(5)_3" = 3
)

NoWPaS_long <- NoWPaS_df %>%
  pivot_longer(cols = 32:34,
               names_to = "Reviewer_slot",
               values_to = "Note") %>%
  mutate(
    Note = as.numeric(Note),
    Year = as.factor(Year),
    Reviewer_num = unname(reviewer_map[Reviewer_slot])
  )

mod1 <- lmer(Note ~ 1 + (1 | Reviewer_slot) + (1 | Year), data = NoWPaS_long)
summary(mod1)

#join reviewer name
notes_long <- NoWPaS_df %>%
  pivot_longer(cols = c(`Theo(1)_app(5)_1`, `Theo(1)_app(5)_2`, `Theo(1)_app(5)_3`),
               names_to = "Reviewer_slot",
               values_to = "Note")

meta_long <- NoWPaS_df %>%
  pivot_longer(cols = c(Metadata_1, Metadata_2, Metadata_3),
               names_to = "Metadata_slot",
               values_to = "Reviewer_name")
metadata_map <- c(
  "Metadata_1" = 1,
  "Metadata_2" = 2,
  "Metadata_3" = 3
)

notes_long$slot <- unname(reviewer_map[notes_long$Reviewer_slot])
meta_long$slot <- unname(metadata_map[meta_long$Metadata_slot])

NoWPaS_long <- notes_long %>%
  left_join(meta_long %>% select(AbstractID, slot, Reviewer_name),
            by = c("AbstractID", "slot")) %>%
  mutate(
    Note = as.numeric(Note),
    Year = as.integer(Year),
    Reviewer_num = slot
  )

#PLOT AVERAGE SCORE
table_total <- NoWPaS_df %>%
  summarise(
    n = n(),
    Mean = mean(Mean_theo_app, na.rm = TRUE),
    SD   = sd(Mean_theo_app, na.rm = TRUE)
  ) %>%
  mutate(
    Plot = "Total",
    Facet = "Overall",
    Mean_SD = sprintf("%.2f ± %.2f", Mean, SD)
  )
table_total

#TEMPORAL TRENDS BY CATEGORIES

#PLOT BY METHOD
Meth_long <- NoWPaS_long %>%
  filter(!is.na(Method_decider_broad)) %>%
  separate_rows(Method_decider_broad, sep = ";\\s*") %>%
  mutate(Method_decider_broad = str_trim(Method_decider_broad)) %>%
  filter(Method_decider_broad != "", Method_decider_broad != "NA")
#overall metrics
table_method <- Meth_long %>%
  group_by(Method_decider_broad) %>%
  summarise(
    n = sum(!is.na(Note)),
    Mean = mean(Note, na.rm = TRUE),
    SD   = sd(Note, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 3) %>%
  mutate(
    Plot = "Method",
    Facet = Method_decider_broad,
    Mean_SD = sprintf("%.2f ± %.2f", Mean, SD)
  )
table_method

#by year
Meth <- Meth_long %>%
  group_by(Year, Method_decider_broad) %>%
  summarise(
    Mean_note = mean(Note, na.rm = TRUE),
    SD_note   = sd(Note, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(ICmin = Mean_note - SD_note,
         ICmax = Mean_note + SD_note) %>%
  filter(count >= 3)

res_method <- test_trend(Meth, "Method_decider_broad")
res_method

stars_df <- res_method %>%
  mutate(stars = as.character(p_to_stars(p_adj))) %>%
  filter(stars != "") %>%
  transmute(Method_decider_broad = category, x = 2024, y = 6, label = stars)

score_method_time <- ggplot(Meth, aes(x = Year, y = Mean_note)) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey30", linewidth = .5) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 1) +
  geom_smooth(method="lm", se = T, linewidth = 1,
              color="black",show.legend = F)+
  scale_x_continuous(breaks = seq(min(Meth$Year), max(Meth$Year), by = 5)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(1, 5, by = 1)) +
  facet_wrap(~ Method_decider_broad, scales = "fixed") +
  labs(x = "", y = expression("Theoretical (1) "~italic("vs.")~"Applied (5) research work")) +
  theme_bw() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.position = "none") +
  geom_text(
    data = stars_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 6,
    hjust = 1.2,
    vjust = 1.2
  )
ggsave(file.path(figures_dir, "score_method_time.png"), plot = score_method_time, width = 8, height = 6, units = "in", dpi = 500)

#PLOT BY SPECIES
sp_keep <- c("Arctic char","Atlantic salmon","Atlantic salmon (landlocked)",
             "Brook char","Brown trout","Sea trout")
table_species <- NoWPaS_long %>%
  filter(!is.na(Species_grouped)) %>%
  separate_rows(Species_grouped, sep = ";\\s*") %>%
  mutate(Species_grouped = str_trim(Species_grouped)) %>%
  filter(Species_grouped %in% sp_keep) %>%
  group_by(Species_grouped) %>%
  summarise(
    n = sum(!is.na(Note)),
    Mean = mean(Note, na.rm = TRUE),
    SD   = sd(Note, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 3) %>%
  mutate(
    Plot = "Species",
    Facet = Species_grouped,
    Mean_SD = sprintf("%.2f ± %.2f", Mean, SD)
  )
table_species

Byspecies <- NoWPaS_long %>%
  filter(!is.na(Species_grouped)) %>%
  separate_rows(Species_grouped, sep = ";\\s*") %>%
  mutate(Species_grouped = str_trim(Species_grouped)) %>%
  filter(Species_grouped %in% sp_keep) %>%
  group_by(Year, Species_grouped) %>%
  summarise(
    Mean_note = mean(Note, na.rm = TRUE),
    SD_note   = sd(Note, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(ICmin = Mean_note - SD_note,
         ICmax = Mean_note + SD_note) %>%
  filter(count >= 3) %>%
  mutate(
    # Species_grouped = factor(Species_grouped, levels = c("Atlantic salmon",
    #                                                      "Brown trout",
    #                                                      "Sea trout",
    #                                                      "Arctic char",
    #                                                      "Atlantic salmon (landlocked)",
    #                                                      "Brook char"))
    Species_grouped = factor(Species_grouped, levels = c("Arctic char",
                                                         "Atlantic salmon",
                                                         "Atlantic salmon (landlocked)",
                                                         "Brook char",
                                                         "Brown trout",
                                                         "Sea trout"
                                                         ))
  )

res_species <- test_trend(Byspecies, "Species_grouped")
res_species

score_species_time <- ggplot(Byspecies, aes(x = Year, y = Mean_note)) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey30", linewidth = .5) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 1) +
  geom_smooth(method="lm", se = T, linewidth = 1,
              color="black",show.legend = F)+
  scale_y_continuous(limits = c(0, 6), breaks = seq(1, 5, by = 1)) +
  scale_x_continuous(breaks = seq(min(Byspecies$Year), max(Byspecies$Year), by = 5)) +
  facet_wrap(~ Species_grouped, scales = "fixed") +
  labs(x = "", y = expression("Theoretical (1) "~italic("vs.")~"Applied (5) research work")) +
  theme_bw() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.position = "none")
ggsave(file.path(figures_dir, "score_species_time.png"), plot = score_species_time, width = 8, height = 5, units = "in", dpi = 500)

#PLOT BY DISCIPLINE
table_discipline <- NoWPaS_long %>%
  filter(!is.na(Discipline_decider)) %>%
  separate_rows(Discipline_decider, sep = ";\\s*") %>%
  mutate(Discipline_decider = str_trim(Discipline_decider)) %>%
  filter(
    Discipline_decider != "",
    Discipline_decider != "Ecotoxicology",
    Discipline_decider != "NA"
  ) %>%
  group_by(Discipline_decider) %>%
  summarise(
    n = sum(!is.na(Note)),
    Mean = mean(Note, na.rm = TRUE),
    SD   = sd(Note, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 3) %>%
  mutate(
    Plot = "Discipline",
    Facet = Discipline_decider,
    Mean_SD = sprintf("%.2f ± %.2f", Mean, SD)
  )
table_discipline

Dis_long <- NoWPaS_long %>%
  filter(!is.na(Discipline_decider)) %>%
  separate_rows(Discipline_decider, sep = ";\\s*") %>%
  mutate(Discipline_decider = str_trim(Discipline_decider)) %>%
  filter(Discipline_decider != "",
         Discipline_decider != "Ecotoxicology",
         Discipline_decider != "NA")

Bydiscipline <- Dis_long %>%
  group_by(Year, Discipline_decider) %>%
  summarise(
    Mean_note = mean(Note, na.rm = TRUE),
    SD_note   = sd(Note, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(ICmin = Mean_note - SD_note,
         ICmax = Mean_note + SD_note) %>%
  filter(count >= 3)

res_discipline <- test_trend(Bydiscipline, "Discipline_decider")
res_discipline

stars_df <- res_discipline %>%
  mutate(stars = as.character(p_to_stars(p_adj))) %>%
  filter(stars != "") %>%
  transmute(Discipline_decider = category, x = 2024, y = 6, label = stars)

score_discipline_time <- ggplot(Bydiscipline, aes(x = Year, y = Mean_note)) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey30", linewidth = .5) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 1) +
  geom_smooth(method="lm", se = T, linewidth = 1,
              color="black",show.legend = F)+
  scale_y_continuous(limits = c(0, 6), breaks = seq(1, 5, by = 1)) +
  scale_x_continuous(breaks = seq(min(Bydiscipline$Year), max(Bydiscipline$Year), by = 5)) +
  facet_wrap(~ Discipline_decider, scales = "fixed") +
  labs(x = "", y = expression("Theoretical (1) "~italic("vs.")~"Applied (5) research work")) +
  theme_bw() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.position = "none") +
  geom_text(
    data = stars_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 6,
    hjust = 1.2,
    vjust = 1.2
  )

#PLOT BY COUNTRY
countries_keep <- c("Canada","Denmark","Finland","France","Iceland",
                    "Ireland","Norway","Sweden","United Kingdom")

table_country <- NoWPaS_long %>%
  filter(!is.na(Country_for_study_map)) %>%
  separate_rows(Country_for_study_map, sep = ";\\s*") %>%
  mutate(
    Country_for_study_map = str_trim(Country_for_study_map)
  ) %>%
  filter(Country_for_study_map %in% countries_keep) %>%
  group_by(Country_for_study_map) %>%
  summarise(
    n = sum(!is.na(Note)),
    Mean = mean(Note, na.rm = TRUE),
    SD   = sd(Note, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 3) %>%
  mutate(
    Plot = "Country",
    Facet = Country_for_study_map,
    Mean_SD = sprintf("%.2f ± %.2f", Mean, SD)
  )
table_country

Bycountry <- NoWPaS_long %>%
  filter(!is.na(Country_for_study_map)) %>%
  separate_rows(Country_for_study_map, sep = ";\\s*") %>%
  mutate(Country_for_study_map = str_trim(Country_for_study_map)) %>%
  filter(Country_for_study_map %in% countries_keep) %>%
  group_by(Year, Country_for_study_map) %>%
  summarise(
    Mean_note = mean(Note, na.rm = TRUE),
    SD_note   = sd(Note, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(ICmin = Mean_note - SD_note,
         ICmax = Mean_note + SD_note) %>%
  filter(count >= 3)

res_country <- test_trend(Bycountry, "Country_for_study_map")
res_country

stars_df <- res_country %>%
  mutate(stars = as.character(p_to_stars(p_adj))) %>%
  filter(stars != "") %>%
  transmute(Country_for_study_map = category, x = 2024, y = 6, label = stars)

score_country_time <- ggplot(Bycountry, aes(x = Year, y = Mean_note)) +
  geom_hline(yintercept = 3, linetype = "dashed", color = "grey30", linewidth = .5) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 1) +
  geom_smooth(method="lm", se = T, linewidth = 1,
              color="black",show.legend = F)+
  scale_y_continuous(limits = c(0, 6), breaks = seq(1, 5, by = 1)) +
  scale_x_continuous(breaks = seq(min(Bycountry$Year), max(Bycountry$Year), by = 5)) +
  facet_wrap(~ Country_for_study_map, scales = "fixed") +
  labs(x = "", y = expression("Theoretical (1) "~italic("vs.")~"Applied (5) research work")) +
  theme_bw() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(colour = "black", fill = "white"),
        legend.position = "none") +
  geom_text(
    data = stars_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 6,
    hjust = 1.2,
    vjust = 1.2
  )

score_country_discipline_time <- ggarrange(score_country_time, score_discipline_time, ncol = 2, nrow = 1, labels = c("(a)", "(b)"), font.label = list(size = 16, face = "plain"))
ggsave(file.path(figures_dir, "score_country_discipline_time.png"), plot = score_country_discipline_time, width = 15, height = 6, units = "in", dpi = 500)

########---------- DRIVERS ---------########
driver <- NoWPaS_df %>%
  separate_rows(Driver_decider, sep = ";\\s*") %>%
  mutate(Driver_decider = trimws(Driver_decider)) %>% 
  filter(!is.na(Driver_decider) & Driver_decider != "NA")
sort(table(driver$Driver_decider))/ nrow(driver) * 100

#STATS DRIVERS OCCURRENCE
drivers_ts <- driver %>%
  group_by(Year, Driver_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Driver_decider, fill = list(n = 0)) %>%
  group_by(Year) %>%
  mutate(N = sum(n)) %>%
  ungroup() %>%
  mutate(Year_scaled = scale(Year, center = TRUE, scale = TRUE)) %>%
  left_join(abstracts_per_year, by = "Year") #need the number of abstracts as the total max number of occurence of the discipline)

adjusted <- fit_glm_trend_time(drivers_ts, "Driver_decider")
write.csv(adjusted, file.path(results_dir, "glm_drivers_time.csv"), row.names = FALSE)

#PLOT TEMPORAL OCCURRENCE
drivers_plot_data <- driver %>%
  group_by(Year, Driver_decider) %>% summarise(n = n(), .groups = "drop") %>%
  complete(Year, Driver_decider, fill = list(n = 0))

stars_df <- make_stars_df(adjusted, "Driver_decider", drivers_plot_data, "n", 2024, 30)

drivers_time <- ggplot(drivers_plot_data, aes(x = Year, y = n)) +
  geom_point(size = 1, show.legend = F) +
  geom_line(size = 1, alpha = 0.5, show.legend = F) +
  geom_smooth(method="lm", se = T, linewidth = 1,
              color="black",show.legend = F)+
  labs(
    y = "Number of studies",
  ) +
  theme_bw() +
  facet_wrap(~ Driver_decider, 
             scales = "fixed")+
  geom_text(
    data = stars_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 6,
    hjust = 1.2,
    vjust = 1.2
  ) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(colour = "black", fill = "white")
  )

#DRIVER BY SPECIES (MAIN SPECIES)
drivers_species <- driver %>%
  mutate(
    species_list = str_split(Species_grouped, "\\s*;\\s*"),
    species_list = map(species_list, str_trim),
    is_salmon = map_lgl(species_list, ~ any(str_detect(.x, regex("^Atlantic salmon$", ignore_case = TRUE)))), #not including landlocked
    is_trout  = map_lgl(species_list, ~ any(str_detect(.x, regex("Brown trout|Sea trout", ignore_case = TRUE))))
  ) %>%
  filter(is_salmon | is_trout) %>%
  pivot_longer(
    cols = c(is_salmon, is_trout),
    names_to = "species_flag",
    values_to = "keep"
  ) %>%
  filter(keep) %>%
  mutate(
    Main_species = recode(species_flag,
                          "is_salmon" = "Atlantic salmon",
                          "is_trout"  = "Brown trout + Sea trout"
    )
  ) %>%
  select(-species_list, -species_flag, -keep)

driver_ts_species <- drivers_species %>%
  group_by(Year, Main_species, Driver_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(Year, Main_species, Driver_decider, fill = list(n = 0))

drivers_species_time <- ggplot(driver_ts_species,
       aes(x = Year, y = n)) +
  geom_point(size = 1, show.legend = FALSE) +
  geom_line(size=1, alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method="lm", se = T, linewidth = 1, show.legend = FALSE, color="black") +
  #facet_grid(Driver_decider ~ Main_species, scales = "fixed") +
  facet_grid(
    Driver_decider ~ Main_species,
    scales = "fixed",
    labeller = labeller(Driver_decider = label_wrap_gen(width = 15))
  ) +
  labs(
    y = "Number of studies",
    x = "Year"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 8, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(colour = "black", fill = "white")
  )
ggsave(file.path(figures_dir, "drivers_species_time.png"), plot = drivers_species_time, width = 5, height = 8, units = "in", dpi = 500)

#stats species interaction
drivers_ts_species_stats <- driver_ts_species %>%
  group_by(Year) %>% mutate(N = sum(n)) %>% ungroup() %>%
  mutate(Year_scaled = scale(Year, center = TRUE, scale = TRUE)) %>%
  left_join(abstracts_per_year, by = "Year")

adjusted <- fit_glm_trend_time(drivers_ts_species_stats, "Driver_decider", interaction_var = "Main_species")
write.csv(adjusted, "results_models/glm_drivers_species_time.csv", row.names = FALSE)

########---------- TRAITS ---------########
traits <- NoWPaS_df %>%
  separate_rows(Trait, sep = ";\\s*") %>%
  mutate(Trait = trimws(Trait))%>% 
  filter(!is.na(Trait) & Trait != "NA")
sort(table(traits$Trait))

#renaming
trait_recodes <- list(
  Behaviour = c("Behaviour (aggression)", "Behaviour (boldness)", "Behaviour (competition)",
                "Behaviour (exploration, boldness, aggression)", "Behaviour (exploration)",
                "Behaviour (feeding)", "Behaviour (spawning)"),
  Straying = c("Dispersal", "Homing"),
  `Habitat use` = c("Habitat niche", "Habitat use (spawning)", "Niche"),
  Microbiome = c("Microbiome (gill)", "Microbiome (gut)"),
  Timing = c("Timing (emergence)", "Timing (hatching)", "Timing (migration)"),
  `Gene expression` = c("Gene regulatory network", "mRNA expression", "Transcriptome"),
  `Habitat quality` = c("Habitat suitability"),
  `DNA methylation` = c("Methylation"),
  Habitat = c("Habitat quality", "Habitat"),
  `Genetic diversity` = c("Genetic structure", "Genetic diversity", "Genetic differentiation"),
  Density = c("Abundance")
)
for (new_val in names(trait_recodes)) {
  traits$Trait[traits$Trait %in% trait_recodes[[new_val]]] <- new_val
}

traits_freq <- traits %>%
  count(Trait, sort = TRUE)

#write a text file with the list of traits and the occurence in parenthesis
write(paste0(traits_freq$Trait, " (", traits_freq$n, ")", collapse = ", "), file.path(results_dir, "traits_list.txt"))

sort(table(traits$Trait)) * 100 / sum(table(traits$Trait)) #% of assignments
#sort(table(traits$Trait)) * 100 / nrow(NoWPaS_df) #% of abstracts


#SANKEY PLOT
traits_species <- traits %>%
  separate_rows(Species_grouped, sep = ";")  %>%
  mutate(Species_grouped = trimws(Species_grouped)) %>% 
  filter(!is.na(Species_grouped) & Species_grouped != "NA")

sort(table(traits_species$Species_grouped))
##Keep species
traits_species <- traits_species %>%
  filter(Species_grouped %in% c("Atlantic salmon","Brown trout", "Sea trout",
                               "Arctic char", "Atlantic salmon (landlocked)","Brook char"))

sort(table(traits_species$Trait))
#keep only the one > 10 occurence
traits_species <- traits_species %>%
  filter(Trait %in% c("Growth", "Movement", "Survival",
                      "Behaviour", "Density", "Habitat use", 
                      "Genetic diversity", "Habitat", "Metabolism",
                      "Gene expression", "Morphology", "Diet",
                      "Timing","Stress","Infection rate",
                      "Age at maturity", "Introgression"))

sort(table(traits_species$Trait))

sankey_data <- traits_species %>%
  group_by(Species_grouped, Trait) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    Trait = fct_reorder(Trait, n, .fun = sum, .desc = TRUE),
    Species_grouped = fct_reorder(Species_grouped, n, .fun = sum, .desc = TRUE)
  )
#keep only connexions with n>3
# sankey_data <- sankey_data %>%
#   filter(n > 3)

#plot with colour gradient
species_levels <- names(sort(tapply(sankey_data$n, sankey_data$Species_grouped, sum),
                             decreasing = TRUE))
species_levels <- rev(species_levels)
sankey_data$Species_grouped <- factor(
  sankey_data$Species_grouped,
  levels = species_levels
)
sankey_data$Trait <- trimws(sankey_data$Trait)

#rename Atlantic salmon (landlocked) in Atl. sal. (landlocked)
sankey_data$Species_grouped <- recode(sankey_data$Species_grouped,
                                      "Atlantic salmon (landlocked)" = "Atl. sal. (landlocked)")
species_cols <- c(
  "Atlantic salmon" = "black",
  "Brown trout" = "dodgerblue4",
  "Sea trout" = "#009E73",
  "Arctic char" = "darkorange",
  "Atl. sal. (landlocked)" = "darkgreen",
  "Brook char" = "red3"
)

trait_levels <- names(sort(tapply(sankey_data$n, sankey_data$Trait, sum),
                           decreasing = TRUE))
trait_levels <- rev(trait_levels)
sankey_data$Trait <- factor(sankey_data$Trait, levels = trait_levels)
trait_cols <- c(
  "Growth" = "#E69F00",
  "Movement" = "#CC79A7",
  "Survival" = "#D55E00",
  "Behaviour" = "#0072B2",
  "Density" = "darkred",
  "Habitat use" = "forestgreen",
  "Genetic diversity" = "lightcoral",
  "Habitat" = "sienna",
  "Metabolism" = "lightsteelblue",
  "Gene expression" = "magenta4",
  "Morphology" = "cyan3",
  "Diet" = "pink3",
  "Timing" = "lightblue4",
  "Stress" = "yellowgreen",
  "Infection rate" = "darkcyan",
  "Age at maturity" = "mediumpurple",
  "Introgression" = "gold3"
)

sankey_data$col1 <- species_cols[as.character(sankey_data$Species_grouped)]
sankey_data$col2 <- trait_cols[as.character(sankey_data$Trait)]
sankey_data <- sankey_data[order(sankey_data$Trait), ]
sankey_data <- sankey_data[order(sankey_data$Species_grouped), ]
sankey_left <- with(sankey_data,
     ggSankeyGrad(
       c1 = Species_grouped,
       c2 = Trait,
       col1 = col1,
       col2 = col2,
       values = n,
       label = TRUE
     ))
ggsave(file.path(figures_dir, "sankey_left.svg"), sankey_left, width = 8, height = 8, units = "in", dpi = 500)

#remember the order of traits for second plot
trait_levels_fixed <- rev(levels(sankey_data$Trait))
#combined with drivers
traits_species_driver <- traits_species %>%
  separate_rows(Driver_decider, sep = ";")  %>%
  mutate(Driver_decider = trimws(Driver_decider)) %>% 
  filter(!is.na(Driver_decider) & Driver_decider != "NA")

sort(table(traits_species_driver$Driver_decider))
sort(table(traits_species_driver$Trait))

sankey_data2 <- traits_species_driver %>%
  group_by(Trait, Driver_decider) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    #Trait = fct_reorder(Trait, n, .fun = sum, .desc = TRUE),
    Trait = factor(Trait, levels = trait_levels_fixed), #keep the same order as before
    Driver_decider = fct_reorder(Driver_decider, n, .fun = sum, .desc = TRUE)
  )
# sankey_data2 <- sankey_data2 %>%
#   filter(n > 3)

trait_levels <- rev(trait_levels_fixed)
sankey_data2$Trait <- factor(sankey_data2$Trait, levels = trait_levels)
sankey_data2$Driver_decider <- trimws(sankey_data2$Driver_decider)

driver_levels <- names(sort(tapply(sankey_data2$n, sankey_data2$Driver_decider, sum),
                            decreasing = TRUE))
driver_levels <- rev(driver_levels)
sankey_data2$Driver_decider <- factor(
  sankey_data2$Driver_decider,
  levels = driver_levels
)
#sankey_data$Trait <- trimws(sankey_data$Trait)
driver_cols <- c(
  "Aquaculture" = "red3",
  "Stocking" = "darkgreen",
  "Climate change" = "darkorange",
  "Hydropower and dams" = "#009E73",
  "Management and restoration" = "#56B4E9",
  "Fundamental research" = "black"
)
sankey_data2$col1 <- trait_cols[as.character(sankey_data2$Trait)]
sankey_data2$col2 <- driver_cols[as.character(sankey_data2$Driver_decider)]
sankey_data2 <- sankey_data2[order(sankey_data2$Driver_decider), ]
sankey_data2 <- sankey_data2[order(sankey_data2$Trait), ]

levels(sankey_data2$Driver_decider) <- c(
  "Aquaculture",
  "Stocking",
  "Climate change",
  "Hydropower\nand dams",
  "Management\nand restoration",
  "Fundamental\nresearch"
)

sankey_right <- with(sankey_data2,
     ggSankeyGrad(
       c1 = Trait,
       c2 = Driver_decider,
       col1 = col1,
       col2 = col2,
       values = n,
       label = TRUE
     ))
ggsave(file.path(figures_dir, "sankey_right.svg"), sankey_right, width = 8, height = 8, units = "in", dpi = 500)


########---------- CO-OCCURENCES HEATMAPS ---------########

# LONG FORMAT + FILTERING
# Categories we want to keep
multi_vars <- c("Discipline_decider","Method_decider_broad","Species_grouped","Lifestage","Habitat","Trait","Driver_decider")

# Creation of the long-format dataset (and filters for any kind of NAs)
long_df <- map_dfr(multi_vars,~expand_multiresponse(NoWPaS_df,.x)) %>%
  filter(!is.na(Response),
         Response!="",
         !str_to_lower(Response)%in%c("na","n/a","none"))

# Species we want to keep
species <- c("Arctic char","Atlantic salmon","Atlantic salmon (landlocked)",
             "Brook char","Brown trout","Sea trout")
species_order <- c("Atlantic salmon","Brown trout","Sea trout",
                   "Arctic char","Atlantic salmon (landlocked)","Brook char")
# Traits we want to keep
traits <- c("Metabolism","Growth","Survival","Movement","Behaviour","Diet","Morphology",
            "Timing","Stress","Infection rate","Age at maturity","Habitat use",
            "Habitat","Density","Gene expression",
            "Genetic diversity","Introgression")

# Final long-format dataset with all filters applied
long_df <- long_df %>%
  filter(!(Variable=="Species_grouped")|Response%in%species) %>%
  filter(!(Variable=="Trait")|Response%in%traits) %>%
  group_by(Variable,Response) %>%
  filter(n()>2) %>%
  ungroup()

# ALL PAIRWISE RESIDUAL HEATMAPS: every possible plot
pairs <- combn(multi_vars,2,simplify=FALSE)

heatmaps_resid <- list()

for(p in pairs){
  cooc <- compute_cooc(long_df,p[1],p[2])
  resid <- compute_residuals(cooc)
  if(is.null(resid)) next
  
  # Apply custom species order if Species_grouped is one of the two variables
  if(p[1] == "Species_grouped"){
    resid$Var1 <- factor(resid$Var1, levels = rev(species_order))
  }
  if(p[2] == "Species_grouped"){
    resid$Var2 <- factor(resid$Var2, levels = rev(species_order))
  }
  
  heatmaps_resid[[paste(p[1],p[2],sep="_")]] <- plot_resid(resid,p[1],p[2])
}

#species-drivers
species_driver_heatmap <- heatmaps_resid[["Species_grouped_Driver_decider"]]

drivers_time_heatmap <- ggarrange(drivers_time, species_driver_heatmap, ncol = 2, nrow = 1, labels = c("(a)", "(b)"), font.label = list(size = 16, face = "plain"))
ggsave(file.path(figures_dir, "drivers_time_heatmap.png"), plot = drivers_time_heatmap, width = 14, height = 5, units = "in", dpi = 500)

# Sp ONLY - faceted 
sp_resid <- map_dfr(setdiff(multi_vars,"Species_grouped"),function(v){
  
  cooc <- compute_cooc(long_df,"Species_grouped",v)
  if(nrow(cooc)==0) return(NULL)
  
  resid <- compute_residuals(cooc)
  if(is.null(resid)) return(NULL)
  
  resid$Var1 <- factor(resid$Var1, levels = rev(species_order))  #species order
  resid$Variable <- v
  resid
})

var_labels <- c(
  "Discipline_decider"      = "Discipline",
  "Method_decider_broad"    = "Method",
  "Lifestage"                = "Life stage",
  "Habitat"                  = "Habitat",
  "Trait"                    = "Trait",
  "Driver_decider"           = "Driver"
)

heatmaps_cooc <- ggplot(sp_resid,aes(Var2,Var1,fill=Residual))+
  geom_tile(color="white")+
  facet_wrap(~Variable,scales="free_x", labeller = labeller(Variable = var_labels))+
  scale_fill_gradient2(low="#2166AC",mid="white",high="#B2182B",midpoint=0)+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1,color="black", size = 10),
    axis.text.y=element_text(color="black", size = 10),
    axis.title=element_text(color="black"),
    axis.ticks=element_line(color="black"),
    strip.background = element_rect(colour = "black", fill = "white"),
    strip.text = element_text(size = 10, face = "bold"),
    panel.border=element_rect(color="black",fill=NA),
    panel.grid=element_blank())+
  labs(x=NULL,y="Species",fill="Std residual")

ggsave(file.path(figures_dir, "heatmaps_cooc.png"), plot = heatmaps_cooc, width = 12, height = 7, units = "in", dpi = 500)

########---------- QUESTIONS (LEXICAL ANALYSIS) ---------########
df <- NoWPaS_df %>%
  clean_names()

# Long format
df1 <- df %>%
  pivot_longer(
    cols = starts_with("question"),
    names_to = "question_id",
    values_to = "question"
  ) %>%
  filter(!is.na(question) & question != "")

#Tokenisation
data(stop_words)
words_df <- df1 %>%
  unnest_tokens(word, question) %>%
  anti_join(stop_words) %>%          # remove frequent words (what, is, the, and etc.)
  filter(str_detect(word, "[a-z]")) # keep useful words

#Frequency of words
word_freq <- words_df %>%
  count(year, word, name = "n") %>%
  group_by(year) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Index calculation (diversity, richness and similarity)
lex_div <- word_freq %>%
  group_by(year) %>%
  summarise(H = diversity(prop),
            S = n_distinct(word),
            J = H / log(S),
            .groups = "drop")

# Plot
lex_div_long <- lex_div %>%
  pivot_longer(cols = c(H, S, J),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric,
                         H = "Shannon diversity (H)",
                         S = "Lexical richness (S)",
                         J = "Evenness (J)"))

# Linear models
mod_H <- lm(H ~ year, data = lex_div); mod_S <- lm(S ~ year, data = lex_div); mod_J <- lm(J ~ year, data = lex_div)
summary(mod_H);summary(mod_S);summary(mod_J)
#combine summary models into one table
model_results <- bind_rows(
  broom::tidy(mod_H) %>% filter(term == "year") %>% mutate(metric = "H"),
  broom::tidy(mod_S) %>% filter(term == "year") %>% mutate(metric = "S"),
  broom::tidy(mod_J) %>% filter(term == "year") %>% mutate(metric = "J")
) %>%
  select(metric, estimate, std.error, statistic, p.value) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

write.csv(model_results, file.path(results_dir, "glm_lexical_time.csv"))

# BY DISCIPLINES
# LONG FORMAT QUESTIONS
df1 <- df %>%
  pivot_longer(cols = starts_with("question"),
               names_to = "question_id",
               values_to = "question") %>%
  filter(!is.na(question) & question != "") %>%
  separate_rows(discipline_decider, sep = ";") %>%
  mutate(discipline_decider = str_trim(discipline_decider)) %>%
  filter(!is.na(discipline_decider),
         discipline_decider != "NA",
         discipline_decider != "Ecotoxicology")

# REMOVE GENERIC TERMS
data(stop_words)
words_df <- df1 %>%
  unnest_tokens(word, question) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]"))

# WORD FREQUENCIES
word_freq <- words_df %>%
  count(year, discipline_decider, word, name = "n") %>%
  group_by(year, discipline_decider) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# LEXICAL INDICES
lex_div <- word_freq %>%
  group_by(year, discipline_decider) %>%
  summarise(H = diversity(prop),
            S = n_distinct(word),
            J = ifelse(S > 1, H / log(S), NA_real_),
            .groups = "drop")

res_H <- fit_models(lex_div, "H")
res_S <- fit_models(lex_div, "S")
res_J <- fit_models(lex_div, "J")
model_results <- bind_rows(res_H, res_S, res_J)

# Correction BH
model_results <- model_results %>%
  mutate(p_adjusted = p.adjust(p_value, method = "BH")) %>%
  mutate(across(where(is.numeric), ~round(., 3)))
model_results
write.csv(model_results, file.path(results_dir, "glm_lexical_time_discipline.csv"))

stars_df <- model_results %>%
  filter(metric == "H") %>%
  mutate(stars = as.character(p_to_stars(p_adjusted))) %>%
  filter(stars != "") %>%
  transmute(discipline_decider = discipline_decider, x = 2020, y = 6.2, label = stars)

# PLOT — Shannon diversity (H) par discipline, un seul panneau facetté
questions_discipline_time <- ggplot(lex_div, aes(year, H)) +
  geom_point(size = 1, show.legend = F) +
  geom_line(size = 1, alpha = 0.5, show.legend = F) +
  geom_smooth(method="lm", se = T, linewidth = 1,
              color="black",show.legend = F)+
  facet_wrap(~discipline_decider, 
             scales = "fixed") +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(colour = "black", fill = "white")
  ) +
  labs(x = "Year", y = "Shannon diversity (H)") +
  geom_text(
    data = stars_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    size = 6,
    hjust = 1.2,
    vjust = 1.2
  ) 

ggsave(file.path(figures_dir, "questions_disciplines_time.png"), plot = questions_discipline_time, width = 8, height = 6, units = "in", dpi = 500)

