## Script for making dataframe for Norfunds the mitigation share of Norfunds DIM portfolio (not CIM)
## Important: 
## The script excludes the CIM portfolio - The climate investment fund, but only from 2022 and onwards as some agreements were transfered from DIM to CIM in 2022.
## The reason for the exclusion is that the CIM capitalisation agreement has a mitigation marker already.
## Update the csv of CIM capitalisation agreements. 
## Using oda/oof dataset. Script is sourced in script klimatabeller.R

# Load packages and data ----
library(tidyverse)
library(here)
library(janitor)
library(writexl)
library(noradstats)

#remotes::install_github("noradno/noradstats")

df_orig <- noradstats::read_aiddata(here("data", "statsys_ten.csv"))

# 1. First dataframe on Norfunds climate relevant investments -------------------------

# inkluderer klimakolonner fra pakken noradstats
df <- noradstats::add_cols_climate(df_orig)

# rydder kolonnenavn
df <- janitor::clean_names(df)

# Climate investment fund (CIM) agreements
vec_cim <- read_csv2(here("data", "cim_agreements.csv")) |> select(agreement_number) |> pull()

# dataframe med norfunds total beregnede brutto investeringer i utslippsreduksjon per år (ekskluderer admin og cim)
df_mitigation_dim_gross_extended_mnok <- df %>%
  filter(type_of_flow == "OOF") %>%
  filter(type_of_assistance != "Administration") %>%
  filter(extending_agency == "Norfund") %>%
  filter(ifelse(year >= 2022, !agreement_number %in% vec_cim, TRUE)) %>%
  filter(year >= 2014) %>%
  group_by(year) %>%
  summarise(mitigation_dim_gross_exended_mnok = sum(climate_mitigation_nok_mill_gross_fix))

# dataframe med norfunds totale brutto investeringer per år (ekskluderer admin og cim)
df_total_dim_gross_exended_mnok <- df %>%
  filter(type_of_flow == "OOF") %>%
  filter(type_of_assistance != "Administration") %>%
  filter(extending_agency == "Norfund") %>%
  filter(ifelse(year >= 2022, !agreement_number %in% vec_cim, TRUE)) %>%
  filter(year >= 2014) %>%
  mutate(amounts_extended_mill_fix = if_else(
    amounts_extended_1000_nok < 0, 0,
    amounts_extended_1000_nok / 1000)) %>%
  group_by(year) %>%
  summarise(total_dim_gross_exended_mnok = sum(amounts_extended_mill_fix))

# dataframe som slår sammen de to dataframene over
df_norfund <- left_join(df_total_dim_gross_exended_mnok, df_mitigation_dim_gross_extended_mnok, by = "year")

# kolonne med andel klimarelatert av total investeringer (2 års gjennomsnitt)

df_norfund$mitigation_share_2yr_avg <- vector("double", nrow(df_norfund))

for (i in 2:nrow(df_norfund)) {
  df_norfund$mitigation_share_2yr_avg[i] <- 
    (df_norfund$mitigation_dim_gross_exended_mnok[i-1] + df_norfund$mitigation_dim_gross_exended_mnok[i]) /
    (df_norfund$total_dim_gross_exended_mnok[i-1] + df_norfund$total_dim_gross_exended_mnok[i])
}

# Fjerner ubrukte objekter
rm(df, df_mitigation_dim_gross_extended_mnok, df_total_dim_gross_exended_mnok, i)

# 2. Second dataframe on Norfund capitalisations -------------------------------

# Kapitaliseringsavtaler fra egen excelfil (husk å oppdatere hvert år)
vec_capitalisations <- readxl::read_xlsx(here("data", "norfund_capitalisation_agreements.xlsx")) %>%
  pull(agreement_number)

# Kapitalisering per år
df_capitalisation <- df_orig %>%
  janitor::clean_names() %>%
  filter(type_of_flow == "ODA") %>%
  filter(type_of_agreement != "Rammeavtale") %>%
  filter(agreement_partner == "Norfund") %>%
  filter(year >= 2014) %>%
  filter(agreement_number %in% vec_capitalisations) %>%
  group_by(year) %>%
  summarise(capitalisation_mnok = sum(disbursed_mill_nok)) %>%
  ungroup()

# 3. Full dataframe on Norfunds incl. climate share of capitalisations (2yr avg.) ----
df_norfund <- left_join(df_norfund, df_capitalisation, by = "year")

# Lager kolonne med beregnet klimaandel av kapitalisering til Norfund
df_norfund <- df_norfund %>%
  mutate(mitigation_share_capitalisation_2yr_avg_mnok = mitigation_share_2yr_avg * capitalisation_mnok)

# Fjerner ubrukte objekter
rm(df_orig, df_capitalisation, vec_capitalisations)

# Lagrer som excelfil ----

if(file.exists(here("output")) == FALSE) {
  dir.create(here("output"))
}

write_csv(df_norfund, here("output", "df_norfund.csv"))
