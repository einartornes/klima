## Script for making dataframe for Norfunds climate relevant investments and climate share of capitalisation
## Using oda/oof dataset. Script is sourced in script klimatabeller.R
## Written by Einar Tornes

# Load packages and data ----
library(tidyverse)
library(here)
library(janitor)
library(writexl)
library(noradstats)

#devtools::install_github("einartornes/noradstats")

df_orig <- noradstats::read_aiddata(here("data", "statsys_ten.csv"))


# 1. First dataframe on Norfunds climaete relevant investments -------------------------

# inkluderer klimakolonner fra pakken noradstats
df <- noradstats::add_cols_climate(df_orig)

# rydder kolonnenavn
df <- janitor::clean_names(df)

# dataframe med norfunds total beregnede brutto investeringer i utslippsreduksjon per år (ekskluderer admin)
df_nf_total_mitigation_invest <- df %>%
  filter(type_of_flow == "OOF") %>%
  filter(type_of_assistance != "Administration") %>%
  filter(extending_agency == "Norfund") %>%
  filter(year >= 2014) %>%
  group_by(year) %>%
  summarise(total_gross_mitigation_investment_mnok = sum(climate_mitigation_nok_mill_gross_fix))

# dataframe med norfunds totale brutto investeringer per år (ekskluderer admin)
df_nf_total_invest <- df %>%
  filter(type_of_flow == "OOF") %>%
  filter(type_of_assistance != "Administration") %>%
  filter(extending_agency == "Norfund") %>%
  filter(year >= 2014) %>%
  mutate(amounts_extended_mill_fix = if_else(
    amounts_extended_1000_nok < 0, 0,
    amounts_extended_1000_nok / 1000)) %>%
  group_by(year) %>%
  summarise(total_gross_investment_mnok = sum(amounts_extended_mill_fix))

# dataframe med norfunds total beregnede brutto investeringer i klima per år (ekskluderer admin). Til unfccc-rapportering hvor det ikke er kun mitigation
df_nf_total_climate_invest <- df %>%
  filter(type_of_flow == "OOF") %>%
  filter(type_of_assistance != "Administration") %>%
  filter(extending_agency == "Norfund") %>%
  filter(year >= 2014) %>%
  group_by(year) %>%
  summarise(total_gross_climate_investment_mnok = sum(climate_aid_nok_gross_fix / 1000000))

# dataframe som slår sammen de to dataframene over
df_norfund <- left_join(df_nf_total_invest, df_nf_total_mitigation_invest, by = "year")

df_norfund <- left_join(df_norfund, df_nf_total_climate_invest, by = "year")

# kolonne med andel klimarelatert av total investeringer (2 års gjennomsnitt)

df_norfund$climate_mitigation_share_2yr_avg <- vector("double", nrow(df_norfund))

for (i in 2:nrow(df_norfund)) {
  df_norfund$climate_mitigation_share_2yr_avg[i] <- 
    (df_norfund$total_gross_mitigation_investment_mnok[i-1] + df_norfund$total_gross_mitigation_investment_mnok[i]) /
    (df_norfund$total_gross_investment_mnok[i-1] + df_norfund$total_gross_investment_mnok[i])
}

# Fjerner ubrukte objekter
rm(df, df_nf_total_mitigation_invest, df_nf_total_invest, df_nf_total_climate_invest, i)

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
  mutate(climate_mitigation_share_capitalisation_2yr_avg_mnok = climate_mitigation_share_2yr_avg * capitalisation_mnok)

# Fjerner ubrukte objekter
rm(df_orig, df_capitalisation, vec_capitalisations)

# Lagrer som excelfil ----

if(file.exists(here("output")) == FALSE) {
  dir.create(here("output"))
}

write_xlsx(df_norfund, path = here("output", "df_norfund.xlsx"))
