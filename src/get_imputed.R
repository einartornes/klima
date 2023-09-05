# Get imputed multilateral climate data and store in output folder
# Imputed multilateral climate --------------------------------------------

library(tidyverse)
library(noradstats)
library(writexl)
library(DBI)
library(odbc)
library(dbplyr)
library(here)
library(janitor)

# Credentials are defined in separate .Renviron file.
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "reskunnbasen-dev-sql.database.windows.net",
                 Database = "reskunnbasen-dev-db",
                 UID = Sys.getenv("userid"),
                 PWD = Sys.getenv("pwd"),
                 Port = 1433)

# imputed_multilateral_marker ------------------------------------------

df_imputed_multilateral_marker <- tbl(con, in_schema("data", "imputed_multilateral_marker")) |> 
  collect()

df_imputed_multilateral_marker <- df_imputed_multilateral_marker |> 
  select(agreement_partner, year, share)


# disbursement data -------------------------------------------------------

df_oda <- noradstats::read_aiddata(here("data", "statsys_ten.csv"))

df_oda <- janitor::clean_names(df_oda)

df_oda <- df_oda |> 
  filter(type_of_flow == "ODA") |> 
  filter(type_of_agreement != "Rammeavtale") |> 
  filter(type_of_assistance == "Core contributions to multilat") |> 
  select(agreement_partner, year, disbursed_nok)

# Calculate imputed multilateral climate aid -----------------------------
df_multi <- left_join(df_imputed_multilateral_marker, df_oda, by = c("agreement_partner", "year"))

df_multi <- df_multi |> 
  mutate(climate_aid_mnok = disbursed_nok * share / 1e6)

# Save as csv in output folder
write_csv(df_multi, here("output", "imputed_multilateral_climate.csv"))