
# Skriv om stopifelse så att vi inte stoppar utan tar bort felaktiga rader och ger varning om man kör lokalt


library(dplyr)

if (require(rccmisc, quietly = TRUE) && !rccmisc::is.inca()) {
    library(incavis)
    rm(list = ls())
    df     <- read.csv2("~/Documents/huvud_hals/atal_blanketter/data/df.csv")
    param  <- list(start = "2009-01-01", slut = "2009-12-31")
}


# Kör hårt!
df_ungrouped <-
    df %>%
    prepare_df() %>%
    transmute(
      CountyOidExtension = lkf2CountyOidExtension(a_lkf),
      behandlingsdatum   = pmin_dates(b_brachystart, b_stralstart, b_medstart,  b_op1dat, b_op2dat),
      ledtid             = as.numeric(behandlingsdatum - as.Date(a_remdat, format = "%Y-%m-%d"))
    ) %>%
    filter(
      !is.na(CountyOidExtension),
      ledtid %in% 0:365,
      between_param_dates(behandlingsdatum)
)

df_lan <- group_by(df_ungrouped, CountyOidExtension )

summarise2 <- function(df) {
    df %>%
    summarise(
      Value                 = median(ledtid, na.rm = TRUE),
      Population            = n(),
      FirstServiceEncounter = min(behandlingsdatum, na.rm = TRUE),
      LastServiceEncounter  = max(behandlingsdatum, na.rm = TRUE)
    )
}


bind_rows(summarise2(df_ungrouped), summarise2(df_lan)) %>%
    vis()


# Make skript file to export to INCA
make_r_script("./inst/test_vis.R", "incavis")
