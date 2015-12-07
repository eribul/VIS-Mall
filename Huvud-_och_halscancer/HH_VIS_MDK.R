
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
    mutate(
        CountyOidExtension = lkf2CountyOidExtension(a_lkf)
    ) %>%
    filter(
        !is.na(CountyOidExtension),
        between_param_dates(a_besldat)
    )


df_lan <- group_by(df_ungrouped,  CountyOidExtension )


summarise2 <- function(df) {
    df %>%
        summarise(
            Denominator           = sum(a_multkonf_beskrivning %in% c("Ja", "Nej")),
            Numerator             = sum(a_multkonf_beskrivning %in% "Ja"),
            FirstServiceEncounter = min(a_besldat, na.rm = TRUE),
            LastServiceEncounter  = max(a_besldat, na.rm = TRUE)
        )
}

bind_rows(summarise2(df_ungrouped), summarise2(df_lan)) %>%
    vis()

# Make skript file to export to INCA
make_r_script("./inst/HH_VIS_MDK_Totalt.R", "incavis")
