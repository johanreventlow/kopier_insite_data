# =============================================================================
# test_kopier_insite_funktioner.R — tests af filnavns-parsing (begge formater)
#
# Kør: '/c/Program Files/R/R-4.6.0/bin/Rscript.exe' test_kopier_insite_funktioner.R
#
# Henter org-mapping fra Supabase (read-only) og validerer parse-funktionerne
# mod kendte eksempler samt en tørkørsel over seneste uge-mappe.
# =============================================================================

suppressPackageStartupMessages({
   library(tidyverse)
   library(fs)
})

source("V:/BFH/BYH/Datadrevet ledelse/4_R/kopier_insite_data/kopier_insite_funktioner.R",
       encoding = "UTF-8")

org_mapping <- hent_org_mapping()

# --- Gammelt format ---
stopifnot(identical(
   parse_afdeling(c(
      "Akutte genindlæggelser - Geriatrisk og Palliativ Afdeling GP, G16.pdf",
      "Akutte genindlæggelser - BISPEBJERG OG FREDERIKSBERG HOSPITAL.pdf",
      "Akutte ambulante - andel der indlægges - Akutafdelingen, AVA-G.pdf",
      "Afsluttede ophold - DS, DS21.pdf",
      "30 dages overlevelse efter udskrivelse - Anæstesiafdeling Z, FIMA.pdf"
   ), org_mapping),
   c("Geriatrisk og Palliativ Afdeling GP",
     "BISPEBJERG OG FREDERIKSBERG HOSPITAL",
     "Akutafdelingen",
     "DS",
     "Anæstesiafdeling Z")
))
cat("OK: gammelt format\n")

# --- Nyt format ---
stopifnot(identical(
   parse_afdeling(c(
      "Korrekt_samlet_scanning_BFH.pdf",
      "Korrekt_samlet_scanning_GP_G16.pdf",
      "Aminoglycosid_antibakterika_J01G__AMA.pdf",
      "Aminoglycosid_antibakterika_J01G__KDAGS.pdf",
      "Korrekt_samlet_scanning_K_KDAGD.pdf",
      "Korrekt_samlet_scanning_Vikar.pdf",
      "Patient-ID_scannet_ved_medicinadministration_Z_FIMA.pdf",
      "Helt_ukendt_struktur_XYZQ.pdf"
   ), org_mapping),
   c("BISPEBJERG OG FREDERIKSBERG HOSPITAL",
     "Geriatrisk og Palliativ Afdeling GP",
     "Akutafdelingen",
     "Abdominalcenter K",          # afsnit KDAGS uden afd-prefix -> parent-kæde
     "Abdominalcenter K",          # nyt/ukendt afsnit, kendt afd-forkortelse
     "Vikarkorpset",
     "Anæstesiafdeling Z",
     NA)
))
cat("OK: nyt format\n")

# --- Indikator-mappe (W:-hierarkiet) ---
lokal <- "C:/Users/x/ddl/2026-34"
stopifnot(identical(
   beregn_indikator_mappe(
      c("Akutte ambulante - andel der indlægges - Akutafdelingen, AVA-G.pdf",
        "Korrekt_samlet_scanning_BFH.pdf",
        "Korrekt_samlet_scanning_BFH.pdf"),
      c(paste0(lokal, "/Akutte ambulante/andel der indlægges"),
        paste0(lokal, "/Medicinsikkert hospital/Korrekt samlet scanning"),
        lokal),   # fil i roden af lokal_sti -> NA (kan ikke placeres)
      lokal),
   c("Akutte ambulante", "Medicinsikkert hospital", NA)
))
cat("OK: indikator-mappe\n")

# --- Tørkørsel: fuldt inventar for seneste uge ---
ddl_uge <- "C:/Users/jrev0004/OneDrive - Region Hovedstaden/ddl/2026-34"
if (dir_exists(ddl_uge)) {
   filer <- dir_ls(ddl_uge, recurse = TRUE, type = "file", glob = "*.pdf")
   navne <- path_file(filer)
   afd <- parse_afdeling(navne, org_mapping)

   nyt <- !er_gammelt_format(navne)
   cat("\n=== Tørkørsel ", ddl_uge, " ===\n", sep = "")
   cat("Filer i alt:", length(navne),
       "| gammelt format:", sum(!nyt),
       "| nyt format:", sum(nyt), "\n")

   cat("\nAfdelings-fordeling, nyt format:\n")
   print(sort(table(afd[nyt], useNA = "ifany"), decreasing = TRUE))

   ukendte <- unique(navne[is.na(afd)])
   cat("\nUparsebare filnavne:", length(ukendte), "\n")
   if (length(ukendte) > 0) print(head(ukendte, 20))

   ind <- beregn_indikator_mappe(navne, as.character(path_dir(filer)), path_tidy(ddl_uge))
   cat("Filer uden indikator-mappe (W:):", sum(is.na(ind)), "\n")
}

cat("\nAlle assertions bestået\n")
