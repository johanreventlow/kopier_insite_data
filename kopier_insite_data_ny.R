library(tidyverse)
library(fs)
library(tictoc)
library(furrr)
library(lubridate)

# Setup parallel workers -----
n_workers <- min(parallelly::availableCores() - 1, 8)
plan(multisession, workers = n_workers)
message(paste("Parallel workers:", n_workers))

# Konstanter -----
ÆMO_AFDELING <- "Geriatrisk og Palliativ Afdeling GP"

# Funktioner -----

# Vektoriseret afdelingsnavns-udtræk (genbrugt i flere funktioner)
extract_afdeling <- function(tekst) {
   str_trim(str_remove(str_extract(tekst, "(?!.* +- )(.*?)((?=, )|(?=\\.))"), "^-"))
}

# Vektoriserede sti-beregninger (rene funktioner, ingen side effects)
beregn_ind_stier <- function(filstier, til_drev) {
   filer <- path_file(filstier)
   fra_stier <- as.character(path_dir(filstier))
   til_mapper <- str_remove(fra_stier, fixed(str_replace_all(lokal_sti, "\\\\", "/")))
   renset <- str_replace_all(til_mapper, "[[~!@#$%^&*{}\\+:<>?;=]]", "")
   renset <- str_replace_all(str_squish(renset), "/ ", "/")
   til_stier <- as.character(path(paste0(til_drev, renset)))
   paste0(til_stier, "/", filer)
}

beregn_afd_stier <- function(filstier, til_drev) {
   filer <- path_file(filstier)
   paste0(til_drev, extract_afdeling(filer), "/", filer)
}

beregn_æmo_afd_stier <- function(filstier, til_drev) {
   filer <- path_file(filstier)
   paste0(til_drev, ÆMO_AFDELING, "/", filer)
}

beregn_afd_ind_stier <- function(filstier, til_drev) {
   filer <- path_file(filstier)
   mapper_ind <- str_sub(filer, 1L, str_locate(filer, " - ")[, 1] - 1)
   paste0(til_drev, extract_afdeling(filstier), "/", mapper_ind, "/", filer)
}

beregn_æmo_afd_ind_stier <- function(filstier, til_drev) {
   filer <- path_file(filstier)
   mapper_ind <- str_sub(filer, 1L, str_locate(filer, " - ")[, 1] - 1)
   paste0(til_drev, ÆMO_AFDELING, "/", mapper_ind, "/", filer)
}

opdater_mapper <- function(filsti) {
   opdateres <- path(paste0(filsti, "_opdateres"))
   file.rename(filsti, opdateres)
   file.rename(opdateres, filsti)
}

scan_mappe <- function(scan_path, recurse = TRUE, type = "file", glob = "*.pdf") {
   scannet <- fs::dir_info(path = scan_path, recurse = recurse, type = type, glob = glob)
   afd_regex <- "(?!.* +- )(.*?)((?=, )|(?=\\.))"
   if (type == "file") {
      df <- dplyr::transmute(scannet,
                      fuld_sti = fs::path_tidy(path),
                      filnavn = fs::path_file(path),
                      fil_sti = fs::path_dir(path),
                      afdeling = stringr::str_trim(stringr::str_remove(
                         stringr::str_extract(filnavn, afd_regex), "^-")),
                      indikator = stringr::str_trim(stringr::str_extract(fs::path_file(path), "[^-]*")),
                      modification_time = lubridate::as_datetime(as.character(modification_time)),
                      change_time = lubridate::as_datetime(as.character(change_time)))
   } else {
      df <- dplyr::transmute(scannet,
                      fuld_sti = fs::path_tidy(path),
                      filnavn = fs::path_file(path),
                      modification_time = lubridate::as_datetime(as.character(modification_time)),
                      change_time = lubridate::as_datetime(as.character(change_time)))
   }
   return(df)
}

# Hjælpefunktion til mappe-hierarki beregning (genbrugt for Z:/ og W:/)
beregn_mappe_hierarki <- function(df, drev_prefix) {
   base <- df %>% mutate(mappe = str_replace(fil_sti, lokal_sti, drev_prefix))
   bind_rows(
      base,
      base %>% mutate(mappe = dirname(mappe)),
      base %>% mutate(mappe = dirname(mappe)) %>% mutate(mappe = dirname(mappe))
   ) %>%
      select(mappe) %>%
      mutate(mappe = str_squish(mappe)) %>%
      unique()
}

omdoeb_afd_navn_i_filer <- function(alle_filer, fra, til) {
   filer_til_omdoeb <- alle_filer %>% fs::path_filter(glob = paste0("*", fra, "*"))
   if (length(filer_til_omdoeb) > 0) {
      file_move(filer_til_omdoeb, str_replace_all(filer_til_omdoeb, fra, til))
   }
}

# Vælg lokal mappe -----
lokal_sti <- path_tidy(DescToolsAddIns::dir.choose(default = "C:/Users/jrev0004/OneDrive - Region Hovedstaden/ddl"))

# Omdøb afdelingsnavne (scan én gang, filtrer 3 gange) -----
alle_pdf_filer <- fs::dir_ls(lokal_sti, recurse = TRUE, type = "file", glob = "*.pdf")
omdoeb_afd_navn_i_filer(alle_pdf_filer, "Arbejds- og Miljømedicinsk Afdeling", "AMED")
omdoeb_afd_navn_i_filer(alle_pdf_filer, "Dermato-Venerologisk Afdeling og Videncenter for Sårheling DS", "DS")
omdoeb_afd_navn_i_filer(alle_pdf_filer, "Dermato-Venerologisk Afdeling og Videncenter for Sårheling D S", "DS")

# Scan alle drev parallelt -----
tic("Scanning alle drev")
f_lokale  <- future(scan_mappe(lokal_sti))
f_ind     <- future(scan_mappe("Z:/"))
f_afd     <- future(scan_mappe("Y:/"))
f_afd_ind <- future(scan_mappe("W:/"))

fil_info_lokale  <- value(f_lokale)
fil_info_ind     <- value(f_ind)
fil_info_afd     <- value(f_afd)
fil_info_afd_ind <- value(f_afd_ind)

# ÆMO er subset af lokale — ingen ekstra scan nødvendig
fil_info_æmo <- fil_info_lokale %>% filter(str_detect(filnavn, "ÆMO"))
toc()

message(paste("Filer fundet — Lokal:", nrow(fil_info_lokale),
              "| Z:/:", nrow(fil_info_ind),
              "| Y:/:", nrow(fil_info_afd),
              "| W:/:", nrow(fil_info_afd_ind),
              "| ÆMO:", nrow(fil_info_æmo)))

# Beregn filer der skal uploades -----
# Indikatorer (Z:/)
ind_filer_der_mangler <- fil_info_lokale %>%
   anti_join(fil_info_ind, by = c("filnavn", "modification_time", "change_time"))

ind_filer_der_skal_opdateres <- fil_info_ind %>%
   semi_join(ind_filer_der_mangler, by = "filnavn")

mappe_ind_der_skal_opdateres <- beregn_mappe_hierarki(ind_filer_der_mangler, "Z:")

# Afdeling (Y:/)
afd_filer_der_mangler <- fil_info_lokale %>%
   anti_join(fil_info_afd, by = c("filnavn", "modification_time", "change_time"))

afd_filer_der_skal_opdateres <- fil_info_afd %>%
   semi_join(afd_filer_der_mangler, by = "filnavn")

afd_filer_der_skal_slettes <- fil_info_afd %>%
   anti_join(fil_info_ind, by = "filnavn")

mappe_afd_der_skal_opdateres <- afd_filer_der_mangler %>%
   mutate(mappe = paste0("Y:/", afdeling)) %>%
   select(mappe, afdeling) %>%
   unique()

# Afdeling/Indikator (W:/)
afd_ind_filer_der_mangler <- fil_info_lokale %>%
   anti_join(fil_info_afd_ind, by = c("filnavn", "modification_time", "change_time"))

afd_ind_filer_der_skal_opdateres <- fil_info_afd_ind %>%
   semi_join(afd_ind_filer_der_mangler, by = "filnavn")

afd_ind_filer_der_skal_slettes <- fil_info_afd_ind %>%
   anti_join(fil_info_ind, by = "filnavn")

mappe_afd_ind_der_skal_opdateres <- {
   base <- afd_ind_filer_der_mangler %>%
      mutate(mappe = str_replace(fil_sti, lokal_sti, paste0("W:/", afdeling)))
   bind_rows(
      base,
      base %>% mutate(mappe = dirname(mappe)),
      base %>% mutate(mappe = dirname(mappe)) %>% mutate(mappe = dirname(mappe))
   ) %>%
      select(mappe) %>%
      mutate(mappe = str_squish(mappe)) %>%
      unique()
}

message(paste("Filer der mangler — Z:/:", nrow(ind_filer_der_mangler),
              "| Y:/:", nrow(afd_filer_der_mangler),
              "| W:/:", nrow(afd_ind_filer_der_mangler)))

save.image("insite_filer.RData")

# === Batch sletning ===
tic("Sletning")
alle_sletninger <- c(
   ind_filer_der_skal_opdateres$fuld_sti,
   afd_filer_der_skal_slettes$fuld_sti,
   afd_filer_der_skal_opdateres$fuld_sti,
   afd_ind_filer_der_skal_slettes$fuld_sti,
   afd_ind_filer_der_skal_opdateres$fuld_sti
)
if (length(alle_sletninger) > 0) {
   message(paste("Sletter", length(alle_sletninger), "filer"))
   file.remove(alle_sletninger)
}
toc()

# === Beregn target-stier ===
tic("Beregn stier + opret mapper")

empty_targets <- tibble(source = character(), target = character())
ind_targets <- afd_targets <- æmo_afd_targets <- afd_ind_targets <- æmo_afd_ind_targets <- empty_targets

if (nrow(ind_filer_der_mangler) > 0) {
   ind_targets <- ind_filer_der_mangler %>%
      transmute(source = fuld_sti, target = beregn_ind_stier(fuld_sti, "Z:/"))
}

if (nrow(afd_filer_der_mangler) > 0) {
   afd_targets <- afd_filer_der_mangler %>%
      transmute(source = fuld_sti, target = beregn_afd_stier(fuld_sti, "Y:/"))
}

if (nrow(fil_info_æmo) > 0) {
   æmo_afd_targets <- fil_info_æmo %>%
      transmute(source = fuld_sti, target = beregn_æmo_afd_stier(fuld_sti, "Y:/"))

   æmo_afd_ind_targets <- fil_info_æmo %>%
      transmute(source = fuld_sti, target = beregn_æmo_afd_ind_stier(fuld_sti, "W:/"))
}

if (nrow(afd_ind_filer_der_mangler) > 0) {
   afd_ind_targets <- afd_ind_filer_der_mangler %>%
      transmute(source = fuld_sti, target = beregn_afd_ind_stier(fuld_sti, "W:/"))
}

all_copies <- bind_rows(ind_targets, afd_targets, æmo_afd_targets, afd_ind_targets, æmo_afd_ind_targets)

# Parallel opret alle unikke mapper på netværksdrev
if (nrow(all_copies) > 0) {
   unique_dirs <- unique(dirname(all_copies$target))
   future_walk(unique_dirs, fs::dir_create, .progress = TRUE)
}
toc()

# === Parallel kopiering ===
tic("Kopiering")
if (nrow(all_copies) > 0) {
   message(paste("Kopierer", nrow(all_copies), "filer",
                 "(Z:/:", nrow(ind_targets),
                 "| Y:/:", nrow(afd_targets) + nrow(æmo_afd_targets),
                 "| W:/:", nrow(afd_ind_targets) + nrow(æmo_afd_ind_targets), ")"))
   future_walk2(
      all_copies$source,
      all_copies$target,
      ~fs::file_copy(.x, .y, overwrite = TRUE),
      .progress = TRUE
   )
} else {
   message("Ingen filer at kopiere")
}
toc()

# === Opdater mapper ===
tic("Opdater mapper")
alle_mapper <- unique(c(
   mappe_ind_der_skal_opdateres$mappe,
   mappe_afd_der_skal_opdateres$mappe,
   mappe_afd_ind_der_skal_opdateres$mappe
))
if (length(alle_mapper) > 0) {
   message(paste("Opdaterer", length(alle_mapper), "mapper"))
   future_walk(alle_mapper, opdater_mapper, .progress = TRUE)
}
toc()

# Cleanup -----
plan(sequential)
message("Færdig!")
