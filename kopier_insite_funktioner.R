# =============================================================================
# kopier_insite_funktioner.R — funktioner til kopier_insite_data_ny.R
#
# Håndterer to filnavnsformater i overgangsperioden (fra august 2026):
#   Gammelt: "Indikator - [Underindikator - ] Afdeling[, Afsnit].pdf"
#   Nyt:     "Indikator_med_underscores_FORK[_AFSNIT].pdf"
#
# Nye filnavne bruger organisatoriske forkortelser svarende til
# organisatorisk_navn_kort i Supabase-tabellen tblOrganisationStruktur.
# Mapping forkortelse -> drev-mappenavn hentes fra Supabase ved kørselsstart
# med lokal RDS-cache som fallback, så en Supabase-nedetid ikke blokerer
# en upload-kørsel.
#
# Forudsætter at tidyverse, fs og lubridate er loadet (sker i hovedscriptet).
# =============================================================================

# Konstanter -----

ÆMO_AFDELING <- "Geriatrisk og Palliativ Afdeling GP"

# Drev-mapperne på Y:/ og W:/ er historisk opstået fra de gamle filnavne og
# afviger for disse afdelinger fra organisationstabellens lange navn.
# Besluttet (aug 2026) at beholde de eksisterende mappenavne i
# overgangsperioden, så nye og gamle filer lander i samme mapper.
MAPPENAVN_OVERRIDES <- c(
   BFH  = "BISPEBJERG OG FREDERIKSBERG HOSPITAL",
   DS   = "DS",
   AMA  = "Akutafdelingen",
   AMED = "AMED",
   FM   = "FM",
   CKFF = "Center for Klinisk Forebyggelse og Forskning",
   KBA  = "Klinisk Biokemisk Afdeling",
   KFA  = "Klinisk Farmakologisk Afdeling FARM",
   KFNM = "Klinisk Fysiologisk Nuklearmedicinsk Afdeling KFNM"
)

SUPABASE_CONFIG_STI <- "C:/Users/jrev0004/OneDrive - Region Hovedstaden/4_R/config.yml"
ORG_CACHE_STI <- "V:/BFH/BYH/Datadrevet ledelse/4_R/kopier_insite_data/org_mapping_cache.rds"

# Org-mapping fra Supabase -----

#' Hent organisationsstruktur fra Supabase og byg mapping-objekt
#'
#' Password læses fra SUPABASE_DB_PASSWORD; findes den ikke i miljøet,
#' forsøges BFHddl-pakkens .Renviron som fallback (samme mønster som
#' 00_org_translation_supabase.R i medicinsikkert_hospital).
hent_org_mapping_supabase <- function(config_sti = SUPABASE_CONFIG_STI) {
   cfg <- config::get(file = config_sti)
   db <- cfg$supabase
   if (is.null(db)) {
      stop("config.yml mangler 'supabase'-sektionen (", config_sti, ")")
   }

   password <- Sys.getenv("SUPABASE_DB_PASSWORD", unset = "")
   if (!nzchar(password) && !is.null(cfg$onedrive_r_sti)) {
      bfhddl_renviron <- file.path(cfg$onedrive_r_sti, "BFHddl", ".Renviron")
      if (file.exists(bfhddl_renviron)) {
         readRenviron(bfhddl_renviron)
         password <- Sys.getenv("SUPABASE_DB_PASSWORD", unset = "")
      }
   }
   if (!nzchar(password)) {
      stop("SUPABASE_DB_PASSWORD ikke sat — tilføj til .Renviron og genstart R")
   }

   con <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = db$host, port = db$port, dbname = db$dbname,
      user = db$user, password = password, sslmode = db$sslmode %||% "require"
   )
   on.exit(DBI::dbDisconnect(con), add = TRUE)

   org <- DBI::dbGetQuery(con, '
      SELECT "Id" AS id, "parent_Id" AS parent_id,
             organisatorisk_niveau AS niveau,
             organisatorisk_navn_kort AS kort,
             organisatorisk_navn_langt AS langt
      FROM "tblOrganisationStruktur"') |>
      tibble::as_tibble()

   byg_org_mapping(org)
}

#' Byg mapping-objekt fra rå organisationstabel
#'
#' @return list med to tibbles:
#'   afd:    kort -> mappenavn (niveau 5 = afdelinger + niveau 2 = hospital)
#'   afsnit: kort -> afd_kort (niveau 6-7, opløst via parent-kæden) — bruges
#'           til suffixer uden afdelings-prefix (fx "_KDAGS")
byg_org_mapping <- function(org) {
   afd <- org |>
      filter(niveau %in% c(2, 5), !is.na(kort), nzchar(kort)) |>
      mutate(mappenavn = if_else(
         kort %in% names(MAPPENAVN_OVERRIDES),
         unname(MAPPENAVN_OVERRIDES[kort]),
         langt
      )) |>
      filter(!is.na(mappenavn)) |>
      distinct(kort, .keep_all = TRUE) |>
      select(kort, mappenavn)

   # Gå parent-kæden op til nærmeste niveau-5-forfader (afdeling)
   find_afd_kort <- function(id) {
      for (i in 1:10) {
         række <- org[match(id, org$id), ]
         if (is.na(række$id)) return(NA_character_)
         if (række$niveau == 5) return(række$kort)
         if (is.na(række$parent_id)) return(NA_character_)
         id <- række$parent_id
      }
      NA_character_
   }

   afsnit <- org |>
      filter(niveau %in% c(6, 7), !is.na(kort), nzchar(kort), !kort %in% afd$kort) |>
      mutate(afd_kort = purrr::map_chr(parent_id, find_afd_kort)) |>
      filter(!is.na(afd_kort), afd_kort %in% afd$kort)

   # Afsnits-korte navne der peger på flere afdelinger er tvetydige — udelad
   tvetydige <- afsnit |>
      distinct(kort, afd_kort) |>
      count(kort) |>
      filter(n > 1) |>
      pull(kort)
   if (length(tvetydige) > 0) {
      message("Org-mapping: udelader tvetydige afsnits-forkortelser: ",
              paste(tvetydige, collapse = ", "))
   }
   afsnit <- afsnit |>
      filter(!kort %in% tvetydige) |>
      distinct(kort, .keep_all = TRUE) |>
      select(kort, afd_kort)

   list(afd = afd, afsnit = afsnit)
}

#' Hent org-mapping med cache-fallback
#'
#' Forsøger Supabase først og opdaterer cachen ved succes; falder tilbage
#' til senest gemte cache hvis databasen er utilgængelig.
hent_org_mapping <- function(cache_sti = ORG_CACHE_STI) {
   mapping <- tryCatch(
      hent_org_mapping_supabase(),
      error = function(e) {
         message("Supabase-opslag fejlede (", conditionMessage(e),
                 ") — prøver lokal cache")
         NULL
      }
   )
   if (!is.null(mapping)) {
      saveRDS(mapping, cache_sti)
      message("Org-mapping hentet fra Supabase (", nrow(mapping$afd),
              " afdelinger, ", nrow(mapping$afsnit), " afsnit) — cache opdateret")
      return(mapping)
   }
   if (file.exists(cache_sti)) {
      message("Bruger cachet org-mapping: ", cache_sti)
      return(readRDS(cache_sti))
   }
   stop("Ingen org-mapping tilgængelig: Supabase nede og ingen cache i ", cache_sti)
}

# Filnavns-parsing (begge formater) -----

er_gammelt_format <- function(filnavne) {
   str_detect(filnavne, " - ")
}

#' Gammelt format: afdeling = tekst efter sidste " - " frem til ", " eller "."
#'
#' VIGTIGT: Må kun anvendes på filnavne — aldrig fulde stier. På en fuld sti
#' matcher regexen " - " i "OneDrive - Region Hovedstaden", hvilket var
#' rodårsagen til støjtræet W:/Region Hovedstaden (aug 2026).
extract_afdeling_gammel <- function(filnavne) {
   str_trim(str_remove(str_extract(filnavne, "(?!.* +- )(.*?)((?=, )|(?=\\.))"), "^-"))
}

#' Nyt format: match forkortelses-tokens bagfra (case-sensitivt)
#'
#' "Indikator_..._FORK.pdf"        -> sidste token er afdelings-forkortelse
#' "Indikator_..._FORK_AFSNIT.pdf" -> næstsidste token er forkortelsen
#' "Indikator_..._AFSNIT.pdf"      -> afsnit uden prefix (fx KDAGS) opløses
#'                                    via org-tabellens parent-kæde
#' Ukendt struktur -> NA (rapporteres og springes over på Y:/ og W:/)
parse_afdeling_ny <- function(filnavne, org_mapping) {
   afd_lookup <- setNames(org_mapping$afd$mappenavn, org_mapping$afd$kort)
   afsnit_lookup <- setNames(org_mapping$afsnit$afd_kort, org_mapping$afsnit$kort)

   tokens <- str_split(fs::path_ext_remove(filnavne), "_+")
   sidste <- purrr::map_chr(tokens, \(t) t[length(t)])
   næstsidste <- purrr::map_chr(tokens, \(t) {
      if (length(t) >= 2) t[length(t) - 1] else NA_character_
   })

   dplyr::case_when(
      sidste %in% names(afd_lookup)     ~ unname(afd_lookup[sidste]),
      næstsidste %in% names(afd_lookup) ~ unname(afd_lookup[næstsidste]),
      sidste %in% names(afsnit_lookup)  ~ unname(afd_lookup[unname(afsnit_lookup[sidste])]),
      .default = NA_character_
   )
}

#' Udtræk afdelings-mappenavn af filnavn — begge formater
parse_afdeling <- function(filnavne, org_mapping) {
   gammel <- er_gammelt_format(filnavne)
   ud <- rep(NA_character_, length(filnavne))
   ud[gammel] <- extract_afdeling_gammel(filnavne[gammel])
   ud[!gammel] <- parse_afdeling_ny(filnavne[!gammel], org_mapping)
   ud
}

#' Indikator-mappe til W:-hierarkiet
#'
#' Gammelt format: tekst før første " - " i filnavnet.
#' Nyt format: øverste mappeniveau under lokal_sti — filnavnet indeholder
#' ikke indikatorgruppen, men den lokale mappestruktur gør.
beregn_indikator_mappe <- function(filnavne, fil_stier, lokal_sti) {
   gammel <- er_gammelt_format(filnavne)
   ud <- rep(NA_character_, length(filnavne))
   ud[gammel] <- str_sub(filnavne[gammel], 1L,
                         str_locate(filnavne[gammel], " - ")[, 1] - 1L)
   rel <- str_remove(as.character(fil_stier[!gammel]),
                     fixed(paste0(lokal_sti, "/")))
   rel[rel == as.character(fil_stier[!gammel])] <- NA_character_
   ud[!gammel] <- str_extract(rel, "^[^/]+")
   ud
}

# Sti-beregning og drev-operationer -----

# SharePoint-drevene (WebDAV) fejler med ENAMETOOLONG når mappe + filnavn
# overstiger Windows' grænse på 260 tegn. Lokalt (OneDrive/NTFS) går det godt,
# så lange mappenavne fra den lokale struktur afkortes på netværksdrevene.
# Bemærk: to mapper med samme første MAX_MAPPENAVN_LAENGDE tegn lander i én mappe.
MAX_MAPPENAVN_LAENGDE <- 50

#' Afkort hvert mappeniveau i en sti til maks `maks` tegn
afkort_mappenavne <- function(stier, maks = MAX_MAPPENAVN_LAENGDE) {
   str_replace_all(stier, "[^/]+", \(navn) str_remove(str_sub(navn, 1L, maks), "[\\s.]+$"))
}

# Z:-hierarkiet spejler den lokale mappestruktur (filnavnet bruges ikke)
beregn_ind_stier <- function(filstier, til_drev, lokal_sti) {
   filer <- path_file(filstier)
   fra_stier <- as.character(path_dir(filstier))
   til_mapper <- str_remove(fra_stier, fixed(str_replace_all(lokal_sti, "\\\\", "/")))
   renset <- str_replace_all(til_mapper, "[[~!@#$%^&*{}\\+:<>?;=]]", "")
   renset <- afkort_mappenavne(str_replace_all(str_squish(renset), "/ ", "/"))
   til_stier <- as.character(path(paste0(til_drev, renset)))
   paste0(til_stier, "/", filer)
}

opdater_mapper <- function(filsti) {
   opdateres <- path(paste0(filsti, "_opdateres"))
   file.rename(filsti, opdateres)
   file.rename(opdateres, filsti)
}

scan_mappe <- function(scan_path, recurse = TRUE, type = "file", glob = "*.pdf") {
   scannet <- fs::dir_info(path = scan_path, recurse = recurse, type = type, glob = glob)
   if (type == "file") {
      df <- dplyr::transmute(scannet,
                      fuld_sti = fs::path_tidy(path),
                      filnavn = fs::path_file(path),
                      fil_sti = fs::path_dir(path),
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
beregn_mappe_hierarki <- function(df, drev_prefix, lokal_sti) {
   base <- df %>% mutate(mappe = afkort_mappenavne(str_replace(fil_sti, lokal_sti, drev_prefix)))
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
