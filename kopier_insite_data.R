library(tidyverse)
library(stringr)
library(fs)
library(future)
library(tictoc)

tic()
plan(multiprocess, workers = availableCores() - 1)

filer <- dir_ls("Y:/", recurse = TRUE, type = "file", glob = "*.pdf")
# filer <- dir_ls("Z:/", recurse = TRUE, type = "file", glob = "*.pdf")
# filsti <- filer[1500]

kopier_filer <- function(filsti, til_drev) {
   fra_drev <- str_sub(filsti, 1L, 3L) %>% as.character()
   fil <- path_file(filsti)
   sti <- path_dir(filsti)

   til_mappe <- paste0(str_replace(sti, fra_drev, til_drev), "/", str_sub(fil, 1L, str_locate(fil, " - ")[1]-1))


   dir_create(til_mappe)
   file_copy(filsti, paste0(til_mappe,"/", fil), overwrite = TRUE)
   message(paste(fil, "\n"))
}

# map(filer, kopier_filer, "W:/")
furrr::future_map(filer, kopier_filer, "W:/")

plan(sequential)
toc()

# fil <- filer[2800]
# locate <- str_locate_all(fil, "/")
# str_sub(fil, locate[[1]][length(locate[[1]])]+1, -1L)
# str_locate_all(path_file(fil), " - ")[[1]][length(str_locate_all(path_file(fil), " - ")[[1]])]+1

# afdelingsseperator <-  str_locate_all(path_file(fil), " - ")[[1]][length(str_locate_all(path_file(fil), " - ")[[1]])]
# afdeling <- str_split(str_sub(path_file(path_ext_remove(fil)),
#         afdelingsseperator+1,
#         -1L),
#         ", ")[[1]][1]
#
# indikator <- str_replace_all(str_sub(path_file(path_ext_remove(fil)), 1, afdelingsseperator-3), " - ", "/")
#
#
# path_split(fil)
# kpath_join(str_replace_all(fil1, " - ", "/"))
