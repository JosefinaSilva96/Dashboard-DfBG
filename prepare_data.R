# =============================================================================
# prepare_data.R  (optional helper)
# -----------------------------------------------------------------------------
# Copies outputs from build_dfbg_database_EN.R to the dashboard's data/ folder.
# Run this ONCE (or whenever you regenerate the database) before runApp().
#
# Set CLEAN_DIR to your OUT_DIR from build_dfbg_database_EN.R.
# =============================================================================

CLEAN_DIR <- "C:/Users/wb631166/OneDrive - WBG/Desktop/DfBG/Data/Clean"
CLASS_XLSX <- "C:/Users/wb631166/OneDrive - WBG/Desktop/DfBG/Data/CLASS_2025_10_07.xlsx" # optional

setwd("C:/WBG/GitHub/Dashboard-DfBG")

dest <- file.path(getwd(), "data")
dir.create(dest, showWarnings = FALSE, recursive = TRUE)

files <- c("dfbg_agency.rds", "dfbg_managers.rds", "dfbg_systems.rds")

for (f in files) {
  src <- file.path(CLEAN_DIR, f)
  if (file.exists(src)) {
    file.copy(src, file.path(dest, f), overwrite = TRUE)
    message("Copied: ", f)
  } else {
    warning("File not found (check CLEAN_DIR): ", src)
  }
}

if (file.exists(CLASS_XLSX)) {
  file.copy(CLASS_XLSX, file.path(dest, basename(CLASS_XLSX)), overwrite = TRUE)
  message("Copied income groups: ", basename(CLASS_XLSX))
} else {
  message("CLASS xlsx not found — dashboard will run without income groups.")
}

message("\nDone. Now run:  shiny::runApp()")
shiny::runApp()