library(tidyverse)
library(lubridate)
library(edwr)

# run MBO query:
#   * Patients - by Medication (Generic)
#       - Facility (Curr): HH HERMANN;HH Trans Care;HH Rehab;HH Clinics
#       - Admit Date: 7/1/2017 - 1/1/2018
#   * Scheduled Queries/consults_vancomycin
#       - Date: 7/1/2017 - 1/1/2018

dir_raw <- "data/raw/mue"
dirr::gzip_files(dir_raw)

pts <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(age >= 18)

mbo_id <- concat_encounters(pts$millennium.id)

# run MBO query:
#   * Labs - Vancomycin

levels <- read_data(dir_raw, "labs-vanc", FALSE) %>%
    as.labs()

pts_levels <- semi_join(pts, levels, by = "millennium.id")

mbo_id <- concat_encounters(pts_levels$millennium.id)

# run MBO query:
#   * Medications - Inpatient - Prompt
#       * Medication (Generic): vancomycin

icu_units <- c(
    "HH CCU",
    "HH CVICU",
    "HH HFIC",
    "HH MICU",
    "HH STIC",
    "HH 7J",
    "HH NVIC",
    "HH TSIC"
)

imu_units <- c(
    "HVI CIMU",
    "HH CVIMU",
    "HH HFIM",
    "HH MIMU",
    "HH SIMU",
    "HH 3CIM",
    "HH NIMU",
    "HH STRK"
)

floor_units <- c(
    "HH 3JP",
    "HH 3CP",
    "HH 4WCP",
    "HH ACE",
    "HH 5ECP",
    "HH 5JP",
    "HH 5WCP",
    "HH 6EJP",
    "HH 6WJP",
    "HH 8NJP",
    "HH EMU",
    "HH NEU",
    "HH 8WJP",
    "HH 9EJP",
    "HH 9WJP",
    "HH REHA",
    "HH TCF"
)

doses <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    filter(med.dose > 0)

doses_icu <- doses %>%
    filter(med.location %in% icu_units)

doses_floor <- anti_join(doses, doses_icu, by = "millennium.id")

doses_duration <- doses_floor %>%
    calc_runtime(cont = FALSE) %>%
    summarize_data(cont = FALSE)

doses_5days <- doses_duration %>%
    filter(duration >= 5 * 24)

doses_3times <- doses %>%
    add_count(millennium.id) %>%
    filter(n >= 3) %>%
    distinct(millennium.id)

pts_doses <- pts_levels %>%
    semi_join(doses_5days, by = "millennium.id") %>%
    semi_join(doses_3times, by = "millennium.id")

mbo_id <- concat_encounters(pts_doses$millennium.id)

# run MBO query
#   * Clinical Events - No Order Id - Prompt
#       - Clinical Event: Creatinine Lvl;Hemodialysis Output Vol;Hemodialysis Output Volume

consults <- read_data(dir_raw, "consults", FALSE) %>%
    rename(millennium.id = `Encounter Identifier`,
           order.datetime = `Date and Time - Order Start`,
           order = `Mnemonic (Primary Generic) FILTER ON`,
           order.location = `Nurse Unit (Order)`) %>%
    format_dates("order.datetime")

pts_consults <- semi_join(pts_doses, consults, by = "millennium.id")

pts_traditional <- anti_join(pts_doses, pts_consults, by = "millennium.id")

