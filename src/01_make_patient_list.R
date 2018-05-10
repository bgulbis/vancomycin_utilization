library(tidyverse)
library(lubridate)
library(edwr)
library(openxlsx)

# run MBO query:
#   * Patients - by Medication (Generic)
#       - Facility (Curr): HH HERMANN;HH Trans Care;HH Rehab;HH Clinics
#       - Admit Date: 7/1/2017 - 5/1/2018
#   * Scheduled Queries/consults_vancomycin
#       - Date: 7/1/2017 - 5/1/2018

dir_raw <- "data/raw/mue"
dirr::gzip_files(dir_raw)

pts <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients() %>%
    filter(age >= 18)

mbo_id <- concat_encounters(pts$millennium.id)

# run MBO query:
#   * Labs - Vancomycin

levels <- read_data(dir_raw, "labs-vanc", FALSE) %>%
    as.labs() %>%
    tidy_data()

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

doses <- read_data(dir_raw, "meds-inpt", FALSE) %>%
    as.meds_inpt() %>%
    filter(med.dose > 0)

doses_icu <- doses %>%
    filter(med.location %in% icu_units)
    # mutate(orig.order.id = order.parent.id) %>%
    # mutate_at("orig.order.id", na_if, y = 0L) %>%
    # mutate_at("orig.order.id", funs(coalesce(., order.id))) %>%
    # add_count(millennium.id) %>%
    # filter(n > 2)

doses_duration <- doses %>%
    anti_join(doses_icu, by = "millennium.id") %>%
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
#   * Clinical Events - Measures
#   * Clinical Events - No Order Id - Prompt
#       - Clinical Event: Creatinine Lvl;Hemodialysis Output Vol;Hemodialysis Output Volume;MRSA by PCR;WBC
#   * Demographics
#   * Identifiers - by Millennium Encounter Id
#   * Vitals - Temp

consults <- read_data(dir_raw, "consults", FALSE) %>%
    rename(millennium.id = `Encounter Identifier`,
           order.datetime = `Date and Time - Order Start`,
           order = `Mnemonic (Primary Generic) FILTER ON`,
           order.location = `Nurse Unit (Order)`) %>%
    format_dates("order.datetime")

measures <- read_data(dir_raw, "measures", FALSE) %>%
    as.events(order_var = FALSE) %>%
    mutate_at("event.result", as.numeric)

height <- measures %>%
    filter(
        event == "height",
        event.result.units == "cm"
    ) %>%
    arrange(millennium.id, desc(event.datetime)) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(
        millennium.id,
        height = event.result
    )

weight <- measures %>%
    filter(
        event == "weight",
        event.result.units == "kg"
    ) %>%
    arrange(millennium.id, desc(event.datetime)) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(
        millennium.id,
        weight = event.result
    )

events <- read_data(dir_raw, "events_", FALSE) %>%
    as.events(order_var = FALSE)

renal_hd <- events %>%
    filter(str_detect(event, "hemodialysis")) %>%
    distinct(millennium.id) %>%
    mutate(hd = TRUE)

pts_doses_hd <- pts_doses %>%
    left_join(renal_hd, by = "millennium.id") %>%
    mutate_at("hd", funs(coalesce(., FALSE)))

pts_consults <- pts_doses_hd %>%
    semi_join(consults, by = "millennium.id") %>%
    mutate(group = "consult")

pts_traditional <- pts_doses_hd %>%
    anti_join(pts_consults, by = "millennium.id") %>%
    mutate(group = "traditional")

demog <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics()

id <- read_data(dir_raw, "identifiers", FALSE) %>%
    as.id() %>%
    # mutate_at("fin", as.numeric) %>%
    add_count(millennium.id) %>%
    filter(n == 1)

data_patients <- pts_consults %>%
    bind_rows(pts_traditional) %>%
    left_join(
        demog,
        by = c(
            "millennium.id",
            "age",
            "facility",
            "visit.type"
        )
    ) %>%
    inner_join(id, by = "millennium.id") %>%
    left_join(height, by = "millennium.id") %>%
    left_join(weight, by = "millennium.id") %>%
    select(
        fin,
        group,
        age,
        gender,
        height,
        weight,
        length.stay,
        hd,
        millennium.id
    )

vanc_duration <- doses %>%
    semi_join(data_patients, by = "millennium.id") %>%
    group_by(millennium.id) %>%
    arrange(millennium.id, med.datetime) %>%
    summarize_at("med.datetime", funs(first, last))

tmp_scr <- events %>%
    semi_join(data_patients, by = "millennium.id") %>%
    left_join(vanc_duration, by = "millennium.id") %>%
    filter(
        event == "creatinine lvl",
        event.datetime >= first - hours(24),
        event.datetime <= last + hours(12)
    ) %>%
    select(-(event.id:last)) %>%
    mutate_at("event.result", as.numeric) %>%
    filter(!is.na(event.result))

tmp_doses <- doses %>%
    semi_join(data_patients, by = "millennium.id") %>%
    select(
        millennium.id,
        event.datetime = med.datetime,
        event = med,
        event.result = med.dose,
        event.result.units = med.dose.units,
        event.location = med.location
    )

tmp_levels <- levels %>%
    semi_join(data_patients, by = "millennium.id") %>%
    select(
        millennium.id,
        event.datetime = lab.datetime,
        event = lab,
        event.result = lab.result,
        event.result.units = lab.result.units,
        event.location = lab.draw.location
    )

# combine doses, levels, SCr
data_doses_levels <- tmp_scr %>%
    bind_rows(tmp_doses, tmp_levels) %>%
    arrange(millennium.id, event.datetime) %>%
    distinct(
        millennium.id,
        event.datetime,
        event,
        event.result,
        .keep_all = TRUE
    ) %>%
    mutate_at(
        "event",
        str_replace_all,
        pattern = c(
            "creatinine lvl" = "scr",
            "vancomycin" = "dose",
            "vanco tr" = "level",
            "vanco lvl" = "level"
        )
    ) %>%
    left_join(id, by = "millennium.id") %>%
    select(
        fin,
        everything(),
        -millennium.id,
        -n
    )

data_wbc <- events %>%
    filter(event == "wbc") %>%
    mutate_at("event.result", as.numeric) %>%
    left_join(vanc_duration, by = "millennium.id") %>%
    filter(
        !is.na(event.result),
        event.datetime >= first - hours(48),
        event.datetime <= last + hours(12)
    ) %>%
    inner_join(id, by = "millennium.id") %>%
    select(
        fin,
        event,
        event.result,
        event.result.units,
        event.location
    )

data_mrsa <- events %>%
    filter(event == "mrsa by pcr") %>%
    inner_join(id, by = "millennium.id") %>%
    select(
        fin,
        event,
        event.result,
        event.location
    )

data_temp <- read_data(dir_raw, "vitals", FALSE) %>%
    as.vitals() %>%
    left_join(vanc_duration, by = "millennium.id") %>%
    filter(
        !is.na(vital.result),
        vital.datetime >= first - hours(48),
        vital.datetime <= last + hours(12)
    ) %>%
    inner_join(id, by = "millennium.id") %>%
    select(
        fin,
        event = vital,
        event.result = vital.result,
        event.result.units = vital.result.units,
        event.location = vital.location
    )


# export data ------------------------------------------

data_patients %>%
    select(-millennium.id) %>%
    write.xlsx("data/external/patients.xlsx")

data_doses_levels %>%
    write.xlsx("data/external/doses_levels.xlsx")

data_wbc %>%
    write.xlsx("data/external/wbc.xlsx")

data_mrsa %>%
    write.xlsx("data/external/mrsa-pcr.xlsx")

data_temp %>%
    write.xlsx("data/external/temperatures.xlsx")
