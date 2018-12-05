library(tidyverse)
library(lubridate)
library(edwr)
library(openxlsx)

# run MBO query
#   * Scheduled Queries/consults_vancomycin

dir_raw <- "data/raw/consults"

dirr::gzip_files(dir_raw)

consults <- read_data(dir_raw, "consults", FALSE) %>%
    rename(
        millennium.id = `Encounter Identifier`,
        order.datetime = `Date and Time - Order Start`,
        order = `Mnemonic (Primary Generic) FILTER ON`,
        order.location = `Nurse Unit (Order)`
    ) %>%
    format_dates("order.datetime") %>%
    filter(
        order.datetime >= mdy("7/1/2017", tz = "US/Central"),
        order.datetime < mdy("12/1/2018", tz = "US/Central")
    ) %>%
    mutate(order.day = floor_date(order.datetime, unit = "days")) %>%
    distinct(millennium.id, order.day, .keep_all = TRUE)

monthly <- consults %>%
    mutate(order.month = floor_date(order.datetime, unit = "month")) %>%
    distinct(millennium.id, order.month) %>%
    count(order.month) %>%
    mutate_at("order.month", as.Date) %>%
    rename(
        month = order.month,
        num_patients = n
    )

write.xlsx(
    monthly,
    "/home/brian/Public/W_Pharmacy/Dosing Services/vancomycin_utilization.xlsx"
)
