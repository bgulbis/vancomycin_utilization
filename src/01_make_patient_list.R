# make patient list

# run MBO query:
#   * Patients - by Order
#       - Facility (Curr): HH HERMANN
#       - Mnemonic (Primary Generic) FILTER ON: Vancomycin Level; Vancomycin
#       Level Trough; Vancomycin Level Trough Request; Vancomycin Level Request;
#       Vancomycin Level Peak; Vancomycin Level Peak Request
#       - Building (Order): HH HVI;HH Cullen;HH Jones;HH Robertson;HH Hermann

library(tidyverse)
library(edwr)

pts <- read_data("data/raw", "patients", FALSE) %>%
    as.patients() %>%
    arrange(millennium.id)

id <- concat_encounters(pts$millennium.id)

# use results to run MBO queries:
#   * Orders
#       - Mnemonic (Primary Generic) FILTER ON: Vancomycin Level; Vancomycin
#       Level Trough; Vancomycin Level Trough Request; Vancomycin Level Request;
#       Vancomycin Level Peak; Vancomycin Level Peak Request
#   * Labs - Vancomycin

# run EDW query:
#   * Identifiers
#       - Millennium Encounter ID

identifiers <- read_data("data/raw", "identifiers") %>%
    as.id()

edw_id <- concat_encounters(identifiers$pie.id)

# run EDW queries:
#   * Orders - Timing - Prompt without Review
#       - Order Catalog Mnemonic: Vancomycin Level; Vancomycin Level Peak;
#       Vancomycin Level Peak Request; Vancomycin Level Request; Vancomycin
#       Level Trough; Vancomycin Level Trough Request
#   * Clinical Events - Prompt
#       - Clinical Event: 	Vanco Lvl; Vanco Pk; Vanco Tr


timing <- read_data("data/raw", "^timing") %>%
    as.order_timing()
# filter(order.unit %in% hvi)

vanc_levels <- read_data("data/raw", "labs-vanc-edw") %>%
    as.labs(extras = list("order.id" = "`Clinical Event Order ID`",
                          "event.unit" = "`Nurse Unit of Clinical Event`")) %>%
    filter(!is.na(event.unit))

orders <- full_join(timing, vanc_levels, by = c("pie.id", "order.id")) %>%
    arrange(pie.id, order.datetime, lab.datetime)

consults <- read_data("data/raw", "order-actions", FALSE) %>%
    as.order_action()

# orders <- bind_rows(timing["order.id"], vanc_levels["order.id"]) %>%
#     distinct()
#
# id <- concat_encounters(orders$order.id, 950)

# run EDW queries:
#   * Orders - Actions - Source Order ID Prompt
#   * Orders - Details - Source Order ID Prompt

# saveRDS(timing, "data/tidy/order_timing.Rds")
# saveRDS(vanc_levels, "data/tidy/vanc_levels.Rds")
write_rds(orders, "data/tidy/orders.Rds", "gz")
write_rds(consults, "data/tidy/consults.Rds", "gz")
