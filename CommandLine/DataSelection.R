library("dplyr")
library("optparse")
library("zoo")

option_list <- list(
    make_option(c("-f", "--file"),
        type = "character", default = NA,
        help = "Specify datafile in .rds format e.g. /home/user/data/donations.rds",
        metavar = "character"
    ),
    make_option(c("-l", "--location"),
                type = "character", default = NA,
                help = "Specify location to save data selections in e.g. \"/home/user/data/\"",
                metavar = "character"
    )
)

opt_parser <- OptionParser(option_list = option_list)

opt <- parse_args(opt_parser)
#opt <- parse_args(opt_parser, args = c("--file=donations_preprocessed.rds"))
# opt <- parse_args(opt_parser, args = c("--file=filtered_donations.rds"))
# opt <- parse_args(opt_parser, args = c("--file=testdonations.rds"))
# opt <- parse_args(opt_parser, args = c("--file=selection/fulldata.rds"))
if (is.na(opt$file)) {
    stop("Please specify a datafile like so Rscript Analysis_code.R -f dataset.rds")
} else {
    filename <- opt$file
}

if(is.na(opt$location)){
  stop("Please specify a location to save your data selections to, like \"/home/user/data/\"")
} else{
  FolderName <- opt$location
}

# Change here!
# Change this according to the dataset. This is for the Netherlands, Sanquin
donor_id_column <- "KeyID" # column with unique key for each donor
donation_id_column <- "EINnummer" # column with unique key for each donation
donation_kind_column <- "Donatiesoortcode" # column with the kind of donation for example donation_kind_column=("Full blood", "Plasma", ...)
date_column <- "Donatiedatum" # column with date of donation
hb_column <- "Hb" # column with Hb measurement in mmol/L
fer_column <- "Ferritine" # column with ferritine measurement in microg/L
sex_column <- "Geslacht" # male or female
birth_date_column <- "Geboortedatum"

fullblood_code <- "Volbloed" # the code that specifies full blood donors in donation_kind_column
new_donor_code <- "Nieuwe donorkeuring" # code that specifies new donors (intake) in donation_kind_column
male_code <- "M"
female_code <- "F"

# Might need to be changed? No?
male_hb_deferral_limit <- 8.4
female_hb_deferral_limit <- 7.4
ferritin_deferral_limit_1 <- 30
# ferritin_deferral_limit_2 <- 15

###

if (endsWith(filename, "donaties.rds") | endsWith(filename, "donations.rds")) {
    print(paste("Reading file", filename))
    print("Doing a lot of processing on raw donations file")
    donations <- readRDS(filename)
    #saveRDS(head(donations, 10000), "testdonations.rds")
    print(paste("rows =", nrow(donations)))
    print("Getting only new full blood donors")
    donations <- donations %>% filter((.data[[donation_kind_column]] == fullblood_code) |
        (.data[[donation_kind_column]] == new_donor_code))

    # select only donors that have at least 1 Ferritine measurement
    # this cleans up nicely
    print(paste("rows =", nrow(donations)))
    print("Getting only donors with at least 1 ferritine measurement and more than 1 donation")
    donations <- donations %>%
        group_by(.data[[donor_id_column]]) %>%
        filter(any(!is.na(.data[[fer_column]]) & n() > 1))

    # saveRDS(donations, "donations_filtered.rds")

    print(paste("rows =", nrow(donations)))
    print("Getting number of ferr measurements")
    numfer <- donations %>%
        arrange(
            .data[[donor_id_column]],
            as.Date(donations[[date_column]], format = "%Y-%m-%d")
        ) %>%
        filter(!is.na(.data[[fer_column]])) %>%
        group_by(.data[[donor_id_column]]) %>%
        mutate(numfer = row_number()) %>%
        ungroup() %>%
        dplyr::select(all_of(c(donation_id_column, "numfer")))

    print("merging") # this is slow
    # donations <- merge(donations, numfer, by = donation_id_column, all.x = T)
    donations <- left_join(donations, numfer, by = donation_id_column) # faster?
    print(paste("rows =", nrow(donations)))
    rm(numfer)

    print("filtering out Hb = nan")
    donations <- donations %>% mutate(
        !!hb_column := ifelse(.data[[hb_column]] == 0 | .data[[hb_column]] > 990, NA, .data[[hb_column]]) # nolint
    )
    donations <- donations %>% filter(!is.na(.data[[hb_column]]))
    print(paste("rows =", nrow(donations)))
    # Lot of stuff going on here
    # Basically select follow-up donations and reference donations
    # also reactivated donors that had not donoations in last 2 years
    print("Getting Reference and follow up hbs") # also slow
    donations <- donations %>%
        arrange(.data[[donor_id_column]], as.Date(.data[[date_column]], format = "%Y-%m-%d")) %>% # nolint
        group_by(.data[[donor_id_column]]) %>%
        mutate(numdon = row_number()) %>%
        # arrange(.data[[donor_id_column]], by_group = T) %>%
        mutate( # nolint
            prev_hb = lag(.data[[hb_column]]), # for possible no deferral before
            prev_fer = lag(.data[[fer_column]]),
            # RefDonDate = min(.data[[date_column]]), # reference donation date
            prev_DonDate = lag(.data[[date_column]]), # nolint, previous donation date
            deltaTime = as.numeric(.data[[date_column]] - prev_DonDate), # nolint, time since last donation
            Elig = case_when( # R=Reference, F=Follow up
                deltaTime <= (2 * 365.25) & !is.na(.data[[fer_column]]) & !is.na(.data[[hb_column]]) ~ "F", # nolint, follow-up with ferritine
                deltaTime > (2 * 365.25) ~ "R", # nolint, reference
                .data[[donation_kind_column]] == new_donor_code ~ "R",
                numdon == 1 ~ "R", numdon == 2 ~ "R"
            ) # nolint
        ) # nolint



    # Mean of first two measurements is used here
    # maybe it is better to just use the first one?
    print("calc reference Hb from 1st two donations")
    ReferenceHb <- donations %>%
        filter(numdon == 1 | numdon == 2) %>%
        group_by(.data[[donor_id_column]]) %>%
        mutate(RefHb = mean(.data[[hb_column]])) %>%
        ungroup() %>%
        filter(numdon == 1) %>%
        # filter(numdon == 1 & !is.na(RefHb)) %>%
        dplyr::select(all_of(c(donor_id_column, "RefHb")))

    print("Joining again")
    donations <- inner_join(donations, ReferenceHb, by = donor_id_column)
    rm(ReferenceHb)
    print(paste("rows =", nrow(donations)))

    # For reactivated donors set RefHb to Hb
    # Note this is not the mean of 2 measurements!
    print("Get reactivated donor reference hb")
    # this is convoluted
    # First set reactivated to True if Elig==R and not first (or intake!) donation
    # For F set to Nan
    donations <- donations %>% mutate(Reactivated = ifelse(Elig == "R" & numdon > 2, T, ifelse(Elig == "F", NaN, F))) # nolint
    # Set Reactivated for all next visists for a donor, including the F(ollow up) visit using na.locf
    # locf = Last Observation Carried Forward
    donations <- donations %>%
        group_by(.data[[donor_id_column]]) %>%
        mutate(Reactivated = na.locf(Reactivated, na.rm = F)) # nolint

    # complicated join to get refhb from previous reactivated
    # For all reactivated set RefHb_R to the Hb measured at that donation
    reactivated <- donations %>%
        group_by(.data[[donor_id_column]]) %>%
        filter(Elig == "R" & Reactivated == T) %>%
        mutate(RefHb_R = Hb) %>%
        select({{ donation_id_column }}, {{ donor_id_column }}, RefHb_R)

    # join the reactivated RefHb_R with the other data
    # and move RefHb_R forward using locf again
    donations <- left_join(donations, reactivated)
    donations <- donations %>%
        group_by(.data[[donor_id_column]]) %>%
        mutate(RefHb_R = na.locf(RefHb_R, na.rm = F))

    # Now set RefHb to RefHB_R if it is not nan (so if it is a reactivation)
    donations <- donations %>%
        mutate(RefHb = ifelse(!is.na(RefHb_R), RefHb_R, RefHb)) %>%
        select(!RefHb_R)

    # Now set RefDonDate in a similar manner to the last Reference date
    # this takes care of reactivated donors
    donations <- donations %>%
        mutate(RefDonDate = as.Date(ifelse(Elig == "R", .data[[date_column]], NaN))) %>%
        group_by(.data[[donor_id_column]]) %>%
        mutate(RefDonDate = na.locf(RefDonDate))

    # shouldn't be, but remove any that have no RefHb
    donations <- donations %>% filter(!is.na(RefHb))
    print(paste("rows =", nrow(donations)))
    # calc prev hb
    donations <- donations %>%
        group_by(.data[[donor_id_column]]) %>%
        mutate(prev_hb = lag(.data[[hb_column]]))
    FileName <- paste0(FolderName, "donations_preprocessed.rds")
    saveRDS(donations, file = FileName)
} else if (endsWith(filename, "donations_preprocessed.rds")) {
    # now select only the follow ups and calculate DHb
    print(paste("Reading file", filename))
    donations <- readRDS(filename)
} else {
    print("Did not understand file")
    stop()
}

# This is much slower
# print("Setting age, logfer, prepostmenopausal")
# donations <- donations %>% mutate(
#     Leeftijd = round(as.numeric((.data[[date_column]] - .data[[birth_date_column]]) / 365.25), 0),
#     LogFer = log10(.data[[fer_column]]),
#     premenopausal = case_when(.data[[sex_column]] == female_code &
#         Leeftijd <= 50 ~ 1, .data[[sex_column]] == female_code & Leeftijd > 50 ~ 0)
# )

# Getting cumulative deferrals per donor
# So can later select donors for no deferalls
# TODO: what if multiple R and F per donor?

donations <- donations %>%
    mutate(deferred = ifelse((!is.na(.data[[fer_column]]) & .data[[fer_column]] < ferritin_deferral_limit_1) | (
        (.data[[sex_column]] == male_code & .data[[hb_column]] < male_hb_deferral_limit) |
            (.data[[sex_column]] == female_code & .data[[hb_column]] < female_hb_deferral_limit)
    ), 1, 0)) %>%
    group_by(.data[[donor_id_column]]) %>%
    mutate(cs_def = cumsum(deferred)) # note includes current measurement



print("selecting only follow up donations now")
data <- donations %>% filter(Elig == "F")
print(paste("rows =", nrow(data)))
# rm(donations)
print("calc dhb etc")
data <- data %>% mutate(
    DHb = .data[[hb_column]] - RefHb,
    Leeftijd = round(as.numeric((.data[[date_column]] - .data[[birth_date_column]]) / 365.25), 0),
    LogFer = log10(.data[[fer_column]]),
    premenopausal = case_when(.data[[sex_column]] == female_code &
        Leeftijd <= 50 ~ 1, .data[[sex_column]] == female_code & Leeftijd > 50 ~ 0)
)

print("Saving at fulldata.rds")
FileName <- paste0(FolderName, "fulldata.rds")
saveRDS(data, file = FileName)


print("Creating subselections:")
data_no_hb_def_prev <- data %>% filter(((.data[[sex_column]] == male_code) & (prev_hb > male_hb_deferral_limit)) |
    (.data[[sex_column]] == female_code) & (prev_hb > female_hb_deferral_limit))
FileName <- paste0(FolderName, "data_no_hb_def_prev.rds")
saveRDS(data_no_hb_def_prev, file=FileName)

data_no_fer_def_prev <- data %>% filter(is.na(prev_fer) | (prev_fer > ferritin_deferral_limit_1))
FileName <- paste0(FolderName, "data_no_fer_def_prev.rds")
saveRDS(data_no_fer_def_prev, file=FileName)

data_no_fer_def_prev <- data_no_fer_def_prev[,donation_id_column, drop=F]
data_no_def_prev <- inner_join(data_no_hb_def_prev, data_no_fer_def_prev, by = donation_id_column)
FileName <- paste0(FolderName, "data_no_def_prev.rds")
saveRDS(data_no_def_prev, file=FileName)

# NOTE!
# It may also happen that now the reactived donation is still used as reference
# TODO: Is that what we want??
# Also sometimes (often?) the first feritin measurement is for somebody who has been a donor quite a long time already
# and has just gotten their first ferritin measurement because it was not done before that time
# probably we want the second (2) ferritin measurement
# is below the same as selecting numfer == 1?
# not sure if this is very clean
# Warning might not be best for SA
data_only_first <- data %>% filter(numfer == 2)
FileName <- paste0(FolderName, "data_first_fer.rds")
saveRDS(data_only_first, file=FileName)

data_0_5_donations <- data %>% filter(numdon <= 5)
data_5_10_donations <- data %>% filter(numdon > 5 & numdon <= 10)
data_10_inf_donations <- data %>% filter(numdon > 10)
FileName <- paste0(FolderName, "data_0_5_donations.rds")
saveRDS(data_0_5_donations, file=FileName)
FileName <- paste0(FolderName, "data_5_10_donations.rds")
saveRDS(data_5_10_donations, file=FileName)
FileName <- paste0(FolderName, "data_10_inf_donations.rds")
saveRDS(data_10_inf_donations, file=FileName)

# deltaTime quantiles should be different for males and females

data_m <- data %>% filter(.data[[sex_column]] == male_code)
data_f <- data %>% filter(.data[[sex_column]] == female_code)

qm <- quantile(data_m$deltaTime, prob = c(0.25, 0.5, 0.75))
qf <- quantile(data_f$deltaTime, prob = c(0.25, 0.5, 0.75))
print(paste("deltaTime quantiles male", qm))
print(paste("deltaTime quantiles female", qf))

data_q1 <- data %>%
    filter(((.data[[sex_column]] == male_code) & (deltaTime < qm[1])) |
        (.data[[sex_column]] == female_code) & (deltaTime < qf[1])) %>%
    mutate(dt_q = ifelse(.data[[sex_column]] == male_code, qm[1], qf[1]))

data_q2 <- data %>%
    filter(((.data[[sex_column]] == male_code) & (deltaTime < qm[2] & deltaTime > qm[1])) |
        (.data[[sex_column]] == female_code) & (deltaTime < qf[2] & deltaTime > qf[1])) %>%
    mutate(dt_q = ifelse(.data[[sex_column]] == male_code, qm[2], qf[2]))

data_q3 <- data %>%
    filter(((.data[[sex_column]] == male_code) & (deltaTime < qm[3] & deltaTime > qm[2])) |
        (.data[[sex_column]] == female_code) & (deltaTime < qf[3] & deltaTime > qf[2])) %>%
    mutate(dt_q = ifelse(.data[[sex_column]] == male_code, qm[3], qf[3]))

data_q4 <- data %>%
    filter(((.data[[sex_column]] == male_code) & (deltaTime > qm[3])) |
        (.data[[sex_column]] == female_code) & (deltaTime > qf[3])) %>%
    mutate(dt_q = ifelse(.data[[sex_column]] == male_code, qm[3], qf[3]))
FileName <- paste0(FolderName, "data_dt_q1.rds")
saveRDS(data_q1, file=FileName)
FileName <- paste0(FolderName, "data_dt_q2.rds")
saveRDS(data_q2, file=FileName)
FileName <- paste0(FolderName, "data_dt_q3.rds")
saveRDS(data_q3, file=FileName)
FileName <- paste0(FolderName, "data_dt_q4.rds")
saveRDS(data_q4, file=FileName)

# No deferrals between F and R

data_no_def <- data %>% filter((cs_def == 0) | (cs_def == 1 &
    (.data[[fer_column]] < ferritin_deferral_limit_1 |
        (.data[[sex_column]] == male_code & .data[[hb_column]] < male_hb_deferral_limit) |
        (.data[[sex_column]] == female_code & .data[[hb_column]] < female_hb_deferral_limit)
    )))
FileName <- paste0(FolderName, "data_no_def_inbetween.rds")
saveRDS(data_no_def, file=FileName)
print("...done")
