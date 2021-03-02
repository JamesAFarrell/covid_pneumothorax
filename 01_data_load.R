
# Load covid data
datadir = "/home/common/covid/cleaned/full/"

#timestamp = "2021-01-21_1038"
timestamp = "2021-02-14_1224"

ccp_data   = read_rds(paste0(datadir, paste0("ccp_data_", timestamp, "_full.rds")))
topline    = read_rds(paste0(datadir, paste0("topline_", timestamp, "_full.rds")))
surv_data  = read_rds(paste0(datadir, "surv_data_", timestamp, "_full.rds"))
treatment  = read_rds(paste0(datadir, "treatment_", timestamp, "_full.rds"))
outcome    = read_rds(paste0(datadir, "outcome_", timestamp, "_full.rds"))

load(paste0(datadir, "helpers_", timestamp, "_full.rda"))

# Load drug name data
drug_to_generic = readRDS('data/drug_to_generic.rds')
