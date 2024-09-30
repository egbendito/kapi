# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. Many valuable variables that need to be integrated still...


carob_script <- function(path) {
  
  "
	SOME DESCRIPTION GOES HERE...
"
  
  
  uri <- "dE9ZB3cP1aKPWR9Idw4Gp5cN"
  group <- "eia"
  
  meta <- data.frame(
    uri = uri,
    dataset_id = uri,
    authors = "Gizaw Desta",
    data_institute = "ICRISAT", 
    title = NA,
    publication=NA,
    description = "Yield Gap Decomposition Add-On Survey",
    group = group,
    license ="none",
    carob_contributor = 'Eduardo Garcia Bendito',
    data_citation = '...',
    project = 'Excellence in Agronomy',
    usecase_code= "USC007",
    usecase_name = 'ET-HighMix-Gvt ETH',
    activity = 'addon',
    data_type = "survey", 
    carob_date="2024-09-25",
    notes= NA
  )
  
  # Manually build path (this can be automated...)
  ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("/home/jovyan/carob-eia/data/raw/eia/Ethiopia-Fertilizer-Addon/YieldGap/", full.names = T))
  
  # Retrieve relevant file
  f <- ff[grep("yield_gap_only_fertilizer_ethiopia_06_10_2023.xlsx", basename(ff))]
  
  # Read household data
  d.hh <- carobiner::read.excel(f, sheet = "EiA_AddOn_Ethiopia_YGD_only")[c(-1),]
  
  # Read household members data
  d.hh.members <- as.data.frame(readxl::read_excel(f, sheet = "hh_members_details_repeat"))[c(-1),]
  
  # Read plots data
  d.hh.plots <- as.data.frame(readxl::read_excel(f, sheet = "hh_plots_repeat"))[c(-1),]
  d.hh.plots.info <- as.data.frame(readxl::read_excel(f, sheet = "plot_information_repeat"))[c(-1),]
  
  # Read plots tillage data
  d.plots.til <- as.data.frame(readxl::read_excel(f, sheet = "tillage_info"))[c(-1),]
  
  # Read plots organic fertilizer data
  d.plots.fert.org <- as.data.frame(readxl::read_excel(f, sheet = "organic_inputs_repeat"))[c(-1),]
  d.plots.fert.org.details <- as.data.frame(readxl::read_excel(f, sheet = "organic_inputs_details_repeat"))[c(-1),]
  
  # Read plots inorganic fertilizer data
  d.plots.fert.inorg <- as.data.frame(readxl::read_excel(f, sheet = "inorganic_inputs_repeat"))[c(-1),]
  
  # Read plots weeding data
  d.plots.weeding <- as.data.frame(readxl::read_excel(f, sheet = "weeding_info"))[c(-1),]
  
  # Format household data
  d.hh$barcodehousehold <- ifelse(!is.na(d.hh$barcodehousehold), d.hh$barcodehousehold, d.hh$barcodehousehold_1)
  d.hh <- d.hh[,c("barcodehousehold", "_index")]

  # Format tillage data
  d.plots.til.methods <- reshape(d.plots.til[,c("_parent_table_name","_parent_index","tillage_id","tillage_method")],
                                 idvar = c("_parent_index", "_parent_table_name"),
                                 timevar = "tillage_id", direction = "wide")
  d.plots.til.methods$land_prep_implement <- c(gsub("; NA", "", matrix(apply(d.plots.til.methods[grep("tillage_method", colnames(d.plots.til.methods))], 1, paste0, collapse='; '), ncol=1)))
  d.plots.til.methods <- d.plots.til.methods[, c("_parent_index", "land_prep_implement")]

  # Format organic fertilizer data
  d.plots.fert.org.details$residue_prevcrop_used <- grepl("plant", d.plots.fert.org.details$organic_ids)
  residue_prevcrop_used <- aggregate(d.plots.fert.org.details$residue_prevcrop_used,
                                     by = list('_parent_index' = d.plots.fert.org.details$`_parent_index`), FUN = sum, na.rm = T)
  colnames(residue_prevcrop_used)[2] <- "residue_prevcrop_used"
  residue_prevcrop_used$residue_prevcrop_used <- as.logical(residue_prevcrop_used$residue_prevcrop_used)
  d.plots.fert.org.details$OM_type <- gsub("_NA", "", d.plots.fert.org.details$organic_ids)
  d.plots.fert.org.details$OM_type[grep("manure", d.plots.fert.org.details$OM_type)] <- "farmyard manure"
  d.plots.fert.org.details$OM_type[grep("compost", d.plots.fert.org.details$OM_type)] <- "compost"
  d.plots.fert.org.details$OM_type[grep(paste0(c("-1", "other_input"), collapse = "|"), d.plots.fert.org.details$OM_type)] <- "unknown"
  d.plots.fert.org.details$OM_type[grep(paste0(c("plant", "-1", "NA"), collapse = "|"), d.plots.fert.org.details$OM_type)] <- NA
  OM_type <- reshape(d.plots.fert.org.details[,c("_parent_index","organic_ids","OM_type")],
                     idvar = "_parent_index", timevar = "organic_ids", direction = "wide")
  OM_type$OM_type <- c(gsub("NA; ", "", gsub("; NA", "", matrix(apply(OM_type[grep("OM_type", colnames(OM_type))], 1, paste0, collapse='; '), ncol=1))))
  OM_type$OM_type <- ifelse(OM_type$OM_type == "NA", NA, OM_type$OM_type)
  OM_type <- OM_type[,c("_parent_index", "OM_type")]
  d.plots.fert.org.details$OM_used <- ifelse(!is.na(d.plots.fert.org.details$OM_type), TRUE, FALSE)
  OM_used <- aggregate(d.plots.fert.org.details$OM_used,
                       by = list('_parent_index' = d.plots.fert.org.details$`_parent_index`), FUN = sum, na.rm = T)
  colnames(OM_used)[2] <- "OM_used"
  OM_used$OM_used <- as.logical(OM_used$OM_used)
  d.plots.fert.org.details$OM_amount[grep(100, d.plots.fert.org.details$organic_input_unit)] <- 100*as.numeric(d.plots.fert.org.details$organic_input_amount[grep(100, d.plots.fert.org.details$organic_input_unit)])
  d.plots.fert.org.details$OM_amount[grep(50, d.plots.fert.org.details$organic_input_unit)] <- 50*as.numeric(d.plots.fert.org.details$organic_input_amount[grep(50, d.plots.fert.org.details$organic_input_unit)])
  d.plots.fert.org.details$OM_amount <- ifelse(grepl("bags", d.plots.fert.org.details$organic_input_unit, fixed = TRUE),
                                             d.plots.fert.org.details$OM_amount,
                                             as.numeric(d.plots.fert.org.details$organic_input_amount))
  OM_amount <- aggregate(d.plots.fert.org.details$OM_amount,
                         by = list('_parent_index' = d.plots.fert.org.details$`_parent_index`), FUN = sum, na.rm = T)
  colnames(OM_amount)[2] <- "OM_amount"
  # EGB:
  # # Adding variable. But there are many issues on this column since some values seem to represent total and others per unit
  d.plots.fert.org.details$OM_price <- as.numeric(d.plots.fert.org.details$organic_input_cost)
  d.plots.fert.org.details <- merge(residue_prevcrop_used, merge(OM_used, merge(OM_type, OM_amount, by = "_parent_index")))
  d.plots.fert.org.details$previous_crop_residue_weight <- ifelse(d.plots.fert.org.details$residue_prevcrop_used == TRUE & d.plots.fert.org.details$OM_used == FALSE,
                                                                  d.plots.fert.org.details$OM_amount, NA)
  d.plots.fert.org.details$OM_amount <- ifelse(d.plots.fert.org.details$residue_prevcrop_used == TRUE & d.plots.fert.org.details$OM_used == FALSE,
                                                                  NA, d.plots.fert.org.details$OM_amount)
  d.plots.fert.org.details$OM_amount <- ifelse(d.plots.fert.org.details$OM_used == FALSE, NA, d.plots.fert.org.details$OM_amount)
  d.plots.fert.org <- merge(d.plots.fert.org[, c("_parent_index", "_index")],
                            d.plots.fert.org.details, by.x = "_index", by.y = "_parent_index", all.x = T)
  d.plots.fert.org <- d.plots.fert.org[,c("_parent_index",
                                          "residue_prevcrop_used","previous_crop_residue_weight",
                                          "OM_used", "OM_type", "OM_amount")]
  
  # Format inorganic fertilizer data
  # # # TBD
  
  # Format weeding data
  d.plots.weeding$weeding_technique <- gsub("Manual Tillage", "manual", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- gsub("Tillage Manual", "tillage", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- gsub("Chemical Manual", "chemical", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- gsub("Manual Chemical", "manual", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- gsub("Chemical Mechanical", "chemical", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- gsub("Chemical Manual other", "chemical", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- gsub("Chemical other", "chemical", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- gsub("Chemical Water_pressure_spraying", "chemical", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- gsub("Manual Chemical Water_pressure_spraying", "manual", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- gsub("Manual Grazing", "manual", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- gsub("Manual Mowing", "manual", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- gsub("Mowing Manual", "mowing", d.plots.weeding$weeding_technique)
  d.plots.weeding$weeding_technique <- tolower(d.plots.weeding$weeding_technique)
  d.plots.wed.technique <- reshape(d.plots.weeding[,c("_parent_table_name","_parent_index","weeding_id","weeding_technique", "herbicide_name", "herbicide_cost")],
                                   idvar = c("_parent_index", "_parent_table_name"),
                                   timevar = "weeding_id", direction = "wide")
  d.plots.wed.technique$weeding_method <- c(gsub("; NA", "", matrix(apply(d.plots.wed.technique[grep("weeding_technique", colnames(d.plots.wed.technique))], 1, paste0, collapse='; '), ncol=1)))
  d.plots.wed.technique$herbicide_product <- c(gsub("; NA", "", matrix(apply(d.plots.wed.technique[grep("herbicide_name", colnames(d.plots.wed.technique))], 1, paste0, collapse='; '), ncol=1)))
  d.plots.wed.technique$herbicide_cost <- c(gsub("; NA", "", matrix(apply(d.plots.wed.technique[grep("herbicide_cost", colnames(d.plots.wed.technique))], 1, paste0, collapse='; '), ncol=1)))
  d.plots.wed.technique <- d.plots.wed.technique[, c("_parent_index", "weeding_method", "herbicide_product", "herbicide_cost")]
  # EGB:
  # # Need to include labor costs
  
  # Format pest and diseases data
  # # # TBD
  
  # Format insects data
  # # # TBD

  # Format plot information
  d.hh.plots <- merge(d.hh.plots, d.hh.plots.info, by.x = c("_parent_index", "index_plot"), by.y = c("_parent_index", "plot_numberid"), all.x = T)
  d.hh.plots$on_farm <- FALSE
  d.hh.plots$is_survey <- TRUE
  d.hh.plots$country <- "Ethiopia"
  d.hh.plots$longitude <- as.numeric(d.hh.plots$plot_longitude)
  d.hh.plots$latitude <- as.numeric(d.hh.plots$plot_latitude)
  d.hh.plots$geo_uncertainty <- as.numeric(d.hh.plots$`_geopoint_plot_precision`)
  d.hh.plots$elevation <- as.numeric(d.hh.plots$`_geopoint_plot_altitude`)
  d.hh.plots$geo_from_source <- TRUE
  d.hh.plots$plot_area <- ifelse(d.hh.plots$unitland == "hectare", as.numeric(d.hh.plots$plot_size)*10000,
                               ifelse(d.hh.plots$unitland == "sqm", as.numeric(d.hh.plots$plot_size), NA))
  d.hh.plots$land_tenure <- d.hh.plots$plot_tenure_status
  d.hh.plots$crop <- d.hh.plots$plot_crop
  d.hh.plots$crop[d.hh.plots$crop == "fababean"] <- "faba bean"
  d.hh.plots$crop[d.hh.plots$crop == "mungbean"] <- "mung bean"
  d.hh.plots$crop[d.hh.plots$crop == "potatoIrish"] <- "potato"
  d.hh.plots$crop[grep("other", d.hh.plots$crop)] <- NA
  d.hh.plots$previous_crop[d.hh.plots$previous_crop == "fababean"] <- "faba bean"
  d.hh.plots$previous_crop[d.hh.plots$previous_crop == "mungbean"] <- "mung bean"
  d.hh.plots$previous_crop[d.hh.plots$previous_crop == "potatoIrish"] <- "potato"
  d.hh.plots$previous_crop[d.hh.plots$previous_crop %in% c("beansBush", "haricotbean")] <- "common bean"
  d.hh.plots$previous_crop[d.hh.plots$previous_crop == "lupin"] <- "tarwi"
  d.hh.plots$previous_crop[d.hh.plots$previous_crop == "grasspea"] <- "grass pea"
  d.hh.plots$previous_crop[d.hh.plots$previous_crop %in% c("fieldpea", "peas")] <- "pea"
  d.hh.plots$previous_crop[d.hh.plots$previous_crop == "potatoSweet"] <- "sweetpotato"
  d.hh.plots$previous_crop[d.hh.plots$previous_crop == "oat"] <- "oats"
  d.hh.plots$previous_crop <- ifelse(d.hh.plots$previous_crop %in% c("other1", "other2", "other3", "no_answer"), NA, d.hh.plots$previous_crop)
  d.hh.plots$crop[grep(paste0(c("other", "no_answer"), collapse = "|"), d.hh.plots$crop)] <- NA
  d.hh.plots$intercrops <- d.hh.plots$inter_crop
  d.hh.plots$intercrops[d.hh.plots$intercrops == "fababean"] <- "faba bean"
  d.hh.plots$intercrops[d.hh.plots$intercrops == "mungbean"] <- "mung bean"
  d.hh.plots$intercrops[d.hh.plots$intercrops == "haricotbean"] <- "common bean"
  d.hh.plots$intercrops[grep("other", d.hh.plots$intercrops)] <- NA
  d.hh.plots$intercrops[d.hh.plots$intercrops == "peas"] <- "pea"
  d.hh.plots$soil_type <- tolower(trimws(d.hh.plots$soilName))
  d.hh.plots$landscape_position <- d.hh.plots$position
  # d.hh.plots$soil_texture <- d.hh.plots$soilName_other
  d.hh.plots$house_distance <- as.numeric(d.hh.plots$distance)
  d.hh.plots$previous_crop_burnt <- grepl("burn", d.hh.plots$residues_fallow, fixed = TRUE)
  d.hh.plots$previous_crop_residue_management <- d.hh.plots$residues_fallow
  d.hh.plots$seed_source <- d.hh.plots$source_material
  d.hh.plots$seed_density <- NA
  d.hh.plots$seed_density[grep("50kg", d.hh.plots$planting_material_unit)] <- 50*as.numeric(d.hh.plots$planting_material_rate[grep("50kg", d.hh.plots$planting_material_unit)])
  d.hh.plots$seed_density[grep("10kg", d.hh.plots$planting_material_unit)] <- 10*as.numeric(d.hh.plots$planting_material_rate[grep("10kg", d.hh.plots$planting_material_unit)])
  d.hh.plots$seed_density[grep("5kg", d.hh.plots$planting_material_unit)] <- 5*as.numeric(d.hh.plots$planting_material_rate[grep("5kg", d.hh.plots$planting_material_unit)])
  d.hh.plots$seed_density <- ifelse(d.hh.plots$planting_material_unit == "kg",
                                    as.numeric(d.hh.plots$planting_material_rate[d.hh.plots$planting_material_unit == "kg"]),
                                    d.hh.plots$seed_density)
  d.hh.plots$seed_cost <- d.hh.plots$planting_material_cost
  # Needs to be reviewed in terminag
  d.hh.plots$planting_method <- d.hh.plots$seeding_method
  d.hh.plots$planting_method[d.hh.plots$seeding_method == "lineseeding"] <- "line sowing"
  d.hh.plots$planting_method[d.hh.plots$seeding_method == "other"] <- "broadcasting; line sowing"
  d.hh.plots$row_spacing <- as.numeric(d.hh.plots$distance_lines_cm)
  d.hh.plots$planting_date <- as.Date(as.numeric(d.hh.plots$planting_date), origin = "1899-12-30")
  d.hh.plots$irrigated <- ifelse(d.hh.plots$irrigation_plot == "Yes", TRUE, FALSE)
  d.hh.plots$irrigation_number <- as.numeric(d.hh.plots$irrigation_number)
  d.hh.plots$harvesting_method <- d.hh.plots$harvest_implement # Not in terminag
  d.hh.plots$yield_part[d.hh.plots$primary_product %in% c("grains", "other")] <- "grain" # If the use is "for seed" we assume the main product is grains
  d.hh.plots$primary_weight <- as.numeric(d.hh.plots$primary_yield)
  d.hh.plots$primary_weight[grep("100kg", d.hh.plots$primary_harvest_unit)] <- 100*as.numeric(d.hh.plots$primary_yield[grep("100kg", d.hh.plots$primary_harvest_unit)])
  d.hh.plots$primary_weight[grep("50kg", d.hh.plots$primary_harvest_unit)] <- 50*as.numeric(d.hh.plots$primary_yield[grep("50kg", d.hh.plots$primary_harvest_unit)])
  d.hh.plots$primary_weight[grep("tonne", d.hh.plots$primary_harvest_unit)] <- ifelse(1000*as.numeric(d.hh.plots$primary_yield[grep("tonne", d.hh.plots$primary_harvest_unit)]) < 1,
                                                                                  1000*as.numeric(d.hh.plots$primary_yield[grep("tonne", d.hh.plots$primary_harvest_unit)]),
                                                                                  as.numeric(d.hh.plots$primary_yield[grep("tonne", d.hh.plots$primary_harvest_unit)]))
  d.hh.plots$yield <- d.hh.plots$primary_weight/(d.hh.plots$plot_area/10000)
  d.hh.plots$primary_weight_sold <- as.numeric(d.hh.plots$selling_primary_amount)
  d.hh.plots$primary_weight_sold[grep("100kg", d.hh.plots$primary_harvest_unit)] <- 100*as.numeric(d.hh.plots$selling_primary_amount[grep("100kg", d.hh.plots$primary_harvest_unit)])
  d.hh.plots$primary_weight_sold[grep("50kg", d.hh.plots$primary_harvest_unit)] <- 50*as.numeric(d.hh.plots$selling_primary_amount[grep("50kg", d.hh.plots$primary_harvest_unit)])
  d.hh.plots$primary_weight_sold[grep("tonne", d.hh.plots$primary_harvest_unit)] <- 1000*as.numeric(d.hh.plots$selling_primary_amount[grep("tonne", d.hh.plots$primary_harvest_unit)])
  d.hh.plots$yield_marketable <- d.hh.plots$primary_weight_sold/(d.hh.plots$plot_area/10000)
  
  d.hh.plots <- d.hh.plots[,c("_parent_index", "index_plot", "on_farm", "is_survey",
                              "country", "longitude", "latitude", "elevation", "geo_uncertainty", "geo_from_source", "plot_area", 
                              "crop", "variety", "intercrops", "previous_crop", "previous_crop_burnt","previous_crop_residue_management",
                              "seed_source", "seed_density", "seed_cost", "planting_method", "row_spacing", "irrigated",
                              "harvesting_method", "yield_part", "yield", "yield_marketable",
                              "soil_type", "landscape_position", "land_tenure", "house_distance")]
  
  # Merge things
  d.hh.members <- merge(d.hh, d.hh.members, by.x = "_index", by.y = "_parent_index", all.y = T)
  d <- merge(d.hh, d.hh.plots, by.x = "_index", by.y = "_parent_index", all.y = T)
  d <- merge(d, d.plots.til.methods, by.x = "_index", by.y = "_parent_index", all.x = T)
  d <- merge(d, d.plots.fert.org, by.x = "_index", by.y = "_parent_index", all.x = T)
  
  d <- d[,c(2, 4:ncol(d))]
  
  d <- carobiner::change_names(d,
                               from = c("barcodehousehold"),
                               to = c("trial_id"))
  
  carobiner::write_files(meta, d, path=path)
  
}
