# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. planting_time, first_rain_date, gemination, pod_density are not yet the carob variable and perhaps should be included later


carob_script <- function(path) {
   
   "
	SOME DESCRIPTION GOES HERE...

"
   
   uri <- "rMygxYbTj3FL8XryV2K96XjF"
   
   meta <- data.frame(
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      authors ="Mary Jane;John Doe",
      publication= NA,
      data_institute =NA,
      title = NA,
      group = "",
      license = NA,
      project = 'Excellence in Agronomy',
      usecase_code ="USC016",
      usecase_name = 'CH-CerLeg-Solidaridad',
      activity = 'experiment',
      carob_contributor = 'Cedric Ngakou',
      data_type = "on-station experiment",
      response_vars= "yield",
      treatment_vars ="N_fertilizer;P_fertilizer;K_fertilizer;intercrops",
      carob_date="2024-08-02"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = "", files = list.files("/home/jovyan/carob-eia/data/raw/eia/Chinyanja-Solidaridad-Soy-NOT/", full.names = T))
   
   
   f <- ff[basename(ff)=="Crop_mixes_Msekera_26.06.23 updated 28 Aug  2023.xlsx"]
   f1 <- ff[basename(ff)=="MOZAMBIQUE  Angonia yeld data  2022 23 validation trial yields 2022 23.xlsx"]
   f2 <- ff[basename(ff)=="NOT TRIALS IIAM Ulongue  2022 23 with yield calcs 24 aug .xlsx"]
   
   ## Process Crop_mixes_Msekera_26.06.23 updated 
   
   ### Read file
   r <- carobiner::read.excel.hdr(f,sheet = "Crop_mixes",skip=3,fix_names = TRUE)
   ## remove empty column name
   r <- r[,-53] 
   
   d <- data.frame(
      treatment= r$Treat.description,
      plot_area= r$Effective.harvest.area,
      rep= r$Rep,
      planting_date= as.character(as.Date(r$Planting.date,"%d/%m/%y")),
      harvest_date= as.character(as.Date(r$harvest.date,"%d/%m/%y")),
      crop=tolower(r$Crop),
      variety= tolower(r$variety),
      #crop_system= r$intercrop.sole,
      planting_time= r$Time.of.planting,
      plot_width= r$Row.length.cm,
      shelling_percentage= as.numeric(r$Shelling.pct),
      yield= r$Grain.kg.ha,
      fwy_total= r$Total.Biomass.kg.ha,
      harvest_index= r$HI,
      plot_nbr= r$Plot.number,
      intercrops= ifelse(grepl("intercrop|inter crop",r$intercrop.sole) & grepl("Maize",r$Crop) ,"soybean",
                         ifelse(grepl("intercrop|inter crop",r$intercrop.sole) & grepl("Soybean",r$Crop),"maize","none"))
      
   )
   
   ## process flowering date file
   r1 <- carobiner::read.excel.hdr(f,sheet = "Days_to_50%_flowering",skip=1,fix_names = TRUE)
   
   d1 <- data.frame(
      plot_nbr= r1$Plot,
      planting_time= r1$Time.of.planting,
      rep= r1$Rep,
      flowering_date= as.character(as.Date(r1$Date.of.50pct.flowering.tasseling,"%d/%m/%y")),
      plant_density= r1$Total.number.of.plants.plot
   )
   
   d <- merge(d,d1,by=c("plot_nbr","rep","planting_time"),all.x= TRUE)
   
   d$plant_density <- (d$plant_density/d$plot_area)*10000
   d <- d[!is.na(d$crop),]
   
   d$planting_time <- NULL
   # process crop management file 
   
   r2 <- t(carobiner::read.excel.hdr(f,sheet = "crop management data",skip=5,fix_names = TRUE))
   colnames(r2) <- as.matrix(r2[1,])
   r2 <- r2[-1,] 
   r2 <- data.frame(rbind(r2))
   row.names(r2) <- 1:nrow(r2)
   r2$basal.rate..kg.ha. <- gsub("kg/ha","",r2$basal.rate..kg.ha.)
   r2$top.dressing.application.rate.kg.ha. <- gsub("kg/ha","",r2$top.dressing.application.rate.kg.ha.)
   d2 <- data.frame(
      crop= r2$crop.name,
      #variety=r2$variety,
      plot_nbr= as.numeric(r2$plot),
      row_spacing= r2$row.spacing.cm.,
      plant_spacing= r2$plant..station..spacing..cm.,
      land_prep_method= r2$tillage.method,
      first_rain_date= as.character(as.Date(r2$date.when.first.planting.rains.were.received.in.area.,"%d/%m/%y")),
      maturity_date= as.character(as.Date(r2$date.of.50..maturity,"%d/%m/%y")),
      fertilizer_type= paste(r2$basal.type, r2$top.dressing.type,sep = ";"),
      fertilizer_date= as.character(as.Date(r2$basal.date.of.application,"%d/%m/%y")),
      fertilizer_amount= ifelse(!is.na(r2$top.dressing.application.rate.kg.ha.),(as.numeric(r2$basal.rate..kg.ha.) + as.numeric(r2$top.dressing.application.rate.kg.ha.)),as.numeric(r2$basal.rate..kg.ha.)),
      N_fertilizer= ifelse(!is.na(r2$top.dressing.application.rate.kg.ha.),as.numeric(r2$basal.rate..kg.ha.)*0.1 + as.numeric(r2$top.dressing.application.rate.kg.ha.)*0.46,as.numeric(r2$basal.rate..kg.ha.)*0.1),
      P_fertilizer= as.numeric(r2$basal.rate..kg.ha.)*0.2,
      K_fertilizer= as.numeric(r2$basal.rate..kg.ha.)*0.1,
      gemination= r2$Estimate.gemination..,
      weeding_dates= paste(as.Date(r2$date.of.first.weeding,"%d/%m/%y"),as.Date(r2$date.of.second.weeding,"%d/%m/%y"),as.Date(r2$date.of.third.weeding,"%d/%m/%y"),sep=";"),
      weeding_times= 3
      
   )
   ## removing unuful row
   d2 <- d2[!is.na(d2$plant_spacing),] 
   d2$row_spacing <-as.numeric(gsub("cm","",d2$row_spacing))
   d2$plant_spacing <- as.numeric(gsub("cm","",d2$plant_spacing))
   d2$crop <- gsub("soya bean","soybean",d2$crop)
   
   ## merge 
   d <- merge(d,d2,by=c("crop","plot_nbr"),all.x=TRUE)
   d$country= "Zambia"
   d$location= "Msekera"
   d$latitude <- -13.6452874
   d$longitude <- 32.5638197
   
   ### Process MOZAMBIQUE  Angonia yeld data
   rr <- carobiner::read.excel.hdr(f1, sheet = "Summary validations",skip=0)
   ### Keep useful variables 
   col <- c(28:42) 
   rr <- rr[,col]
   colnames(rr) <- rr[4,] ### name the variables with appropriate name 
   rr <- rr[-c(1:4),]
   row.names(rr) <- NULL ### reset index 
   
   dd <- data.frame(
      variety= rr$Variety,
      plot_area= rr$`Effective harvest area`,
      pod_density= rr$`Number of Pods per ha`,
      yield= rr$`Grain (kg/ha)`,
      fwy_total= rr$`Total Biomass (kg/ha)`,
      shelling_percentage= as.numeric(rr$`Shelling %`),
      harvest_index= as.numeric(rr$HI),
      country= "Mozambique",
      adm1="Angonia",
      location= "Úlonguè",
      latitude= -14.7093,
      longitude= 34.3618,
      crop= "soybean"
   )
   
   ### Process NOT TRIALS IIAM Ulongue
   
   rr1 <- carobiner::read.excel.hdr(f2,sheet = "NOT TRIALS 2022-23",skip=0)
   ## Keep useful columns
   col <- c(25:46) 
   rr1 <- rr1[,col]
   colnames(rr1) <- rr1[107,] 
   rr1 <- rr1[-c(1:107),] ## header 
   row.names(rr1) <- NULL ## reset index
   
   dd1 <- data.frame(
      rep= rr1$Rep,
      treatment= as.numeric(rr1$`Fertilization treat`),
      N_fertilizer= as.numeric(rr1$`N (kg/ha)`),
      P_fertilizer= as.numeric(rr1$`P (kg/ha)`),
      K_fertilizer= as.numeric(rr1$`K (kg/ha)`),
      planting_date= as.character(as.Date(rr1$`Planting date`,"%d/%m/%y")),
      #harvest_date= as.Date(rr1$`harvest date`,"%d/%m/%y"),
      #crop_system= rr1$`Crop Sys`,
      variety= rr1$Variety,
      plot_area= rr1$`Effective harvest area`,
      yield= rr1$`Grain (kg/ha)`,
      fwy_total= rr1$`Total Biomass (kg/ha)`,
      shelling_percentage= as.numeric(rr1$`Shelling %`),
      distance= round(as.numeric(rr1$`average distances`),2),
      inoculated= ifelse(grepl("Innoculated",rr1$`Innoc Descrip`),TRUE,FALSE),
      country= "Mozambique",
      adm1="Angonia",
      location= "Úlonguè",
      latitude= -14.7093,
      longitude= 34.3618,
      crop="soybean"
   )
   
   dd1 <- dd1[!is.na(dd1$treatment),]
   
   #var <- c("Numbers of nodules R1","Numbers of nodules R2","Numbers of nodules R3")
   
   # #dd2 <- lapply(var, \(i){
   #   rr2 <- carobiner::read.excel.hdr(f2,sheet =i,skip=0)
   #  names(rr2) <- gsub("Distancian","Distancia",names(rr2))
   #   data.frame(
   #     nodule_weight=rr2$NN,
   #     rep= rr2$Rep,
   #     treatment= rr2$trat,
   #     distance= rr2$Distancia
   #   )
   # }) 
   
   #dd2 <- do.call(rbind,dd2)
   #dd2$distance <- round(dd2$distance,2)
   
   #dd1 <- merge(dd1,dd2,by=c("rep","treatment","distance"),all.x=TRUE)
   
   dd1$treatment <- paste("N",dd1$N_fertilizer,"P",dd1$P_fertilizer,"K",dd1$K_fertilizer,sep="")
   dd1$row_spacing <- 45
   dd1$plant_spacing <- 5
   dd1$land_prep_method <- "conventional"
   dd1$maturity_date <- "2023-03-23"
   dd1$weeding_dates <- "2023-01-20"
   dd1$fertilizer_date <- "2022-12-20"
   dd1$first_rain_date <- "2022-12-18" 
   
   
   d <- carobiner::bindr(d,dd,dd1)
   
   d$plot_nbr <- d$distance <- NULL
   
   d$trial_id <- as.character(1:nrow(d))
   d$irrigated <- as.logical(NA)
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$inoculated[is.na(d$inoculated)] <- FALSE
   d$yield_part <- "grain"
   d$geo_from_source <- FALSE
   
   ###Fixing fertilizer type
   d$fertilizer_type <- gsub("compound D","D-compound", d$fertilizer_type)
   d$fertilizer_type <- gsub(NA,"", d$fertilizer_type)
   d$land_prep_method <- gsub("Ridge","ridge tillage", d$land_prep_method)
   
   d$intercrops[is.na(d$intercrops)] <- "none"
   
   ## Fixing harvest date 
   d$harvest_date[grep("2020-05-22",d$harvest_date)] <- "2021-05-22"
   
   ## data type
   d$yield <- as.numeric(d$yield)
   d$rep <- as.integer(d$rep)
   d$fwy_total <- as.numeric(d$fwy_total)
   d$plot_area <- as.numeric(d$plot_area)
   d$weeding_times <- as.integer(d$weeding_times)
   
   carobiner::write_files(meta, d, path=path)
   
}

#carob_script(path)

