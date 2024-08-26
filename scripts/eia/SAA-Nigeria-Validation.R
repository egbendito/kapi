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
   
   uri <- "doi:Nigeria-SAA-Validation"
   group <- "eia"
   
   dset <- data.frame(
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      authors = "Christine Kreye",
      data_institute = "IITA", #International Institute of Tropical Agriculture",
      title = NA,
      publication=NA,
      description = "Validations of the SAA Nigeria Use Case MVP",
      group = group,
      license ="none",
      carob_contributor = 'Eduardo Garcia Bendito;Cedric Ngakou',
      data_citation = '...',
      treatment_vars="variety;crop;longitude;latitude" ,
      response_vars= "yield" ,
      project = 'Excellence in Agronomy',
      use_case = 'NG-Akilimo-SAA',
      activity = 'validation',
      data_type = "experiment", 
      carob_date="2024-05-22",
      note= "N_fertilizer,P_fertilizer and K_fertilizer rate are missing in the data"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("/home/jovyan/carob-eia/data/raw/eia/Nigeria-SAA-Validation", full.names = T))
   
   # Maize
   fmc <- ff[basename(ff) == "maizeClean.csv"]
   fmp <- ff[basename(ff) == "plotDimS.csv"]
   fmm <- ff[basename(ff) == "grainMoistureM.csv"]
   fms <- ff[basename(ff) == "scoreHigh.csv"]
   fmf <- ff[basename(ff) == "NEfert.csv"]
   
   # Read relevant file(s)
   rmc <- read.csv(fmc)
   rmp <- read.csv(fmp)
   rmm <- read.csv(fmm)
   rms <- read.csv(fms)
   rmf <- read.csv(fmf)
   
   ## Processing maize data
   
   # maizeClean
   dh <- data.frame(
     trial_id = rmc$HHID,
     event = rmc$event,
     adm1 = rmc$stateEA,
     location = rmc$lga,
     year = rmc$Year,
     country = rmc$country,
     plants_SSR = rmc$nrPlants_SSR,
     plants_BRR = rmc$nrPlants_BRR,
     plants_ZCC = rmc$nrPlants_ZCC,
     cobs_SSR = rmc$maizeCobsFW_plot_SSR,
     cobs_BRR = rmc$maizeCobsFW_plot_BRR,
     cobs_ZCC = rmc$maizeCobsFW_plot_ZCC,
     yield_SSR = rmc$maizeGrainFW_plot_SSR,
     yield_BRR = rmc$maizeGrainFW_plot_BRR,
     yield_ZCC = rmc$maizeGrainFW_plot_ZCC,
     land_prep_method = rmc$tillageMethod1,
     harvest_date = rmc$harvestDate
   )
   
   # Maize
   # Event1: Planting
   # Event2: 1st fertilizer application
   # Event3: 2nd fertilizer application
   # Event4: Monitoring flowering
   # Event5: Harvesting
   ## Selecting the harvest event
   dh <- dh[dh$event == "event5M",]
   
   dh1 <- rmc[rmc$event == "event1M", c("HHID", "plotL1_BRR", "plotW1_BRR", "plotL1_SSR", "plotW1_SSR", "plotL1_ZCC", "plotW1_ZCC")]
   dh1$plot_area_BRR <- (dh1$plotL1_BRR * dh1$plotW1_BRR)/10000
   dh1$plot_area_SSR <- (dh1$plotL1_SSR * dh1$plotW1_SSR)/10000
   dh1$plot_area_ZCC <- (dh1$plotL1_ZCC * dh1$plotW1_ZCC)/10000
   dh1 <- dh1[,c(1,8,9,10)]
   colnames(dh1)[1] <- "trial_id"
   dmh <- merge(dh,dh1, by=c("trial_id"), all.x= TRUE) 
   
   ## grainMoistureM
   
   dh2 <- data.frame(
     trial_id= rmm$HHID,
     yield_moisture_BRR= rmm$moistureBRR,
     yield_moisture_SSR= rmm$moistureSSR,
     yield_moisture_ZCC= rmm$moistureZCC
   )
   
   dmh <- merge(dmh,dh2, by=c("trial_id"), all.x= TRUE) 
   
   ## Maize fertilizer (NEfert)
   
   dh3 <- data.frame(
     trial_id= rmf$HHID,
     adm1= rmf$state,
     longitude =rmf$Longitude,
     latitude = rmf$Latitude,
     elevation = rmf$Altitude,
     geo_uncertainty = rmf$Accuracy,
     planting_date= rmf$plantingDate,
     OM_used= ifelse(grepl("yes",rmf$orgApply1)|grepl("yes",rmf$orgApply2),TRUE,FALSE),
     fertilizer_price= as.character(rowSums(rmf[,c("costUrea","costNPK")])),
     variety= rmf$variety,
     row_spacing= substr(rmf$plantingDensity,1,4),
     plant_spacing= substr(rmf$plantingDensity,8,11),
     fertilizer_amount_BRR= rmf$NPKBRR + rmf$plotUreaSplit1BRR+ rmf$plotUreaSplit2BRR,
     fert= rmf$plotNPKBRR_BE + rmf$plotUreaBRR_BE,
     fertilizer_type_BRR= "NPK;urea"
   )
   
   dh3$fertilizer_amount_BRR[is.na(dh3$fertilizer_amount_BRR)] <- dh3$fert[is.na(dh3$fertilizer_amount_BRR)]
   dh3$adm1[grep("BE",dh3$adm1)] <- "benue"
   dh3$adm1[grep("KD",dh3$adm1)] <- "kaduna"
   dh3$adm1[grep("KA",dh3$adm1)] <- "kano"
   dh3$fert <- NULL
   dh3$fertilizer_type_SSR <- NA
   dh3$fertilizer_type_ZCC <- "NPK"
   dh3$fertilizer_amount_SSR <- NA ## Unknwon 
   dh3$fertilizer_amount_ZCC <- 0
   
   dmh <- merge(dmh,dh3, by=c("trial_id","adm1"), all.x= TRUE)  
   
   var <- names(dmh)[grep("SSR|BRR|ZCC",names(dmh))] 
   dm <- lapply(c("_BRR","_SSR","_ZCC"),\(i){
     dd <- dmh[,var]
     dmh[var] <- NULL
     dd <- dd[,names(dd)[grep(i,names(dd))]]
     names(dd) <- gsub(i,"", names(dd))
     dd$treatment <- gsub("_","",i)
     cbind(dmh,dd)
   })
   
   dm <- do.call(carobiner::bindr, dm)
   dm$yield[!is.na(dm$plot_area)] <- (dm$yield[!is.na(dm$plot_area)]/dm$plot_area[!is.na(dm$plot_area)])
   dm$yield <- ifelse(is.na(dm$plot_area), NA, dm$yield)
   dm$plant_density[!is.na(dm$plot_area)] <- (dm$plants[!is.na(dm$plot_area)]/dm$plot_area[!is.na(dm$plot_area)])
   dm$cob_density[!is.na(dm$plot_area)] <- (dm$cobs[!is.na(dm$plot_area)]/dm$plot_area[!is.na(dm$plot_area)])
   dm$crop <- "maize"
   dm$planting_date <- as.character(as.Date(dm$planting_date, "%b %d, %Y"))
   dm$harvest_date <- as.character(as.Date(dm$harvest_date, "%b %d, %Y"))
   
## Processing rice data 
   
   frc <- ff[basename(ff) == "riceClean.csv"]
   frm <- ff[basename(ff) == "grainMoistureR.csv"]
   frs <- ff[basename(ff) == "dRed.csv"]
   frf <- ff[basename(ff) == "Ralfert.csv"]
   
   ### read()
   rrc <- read.csv(frc) 
   rrm <- read.csv(frm) 
   rrs <- read.csv(frs) 
   rrf <- read.csv(frf) 
   
## riceClean file 
   
   drh <- data.frame(
      trial_id= rrc$HHID,
      event= rrc$event,
      adm1= rrc$stateEA,
      location= rrc$lga,
      year= rrc$Year,
      country=rrc$country,
      longitude=rrc$Longitude,
      latitude= rrc$Latitude,
      yield_SSR= rrc$riceGrainFW_plot_SSR,
      yield_BRR= rrc$riceGrainFW_plot_BRR,
      yield_ZCC= rrc$riceGrainFW_plot_ZCC,
      land_prep_method= rrc$tillageMethod1
      
   )
   
## Selecting the harvest event
   
   drh <- drh[drh$event == "event6R",]
   
   ## Adding plot size 
   drh <- merge(drh,dh1, by=c("trial_id","adm1"), all.x= TRUE)
   
## grainMoistureR
   
   drh1 <- data.frame(
      trial_id= rrm$HHID,
      yield_moisture_BRR= rrm$moistureBRR,
      yield_moisture_SSR= rrm$moistureSSR,
      yield_moisture_ZCC= rrm$moistureZCC
   )
   
   drh <- merge(drh,drh1, by=c("trial_id"), all.x= TRUE) 
   
## fertilizer file for rice (Ralfert)
   
   drh2 <- data.frame(
      trial_id= rrf$HHID,
      adm1= rrf$state,
      elevation= rrf$Altitude,
      planting_date= rrf$plantingDate,
      OM_used= ifelse(grepl("yes",rrf$orgApply1)|grepl("yes",rrf$orgApply2),TRUE,FALSE),
      fertilizer_price= as.character(rowSums(rrf[,c("costUrea","costNPK")])),
      variety= rrf$variety,
      row_spacing= substr(rrf$plantingDensity,1,4),
      plant_spacing= substr(rrf$plantingDensity,8,11),
      fertilizer_amount_BRR= rrf$NPKBRR + rrf$plotUreaSplit1BRR,
      #fert= rrf$plotNPKBRR_B + rrf$plotUreaBRR_BE*0.46,
      fertilizer_type_BRR= "NPK;urea"
   )
   
   drh2$adm1[grep("BE",drh2$adm1)] <- "benue"
   drh2$adm1[grep("KD",drh2$adm1)] <- "kaduna"
   drh2$adm1[grep("KA",drh2$adm1)] <- "kano"
   drh2$fertilizer_type_SSR <- NA
   drh2$fertilizer_type_ZCC <- "NPK"
   drh2$fertilizer_amount_SSR <- NA ## Unknwon 
   drh2$fertilizer_amount_ZCC <- 0
   
   drh <- merge(drh,drh2, by=c("trial_id","adm1"), all.x= TRUE) 
   
   var <- names(drh)[grep("SSR|BRR|ZCC",names(drh))] 
   dr <- lapply(c("_BRR","_SSR","_ZCC"),\(i){
      dd <- drh[,var]
      drh[var] <- NULL
      dd <- dd[,names(dd)[grep(i,names(dd))]]
      names(dd) <- gsub(i,"", names(dd))
      dd$treatment <- gsub("_","",i)
      cbind(drh,dd)
   })
   
   dr <- do.call(carobiner::bindr, dr)
   dr$yield[!is.na(dr$plot_area)] <- (dr$yield[!is.na(dr$plot_area)]/dr$plot_area[!is.na(dr$plot_area)])*10000
   dr$crop <- "rice"
   
## Combine Maize data and Rice data 
   
   d <- carobiner::bindr(dm,dr)
   d$event <- d$year <- NULL
### Fixing date
   d$planting_date <- gsub(" |, ", "-", d$planting_date) 
   d$planting_date <- as.character(as.Date(d$planting_date,"%b-%d-%Y"))
   
### Adding variables 
   
   d$country <- "Nigeria"
   d$is_survey <- FALSE
   d$on_farm <- TRUE
   d$yield_part <- "grain"
   d$geo_from_source <- TRUE
   d$irrigated <- FALSE
   
   d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
   
## data type
   
   d$row_spacing <- as.numeric(gsub("othe",NA,d$row_spacing))*100 #cm
   d$plant_spacing <- as.numeric(d$plant_spacing)*100 #cm
   
   carobiner::write_files(dset, d, path=path)
}
