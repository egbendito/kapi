# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. Many valuable variables that need to be integrated still...
# 5. ...

carob_script <- function(path) {
   
   "
	SOME DESCRIPTION GOES HERE...

"
   
   # uri <- "doi:Chinyanja-Solidaridad-Soy-AddOn"
   uri <- "USC016"
   group <- "eia"
   
   dset <- data.frame(
      # Need to fill-in metadata...
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      # uri = carobiner::simple_uri(uri),
      uri = uri,
      dataset_id = uri,
      authors =NA,
      data_institute =NA,
      title = NA,
      group = group,
      license = 'Some license here...',
      carob_contributor = 'Cedric Ngakou',
      project = 'Excellence in Agronomy;Chinyanja-Solidaridad-Soy;AddOn',
      data_type = "survey", # or, e.g. "on-farm experiment", "survey", "compilation"
      carob_date="2024-06-24"
      # treatment_vars = "longitude;latitude;variety;N_fertilizer;P_fertilizer;K_fertilizer"
   )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = group, files = list.files("/home/jovyan/carob-eia/data/raw/eia/Chinyanja-Solidaridad-Soy-AddOn/", full.names = T))
   
   # Retrieve relevant file
   f1 <- ff[grep("farmer_segmentation_July",basename(ff))]
   
   # Read relevant files
   ## Process geodata and some crop management practices 
   process <- function(f){ 
      r <- carobiner::read.excel(f,sheet ="EiA_AddOn_Full_Survey_Solidarid")
      r <- r[-1,] ## remove the first row of r
      names(r) <- gsub("_index","index",names(r))
      d <- data.frame(
         country = r$country,
         on_farm = FALSE,
         is_survey = TRUE,
         adm1=r$admin_1,
         adm2=r$admin_2,
         location=r$village,
         longitude=as.numeric(r$longitude),
         latitude=as.numeric(r$latitude),
         currency=r$local_currency,
         trial_id = r$barcodehousehold_1,
         fertilizer_amount= as.numeric(r$fertiliser_amount),
         fertilizer_type= r$fertiliser_type,
         irrigation_dates= r$Irrigation_months, 
         irrigated= r$land_irrigated, 
         land_prep_method=r$tillage_power,
         index=r$index,
         fertilizer_constraint= r$constraint_fertilizers ## Not a carob variable
         
      )
      d
   }
   
   d <- lapply(f1,process) 
   d <- do.call(rbind, d)
   
   d$irrigated[d$irrigated=="No"] <- FALSE
   d$irrigated[d$irrigated=="Yes"] <- TRUE
   d$irrigated[d$irrigated=="no_answer"] <- as.logical(NA)
   
   ### Adding N fertilizer ###
   ## More details on fertilizer can be found on  "Fertilizer_subsidy_bought" sheet
   d$N_fertilizer <- 0
   i <- grep("urea",d$fertilizer_type)
   d$N_fertilizer[i] <- (d$fertilizer_amount[i])*0.46
   d$N_fertilizer[grep("urea CAN",d$fertilizer_type)] <- d$fertilizer_amount[grep("urea CAN",d$fertilizer_type)]*0.27 + d$N_fertilizer[grep("urea CAN",d$fertilizer_type)]
   d$N_fertilizer[grep("urea NPK17",d$fertilizer_type)] <- 17 + d$N_fertilizer[grep("urea NPK17",d$fertilizer_type)]
   d$N_fertilizer[grep("urea NPK23",d$fertilizer_type)] <- 23 + d$N_fertilizer[grep("urea NPK23",d$fertilizer_type)]
   d$N_fertilizer[grep("super_d urea",d$fertilizer_type)] <- 10 + d$N_fertilizer[grep("super_d urea",d$fertilizer_type)]
   d$N_fertilizer[grep("NPK10 urea" ,d$fertilizer_type)] <- 10 + d$N_fertilizer[grep("NPK10 urea" ,d$fertilizer_type)] ## super_d : 10:20:20 
   d$N_fertilizer[grep("NPK10" ,d$fertilizer_type)] <- 10 
   d$N_fertilizer[grep("super_d",d$fertilizer_type)] <- 10
   d$N_fertilizer[grep("NPK15" ,d$fertilizer_type)] <- 15
   d$N_fertilizer[grep("NPK19" ,d$fertilizer_type)] <- 19
   d$N_fertilizer[grep("NPK23"   ,d$fertilizer_type)] <- 23
   
   ### Adding P fertilizer ###
   d$P_fertilizer <- 0
   d$P_fertilizer[grep("NPK17",d$fertilizer_type)] <- 17 
   d$P_fertilizer[grep("NPK23",d$fertilizer_type)] <- 10.5/2.29
   d$P_fertilizer[grep("super_d",d$fertilizer_type)] <- 24/2.29
   d$P_fertilizer[grep("NPK10" ,d$fertilizer_type)] <- 20/2.29
   d$P_fertilizer[grep("NPK15" ,d$fertilizer_type)] <- 15/2.29
   d$P_fertilizer[grep("NPK19" ,d$fertilizer_type)] <- 19/2.29
   
   ## Adding K fertilizer ###
   d$K_fertilizer <- 0
   d$K_fertilizer[grep("NPK17",d$fertilizer_type)] <- 17/1.2051
   d$K_fertilizer[grep("NPK15" ,d$fertilizer_type)] <- 15/1.2051
   d$K_fertilizer[grep("NPK10" ,d$fertilizer_type)] <- 10/1.2051
   d$K_fertilizer[grep("NPK23",d$fertilizer_type)] <- 0
   d$K_fertilizer[grep("super_d",d$fertilizer_type)] <- 20/1.2051
   d$K_fertilizer[grep("NPK19" ,d$fertilizer_type)] <- 19/1.2051
   
   ### Process yield data #################################
   
   process <- function(f){
      r1 <- carobiner::read.excel(f,sheet = "crop_repeat")
      r1<- r1[-1,] ## remove the first row of r1
      names(r1) <- gsub("_index","index",names(r1))
      d1 <- data.frame(
         crop= r1$crop_name,
         rep= as.integer(r1$crop_rep_number),
         season=r1$season_grown,
         yield= as.numeric(r1$crop_yield),
         crop_system= r1$crop_intercrop, ## Not a carob variable
         crop_price= as.numeric(r1$crop_sold_income),
         #OM_type= r1$crop_residue_use,
         index=r1$index
      )
      d1
   }
   
   d1 <- lapply(f1,process) 
   d1 <- do.call(rbind, d1)
   ## merge geo data and  yield data
   
   d <- merge(d,d1,by="index", all.x=TRUE)
   
   ### Adding others variables ##################################
   
   proces <- function(f){ 
     r.nms <- colnames(readxl::read_excel(f, sheet = "plot_information_repeat", range = "A1:FO2"))
     r2 <- readxl::read_excel(f, sheet = "plot_information_repeat", skip = 2, col_names = FALSE)
     colnames(r2) <- r.nms
     names(r2) <- gsub("_index","index",names(r2))
     # EGB:
     # # There seems to be some errors in the original data with the planting and harvest dates
     for (rows in which(format(as.Date(r2$planting_date), "%Y")<2022)) {
       r2$planting_date[rows] <- paste0(2022, "-",
                                        format(as.Date(r2$planting_date), "%m"), "-",
                                        format(as.Date(r2$planting_date), "%d"))
     }
     for (rows in which(format(as.Date(r2$harvesting_date), "%Y")>2023)) {
       r2$harvesting_date[rows] <- paste0(2023, "-",
                                          format(as.Date(r2$harvesting_date), "%m"), "-",
                                          format(as.Date(r2$harvesting_date), "%d"))
     }
     d2 <- data.frame(
       variety= r2$variety,
       previous_crop=r2$previous_crop,
       intercrops= r2$inter_crop,
       planting_date= as.character(as.Date(r2$planting_date, "%Y-%m-%d")),
       harvest_date= as.character(as.Date(r2$harvesting_date, "%Y-%m-%d")),
       season_constraint= r2$heat_stress,
       pest_species= r2$pest_name,
       pest_severity=r2$pest,
       disease_severity= r2$disease,
       disease= r2$disease_name,
       weed_severity= r2$weeds,
       seed_method= r2$seeding_method,
       index=r2$index
     )
     
     d2
   }
   
   d2 <- do.call(rbind, lapply(f1,proces))
   d <- merge(d,d2,by="index", all.x=TRUE)
   d$index <- NULL
   
   ### Fixing crop content ###
   C <- carobiner::fix_name(d$crop)
   C <- gsub("potatoIrish","potato",C)
   C <- gsub("potato_sweet","sweetpotato",C)
   C <- gsub("beansBush","common bean",C)
   C <- gsub("beansCommon","common bean",C)
   C <- gsub("none1","none",C)
   C <- gsub("other","none",C)
   C <- gsub("other1","none",C)
   C <- gsub("finger_millet","finger millet",C)
   C <- gsub("groundnuts","groundnut",C)
   C <- gsub("peas" ,"pea",C)
   C <- gsub("none1" ,"none",C)
   d$crop <- C
   
   ### Fixing intercrop content ###################
   
   C <- carobiner::fix_name(d$intercrops)
   C <- gsub("beansCommon","common bean",C)
   C <- gsub("beansBush","common bean",C)
   C <- gsub("beansClimbing","common bean",C)
   C <- gsub("Beans","common bean",C)
   C <- gsub(NA ,"none",C)
   C <- gsub("other","none",C)
   C <- gsub("no_answer","none",C)
   C <- gsub("finger_millet","finger millet",C)
   C <- gsub("groundnuts","groundnut",C)
   C <- gsub("pigeon_pea" ,"pigeon pea",C)
   d$intercrops <- C
   
   ### Fixing previous crop content ###
   
   C <- carobiner::fix_name(d$previous_crop)
   C <- gsub("potatoIrish","potato",C)
   C <- gsub("potato_sweet","sweetpotato",C)
   C <- gsub("beansBush","common bean",C)
   C <- gsub("beansClimbing","common bean",C)
   C <- gsub("beansCommon","common bean",C)
   C <- gsub("none1","none",C)
   C <- gsub("other","none",C)
   C <- gsub("other1","none",C)
   C <- gsub("no_answer","none",C)
   C <- gsub("carrots","carrot",C)
   C <- gsub("groundnuts","groundnut",C)
   C <- gsub("peas" ,"pea",C)
   C <- gsub("That part of the farm has just been opened","none",C)
   C <- gsub("New field","none",C)
   C <- gsub("Batata reno","unknown",C)
   C <- gsub("Sunflower","unknown",C)
   d$previous_crop <- C
   
   ## Fixing fertilizer type #################
   
   p <- carobiner::fix_name(d$fertilizer_type)
   p <- gsub(" ","; ",p)
   p <- gsub("NPK10","NPK",p)
   p <- gsub("NPK17","NPK",p)
   p <- gsub("NPK17","NPK",p)
   p <- gsub("NPK15","NPK",p)
   p <- gsub("ammonium_sulphate","DAS",p)
   p <- gsub("ammonium_nitrate","AN",p)
   p <- gsub("Mix_UREA_With_manure","urea",p)
   p <- gsub("NPK19","NPK",p)
   p <- gsub("NPK23","NPK",p)
   p <- gsub("other","none",p)
   p <- gsub("super_d","none",p)
   p <- gsub("NA","none",p)
   p <- gsub("Innoculant","unknown",p)
   d$fertilizer_type <- p
   
   ### 
   d$land_prep_method[d$land_prep_method=="manual" | d$land_prep_method== "animal" | d$land_prep_method=="manual animal" | d$land_prep_method=="animal manual" | d$land_prep_method=="mechanical"
                      | d$land_prep_method=="animal 4wheel_tractor" | d$land_prep_method=="4wheel_tractor manual" | d$land_prep_method=="4wheel_tractor" | d$land_prep_method=="manual 4wheel_tractor"] <- "conventional"
   
   # EGB:
   # # Fix irrigation dates
   d$irrigation_dates <- gsub(" ", "; ", d$irrigation_dates)
   d$irrigation_dates <- gsub("jan", "2023-01", d$irrigation_dates)
   d$irrigation_dates <- gsub("feb", "2023-02", d$irrigation_dates)
   d$irrigation_dates <- gsub("mar", "2023-03", d$irrigation_dates)
   d$irrigation_dates <- gsub("apr", "2023-04", d$irrigation_dates)
   d$irrigation_dates <- gsub("may", "2023-05", d$irrigation_dates)
   d$irrigation_dates <- gsub("jun", "2022-06", d$irrigation_dates)
   d$irrigation_dates <- gsub("jul", "2022-07", d$irrigation_dates)
   d$irrigation_dates <- gsub("aug", "2022-08", d$irrigation_dates)
   d$irrigation_dates <- gsub("sep", "2022-09", d$irrigation_dates)
   d$irrigation_dates <- gsub("oct", "2022-10", d$irrigation_dates)
   d$irrigation_dates <- gsub("nov", "2022-11", d$irrigation_dates)
   d$irrigation_dates <- gsub("dec", "2022-12", d$irrigation_dates)
   
   
   message("yield is given in kg,bags and tonnes instead of kg/ha")
   
   carobiner::write_files(dset, d, path=path)
}

# carob_script(path)
