# R script for EiA version of"carob"

## ISSUES
# 1. DOI and much of the metadata is missing
# 2. Data reads are still unstable and user needs to have access
# 3. License is missing (CC-BY)?
# 4. Many valuable variables that need to be integrated still...
# 5. Intercrops arrangement and cost benefit balance(cbb) are not yet included in carob as standard variable
# 6. Some rows have the harvest date within 45 days after planting wchih is not correct for soybean or maize

carob_script <- function(path) {
   
   "
	SOME DESCRIPTION GOES HERE...

"
   uri <- "inzOQVrqT0rowUbaxaDsJFV2"
   
   
   meta <- data.frame(
      # Need to fill-in metadata...
      # carobiner::read_metadata(uri, path, group, major=2, minor=0),
      # uri = carobiner::simple_uri(uri),
      dataset_id = uri,
      uri = uri,
      authors =NA,
      data_institute =NA,
      title = NA,
      group = "",
      license = 'Some license here...',
      project = 'Excellence in Agronomy ',
      usecase_code= "USC010",
      usecase_name= "GH-CerLeg-Esoko",
      activity="experiment",
      carob_contributor = 'Cedric Ngakou',
      data_type = "on-farm experiment",
      response_vars= "yield",
      treatment_vars = "intercrops;fertilizer_type;N_fertilizer;P_fertilizer;K_fertilizer",
      carob_date="2024-07-24"
      )
   
   # Manually build path (this can be automated...)
   ff <- carobiner::get_data(uri = uri, path = path, group = "", files = list.files("/home/jovyan/carob-eia/data/raw/eia/Ghana-Soybean-NOT/", full.names = T))
   
   f <-  ff[basename(ff)=="Ghana Soy Esoko_Data_Fertilizer Type Recommendation Trial 2023.xlsx"]
   f1 <- ff[basename(ff)=="Ghana Soy Esoko_Data_Nutrient Omission Trial 2023.xlsx"]
   f2 <- ff[basename(ff)=="Ghana Soy Esoko_Data_Vareital Suitability Trial 2023.xlsx"]
   f3 <- ff[basename(ff)=="Ghana Soy Esoko_Data_Intercropping Trial 2023.xlsx"]
   
   ## process Fertilize type Recommendation trial
   # Read file
   r <- carobiner::read.excel.hdr(f,sheet = "Fertilizer Type Recommenatiod",skip=0)
   d <- data.frame(
      country = "Ghana",
      location= r$District,
      longitude= r$Lontitude,
      latitude= r$Latitude,
      crop= tolower(r$Crop),
      fertilizer_type= r$Fertilizer.type,
      rep= as.integer(r$Replicate),
      planting_date= r$Planting.date,
      harvest_date= r$Harvesting.date,
      yield= r$Yield,
      cbb = r$CBB, #Cost_benefit_balance ### not yet a carob variable
      currency= "USD",
      season= as.character(r$Year),
      intercrops= "none",
      trial_id= "Soy Esoko_Data_Fertilizer Type Recommendation Trial"
   )
   
   ### process Nutrient omission trial
   r1 <- carobiner::read.excel.hdr(f1, sheet = "Nutrient Omission",skip=0,na=c("zzzz", NA))
   d1 <- data.frame(
      country= "Ghana",
      location= r1$District,
      longitude= r1$Lontitude,
      latitude= r1$Latitude,
      rep= as.integer(r1$Replicate),
      treatment= r1$Treatment,
      N_fertilizer= as.numeric(r1$N),
      P_fertilizer= r1$P,
      K_fertilizer= r1$K,
      Ca_fertilizer= r1$Ca,
      Mg_fertilizer= r1$Mg,
      Zn_fertilizer= r1$Zn,
      Fe_fertilizer= r1$Fe,
      planting_date= r1$Planting.date,
      harvest_date= r1$Harvesting.date,
      yield= r1$Yield,
      cbb= r1$CBB,
      currency= "USD",
      crop= tolower(r1$Crop),
      season= as.character(r1$Year),
      intercrops= "none",
      trial_id= "Esoko_Data_Nutrient Omission Trial"
   )
   
   ###Process varietal suitability Trial
   r2 <- carobiner::read.excel.hdr(f2, sheet = "Varietal Suitability ",skip=0)
   
   d2 <- data.frame(
      country= "Ghana",
      location= r2$District,
      longitude= r2$Lontitude,
      latitude= r2$Latitude,
      crop= tolower(r2$Crop),
      season= as.character(r2$Year),
      rep= as.integer(r2$Replicate),
      variety= r2$Variety,
      maturity_days= r2$Maturity,
      planting_date= r2$Planting.date.dd.mm.yyyy,
      harvest_date= r2$Harvesting.date.dd.mm.yyyy,
      yield= r2$Yield,
      cbb= r2$CBB,
      currency= "USD",
      intercrops= "none",
      trial_id= "Esoko_Data_Vareital Suitability Trial"
   )
   
   d2$maturity_days[d2$maturity_days=="115-120"] <- mean(c(115,120))   
   d2$maturity_days[d2$maturity_days=="110-115"] <- mean(c(110,115)) 
   d2$maturity_days[d2$maturity_days=="80-85"] <- mean(c(80,85))
   d2$maturity_days[d2$maturity_days=="90-95"] <- mean(c(90,95))
   d2$maturity_days[d2$maturity_days=="85-90"] <- mean(c(85,90))
   
   ## process Esoko_Data_Intercropping Trial
   d3 <-  lapply(c("Intercrop I_Yield","Intercrop II_Yield"), \(i){
      r1 <- carobiner::read.excel.hdr(f3, sheet =i, skip=0)
      names(r1) <- gsub("Maize.fertilizer","Fertilizer.Maize",names(r1))
      names(r1) <- gsub("Soybean.fertilizer","Fertilizer.Soybean",names(r1))
      
      ## crop maize yield
      dd <- data.frame(
         country= "Ghana",
         location= r1$District,
         longitude= r1$Longitude,
         latitude= r1$Latitude,
         rep= as.integer(r1$Replicate),
         intercrops_arrangement= r1$Intercrop.arrangement,
         fertilizer_type= r1$Fertilizer.Maize,
         planting_date= r1$Maize.planting.date,
         harvest_date= r1$Maize.harvesting.date,
         yield= r1$Maize.yield,
         #season= r1$Year,
         crop= "maize",
         intercrops= "soybean",
         trial_id= "Esoko_Data_Intercropping Trial"
      )
      
      ## crop soybean yield
      dd1 <- data.frame(
         country= "Ghana",
         location= r1$District,
         longitude= r1$Longitude,
         latitude= r1$Latitude,
         rep= as.integer(r1$Replicate),
         intercrops_arrangement= r1$Intercrop.arrangement,
         fertilizer_type= r1$Fertilizer.Soybean,
         planting_date= r1$Soybean.planting.date,
         harvest_date= r1$Soyabean.harvesting.date,
         yield= r1$Soybean.yield,
         #season= r1$Year,
         crop= "soybean",
         intercrops= "maize",
         trial_id= "Esoko_Data_Intercropping Trial"
      )
      
      rbind(dd,dd1)
      
   })
   
   d3 <- do.call(rbind,d3)
   
   ## Economic analysis data
   d33 <-  lapply(c("Intercrop I_EconomicAnalysis","Intercrop II_EconomicAnalysis"), \(i){
      r2 <- carobiner::read.excel.hdr(f3, sheet =i, skip=0)
      
      data.frame(
         location= r2$District,
         longitude= r2$Longitude,
         latitude= r2$Latitude,
         rep= as.integer(r2$Replicate),
         intercrops_arrangement= r2$Intercrop.arrangement,
         cbb=r2$CBB,
         currency= "USD"
      )
   })
   
   d33 <- do.call(rbind,d33)
   
   ## Intercroping trial data with cost benefit balance (cbb)
   d3 <- merge(d3,d33,by=c("location","rep","longitude","latitude","intercrops_arrangement"), all.x=TRUE)
   ## keep rows with no empty yield 
   d3 <- d3[!is.na(d3$yield),] 
   ### Joint all the trials
   
   d <- carobiner::bindr(d,d1,d2,d3)
   
   d$on_farm <- TRUE
   d$is_survey <- FALSE
   d$irrigated <- as.logical(NA)
   d$yield_part <- "grain"
   d$inoculated <- FALSE
   d$inoculated[grep("Inoculant",d$fertilizer_type)] <- TRUE
   ### Fixing fertilizer type  
   d$fertilizer_type <- gsub(" only","",d$fertilizer_type)
   d$fertilizer_type <- gsub("\\+","",d$fertilizer_type)
   d$fertilizer_type <- gsub("  | ",";",d$fertilizer_type)
   
   
   #d$date1 <- d$planting_date
   #d$date2 <- d$harvest_date
   d$planting_date <- as.character(as.Date(d$planting_date, "%d/%m/%Y"))
   d$harvest_date <- as.character(as.Date(d$harvest_date, "%d/%m/%Y"))
   #d$planting_date[is.na(d$planting_date)] <- as.character(as.Date(as.numeric(d$date1[is.na(d$planting_date)]),"1899-12-30"))
   #d$harvest_date[is.na(d$harvest_date)] <- as.character(as.Date(as.numeric(d$date2[is.na(d$harvest_date)]),"1899-12-30"))
   #d$date1 <-  d$date2 <- NULL
   
   ## Data type
   d$maturity_days <- as.numeric(d$maturity_days)
   
   carobiner::write_files(meta, d, path=path)
   
}

# carob_script(path)

