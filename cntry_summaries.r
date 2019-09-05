#rm(list=ls())

# Code to produce country level summaries of benefits and costs for a solution.
# Also produces error estimates for those values. For carbon, this is based on
# the Delta C raster +/- 1.96 * SD, where SD is the standard deviation extracted
# from Cata's raster. Note that *negative values are permitted* in the Delta C
# raster, therefore it is acceptable for the error estimates to also include
# negative values.


library(raster)

country.regions = T # if false, use ecoregions instead
run.continents = F # overwrites country.regions, and computes per continents instead
area.only = T # compute only restored areas

# set the global input directory variable
setwd('~/Documents/IIS_PROJECTS/plangea-legacy')
dir <- "inputdata_v8/"

# load the master index:
load(paste0(dir, "master_index.RData"))



# constant to convert tonnes/ha to mega tonnes (also = peta grammes)
const_cb <- 1E-4

# constant to convert oc to $millions
const_oc <- 1E-4

# constant to convert area to millions of km2
const_area <- 1E-6

# constant to convert mean abs extinction risk delta proportion to a percent
const_bd <- 1E2



### PREPROCESSING - ONLY NEED TO RUN THIS ONCE EVERY TIME THE MASTER INDEX CHANGES ###
# Creates a data structure that is saved to the input data folder and can be reused.

# load the CB and CB SD rasters
r.cb = raster('/home/alvaro/Documents/IIS_PROJECTS/global_rest_prior/global_rest_priorization/rawdata/5km/others/DELTA_C_BiomassSoil30cm_v12.1.tif')
r.cbsd = raster('/home/alvaro/Documents/IIS_PROJECTS/global_rest_prior/global_rest_priorization/rawdata/5km/others/C_SD_Biomass_Combined_COMPLETE_v12.1.tif')

# Check that projections and dimensions are identical:
r.cb
r.cbsd

# extract the CB SD values:
cbsd <- values(r.cbsd)[master_index]
summary(cbsd)
# fill in NA values with mean carbon
# this is not needed as there are currently no NA cells, but it is safe to run this code
# because it does nothing if there are no NA cells
if (length(which(is.na(cbsd))) > 0) cbsd <- replace(cbsd, which(is.na(cbsd)), mean(cbsd, na.rm=T))
save(cbsd, file=paste0(dir, "cbsd.RData"))
summary(cbsd)


# EXTRACT THE GRADIENT RASTER VALUES
# r.gr <- raster('./display_results_WRLD_v8/scen_world-world_gradient-cb-bd-oc_total.restored.pu.tif')
# r.gr
# 
# gradient <- values(r.gr)[master_index]
# summary(gradient)
# # fill in NA values with mean carbon
# # this is not needed as there are currently no NA cells, but it is safe to run this code
# # because it does nothing if there are no NA cells
# if (length(which(is.na(gradient))) > 0) gradient <- replace(gradient, which(is.na(gradient)), mean(gradient, na.rm=T))
# save(gradient, file=paste0(dir, "gradient.RData"))
# summary(gradient)

### END PREPROCESSING ###




### FUNCTION THAT TAKES A SCENARIO SOLUTION AS INPUT AND PRODUCES COUNTRY-LEVEL SUMMARIES

# assumes that the global "dir" variable has been set that defines the input folder

# the function writes a csv file that can be opened in Excel in order to easily create
# a table that can be embedded in Word - so the countries are ordered alphabetically


# Alvaro: I set this as a variable because it is likely to be different on your system:
# countrycodesfile = "../world-prod-estimates/countries-shp/countries-code.csv"

if (country.regions){
  countrycodesfile = "./inputdata_v8/countries-code.csv"
  outfile_suffix = 'country'
  load(paste0(dir, "cntry.RData"))
} else {
  outfile_suffix = 'ecoregion'
  countrycodesfile = "/home/alvaro/Documents/IIS_PROJECTS/global_rest_prior/global_rest_priorization/rawdata/5km/Ecoregions/ecoregions2017_moll_5km_all_classes_mosaic.csv"
  cntry = raster("/home/alvaro/Documents/IIS_PROJECTS/global_rest_prior/global_rest_priorization/rawdata/5km/Ecoregions/ecoregions2017_moll_5km_all_classes_mosaic.tif")[master_index]
}

if (run.continents){
  countrycodesfile = "./inputdata_v8/continents-code.csv"
  outfile_suffix = 'continents'
  load(paste0(dir, "continents.RData"))
  }


source('./post_proc_summary.R')

# LOAD THE FUNCTION BELOW NOW, THEN RUN IT:
#resultfile = "country-analysis/scen_cb-bd-oc_res.total.restored.pu_w_5.RData"
#outfile = "scen_cb-bd-oc_res.total.restored.pu_w_5.csv"
#

# IKI proposal #################################################################
# Selecting countries: Brazil (33), Colombia (48), Costa Rica (51), and Peru (171)
#subsetlist = c(48, 51, 171)
#targetlist = 0.05 * 2:20

#for (tgt in targetlist){
#  bd_file = paste0('./opt_results_CBD_v8/scen_bd-global-limit-at_', tgt, '_res.prop.restored.pu_w_1.RData')
#  bd_out = paste0("./display_results_CBD_v8/scen_bd-ctrylim_", tgt, "_per_country_post_processed.csv")
#  print(paste0('Processing file ', bd_out))
#  postproc.country.summary(bd_file, cntry, countrycodesfile, subsetlist, bd_out)
#  oc_file = paste0('./opt_results_CBD_v8/scen_oc-ctrylim_', tgt, '_res.prop.restored.pu_w_1.RData')
#  oc_out = paste0("./display_results_CBD_v8/scen_oc-ctrylim_", tgt, "_per_country_post_processed.csv")
#  print(paste0('Processing file ', oc_out))
#  postproc.country.summary(oc_file, cntry, countrycodesfile, subsetlist, oc_out)
#  }
# IKI proposal #################################################################



sub_list = function(list, pattern, invert=F){
  if (invert){
    list[sapply(list, function(x){!grepl(pattern, x)})]
  } else {
    list[sapply(list, function(x){grepl(pattern, x)})]    
  }
}

# com ublim_15% e ctry_15%, e ublimecon e ctryecon

res_filenames = dir(path = "./opt_results_CBD_v8/", pattern = "res.total.restored.pu")
res_filenames = sub_list(res_filenames, '_step', invert=T)

cb_list = sub_list(res_filenames, 'scen_cb')
cb_list = sub_list(cb_list, 'cb-bd', invert=T)
cb_list = sub_list(cb_list, 'cb-oc', invert=T)
cb_list = sub_list(cb_list, 'global-limit')
cb_list = sub_list(cb_list, 'w_1.RData')
# cb_list = cb_list[c(1, 3, 10, 11, 13)]

bd_list = sub_list(res_filenames, 'scen_bd')
bd_list = sub_list(bd_list, 'bd-oc', invert=T)
bd_list = sub_list(bd_list, 'global-limit')
bd_list = sub_list(bd_list, 'w_1.RData')
#bd_list = bd_list[c(1, 3, 10, 11, 13)]

oc_list = sub_list(res_filenames, 'scen_oc')
oc_list = sub_list(oc_list, 'global-limit')
oc_list = sub_list(oc_list, 'w_1.RData')
#oc_list = oc_list[c(1, 3, 10, 11, 12)]

# environment trade-off
et_list = sub_list(res_filenames, 'scen_cb-bd')
et_list = sub_list(et_list, 'global-limit')
et_list = sub_list(et_list, 'w_9')

# multi-criteria (mc)
mc_list = sub_list(res_filenames, 'scen_cb-bd-oc')
mc_list = sub_list(mc_list, 'global-limit')
mc_list = sub_list(mc_list, 'w_5')
# mc_list = mc_list[c(1, 3, 10, 11, 13)]

overall_list = c(cb_list, bd_list, oc_list, et_list, mc_list)

for (resultfile in overall_list[!is.na(overall_list)]) {
  res.file = paste0("./opt_results_CBD_v8/", resultfile)

    outfile = paste0("./display_results_CBD_v8/",
                   gsub(pattern = ".RData", "",
                        resultfile), "-per_", outfile_suffix, "_post_processed.csv")
  
  if (area.only) {
    if (!exists("res.df")) {
      cntry.codes = read.csv(countrycodesfile)
      res.df = data.frame('Country' = cntry.codes$NAME[cntry.codes$CODE %in% sort(unique(cntry))])
    }
    print(paste0('[', ncol(res.df), '] Processing file ', res.file))
    load("./inputdata_v8/A.RData")
    load(res.file)
    res = res.total.restored.pu * A
    res.df[,ncol(res.df)+1] = sapply(sort(unique(cntry)), function(x){sum(res * (cntry == x))})
    
  } else {
    postproc.country.summary(res.file, cntry, countrycodesfile, outfile)
  }
}

# cb_list, bd_list, oc_list, mc_list
if (area.only){
  cb.df = res.df[,1]; bd.df = res.df[,1]; oc.df = res.df[,1];
  et.df = res.df[,1]; mc.df = res.df[,1];
  
  cb.df = cbind(cb.df, res.df[,1 + 1:length(cb_list)])
  bd.df = cbind(bd.df, res.df[,(length(cb_list)+1) + 1:length(bd_list)])
  oc.df = cbind(oc.df, res.df[,(length(cb_list)+length(bd_list)+1) + 1:length(oc_list)])
  et.df = cbind(et.df, res.df[,(length(cb_list)+length(bd_list)+length(oc_list)+1) + 1:length(et_list)])
  mc.df = cbind(mc.df, res.df[,(ncol(res.df)-(length(mc_list)-1)):ncol(res.df)])
  
  names(cb.df) = c(outfile_suffix, paste0(5*(1:20), '%'))
  names(bd.df) = names(cb.df)
  names(oc.df) = names(cb.df)
  names(et.df) = names(cb.df)
  names(mc.df) = names(cb.df)
  
  write.csv(cb.df, paste0("./display_results_CBD_v8/scen_cb-global-limit-per_", outfile_suffix, "_aggregated.csv"))
  write.csv(bd.df, paste0("./display_results_CBD_v8/scen_bd-global-limit-per_", outfile_suffix, "_aggregated.csv"))
  write.csv(oc.df, paste0("./display_results_CBD_v8/scen_oc-global-limit-per_", outfile_suffix, "_aggregated.csv"))
  write.csv(et.df, paste0("./display_results_CBD_v8/scen_cb-bd-global-limit-per_", outfile_suffix, "_aggregated.csv"))
  write.csv(mc.df, paste0("./display_results_CBD_v8/scen_cb-bd-oc-global-limit-per_", outfile_suffix, "_aggregated.csv"))
}







