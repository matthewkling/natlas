#Aquiring datasets/where datasets were found
setwd("/Users/lauraalexander/taxorama")

#### Download complete iNat dataset####
  # Data suggested by Ken-ichi, found at http://www.inaturalist.org/observations/gbif-observations-dwca.zip

download.file("http://www.inaturalist.org/observations/gbif-observations-dwca.zip", "raw_data/all_inat_obs")
    unzip("raw_data/all_inat_obs", files = "observations.csv", exdir = "raw_data")
    file.remove("raw_data/all_inat_obs")
    
  
#### Download National Park Boundaries####
    # Found at https://catalog.data.gov/dataset/national-park-boundariesf0a4c
    # Metadata at https://catalog.data.gov/harvest/object/d66865e2-e232-4da9-b399-1da516e1b73c/html
    
    # download.file("http://gstore.unm.edu/apps/rgis/datasets/7bbe8af5-029b-4adf-b06c-134f0dd57226/nps_boundary.original.zip", "raw_data/park_boundaries_download")
    # unzip("raw_data/park_boundaries_download", exdir = "raw_data/park_boundaries")
    # file.remove("raw_data/park_boundaries_download")

#List source for species list
    
#### Sources for the county data ####
#Links to populatopn, shapefiles, economic/educational all exist if we ever want to go back to them
    


