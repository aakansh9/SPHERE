require(dplyr)
require(data.table)

# check pir files in test data
rows = lapply(11:882, function(i){
  pir <- read.csv(paste0('data/raw_data/public_data/test/',sprintf('%05d',i),'/pir.csv'))
  return(nrow(pir))
})

# pir data is almost inexistent for test data

# find test sequence lengths
sub = read.csv('data/raw_data/public_data/submission_uniform_baseline.csv') %>% data.table
sub[,by='record_id', max(end)]$V1 # 5-29 sec

# test has approx 11 people recordings of approx. 1500 sec each 
# divided on average into sequences of length 20 sec.


# hall, stairs can be predicted by transitions
# stairs = ascend, descend, 
# hall = walk, loadwalk, ...
# some pir sensors ex hall, study are not reliable




# load

accel <- read.csv('data/raw_data/public_data/train/00001/acceleration.csv')
target <- read.csv('data/raw_data/public_data/train/00001/targets.csv')
pir <- read.csv('data/raw_data/public_data/train/00001/pir.csv')
video_hallway <- read.csv('data/raw_data/public_data/train/00001/video_hallway.csv')
video_kitchen <- read.csv('data/raw_data/public_data/train/00001/video_kitchen.csv')
video_living_room <- read.csv('data/raw_data/public_data/train/00001/video_living_room.csv')
loc0 = read.csv('data/raw_data/public_data/train/00001/location_0.csv')

# PIR (passive infrared) sensors are located in all 0-8 (9 rooms).
# Sensors may have false negative and false positive acitvations.
# We produce best possible predictions of location in every 1 sec. window just based on PIR data.
# A person spends most of time in living, kitchen, bed2
# Minor times are spent on stairs, hall, study


# find location in 1s windows
rooms = lapply(target$end, function(e){
  pir[-which((pir$end <= e-1) | (pir$start > e)),'index'] %>% unique
})
