# Score Board

See [https://gitlab.com/aakansh9/SPHERE-Challenge]

Keep on updating LB data !

| Author | Submission-name | Features | Model |   Local CV score | LB score |
| -------- | -------- | -------- | -------- | -------- | -------- |
| AKA  | xgboost_default.csv   | Default features | xgboost | 0.1912 + 0.0240 | 0.1760 |
| GUP | Submission_2016-07-28-17-43.csv   | Default features including mine `1555` | RF(RG-X, SW-X, Criterion-Entropy, BS-False, Est-100) | 0.18305 + 0.02767 | 0.1828 |
| AKA  | xgboost_july30_t1.csv   | top 370 AKA feat + GUP feat | xgboost | 0.1741 + 0.0333 | 0.1953 |
| AKA  | xgboost_july28_t1.csv   | all AKA feat | xgboost | 0.1854 + 0.0264 | 0.1658 |
| GUP | Submission_2016-07-29-21-33.csv   | all AKA feat | RF(RG-X, SW-X, Criterion-Entropy, BS-False, Est-100) | 0.1922 + 0.0242 | 0.1684 |
| GUP | Submission_2016-07-29-21-33.csv   | all AKA feat + GUP video feat | RF(RG-X, SW-X, Criterion-Entropy, BS-False, Est-100) | 0.1904 + 0.0238 | 0.1653 |

# Features List

Train data location =  ` /data/raw_data/public_data/train/000xx/...`

Test data location =  `/data/raw_data/public_data/test/00xxx/...`

Files Required = 

- *acceleration.csv* ( Column names  = t, x, y, z, Kitchen_AP, Lounge_AP, Upstairs_AP, Study_AP collected at 20 Hz)
- *pir.csv* ( Passive Infrared data)
- *video_hallway.csv* ( video data of hallway camera)
- *video_kitchen.csv* ( video data of kitchen camera)
- *video_living_room.csv* ( video data of living room camera)

## List of features

Each data point is a window of size **1 second** ( t - 1, t ) for which 20 activities have to be predicted.

#### Accelerometer Features

- [x] {+aka+} {-accel_feat1.csv-} Statistical features { mean, sd, **5**-quantiles } of **raw** x, y, z axis in **{1}-sec** windows `21`

- [ ] Statistical features { mean, sd, **3**-quantiles } of **preprocessed** x, y, z axis in **{1}-sec** windows
  - [x] {+aka+} {-accel_feat2.csv-} Preprocessing  = Moving median of width **5** `15`
  - [x] {+aka+} {-accel_feat3.csv-} Preprocessing  = Moving mean of width **5** `15`
  - [ ] {+aka+} Preprocessing  = velocity, jerk + moving mean/median (?)
  - [x] `*_f*_accelerator_feat.csv`Preprocessing  = Filter Bank ( [similar thing](https://github.com/alexandrebarachant/Grasp-and-lift-EEG-challenge/tree/master/preprocessing) ) Only Lowpass not Bandpass for frequencies with `sampling_frequency = 20` and `order = 5` - `[0.01668, 0.05995, 0.21544, 0.77426, 2.78256]` `5 x 9 x 7 = 315`
      - [x]  {+guppsy+} `a_x` = `x` low pass butterworth filters `7`
      - [x]  {+guppsy+} `a_y`= `y` low pass butterworth filters `7`
      - [x]  {+guppsy+} `a_z`= `z` low pass butterworth filters `7`
      - [x]  {+guppsy+} `a_xyz`= `x+y+z`low pass butterworth filters `7`
      - [x]  {+guppsy+} `a_abs_xyz` = `|x|+|y|+|z|` low pass butterworth filters `7`
      - [x]  {+guppsy+} `a_x2y2z2`= `sqrt(x^2+y^2+z^2)` low pass butterworth filters `7`
      - [x]  {+guppsy+} `a_x2y2` = `sqrt(x^2+y^2)` low pass butterworth filters `7`
      - [x]  {+guppsy+} `a_y2z2` = `sqrt(y^2+z^2)` low pass butterworth filters `7`
      - [x]  {+guppsy+} `a_x2z2`= `sqrt(x^2+z^2)` low pass butterworth filters `7`

- [ ] Statistical features { mean, sd, **5**-quantiles } of  **raw** x, y, z axis **interactions** in **{1}-sec** windows
  - [x] {+aka+}  {-accel_feat4.csv-} Interactions = x+y+z, |x| + |y| + |z| , sqrt( x^2 + y^2 + z^2 ), sqrt( x^2 + y^2 ), sqrt( y^2 + z^2 ), sqrt( x^2  + z^2 ) `42`
  - [ ] {+aka+} Interactions = Read from research papers ?

- [ ]  Statistical features { mean, sd, **3**-quantiles} of  **preprocessed** x, y, z axis **interactions** in **{1}-sec** windows
  - [x] {+aka+} {-accel_feat5.csv-} Preprocessing  = Moving median of width **5**, Interactions = x+y+z, |x| + |y| + |z| , sqrt( x^2 + y^2 + z^2 ), sqrt( x^2 + y^2 ), sqrt( y^2 + z^2 ), sqrt( x^2  + z^2 ) `30`
  - [x] {+aka+} {-accel_feat6.csv-} Preprocessing  = Moving mean of width **5**, Interactions = x+y+z, |x| + |y| + |z| , sqrt( x^2 + y^2 + z^2 ), sqrt( x^2 + y^2 ), sqrt( y^2 + z^2 ), sqrt( x^2  + z^2 ) `30`
  - [ ] {+guppsy+} Preprocessing  =  selected filters, Interactions = sqrt( x^2 + y^2 + z^2 )

#### AP/RSSI (Access Point signal strength Features)

Treat missing data as NA when calculating statistical features.

Attributes = { kitchen_AP, upstairs_AP, lounge_AP, study_AP }

- [x] {+aka+} {-AP_feat1.csv-} Statistical features { mean, sd, **5**-quantiles } of **raw** atrtibutes in **{1}**-sec windows `28`
- [ ] {+aka+} Statistical features { mean, sd, **5**-quantiles } of **raw** attributes in **{2,3}**-sec windows

- [x] Statistical features { mean, sd, **3**-quantiles} of **preprocessed**  attributes in **{1}**-sec windows
  - [x] {+aka+} {-AP_feat2.csv-} Preprocessing = moving **mean** of width **5** `20`
- [x] {+aka+} {-AP_feat3.csv-} Statistical features { mean, sd, max} of **binarized** (0-1) attributes in **{1}**-sec windows `12`



#### PIR Features

Data is very bad. Lots of missing/ FP/ FN data. First convert data into windows of 1 sec. with 9 binary attributes corresponding to each room.

- [x] {+aka+} {-PIR_feat1.csv-} signal presence (0-1) in each of 9 rooms in {1}-sec windows `10`
- [ ] Other features to predict which room person is in ?
- [ ] Other features to find if the person is on stairs ? (only some actions are performed on stairs)
- [ ] Other features to find if the person is in hall ?

#### Camera Features

Consider missing data as NA to find statistical features.

Points = center_2d, br, tl, boxcenter_2d, brb, flt, center_3d, boxcenter_3d

Attributes are coordinates of above points.
 
- [x] {+aka+} {-video_feat1.csv-} Statistical features { mean, sd, 5-quantiles } + binary (is_signal_present) features of **raw** attributes in 1-sec windows `423`
- [x] {+aka+} {-video_feat2.csv-} Statistical features { mean, sd, 3-quantiles } features of **diff ( )** of attributes in 1-sec windows (diff measures velocity components) `300`
- [x] {+aka+} {-video_feat3.csv-} Displacements and directions of all points in 1-sec windows `39`

- [x]  `camera_*_feat.csv` Statistical features of **preprocessed** attributes in {1, 2, 3}-sec windows - `85 x 3`
  - [x] {+guppsy+} `v*_bb_avail` = convert to binary 0 ( NA ), 1( not NA ) `1`
  - [x] {+guppsy+} `v*_bb_2d_area` = area of 2D box `7`
  - [x] {+guppsy+} `v*_bb_3d_vol` = volume of 3D box `7`
  - [x] {+guppsy+} `v*_bb_2d_c_avg_diff_x` = Difference in X-coordinate of Center of Mass? (`centre_2d_x`) and X-Coordinate of 2D Bounding Box's Center `7`
  - [x] {+guppsy+} `v*_bb_2d_c_avg_diff_y` = Difference in Y-coordinate of Center of Mass? (`centre_2d_y`) and Y-Coordinate of 2D Bounding Box's Center `7`
  - [x] {+guppsy+} `v*_bb_2d_c_avg_disp` = Displacement in Center of Mass? and Center of 2D Bounding Box - `sqrt(vh_bb_2d_c_avg_diff_x**2+vh_bb_2d_c_avg_diff_y**2)` `7`
  - [x] {+guppsy+} `v*_bb_2d_x_diff` =  Width of 2D Bounding Box `7`
  - [x] {+guppsy+} `v*_bb_2d_y_diff` =  Height of 2D Bounding Box `7`
  - [x] {+guppsy+} `v*_bb_2d_disp` =  Length of Diagonal of 2D Bounding Box (?) `7`
  - [x] {+guppsy+} `v*_bb_3d_x_diff` = Width of 3D box `7`
  - [x] {+guppsy+} `v*_bb_3d_y_diff` = Height of 3D box `7`
  - [x] {+guppsy+} `v*_bb_3d_z_diff` = Depth of 3D box `7`
  - [x] {+guppsy+} `v*_bb_3d_disp` = Diagonal Length of 3D box `7`


# Model Files to save

- code
- random seeds of CV model, preprocessing 
- hyperparameters
- CV score
- Training data predictions of CV (reproducible)
- Final model dump
- Random seeds of Final model, preprocessing
- Final test predictions

# Models

- xgboost
- RF
- extra trees
- NN / CNN / RNN / LSTM
- k-NN
- Logistic Regression / Some Linear Model
- CRF
- FM

# Variation for each model

- Sample weighing
- Thresholding
- Dividing into 3 groups of target
- Play with Objective/Evaluation function








