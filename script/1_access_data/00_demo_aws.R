# before you begin, run renv::status() to check for dependencies

library(aws.s3)
library(tidyverse)

# set aws profile and region, you should have your credentials stored in ~/.aws/credentials
Sys.setenv(AWS_PROFILE = "pfas", AWS_DEFAULT_REGION = "us-east-1")

# quick check if authetication works
# try running aws.s3::bucketlist(), you should see a list of buckets

# read a csv file from s3
as_wqp <- s3read_using(
  FUN    = read.csv,
  object = "03_WQP_inorganics/processed_As_results_with_locations.csv",
  bucket = "national-pfas-model"
)
# slow, so try to save your intermediate data in rds format

# read a RDS file from s3
as_toy_data <- s3read_using(
  FUN    = readRDS,
  object = "01_sample_data/As_df_PredictorsSelected.rds",
  bucket = "national-pfas-model"
)
