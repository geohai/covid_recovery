import json
from numpy import product
import safegraphql.client as sqgl
import os
from glob import glob
import time
from pyspark.sql import SparkSession
from pyspark.sql.functions import udf, explode
from pyspark.sql import functions as sf
from pyspark.sql.types import *


## Downloading data from folders
PATH = "/Users/esrieves/Documents/school/Research/foot_traffic/downloads/big5_cities"
EXT = "core_poi-patterns.csv.gz"

all_csv_files = [file
                for path, subdir, files in os.walk(PATH)
                for file in glob(os.path.join(path, EXT))]


## Functions
# Parse JSON
def parser(element):
    parsed = json.loads(element)

    if parsed is not None:
        return parsed
    else:
        return None


## Instantiate spark session
spark = SparkSession.builder.appName('SafeGraph').getOrCreate()


## Reading CSV with formartting
df = spark.read.option('header', 'True') \
    .option('inferSchema','True') \
    .option('escape', "\"") \
    .csv(all_csv_files)


## Clean data    
# Drop nan in df
df = df.dropna(subset=["visitor_home_cbgs", "poi_cbg", "placekey"])
df = df.where("visitor_home_cbgs!='{}'")

# Add leading zero to poi_cbg column (lost in csv format)
df = df.withColumn("dest_cbg", sf.format_string("%012d","poi_cbg"))

# Remove additional NAICS code digits > 2
df = df.withColumn("naics", df.naics_code.substr(1,2))


## Parse JSON
jsonudf = udf(parser, MapType(StringType(), IntegerType()))

visitor_home_cbgs_parsed = df.withColumn("parsed_visitor_home_cbgs", jsonudf("visitor_home_cbgs"))
visitor_home_cbgs_exploded = visitor_home_cbgs_parsed.select("placekey", "dest_cbg", "naics",
                                                             "date_range_start", "date_range_end", "city",
                                                             explode("parsed_visitor_home_cbgs"))



## Convert census block group to census tract (by removing last digit) - post explosion to access JSON nested values
# https://www.policymap.com/2012/08/tips-on-fips-a-quick-guide-to-geographic-place-codes-part-iii/
visitor_home_cbgs_exploded = visitor_home_cbgs_exploded.withColumn("dest_tract", visitor_home_cbgs_exploded.dest_cbg.substr(1,11)) \
    .withColumn("sender_tract", visitor_home_cbgs_exploded.key.substr(1,11)) \
    .drop("key") \
    .drop("sender_cbg")

#visitor_home_cbgs_exploded.show()

# create new spark df grouped by the sender CBG, summing the # of visitors for each cbg
sender_cbgs = visitor_home_cbgs_exploded.groupby(["sender_tract","dest_tract","date_range_end","naics"]).sum("value")

#sender_cbgs.show()
print(sender_cbgs.count)

start_time = time.time()
sender_cbgs.repartition(10).write.csv("/clean_data.csv")


print('Time of Exporting to CSV: ', time.time() - start_time)