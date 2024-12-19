import quackosm as qosm
from shapely.geometry import box
from shapely import get_coordinates
import os
import gc
import pandas as pd
    
pbf_path="../brazil-latest.osm.pbf"

states_df = pd.read_csv('../br_states.csv')

for _, row in states_df.iterrows():
    print(f"Processing {row['name_state']}: {row['abbrev_state']}")
    g= box(row['xmin'], row['ymin'], row['xmax'], row['ymax'])
    parquet_path=f"../{row['abbrev_state']}.parquet"
    if os.path.exists(parquet_path):
            print(f"Skipping - {row['abbrev_state']}.parquet already exists")
            continue
    gpq_path = qosm.convert_pbf_to_parquet(
        pbf_path,
        geometry_filter=g,
        tags_filter={"highway": True, "waterway": True},
        result_file_path=parquet_path
    )
    gc.collect()
