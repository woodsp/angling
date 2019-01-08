
###
### Code to intersect angling with lake polygons
###

## Inputs are angling events and polygons of lakes
## Outpus are table of angling events by lake

## Rough outline
# 1. Read angoing point events
# 2. Read NHD spatial data
# 3. Filter NHD to lakes
# 4. Buffer NHD lakes by 50m
# 5. Intersect angling points with buffered lake polys


