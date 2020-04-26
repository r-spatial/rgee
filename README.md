# rgee-examples

A collection of **250+** examples for using Google Earth Engine with R. 

## 1. Description

This repository is a collection of **250+** R script examples. The majority of theses examples were adapted from adapting the repos [qgis-earthengine-examples](https://github.com/giswqs/qgis-earthengine-examples) and [earthengine-py-notebooks](https://github.com/giswqs/earthengine-py-notebooks) from Python to R. Recognition for organizing and develop these amazing examples should always be given to Professor [Qiusheng Wu](https://wetlands.io).

## 2. Examples

The Table of Contents below mimics the structure of the Google Earth Engine [API Documentation](https://developers.google.com/earth-engine). I strongly encourage you to check out the API Documentation if you need an in-depth explanation of each R example. Please note that the list below does not include all the R examples contained in this repository. You are welcome to explore the repository and find more examples to suit your needs.

### [Get Started](https://github.com/csaybar/rgee/tree/master/examples/GetStarted)

* [Hello world!](https://github.com/csaybar/rgee/blob/master/examples//GetStarted/01_hello_world.R)
* [Adding data to Map](https://github.com/csaybar/rgee/blob/master/examples//GetStarted/02_simple_mapdisplay.R)
* [Finding images](https://github.com/csaybar/rgee/blob/master/examples//GetStarted/03_finding_images.R)
* [Band math](https://github.com/csaybar/rgee/blob/master/examples//GetStarted/04_band_math.R)
* [Mapping (what to do instead of a for-loop)](https://github.com/csaybar/rgee/blob/master/examples//GetStarted/05_map_function.R)
* [Reducing](https://github.com/csaybar/rgee/blob/master/examples//GetStarted/06_reducing.R)
* [Image statistics](https://github.com/csaybar/rgee/blob/master/examples//GetStarted/07_image_statistics.R)
* [Masking](https://github.com/csaybar/rgee/blob/master/examples//GetStarted/08_masking.R)
* [A complete example](https://github.com/csaybar/rgee/blob/master/examples//GetStarted/09_a_complete_example.R)

### [Machine Learning](https://github.com/csaybar/rgee/tree/master/examples/MachineLearning)

* Supervised Classification Algorithms
  * [Classification and Regression Trees (CART)](https://github.com/csaybar/rgee/blob/master/examples//MachineLearning/cart_classifier.R) 
  * [Support Vector Machine (SVM)](https://github.com/csaybar/rgee/blob/master/examples//MachineLearning/svm_classifier.R) 
  * [Confusion Matrix](https://github.com/csaybar/rgee/blob/master/examples//MachineLearning/confusion_matrix.R)
* Unsupervised Classification Algorithms
  * [KMeans Clustering](https://github.com/csaybar/rgee/blob/master/examples//MachineLearning/clustering.R)

### [Image](https://github.com/csaybar/rgee/blob/master/examples/image)

* [Image Overview](https://github.com/csaybar/rgee/blob/master/examples/image/image_overview.R)
* [Image Visualization](https://github.com/csaybar/rgee/blob/master/examples/image/image_vis.R)
* [Image information and metadata](https://github.com/csaybar/rgee/blob/master/examples/image/image_metadata.R)
* [Mathematical operations](https://github.com/csaybar/rgee/blob/master/examples/image/band_math.R)
* [Relational, conditional and Boolean operations](https://github.com/csaybar/rgee/blob/master/examples/image/conditional_operations.R)
* [Convolutions](https://github.com/csaybar/rgee/blob/master/examples/image/convolutions.R)
* [Morphological Operations](https://github.com/csaybar/rgee/blob/master/examples/image/morphological_operations.R)
* [Gradients](https://github.com/csaybar/rgee/blob/master/examples/image/gradients.R)
* [Edge detection](https://github.com/csaybar/rgee/blob/master/examples/image/edge_detection.R)
* [Spectral transformations](https://github.com/csaybar/rgee/blob/master/examples/image/spectral_unmixing.R)
* [Texture](https://github.com/csaybar/rgee/blob/master/examples/image/texture.R)
* [Object-based methods](https://github.com/csaybar/rgee/blob/master/examples/image/object_based.R)
* [Cumulative Cost Mapping](https://github.com/csaybar/rgee/blob/master/examples/image/cumulative_cost_mapping.R)
* [Registering Images](https://github.com/csaybar/rgee/blob/master/examples/image/image_displacement.R)
* Miscellaneous
  * [Band statistics (min, max, mean, std)](https://github.com/csaybar/rgee/blob/master/examples/Image/band_stats.R)
  * [Image statistics by band](https://github.com/csaybar/rgee/blob/master/examples/image/image_stats_by_band.R)
  * [Extract value to points](https://github.com/csaybar/rgee/blob/master/examples/image/extract_value_to_points.R) 
  * [Rename bands](https://github.com/csaybar/rgee/blob/master/examples/image/rename_bands.R) 
  * [Clipping](https://github.com/csaybar/rgee/blob/master/examples/image/clipping.R) 
  * [Find image by path and row](https://github.com/csaybar/rgee/blob/master/examples/image/find_image_by_path_row.R)
  * [Get image resolution](https://github.com/csaybar/rgee/blob/master/examples/image/get_image_resolution.R) 
  * [Get image extent](https://github.com/csaybar/rgee/blob/master/examples/image/get_image_extent.R) 
  * [Set image properties](https://github.com/csaybar/rgee/blob/master/examples/image/set_image_properties.R) 
  * [Select bands](https://github.com/csaybar/rgee/blob/master/examples/image/select_bands.R) 
  * [Convert bands to ImageCollection](https://github.com/csaybar/rgee/blob/master/examples/image/convert_bands_to_image_collection.R) 
  * [Reclassify](https://github.com/csaybar/rgee/blob/master/examples/image/reclassify.R)   
  * [Composite bands](https://github.com/csaybar/rgee/blob/master/examples/image/composite_bands.R) 
  * [Image smoothing](https://github.com/csaybar/rgee/blob/master/examples/image/image_smoothing.R) 
  * [Download image](https://github.com/csaybar/rgee/blob/master/examples/image/download.R) 
  * [Cell statistics](https://github.com/csaybar/rgee/blob/master/examples/image/cell_statistics.R) 
  * [Image patch area](https://github.com/csaybar/rgee/blob/master/examples/image/image_patch_area.R) 
  * [Get image id](https://github.com/csaybar/rgee/blob/master/examples/image/get_image_id.R) 
  * [Get band name and type](https://github.com/csaybar/rgee/blob/master/examples/image/get_band_name_and_type.R) 
  * [Filtering by calendar range](https://github.com/csaybar/rgee/blob/master/examples/ImageCollection/filtering_by_calendar_range.R) 

### [ImageCollection](https://github.com/csaybar/rgee/tree/master/examples/ImageCollection)

* [ImageCollection Overview](https://github.com/csaybar/rgee/blob/master/examples//ImageCollection/overview.R)
* [ImageCollection Information and Metadata](https://github.com/csaybar/rgee/blob/master/examples//ImageCollection/metadata.R)
* [Filtering an ImageCollection](https://github.com/csaybar/rgee/blob/master/examples//ImageCollection/filtering_collection.R)
* [Mapping over an ImageCollection](https://github.com/csaybar/rgee/blob/master/examples//ImageCollection/map_function.R)
* [Reducing an ImageCollection](https://github.com/csaybar/rgee/blob/master/examples//ImageCollection/reducing_collection.R)
* [Compositing and Mosaicking](https://github.com/csaybar/rgee/blob/master/examples//ImageCollection/mosaicking.R)
* Miscellaneous
  * [Get image centroid](https://github.com/csaybar/rgee/blob/master/examples/ImageCollection/get_image_centroid.R) 
  * [Convert ImageCollection to Image](https://github.com/csaybar/rgee/blob/master/examples/ImageCollection/convert_imagecollection_to_image.R) 
  * [Sort by cloud and date](https://github.com/csaybar/rgee/blob/master/examples/ImageCollection/sort_by_cloud_and_date.R) 
  * [Filtering by metadata](https://github.com/csaybar/rgee/blob/master/examples/ImageCollection/filtering_by_metadata.R) 
  * [Filtering by band names](https://github.com/csaybar/rgee/blob/master/examples/ImageCollection/filtering_by_band_names.R) 
  * [Select image by index](https://github.com/csaybar/rgee/blob/master/examples/ImageCollection/select_image_by_index.R) 
  * [Creating monthly imagery](https://github.com/csaybar/rgee/blob/master/examples/ImageCollection/creating_monthly_imagery.R)

### [Geometry, Feature, FeatureCollection](https://github.com/csaybar/rgee/tree/master/examples/FeatureCollection)

* [Geometry Overview](https://github.com/csaybar/rgee/blob/master/examples//FeatureCollection/creating_feature.R)
* [Geodesic vs. Planar Geometries](https://github.com/csaybar/rgee/blob/master/examples//Visualization/visualizing_geometries.R)
* [Geometry Visualization and Information](https://github.com/csaybar/rgee/blob/master/examples//Visualization/visualizing_geometries.R)
* [Geometric Operations](https://github.com/csaybar/rgee/blob/master/examples//FeatureCollection/geometric_operations.R)
* [Feature Overview](https://github.com/csaybar/rgee/blob/master/examples//FeatureCollection/creating_feature.R)
* [FeatureCollection Overview](https://github.com/csaybar/rgee/blob/master/examples//FeatureCollection/from_polygons.R)
* [Feature and FeatureCollection Visualization](https://github.com/csaybar/rgee/blob/master/examples//Visualization/visualizing_feature_collection.R)
* [FeatureCollection Information and Metadata](https://github.com/csaybar/rgee/blob/master/examples//FeatureCollection/metadata_aggregation.R)
* [Filtering a FeatureCollection](https://github.com/csaybar/rgee/blob/master/examples//FeatureCollection/filtering_feature_collection.R)
* [Mapping over a FeatureCollection](https://github.com/csaybar/rgee/blob/master/examples//FeatureCollection/map_function.R)
* [Reducing a FeatureCollection](https://github.com/csaybar/rgee/blob/master/examples//FeatureCollection/reducing_feature_collection.R)
* [Vector to Raster Interpolation](https://github.com/csaybar/rgee/blob/master/examples//FeatureCollection/idw_interpolation.R)
* Miscellaneous
  * [Add new attribute](https://github.com/csaybar/rgee/blob/master/examples//FeatureCollection/add_new_attribute.R) 
  * [Add area column](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/add_area_column.R) 
  * [Add random value column](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/add_random_value_column.R) 
  * [Single column statistics](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/column_statistics.R) 
  * [Multiple column statistics](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/column_statistics_multiple.R) 
  * [Simplify polygons](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/simplify_polygons.R) 
  * [Column statistics by group](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/column_statistics_by_group.R) 
  * [Select by location](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/select_by_location.R) 
  * [Select by attributes](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/select_by_attributes.R) 
  * [Select by strings](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/select_by_strings.R) 
  * [Vector symbology](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/vector_symbology.R) 
  * [Merge FeatureCollection](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/merge_feature_collections.R) 
  * [Search by buffer distance](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/search_by_buffer_distance.R) 
  * [Select columns](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/select_columns.R) 
  * [Mimimum bounding geometry](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/minimum_bounding_geometry.R) 
  * [Clipping polygons](https://github.com/csaybar/rgee/blob/master/examples/FeatureCollection/clipping.R)

### [Reducer](https://github.com/csaybar/rgee/tree/master/examples/Reducer)

* [Reducer Overview](https://github.com/csaybar/rgee/blob/master/examples//Reducer/min_max_reducer.R)
* [ImageCollection Reductions](https://github.com/csaybar/rgee/blob/master/examples//Reducer/median_reducer.R)
* [Image Reductions](https://github.com/csaybar/rgee/blob/master/examples//Reducer/image_reductions.R)
* [Statistics of an Image Region](https://github.com/csaybar/rgee/blob/master/examples//Reducer/stats_of_an_image_region.R)
* [Statistics of Image Regions](https://github.com/csaybar/rgee/blob/master/examples//Reducer/stats_of_image_regions.R)
* [Statistics of Image Neighborhoods](https://github.com/csaybar/rgee/blob/master/examples//Reducer/stats_of_image_neighborhoods.R)
* [Statistics of FeatureCollection Columns](https://github.com/csaybar/rgee/blob/master/examples//Reducer/stats_of_columns.R)
* [Raster to Vector Conversion](https://github.com/csaybar/rgee/blob/master/examples//Reducer/convert_raster_to_vector.R)
* [Vector to Raster Conversion](https://github.com/csaybar/rgee/blob/master/examples//Reducer/convert_vector_to_raster.R)
* Grouped Reductions and Zonal Statistics
  * [Statistics by group](https://github.com/csaybar/rgee/blob/master/examples//Reducer/stats_by_group.R) 
  * [Zonal Statistics](https://github.com/csaybar/rgee/blob/master/examples//Reducer/zonal_statistics.R)
  * [Weighted Reductions](https://github.com/csaybar/rgee/blob/master/examples//Reducer/weighted_reductions.R)
  * [Linear Regression](https://github.com/csaybar/rgee/blob/master/examples//Reducer/linear_regression.R)

### [Join](https://github.com/csaybar/rgee/tree/master/examples/Join)

* [Simple Joins](https://github.com/csaybar/rgee/blob/master/examples//Join/simple_joins.R)
* [Inverted Joins](https://github.com/csaybar/rgee/blob/master/examples//Join/inverted_joins.R)
* [Inner Joins](https://github.com/csaybar/rgee/blob/master/examples//Join/inner_joins.R)
* [Save-All Joins](https://github.com/csaybar/rgee/blob/master/examples//Join/save_all_joins.R)
* [Save-Best Joins](https://github.com/csaybar/rgee/blob/master/examples//Join/save_best_joins.R)
* [Spatial Joins](https://github.com/csaybar/rgee/blob/master/examples//Join/spatial_joins.R)

### [Array](https://github.com/csaybar/rgee/tree/master/examples/Array)

* [Arrays and Array Images](https://github.com/csaybar/rgee/blob/master/examples//Array/array_images.R)
* [Array Transformations](https://github.com/csaybar/rgee/blob/master/examples//Array/array_transformations.R)
* [Eigen Analysis](https://github.com/csaybar/rgee/blob/master/examples//Array/eigen_analysis.R)
* [Array Sorting and Reducing](https://github.com/csaybar/rgee/blob/master/examples//Array/array_sorting.R)

### [Specialized Algorithms](https://github.com/csaybar/rgee/tree/master/examples/Algorithms)

* Landsat Algorithms
  * [Radiance](https://github.com/csaybar/rgee/blob/master/examples//Algorithms/landsat_radiance.R) 
  * [Surface Reflectance](https://github.com/csaybar/rgee/blob/master/examples//Algorithms/landsat_surface_reflectance.R) 
  * [Simple cloud score](https://github.com/csaybar/rgee/blob/master/examples//Algorithms/landsat_cloud_score.R) 
  * [Simple composite](https://github.com/csaybar/rgee/blob/master/examples//Algorithms/landsat_simple_composite.R)
* [Sentinel-1 Algorithms](https://github.com/csaybar/rgee/blob/master/examples//Algorithms/sentinel-1_filtering.R)
* Resampling and Reducing Resolution
  * [Resampling](https://github.com/csaybar/rgee/blob/master/examples//Algorithms/resampling.R) 
  * [Reducing Resolution](https://github.com/csaybar/rgee/blob/master/examples//Algorithms/reduce_resolution.R)
  * [Linear fit](https://github.com/csaybar/rgee/blob/master/examples//Algorithms/ntl_linear_fit.R)
* Pattern recognition
  * [Center-pivot Irrigation Detector](https://github.com/csaybar/rgee/blob/master/examples//Algorithms/center_pivot_irrigation_detector.R)

### [Asset Management](https://github.com/csaybar/rgee/tree/master/examples/AssetManagement)

* [Exporting Image](https://github.com/csaybar/rgee/blob/master/examples//AssetManagement/export_raster.R)
* [Exporting ImageCollection](https://github.com/csaybar/rgee/blob/master/examples//AssetManagement/export_ImageCollection.R)
* [Exporting Vector](https://github.com/csaybar/rgee/blob/master/examples//AssetManagement/export_vector.R)
* [Exporting FeatureCollection](https://github.com/csaybar/rgee/blob/master/examples//AssetManagement/export_FeatureCollection.R)
* [Exporting Table](https://github.com/csaybar/rgee/blob/master/examples//AssetManagement/export_table.R)
* [Exporting TimeSeries](https://github.com/csaybar/rgee/blob/master/examples//AssetManagement/export_TimeSeries.R)

### [How Earth Engine Works](https://github.com/csaybar/rgee/tree/master/examples/HowEarthEngineWorks)

* [Client vs. Server](https://github.com/csaybar/rgee/blob/master/examples//HowEarthEngineWorks/ClientVsServer.R)
* [Deferred Execution](https://github.com/csaybar/rgee/blob/master/examples//HowEarthEngineWorks/DeferredExecution.R)
* [Projections](https://github.com/csaybar/rgee/blob/master/examples//HowEarthEngineWorks/Projections.R)

### [Filter](https://github.com/csaybar/rgee/tree/master/examples/Filter)

* [Filter to metadata equal to the given value](https://github.com/csaybar/rgee/blob/master/examples/Filter/filter_eq.R)
* [Filter to metadata not equal to the given value](https://github.com/csaybar/rgee/blob/master/examples/Filter/filter_neq.R)
* [Filter on metadata contained in a list](https://github.com/csaybar/rgee/blob/master/examples/Filter/filter_in_list.R)
* [Filter on metadata that cotains a certain string](https://github.com/csaybar/rgee/blob/master/examples/Filter/filter_string_contains.R)
* [Filter on metadata that starts with a certain string](https://github.com/csaybar/rgee/blob/master/examples/Filter/filter_string_starts_with.R)
* [Filter on metadata that ends with a certain string](https://github.com/csaybar/rgee/blob/master/examples/Filter/filter_string_ends_with.R)
* [Filter on metadata that falls within a specified range](https://github.com/csaybar/rgee/blob/master/examples/Filter/filter_range_contains.R)

### [Visualization](https://github.com/csaybar/rgee/tree/master/examples/Visualization)

* [RGB composite](https://github.com/csaybar/rgee/blob/master/examples//Visualization/image_rgb_composite.R)
* [Color palettes](https://github.com/csaybar/rgee/blob/master/examples//Visualization/image_color_palettes.R)
* [Color ramp](https://github.com/csaybar/rgee/blob/master/examples//Visualization/image_color_ramp.R)
* [Hillshade](https://github.com/csaybar/rgee/blob/master/examples//Visualization/hillshade.R)
* [Image stretch](https://github.com/csaybar/rgee/blob/master/examples//Visualization/image_stretch.R)
* [Image thumbnail](https://github.com/csaybar/rgee/blob/master/examples//Visualization/image_thumbanil.R)
* [Rendering categorical maps](https://github.com/csaybar/rgee/blob/master/examples//Visualization/rendering_categorical_maps.R)
* [Styled layer descriptors](https://github.com/csaybar/rgee/blob/master/examples//Visualization/styled_layer_descriptors.R)
* [Terrain visualization](https://github.com/csaybar/rgee/blob/master/examples//Visualization/terrain_visualization.R)
* [Visualizing FeatureCollection](https://github.com/csaybar/rgee/blob/master/examples//Visualization/visualizing_feature_collection.R)
* [Visualizing Geometry](https://github.com/csaybar/rgee/blob/master/examples//Visualization/visualizing_geometries.R)
* [NLCD Land Cover](https://github.com/csaybar/rgee/blob/master/examples//Visualization/nlcd_land_cover.R)
* [US Counties](https://github.com/csaybar/rgee/blob/master/examples//Visualization/us_counties.R)
* Miscellaneous
  * [NDVI symbology](https://github.com/csaybar/rgee/blob/master/examples/Visualization/ndvi_symbology.R) 
  * [NDWI symbology](https://github.com/csaybar/rgee/blob/master/examples/Visualization/ndvi_symbology.R) 
  * [Landsat symbology](https://github.com/csaybar/rgee/blob/master/examples/Visualization/landsat_symbology.R) 
  * [NWI wetlands symbology](https://github.com/csaybar/rgee/blob/master/examples/Visualization/nwi_wetlands_symbology.R) 
  * [Color by attribute](https://github.com/csaybar/rgee/blob/master/examples/Visualization/color_by_attribute.R) 
  * [Random color visualizer](https://github.com/csaybar/rgee/blob/master/examples/Visualization/random_color_visualizer.R)

### [Datasets](https://github.com/csaybar/rgee/tree/master/examples/Datasets)

* [Terrain](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Terrain)
* [Water](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Water)
* [Vector datasets catalog](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors)
* [Large Scale International Boundary Polygons (LSIB)](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/international_boundary.R)
* [TIGER: US 2018 Census Counties](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/us_census_counties.R)
* [TIGER: US 2018 Census States](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/us_census_states.R)
* [TIGER: US 2016 Census Roads](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/us_census_roads.R)
* [TIGER: US 2010 Census Blocks](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/us_census_blocks.R)
* [TIGER: US Census 2010 Census Tracts + Demographic Profile 1 aggregate statistics](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/us_census_tracts.R)
* [TIGER: US Census 2010 5-digit ZIP Code Tabulation Areas](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/us_census_zip_code.R)
* [GLIMS: Global Land Ice Measurements from Space](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/global_land_ice_measurements.R)
* [USGS Watershed Boundary Datasets](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/usgs_watershed_boundary.R)
* [USA EPA Ecoregions](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/us_epa_ecoregions.R)
* [RESOLVE Ecoregions](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/resolve_ecoregions.R)
* [World Database on Protected Areas (WDPA)](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/world_database_on_protected_areas.R)
* [WRI Global Power Plant Database](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/global_power_plant_database.R)
* [Landsat WRS-2 grid](https://github.com/csaybar/rgee/blob/master/examples/Datasets/Vectors/landsat_wrs2_grid.R)

### [Tutorials](https://github.com/csaybar/rgee/tree/master/examples/Tutorials)

* [Global Surface Water](https://github.com/csaybar/rgee/blob/master/examples/Tutorials/GlobalSurfaceWater)
* [Keiko tips](https://github.com/csaybar/rgee/tree/master/examples/Tutorials/Keiko)

### [RGEE examples](https://github.com/csaybar/rgee/tree/master/examples/rgee)

##### COMING SOON!

---
