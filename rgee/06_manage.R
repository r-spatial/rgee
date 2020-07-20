library(rgee)
library(sf)

ee_Initialize(drive = TRUE)
ee_user_info()

# Obtain your asset home name
user <- ee_get_assethome()
addm <- function(x) sprintf("%s/%s",user, x)

# 1. Create a folder
# Change path asset according to your specific user
ee_manage_create(addm("rgee"))

# 2. List all the elements inside your asset home name
ee_manage_assetlist()

# 3. Create a folder
ee_manage_create(
  path_asset = addm("rgee/rgee_folder"),
  asset_type = "Folder"
)

# 4. Create an ImageCollection
ee_manage_create(
  path_asset = addm("rgee/rgee_ic"),
  asset_type = "ImageCollection"
)

# 5. List all the elements inside rgee folder
ee_manage_assetlist(path_asset = addm("rgee"))

# 6. Shows Earth Engine quota
ee_manage_quota()

# 7. Move an EE object to another folder
ee_manage_move(
  path_asset = addm("rgee/rgee_ic"),
  final_path = addm("rgee/rgee_folder/rgee_ic_moved")
)

# 8. List all the elements inside rgee/rgee_folder folder
ee_manage_assetlist(path_asset = addm("rgee/rgee_folder"))

# 9. Set properties to an EE object.
ee_manage_set_properties(
  path_asset = addm("rgee/rgee_folder/rgee_ic_moved"),
  add_properties = list(message = "hello-world", language = "R")
)

# Read the ImageCollection
test_ic <- ee$ImageCollection(addm("rgee/rgee_folder/rgee_ic_moved"))
test_ic$getInfo()

# 10. Delete properties
ee_manage_delete_properties(
  path_asset = addm("rgee/rgee_folder/rgee_ic_moved"),
  del_properties = c("message", "language")
)
test_ic$getInfo()

# 11. Create a report based on all the tasks
# that are running or have already been completed.
ee_manage_task()

# 12. Cancel all the running task
ee_manage_cancel_all_running_task()

# 13. Delete EE objects or folders
ee_manage_delete(addm("rgee/"))
