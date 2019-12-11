context("rgee: ee_manage test")

test_that("simple_test",{
  ee_manage_create('users/aybar1994/rgee')
  ee_manage_assetlist(path_asset = 'users/aybar1994/rgee')
  ee_manage_create('users/aybar1994/rgee/rgee_folder',asset_type = 'folder')
  ee_manage_create('users/aybar1994/rgee/rgee_ic',asset_type = 'imagecollection')
  ee_manage_assetlist('users/aybar1994/rgee')
  ee_manage_size('users/aybar1994/rgee')
  ee_manage_move(path_asset = 'users/aybar1994/rgee/rgee_ic',
                 final_path = 'users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
  ee_manage_assetlist('users/aybar1994/rgee/rgee_folder')
  ee_manage_set_properties(path_asset = 'users/aybar1994/rgee/rgee_folder/rgee_ic_moved',
                           properties = list(message='hello-world',language = 'R'))
  test_ic <- ee$ImageCollection('users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
  ee_manage_delete_properties(path_asset = 'users/aybar1994/rgee/rgee_folder/rgee_ic_moved',
                              property = c("message","language"))
  ee_manage_assets_access('users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
  ee$data$getAssetAcl('users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
  ee_manage_task()
  ee_manage_cancel_all_running_taks()
  message <- ee_manage_delete('users/aybar1994/rgee/')
  expect_true(message)
})

