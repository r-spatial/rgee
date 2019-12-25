context("rgee: ee_manage test")

message <- try(ee_manage_delete('users/aybar1994/rgee/'))

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
  ee_manage_assets_access(path_asset = 'users/aybar1994/rgee/rgee_folder/')
  ee$data$getAssetAcl('users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
  ee_manage_task(cache = TRUE)
  ee_manage_task(cache = FALSE)
  ee_manage_cancel_all_running_taks()
  message <- ee_manage_delete('users/aybar1994/rgee/')
  expect_true(message)
})

test_that("manage_quota",{
  expect_type(ee_manage_quota(),"character")
})

test_that("ee_manage_size: f,i,fc,ic",{
  ic_size <- ee_manage_size('users/aybar1994/ic_test')
  fc_size <- ee_manage_size('users/aybar1994/peru')
  i_size <- ee_manage_size('users/aybar1994/L7_ETMs')
  total_size <- ic_size + fc_size + i_size
  expect_equal(total_size,3)
})

test_that("ee_manage_copy",{
  l7etm_init <- 'users/aybar1994/L7_ETMs'
  l7etm_last <- 'users/aybar1994/testing_01/L7_ETMs'
  copy_mn <- ee_manage_copy(path_asset = l7etm_init,
                            final_path =  l7etm_last,
                            quiet = FALSE)
  ee_manage_delete(l7etm_last)
  expect_equal(copy_mn,TRUE)
})

test_that("ee_manage_move folder",{
  folder_init <- 'users/aybar1994/ee_manage'
  folder_last <- 'users/aybar1994/ee_manage2'
  copy_mn <- ee_manage_move(path_asset = folder_init,
                            final_path =  folder_last,
                            quiet = FALSE)
  Sys.sleep(2)
  copy_mn <- ee_manage_move(path_asset = folder_last,
                            final_path =  folder_init,
                            quiet = FALSE)
  expect_equal(copy_mn,TRUE)
})

test_that("ee_manage_copy folder",{
  folder_init <- 'users/aybar1994/ee_manage'
  folder_last <- 'users/aybar1994/ee_manage2'
  Sys.sleep(2)
  copy_mn <- ee_manage_copy(path_asset = folder_init,
                            final_path =  folder_last,
                            quiet = FALSE)
  ee_manage_delete(folder_last)
  expect_equal(copy_mn,TRUE)
})

test_that("ee_verify_filename - error message",{
  expect_error(
    ee_verify_filename(path_asset = "user/aybar1994/xxx",
                       strict = TRUE)
    )
  }
)

test_that("ee_manage_create - error message", {
  expect_error(object =
                 ee_manage_create(
                   path_asset = 'users/aybar1994/to_c',
                   asset_type = 'nekko')
               )
  expect_equal(
    object = ee_manage_create(
      path_asset = 'users/aybar1994/ee_manage',
      asset_type = 'nekko'),
    expected =  TRUE)
  }
)
