context("rgee: ee_manage test")
ee_Initialize()
message <- try(ee_manage_delete(path_asset = 'users/datacolecfbf/rgee/'))

test_that("ee_manage_create", {
  ee_manage_create(path_asset = 'users/datacolecfbf/rgee')
  ee_manage_create(path_asset = 'users/datacolecfbf/rgee/rgee_folder',
                   asset_type = 'Folder')
  msg <- ee_manage_create(path_asset = 'users/datacolecfbf/rgee/rgee_ic',
                          asset_type = 'ImageCollection')
  expect_true(msg)
})

test_that("ee_manage_assetlist", {
  data <- ee_manage_assetlist(path_asset = 'users/datacolecfbf/rgee')
  expect_s3_class(data,'data.frame')
})

test_that("ee_manage_move", {
  ee_manage_move(path_asset = 'users/datacolecfbf/rgee/rgee_ic',
                 final_path = 'users/datacolecfbf/rgee/rgee_folder/rgee_ic_moved')
  ee_manage_create(path_asset = 'users/datacolecfbf/rgee/rgee_ic',
                   asset_type = 'ImageCollection')
  ret <- ee_manage_delete('users/datacolecfbf/rgee/rgee_folder/rgee_ic_moved')
  expect_true(ret)
})

test_that("ee_manage_copy", {
  ee_manage_create(path_asset = 'users/datacolecfbf/rgee/rgee_ic2',
                   asset_type = 'ImageCollection')
  ee_manage_copy(path_asset = 'users/datacolecfbf/rgee_test',
                 final_path = 'users/datacolecfbf/rgee/rgee_ic2/rgee_test')
  ret <- ee_manage_delete('users/datacolecfbf/rgee/rgee_ic2')
  expect_true(ret)
})

test_that("ee_manage_move - II", {
  ee_manage_create(path_asset = 'users/datacolecfbf/rgee/rgee_ic2',
                   asset_type = 'ImageCollection')
  ee_manage_copy(path_asset = 'users/datacolecfbf/rgee_test',
                 final_path = 'users/datacolecfbf/rgee/rgee_ic2/rgee_test')
  ee_manage_move(path_asset = 'users/datacolecfbf/rgee/rgee_ic2',
                 final_path = 'users/datacolecfbf/rgee/rgee_folder/rgee_ic_moved')
  ret <- ee_manage_delete('users/datacolecfbf/rgee/rgee_folder/rgee_ic_moved')
  expect_true(ret)
})

test_that("ee_manage_set_properties", {
  ee_manage_copy(path_asset = 'users/datacolecfbf/rgee_test',
                 final_path = 'users/datacolecfbf/rgee/rgee_test')
  ee_manage_set_properties(path_asset = 'users/datacolecfbf/rgee/rgee_test',
                           add_properties = list(message = 'hello-world',
                                                 language = 'R'))
  ret <- ee_manage_delete(path_asset = 'users/datacolecfbf/rgee/rgee_test')
  expect_true(ret)
})


test_that("ee_manage_delete_properties", {
  ee_manage_copy(path_asset = 'users/datacolecfbf/rgee_test',
                 final_path = 'users/datacolecfbf/rgee/rgee_test')
  ee_manage_set_properties(path_asset = 'users/datacolecfbf/rgee/rgee_test',
                           add_properties = list(message = 'hello-world',
                                                 language = 'R'))
  #ee$data$getAsset('users/datacolecfbf/rgee/L7')$properties
  ee_manage_delete_properties('users/datacolecfbf/rgee/rgee_test')
  prop <- ee$data$getAsset('users/datacolecfbf/rgee/rgee_test')$properties
  ee_manage_delete('users/datacolecfbf/rgee/rgee_test')
  expect_null(prop)
})

test_that("ee_manage_task", {
  ee_manage_task(cache = FALSE)
  ee_manage_task(cache = TRUE)
  ret <- ee_manage_cancel_all_running_task()
  expect_true(ret)
})

test_that("ee_verify_filename - error message",{
  expect_error(
    ee_verify_filename(path_asset = "user/datacolecfbf/xxx",
                       strict = TRUE)
    )
  }
)

test_that("ee_manage_create - error message", {
  expect_error(object =
                 ee_manage_create(
                   path_asset = 'users/datacolecfbf/to_c',
                   asset_type = 'nekko')
               )
  expect_error(
    object = ee_manage_create(
      path_asset = 'users/datacolecfbf/ee_manage',
      asset_type = 'nekko'),
    expected =  TRUE)
  }
)
