context("rgee: ee_manage test")
message <- try(ee_manage_delete(path_asset = 'users/aybar1994/rgee/'))

test_that("ee_manage_create", {
  ee_manage_create(path_asset = 'users/aybar1994/rgee')
  ee_manage_create(path_asset = 'users/aybar1994/rgee/rgee_folder',
                   asset_type = 'Folder')
  msg <- ee_manage_create(path_asset = 'users/aybar1994/rgee/rgee_ic',
                          asset_type = 'ImageCollection')
  expect_true(msg)
})

test_that("ee_manage_assetlist", {
  data <- ee_manage_assetlist(path_asset = 'users/aybar1994/rgee')
  expect_s3_class(data,'data.frame')
})

test_that("ee_manage_move", {
  ee_manage_move(path_asset = 'users/aybar1994/rgee/rgee_ic',
                 final_path = 'users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
  ee_manage_create(path_asset = 'users/aybar1994/rgee/rgee_ic',
                   asset_type = 'ImageCollection')
  ret <- ee_manage_delete('users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
  expect_true(ret)
})

test_that("ee_manage_copy", {
  ee_manage_create(path_asset = 'users/aybar1994/rgee/rgee_ic2',
                   asset_type = 'ImageCollection')
  ee_manage_copy(path_asset = 'users/aybar1994/L7',
                 final_path = 'users/aybar1994/rgee/rgee_ic2/L7')
  ret <- ee_manage_delete('users/aybar1994/rgee/rgee_ic2')
  expect_true(ret)
})

test_that("ee_manage_move - II", {
  ee_manage_create(path_asset = 'users/aybar1994/rgee/rgee_ic2',
                   asset_type = 'ImageCollection')
  ee_manage_copy(path_asset = 'users/aybar1994/L7',
                 final_path = 'users/aybar1994/rgee/rgee_ic2/L7')
  ee_manage_move(path_asset = 'users/aybar1994/rgee/rgee_ic2',
                 final_path = 'users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
  ret <- ee_manage_delete('users/aybar1994/rgee/rgee_folder/rgee_ic_moved')
  expect_true(ret)
})

test_that("ee_manage_set_properties", {
  ee_manage_copy(path_asset = 'users/aybar1994/L7',
                 final_path = 'users/aybar1994/rgee/L7')
  ee_manage_set_properties(path_asset = 'users/aybar1994/rgee/L7',
                           add_properties = list(message = 'hello-world',
                                                 language = 'R'))
  ret <- ee_manage_delete(path_asset = 'users/aybar1994/rgee/L7')
  expect_true(ret)
})


test_that("ee_manage_delete_properties", {
  ee_manage_copy(path_asset = 'users/aybar1994/L7',
                 final_path = 'users/aybar1994/rgee/L7')
  ee_manage_set_properties(path_asset = 'users/aybar1994/rgee/L7',
                           add_properties = list(message = 'hello-world',
                                                 language = 'R'))
  #ee$data$getAsset('users/aybar1994/rgee/L7')$properties
  ee_manage_delete_properties('users/aybar1994/rgee/L7')
  prop <- ee$data$getAsset('users/aybar1994/rgee/L7')$properties
  ee_manage_delete('users/aybar1994/rgee/L7')
  expect_null(prop)
})

test_that("ee_manage_task", {
  ee_manage_task(cache = TRUE)
  ee_manage_task(cache = FALSE)
  ret <- ee_manage_cancel_all_running_task()
  expect_true(ret)
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
