context("rgee: ee_manage test")
skip_if_no_pypkg()
# -------------------------------------------------------------------------
try(ee_manage_delete(path_asset = sprintf("%s/rgee/",ee_get_assethome())))

test_that("ee_manage_create", {
  ee_manage_create(
    path_asset = sprintf("%s/rgee/rgee_folder",ee_get_assethome()),
    asset_type = 'Folder'
  )
  ee_manage_copy(
    path_asset = sprintf("%s/rgee/rgee_folder",ee_get_assethome()),
    final_path = sprintf("%s/rgee/rgee_folder1",ee_get_assethome())
  )
  msg <- ee_manage_create(
    path_asset = sprintf("%s/rgee/rgee_ic",ee_get_assethome()),
    asset_type = 'ImageCollection'
  )
  expect_error(ee_manage_create(path_asset = 'users/pinkipie/rgee/rgee_ic'))
  expect_true(msg)
})

test_that("ee_manage_assetlist", {
  data_01 <- ee_manage_assetlist(path_asset = sprintf("%s/rgee",ee_get_assethome()))
  expect_s3_class(data_01,'data.frame')
})

test_that("ee_manage_move", {
  ee_manage_move(
    path_asset = sprintf("%s/rgee/rgee_ic",ee_get_assethome()),
    final_path = sprintf("%s/rgee/rgee_folder/rgee_ic_moved",ee_get_assethome())
  )
  ee_manage_create(path_asset = sprintf("%s/rgee/rgee_ic",ee_get_assethome()),
                   asset_type = 'ImageCollection')
  ret <- ee_manage_delete(sprintf("%s/rgee/rgee_folder/rgee_ic_moved",ee_get_assethome()))
  expect_true(ret)
})

# test_that("ee_manage_copy", {
#   ee_manage_create(path_asset = sprintf("%s/rgee/rgee_ic2",ee_get_assethome()),
#                    asset_type = 'ImageCollection')
#   ee_manage_create(sprintf("%s/rgee_test",ee_get_assethome()),"ImageCollection")
#   ee_manage_copy(path_asset = sprintf("%s/rgee_test",ee_get_assethome()),
#                  final_path = sprintf("%s/rgee/rgee_ic2/rgee_test",ee_get_assethome()))
#   ret <- ee_manage_delete(sprintf("%s/rgee/rgee_ic2",ee_get_assethome()))
#   expect_true(ret)
# })

# test_that("ee_manage_move - II", {
#   ee_manage_create(path_asset = sprintf("%s/rgee/rgee_ic2",ee_get_assethome()),
#                    asset_type = 'ImageCollection')
#   ee_manage_copy(
#     path_asset = sprintf("%s/rgee_test",ee_get_assethome()),
#     final_path = sprintf("%s/rgee/rgee_ic2/rgee_test",ee_get_assethome())
#   )
#   ee_manage_move(
#     path_asset = sprintf("%s/rgee/rgee_ic2",ee_get_assethome()),
#     final_path = sprintf("%s/rgee/rgee_folder/rgee_ic_moved",ee_get_assethome())
#   )
#   ret <- ee_manage_delete(sprintf("%s/rgee/rgee_folder/rgee_ic_moved",ee_get_assethome()))
#   expect_true(ret)
# })

test_that("ee_manage_set_properties", {
  ee_manage_create(sprintf("%s/rgee_test",ee_get_assethome()),"ImageCollection")
  ee_manage_copy(path_asset = sprintf("%s/rgee_test",ee_get_assethome()),
                 final_path = sprintf("%s/rgee/rgee_test",ee_get_assethome()))
  ee_manage_set_properties(path_asset = sprintf("%s/rgee/rgee_test",ee_get_assethome()),
                           add_properties = list(message = 'hello-world',
                                                 language = 'R'))
  ret <- ee_manage_delete(path_asset = sprintf("%s/rgee/rgee_test",ee_get_assethome()))
  expect_true(ret)
})

test_that("ee_manage_delete_properties", {
  ee_manage_copy(path_asset = sprintf("%s/rgee_test",ee_get_assethome()),
                 final_path = sprintf("%s/rgee/rgee_test",ee_get_assethome()))
  ee_manage_set_properties(path_asset = sprintf("%s/rgee/rgee_test",ee_get_assethome()),
                           add_properties = list(message = 'hello-world',
                                                 language = 'R'))
  #ee$data$getAsset('users/datacolecfbf/rgee/L7')$properties
  ee_manage_delete_properties(sprintf("%s/rgee/rgee_test",ee_get_assethome()))
  prop <- ee$data$getAsset(sprintf("%s/rgee/rgee_test",ee_get_assethome()))$properties
  ee_manage_delete(sprintf("%s/rgee/rgee_test",ee_get_assethome()))
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
    ee_verify_filename(path_asset = sprintf("%s/xxx",ee_get_assethome()),
                       strict = TRUE)
    )
  }
)

test_that("ee_manage_create - error message", {
  expect_error(object =
                 ee_manage_create(
                   path_asset = sprintf("%s/to_c",ee_get_assethome()),
                   asset_type = 'nekko')
               )
  expect_error(
    object = ee_manage_create(
      path_asset = sprintf("%s/ee_manage",ee_get_assethome()),
      asset_type = 'nekko'),
    expected =  TRUE)
  }
)

test_that("ee_manage_quota", {
  expect_type(ee_manage_quota(), "character")
  }
)

test_that("ee_manage_asset_access", {
  ee_manage_asset_access(path_asset = sprintf("%s/rgee",ee_get_assethome()))
  message <- ee_manage_asset_access(sprintf("%s/rgee",ee_get_assethome()),
                                    editor =  'csaybar@gmail.com',
                                    all_users_can_read = TRUE)
  expect_true(message)
  }
)

test_that("ee_manage_asset_access", {
  mes_01 <- ee_humansize(120)
  mes_02 <- ee_humansize(1200)
  expect_type(mes_01, "character")
  expect_type(mes_02, "character")
})

test_that("ee_manage_asset_size", {
expect_type(
  ee_manage_asset_size(path_asset = 'MODIS/006/MOD09GA/2012_03_09'),
  "double"
)
})

test_that("ee_manage_create - extra", {
  ic_name <- sprintf("%s/rgee/cs/cs/rgee_folder",ee_get_assethome())
  ic <- ee_manage_create(
    path_asset = ic_name,
    asset_type = 'ImageCollection'
  )
  ic <- ee_manage_create(
    path_asset = ic_name,
    asset_type = 'ImageCollection'
  )
  ic <- ee$ImageCollection(ic_name)$getInfo()
  expect_type(ic, "list")
})


test_that("ee_manage_copy - ImageCollection", {
  lesly_ic_in <- sprintf("%s/rgee/lesly_ic",ee_get_assethome())
  lesly_ic_out <- sprintf("%s/rgee/lesly/lesly_ic",ee_get_assethome())
  ee_manage_create(lesly_ic_in, "ImageCollection")
  exp_01 <- ee_manage_copy(
    path_asset = lesly_ic_in,
    final_path = lesly_ic_out
  )
  expect_true(exp_01)
})

test_that("ee_manage_move - Folder", {
  lesly_f_in <- sprintf("%s/rgee/lesly_folder/",ee_get_assethome())
  lesly_f_out <- sprintf("%s/rgee/lesly/lesly_folder/",ee_get_assethome())
  ee_manage_create(lesly_f_in)
  exp_02 <- ee_manage_move(
    path_asset = lesly_f_in,
    final_path = lesly_f_out
  )
  expect_true(exp_02)
})
