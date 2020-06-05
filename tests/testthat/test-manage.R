context("rgee: ee_manage test")

# Pre-checking ------------------------------------------------------
# Google credentials were loaded in the system?
skip_if_no_credentials <- function(user) {
  ee_path <- path.expand(sprintf("~/.config/earthengine/%s", user))
  credentials <- list.files(
    path = ee_path,
    pattern = "@gmail.com|credentials|GCS_AUTH_FILE.json"
  )
  if (length(credentials) != 3) {
    skip("All google credentials were not found")
  }
}

# Neccesary Python packages were loaded?
skip_if_no_pypkg <- function() {
  have_ee <- reticulate::py_module_available("ee")
  have_numpy <- reticulate::py_module_available("numpy")
  if (isFALSE(have_ee)) {
    skip("ee not available for testing")
  }
  if (isFALSE(have_numpy)) {
    skip("numpy not available for testing")
  }
}

# Init Earth Engine just if it is necessary
init_rgee <- function() {
  tryCatch(
    expr = ee$Image()$getInfo(),
    error = function(e) {
      ee_Initialize(
        email = 'data.colec.fbf@gmail.com',
        drive = TRUE,
        gcs = TRUE
      )
    }
  )
}

user <- "data.colec.fbf"
skip_if_no_credentials(user)
skip_if_no_pypkg()
init_rgee()
# -------------------------------------------------------------------------


try(ee_manage_delete(path_asset = 'users/datacolecfbf/rgee/'))

test_that("ee_manage_create", {
  ee_manage_create(
    path_asset = 'users/datacolecfbf/rgee/rgee_folder',
    asset_type = 'Folder'
  )
  ee_manage_copy(
    path_asset = 'users/datacolecfbf/rgee/rgee_folder',
    final_path = 'users/datacolecfbf/rgee/rgee_folder1'
  )
  msg <- ee_manage_create(
    path_asset = 'users/datacolecfbf/rgee/rgee_ic',
    asset_type = 'ImageCollection'
  )
  expect_error(ee_manage_create(path_asset = 'users/pinkipie/rgee/rgee_ic'))
  expect_true(msg)
})


test_that("ee_manage_assetlist", {
  data_01 <- ee_manage_assetlist(path_asset = 'users/datacolecfbf/rgee')
  expect_s3_class(data_01,'data.frame')
})

test_that("ee_manage_move", {
  ee_manage_move(
    path_asset = 'users/datacolecfbf/rgee/rgee_ic',
    final_path = 'users/datacolecfbf/rgee/rgee_folder/rgee_ic_moved'
  )
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
  ee_manage_copy(
    path_asset = 'users/datacolecfbf/rgee_test',
    final_path = 'users/datacolecfbf/rgee/rgee_ic2/rgee_test'
  )
  ee_manage_move(
    path_asset = 'users/datacolecfbf/rgee/rgee_ic2',
    final_path = 'users/datacolecfbf/rgee/rgee_folder/rgee_ic_moved'
  )
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

test_that("ee_manage_quota", {
  expect_type(ee_manage_quota(), "character")
  }
)

test_that("ee_manage_asset_access", {
  ee_Initialize(
    email = "data.colec.fbf@gmail.com",
    drive = TRUE,
    gcs = TRUE
  )
  ee_manage_asset_access('users/datacolecfbf/rgee',
                       editor =  'data.colec.fbf@gmail.com')
  message <- ee_manage_asset_access('users/datacolecfbf/rgee',
                                    editor =  'data.colec.fbf@gmail.com',
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
  ic_name <- 'users/datacolecfbf/rgee/cs/cs/rgee_folder'
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
  lesly_ic_in <- "users/datacolecfbf/rgee/lesly_ic"
  lesly_ic_out <- "users/datacolecfbf/rgee/lesly/lesly_ic"
  ee_manage_create(lesly_ic_in, "ImageCollection")
  exp_01 <- ee_manage_copy(
    path_asset = lesly_ic_in,
    final_path = lesly_ic_out
  )
  expect_true(exp_01)
})

test_that("ee_manage_move - Folder", {
  lesly_f_in <- "users/datacolecfbf/rgee/lesly_folder/"
  lesly_f_out <- "users/datacolecfbf/rgee/lesly/lesly_folder/"
  ee_manage_create(lesly_f_in)
  exp_02 <- ee_manage_move(
    path_asset = lesly_f_in,
    final_path = lesly_f_out
  )
  expect_true(exp_02)
})
