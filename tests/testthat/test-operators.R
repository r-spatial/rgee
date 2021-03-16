context("rgee: Operators test")
skip_if_no_pypkg()
# -------------------------------------------------------------------------

test_that("Arithmetic Operator", {
  img <- ee$Image(1)

  # sum
  expect_equal((img + img), (img$add(img)))

  # subtract
  expect_equal((img - img), (img$subtract(img)))

  # Negative (-) 1 -> -1
  expect_equal(ee_extract(-img, ee$Geometry$Point(0, 0))$constant, -1)

  # multiply
  expect_equal(ee_extract(2 * img, ee$Geometry$Point(0, 0))$constant, 2)

  # pow
  expect_equal(ee_extract(2 ** img, ee$Geometry$Point(0, 0))$constant, 2)

  # Module %%
  expect_equal(ee_extract(img %% 3, ee$Geometry$Point(0, 0))$constant, 1)

  # Integer division %/%
  expect_equal(ee_extract(img %/% 2, ee$Geometry$Point(0, 0))$constant, 0)

  # Division /
  expect_equal(ee_extract(img / 2, ee$Geometry$Point(0, 0))$constant, 0.5)
})


test_that("Logic Operator", {
  img <- ee$Image(0)

  # Not !
  expect_equal(ee_extract(!img, ee$Geometry$Point(0, 0))$constant, 1)

  # And &
  expect_equal(ee_extract(img & TRUE, ee$Geometry$Point(0, 0))$constant, 0)

  # Or |
  expect_equal(ee_extract(1 | img, ee$Geometry$Point(0, 0))$constant, 1)

  # eq ==
  expect_equal(ee_extract(1 == img, ee$Geometry$Point(0, 0))$constant, 0)

  # neq !=
  expect_equal(ee_extract(1 != img, ee$Geometry$Point(0, 0))$constant, 1)

  # lt <
  expect_equal(ee_extract(10 < img, ee$Geometry$Point(0, 0))$constant, 0)

  # lte <=
  expect_equal(ee_extract(0 <= img, ee$Geometry$Point(0, 0))$constant, 1)


  # gt >
  expect_equal(ee_extract(10 > img, ee$Geometry$Point(0, 0))$constant, 1)

  # gte >=
  expect_equal(ee_extract(img >= 0 , ee$Geometry$Point(0, 0))$constant, 1)
})


test_that("Mathematical functions", {
  ee_geom <- ee$Geometry$Point(0, 0)

  # abs
  expect_equal(ee_extract(abs(ee$Image(-10)), ee_geom)$constant, 10)

  # sign
  expect_equal(ee_extract(sign(ee$Image(-10)), ee_geom)$constant, -1)

  # sqrt
  expect_equal(ee_extract(sqrt(ee$Image(10)), ee_geom)$constant, sqrt(10))

  # ceiling
  expect_equal(ee_extract(ceiling(ee$Image(10.4)), ee_geom)$constant, ceiling(10.4))

  # cumsum
  ee_series <- ee_extract(
    x = cumsum(ee$ImageCollection(lapply(1:10, function(x) ee$Image(x)))$toBands()),
    y =  ee_geom
  ) %>% as.numeric()
  expect_equal(ee_series, cumsum(1:10))

  # cumprod
  ee_series <- ee_extract(
    x = cumprod(ee$ImageCollection(lapply(1:10, function(x) ee$Image(x)))$toBands()),
    y =  ee_geom
  ) %>% as.numeric()
  expect_equal(ee_series, cumprod(1:10))

  # log
  expect_equal(ee_extract(log(ee$Image(10)), ee_geom)$constant, log(10))
  expect_equal(
    object = ee_extract(log(ee$Image(10), base = 5), ee_geom)$constant,
    expected = log(10, base = 5),
    tolerance = 1e-07
  )

  # log10
  expect_equal(ee_extract(log10(ee$Image(10)), ee_geom)$constant, log10(10))

  # log1p
  expect_equal(ee_extract(log1p(ee$Image(10)), ee_geom)$constant, log1p(10))

  # log2
  expect_equal(ee_extract(log2(ee$Image(10)), ee_geom)$constant, log2(10))

  # acos
  expect_equal(ee_extract(acos(ee$Image(0.1)), ee_geom)$constant, acos(0.1))

  # floor
  expect_equal(ee_extract(floor(ee$Image(0.1)), ee_geom)$constant, floor(0.1))

  # asin
  expect_equal(
    object = ee_extract(asin(ee$Image(0.1)), ee_geom)$constant,
    expected = asin(0.1),
    tolerance = 1e-07
  )

  # atan
  expect_equal(
    object = ee_extract(atan(ee$Image(0.1)), ee_geom)$constant,
    expected = atan(0.1),
    tolerance = 1e-07
  )

  # exp
  expect_equal(
    object = ee_extract(exp(ee$Image(0.1)), ee_geom)$constant,
    expected = exp(0.1),
    tolerance = 1e-07
  )

  # expm1
  expect_equal(
    object = ee_extract(expm1(ee$Image(0.1)), ee_geom)$constant,
    expected = expm1(0.1),
    tolerance = 1e-07
  )

  # cos
  expect_equal(
    object = ee_extract(cos(ee$Image(0.1)), ee_geom)$constant,
    expected = cos(0.1),
    tolerance = 1e-07
  )

  # cosh
  expect_equal(
    object = ee_extract(cosh(ee$Image(0.1)), ee_geom)$constant,
    expected = cosh(0.1),
    tolerance = 1e-07
  )

  # sin
  expect_equal(
    object = ee_extract(sin(ee$Image(0.1)), ee_geom)$constant,
    expected = sin(0.1),
    tolerance = 1e-07
  )

  # sinh
  expect_equal(
    object = ee_extract(sinh(ee$Image(0.1)), ee_geom)$constant,
    expected = sinh(0.1),
    tolerance = 1e-07
  )

  # tan
  expect_equal(
    object = ee_extract(tan(ee$Image(0.1)), ee_geom)$constant,
    expected = tan(0.1),
    tolerance = 1e-07
  )

  # tanh
  expect_equal(
    object = ee_extract(tanh(ee$Image(0.1)), ee_geom)$constant,
    expected = tanh(0.1),
    tolerance = 1e-07
  )
})


test_that("Summary functions", {
    ee_geom <- ee$Geometry$Point(0, 0)

    # mean Image
    mean_img <- mean(ee$Image(0), ee$Image(1), ee$Image(2), ee$Image(3))
    expect_equal(ee_extract(mean_img, ee_geom)$mean, 1.5)

    # max Image
    max_img <- max(ee$Image(0), ee$Image(1), ee$Image(2), ee$Image(3))
    expect_equal(ee_extract(max_img, ee_geom)$max, 3)

    # min Image
    min_img <- min(ee$Image(0), ee$Image(1), ee$Image(2), ee$Image(3))
    expect_equal(ee_extract(min_img, ee_geom)$min, 0)

    # range Image
    range_img <- range(ee$Image(0), ee$Image(1), ee$Image(2), ee$Image(3))
    expect_equal(mean(as.numeric(ee_extract(range_img, ee_geom))), 1.5)

    # sum Image
    sum_img <- sum(ee$Image(0), ee$Image(1), ee$Image(2), ee$Image(3))
    expect_equal(ee_extract(sum_img, ee_geom)$sum, 6)

    prod_img <- prod(ee$Image(0), ee$Image(1), ee$Image(2), ee$Image(3))
    expect_equal(ee_extract(prod_img, ee_geom)$product, 0)
  }
)

