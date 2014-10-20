context("is.constant")
test_that(
  desc="is.constant without NA works", 
  code = {    
    expect_identical(is.constant(rep(1, 5)), TRUE)
    expect_identical(is.constant(c(2,3)), FALSE)
  })
test_that(
  desc="is.constant with NA works", 
  code = {    
    expect_false(is.constant(c(1,1,NA)))
    expect_true(is.constant(c(1,1,NA), na.rm=TRUE))
    expect_false(is.constant(c(1,2,NA)))
    expect_false(is.constant(c(1,2,NA), na.rm=TRUE))
  })