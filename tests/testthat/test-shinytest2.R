library(shinytest2)

test_that("{shinytest2} recording: gstudyEstimate", {
  app <- AppDriver$new(variant = platform_variant(), name = "gstudyEstimate", height = 760, 
      width = 899)
  rlang::warn(paste0("`file` should be the path to the file, relative to the app's tests/testthat directory.\n", 
      "Remove this warning when the file is in the correct location."))
  app$upload_file(file = "Brennan.3.2Wide.csv")
  app$click("transform")
  app$set_window_size(width = 1057, height = 781)
  app$click("runRecommModel")
  app$expect_screenshot()
  app$expect_values()
  app$click("runRecommModelBoot")
  app$expect_values()
  app$expect_screenshot()
})
