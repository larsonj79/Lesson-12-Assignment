library(testthat)

# each call to test_that() produces one test
# each test represents one point value
# you can have multiple tests for each question

library(readr)
library(dplyr)
library(ggplot2)
fexpprekey <- read_csv("fexppre.csv")
fexpdurkey <- read_csv("fexpdur.csv")
fexpratioskey <- read_csv("fexpratios.csv")
fexpcondratkey <- read_csv("fexpcondrat.csv")
fexppredictkey <- read_csv("fexppredict.csv")

grplot2key <- ggplot(fexpprekey, aes(x = GR, y = WebSales, size = POPN)) +
  geom_point(alpha = .5)

frplotkey <- ggplot(fexpdurkey, aes(x = FR, y = WebSales, size = POPN)) +
  geom_point(alpha = .5)

popnplotkey <- ggplot(fexpratioskey, aes(x = POPN, y = WebSales)) +
  geom_point() +
  scale_x_log10()

test_that("Q1 (visible)", {
  
  expect_true(dim(top10sales)[1] == 10)
  expect_equal(top10sales$DMA_NAME[1], "New York, NY")
  expect_equal(top10sales$GP[3], 6927.693, tolerance = 1)
  expect_equal(top10sales$FR[8], 5757.17, tolerance = 1)
  
})

test_that("Q2 (visible)", {
  
  expect_equal(as.numeric(mod1$coefficients[2]), 17.23678, tolerance = 1e-2)
  expect_equal(as.numeric(mod1$coefficients[4]), 3.578594, tolerance = 1e-2)
  expect_equal(as.numeric(mod1$coefficients[5]), 12.58874, tolerance = 1e-2)
  
})

test_that("Q5 plot (visible)", {
  
  expect_equal(grplot2$layers[[1]], grplot2key$layers[[1]])
  expect_equal(grplot2$scales, grplot2key$scales)
  expect_equal(grplot2$mapping, grplot2key$mapping)
  expect_equal(grplot2$labels, grplot2key$labels)
  
})

test_that("Q9 (visible)", {
  
  expect_equal(as.numeric(mod2$coefficients[2]), 6.929067, tolerance = 1e-2)
  expect_equal(as.numeric(mod2$coefficients[4]), 7.301376, tolerance = 1e-2)
  expect_equal(as.numeric(mod2$coefficients[5]), 12.31848, tolerance = 1e-2)
  
})

test_that("Q12 plot (visible)", {
  
  expect_equal(frplot$layers[[1]], frplotkey$layers[[1]])
  expect_equal(frplot$scales, frplotkey$scales)
  expect_equal(frplot$mapping, frplotkey$mapping)
  expect_equal(frplot$labels, frplotkey$labels)
  
})

test_that("Q14 (visible)", {
  
  expect_equal(as.numeric(mod3$coefficients[2]), .1524363, tolerance = 1e-3)
  expect_equal(as.numeric(mod3$coefficients[4]), .007598301, tolerance = 1e-3)
  expect_equal(as.numeric(mod3$coefficients[5]), -.003606119, tolerance = 1e-3)
  
})

test_that("Q16 plot (visible)", {
  
  expect_equal(popnplot$layers[[1]], popnplotkey$layers[[1]])
  expect_equal(popnplot$scales, popnplotkey$scales)
  expect_equal(popnplot$mapping, popnplotkey$mapping)
  expect_equal(popnplot$labels, popnplotkey$labels)
  
})

test_that("Q19 (visible)", {
  
  expect_equal(as.numeric(mod4$coefficients[2]), .1186392, tolerance = 1e-3)
  expect_equal(as.numeric(mod4$coefficients[4]), .071246, tolerance = 1e-3)
  expect_equal(as.numeric(mod4$coefficients[5]), .01427178, tolerance = 1e-3)
  
})

test_that("Q25 (visible)", {
  
  expect_true(dim(fexpcondrat)[2] == 8)
  expect_equal(fexpcondrat$AOV[1], .93686, tolerance = 1e-2)
  expect_equal(fexpcondrat$AOV[2], .8998185, tolerance = 1e-2)
  expect_equal(fexpcondrat$AOV[14], .8733692, tolerance = 1e-2)

})

test_that("Q26 (visible)", {
  
  expect_equal(as.numeric(mod5$coefficients[2]), .1631996, tolerance = 1e-3)
  expect_equal(as.numeric(mod5$coefficients[4]), .08963341, tolerance = 1e-3)
  expect_equal(as.numeric(mod5$coefficients[5]), .03287271, tolerance = 1e-3)
  
})

test_that("Q27 (visible)", {
  
  expect_equal(as.numeric(mod6$coefficients[2]), -.02501135, tolerance = 1e-3)
  expect_equal(as.numeric(mod6$coefficients[4]), -.007124323, tolerance = 1e-3)
  expect_equal(as.numeric(mod6$coefficients[5]), -.01271835, tolerance = 1e-3)
  
})

test_that("Q29 (visible)", {
  
  expect_equal(as.numeric(mod7$coefficients[2]), .11888, tolerance = 1e-3)
  expect_equal(as.numeric(mod7$coefficients[4]), .08008456, tolerance = 1e-3)
  expect_equal(as.numeric(mod7$coefficients[6]), .06627872, tolerance = 1e-3)
  
})

test_that("Q31 (visible)", {
  
  expect_equal(as.numeric(predsales[1]), 1.273185, tolerance = 1e-3)
  expect_equal(as.numeric(predsales[2]), 1.280661, tolerance = 1e-3)
  expect_equal(as.numeric(predsales[3]), 1.310217, tolerance = 1e-3)

})

test_that("Q32 Extra Credit 1 (visible)", {
  
  expect_equal(dim(admaxsales)[1], 1, tolerance = 1e-3)
  expect_equal(dim(admaxsales)[2], 4, tolerance = 1e-3)
  expect_equal(sum(admaxsales), 4.4, tolerance = 1e-3)
  
})

test_that("Q32 Extra Credit 2 (visible)", {
  
  expect_equal(admaxsales$GP[1], 1.4, tolerance = 1e-3)
  expect_equal(admaxsales$GR[1], 1, tolerance = 1e-3)

})

