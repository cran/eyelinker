## Test eyelinker utility functions ##

# Test interval checking function
test_that("test_whichInterval", {

    # Create a set of test invervals
    start <- c(0, 1, 2)
    end <- c(.5, 1.3, 3)
    intv <- cbind(start, end)

    # Test interval identification
    expect_equal(whichInterval(2.5, intv), 3)  # check if 2.5 in 3rd interval
    expect_equal(whichInterval(0, intv), 1)  # check if 0 in 1st interval
    expect_equal(whichInterval(1.3, intv), 2)  # check if 1.3 in 2nd interval

    # Test for NA when number not within any interval
    expect_equal(is.na(whichInterval(1.6, intv)), TRUE)

    # Test vector input
    x <- c(0, 2.1, 1.2, 0.4)
    tst_intervals <- whichInterval(x, intv)
    expect_equal(tst_intervals, c(1, 3, 2, 1))

    # Test integer input
    expect_equal(whichInterval(0L, intv), 1)  # check if 0 in 1st interval

    # Test tidyverse-style alias
    expect_equal(which_interval(2.5, intv), 3)

})


# Test operator for checking if values fall in any of a set of intervals
test_that("test_In_operator", {

    # Create a set of test invervals
    start <- c(0, 1, 2)
    end <- c(.5, 1.3, 3)
    intv <- cbind(start, end)

    # Test wheter values fall within any intervals
    expect_equal(0.5 %In% intv, TRUE)
    expect_equal(0.6 %In% intv, FALSE)

    # Test vector input
    values <- c(5, 0, 0.8, 1.2, 1.8, 2.3)
    expected <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
    expect_equal(values %In% intv, expected)

    # Test integer input
    expect_equal(0L %In% intv, TRUE)

    # Test tidyverse-style alias
    expect_equal(0.5 %within% intv, TRUE)

})