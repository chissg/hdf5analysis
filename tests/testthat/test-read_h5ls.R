library(hdf5analysis)
context("read_h5ls")

EXPECTED_COLUMN_NAMES = c("table", "name", "nrec", "asize", "logical", "alloc", "fullname")

test_that("read_h5ls returns an empty tbl_df on a file with no tables",
          {
            tmp <- read_h5ls("empty_h5ls.output")
            expect_is(tmp, "tbl_df")
            expect_equal(nrow(tmp), 0)
            expect_setequal(names(tmp), EXPECTED_COLUMN_NAMES)
          })

test_that("read_h5ls returns a correct tbl_df on non-emply sample file", {
  tmp <- read_h5ls("sample_h5ls.output")
  expect_is(tmp, "tbl_df")
  expect_equal(nrow(tmp), 67)
  expect_equal(max(tmp$asize), 15)
  expect_equal(min(tmp$asize), 1)
  expect_equal(length(table(tmp$table)), 4) # 4 tables in the file
  expect_setequal(names(tmp), EXPECTED_COLUMN_NAMES)
})
