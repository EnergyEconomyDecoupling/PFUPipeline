#
# This file is sourced before any testing is done.
# The command for testing this repository should be
# testthat::test_dir("tests/testthat)

print(getwd())
init_path <- file.path("..", "..", "R", "init.R")
print(file.exists(init_path))
source(init_path)
