# this is needed for non-standard evaluation in data.table (and some other packages).
# multiple links suggest using `utils::globalVariables` to remove notes when checking the package.
# https://www.r-bloggers.com/no-visible-binding-for-global-variable/
# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c("age_int", "age_group_years_start", "age_group_years_end",
                         "test", "px", "qx", "lx", "dx", "mx", "ax", "ex", "Tx", "nLx",
                         "head", "age", "age_start", "age_end", "check",
                         "age_count", "abridged_age", "axdx_full_years"))
