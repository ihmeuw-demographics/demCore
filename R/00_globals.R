# For references:
#' @importFrom Rdpack reprompt

# this is needed for non-standard evaluation in data.table (and some other
# packages). multiple links suggest using `utils::globalVariables` to remove
# notes when checking the package.
# https://www.r-bloggers.com/no-visible-binding-for-global-variable/
# https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
utils::globalVariables(c("age_int", "age_group_years_start", "age_group_years_end",
                         "test", "px", "qx", "lx", "dx", "mx", "ax", "ex", "Tx",
                         "nLx", "nSx", "head",
                         "age", "age_start", "age_end", "check",
                         "agg_age_start", "ax_full_years", "axdx_total",
                         "age_count", "abridged_age", "axdx_full_years",
                         "age_length", "reg_qx", "slope", "intercept",
                         "pred_px", "pred_px_abridged", "adjustment_factor",
                         "pred_px_adjusted", "pred_qx_adjusted",
                         "value", "life_table_parameter",
                         "id_cols_no_age", "sex", "mx_inf", "has_1m0",
                         "new_ax", "max_ax_diff",
                         "year_start", "year_end", "initial_ax",
                         "prop_female", "srb", "asfr"))
