# Build the Penta3 SAE presentation (PowerPoint) from outputs_covariates/
# Run after SAE_covariates_penta3.R.  Text is generated from the saved results.
suppressPackageStartupMessages({ library(officer); library(flextable); library(dplyr); library(tidyr); library(png); library(readr) })
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/SAE")
od <- "outputs_covariates"
R <- readRDS(file.path(od, "results.rds")); a1 <- R$a1; a2 <- R$a2; tabs <- R$tabs; coefs <- R$coefs
hy <- R$hypers; shr <- R$shrink; nos <- R$nos; nat <- R$national; assoc <- read_csv(file.path(od, "covariate_vs_direct.csv"), show_col_types = FALSE)
rk <- read_csv(file.path(od, "rankings_final.csv"), show_col_types = FALSE)
navy <- "#084887"; blue <- "#27aae1"; orange <- "#fe7501"; yellow <- "#efca08"; green <- "#497048"; teal <- "#006d77"; grey <- "#5a5a5a"
pc <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)
pp <- function(x, d = 1) sprintf(paste0("%.", d, "f"), 100 * x)
cov_labels <- c(education_yrs = "Mean years of education", anc4plus = "4+ ANC visits",
                access_difficulty = "Difficulty accessing a facility", facility_delivery = "Facility delivery")
nm2 <- function(x) sub("^.*_", "", x)

# ---------- derived numbers ---------------------------------------------------
rg <- function(d, col, nm) { o <- order(d[[col]]); list(lo = d[[nm]][o[1]], lo_v = d[[col]][o[1]], hi = d[[nm]][rev(o)[1]], hi_v = d[[col]][rev(o)[1]],
                                                       low5 = d[[nm]][o[1:5]], high5 = d[[nm]][rev(o)[1:5]]) }
g1 <- rg(a1, "FH1_mean", "admin1.name"); g1d <- rg(a1, "direct_mean", "admin1.name")
g2 <- rg(a2, "UN2_0_mean", "admin2.name.full"); g2c <- rg(a2, "UN2_mean", "admin2.name.full")
n_below80 <- sum(a2$UN2_0_mean < .80); n_ge95 <- sum(a2$UN2_0_mean >= .95)
n1_below85 <- sum(a1$FH1_mean < .85)
sdcmp <- function(fam, lvl, nested = FALSE) {
  t <- tabs |> filter(family == fam, level == lvl, nested == !!nested)
  list(none = t[grepl("No covariates", t$model), ], edu = t[t$model %in% c("education_yrs", "Nested: education_yrs"), ],
       all = t[t$n_cov == 4, ], best = t[which.min(t$waic), ])
}
c_fh1 <- sdcmp("Fay-Herriot", "Admin 1"); c_un1 <- sdcmp("Unit-level", "Admin 1")
c_fh2 <- sdcmp("Fay-Herriot", "Admin 2"); c_un2 <- sdcmp("Unit-level", "Admin 2"); c_un2n <- sdcmp("Unit-level", "Admin 2", TRUE)
edu <- function(fam, lvl, model) coefs |> filter(family == fam, level == lvl, term == "education_yrs", model == !!model)
e_fh1 <- edu("Fay-Herriot", "Admin 1", "education_yrs"); e_un1 <- edu("Unit-level", "Admin 1", "education_yrs")
e_fh2 <- edu("Fay-Herriot", "Admin 2", "education_yrs"); e_un2 <- edu("Unit-level", "Admin 2", "education_yrs")
spvar <- function(fam, lvl, spec_all, nested = FALSE) {
  h <- hy |> filter(family == fam, level == lvl, grepl("Precision for", hyper), !grepl("overdisp", hyper))
  h <- if (nested) h[grepl("Nested", h$model), ] else h[!grepl("Nested", h$model), ]
  list(none = 1 / h$mean[!grepl("education", h$model)][1], all = 1 / h$mean[grepl("education", h$model)][1])
}
v_fh1 <- spvar("Fay-Herriot", "Admin 1"); v_un2 <- spvar("Unit-level", "Admin 2")
sig_cov <- function(fam, lvl) coefs |> filter(family == fam, level == lvl, n_cov == 1, excludes_zero) |> pull(term) |> unique()

# ---------- slide helpers -----------------------------------------------------
x <- read_pptx()
d <- x$presentation$get(); n <- xml2::xml_find_first(d, "//p:sldSz", xml2::xml_ns(d))
xml2::xml_set_attr(n, "cx", "12192000"); xml2::xml_set_attr(n, "cy", "6858000"); xml2::xml_set_attr(n, "type", NULL)
W <- 13.333; H <- 7.5
tx <- function(x, text, l, t, w, h, size = 16, bold = FALSE, col = grey, align = "left", font = "Calibri", italic = FALSE) {
  fp <- fp_text(font.size = size, bold = bold, color = col, font.family = font, italic = italic)
  ph_with(x, block_list(fpar(ftext(text, fp), fp_p = fp_par(text.align = align))), ph_location(left = l, top = t, width = w, height = h))
}
bullets <- function(x, items, l, t, w, h, size = 17, col = grey, gap = 6) {
  bl <- lapply(items, function(it) {
    if (is.list(it)) {   # list(bold, rest)
      fpar(ftext("■  ", fp_text(font.size = size * .6, color = orange)), ftext(it[[1]], fp_text(font.size = size, bold = TRUE, color = navy, font.family = "Calibri")),
           ftext(it[[2]], fp_text(font.size = size, color = col, font.family = "Calibri")), fp_p = fp_par(padding.bottom = gap, line_spacing = 1.05))
    } else fpar(ftext("■  ", fp_text(font.size = size * .6, color = orange)), ftext(it, fp_text(font.size = size, color = col, font.family = "Calibri")),
                fp_p = fp_par(padding.bottom = gap, line_spacing = 1.05))
  })
  ph_with(x, do.call(block_list, bl), ph_location(left = l, top = t, width = w, height = h))
}
box <- function(x, l, t, w, h, fill) {
  f <- file.path(tempdir(), paste0("box_", sub("#", "", fill), ".png"))
  rgbv <- as.numeric(col2rgb(fill)) / 255; arr <- array(0, c(4, 4, 3)); for (k in 1:3) arr[, , k] <- rgbv[k]
  writePNG(arr, f)
  ph_with(x, external_img(f, width = w, height = h), ph_location(left = l, top = t, width = w, height = h))
}
img <- function(x, f, l, t, w, h) {   # fit inside box, keep aspect, centre
  dm <- dim(readPNG(file.path(od, f))); ar <- dm[2] / dm[1]
  ww <- w; hh <- w / ar; if (hh > h) { hh <- h; ww <- h * ar }
  ph_with(x, external_img(file.path(od, f), width = ww, height = hh), ph_location(left = l + (w - ww) / 2, top = t + (h - hh) / 2, width = ww, height = hh))
}
slide_no <- 0
new_slide <- function(x, title, sub = NULL) {
  slide_no <<- slide_no + 1
  x <- add_slide(x, "Blank", "Office Theme")
  x <- box(x, 0, 0, W, .12, navy)
  x <- tx(x, title, .55, .3, W - 1.1, .7, size = 24, bold = TRUE, col = navy)
  if (!is.null(sub)) x <- tx(x, sub, .55, 1.12, W - 1.1, .4, size = 14, col = grey, italic = TRUE)
  x <- box(x, .55, 1.08, 1.3, .045, orange)
  x <- tx(x, "Penta3 coverage, Kenya DHS 2022  |  CEMA SAE short course", .55, 7.08, 8, .3, size = 10, col = "#999999")
  tx(x, as.character(slide_no + 1), W - 1.1, 7.08, .6, .3, size = 10, col = "#999999", align = "right")
}
ft_style <- function(df, widths = NULL, size = 12, hl_rows = NULL) {
  ft <- flextable(df) |> theme_booktabs() |> fontsize(size = size, part = "all") |> font(fontname = "Calibri", part = "all") |>
    bg(bg = navy, part = "header") |> color(color = "white", part = "header") |> bold(part = "header") |>
    color(color = grey, part = "body") |> align(align = "center", part = "all") |> align(j = 1, align = "left", part = "all") |> border_remove() |>
    hline(border = fp_border(color = "#DDDDDD", width = .75), part = "body") |> padding(padding.top = 3, padding.bottom = 3, part = "all")
  if (!is.null(widths)) ft <- width(ft, width = widths)
  if (!is.null(hl_rows)) ft <- bg(ft, i = hl_rows, bg = "#EAF4FB", part = "body")
  ft
}
add_ft <- function(x, ft, l, t) ph_with(x, ft, ph_location(left = l, top = t))
callout <- function(x, text, l, t, w, h, fill = "#EAF4FB", size = 15) {
  x <- box(x, l, t, w, h, fill)
  x <- box(x, l, t, .07, h, orange)
  tx(x, text, l + .2, t + .05, w - .3, h - .1, size = size, col = navy)
}

# =============================================================================
# 1. Title
x <- add_slide(x, "Blank", "Office Theme")
x <- box(x, 0, 0, W, H, navy)
x <- box(x, .8, 3.55, 2.2, .07, orange)
x <- tx(x, "Do area-level covariates sharpen small-area estimates of Penta3 coverage?", .8, 1.5, 11.5, 1.9, size = 38, bold = TRUE, col = "white")
x <- tx(x, "Direct, Fay-Herriot and unit-level estimates for Kenyan counties and sub-counties, DHS 2022", .8, 3.8, 11.5, .9, size = 20, col = "#CFE3F3")
x <- tx(x, "CEMA SAE short course  |  Hands-on project  |  SUMMER / surveyPrev", .8, 6.2, 11, .4, size = 14, col = "#9CC3E4")

# 2. Context
x <- new_slide(x, "Context: 89% receive Penta3 nationally, but local gaps are hidden")
x <- tx(x, "89.2%", .7, 1.55, 3.6, 1.2, size = 66, bold = TRUE, col = navy)
x <- tx(x, "of children aged 12–23 months had received DPT3 / Penta3 (DHS final report 89.2%; DHS app 89.18%)", .75, 2.75, 3.6, 1.2, size = 14)
kp <- data.frame(Item = c("Clusters", "Children 12–23 months", "Vaccinated (events)", "Counties (admin 1)", "Sub-counties (admin 2)", "Areas with no sample"),
                 Value = c("1,448", "3,679", "3,148", "47", "300", "0 counties; 8 sub-counties (2.67%)"))
x <- add_ft(x, ft_style(kp, c(2.7, 2.9), 13), 4.8, 1.6)
x <- bullets(x, list(list("Question set. ", "How large is the geographic variation, why do areas differ, and do covariates add information beyond the survey?"),
                     list("Approach. ", "Three estimators at two levels, each fitted without and with four area-level covariates, all in SUMMER / surveyPrev.")),
             .7, 4.65, 12, 2.2, 16)

# 3. Methods
x <- new_slide(x, "Method: three estimators, two admin levels, with and without covariates")
mt <- data.frame(Estimator = c("Direct", "Fay-Herriot (area-level)", "Unit-level (cluster)"),
                 `SUMMER function` = c("directEST()", "fhModel()", "clusterModel()"),
                 `What it uses` = c("Design-weighted survey estimate per area; no borrowing", "Direct logit estimates with known variance + BYM2 spatial smoothing",
                                    "Cluster counts, beta-binomial likelihood + BYM2; nested variant adds a county effect"),
                 Covariates = c("Not applicable", "Fixed effects on the logit scale", "Fixed effects on the logit scale"), check.names = FALSE)
x <- add_ft(x, ft_style(mt, c(2.6, 2.1, 5.2, 2.5), 13), .7, 1.6)
x <- bullets(x, list(list("Covariates. ", "Mean years of education, 4+ ANC visits, difficulty accessing a facility and facility delivery, supplied per county and sub-county and standardised (effects are per 1 SD)."),
                     list("Direct estimates are unchanged by covariates; ", "only the smoothed models can use them."),
                     list("Validation. ", "Refitting the intercept-only models reproduced the saved DHS-app results (direct: identical; Fay-Herriot and unit-level: correlation 1.000)."),
                     list("Model choice. ", "Covariate sets compared by WAIC, covariate effects with 95% credible intervals, precision (posterior SD, CV) and the spatial variance explained.")),
             .7, 4.1, 12, 2.9, 16)

# 4. County variation
x <- new_slide(x, sprintf("County level: coverage ranges from %s to %s", pc(min(a1$FH1_0_mean), 0), pc(max(a1$FH1_0_mean), 0)),
               "Direct, Fay-Herriot, and Fay-Herriot with covariates (same colour scale)")
x <- img(x, "fig_admin1_maps.png", .5, 1.55, 8.6, 5.4)
x <- bullets(x, list(list("Lowest: ", paste(g1$low5[1:4], collapse = ", "), "."), list("Highest: ", paste(g1$high5[1:4], collapse = ", "), "."),
                     list("", sprintf("%d of 47 counties have a smoothed estimate below 85%%.", n1_below85)),
                     list("Pattern: ", paste0("low coverage is concentrated in the north-east and east; the highest estimates are in ", paste(g1$high5[1:4], collapse = ", "), "."))),
             9.3, 1.7, 3.6, 5, 15)
# fix list(…) with empty first elements handled in bullets()

# 5. Sub-county variation
x <- new_slide(x, sprintf("Sub-county level: the range widens to %s–%s", pc(min(a2$UN2_0_mean), 0), pc(max(a2$UN2_0_mean), 0)),
               "Direct (design-based), unit-level, unit-level with covariates, and nested with covariates")
x <- img(x, "fig_admin2_maps.png", .3, 1.55, 12.7, 3.9)
x <- bullets(x, list(list("", sprintf("%d sub-counties fall below 80%% and %d are at or above 95%% (unit-level, no covariates).", n_below80, n_ge95)),
                     list("Lowest: ", paste(sub("_", " / ", g2$low5[1:4]), collapse = "; ")),
                     list("Direct map has gaps: ", sprintf("%d sub-counties are unsampled and %d more have 100%% or zero-variance direct estimates.", sum(a2$n_children == 0), sum(!a2$direct_usable) - sum(a2$n_children == 0)))),
             .7, 5.55, 12, 1.5, 14, gap = 3)

# 6. Reasons: association scatter (admin 1)
a1c <- assoc |> filter(level == "Admin 1")
x <- new_slide(x, "Why counties differ: education is the covariate most aligned with coverage",
               "County direct estimates against each covariate (z-scores)")
x <- img(x, "fig_admin1_cov_scatter.png", .4, 1.55, 12.5, 3.3)
ra <- a1c |> arrange(desc(abs(r)))
x <- bullets(x, list(list("Strongest: ", sprintf("%s (r = %.2f, R² = %.2f). Weakest: %s (r = %.2f).", cov_labels[ra$covariate[1]], ra$r[1], ra$R2[1], cov_labels[ra$covariate[4]], ra$r[4])),
                     list("Plausible mechanism: ", "caregiver education and facility use raise the chance a child completes the three-dose schedule; poor facility access lowers it."),
                     list("Caution: ", "county covariates are area averages and the association is ecological; it does not show that education causes higher coverage.")),
             .7, 5.0, 12, 2, 15)

# 7. Sub-county scatter + correlation
x <- new_slide(x, "Sub-counties: same direction, much weaker association than counties", "Unit-level estimates against each covariate; correlations among covariates")
x <- img(x, "fig_admin2_cov_scatter.png", .4, 1.5, 12.5, 2.7)
x <- img(x, "fig_cov_correlation.png", .4, 4.25, 7.2, 2.8)
cm <- R$cov_cor$`Admin 2`; cm_off <- cm[upper.tri(cm)]
a2c <- assoc |> filter(level == "Admin 2")
x <- bullets(x, list(list("Weaker at sub-county level: ", sprintf("education r = %.2f (%d sub-counties with a usable direct estimate) versus r = %.2f for counties.", a2c$r[a2c$covariate == "education_yrs"], sum(a2$direct_usable & a2$direct_mean > 0 & a2$direct_mean < 1), a1c$r[a1c$covariate == "education_yrs"])),
                     list("Overlap: ", sprintf("covariate pairs correlate up to r = %.2f at county level (education and facility delivery), so effects are partly shared when entered together.", max(abs(R$cov_cor$`Admin 1`[upper.tri(R$cov_cor$`Admin 1`)])))),
                     list("Missing values: ", sprintf("%d sub-counties lacked covariate values (%s); county means of the remaining sub-counties were used.", length(R$miss_cov), paste(sub("_", " / ", R$miss_cov), collapse = "; ")))),
             7.8, 4.4, 5.2, 2.6, 14)

# 8. Coefficients
x <- new_slide(x, "Only education has a clear effect; it fades when all four enter together",
               "Posterior mean and 95% credible interval, effect on logit coverage per 1 SD")
x <- img(x, "fig_coefficients.png", .4, 1.5, 8.3, 5.5)
x <- bullets(x, list(list("Education alone: ", sprintf("+%.2f logits per SD at county level (Fay-Herriot, 95%% CrI %.2f to %.2f) and +%.2f at sub-county level (unit-level, %.2f to %.2f).",
                                                       e_fh1$mean, e_fh1$lower, e_fh1$upper, e_un2$mean, e_un2$lower, e_un2$upper)),
                     list("All four together: ", "education stays positive in the unit-level models but intervals widen; ANC, access and facility delivery are consistently weak."),
                     list("Direction is as expected: ", "access difficulty is negative, the others positive.")),
             8.9, 1.7, 4.1, 5.2, 14)

# 9. Did covariates help?
mk <- function(lbl, r, v = NA) data.frame(Model = lbl, `ΔWAIC` = sprintf("%+.1f", r$d_waic), `Mean SD (pp)` = pp(r$mean_sd, 2), `Mean CV` = sprintf("%.2f", r$mean_cv), check.names = FALSE)
tt <- bind_rows(
  mk("Fay-Herriot, county: none", c_fh1$none), mk("  + education", c_fh1$edu), mk("  + all four", c_fh1$all),
  mk("Unit-level, county: none", c_un1$none), mk("  + education", c_un1$edu), mk("  + all four", c_un1$all),
  mk("Fay-Herriot, sub-county: none", c_fh2$none), mk("  + all four", c_fh2$all),
  mk("Unit-level, sub-county: none", c_un2$none), mk("  + education", c_un2$edu), mk("  + all four", c_un2$all))
x <- new_slide(x, "Covariates add little: slightly tighter estimates, no WAIC gain",
               "Lower WAIC is better; ΔWAIC is relative to the model without covariates")
x <- add_ft(x, ft_style(tt, c(3.4, 1.2, 1.5, 1.2), 12, hl_rows = c(2, 5, 10)), .5, 1.55)
x <- bullets(x, list(list("Precision: ", sprintf("education trims the mean county SD from %s to %s pp (Fay-Herriot) and the sub-county SD from %s to %s pp (unit-level).",
                                                  pp(c_fh1$none$mean_sd, 2), pp(c_fh1$edu$mean_sd, 2), pp(c_un2$none$mean_sd, 2), pp(c_un2$edu$mean_sd, 2))),
                     list("Information: ", sprintf("the county spatial variance falls from %.2f to %.2f once all four covariates are included, so they explain part of the between-county differences.", v_fh1$none, v_fh1$all)),
                     list("Model fit: ", sprintf("county WAIC changes are small (%+.1f for education, %+.1f for all four, Fay-Herriot) and sub-county WAIC is worse (%+.1f and %+.1f, unit-level), so the evidence for adding covariates is weak.", c_fh1$edu$d_waic, c_fh1$all$d_waic, c_un2$edu$d_waic, c_un2$all$d_waic)),
                     list("Recommendation: ", "keep education as a single covariate where a modest precision gain is wanted; do not add all four.")),
             8.2, 1.6, 4.8, 5.4, 13, gap = 4)

# 10. Precision
x <- new_slide(x, "Precision: model-based beats direct; covariates add a small gain")
x <- img(x, "fig_precision.png", .4, 1.4, 12.5, 4.6)
x <- bullets(x, list(list("", sprintf("Median county SD: direct %s pp, Fay-Herriot %s pp, with covariates %s pp.", pp(median(a1$direct_sd), 2), pp(median(a1$FH1_0_sd), 2), pp(median(a1$FH1_sd), 2))),
                     list("", sprintf("Median sub-county SD: direct %s pp (usable areas only), unit-level %s pp, with covariates %s pp.", pp(median(a2$direct_sd[a2$direct_usable]), 2), pp(median(a2$UN2_0_sd), 2), pp(median(a2$UN2_sd), 2)))),
             .7, 6.05, 12, 1.0, 14, gap = 2)

# 11. Missing data
x <- new_slide(x, "Unsampled sub-counties: plausible estimates that rest on neighbours",
               sprintf("%d of 300 sub-counties had no sampled cluster", nrow(nos)))
nt <- nos |> transmute(`Sub-county` = sub("_", " / ", admin2.name.full), `Fay-Herriot` = pc(FH2_0_mean, 0), `Unit-level` = pc(UN2_0_mean, 0),
                       `Unit-level + cov.` = pc(UN2_mean, 0), `95% interval (unit + cov.)` = paste0(pc(UN2_lo, 0), "–", pc(UN2_hi, 0)), `County mean (sampled areas)` = pc(county_mean_unit, 0))
x <- add_ft(x, ft_style(nt, c(2.6, 1.3, 1.3, 1.6, 2.3, 2.3), 12), .5, 1.55)
x <- img(x, "fig_admin2_missing.png", 10.1, 3.9, 3.0, 3.1)
x <- bullets(x, list(list("Sensible: ", "most estimates sit close to the county average with wide intervals (SD 5–13 pp)."),
                     list("Check: ", "covariates move some estimates visibly (Turkana / unknown 2 85% to 80%; Nairobi / Dagoretti North 85% to 89%), and the nested model gives an implausible 41% for Garissa / Lagdera."),
                     list("Use with care: ", "report these with their intervals and flag them as covariate- and neighbour-driven.")),
             .7, 4.55, 9.2, 2.5, 14, gap = 3)

# 12. Shrinkage admin 1
sh <- function(l, m) shr |> filter(level == l, model == m)
x <- new_slide(x, "Shrinkage at county level is moderate; covariates reduce it slightly", "Model-based estimate against direct estimate, county level")
x <- img(x, "fig_admin1_shrinkage.png", .4, 1.5, 6.2, 5.5)
st <- shr |> filter(level == "Admin 1") |> transmute(Model = model, `Slope vs direct` = sprintf("%.2f", slope), `Variance retained` = sprintf("%.2f", var_ratio),
                                                      `Mean |shift| (pp)` = pp(mean_abs_shift, 1), `SE ratio` = sprintf("%.2f", mean_se_ratio))
x <- add_ft(x, ft_style(st, c(2.4, 1, 1.2, 1.1, .8), 11), 6.8, 1.7)
x <- bullets(x, list(list("Reading: ", "a slope of 1 means no shrinkage; values below 1 show estimates pulled to the mean."),
                     list("Covariates ", sprintf("raise the retained variance from %.2f to %.2f (Fay-Herriot): less blanket shrinkage because education explains part of the spread.", sh("Admin 1", "Fay-Herriot")$var_ratio, sh("Admin 1", "Fay-Herriot + covariates")$var_ratio))),
             6.8, 4.3, 6.2, 2.6, 14)

# 13. Shrinkage admin 2
x <- new_slide(x, "Shrinkage is strong at sub-county level (about 5 clusters per area)", "Model-based estimate against direct estimate (sub-counties with a direct estimate)")
x <- img(x, "fig_admin2_shrinkage.png", .4, 1.5, 6.6, 5.5)
st2 <- shr |> filter(level == "Admin 2") |> transmute(Model = model, Slope = sprintf("%.2f", slope), `Variance retained` = sprintf("%.2f", var_ratio), `Mean |shift| (pp)` = pp(mean_abs_shift, 1))
x <- add_ft(x, ft_style(st2, c(2.6, .8, 1.2, 1.1), 11), 7.3, 1.7)
x <- bullets(x, list(list("", sprintf("Unit-level estimates retain %.0f%% of the direct variance; Fay-Herriot with the same covariates retains %.0f%%.", 100 * sh("Admin 2", "Unit-level")$var_ratio, 100 * sh("Admin 2", "Fay-Herriot + covariates")$var_ratio)),
                     list("", "Sparse direct estimates (often 100%) are replaced by model values, which accounts for much of the shift.")),
             7.3, 4.95, 5.7, 2.0, 14)

# 14. Nested
x <- new_slide(x, "Nested models did not help: worse fit and wider intervals", "Unit-level model with a county IID effect plus sub-county BYM2")
nt2 <- tabs |> filter(level == "Admin 2", family %in% c("Unit-level", "Fay-Herriot"), model %in% c("No covariates", "Nested: No covariates", "education_yrs + anc4plus + access_difficulty + facility_delivery", "Nested: education_yrs + anc4plus + access_difficulty + facility_delivery")) |>
  transmute(Family = family, Structure = ifelse(nested, "Nested", "BYM2 only"), Covariates = ifelse(n_cov == 0, "None", "All four"), WAIC = sprintf("%.0f", waic), `Mean SD (pp)` = pp(mean_sd, 2), `Mean CV` = sprintf("%.2f", mean_cv)) |>
  arrange(Family, Covariates, Structure)
x <- add_ft(x, ft_style(nt2, c(1.7, 1.5, 1.4, 1, 1.4, 1.1), 12), .5, 1.6)
x <- bullets(x, list(list("Why it was tried: ", "a county effect can reduce over-shrinkage of unit-level estimates towards a single global mean."),
                     list("What happened: ", sprintf("unit-level WAIC rose from %.0f to %.0f (%.0f with covariates) and the mean CV almost doubled, because the county term absorbs most of the spatial signal (sub-county spatial precision rises from %.1f to %.1f) and leaves little for sparse sub-counties.", c_un2$none$waic, c_un2n$none$waic, c_un2n$all$waic, hy$mean[hy$family == "Unit-level" & hy$level == "Admin 2" & hy$hyper == "Precision for sID" & hy$model == "No covariates"], hy$mean[hy$family == "Unit-level" & hy$level == "Admin 2" & hy$hyper == "Precision for sID" & hy$model == "Nested: No covariates"])),
                     list("Take-away: ", "retain the single-level BYM2 unit-level model; revisit nesting with more clusters per county or informative priors.")),
             .7, 4.5, 12, 2.5, 15)

# 15. Differences
x <- new_slide(x, "Model-based estimates agree on ranking and narrow the intervals",
               "County estimates with 95% intervals, ordered by direct estimate; dashed line is the national 89.2%")
x <- img(x, "fig_admin1_lineplot.png", .4, 1.55, 12.5, 4.35)
dd <- a1 |> mutate(d_fh = FH1_0_mean - direct_mean, wr = (direct_hi - direct_lo) / (FH1_hi - FH1_lo)) |> arrange(desc(abs(d_fh)))
x <- bullets(x, list(list("Largest shifts: ", paste0(dd$admin1.name[1:4], " (", sprintf("%+.1f", 100 * dd$d_fh[1:4]), " pp)", collapse = ", "), ", mostly low-coverage counties with wide direct intervals."),
                     list("Interval width: ", sprintf("direct intervals are on average %.1f times wider than Fay-Herriot with covariates.", mean(dd$wr))),
                     list("Unit-level vs Fay-Herriot: ", sprintf("mean absolute difference %s pp; both lead to the same county ranking (rank correlation %.2f).", pp(mean(abs(a1$UN1_mean - a1$FH1_mean)), 2), cor(a1$UN1_mean, a1$FH1_mean, method = "spearman"))),
                     list("Direct estimates ", "can reach 100% with wide intervals and should not be ranked without smoothing.")),
             .7, 5.95, 12, 1.15, 12, gap = 1)

# 16. Rankings admin 1
rt <- rk
x <- new_slide(x, "County ranking: low coverage stands out in the north-east and east", "Fay-Herriot with covariates; colour shows whether the 95% interval excludes the national estimate")
x <- img(x, "fig_admin1_ranking.png", .4, 1.4, 12.5, 4.5)
r1 <- rt |> filter(level == "Admin 1"); below <- r1 |> filter(grp == "CI below national") |> arrange(m); above <- r1 |> filter(grp == "CI above national") |> arrange(desc(m))
x <- bullets(x, list(list(paste0("Clearly below national (", nrow(below), "): "), paste0(paste(below$name, collapse = ", "), ".")),
                     list(paste0("Clearly above national (", nrow(above), "): "), paste0(paste(head(above$name, 8), collapse = ", "), if (nrow(above) > 8) ", …" else "", ".")),
                     list("Most counties ", "overlap the national value, so only the extremes are statistically distinguishable.")),
             .7, 5.95, 12, 1.2, 13, gap = 2)

# 17. Rankings admin 2
r2 <- rt |> filter(level == "Admin 2"); b2 <- r2 |> filter(grp == "CI below national"); a2h <- r2 |> filter(grp == "CI above national")
x <- new_slide(x, sprintf("Sub-county ranking: %d clearly below national, mostly in Mandera, Garissa, Wajir", nrow(b2)), "Unit-level with covariates; 20 lowest and 20 highest shown")
x <- img(x, "fig_admin2_ranking.png", .3, 1.5, 9.0, 5.5)
x <- bullets(x, list(list("Lowest estimates: ", paste(sub(", ", " / ", head(r2$name, 5)), collapse = "; "), "."),
                     list("Intervals entirely below national: ", sprintf("%d sub-counties; entirely above: %d.", nrow(b2), nrow(a2h))),
                     list("Interpretation: ", "wide intervals (CV about 0.5) mean most sub-county ranks are uncertain; use them for targeting only with the intervals.")),
             9.3, 1.7, 3.7, 5.2, 14)

# 18. Suggestions
x <- new_slide(x, "Suggestions for the app and issues encountered")
x <- bullets(x, list(
  list("Covariate guidance. ", "Show WAIC / DIC and the explained spatial variance next to the covariate effects, so users can judge benefit directly."),
  list("Fay-Herriot WAIC. ", "fhModel() does not request WAIC, so it had to be recomputed by refitting the stored INLA call; return it in the model object."),
  list("Admin-2 Fay-Herriot fails without a variance fix. ", "Direct estimates of 100% give infinite logits and a singular matrix; var.fix = TRUE is needed and should be the default with a warning."),
  list("Plot arguments are inconsistent. ", "prevMap() needs value.to.plot = \"mean\" for directEST objects although the column is direct.est."),
  list("Covariate names. ", "Templates contain placeholders such as “unknown 7” and blank cells; the app should flag missing covariate values before fitting."),
  list("Feature requests. ", "multi-covariate comparison table, collinearity warning, side-by-side nested vs non-nested results, and shrinkage plots that include the covariate model."),
  list("Reproducibility. ", "Allow saving the cluster-level input with the model object; here it had to be recovered from the INLA data.")),
  .7, 1.5, 12, 5.5, 16, gap = 8)

# 19. Conclusions
x <- new_slide(x, "Conclusions")
x <- bullets(x, list(
  list("Variation. ", sprintf("County coverage spans %s–%s and sub-county %s–%s; the north and north-east are lowest.", pc(min(a1$FH1_0_mean), 0), pc(max(a1$FH1_0_mean), 0), pc(min(a2$UN2_0_mean), 0), pc(max(a2$UN2_0_mean), 0))),
  list("Reasons. ", "Education and facility use track coverage across areas; access difficulty is negatively related, but the covariates overlap."),
  list("Covariates. ", "Education is the only covariate with a clear effect; adding it tightens precision by roughly 3% (county) to 7% (sub-county) but does not improve WAIC."),
  list("Missing sub-counties. ", "Model estimates are plausible and close to county averages, with wide intervals."),
  list("Shrinkage. ", "Moderate at county level, strong at sub-county level; covariates reduce it a little."),
  list("Nested models. ", "Did not help in this application."),
  list("Rankings. ", "Only the extremes differ credibly from the national level; report intervals alongside ranks.")),
  .7, 1.5, 12, 5.5, 17, gap = 9)

print(x, "Penta3_SAE_covariates_presentation.pptx")
cat("Saved Penta3_SAE_covariates_presentation.pptx with", slide_no + 1, "slides\n")
