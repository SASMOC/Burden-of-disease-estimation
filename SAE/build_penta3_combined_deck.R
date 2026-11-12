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
q_tag <- ""
new_slide <- function(x, title, sub = NULL) {
  slide_no <<- slide_no + 1
  x <- add_slide(x, "Blank", "Office Theme")
  x <- box(x, 0, 0, W, .12, navy)
  x <- tx(x, title, .55, .3, W - 1.1, .7, size = 24, bold = TRUE, col = navy)
  if (!is.null(sub)) x <- tx(x, sub, .55, 1.12, W - 1.1, .4, size = 14, col = grey, italic = TRUE)
  x <- box(x, .55, 1.08, 1.3, .045, orange)
  if (nzchar(q_tag)) x <- tx(x, q_tag, W - 5.1, .14, 4.5, .28, size = 11, bold = TRUE, col = orange, align = "right")
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


# ---- app helpers -----------------------------------------------------------
appcmp <- function(x, f, top, h, lt = "Shiny app: without covariates", rt = "Shiny app: with covariates") {
  x <- tx(x, lt, .4, top - .38, 6.2, .35, size = 13, bold = TRUE, col = navy, align = "center")
  x <- tx(x, rt, 6.75, top - .38, 6.2, .35, size = 13, bold = TRUE, col = orange, align = "center")
  x <- img(x, paste0("app_nocov_", f, ".png"), .4, top, 6.2, h)
  img(x, paste0("app_cov_", f, ".png"), 6.75, top, 6.2, h)
}
wd <- function(d, m) d[[paste0(m, "_hi")]] - d[[paste0(m, "_lo")]]
a1$w_d <- a1$direct_hi - a1$direct_lo
pick <- function(nm, col) a1[[col]][a1$admin1.name == nm]

# 1. Title
x <- add_slide(x, "Blank", "Office Theme")
x <- box(x, 0, 0, W, H, navy)
x <- box(x, .8, 3.55, 2.2, .07, orange)
x <- tx(x, "Do area-level covariates sharpen small-area estimates of Penta3 coverage?", .8, 1.5, 11.5, 1.9, size = 38, bold = TRUE, col = "white")
x <- tx(x, "Shiny app reports (without and with covariates) combined with a SUMMER reproduction: direct, Fay-Herriot and unit-level estimates, counties and sub-counties, DHS 2022", .8, 3.8, 11.5, 1.2, size = 20, col = "#CFE3F3")
x <- tx(x, "CEMA SAE short course  |  Hands-on project  |  SUMMER / surveyPrev", .8, 6.2, 11, .4, size = 14, col = "#9CC3E4")

q_tag <- "Context and method"
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

# 3. Sources and method
x <- new_slide(x, "Two routes to the same estimates: the Shiny app and SUMMER")
mt <- data.frame(Source = c("Shiny app report, no covariates", "Shiny app report, with covariates", "SUMMER / surveyPrev refit"),
                 `What it gives` = c("Direct, area-level and unit-level maps and scatters (county; unit-level also sub-county)", "The same report after uploading county and sub-county covariate templates",
                                     "Every model, with and without covariates, plus WAIC, coefficients, shrinkage and rankings"),
                 `Used for` = c("Baseline; national consistency check (89.18% vs 89.20%)", "Visual effect of covariates as a user of the app sees it", "Answering the questions the app does not report"), check.names = FALSE)
x <- add_ft(x, ft_style(mt, c(3.2, 5.3, 3.9), 13), .5, 1.55)
x <- bullets(x, list(list("Estimators. ", "Direct (directEST), Fay-Herriot (fhModel, BYM2) and unit-level (clusterModel, beta-binomial BYM2); nested unit-level variant at sub-county level."),
                     list("Covariates. ", "Mean years of education, 4+ ANC visits, difficulty accessing a facility and facility delivery, standardised (effects per 1 SD)."),
                     list("Agreement. ", "The app's no-covariate results were reproduced exactly (direct identical; model-based correlation 1.000), and the with-covariate app results match the SUMMER fit with all four covariates (agreement slide under Q3)."),
                     list("Why both. ", "The app gives the maps users see; SUMMER adds the model-comparison numbers the app does not show.")),
             .5, 3.9, 12.3, 3.1, 15)

# Overview: questions and short answers
q_tag <- "Overview"
sdr <- function(f, l, m) shr |> filter(level == l, model == m) |> pull(f)
a1c0 <- assoc |> filter(level == "Admin 1"); a2c0 <- assoc |> filter(level == "Admin 2")
qa <- data.frame(Question = c("Q1 Geographic variation?", "Q2 Reasons for differences?", "Q3 Covariates change estimates / precision?", "Q4 Unsampled sub-counties plausible?",
                              "Q5 How strong is shrinkage?", "Q6 Nested models useful?", "Q7 Direct vs area vs unit differences?", "Q8 Very high or low areas?", "Q9 Suggestions and bugs?"),
                 `Short answer` = c(sprintf("Counties %s–%s; sub-counties %s–%s. Lowest in the north-east and east.", pc(min(a1$FH1_0_mean), 0), pc(max(a1$FH1_0_mean), 0), pc(min(a2$UN2_0_mean), 0), pc(max(a2$UN2_0_mean), 0)),
                                    sprintf("Education tracks coverage (county r = %.2f, sub-county r = %.2f); facility delivery at county level; access difficulty is negative.", a1c0$r[a1c0$covariate == "education_yrs"], a2c0$r[a2c0$covariate == "education_yrs"]),
                                    sprintf("Education: clear effect, but small precision gain (county SD %s to %s pp, sub-county %s to %s pp) and no WAIC gain.", pp(c_fh1$none$mean_sd, 2), pp(c_fh1$edu$mean_sd, 2), pp(c_un2$none$mean_sd, 2), pp(c_un2$edu$mean_sd, 2)),
                                    sprintf("Mostly: all %d are close to county averages with wide intervals; the nested model gives an implausible 41%% for Garissa / Lagdera.", nrow(nos)),
                                    sprintf("County slope %.2f (Fay-Herriot) / %.2f (unit-level); sub-county %.2f / %.2f. Covariates change little.", sdr("slope", "Admin 1", "Fay-Herriot"), sdr("slope", "Admin 1", "Unit-level"), sdr("slope", "Admin 2", "Fay-Herriot"), sdr("slope", "Admin 2", "Unit-level")),
                                    sprintf("No: WAIC %.0f to %.0f, wider intervals, one implausible estimate.", c_un2$none$waic, c_un2n$none$waic),
                                    "Same ranking and pattern; model intervals narrower than direct; direct 100% values shrink most.",
                                    sprintf("%d counties and %d sub-counties have intervals entirely below the national level; %d and %d above.", sum(rk$level == "Admin 1" & rk$grp == "CI below national"), sum(rk$level == "Admin 2" & rk$grp == "CI below national"), sum(rk$level == "Admin 1" & rk$grp == "CI above national"), sum(rk$level == "Admin 2" & rk$grp == "CI above national")),
                                    "Report covariate effects and WAIC; fix Fay-Herriot admin-2 failure and prevMap argument; flag missing covariates."), check.names = FALSE)
x <- new_slide(x, "The nine questions and their short answers", "Each section that follows is tagged with its question number")
x <- add_ft(x, ft_style(qa, c(3.9, 8.5), 12), .5, 1.6)

q_tag <- "Q1 · Geographic variation"
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

# 6b. App direct estimates
x <- new_slide(x, "Shiny app, county level: direct estimates are the same with and without covariates", "Direct estimates cannot use covariates; the app repeats this panel in both reports")
x <- img(x, "app_cov_p06.png", .4, 1.55, 8.4, 5.4)
g <- a1[order(-a1$w_d), ]
x <- bullets(x, list(list("Map: ", "low estimates in the north-east and east; the widest intervals (yellow to purple in the width map) are in the same areas."),
                     list("Widest direct intervals: ", paste0(g$admin1.name[1:4], " (", sprintf("%.2f", g$w_d[1:4]), ")", collapse = ", "), "."),
                     list("Implication: ", "these are the counties where borrowing strength, and covariates, can matter most.")),
             9.0, 1.7, 4.1, 5.2, 15)

q_tag <- "Q2 · Reasons for differences"
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

q_tag <- "Q3 · Covariates, estimates and precision"
# 9. App: area-level admin 1
x <- new_slide(x, "County, app Fay-Herriot: covariates change estimates only locally", "Legend ranges are identical: mean 0.50–0.90, CI width 0.10–0.30, CV 4–16%")
x <- appcmp(x, "p07", 1.95, 4.0)
x <- bullets(x, list(list("Overall picture unchanged; changes are local (SUMMER values). ", sprintf("Samburu: %s to %s, CI width %.2f to %.2f. Nyeri: CI width %.2f to %.2f. Elgeyo-Marakwet: %.2f to %.2f.",
                       pc(pick("Samburu", "FH1_0_mean"), 1), pc(pick("Samburu", "FH1_mean"), 1), wd(a1, "FH1_0")[a1$admin1.name == "Samburu"], wd(a1, "FH1")[a1$admin1.name == "Samburu"],
                       wd(a1, "FH1_0")[a1$admin1.name == "Nyeri"], wd(a1, "FH1")[a1$admin1.name == "Nyeri"], wd(a1, "FH1_0")[a1$admin1.name == "Elgeyo-Marakwet"], wd(a1, "FH1")[a1$admin1.name == "Elgeyo-Marakwet"])),
                     list("Average effect is small: ", sprintf("mean CI width %.3f without covariates, %.3f with all four, %.3f with education only.", c_fh1$none$mean_width, c_fh1$all$mean_width, c_fh1$edu$mean_width))),
             .5, 6.0, 12.3, 1.05, 13, gap = 3)

# 10. App: unit-level admin 1
x <- new_slide(x, "County, app unit-level: virtually no visible change", "Legend ranges identical: mean 0.50–0.90, CI width 0.10–0.20, CV 2–10%")
x <- appcmp(x, "p08", 1.95, 4.0)
x <- bullets(x, list(list("The cluster-level model already uses all 3,679 children, ", sprintf("so covariates add little: mean CI width %.3f without covariates, %.3f with all four, %.3f with education only (SUMMER).", c_un1$none$mean_width, c_un1$all$mean_width, c_un1$edu$mean_width)),
                     list("Unit-level intervals are narrower than Fay-Herriot ", sprintf("(%.3f vs %.3f), because the model uses cluster counts rather than the 47 direct estimates.", c_un1$none$mean_width, c_fh1$none$mean_width))),
             .5, 6.0, 12.3, 1.05, 13, gap = 3)

# 11. App: unit-level admin 2
x <- new_slide(x, "Sub-county, app unit-level: tighter intervals, similar pattern", "Mean legend 0.40–0.80 becomes 0.40–0.90; the CI-width legend tops out lower (about 0.5 to 0.4–0.5)")
x <- appcmp(x, "p09", 1.95, 4.0)
x <- bullets(x, list(list("Visible change is in precision, not in the pattern: ", sprintf("median CI width %.2f without covariates, %.2f with all four (SUMMER, 300 sub-counties); the highest widths fall from %.2f to %.2f.", median(wd(a2, "UN2_0")), median(wd(a2, "UN2")), max(wd(a2, "UN2_0")), max(wd(a2, "UN2")))),
                     list("To check: ", "one north-central polygon is grey in the with-covariates exceedance map; it should be confirmed whether it is an area with no estimate.")),
             .5, 6.0, 12.3, 1.05, 13, gap = 3)

# 8. Coefficients
cf <- function(fam, lvl, model, term = "education_yrs") coefs |> filter(family == fam, level == lvl, model == !!model, term == !!term)
ALL4 <- "education_yrs + anc4plus + access_difficulty + facility_delivery"
ed1 <- bind_rows(edu("Fay-Herriot", "Admin 1", "education_yrs"), edu("Unit-level", "Admin 1", "education_yrs"), edu("Fay-Herriot", "Admin 2", "education_yrs"), edu("Unit-level", "Admin 2", "education_yrs"))
ed4 <- bind_rows(cf("Fay-Herriot", "Admin 1", ALL4), cf("Unit-level", "Admin 1", ALL4), cf("Fay-Herriot", "Admin 2", ALL4), cf("Unit-level", "Admin 2", ALL4))
fd1 <- bind_rows(cf("Fay-Herriot", "Admin 1", "facility_delivery", "facility_delivery"), cf("Unit-level", "Admin 1", "facility_delivery", "facility_delivery"))
x <- new_slide(x, "Education is the one consistently clear covariate",
               "Posterior mean and 95% credible interval, effect on logit coverage per 1 SD")
x <- img(x, "fig_coefficients.png", .4, 1.5, 8.3, 5.5)
x <- bullets(x, list(list("One at a time: ", sprintf("education is clear in all four models (+%.2f to +%.2f logits per SD, intervals above zero). Facility delivery is clear only at county level (+%.2f Fay-Herriot, +%.2f unit-level). 4+ ANC and access difficulty are not clear in any model; access difficulty is negative and borderline.",
                                                     min(ed1$mean), max(ed1$mean), fd1$mean[1], fd1$mean[2])),
                     list("All four together: ", sprintf("education stays clear in %d of 4 models (+%.2f to +%.2f) but not in county Fay-Herriot (+%.2f, %.2f to %.2f); the other three covariates are not clear anywhere.",
                                                         sum(ed4$excludes_zero), min(ed4$mean[ed4$excludes_zero]), max(ed4$mean[ed4$excludes_zero]), ed4$mean[1], ed4$lower[1], ed4$upper[1])),
                     list("Why: ", sprintf("the covariates overlap (education and facility delivery r = %.2f across counties), so their effects are shared when entered together.", max(abs(R$cov_cor$`Admin 1`[upper.tri(R$cov_cor$`Admin 1`)]))))),
             8.9, 1.7, 4.1, 5.2, 13, gap = 5)

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

# 15. Agreement
x <- new_slide(x, "The app and SUMMER agree, so the SUMMER numbers can explain the app maps",
               "Values read from the app's with-covariate scatter plots against the SUMMER refit (all four covariates)")
ag <- data.frame(Quantity = c("Garissa, direct estimate", "Garissa, Fay-Herriot + cov.", "Kajiado, Fay-Herriot + cov.", "Tana River, Fay-Herriot (none → cov.)", "Samburu, CI width (none → cov.)"),
                 `App (read from plot)` = c("0.35", "≈0.48", "≈0.80", "≈0.76 → 0.74", "≈0.20 → 0.245"),
                 SUMMER = c(sprintf("%.3f", pick("Garissa", "direct_mean")), sprintf("%.3f", pick("Garissa", "FH1_mean")), sprintf("%.3f", pick("Kajiado", "FH1_mean")),
                            sprintf("%.3f → %.3f", pick("Tana River", "FH1_0_mean"), pick("Tana River", "FH1_mean")), sprintf("%.3f → %.3f", wd(a1, "FH1_0")[a1$admin1.name == "Samburu"], wd(a1, "FH1")[a1$admin1.name == "Samburu"])), check.names = FALSE)
x <- add_ft(x, ft_style(ag, c(4.6, 3.3, 3.3), 14), .6, 1.7)
x <- bullets(x, list(list("Conclusion: ", "the app with covariates behaves as a Fay-Herriot / unit-level model with all four covariates, which is what SUMMER fitted."),
                     list("Limit: ", "the app PDF contains pictures, not numbers; exact values come from the SUMMER refit.")),
             .6, 4.6, 12.1, 2.2, 16)

q_tag <- "Q4 · Unsampled sub-counties"
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

q_tag <- "Q5 · Shrinkage"
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

# App scatter slides
x <- new_slide(x, "App scatter, county Fay-Herriot: means nearly identical, interval widths shift slightly", "Model-based against direct estimate, county level (dashed line = no change)")
x <- appcmp(x, "p10", 1.95, 3.3)
x <- bullets(x, list(list("Means: ", sprintf("both versions pull low direct values towards the middle (Garissa 0.35 to %.2f); the shrinkage slope is %.2f without and %.2f with covariates.", pick("Garissa", "FH1_mean"), sh("Admin 1", "Fay-Herriot")$slope, sh("Admin 1", "Fay-Herriot + covariates")$slope)),
                     list("Widths: ", "almost all counties sit below the line, so smoothing narrows intervals; the Samburu point moves up with covariates (0.20 to 0.25)."),
                     list("Reading: ", "covariates changed local estimates but did not change the general picture.")), .5, 5.4, 12.3, 1.6, 14, gap = 3)
x <- new_slide(x, "App scatter, county unit-level: stronger narrowing, same means", "Model-based against direct estimate, county level (dashed line = no change)")
x <- appcmp(x, "p11", 1.95, 3.3)
x <- bullets(x, list(list("Means: ", sprintf("same pattern as Fay-Herriot; slope %.2f without and %.2f with covariates.", sh("Admin 1", "Unit-level")$slope, sh("Admin 1", "Unit-level + covariates")$slope)),
                     list("Widths: ", "intervals are narrower than Fay-Herriot for nearly every county, and the two panels are hard to tell apart."),
                     list("Reading: ", "the unit-level model shrinks more because it relies on cluster counts and the spatial structure.")), .5, 5.4, 12.3, 1.6, 14, gap = 3)

q_tag <- "Q6 · Nested models"
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

q_tag <- "Q7 · Direct vs area-level vs unit-level"
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

q_tag <- "Q8 · Rankings"
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

q_tag <- "Q9 · Suggestions and bugs"
# Suggestions
x <- new_slide(x, "Suggestions for the app and issues encountered")
x <- bullets(x, list(
  list("Report covariate effects. ", "The with-covariates PDF shows maps and scatters only; add coefficients with credible intervals, WAIC and explained spatial variance."),
  list("Show differences. ", "Add maps of the change in mean and CI width with versus without covariates."),
  list("Fay-Herriot WAIC. ", "fhModel() does not request WAIC, so it was recomputed by refitting the stored INLA call; return it in the model object."),
  list("Admin-2 Fay-Herriot fails without a variance fix. ", "Direct estimates of 100% give infinite logits and a singular matrix; var.fix = TRUE is needed, with a warning."),
  list("Plot arguments. ", "prevMap() needs value.to.plot = \"mean\" for directEST objects although the column is direct.est."),
  list("Covariate templates. ", "Placeholders such as “unknown 7” and blank cells; flag missing covariate values before fitting. A grey polygon in the exceedance map needs explaining."),
  list("Feature requests. ", "multi-covariate comparison table, collinearity warning, nested versus non-nested side by side, shrinkage plots that include the covariate model."),
  list("Reproducibility. ", "Save the cluster-level input with the model object; here it was recovered from the INLA data.")),
  .7, 1.5, 12, 5.5, 15, gap = 6)

q_tag <- "Summary"
# 19. Conclusions
x <- new_slide(x, "Conclusions")
x <- bullets(x, list(
  list("App and SUMMER. ", "The app with covariates matches the SUMMER fit with all four covariates, so the app maps show the same small precision gain."),
  list("Variation. ", sprintf("County coverage spans %s–%s and sub-county %s–%s; the north and north-east are lowest.", pc(min(a1$FH1_0_mean), 0), pc(max(a1$FH1_0_mean), 0), pc(min(a2$UN2_0_mean), 0), pc(max(a2$UN2_0_mean), 0))),
  list("Reasons. ", "Education and facility use track coverage across areas; access difficulty is negatively related, but the covariates overlap."),
  list("Covariates. ", "Education is the one consistently clear covariate (facility delivery only at county level, one at a time); adding education tightens precision by roughly 3% (county) to 7% (sub-county) but does not improve WAIC."),
  list("Missing sub-counties. ", "Model estimates are plausible and close to county averages, with wide intervals."),
  list("Shrinkage. ", "Moderate at county level, strong at sub-county level; covariates reduce it a little."),
  list("Nested models. ", "Did not help in this application."),
  list("Rankings. ", "Only the extremes differ credibly from the national level; report intervals alongside ranks.")),
  .7, 1.5, 12, 5.6, 16, gap = 7)

print(x, "Penta3_SAE_app_and_SUMMER_combined_presentation.pptx")
cat("Saved combined deck with", slide_no + 1, "slides\n")
