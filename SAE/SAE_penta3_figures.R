# Presentation-quality figures for the Penta3 SAE project.
# Reads outputs_covariates/results.rds (written by SAE_covariates_penta3.R) and
# re-draws maps, rankings, covariate scatterplots, shrinkage and the line plot.
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2); library(sf); library(patchwork) })
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Burden-of-disease-estimation_cema/SAE")
od <- "outputs_covariates"; R <- readRDS(file.path(od, "results.rds")); a1 <- R$a1; a2 <- R$a2; nat <- R$national$dhs_report
colors_cema <- c("#27aae1", "#fe7501", "#084887", "#efca08", "#497048", "#006d77")
pal <- c("#efca08", "#27aae1", "#084887")
theme_set(theme_minimal(base_size = 13) + theme(plot.title = element_text(face = "bold"), legend.position = "bottom"))
sv <- function(p, f, w, h) ggsave(file.path(od, f), p, width = w, height = h, dpi = 200, bg = "white")
cn <- c("education_yrs", "anc4plus", "access_difficulty", "facility_delivery")
cov_labels <- c(education_yrs = "Mean years of education", anc4plus = "4+ ANC visits",
                access_difficulty = "Difficulty accessing facility", facility_delivery = "Facility delivery")
s1 <- st_read("../kenya_shape_files/gadm41_KEN_1.shp", quiet = TRUE)
s2 <- st_read("../kenya_shape_files/gadm41_KEN_2.shp", quiet = TRUE) |> mutate(admin2.name.full = paste0(NAME_1, "_", NAME_2))
m1 <- s1 |> left_join(a1, by = c("NAME_1" = "admin1.name")); m2 <- s2 |> left_join(a2, by = "admin2.name.full")

# ---- maps: thin borders, percent legend, common limits (values outside are squished) ----
map1 <- function(d, v, ttl, lim, lw, lab = scales::percent) ggplot(d) + geom_sf(aes(fill = .data[[v]]), colour = "white", linewidth = lw) +
  scale_fill_gradientn(colours = pal, limits = lim, oob = scales::squish, labels = lab, name = NULL, na.value = "grey85",
                       guide = guide_colourbar(barwidth = 14, barheight = .6)) +
  labs(title = ttl) + theme_void(base_size = 12) + theme(plot.title = element_text(face = "bold", hjust = .5, size = 12), legend.position = "bottom")
lim1 <- c(.4, 1); lim2 <- c(.4, 1)
sv(map1(m1, "direct_mean", "Direct", lim1, .25) + map1(m1, "FH1_0_mean", "Fay-Herriot", lim1, .25) + map1(m1, "FH1_mean", "Fay-Herriot + covariates", lim1, .25) +
     plot_layout(guides = "collect") & theme(legend.position = "bottom"), "fig_admin1_maps.png", 13, 5.6)
sv(map1(m1, "UN1_0_mean", "Unit-level", lim1, .25) + map1(m1, "UN1_mean", "Unit-level + covariates", lim1, .25) +
     plot_layout(guides = "collect") & theme(legend.position = "bottom"), "fig_admin1_maps_unit.png", 9, 5.6)
sv(map1(m2, "direct_mean", "Direct (grey: no sample)", lim2, .04) + map1(m2, "UN2_0_mean", "Unit-level", lim2, .04) +
     map1(m2, "UN2_mean", "Unit-level + covariates", lim2, .04) + map1(m2, "UN2n_mean", "Nested + covariates", lim2, .04) +
     plot_layout(nrow = 1, guides = "collect") & theme(legend.position = "bottom"), "fig_admin2_maps.png", 16, 5.6)
cvl <- c(0, quantile(unlist(a2[c("UN2_0_cv", "UN2_cv", "UN2n_cv")]), .98, na.rm = TRUE))
sv(map1(m2, "UN2_0_cv", "Unit-level", cvl, .04, scales::percent) + map1(m2, "UN2_cv", "Unit-level + covariates", cvl, .04, scales::percent) +
     map1(m2, "UN2n_cv", "Nested + covariates", cvl, .04, scales::percent) + plot_layout(nrow = 1, guides = "collect") &
     scale_fill_gradientn(colours = rev(pal), limits = cvl, oob = scales::squish, labels = scales::percent, name = "CV", na.value = "grey85",
                          guide = guide_colourbar(barwidth = 14, barheight = .6)) & theme(legend.position = "bottom"), "fig_admin2_cv_maps.png", 14, 5.6)

# ---- covariate scatterplots with bounded (quasi-binomial) trend lines ------
sc <- function(d, y, ttl, ylab, pt) d |> pivot_longer(all_of(cn), names_to = "cov", values_to = "x") |>
  mutate(cov = factor(cov_labels[cov], levels = cov_labels)) |> filter(!is.na(.data[[y]])) |>
  ggplot(aes(x, .data[[y]])) + geom_point(colour = colors_cema[3], alpha = .6, size = pt) +
  geom_smooth(method = "glm", method.args = list(family = quasibinomial), colour = colors_cema[2], fill = colors_cema[2], alpha = .15, formula = y ~ x) +
  facet_wrap(~cov, nrow = 1, scales = "free_x") + scale_y_continuous(labels = scales::percent, limits = c(NA, 1), breaks = seq(0, 1, .2)) +
  labs(x = NULL, y = ylab, title = ttl)
sv(sc(a1, "direct_mean", "Counties: covariates vs direct estimate", "Direct estimate", 2.2), "fig_admin1_cov_scatter.png", 13, 4.3)
sv(sc(a2, "UN2_0_mean", "Sub-counties: covariates vs unit-level estimate", "Unit-level estimate (no covariates)", 1.6), "fig_admin2_cov_scatter.png", 13, 4.3)

# ---- shrinkage scatterplots --------------------------------------------------
shp <- function(d, ys, labs, ttl, lim) {
  l <- bind_rows(Map(function(y, lb) tibble(direct = d$direct_mean, est = d[[y]], model = lb), ys, labs)) |> filter(!is.na(direct), !is.na(est)) |>
    mutate(model = factor(model, levels = labs))
  ggplot(l, aes(direct, est, colour = model)) + geom_abline(linetype = 2, colour = "grey50") + geom_point(alpha = .65, size = 1.8) +
    scale_colour_manual(values = colors_cema[c(1, 2, 3, 4)], name = NULL) +
    scale_x_continuous(labels = scales::percent, limits = lim, breaks = seq(.4, 1, .1)) + scale_y_continuous(labels = scales::percent, limits = lim, breaks = seq(.4, 1, .1)) +
    guides(colour = guide_legend(nrow = 2, override.aes = list(size = 3, alpha = 1))) + coord_equal() + labs(x = "Direct estimate", y = "Model-based estimate", title = ttl)
}
sv(shp(a1, c("FH1_0_mean", "FH1_mean", "UN1_mean"), c("Fay-Herriot", "Fay-Herriot + covariates", "Unit-level + covariates"), "Counties: shrinkage", c(.3, 1)), "fig_admin1_shrinkage.png", 7, 6.8)
sv(shp(a2, c("FH2_mean", "UN2_0_mean", "UN2_mean", "UN2n_mean"), c("Fay-Herriot + covariates", "Unit-level", "Unit-level + covariates", "Nested + covariates"),
       "Sub-counties: shrinkage (direct 100% estimates form the right-hand column)", c(.45, 1)) + theme(plot.title = element_text(size = 11)), "fig_admin2_shrinkage.png", 7.5, 7)

# ---- rankings with readable labels -------------------------------------------
rk_cols <- c("CI below national" = colors_cema[2], "CI above national" = colors_cema[3], "CI overlaps national" = "grey55")
rk_prep <- function(d, tag, nm) d |> transmute(name = gsub("_", ", ", .data[[nm]]), m = .data[[paste0(tag, "_mean")]], lo = .data[[paste0(tag, "_lo")]], hi = .data[[paste0(tag, "_hi")]]) |>
  arrange(m) |> mutate(rank = row_number(), grp = factor(case_when(hi < nat ~ "CI below national", lo > nat ~ "CI above national", TRUE ~ "CI overlaps national"), levels = names(rk_cols)))
rk_panel <- function(x, ttl, xl = c(0, 1)) ggplot(x, aes(m, factor(name, levels = name), colour = grp)) + geom_vline(xintercept = nat, linetype = 2, colour = "grey40") +
  geom_pointrange(aes(xmin = lo, xmax = hi), size = .3, linewidth = .5) + scale_colour_manual(values = rk_cols, name = NULL, drop = FALSE) +
  scale_x_continuous(labels = scales::percent, limits = xl) + labs(x = "Penta3 coverage (95% interval)", y = NULL, title = ttl)
r1 <- rk_prep(a1, "FH1", "admin1.name")
sv(rk_panel(r1, "County ranking: Fay-Herriot + covariates") + theme(axis.text.y = element_text(size = 9)), "fig_admin1_ranking.png", 8, 9.5)
r2 <- rk_prep(a2, "UN2", "admin2.name.full")
sv((rk_panel(head(r2, 20), "Lowest 20 sub-counties") + rk_panel(tail(r2, 20), "Highest 20 sub-counties", c(.8, 1))) + plot_annotation(caption = "Orange: 95% interval entirely below the national 89.2%; blue: entirely above; grey: overlaps. Dashed line: national estimate.") &
     theme(legend.position = "none", axis.text.y = element_text(size = 8)), "fig_admin2_ranking.png", 12.5, 7)
write.csv(bind_rows(r1 |> mutate(level = "Admin 1"), r2 |> mutate(level = "Admin 2")), file.path(od, "rankings_final.csv"), row.names = FALSE)

# ---- county line plot: direct, FH + cov, unit-level + cov ----------------------
l1 <- a1 |> arrange(direct_mean) |> mutate(admin1.name = factor(admin1.name, levels = admin1.name))
lp <- bind_rows(lapply(list(c("direct", "Direct"), c("FH1", "Fay-Herriot + covariates"), c("UN1", "Unit-level + covariates")), function(p)
  tibble(area = l1$admin1.name, m = l1[[paste0(p[1], "_mean")]], lo = l1[[paste0(p[1], "_lo")]], hi = l1[[paste0(p[1], "_hi")]], model = p[2]))) |>
  mutate(model = factor(model, levels = c("Direct", "Fay-Herriot + covariates", "Unit-level + covariates")))
sv(ggplot(lp, aes(area, m, colour = model)) + geom_hline(yintercept = nat, linetype = 2, colour = "grey40") +
     geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(.7), size = .22, linewidth = .4) +
     scale_colour_manual(values = colors_cema[c(2, 3, 5)], name = NULL) + scale_y_continuous(labels = scales::percent) + coord_flip() +
     labs(x = NULL, y = "Penta3 coverage (95% interval)", title = "Counties: direct vs model-based estimates", subtitle = "Dashed line: national estimate 89.2%") +
     theme(axis.text.y = element_text(size = 9)), "fig_admin1_lineplot.png", 8, 11)
cat("figures redrawn\n")

# ---- two-column layouts for the 47-county ranking and line plot (larger text on slides) ----
cap_rk <- "Orange: 95% interval entirely below the national 89.2%; blue: entirely above; grey: overlaps. Dashed line: national estimate."
hh <- ceiling(nrow(r1) / 2)
sv((rk_panel(r1[1:hh, ], "Counties: lowest 24", c(.3, 1)) + rk_panel(r1[(hh + 1):nrow(r1), ], "Counties: highest 23", c(.3, 1))) +
     plot_annotation(caption = cap_rk) & theme(legend.position = "none", axis.text.y = element_text(size = 9)), "fig_admin1_ranking.png", 13, 6.3)
lvl <- levels(l1$admin1.name); grp1 <- lvl[1:hh]; grp2 <- lvl[(hh + 1):length(lvl)]
lpf <- function(g, ttl) ggplot(lp |> filter(area %in% g), aes(area, m, colour = model)) + geom_hline(yintercept = nat, linetype = 2, colour = "grey40") +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(.7), size = .22, linewidth = .4) +
  scale_colour_manual(values = colors_cema[c(2, 3, 5)], name = NULL) + scale_y_continuous(labels = scales::percent, limits = c(.2, 1)) + coord_flip() +
  labs(x = NULL, y = "Penta3 coverage (95% interval)", title = ttl) + theme(axis.text.y = element_text(size = 9))
sv((lpf(grp1, "Lowest 24 counties (by direct estimate)") + lpf(grp2, "Highest 23 counties")) + plot_layout(guides = "collect") &
     theme(legend.position = "bottom"), "fig_admin1_lineplot.png", 13, 6.6)
cat("two-column figures done\n")
