invisible(Sys.setlocale("LC_ALL", "en_US.UTF-8"))
# Extract immunisation and maternal-care indicators from Appendix B (sampling-error
# tables B.2-B.51) of the 2025-26 Kenya Mini DHS: national, urban, rural and 47 counties.
suppressMessages({library(pdftools); library(openxlsx)})

pdf_file <- "2025-26-Kenya-Mini-Demographic-and-Health-Survey1.pdf"
pages <- pdf_text(pdf_file)
first_pg <- 459; last_pg <- 511   # B.2 (Total) ... B.51 (Nairobi City)

num <- function(x) suppressWarnings(as.numeric(gsub(",", "", ifelse(x %in% c("na", "-"), NA, x))))

rows <- list(); geo <- NA; tbl <- NA
for (pg in first_pg:last_pg) {
  lines <- strsplit(pages[pg], "\n")[[1]]
  h <- regmatches(pages[pg], regexpr("Table B\\.[0-9]+[^\n]*", pages[pg]))
  tb <- sub("^(Table B\\.[0-9]+).*", "\\1", h)
  if (length(h) == 1 && !grepl("Continued", h)) {
    g <- sub(".*Sampling errors: (.*) sample,.*", "\\1", h)
    geo <- g; tbl <- tb
  }
  for (ln in lines) {
    f <- strsplit(trimws(ln), "\\s+")[[1]]
    if (length(f) < 9) next
    v <- tail(f, 8)
    if (!all(grepl("^(-?[0-9][0-9,]*\\.?[0-9]*|na|-)$", v))) next
    nm <- trimws(paste(head(f, -8), collapse = " "))
    rows[[length(rows) + 1]] <- data.frame(
      Geography = geo, Table = tbl, Indicator = nm,
      Value = num(v[1]), SE = num(v[2]), N_unweighted = num(v[3]), N_weighted = num(v[4]),
      DEFT = num(v[5]), Rel_error = num(v[6]), Lower = num(v[7]), Upper = num(v[8]))
  }
}
d <- do.call(rbind, rows)
d$Indicator <- gsub("–|—", "-", d$Indicator)
d$Indicator <- gsub("DPT-HepB\\+Hib", "DPT-HepB-Hib", d$Indicator)
d$Indicator <- gsub("Condom use at last sex \\(women 12-24\\)", "Condom use at last sex (women 15-24)", d$Indicator)

fmt <- function(x) x |> transform(Value_pct = round(Value * 100, 1), Lower_pct = round(Lower * 100, 1),
                                 Upper_pct = round(Upper * 100, 1),
                                 Coverage_95CI = sprintf("%.1f (%.1f-%.1f)", Value * 100, Lower * 100, Upper * 100))

vax12 <- c("Ever had a vaccination card", "Received BCG vaccination", "Received DPT-HepB-Hib vaccination (3 doses)", "Received pneumococcal vaccination (3 doses)", "Received measles vaccination", "Fully vaccinated according to national schedule (12-23 months)")  # children 12-23 months
vax24 <- c("Received measles/measles-rubella 2 vaccination (24-35 months)",
           "Fully vaccinated according to national schedule (24-35 months)")
mat <- c("Received ANC from a skilled provider", "4+ ANC visits", "8+ ANC visits",
         "Took any iron-containing supplements", "Mothers protected against tetanus for last birth",
         "Received 3+ doses of SP/Fansidar", "Delivered in a health facility (live births)",
         "Delivered by a skilled provider (live births)", "Delivered by C-section (live births)",
         "Women with postnatal check during first 2 days", "Newborns with postnatal check during first 2 days")

pick <- function(inds) { x <- fmt(d[d$Indicator %in% inds, ]); x$Indicator <- factor(x$Indicator, inds)
  x <- x[order(match(x$Geography, unique(d$Geography)), x$Indicator), ]; x$Indicator <- as.character(x$Indicator)
  x[, c("Geography","Table","Indicator","Value_pct","Lower_pct","Upper_pct","Coverage_95CI","SE","N_unweighted","N_weighted","DEFT")] }
t1 <- pick(vax12); t2 <- pick(vax24); t3 <- pick(mat)

wide <- function(x) { w <- reshape(x[, c("Geography","Indicator","Coverage_95CI")], idvar = "Geography",
  timevar = "Indicator", direction = "wide"); names(w) <- sub("Coverage_95CI\\.", "", names(w)); rownames(w) <- NULL; w }

wb <- createWorkbook()
add <- function(nm, x) { addWorksheet(wb, nm); writeData(wb, nm, x); setColWidths(wb, nm, 1:ncol(x), "auto")
  addStyle(wb, nm, createStyle(textDecoration = "bold", fgFill = "#DDEBF7"), 1, 1:ncol(x), gridExpand = TRUE); freezePane(wb, nm, firstRow = TRUE) }
add("1_Vacc_12-23m_long", t1); add("1_Vacc_12-23m_wide", wide(t1))
add("2_Vacc_24-35m_long", t2); add("2_Vacc_24-35m_wide", wide(t2))
add("3_Maternal_long", t3); add("3_Maternal_wide", wide(t3))
saveWorkbook(wb, "MKDHS_2025-26_vaccination_and_maternal_by_county.xlsx", overwrite = TRUE)
write.csv(d, "MKDHS_2025-26_appendixB_all_indicators.csv", row.names = FALSE)

cat("Geographies parsed:", length(unique(d$Geography)), "\n")
for (x in list(t1,t2,t3)) print(table(table(x$Geography))); print(wide(t3)[c(1,4,50), ])
