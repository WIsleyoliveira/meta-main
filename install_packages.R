# One-time install of the packages the app needs.
pkgs <- c("shiny", "bslib", "DT", "meta", "metafor", "svglite", "ragg",
          "survival", "IPDfromKM",   # survival and IPDfromKM are for Kaplan-Meier mode
          "estmeansd")   # skewed-data methods in the median-to-mean calculator
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
cat("meta version:", as.character(packageVersion("meta")), "(8.0 or later is required)\n")
