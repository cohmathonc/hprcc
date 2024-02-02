## Integration test for the hprcc package
library(targets)
print(sapply(names(options())[grep("hprcc.*", names(options()))], function(x) getOption(x)))

# old <- getwd()
# dir <- paste0(old, "/targets_temp_dir")
# dir.create(dir, showWarnings = FALSE)
# setwd(dir)

# tar_config_set(store = paste0(dir, "/_targets"))

unlink("_targets", recursive = TRUE)
unlink("_targets.R", recursive = TRUE)
tar_script(
    {
        library(hprcc)
        #devtools::load_all()
      list(
            tar_target(y1, 1 + 1, deployment = "main"),
            tar_target(y2, 1 + 1, resources = tiny)
          #  tar_target(z, y1 + y2, resources = small)
        )
    },
    ask = FALSE
)

tar_make()

# setwd(old)
# unlink(dir, recursive = TRUE)
