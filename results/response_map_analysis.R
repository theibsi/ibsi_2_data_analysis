n_tests <- 36

require("ggplot2")
require("RNifti")
require("data.table")

export_folder <- "./export"

# Plotting options -------------------------------------------------------------
plot_theme <- ggplot2::theme_light(base_size=10)
plot_theme$panel.background <- ggplot2::element_blank()
plot_theme$plot.background <- ggplot2::element_blank()
plot_theme$strip.background <- ggplot2::element_blank()
plot_theme$strip.text$colour <- NULL
plot_theme$title <- ggplot2::element_text(
  hjust=0.5,
  size=ggplot2::rel(0.8))

.plot_breaks <- c("0.01", "1E-3", "1E-5", "1E-8", "lt1E-8")
.plot_labels <- c(
  expression(delta[m] >= 0.01),
  expression(paste(10^{-3} <= delta[m], phantom() < 0.01)),
  expression(paste(10^{-5} <= delta[m], phantom() < 10^{-3})),
  expression(paste(10^{-5} <= delta[m], phantom() < 10^{-8})),
  expression(delta[m] < 10^{-8})
)
.plot_colours <- c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2")

.delta_rates <- c(1E-5, 1E-4, 1E-3, 2E-3, 5E-3, 1E-2, 2E-2, 5E-2, 1E-1)
.delta_rates_labels <- c(
  expression(10^{-5}),
  expression(10^{-4}),
  expression(0.001),
  expression(0.002),
  expression(0.005),
  expression(0.01),
  expression(0.02),
  expression(0.05),
  expression(0.10)
)


# Test minimal achievable max deviation ----------------------------------------

data_file <- file.path(".", "phase_1_response_maps", "minimum_delta.RDS")
if (!file.exists(data_file)) {
  
  final_response_map_dir <- "./phase_1_response_maps/final"
  
  .compute_min_pass_rates <- function(config_id, main_dir, delta_rate) {
    
    # Load all Nifti-files in the directory.
    nifti_files <- list.files(
      path = file.path(main_dir, config_id),
      pattern = "nii",
      full.names = TRUE,
      recursive = FALSE
    )
    
    nifti_images <- lapply(nifti_files, RNifti::readNifti)
    n_total_images <- length(nifti_files)
    
    reference_response_map_found <- FALSE
    
    # Enforce at least moderate consensus and majority consensus.
    while(!reference_response_map_found && length(nifti_images) > 3 && length(nifti_images) > ceiling(0.5 * n_total_images)) {
      
      # Compute average response map.
      tentative_reference <- Reduce("+", nifti_images) / length(nifti_images)
      intensity_range <- max(tentative_reference) - min(tentative_reference)
      
      info <- lapply(
        nifti_images,
        function(nifti_image, tentative_reference, delta_rate) {
          voxel_wise_difference <- abs(nifti_image - tentative_reference)
          
          return(data.table(
            "min_delta_rate" = max(voxel_wise_difference / intensity_range),
            "distance" = sum(voxel_wise_difference),
            "all_voxel_pass" = all(voxel_wise_difference <= delta_rate * intensity_range)
          ))
        },
        tentative_reference = tentative_reference,
        delta_rate = delta_rate
      )
      
      info <- data.table::rbindlist(info)
      
      if (!all(info$all_voxel_pass)) {
        nifti_images <- nifti_images[-c(which.max(info$distance))]
        
      } else {
        reference_response_map_found <- TRUE
      }
    }
    
    if (reference_response_map_found) {
      return(data.table::data.table(
        "config_id" = config_id,
        "delta_rate" = delta_rate,
        "intensity_range" = intensity_range,
        "min_delta_rate" = max(info$min_delta_rate),
        "n_matching" = nrow(info),
        "n_total" = n_total_images
      ))
      
    } else {
      return(data.table::data.table(
        "config_id" = config_id,
        "delta_rate" = delta_rate,
        "intensity_range" = NA_real_,
        "min_delta_rate" = NA_real_,
        "n_matching" = NA_integer_,
        "n_total" = n_total_images
      ))
    }
  }
  
  configuration_ids <- list.dirs(
    path = final_response_map_dir, 
    full.names = FALSE, 
    recursive = FALSE
  )
  
  info <- lapply(
    .delta_rates,
    function(delta_rate, configuration_ids, main_dir) {
      
      info <- lapply(
        configuration_ids,
        .compute_min_pass_rates,
        main_dir = main_dir,
        delta_rate = delta_rate
      )
      
      return(data.table::rbindlist(info))
    },
    configuration_ids = configuration_ids,
    main_dir = final_response_map_dir
  )
  
  info <- data.table::rbindlist(info)
  
  saveRDS(info, file = data_file)
  
}

info <- readRDS(data_file)

info[is.finite(min_delta_rate), "min_delta_rate_category" := "0.01"]
info[is.finite(min_delta_rate) & min_delta_rate < 0.01, "min_delta_rate_category" := "1E-3"]
info[is.finite(min_delta_rate) & min_delta_rate < 1E-3, "min_delta_rate_category" := "1E-5"]
info[is.finite(min_delta_rate) & min_delta_rate < 1E-5, "min_delta_rate_category" := "1E-8"]
info[is.finite(min_delta_rate) & min_delta_rate < 1E-8, "min_delta_rate_category" := "lt1E-8"]
info$min_delta_rate_category <- factor(
  x = info$min_delta_rate_category,
  levels = .plot_breaks
)

info$delta_rate_cat <- factor(
  x = info$delta_rate,
  levels = .delta_rates
)

info$config_id <- factor(
  x = info$config_id,
  levels = c(
    "1.a.1", "1.a.2", "1.a.3", "1.a.4", "1.b.1", "2.a", "2.b", "2.c",
    "3.a.1", "3.a.2", "3.a.3", "3.b.1", "3.b.2", "3.b.3", "3.c.1", "3.c.2",
    "3.c.3", "4.a.1", "4.a.2", "4.b.1", "4.b.2", "5.a.1", "5.a.2", "6.a.1",
    "6.a.2", "7.a.1", "7.a.2", "8.a.1", "8.a.2", "8.a.3", "9.a", "9.b.1",
    "9.b.2", "10.a", "10.b.1", "10.b.2"
  )
)

info[, "min_delta_rate_plot" := min_delta_rate]
info[min_delta_rate < 1E-8, "min_delta_rate_plot" := 1E-8]

info[, "figure_id" := 1L]
info[config_id %in% levels(config_id)[19:36], "figure_id" := 2]

info[, ":="(
  "x_min" = as.numeric(delta_rate_cat) - 0.45,
  "x_max" = as.numeric(delta_rate_cat) + 0.45,
  "y_min" = 1E-9,
  "y_max" = min_delta_rate_plot
)]

for (data in split(info, by = "figure_id")) {
  
  p <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      # x = delta_rate,
      # y = min_delta_rate_plot,
      xmin = x_min,
      xmax = x_max,
      ymin = y_min,
      ymax = y_max,
      fill = min_delta_rate_category
    )
  )
  p <- p + plot_theme
  p <- p + ggplot2::geom_rect()
  p <- p + ggplot2::geom_point(
    data = data[delta_rate_cat != "0.01"],
    mapping = ggplot2::aes(
      x = as.numeric(delta_rate_cat),
      y = delta_rate
    ),
    colour = "grey40",
    show.legend = FALSE
  )
  p <- p + ggplot2::geom_point(
    data = data[delta_rate_cat == "0.01"],
    mapping = ggplot2::aes(
      x = as.numeric(delta_rate_cat),
      y = delta_rate
    ),
    colour = "#F21A00",
    show.legend = FALSE
  )
  p <- p + ggplot2::facet_wrap("config_id", ncol = 3)
  p <- p + ggplot2::scale_y_continuous(
    name = expression(delta[m]),
    trans = "log10",
    limits = c(1E-9, 1E-1) 
  )
  p <- p + ggplot2::scale_x_continuous(
    name = expression(delta),
    breaks = seq_along(.delta_rates),
    labels = .delta_rates_labels
  )
  p <- p + ggplot2::scale_fill_manual(
    name = expression(paste(delta[m], phantom(0), plain(category))),
    breaks = .plot_breaks,
    labels = .plot_labels,
    values = .plot_colours,
    drop = FALSE,
    na.translate = FALSE)
  
  p <- p + ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 40, vjust = 0.75)
  )
  
  ggplot2::ggsave(
    filename = file.path(export_folder, paste0("suppl_figure_min_deviation_", data$figure_id[1],  ".png")),
    plot = p,
    scale = 1,
    width = 16.0,
    height = 20.0,
    units = "cm",
    device = "png"
  )
}

# Export minimal maximum deviation values --------------------------------------

.set_value <- function(x, digits=3L){
  
  if (is.na(x)) return(NA_character_)
  
  decimal_base <- 0L
  if (x != 0.0) {
    decimal_base <- as.integer(floor(log10(abs(x))))
  }
  
  decimal_base <- decimal_base - digits + 1
  
  x <- 10^decimal_base * ceiling(x/10^decimal_base)
  
  return(format(x, scientific = -100, digits = digits))
}

data <- data.table::copy(info)[delta_rate == 0.01]
data[, "min_delta_rate_trunc" := .set_value(min_delta_rate), by = config_id]
data[, "val_delta_rate_trunc" := .set_value(intensity_range * as.numeric(min_delta_rate_trunc)), by = config_id]

data.table::fwrite(
  x = data[order(config_id), mget(c("config_id", "min_delta_rate_trunc", "val_delta_rate_trunc"))],
  file = file.path(export_folder, paste0("suppl_table_min_deviation.csv"))
)
