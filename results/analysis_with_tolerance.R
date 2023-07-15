phase_1_analysis_folder <- "./phase_1"
phase_2_analysis_folder <- "./phase_2"
phase_3_analysis_folder <- "./phase_3"
tolerance_folder <- "./phase_2/20220922_perturbations"
export_folder <- "./export"

plot_type <- "png"

require(data.table)
require(ggplot2)

# Non-standard evaluation of variables in data.table.
filter_id <- team <- category <- image_type <- value <- has_contributed <- submission_date <- feature <- NULL
n <- n_teams <- n_match <- majority_match <- match_category <- date_id <- NULL

#### Plotting options ----------------------------------------------------------
plot_theme <- ggplot2::theme_light(base_size=10)
plot_theme$panel.background <- ggplot2::element_blank()
plot_theme$plot.background <- ggplot2::element_blank()
plot_theme$strip.background <- ggplot2::element_blank()
plot_theme$strip.text$colour <- NULL
plot_theme$title <- ggplot2::element_text(
  hjust=0.5,
  size=ggplot2::rel(0.8))

.plot_breaks <- c("none", "weak", "moderate", "strong", "very strong")
.plot_colours <- c("#F21A00", "#E1AF00", "#EBCC2A", "#78B7C5", "#3B9AB2")

.plot_repro_breaks <- c("unknown", "poor", "moderate", "good", "excellent")
.plot_repro_colours <- c("#A6A6A6", "#F21A00", "#EBCC2A", "#78B7C5", "#3B9AB2")

#### Auxiliary functions -------------------------------------------------------

.compute_icc <- function(data, type="2"){
  
  # We start from the following equation: xij = mu + a_i + b_j + e_ij, with mu
  # the population mean, a_i the rater-dependent change, b_j the
  # subject-dependent change and eij an error with mean 0.
  #
  # * type = "1": ICC 1: We assume that each rated value is cased by a random
  # effect of the rater: i.e. the actual rotation is not associated with an
  # systematic change in value, and there is no interaction between rater and
  # subject. [Shrout 1979]. This means that a_i has mean 0, so that wij = ai +
  # eij. Put differently, raters are randomly chosen from a larger population
  # for each sample.
  #
  # * type = "2": ICC 2: There is a panel of raters, and the entire panel
  # evaluates each sample. However, the panel is assumed to be part of a larger
  # population of raters. Raters are assumed to be systematically biased.
  # 
  # * type = "3": ICC 3: There is a panel of raters and the entire panel evaluates each
  # subject. The panel is the entire population and raters are assumed to be
  # systematically biased.
  #
  # Original code from the familiar R package.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- mu <- bj <- ai <- NULL
  
  # Extract general data.
  filter_id <- data$filter_id[1]
  feature <- data$feature[1]
  modality <- data$modality[1]
  filter_descriptor <- data$filter_descriptor[1]
  
  # Determine identifier columns.
  sample_id_columns <- "sample_id"
  repetition_id_column <- "team"
  
  # Remove invalid entries.
  data <- data[is.finite(value)]
  
  if(nrow(data) == 0){
    return(data.table::data.table(
      "filter_id" = filter_id,
      "feature" = feature,
      "modality" = modality,
      "filter_descriptor" = filter_descriptor,
      "n_raters" = 0L,
      "icc" = NA_real_,
      "icc_low" = NA_real_,
      "icc_up" = NA_real_))
  }
  
  # Calculate samples and raters.
  n_samples <- data.table::uniqueN(data, by=sample_id_columns)
  n_raters <- data.table::uniqueN(data, by=repetition_id_column)
  
  if(n_raters <= 2){
    return(data.table::data.table(
      "filter_id" = filter_id,
      "feature" = feature,
      "modality" = modality,
      "filter_descriptor" = filter_descriptor,
      "n_raters" = n_raters,
      "icc" = NA_real_,
      "icc_low" = NA_real_,
      "icc_up" = NA_real_))
  }
  
  if(all(data$value == data$value[1])){
    return(data.table::data.table(
      "filter_id" = filter_id,
      "feature" = feature,
      "modality" = modality,
      "filter_descriptor" = filter_descriptor,
      "n_raters" = n_raters,
      "icc" = 1.0,
      "icc_low" = 1.0,
      "icc_up" = 1.0))
  }
  
  # Transform using robust, shift-sensitive Yeo-Johnson transformation.
  # Manuscript in preparation.
  # data[, "value":=power.transform::power_transform(x=value)]
  
  # Calculate each parameter in the ICC equation.
  data[,"mu":=mean(value, na.rm=TRUE)]
  data[,"bj":=mean(value, na.rm=TRUE)-mu, by=sample_id_columns]
  data[,"ai":=mean(value, na.rm=TRUE)-mu, by=repetition_id_column]
  data[,"eij":=value-mu-bj-ai]
  
  # Calculate mean squared errors: msb between subjects (bj), msj between raters
  # (ai), mse of error (eij) and msw of error with rater (ai + eij).
  if(n_samples > 1){
    msb <- sum(data$bj^2, na.rm=TRUE) / (n_samples-1)
  }
  
  if(type=="1"){
    # Calculate mean squared of error with rater
    msw <- (sum(data$eij^2, na.rm=TRUE) + sum(data$ai^2, na.rm=TRUE)) / (n_samples * (n_raters-1))
    
    # Calculate icc for individual rater and rater panel
    if(msb==0 & msw==0) {
      icc <- icc_panel <- icc_ci_low <- icc_ci_up <- icc_panel_ci_low <- icc_panel_ci_up <- 1
      
    } else {
      icc <- (msb-msw) / (msb+ (n_raters-1) * msw)
      icc_panel <- (msb-msw) / msb
      
      # Fisher score
      s_fisher <- msb/msw
      s_fisher_low <- s_fisher / stats::qf(0.975, n_samples-1, n_samples * (n_raters-1))
      s_fisher_up <- s_fisher / stats::qf(0.025, n_samples-1, n_samples * (n_raters-1))
      
      # Calcuate confidence intervals from fisher score
      icc_ci_low <- (s_fisher_low - 1) / (s_fisher_low + n_raters - 1)
      icc_ci_up <- (s_fisher_up  - 1) / (s_fisher_up  + n_raters - 1)
      icc_panel_ci_low <- 1 - 1/s_fisher_low
      icc_panel_ci_up  <- 1 - 1/s_fisher_up
    }
  }
  
  if(type=="2"){
    # Calculate mean squared error (mse) and mean squared rater error (msj)
    msj <- sum(data$ai^2, na.rm=TRUE) / (n_raters-1)
    mse <- sum(data$eij^2, na.rm=TRUE) / ((n_samples-1) * (n_raters-1))
    
    # Calculate icc for individual rater and rater panel
    if(msb==0 & mse==0) {
      icc <- icc_panel <- icc_ci_low <- icc_ci_up <- icc_panel_ci_low <- icc_panel_ci_up <- 1
      
    } else {
      icc <- (msb-mse) / (msb+ (n_raters-1) * mse + (n_raters/n_samples) * (msj-mse))
      icc_panel <- (msb-mse) / (msb + (msj-mse)/n_samples)
      
      # Determine confidence intervals
      vn <- (n_raters-1)*(n_samples-1) * (n_raters*icc*msj/mse +  n_samples*(1+(n_raters-1)*icc) - n_raters*icc)^2
      vd <- (n_samples-1) * n_raters^2 * icc^2 * (msj/mse)^2   + (n_samples*(1+(n_raters-1)*icc) - n_raters*icc)^2
      v  <- vn/vd
      thresh_low <- stats::qf(0.975, n_samples-1, v)
      thresh_up <- stats::qf(0.025, n_samples-1, v)
      
      # Calcuate confidence intervals from fisher score
      icc_ci_low <- n_samples * (msb - thresh_low*mse) / (thresh_low*(n_raters*msj+(n_raters*n_samples-n_raters-n_samples)*mse) + n_samples*msb)
      icc_ci_up <- n_samples * (msb - thresh_up*mse)  / (thresh_up*(n_raters*msj+(n_raters*n_samples-n_raters-n_samples)*mse)  + n_samples*msb)
      icc_panel_ci_low <- icc_ci_low * n_raters / (1 + icc_ci_low*(n_raters-1) )
      icc_panel_ci_up <- icc_ci_up * n_raters / (1 + icc_ci_up*(n_raters-1) )
    }
  }
  
  if(type=="3"){
    # Calculate mean squared error (mse)
    mse <- sum(data$eij^2, na.rm=TRUE) / ((n_samples-1) * (n_raters-1))
    
    # Calculate icc for individual rater and rater panel
    if(msb==0 & mse==0) {
      icc <- icc_panel <- icc_ci_low <- icc_ci_up <- icc_panel_ci_low <- icc_panel_ci_up <- 1
      
    } else {
      icc <- (msb-mse) / (msb+ (n_raters-1) * mse)
      icc_panel <- (msb-mse) / msb
      
      # Fisher score
      s_fisher <- msb/mse
      s_fisher_low <- s_fisher / stats::qf(0.975, n_samples-1, (n_samples-1) * (n_raters-1))
      s_fisher_up <- s_fisher / stats::qf(0.025, n_samples-1, (n_samples-1) * (n_raters-1))
      
      # Calcuate confidence intervals from fisher score
      icc_ci_low <- (s_fisher_low - 1) / (s_fisher_low + n_raters - 1)
      icc_ci_up <- (s_fisher_up  - 1) / (s_fisher_up  + n_raters - 1)
      icc_panel_ci_low <- 1 - 1/s_fisher_low
      icc_panel_ci_up  <- 1 - 1/s_fisher_up
    }
  }
  
  if(icc == 1.0){
    if(!is.finite(icc_ci_low)) icc_ci_low <- 1.0
    if(!is.finite(icc_ci_up)) icc_ci_up <- 1.0
  }
  
  if(icc_panel == 1.0){
    if(!is.finite(icc_ci_low)) icc_panel_ci_low <- 1.0
    if(!is.finite(icc_ci_up)) icc_panel_ci_up <- 1.0
  }
   
  return(data.table::data.table(
    "filter_id" = filter_id,
    "feature" = feature,
    "modality" = modality,
    "filter_descriptor" = filter_descriptor,
    "n_raters" = n_raters,
    "icc" = icc,
    "icc_low" = icc_ci_low,
    "icc_up" = icc_ci_up))
}


#### Team names ----------------------------------------------------------------
.lookup_team_name <- function(x){
  team_list <- list(
    "LIFEx" = "LIFEx",
    "UdeS" = "UdeS",
    "Veneto Institute of Oncology - IOV IRCCS Padua" = "S-IBEX",
    "Veneto Institute of Oncology" = "S-IBEX",
    "Cardiff University" = "Cardiff University",
    "NCT Dresden" = "MIRP",
    "Qurit SERA" = "Qurit SERA",
    "UPenn" = "UPenn",
    "King's College London" = "KCL",
    "USZ" = "USZ",
    "UCSF" = "UCSF",
    "CERR" = "CERR")
  
  team_name <- team_list[[x]]
  
  if(is.null(team_name)) stop(paste0("Unknown team encountered: ", team_name))
  
  return(team_name)
}

#### Feature names -------------------------------------------------------------
# Intensity statistics features
statistic_features <- data.table::data.table(
  "internal"=c(
    "stat_mean",
    "stat_var",
    "stat_skew",
    "stat_kurt",
    "stat_median",
    "stat_min",
    "stat_p10",
    "stat_p90",
    "stat_max",
    "stat_iqr",
    "stat_range",
    "stat_mad",
    "stat_rmad",
    "stat_medad",
    "stat_cov",
    "stat_qcod",
    "stat_energy",
    "stat_rms"
  ),
  "feature_tag"=c(
    "stat_mean",
    "stat_var",
    "stat_skew",
    "stat_kurt",
    "stat_median",
    "stat_min",
    "stat_p10",
    "stat_p90",
    "stat_max",
    "stat_iqr",
    "stat_range",
    "stat_mad",
    "stat_rmad",
    "stat_medad",
    "stat_cov",
    "stat_qcod",
    "stat_energy",
    "stat_rms"
  ),
  "name"=c(
    "Mean",
    "Variance",
    "Skewness",
    "(Excess) kurtosis",
    "Median",
    "Minimum",
    "10th percentile",
    "90th percentile",
    "Maximum",
    "Interquartile range",
    "Range",
    "Mean absolute deviation",
    "Robust mean absolute deviation",
    "Median absolute deviation",
    "Coefficient of variation",
    "Quartile coefficient of dispersion",
    "Energy",
    "Root mean square"
  ),
  "abbr_name" = c(
    "Mean",
    "Variance",
    "Skewness",
    "Kurtosis",
    "Median",
    "Minimum",
    "10th perc",
    "90th perc",
    "Maximum",
    "IQR",
    "Range",
    "MAD",
    "rMAD",
    "MedAD",
    "CoV",
    "QCoD",
    "energy",
    "RMS"
  )
)


#### Phase 2: Test descriptor data ---------------------------------------------

phase_2_filter_names <- c(
  "1.A", "1.B", "2.A", "2.B", "3.A", "3.B", "4.A", "4.B",
  "5.A", "5.B", "6.A", "6.B", "7.A", "7.B", "8.A", "8.B",
  "9.A", "9.B", "10.A", "10.B", "11.A", "11.B")

phase_2_filter_data <- data.table::data.table("filter_id"=factor(phase_2_filter_names, levels=phase_2_filter_names))

# Add filter descriptor
phase_2_filter_data[startsWith(as.character(filter_id), "1."), "filter_descriptor":="none"]
phase_2_filter_data[startsWith(as.character(filter_id), "2."), "filter_descriptor":="mean"]
phase_2_filter_data[startsWith(as.character(filter_id), "3."), "filter_descriptor":="LoG"]
phase_2_filter_data[startsWith(as.character(filter_id), "4."), "filter_descriptor":="Laws"]
phase_2_filter_data[startsWith(as.character(filter_id), "5."), "filter_descriptor":="Gabor"]
phase_2_filter_data[startsWith(as.character(filter_id), "6."), "filter_descriptor":="sep. wavelet"]
phase_2_filter_data[startsWith(as.character(filter_id), "7."), "filter_descriptor":="decomp. sep. wavelet"]
phase_2_filter_data[startsWith(as.character(filter_id), "8."), "filter_descriptor":="non-sep. wavelet"]
phase_2_filter_data[startsWith(as.character(filter_id), "9."), "filter_descriptor":="decomp. non-sep. wavelet"]
phase_2_filter_data[startsWith(as.character(filter_id), "10."), "filter_descriptor":="Riesz transform"]
phase_2_filter_data[startsWith(as.character(filter_id), "11."), "filter_descriptor":="steered Riesz transform"]

# Add 2d/ 3d label
phase_2_filter_data[endsWith(as.character(filter_id), ".A"), "image_type":="2D"]
phase_2_filter_data[endsWith(as.character(filter_id), ".B"), "image_type":="3D"]


#### Phase 3: Test descriptor data ---------------------------------------------
phase_3_filter_names <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")

phase_3_filter_data <- data.table::data.table("filter_id"=factor(phase_3_filter_names, levels=phase_3_filter_names))

# Add filter descriptor
phase_3_filter_data[startsWith(as.character(filter_id), "1"), "filter_descriptor":="none"]
phase_3_filter_data[startsWith(as.character(filter_id), "2"), "filter_descriptor":="mean"]
phase_3_filter_data[startsWith(as.character(filter_id), "3"), "filter_descriptor":="LoG"]
phase_3_filter_data[startsWith(as.character(filter_id), "4"), "filter_descriptor":="Laws"]
phase_3_filter_data[startsWith(as.character(filter_id), "5"), "filter_descriptor":="Gabor"]
phase_3_filter_data[startsWith(as.character(filter_id), "6"), "filter_descriptor":="sep. wavelet"]
phase_3_filter_data[startsWith(as.character(filter_id), "7"), "filter_descriptor":="decomp. sep. wavelet"]
phase_3_filter_data[startsWith(as.character(filter_id), "8"), "filter_descriptor":="non-sep. wavelet"]
phase_3_filter_data[startsWith(as.character(filter_id), "9"), "filter_descriptor":="decomp. non-sep. wavelet"]


#### Phase 1: Read and parse primary data --------------------------------------
phase_1_data_files <- list.files(phase_1_analysis_folder, pattern=".csv")
phase_1_data_list <- list()

for(ii in seq_along(phase_1_data_files)){
  
  # Read csv files
  data <- data.table::fread(file=file.path(phase_1_analysis_folder, phase_1_data_files[ii]))
  
  # Convert to long format.
  data <- data.table::melt(
    data,
    id.vars = NULL,
    measure.vars = c("none", "weak", "moderate", "strong", "very strong"),
    variable.name = "match_category",
    variable.factor = FALSE,
    value.name = "n"
  )
  
  # Treat match_category as categorical variable.
  data$match_category <- factor(
    x = data$match_category,
    levels = c("none", "weak", "moderate", "strong", "very strong"),
    labels = c("none", "weak", "moderate", "strong", "very strong"))
  
  # Extract time points from the file names.
  submission_date <- gsub(pattern="consensus_strength_submissions-", x=phase_1_data_files[ii], replacement="", fixed=TRUE)
  submission_date <- gsub(pattern=".csv", x=submission_date, replacement="", fixed=TRUE)
  submission_date <- gsub(pattern="-", x=submission_date, replacement="", fixed=TRUE)
  submission_date <- as.integer(submission_date)
  
  # Add time point.
  data[, "submission_date":=submission_date]
  
  # Add to list.
  phase_1_data_list[[ii]] <- data
}

# Combine all phase_1_data.
phase_1_data <- data.table::rbindlist(phase_1_data_list)

# Remove redundant time-points.
phase_1_data <- phase_1_data[!submission_date %in% c(
  20221216, 20221030, 20220930, 20220830,
  20211031, 20211130, 20220101, 20220228, 20220629,
  20210531, 20210630, 20210731, 20210831)]

phase_1_data[submission_date >= 20210131, "n_teams" := 9L]
phase_1_data[submission_date >= 20210228, "n_teams" := 10L]
phase_1_data[submission_date >= 20210331, "n_teams" := 12L]
phase_1_data[submission_date >= 20210430, "n_teams" := 13L]
phase_1_data[submission_date >= 20210930, "n_teams" := 14L]
phase_1_data[submission_date >= 20220530, "n_teams" := 15L]

# Add date id.
phase_1_data[, "date_id":=.GRP, by="submission_date"]
phase_1_data$date_id <- factor(x=phase_1_data$date_id)

rm(data, ii, phase_1_data_files, phase_1_data_list)

#### Phase 1: Plot consensus over time -----------------------------------------

p_phase_1 <- ggplot2::ggplot(
  data = phase_1_data,
  mapping = ggplot2::aes(
    x=date_id,
    y=n,
    fill=match_category))
p_phase_1 <- p_phase_1 + plot_theme
p_phase_1 <- p_phase_1 + ggplot2::geom_bar(
  stat="identity")
p_phase_1 <- p_phase_1 + ggplot2::scale_fill_manual(
  name="consensus",
  breaks=.plot_breaks,
  values=.plot_colours)
p_phase_1 <- p_phase_1 + ggplot2::geom_text(
  mapping = ggplot2::aes(
    x = date_id,
    label = n_teams),
  colour = plot_theme$axis.text$colour,
  family = plot_theme$text$family,
  fontface = "bold",
  size = plot_theme$text$size * as.numeric(plot_theme$axis.text$size) / 2.845276,
  y = 0.0,
  hjust = 0.5,
  vjust = -0.5
)
p_phase_1 <- p_phase_1 + ggplot2::labs(
  x="analysis time point (arb. unit)",
  y="convolutional filter tests")


#### Phase 2: Read and parse primary data --------------------------------------
phase_2_data_files <- list.files(phase_2_analysis_folder, pattern=".csv")
phase_2_data_list <- list()

for(ii in seq_along(phase_2_data_files)){
  
  # Read csv files
  data <- data.table::fread(file=file.path(phase_2_analysis_folder, phase_2_data_files[ii]))
  
  # Rename columns.
  data.table::setnames(
    data,
    old=c("Filter ID", "Metric Category", "Metric Name"),
    new=c("filter_id", "category", "feature"))
  
  # Create long format.
  data <- data.table::melt(
    data,
    id.vars=c("filter_id", "category", "feature"),
    variable.name=c("team"),
    variable.factor = FALSE,
    value.name="value")
  
  # Look up team name.
  data[, "team":=.lookup_team_name(team), by="team"]
  
  # Join with phase_2_filter_data
  data <- data[phase_2_filter_data, on="filter_id"]
  data$filter_id <- factor(data$filter_id, levels=phase_2_filter_names)
  
  # Add submission date.
  data[, "submission_date":=as.integer(tools::file_path_sans_ext(basename(phase_2_data_files[ii])))]
  
  # Add to list.
  phase_2_data_list[[ii]] <- data
}

# Combine all phase_2_data.
phase_2_data <- data.table::rbindlist(phase_2_data_list)

rm(data, ii, phase_2_data_files, phase_2_data_list)

#### Phase 2: Number of contributing teams per filter --------------------------

# Select teams that contributed at least one value.
contribution_data <- unique(data.table::copy(phase_2_data[category == "Statistics", c("filter_id", "team", "value", "submission_date")]))
contribution_data <- contribution_data[, list("has_contributed"=any(is.finite(value))), by=c("filter_id", "team", "submission_date")]
contribution_data <- contribution_data[, list("n"=sum(has_contributed)), by=c("filter_id", "submission_date")]
contribution_data <- contribution_data[phase_2_filter_data, on="filter_id"][order(submission_date)]

# # Construct plot
# p <- ggplot2::ggplot(data=contribution_data, mapping=ggplot2::aes(
#   x=filter_id,
#   y=n,
#   fill=image_type))
# p <- p + ggplot2::geom_histogram(stat="identity")
# p <- p + ggplot2::theme_light()
# p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, hjust=1, vjust=0.5))
# p <- p + ggplot2::scale_x_discrete(
#   name="filter test",
#   labels=paste0(
#     phase_2_filter_data$filter_descriptor, " (",
#     phase_2_filter_data$image_type, ") ",
#     as.character(phase_2_filter_data$filter_id)))
# p <- p + ggplot2::scale_fill_discrete(name="volume", type=hcl.colors(n=2,palette="Zissou 1"))
# p <- p + ggplot2::ylab("n teams")
# p
# 
# # Write to file.
# ggplot2::ggsave(
#   filename=file.path(export_folder, "n-teams.svg"),
#   plot=p,
#   width=22,
#   height=15,
#   units="cm")

#### Phase 2: Read and parse tolerance data ------------------------------------

# Function to select statistical features.
select_feature_columns <- function(x, column_names){
  filter_feature <- grepl(pattern=paste0("_", x), x=column_names)
  if(any(filter_feature)) return(column_names[filter_feature])
  
  filter_feature <- grepl(pattern=x, x=column_names)
  if(any(filter_feature)) return(column_names[filter_feature])
}

# Replace feature name.
.replace_feature_name <- function(x){
  # Non-standard evaluation of variables in data.table.
  internal <- NULL
  
  return(statistic_features[internal == x]$name)
}

# Find CSV files with features.
phase_2_tolerance_files <- list.files(
  path=tolerance_folder,
  pattern="*_features.csv",
  full.names=TRUE
)

# Collect tolerance data.
tolerance_data <- lapply(phase_2_tolerance_files, function(csv_file){
  # Read CSV file.
  data <- data.table::fread(csv_file)
  
  # Find the name of the statistics features that should be obtained. The first
  # priority is those based on response maps.
  selected_stat_features <- sapply(
    statistic_features$internal,
    select_feature_columns,
    column_names=colnames(data))
  
  # Remove unused features.
  data <- data[, mget(c("img_data_settings_id", selected_stat_features))]
  
  # Rename any intensity features based on response maps.
  data.table::setnames(
    data,
    old=selected_stat_features,
    new=names(selected_stat_features))
  
  return(data)
})

# Merge to single table
tolerance_data <- data.table::rbindlist(tolerance_data, use.names=TRUE)

# Experiments as columns. First melt then cast.
tolerance_data <- data.table::melt(
  tolerance_data,
  id.vars="img_data_settings_id",
  variable.name="feature",
  value.name="value")

# Compute tolerance for each filter and feature.
tolerance_data <- tolerance_data[, list("tolerance"=0.10 * stats::IQR(value)), by=c("img_data_settings_id", "feature")]

# Rename img_data_settings_id to filter_id
data.table::setnames(
  tolerance_data,
  old="img_data_settings_id",
  new="filter_id")

# Update feature to use the actual names.
tolerance_data[, "feature":=.replace_feature_name(feature), by="feature"]

rm(phase_2_tolerance_files, select_feature_columns, .replace_feature_name)

#### Phase 2: Consensus --------------------------------------------------------
.find_common_decimal_base <- function(x){
  # Non-standard evaluation of variables in data.table.
  decimal_base <- NULL
  
  x_decimal_base <- integer(length(x))
  
  if(any(x != 0.0)){
    x_decimal_base[x != 0.0] <- as.integer(floor(log10(abs(x[x != 0.0]))))
  }
  
  # Find common base.
  data <- data.table::data.table("decimal_base"=x_decimal_base)[, list("n"=.N), by="decimal_base"][order(-n, decimal_base)]
  
  return(head(data, n=1)$decimal_base)
}

.set_significant_value <- function(x, decimal_base, digits=3L){
  x <- 10^decimal_base * signif(x/10^decimal_base, digits=digits)
}

.set_significant_tolerance <- function(x, decimal_base, digits=3L){
  # Round to 2 significant digits.
  x <- round(x/10^decimal_base, digits=digits-1)
  
  # The tolerance can not be 0, and is set to the next smallest value instead.
  x[x == 0.0] <- 1.0 * 10^(-digits + 1)
  
  # Convert back to normal base.
  x <- x * 10^decimal_base
  
  return(x)
}

.find_consensus_value <- function(x, dx){
  
  # Non-standard evaluation of variables in data.table.
  n_matching <- n <- NULL
  
  # If tolerance is NA, set to 0.
  if(all(is.na(dx))) dx <- 0.0
  
  # Count the number of values that are the same.
  data <- data.table::data.table("value"=x)[, list("n"=.N), by="value"]
  
  # Find the number of matching values that are found within tolerance for each
  # unique value.
  data[, "n_matching":=sum(x <= value + dx & x >= value - dx), by="value"]
  
  # Order by number of matches, and if equal the number of exactly matching
  # values.
  data <- data[order(-n_matching, -n)]
  
  return(list(
    "consensus_value"=head(data, n=1L)$value,
    "n_matching"=head(data, n=1L)$n_matching))
}

# Non-standard evaluation of variables in data.table.
decimal_base <- common_decimal_base <- tolerance <- consensus_value <- NULL

# Make local copy of data, and keep only contributed values.
consensus_data <- data.table::copy(phase_2_data[is.finite(value) & category == "Statistics"])

# Determine the common decimal base of each value.
consensus_data[, "common_decimal_base":=.find_common_decimal_base(x=value), by=c("feature", "filter_id")]

# Merge with tolerance data
consensus_data <- merge(
  x=consensus_data,
  y=tolerance_data,
  all.x=TRUE,
  all.y=FALSE,
  by=c("filter_id", "feature")
)

# Round to 3 significant values.
consensus_data[, "value":=.set_significant_value(
  x=value,
  decimal_base=common_decimal_base)]

# Round to tolerance to 3 significant values.
consensus_data[is.finite(tolerance), "tolerance":=.set_significant_tolerance(
  x=tolerance,
  decimal_base=common_decimal_base)]

# Find consensus value and the number of matches.
consensus_data[, c("consensus_value", "n_match"):=.find_consensus_value(
  x=value,
  dx=tolerance),
  by=c("filter_id", "feature", "submission_date")]

# Extract phase-2 data.
phase_2_team_data <- data.table::copy(consensus_data)

# Count both total number of contributing teams.
consensus_data[, "n_teams":=.N, by=c("filter_id", "feature", "submission_date")]

# Remove team-level data.
consensus_data <- consensus_data[, list(
  "consensus_value"=max(consensus_value),
  "tolerance"=max(tolerance),
  "n_match"=max(n_match),
  "n_teams"=max(n_teams)),
  by=c("filter_id", "feature", "submission_date")]

# Check majority matches > 50%
consensus_data[, "majority_match":=n_match/n_teams > 0.5]

# Create consensus categories
consensus_data[data.table::between(n_match, 0, 2), "match_category":="weak"]
consensus_data[data.table::between(n_match, 3, 5), "match_category":="moderate"]
consensus_data[data.table::between(n_match, 6, 9), "match_category":="strong"]
consensus_data[data.table::between(n_match, 10, Inf), "match_category":="very strong"]

# Override categories that lack a majority.
consensus_data[majority_match == FALSE, "match_category":="none"]

# Parse to an ordinal form.
consensus_data$match_category <- ordered(
  consensus_data$match_category,
  levels=c("none", "weak", "moderate", "strong", "very strong"))

# Compute the number of features and the number of filters.
n_features <- data.table::uniqueN(phase_2_data[category == "Statistics"]$feature)

# Ensure that the filter id is categorical to allow for correct plotting.
consensus_data$filter_id <- factor(
  x=consensus_data$filter_id,
  levels=phase_2_filter_names)

# Compute consensus fractions.
per_filter_consensus_data <- consensus_data[, list("n"=.N), by=c("filter_id", "match_category", "submission_date")]

# Add in filter descriptor info.
per_filter_consensus_data <- per_filter_consensus_data[phase_2_filter_data, on="filter_id"][order(submission_date)]

# Fix tests for which no values have been contributed altogether.
per_filter_consensus_data[is.na(match_category), ":="("n"= n_features, "match_category"="none")]

# Find unique submission dates.
unique_submission_dates <- unique(per_filter_consensus_data$submission_date)
unique_submission_dates <- unique_submission_dates[is.finite(unique_submission_dates)]

# Update filters for which dates are missing, notably 11.A and 11.B.
per_filter_consensus_data <- lapply(
  split(per_filter_consensus_data, by="filter_id"),
  function(x, dates){
    if(all(dates %in% x$submission_date)) return(x)
    
    missing_dates <- setdiff(dates, submission_date)
    if(length(missing_dates) == length(dates)){
      x <- data.table::copy(x[rep(1L, length(dates))])
      x[, "submission_date":=dates]
      
    } else {
      stop()
    }
    
    return(x)
  },
  dates = unique_submission_dates)

per_filter_consensus_data <- data.table::rbindlist(per_filter_consensus_data)

# Deep-copy to avoid updating by reference.
aggregated_consensus_data <- data.table::copy(per_filter_consensus_data)
aggregated_consensus_data <- aggregated_consensus_data[, list("n"=sum(n)), by=c("match_category", "submission_date")][order(submission_date)]

# Add percentages and date id to the aggregated table.
aggregated_consensus_data[, "pct":=n / (n_features * length(phase_2_filter_names)) * 100]
aggregated_consensus_data[, "date_id":=.GRP, by="submission_date"]
aggregated_consensus_data$date_id <- factor(x=aggregated_consensus_data$date_id)

aggregated_consensus_data[submission_date >= 20220504, "n_teams" := 5L]
aggregated_consensus_data[submission_date >= 20220630, "n_teams" := 7L]
aggregated_consensus_data[submission_date >= 20221117, "n_teams" := 8L]
aggregated_consensus_data[submission_date >= 20230111, "n_teams" := 11L]


# Add percentages and date id to the per-filter table. Only keep initial and
# final dates.
per_filter_consensus_data[, "pct":=n / n_features * 100]
per_filter_consensus_data <- per_filter_consensus_data[submission_date %in% c(min(per_filter_consensus_data$submission_date), max(per_filter_consensus_data$submission_date))]
per_filter_consensus_data[, "date_id":=.GRP, by="submission_date"]
per_filter_consensus_data$date_id <- factor(
  x = per_filter_consensus_data$date_id,
  levels = c(1, 2),
  labels = c("initial", "final"))

# Generate filter labels
per_filter_consensus_data[, "filter_label":=paste0(filter_id, ": ", filter_descriptor, " (", image_type, ")")]
per_filter_consensus_data$filter_label <- factor(
  x = per_filter_consensus_data$filter_label,
  levels = unique(per_filter_consensus_data$filter_label))

rm(.find_consensus_value, .set_significant_tolerance, .set_significant_value, .find_common_decimal_base)
rm(n_features, unique_submission_dates)

#### Phase 2: Plot consensus over time -----------------------------------------

p_phase_2 <- ggplot2::ggplot(
  data = aggregated_consensus_data,
  mapping = ggplot2::aes(
    x=date_id,
    y=pct,
    fill=match_category))
p_phase_2 <- p_phase_2 + plot_theme
p_phase_2 <- p_phase_2 + ggplot2::geom_bar(
  stat="identity")
p_phase_2 <- p_phase_2 + ggplot2::scale_fill_manual(
  name="consensus",
  breaks=.plot_breaks,
  values=.plot_colours)
p_phase_2 <- p_phase_2 + ggplot2::geom_text(
  mapping = ggplot2::aes(
    x = date_id,
    label = n_teams),
  colour = plot_theme$axis.text$colour,
  family = plot_theme$text$family,
  fontface = "bold",
  size = plot_theme$text$size * as.numeric(plot_theme$axis.text$size) / 2.845276,
  y = 0.0,
  hjust = 0.5,
  vjust = -0.5
)
p_phase_2 <- p_phase_2 + ggplot2::labs(
  x="analysis time point (arb. unit)",
  y="features (%)")


# For the supplement: plot initial and final dates per filter.
p_phase_2_suppl <- ggplot2::ggplot(
  data = per_filter_consensus_data,
  mapping = ggplot2::aes(
    x=date_id,
    y=pct,
    fill=match_category))
p_phase_2_suppl <- p_phase_2_suppl + plot_theme
p_phase_2_suppl <- p_phase_2_suppl + ggplot2::geom_bar(
  stat="identity")
p_phase_2_suppl <- p_phase_2_suppl + ggplot2::scale_fill_manual(
  name="consensus",
  breaks=.plot_breaks,
  values=.plot_colours)
p_phase_2_suppl <- p_phase_2_suppl + ggplot2::labs(
  x="analysis time point",
  y="features (%)")
p_phase_2_suppl <- p_phase_2_suppl + ggplot2::facet_wrap(
  facets = c("filter_label"),
  labeller = ggplot2::label_wrap_gen(),
  ncol = 4L)

if(!is.null(plot_type)){
  ggplot2::ggsave(
    filename = file.path(export_folder, paste0("phase_2_suppl_figure_1.", plot_type)),
    plot = p_phase_2_suppl,
    scale = 1.0,
    width = 16.0,
    height = 20.0,
    units = "cm",
    device = plot_type
  )
}


#### Phase 3: Read and parse primary data --------------------------------------

# Non-standard evaluation of variables in data.table.
missing_filter <- i.valid <- modality <- NULL

phase_3_data_directories <- list.dirs(phase_3_analysis_folder, full.names = FALSE, recursive = FALSE)
phase_3_data_directories <- setdiff(phase_3_data_directories, c("_raw", "_replaced"))

phase_3_data_list <- list()
file_counter <- 1L

for(ii in seq_along(phase_3_data_directories)){
  
  phase_3_data_files <- list.files(
    file.path(phase_3_analysis_folder, phase_3_data_directories[ii]),
    pattern=".csv")
  
  for(jj in seq_along(phase_3_data_files)){
    # Read csv files
    data <- data.table::fread(file=file.path(phase_3_analysis_folder, phase_3_data_directories[ii], phase_3_data_files[jj]))
    
    # Keep only relevant columns.
    data <- data[family == "Statistics", mget(c("feature_tag", paste0("ID_", phase_3_filter_names)))]
    
    # Create long format.
    data <- data.table::melt(
      data,
      id.vars="feature_tag",
      variable.name="filter_id",
      value.name="value")
    
    data[, "missing_filter":=all(value == 0.0), by="filter_id"]
    data[missing_filter == TRUE, "value":=NA_real_]
    data[, "missing_filter":=NULL]
    
    # Rename levels of filter_id.
    data$filter_id <- factor(
      data$filter_id,
      levels=paste0("ID_", phase_3_filter_names),
      labels=phase_3_filter_names)
    
    # Join with phase_3_filter_data
    data <- data[phase_3_filter_data, on="filter_id"]
    
    # Add in team name.
    data[, "team":=.lookup_team_name(phase_3_data_directories[ii])]
    
    # Split file name.
    file_name_deparsed <- strsplit(
      x = tools::file_path_sans_ext(basename(phase_3_data_files[jj])),
      split = "_",
      fixed = TRUE)[[1]]
    
    # Add in sample name.
    data[, "sample_id":=paste0(file_name_deparsed[1], "_", file_name_deparsed[2])]
    
    # Add in modality.
    data[, "modality":=file_name_deparsed[3]]
    
    # Attach to data list.
    phase_3_data_list[[file_counter]] <- data
    
    # Increment file counter.
    file_counter <- file_counter + 1L
  }
}

# Combine all phase_3_data.
phase_3_data <- data.table::rbindlist(phase_3_data_list)
phase_3_data[, "valid":=FALSE]

# Merge with statistic_data
phase_3_data <- phase_3_data[statistic_features[, mget(c("feature_tag", "name"))], on="feature_tag"]
phase_3_data[, "feature_tag":=NULL]
data.table::setnames(phase_3_data, old="name", new="feature")

rm(data, ii, jj, file_counter, file_name_deparsed, phase_3_data_files, phase_3_data_list, phase_3_data_directories)

# Parse phase_2_team_data
phase_2_team_data <- phase_2_team_data[submission_date == max(submission_date) & image_type == "3D"]

# Determine if the submission matches the consensus value.
phase_2_team_data[, "valid":=FALSE]
phase_2_team_data[value <= consensus_value + tolerance & value >= consensus_value - tolerance, "valid":=TRUE]

# Keep only relevant.
phase_2_team_data <- phase_2_team_data[, mget(c("feature", "filter_descriptor", "team", "valid"))]

# Use team data to update validity per filter, feature and team.
phase_3_data[phase_2_team_data, "valid":=i.valid, on=c("feature", "filter_descriptor", "team")]

# Temporary clamps on data until configuration issues are checked.
phase_3_data[team == "S-IBEX" & filter_id %in% c(8, 9), "valid":=FALSE]
phase_3_data[team == "Qurit SERA" & filter_id %in% c(7), "valid":=FALSE]
phase_3_data[team == "Qurit SERA" & modality %in% "CT", "valid":=FALSE]

# Set all values that are not valid to NA.
phase_3_data[valid == FALSE, "value":=NA_real_]

# Compute ICC(2,1) per feature, modality and filter.
phase_3_icc_data <- lapply(
  split(phase_3_data, by=c("feature", "modality", "filter_descriptor")),
  .compute_icc,
  type = 2)

# Combine to list.
phase_3_icc_data <- data.table::rbindlist(phase_3_icc_data)

phase_3_icc_data$modality <- factor(
  x = phase_3_icc_data$modality,
  levels = c("CT", "PET", "MR"),
  labels = c("CT", "FDG-PET", "T1w-MR")
)

# Set reproducibility.
phase_3_icc_data[is.na(icc_low), "reproducibility":="unknown"]
phase_3_icc_data[icc_low < 0.50, "reproducibility":="poor"]
phase_3_icc_data[icc_low < 0.75 & icc_low >= 0.50, "reproducibility":="moderate"]
phase_3_icc_data[icc_low < 0.90 & icc_low >= 0.75, "reproducibility":="good"]
phase_3_icc_data[icc_low >= 0.90, "reproducibility":="excellent"]

phase_3_icc_data$reproducibility <- factor(
  x = phase_3_icc_data$reproducibility,
  levels = c("unknown", "poor", "moderate", "good", "excellent"))

n_features <- data.table::uniqueN(phase_3_data$feature)

# Aggregate per modality.
aggregated_phase_3_data <- phase_3_icc_data[, list("n"=.N), by=c("reproducibility", "modality")]
aggregated_phase_3_data[, "pct":=n / (n_features * length(phase_3_filter_names)) * 100]

# Aggregate per modality and filter_id.
per_filter_phase_3_data <- phase_3_icc_data[, list("n"=.N), by=c("reproducibility", "modality", "filter_id")]
per_filter_phase_3_data[, "pct":=n / n_features * 100]
per_filter_phase_3_data <- per_filter_phase_3_data[phase_3_filter_data, on="filter_id"]

# Generate filter labels
per_filter_phase_3_data[, "filter_label":=paste0(filter_id, ": ", filter_descriptor)]
per_filter_phase_3_data$filter_label <- factor(
  x = per_filter_phase_3_data$filter_label,
  levels = unique(per_filter_phase_3_data$filter_label))

#### Phase 3: Plot reproducibility ---------------------------------------------

p_phase_3 <- ggplot2::ggplot(
  data = aggregated_phase_3_data,
  mapping = ggplot2::aes(
    x=modality,
    y=pct,
    fill=reproducibility))
p_phase_3 <- p_phase_3 + plot_theme
p_phase_3 <- p_phase_3 + ggplot2::geom_bar(
  stat="identity")
p_phase_3 <- p_phase_3 + ggplot2::scale_fill_manual(
  name="reproducibility",
  breaks=.plot_repro_breaks,
  values=.plot_repro_colours,
  drop=FALSE)
p_phase_3 <- p_phase_3 + ggplot2::labs(
  x="imaging modality",
  y="features (%)")

# For the supplement: split reproducibility per filter.
p_phase_3_suppl <- ggplot2::ggplot(
  data = per_filter_phase_3_data,
  mapping = ggplot2::aes(
    x=modality,
    y=pct,
    fill=reproducibility))
p_phase_3_suppl <- p_phase_3_suppl + plot_theme
p_phase_3_suppl <- p_phase_3_suppl + ggplot2::geom_bar(
  stat="identity")
p_phase_3_suppl <- p_phase_3_suppl + ggplot2::scale_fill_manual(
  name="reproducibility",
  breaks=.plot_repro_breaks,
  values=.plot_repro_colours,
  drop=FALSE)
p_phase_3_suppl <- p_phase_3_suppl + ggplot2::labs(
  x="analysis time point",
  y="features (%)")
p_phase_3_suppl <- p_phase_3_suppl + ggplot2::facet_wrap(
  facets = c("filter_label"),
  labeller = ggplot2::label_wrap_gen(),
  ncol = 3L)

if(!is.null(plot_type)){
  ggplot2::ggsave(
    filename = file.path(export_folder, paste0("phase_3_suppl_figure_1.", plot_type)),
    plot = p_phase_3_suppl,
    scale = 1.0,
    width = 16.0,
    height = 20.0,
    units = "cm",
    device = plot_type
  )
}


#### Combine main plots --------------------------------------------------------

p_phase_1 <- p_phase_1 + ggplot2::labs(
  title = "Phase 1: consensus on reference response maps",
  tag = "A.")

p_phase_2 <- p_phase_2 + ggplot2::labs(
  title = "Phase 2: consensus on reference feature values",
  tag = "B.")

p_phase_3 <- p_phase_3 + ggplot2::labs(
  title = "Phase 3: reproducibility of feature values",
  tag = "C.")

p_combined <- egg::ggarrange(
  plots=list(p_phase_1, p_phase_2, p_phase_3),
  nrow=3,
  draw=FALSE)

if(!is.null(plot_type)){
  ggplot2::ggsave(
    filename = file.path(export_folder, paste0("main_figure_1.", plot_type)),
    plot = p_combined,
    scale = 1.0,
    width = 16.0,
    height = 12.0,
    units = "cm",
    device = plot_type
  )
}


# Supplementary Table: poor and moderate reproducibility -----------------------

# Write table with poor and moderate reproducibility.
poor_reproducibility_data <- data.table::copy(phase_3_icc_data[reproducibility %in% c("poor", "moderate")])

# Format ICC and ICC-lower.
poor_reproducibility_data$icc <- format(round(poor_reproducibility_data$icc, digits = 2L))
poor_reproducibility_data$icc_low <- format(round(poor_reproducibility_data$icc_low, digits = 2L))
poor_reproducibility_data$icc_up <- format(round(poor_reproducibility_data$icc_up, digits = 2L))

poor_reproducibility_data[feature == "Coefficient of variation", "feature" := "CoV"]
poor_reproducibility_data[feature == "Quartile coefficient of dispersion", "feature" := "QCoD"]
poor_reproducibility_data[feature == "(Excess) kurtosis", "feature" := "Kurtosis"]

# Drop unimportant columns.
poor_reproducibility_data[, ":="(
  "n_raters" = NULL,
  "filter_id" = NULL,
  "reproducibility" = NULL)]

data.table::fwrite(
  poor_reproducibility_data,
  file = file.path(export_folder, paste0("suppl_table_reproducibility.csv")))

# Supplementary Table: full reproducibility ------------------------------------

icc_table <- data.table::copy(phase_3_icc_data)

# Format ICC into a range.
icc_table[, "icc_range" := paste0(
  trimws(format(round(icc, digits = 2L))),
  " (",
  trimws(format(round(icc_low, digits = 2L))),
  "-",
  trimws(format(round(icc_up, digits = 2L))),
  ")")
]
icc_table[is.na(icc), "icc_range" := "—"]

# Merge in abbreviated feature names.
icc_table <- merge(
  x = icc_table,
  y = statistic_features[, mget(c("name", "abbr_name"))],
  by.x = "feature",
  by.y = "name")
icc_table$abbr_name <- factor(
  x = icc_table$abbr_name,
  levels = statistic_features$abbr_name
)

lapply(
  split(icc_table, by = "filter_id"),
  function(x) {
    data.table::fwrite(
      x = dcast(
        data = x,
        abbr_name ~ modality,
        value.var = "icc_range"
      ),
      file = file.path(export_folder, paste0("suppl_table_full_reproducibility_", head(x$filter_id, n = 1L), ".csv"))
    )
  }
)


# IBSI website data ------------------------------------------------------------
reference_data <- data.table::copy(consensus_data[submission_date == 20230111])
reference_data[
  match_category %in% c("none", "weak") | majority_match == FALSE,
  ":="(
    "consensus_value" = NA_real_,
    "tolerance" = NA_real_
  )
]
reference_data <- merge(
  x = reference_data[, mget(c("filter_id", "feature", "consensus_value", "tolerance"))],
  y = statistic_features[, mget(c("name", "feature_tag"))],
  by.x = "feature",
  by.y = "name",
  all = TRUE
)
reference_data <- reference_data[!filter_id %in% c("10.A", "10.B", "11.A", "11.B")]
reference_data$feature_tag <- factor(
  x = reference_data$feature_tag,
  levels = statistic_features$feature_tag)
reference_data <- reference_data[order(filter_id, feature_tag)]
data.table::setcolorder(reference_data, neworder = c(
  "filter_id", "feature", "feature_tag", "consensus_value", "tolerance"
))

data.table::fwrite(
  x = reference_data,
  file = file.path(export_folder, "ibsi_2_phase_2_reference_values.csv"),
  sep = ";"
)


# Compliance spreadsheet -------------------------------------------------------
reference_data <- data.table::copy(consensus_data[submission_date == 20230111])

reference_data[, "consensus" := match_category]
reference_data$consensus <- factor(
  x = reference_data$consensus,
  levels = c("none", "weak", "moderate", "strong", "very strong"),
  labels = c("not standardized", "< 3", "3-5", "6-9", "≥ 10")
)
reference_data[majority_match == FALSE | match_category <= "weak", "consensus" := "not standardized"]
reference_data[consensus == "not standardized", ":="("consensus_value" = NA_real_, "tolerance" = NA_real_)]

missing_filters <- data.table::copy(reference_data[filter_id %in% c("10.A", "10.B")])
missing_filters[filter_id == "10.A", "filter_id" := "11.A"]
missing_filters[filter_id == "10.B", "filter_id" := "11.B"]
reference_data <- rbind(reference_data, missing_filters)

rm(missing_filters)

reference_data <- merge(
  x = reference_data[, mget(c("filter_id", "feature", "consensus", "consensus_value", "tolerance"))],
  y = phase_2_filter_data[, mget(c("filter_id", "filter_descriptor"))],
  by = "filter_id",
  all = TRUE
)
reference_data <- merge(
  x = reference_data[, mget(c("filter_id", "filter_descriptor", "feature", "consensus", "consensus_value", "tolerance"))],
  y = statistic_features[, mget(c("name", "feature_tag"))],
  by.x = "feature",
  by.y = "name",
  all = TRUE
)
reference_data$feature_tag <- factor(
  x = reference_data$feature_tag,
  levels = statistic_features$feature_tag)
reference_data <- reference_data[order(filter_id, feature_tag)]
data.table::setcolorder(reference_data, neworder = c(
  "filter_id", "filter_descriptor", "feature", "consensus", "consensus_value", "tolerance", "feature_tag"))

data.table::fwrite(
  x = reference_data,
  file = file.path(export_folder, "compliance_spreadsheet.csv"),
  sep = ";",
  na = ""
)

# Reference manual data --------------------------------------------------------

.export_reference_values <- function(data, export_folder){
  
  current_feature_name <- tolower(data$feature[1])
  
  # Define the caption
  caption_text <- paste0("Reference values for the \\textit{", current_feature_name, "} feature.")
  
  # Sanitise the feature name for writing the file.
  file_name <- current_feature_name
  if(current_feature_name == "(excess) kurtosis") file_name <- "kurtosis"
  label_tag <- file_name <- gsub(pattern = " ", replacement = "_", x = file_name, fixed = TRUE)
  
  # Define the file name
  file_name <- file.path(export_folder, "reference_table", paste0(file_name, ".txt"))
  
  # Define a bolding function
  bold <- function(x) paste("{\\textbf{", x, "}}", sep ="")
  
  # Write to file
  con <- file(file_name, open="w")
  
  # Remove redundant columns
  data <- data[, mget(c("filter_id", "consensus_value", "tolerance", "match_category"))]
  
  # Reorder and rename columns
  data.table::setcolorder(data, c("filter_id", "consensus_value", "tolerance", "match_category"))
  
  # Add in 11.A and 11.B.
  data <- rbind(data, data.table::data.table(
      "filter_id" = c("11.A", "11.B"),
      "consensus_value" = c(NA_real_, NA_real_),
      "tolerance" = c(NA_real_, NA_real_),
      "match_category" = c("none", "none")))
  
  data <- data[order(filter_id)]
  data.table::setnames(data, c("filter ID", "value", "tol.", "consensus"))
  
  # To xtable
  x <- xtable::xtable(
    data,
    display = c("d", "s", "g", "g", "s"),
    digits = c(3, 1, 3, 3, 1),
    align = c("l", "c", "c", "c", "c"),
    caption = caption_text)
  
  # File header for spacing and setting font size
  # writeLines("\\vspace{2mm}", con = con)
  # writeLines("\\begin{table}[ht]", con = con)
  writeLines("\\centering", con = con)
  writeLines("\\small", con = con)
  
  # Note that the regular tabular enviroment is used, as these tables are
  # smaller.
  writeLines(
    xtable::print.xtable(
      x = x,
      floating = FALSE,
      tabular.environment = "tabular",
      booktabs = TRUE,
      math.style.negative = TRUE,
      math.style.exponents = TRUE,
      include.rownames = FALSE,
      NA.string="\\textemdash",
      sanitize.colnames.function = bold,
      print.results = FALSE),
    con = con)
  
  writeLines(paste0("\\captionof{table}{", caption_text, "}"), con = con)
  writeLines(paste0("\\label{tab:", label_tag, "}"), con = con)
  # writeLines("\\end{table}", con = con)
  # writeLines("\\FloatBarrier", con = con)
  
  # Close file connection
  close(con)
  
  return(invisible(NULL))
}

reference_data <- data.table::copy(consensus_data[submission_date == 20230111])
reference_data[
  match_category %in% c("none", "weak") | majority_match == FALSE,
  ":="(
    "consensus_value" = NA_real_,
    "tolerance" = NA_real_
  )
]

lapply(
  split(reference_data, by="feature"),
  .export_reference_values,
  export_folder = export_folder
)



# Python test data (MIRP) ------------------------------------------------------

.export_to_python_unit_test <- function(data, export_folder) {
  
  filter_id <- tolower(data$filter_id[1])

  # Define the file name
  file_name <- file.path(export_folder, "unit_test", paste0(filter_id, ".txt"))
  
  # Write to file
  con <- file(file_name, open = "w")
  
  for (ii in seq_len(nrow(data))) {
    
    if (is.na(data[ii, ]$consensus_value)) next
    
    test_str <- paste0(
      "assert(within_tolerance(",
      data[ii, ]$consensus_value,
      ", ",
      data[ii, ]$tolerance,
      ", data[\"",
      data[ii, ]$internal,
      "\"]))"
    )
    
    writeLines(test_str, con = con)
  }
  
  # Close file connection
  close(con)
  
  return(invisible(NULL))
}

lapply(
  split(merge(x = reference_data, y = statistic_features, by.x = "feature", by.y = "name"), by = "filter_id"),
  .export_to_python_unit_test,
  export_folder = export_folder
)
