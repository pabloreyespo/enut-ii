library(tidyverse)
library(haven)

# Function to discretize continuous variables by assigning each observation to a bin
# representing the mid-point with the adjacent observation.
# When the number of unique values exceeds max_bins, falls back to quantile-based
# breakpoints to keep the joint table size manageable.
discretize_midpoints <- function(x, lower_cap = -Inf, upper_cap = Inf, max_bins = 50) {
    # Get sorted unique values (ignoring NAs to avoid errors)
    v <- sort(unique(x[!is.na(x)]))

    # Handle edge cases where variables cannot be split
    if (length(v) == 0) {
        return(x)
    }
    if (length(v) == 1) {
        return(as.factor(x))
    }

    # If too many unique values, reduce to quantile-based breakpoints
    if (length(v) > max_bins) {
        probs <- seq(0, 1, length.out = max_bins + 1)
        quantile_breaks <- unique(quantile(x, probs = probs, na.rm = TRUE))
        # Use inner quantile breaks as the representative unique values
        v <- quantile_breaks[-c(1, length(quantile_breaks))]
    }

    # Calculate midpoints between adjacent unique values
    midpoints <- (v[-1] + v[-length(v)]) / 2

    # Define boundaries for the bins using the provided caps
    breaks <- c(lower_cap, midpoints, upper_cap)

    # Create labels for the bins conditionally based on whether limits are infinite
    labels <- character(length(breaks) - 1)

    labels[1] <- if (is.infinite(lower_cap)) paste0("< ", midpoints[1]) else paste0(lower_cap, "-", midpoints[1])

    if (length(midpoints) > 1) {
        for (i in 1:(length(midpoints) - 1)) {
            labels[i + 1] <- paste0(midpoints[i], "-", midpoints[i + 1])
        }
    }

    labels[length(labels)] <- if (is.infinite(upper_cap)) paste0(midpoints[length(midpoints)], " +") else paste0(midpoints[length(midpoints)], "-", upper_cap)

    # Use cut to discretize: include.lowest ensures the exact matching boundaries don't fall off
    binned_x <- cut(x, breaks = breaks, labels = labels, right = FALSE, include.lowest = TRUE)

    # Attach the breaks as an attribute so we know the range when sampling later
    attr(binned_x, "breaks") <- breaks

    return(binned_x)
}

# Function to sample uniformly assuming uniform probabilities within each bin's range.
sample_from_bins <- function(binned_x) {
    breaks <- attr(binned_x, "breaks")
    if (is.null(breaks)) {
        stop("Input factor has no 'breaks' attribute. Make sure it was created with discretize_midpoints.")
    }

    # Convert factor to integer to get the bin indices (1 to length(levels))
    indices <- as.integer(binned_x)

    # Each index i corresponds to bounds (breaks[i], breaks[i+1])
    lows <- breaks[indices]
    highs <- breaks[indices + 1]

    if (any(is.infinite(lows) | is.infinite(highs), na.rm = TRUE)) {
        warning("Some bins have infinite bounds. You must use finite lower_cap and upper_cap in discretize_midpoints to sample uniformly.")
    }

    # Sample uniformly between limits. vectorized using runif
    sampled <- runif(length(binned_x), min = lows, max = highs)

    # Ensure NAs remain NAs
    sampled[is.na(binned_x)] <- NA

    return(sampled)
}

# Function to compute the joint probability distribution given a data frame and a set of columns
compute_joint_distribution <- function(data, cols) {
    # Check if all specified columns exist in the dataframe
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0) {
        stop(paste("The following columns were not found in the data:", paste(missing_cols, collapse = ", ")))
    }

    # Subset data to only the requested columns
    subset_data <- data[, cols, drop = FALSE]

    # Compute the multidimensional frequency table
    # By keeping the list names, table() automatically names the dimensions (dnn)
    freq_table <- do.call(table, c(as.list(subset_data), useNA = "ifany"))

    # Convert absolute frequencies to proportions (joint probability)
    joint_dist <- prop.table(freq_table)

    return(joint_dist)
}

# Function to compute marginal probabilities simply by passing the Joint Distribution
# and the names of the columns you want to query.
query_marginal_distribution <- function(joint_dist, target_cols) {
    dim_names <- names(dimnames(joint_dist))

    if (is.null(dim_names)) {
        stop("The given distribution doesn't have dimension names. Make sure you used compute_joint_distribution()")
    }

    missing_cols <- setdiff(target_cols, dim_names)
    if (length(missing_cols) > 0) {
        stop(paste("These columns weren't found in your joint distribution:", paste(missing_cols, collapse = ", ")))
    }

    # Find the numerical indices for the target margins
    margins <- match(target_cols, dim_names)

    # Calculate the marginal distribution
    marginal_dist <- margin.table(joint_dist, margin = margins)

    return(marginal_dist)
}

# Function to compute conditional probabilities P(target_cols | given_cols)
query_conditional_distribution <- function(joint_dist, target_cols, given_cols) {
    dim_names <- names(dimnames(joint_dist))

    all_needed <- c(target_cols, given_cols)
    missing_cols <- setdiff(all_needed, dim_names)
    if (length(missing_cols) > 0) {
        stop(paste("Columns not found in joint distribution:", paste(missing_cols, collapse = ", ")))
    }

    # 1. Get the marginal distribution of just the target + given columns
    marginal_subset <- query_marginal_distribution(joint_dist, all_needed)

    # 2. Compute the conditional probabilities by dividing by the margin of given_cols
    given_indices <- match(given_cols, names(dimnames(marginal_subset)))
    conditional_dist <- prop.table(marginal_subset, margin = given_indices)

    return(conditional_dist)
}

# Function to easily convert these multi-dimensional tables into a clean dataframe
table_to_df <- function(dist_table) {
    df <- as.data.frame(as.table(dist_table), stringsAsFactors = FALSE)
    names(df)[names(df) == "Freq"] <- "probability"
    return(df)
}

# Function to query specific probabilities using named arguments
# e.g., get_probability(my_dist, edad_anios = 18, sexo = 1)
get_probability <- function(dist_table, ...) {
    df <- table_to_df(dist_table)
    conditions <- list(...)

    if (length(conditions) == 0) {
        return(df)
    }

    for (col in names(conditions)) {
        if (col %in% names(df)) {
            # Filter matches (handles both single values and vectors)
            df <- df[df[[col]] %in% as.character(conditions[[col]]), ]
        } else {
            stop(paste("Variable '", col, "' not found in this distribution table."))
        }
    }

    # Remove row names for cleaner output
    rownames(df) <- NULL
    return(df)
}

# Like get_probability but samples one joint outcome from multiple target variables
# based on the conditional probability weights — returning a named list.
# sample_cols: character vector of columns to sample jointly (e.g., c("sleep", "paid_work"))
# ...: fixed conditioning values (e.g., edad_anios = 18, sexo = 1)
sample_conditional <- function(dist_table, sample_cols, ...) {
    df <- get_probability(dist_table, ...)

    missing <- setdiff(sample_cols, names(df))
    if (length(missing) > 0) {
        stop(paste("sample_cols not found in distribution:", paste(missing, collapse = ", ")))
    }

    total_prob <- sum(df$probability, na.rm = TRUE)
    if (total_prob == 0) {
        warning("All probabilities are zero for the given conditions. Returning NAs.")
        return(setNames(as.list(rep(NA, length(sample_cols))), sample_cols))
    }

    # Sample one row jointly according to probability weights
    idx <- sample(nrow(df), size = 1, prob = df$probability / total_prob)

    # Return a named list of the sampled values for all target columns
    return(as.list(df[idx, sample_cols, drop = FALSE]))
}

# Applies sample_conditional row-wise to an entire dataframe.
# For each row, uses the values of `given_cols` to condition the distribution
# and samples a joint outcome for `sample_cols`, adding them as new columns.
#
# @param data        A dataframe whose rows will be sampled
# @param dist_table  A conditional distribution from query_conditional_distribution()
# @param sample_cols Character vector of columns to sample (target variables)
# @param given_cols  Character vector of conditioning columns present in `data`
sample_conditional_df <- function(data, dist_table, sample_cols, given_cols) {
    missing <- setdiff(given_cols, names(data))
    if (length(missing) > 0) {
        stop(paste("given_cols not found in data:", paste(missing, collapse = ", ")))
    }

    # Build a list of conditioning args for each row, then call sample_conditional
    sampled_rows <- purrr::pmap(
        data[, given_cols, drop = FALSE],
        function(...) {
            conditions <- list(...)
            do.call(sample_conditional, c(list(dist_table = dist_table, sample_cols = sample_cols), conditions))
        }
    )

    # Bind results back as new columns into the original dataframe
    sampled_df <- dplyr::bind_rows(sampled_rows)
    data[, sample_cols] <- sampled_df

    return(data)
}

# --------------------------

tiempo <- read.csv("data/enut-ii-11.csv") %>%
    mutate(
        # paid_work = discretize_midpoints(t_paid_work, lower_cap = 0, upper_cap = 168),
        sleep = discretize_midpoints(t_sleep, lower_cap = 0, upper_cap = 168)
    )


# 1. Compute a large joint distribution
large_joint <- compute_joint_distribution(tiempo, c("edad_anios", "sexo", "paid_work", "sleep"))

# 2. Extract a specific conditional distribution P(Target | Given)
# e.g., P(sleep, paid_work | edad_anios, sexo)
sleep_given_age_sex <- query_conditional_distribution(large_joint, target_cols = c("sleep", "paid_work"), given_cols = c("edad_anios", "sexo"))

temp <- tiempo[1:100, ] %>% select(id_persona, edad_anios, sexo)

tiempo_synth <- sample_conditional_df(
    data = temp,
    dist_table = sleep_given_age_sex,
    sample_cols = c("sleep", "paid_work"),
    given_cols = c("edad_anios", "sexo")
)
