library(tidyverse)
library(yaml)

# Define the custom colors (converted from RGB) according to Piro's excel figures
custom_colors <- c(
  "D_"     = rgb(201, 184, 219, maxColorValue = 255),
  "D_tot"  = rgb(104, 52, 154,  maxColorValue = 255),
  "T_"     = rgb(237, 119, 115, maxColorValue = 255),
  "T_tot"  = rgb(234, 51, 35,   maxColorValue = 255),
  "delta_"  = rgb(58, 56, 56,    maxColorValue = 255)
)

# Get a list of all Excel files in the current folder
file_list <- list.files(
  path = "configs", 
  pattern = "^parameter_fig_.*\\.yml$", 
  full.names = TRUE
)

#' Patch initialized objects with values from YAML config
#' @param target The pre-initialized vector or matrix (usually all zeros)
#' @param updates The list from the YAML file (e.g., params$ch_2prime)
apply_updates <- function(target, updates) {
  # If the parameter isn't in the YAML, just return the original zeros
  if (is.null(updates)) return(target)
  
  for (idx_str in names(updates)) {
    # Convert string "1,3" into a numeric vector c(1, 3)
    indices <- as.numeric(unlist(strsplit(idx_str, ",")))
    val <- updates[[idx_str]]
    
    if (length(indices) == 3) {
      # It's a Matrix: target[x, y, z]
      target[indices[1], indices[2], indices[3]] <- val
    } else if (length(indices) == 2) {
      target[indices[1], indices[2]] <- val
    } else if (length(indices) == 1) {
      # It's a Vector: target[index]
      target[indices] <- val
    }
  }
  return(target)
}

for (config_path in file_list) {

  # Extract the base filename (e.g., "parameter_fig_1.yml")
  file_name <- basename(config_path)

  # Extract the number between "fig_" and "."
  fig_num <- str_extract(file_name, "(?<=fig_)\\d+(?=\\.)")
  fig_val <- as.numeric(fig_num)

  # Process the file
  params <- read_yaml(config_path)

  # Constants
  n <- params$n %||% 4
  m <- params$m %||% 2
  k_0 <- params$k_0 %||% 0.0125
  k_1 <- params$k_1 %||% 0.1
  dt <- params$dt %||% 0.2
  t_max <- params$t_max %||% 5000
  D_ <- params$D_ %||% 0.5
  T_ <- params$T_ %||% 0.5
  # This is necessary due to the even/odd computations below
  n <- if (n < 2*m) 2*m else n

  # Initialize 2D Matrices (n x n)
  # ReDim ... (1 To n, 1 To n)
  on_prime      <- matrix(0, nrow = n, ncol = n)
  off_prime     <- matrix(0, nrow = n, ncol = n)
  ch_prime      <- matrix(0, nrow = n, ncol = n)
  uch_prime     <- matrix(0, nrow = n, ncol = n)
  alpha_prime   <- matrix(0, nrow = n, ncol = n)

  on_2prime     <- matrix(0, nrow = n, ncol = n)
  off_2prime    <- matrix(0, nrow = n, ncol = n)
  ch_2prime     <- matrix(0, nrow = n, ncol = n)
  uch_2prime    <- matrix(0, nrow = n, ncol = n)
  alpha_2prime  <- matrix(0, nrow = n, ncol = n)

  Y_prime       <- matrix(0, nrow = n, ncol = n)
  Y_prime_sav   <- matrix(0, nrow = n, ncol = n)
  Z_prime       <- matrix(0, nrow = n, ncol = n)
  Z_prime_sav   <- matrix(0, nrow = n, ncol = n)
  d_Y_2prime    <- matrix(0, nrow = n, ncol = n)
  d_Z_2prime    <- matrix(0, nrow = n, ncol = n)

  Y_prime       <- matrix(0, nrow = n, ncol = n)
  Y_prime_sav   <- matrix(0, nrow = n, ncol = n)
  Z_prime       <- matrix(0, nrow = n, ncol = n)
  Z_prime_sav   <- matrix(0, nrow = n, ncol = n)
  Y_2prime      <- matrix(0, nrow = n, ncol = n)
  Y_2prime_sav  <- matrix(0, nrow = n, ncol = n)
  Z_2prime      <- matrix(0, nrow = n, ncol = n)
  Z_2prime_sav  <- matrix(0, nrow = n, ncol = n)
  d_Y_prime     <- matrix(0, nrow = n, ncol = n)
  d_Z_prime     <- matrix(0, nrow = n, ncol = n)
  d_Y_2prime    <- matrix(0, nrow = n, ncol = n)
  d_Z_2prime    <- matrix(0, nrow = n, ncol = n)

  # Initialize 3D Arrays (n x n x n)
  # ReDim ... (1 To n, 1 To n, 1 To n)
  cat_prime     <- array(0, dim = c(n, n, n))
  cat_2prime    <- array(0, dim = c(n, n, n))

  # Initialize 1D Vectors (n x 1)
  # Note: In R, these are usually just vectors, not 2D matrices with 1 column
  on_3prime_D    <- numeric(n)
  off_3prime_D   <- numeric(n)
  ch_3prime_D    <- numeric(n)
  uch_3prime_D   <- numeric(n)
  alpha_3prime_D <- numeric(n)
  cat_3prime_T   <- numeric(n)
  on_3prime_T    <- numeric(n)
  off_3prime_T   <- numeric(n)
  ch_3prime_T    <- numeric(n)
  uch_3prime_T   <- numeric(n)
  alpha_3prime_T <- numeric(n)
  cat_3prime_D   <- numeric(n)
  Y_3prime_D     <- numeric(n)
  Y_3prime_D_sav <- numeric(n)
  Z_3prime_D     <- numeric(n)
  Z_3prime_D_sav <- numeric(n)
  Y_3prime_T     <- numeric(n)
  Y_3prime_T_sav <- numeric(n)
  Z_3prime_T     <- numeric(n)
  Z_3prime_T_sav <- numeric(n)
  d_R            <- numeric(n)
  d_Y_3prime_D   <- numeric(n)
  d_Z_3prime_D   <- numeric(n)
  d_Y_3prime_T   <- numeric(n)
  d_Z_3prime_T   <- numeric(n)
  R_sav          <- numeric(n)
  R_tot          <- numeric(n)
  F_             <- numeric(n)
  R_             <- numeric(n)

  # Initialize 1D Vectors (m x 1)
  a_            <- numeric(m)
  b_            <- numeric(m)

  d_R <- apply_updates(d_R, params$d_R)
  on_prime <- apply_updates(on_prime, params$on_prime)
  off_prime <- apply_updates(off_prime, params$off_prime)
  ch_prime <- apply_updates(ch_prime, params$ch_prime)
  uch_prime <- apply_updates(uch_prime, params$uch_prime)
  cat_prime <- apply_updates(cat_prime, params$cat_prime)
  alpha_prime <- apply_updates(alpha_prime, params$alpha_prime)
  d_Y_prime <- apply_updates(d_Y_prime, params$d_Y_prime)
  d_Z_prime <- apply_updates(d_Z_prime, params$d_Z_prime)
  on_2prime <- apply_updates(on_2prime, params$on_2prime)
  off_2prime <- apply_updates(off_2prime, params$off_2prime)
  ch_2prime <- apply_updates(ch_2prime, params$ch_2prime)
  uch_2prime <- apply_updates(uch_2prime, params$uch_2prime)
  cat_2prime <- apply_updates(cat_2prime, params$cat_2prime)
  alpha_2prime <- apply_updates(alpha_2prime, params$alpha_2prime)
  d_Y_2prime <- apply_updates(d_Y_2prime, params$d_Y_2prime)
  d_Z_2prime <- apply_updates(d_Z_2prime, params$d_Z_2prime)
  R_ <- apply_updates(R_, params$R_)
  on_3prime_D <- apply_updates(on_3prime_D, params$on_3prime_D)
  off_3prime_D <- apply_updates(off_3prime_D, params$off_3prime_D)
  ch_3prime_D <- apply_updates(ch_3prime_D, params$ch_3prime_D)
  uch_3prime_D <- apply_updates(uch_3prime_D, params$uch_3prime_D)
  cat_3prime_T <- apply_updates(cat_3prime_T, params$cat_3prime_T)
  alpha_3prime_D <- apply_updates(alpha_3prime_D, params$alpha_3prime_D)
  d_Y_3prime_D <- apply_updates(d_Y_3prime_D, params$d_Y_3prime_D)
  d_Z_3prime_D <- apply_updates(d_Z_3prime_D, params$d_Z_3prime_D)
  on_3prime_T <- apply_updates(on_3prime_T, params$on_3prime_T)
  off_3prime_T <- apply_updates(off_3prime_T, params$off_3prime_T)
  ch_3prime_T <- apply_updates(ch_3prime_T, params$ch_3prime_T)
  uch_3prime_T <- apply_updates(uch_3prime_T, params$uch_3prime_T)
  cat_3prime_D <- apply_updates(cat_3prime_D, params$cat_3prime_D)
  alpha_3prime_T <- apply_updates(alpha_3prime_T, params$alpha_3prime_T)
  d_Y_3prime_T <- apply_updates(d_Y_3prime_T, params$d_Y_3prime_T)
  d_Z_3prime_T <- apply_updates(d_Z_3prime_T, params$d_Z_3prime_T)
  a_ <- apply_updates(a_, params$a_)
  b_ <- apply_updates(b_, params$b_)

  # Calculate Totals (R_tot, D_tot, T_tot)
  for (i in 1:n) {
    tmp1 <- 0
    for (j in 1:n) {
      tmp1 <- tmp1 + Y_prime[i, j] + Y_prime[j, i] + Z_prime[i, j] + Z_prime[j, i]
      tmp1 <- tmp1 + Y_2prime[i, j] + Y_2prime[j, i] + Z_2prime[i, j] + Z_2prime[j, i]
    }
    R_tot[i] = R_[i] + Y_3prime_D[i] + Z_3prime_D[i] + Y_3prime_T[i] + Z_3prime_T[i] + tmp1
  }

  tmp1 <- 0
  for (i in 1:n) {
    tmp1 <- tmp1 + Y_3prime_D[i] + Z_3prime_D[i]
  }
  D_tot <- D_ + tmp1

  tmp1 <- 0
  for (k in 1:m) {
    tmp1 <- tmp1 + R_tot[2 * k]
  }
  for (i in 1:n) {
    tmp1 <- tmp1 + Y_3prime_T[i] + Z_3prime_T[i]
  }
  T_tot <- T_ + tmp1

  #Start values for ribozymes and intermediate compexes
  T_demand <- 0
  for (i in 1:n) {
    for (j in 1:n) {
      T_demand <- T_demand + alpha_prime[i, j] * ch_prime[i, j] * Y_prime[i, j] + alpha_2prime[i, j] * ch_2prime[i, j] * Y_2prime[i, j]
    }
    T_demand <- T_demand + alpha_3prime_D[i] * ch_3prime_D[i] * Y_3prime_D[i] + alpha_3prime_T[i] * ch_3prime_T[i] * Y_3prime_T[i]
  }
  T_supply <- k_0 * T_
  for (i in 1:n) {
    T_supply <- T_supply + cat_3prime_D[i] * Z_3prime_T[i]
  }

  delta_ <- if (T_demand > T_supply) T_supply / T_demand else 1.0

  # 3. Setup Storage for Plotting
  # Instead of writing to Cells, we'll create a matrix to hold results over time
  # Define exactly how many columns you need
  # (n for R_) + (n for R_tot) + (5 for the single variables D, T, Dtot, Ttot, delta_)
  num_cols <- n + n + 5 

  history <- matrix(0, nrow = t_max+1, ncol = num_cols)
  colnames(history) <- c(
    paste0("R_", 1:n), 
    paste0("Rtot_", 1:n), 
    "D", "T", "D_tot", "T_tot", "delta_"
  )

  # save initial state
  history[1, ] <- c(R_, R_tot, D_, T_, D_tot, T_tot, delta_)

  # --- START TIME LOOP ---
  for (t_1 in 1:t_max) {
    # VBA 'sav' variables (Create a snapshot of current state)
    R_sav <- R_
    Y_prime_sav <- Y_prime
    Z_prime_sav <- Z_prime
    Y_2prime_sav <- Y_2prime
    Z_2prime_sav <- Z_2prime
    Y_3prime_D_sav <- Y_3prime_D
    Z_3prime_D_sav <- Z_3prime_D
    Y_3prime_T_sav <- Y_3prime_T
    Z_3prime_T_sav <- Z_3prime_T
    D_sav <- D_
    T_sav <- T_

    # Initialize Alocation Bound/Released
    T_allo_bound <- 0
    T_allo_released <- 0

    for (k in 1:m) {
      T_allo_bound <- T_allo_bound + a_[k] * R_sav[2 * k - 1] * T_sav
      T_allo_released <- T_allo_released + (b_[k] + d_R[2 * k]) * R_sav[2 * k]
      T_allo_released <- T_allo_released + d_Y_3prime_D[2 * k] * Y_3prime_D_sav[2 * k] + d_Z_3prime_D[2 * k] * Z_3prime_D_sav[2 * k]
      T_allo_released <- T_allo_released + d_Y_3prime_T[2 * k] * Y_3prime_T_sav[2 * k] + d_Z_3prime_T[2 * k] * Z_3prime_T_sav[2 * k]
      for (i in 1:n) {
        T_allo_released <- T_allo_released + d_Y_prime[i, 2 * k] * Y_prime_sav[i, 2 * k] + d_Y_prime[2 * k, i] * Y_prime_sav[2 * k, i] + d_Z_prime[i, 2 * k] * Z_prime_sav[i, 2 * k] + d_Z_prime[2 * k, i] * Z_prime_sav[2 * k, i]
        T_allo_released <- T_allo_released + d_Y_2prime[i, 2 * k] * Y_2prime_sav[i, 2 * k] + d_Y_2prime[2 * k, i] * Y_2prime_sav[2 * k, i] + d_Z_2prime[i, 2 * k] * Z_2prime_sav[i, 2 * k] + d_Z_2prime[2 * k, i] * Z_2prime_sav[2 * k, i]
      }
      for (i in 1:n) {
        for (j in 1:n) {
          T_allo_bound <- T_allo_bound + cat_2prime[2 * k, i, j] * Z_2prime_sav[i, j]
        }
      }
    }

    # Michaelis-Menten equations for ADP-ATP cycle:
    cat_tmp1 <- 0
    tmp1 <- 0
    for (i in 1:n) {
      cat_tmp1 <- cat_tmp1 + cat_3prime_T[i] * Z_3prime_D_sav[i]
      tmp1 <- tmp1 + off_3prime_T[i] * Y_3prime_T_sav[i] - on_3prime_T[i] * R_sav[i] * T_sav + d_Y_3prime_T[i] * Y_3prime_T_sav[i] + d_Z_3prime_T[i] * Z_3prime_T_sav[i]
    }
    T_ <- T_sav + dt * (-k_0 * T_sav + k_1 * D_sav - T_allo_bound + T_allo_released + tmp1 + cat_tmp1)
    cat_tmp1 <- 0
    tmp1 <- 0
    for (i in 1:n) {
      cat_tmp1 <- cat_tmp1 + cat_3prime_D[i] * Z_3prime_T_sav[i]
      tmp1 <- tmp1 + off_3prime_D[i] * Y_3prime_D_sav[i] - on_3prime_D[i] * R_sav[i] * D_sav + d_Y_3prime_D[i] * Y_3prime_D_sav[i] + d_Z_3prime_D[i] * Z_3prime_D_sav[i]
    }
    D_ <- D_sav + dt * (k_0 * T_sav - k_1 * D_sav + tmp1 + cat_tmp1)
    for (i in 1:n) {
      Y_3prime_D[i] <- Y_3prime_D_sav[i] + dt * (-d_Y_3prime_D[i] * Y_3prime_D_sav[i] + on_3prime_D[i] * R_sav[i] * D_sav - (off_3prime_D[i] + delta_ * ch_3prime_D[i]) * Y_3prime_D_sav[i] + uch_3prime_D[i] * Z_3prime_D_sav[i])
      Z_3prime_D[i] <- Z_3prime_D_sav[i] + dt * (-d_Z_3prime_D[i] * Z_3prime_D_sav[i] + delta_ * ch_3prime_D[i] * Y_3prime_D_sav[i] - uch_3prime_D[i] * Z_3prime_D_sav[i] - cat_3prime_T[i] * Z_3prime_D_sav[i])
      Y_3prime_T[i] <- Y_3prime_T_sav[i] + dt * (-d_Y_3prime_T[i] * Y_3prime_T_sav[i] + on_3prime_T[i] * R_sav[i] * T_sav - (off_3prime_T[i] + delta_ * ch_3prime_T[i]) * Y_3prime_T_sav[i] + uch_3prime_T[i] * Z_3prime_T_sav[i])
      Z_3prime_T[i] <- Z_3prime_T_sav[i] + dt * (-d_Z_3prime_T[i] * Z_3prime_T_sav[i] + delta_ * ch_3prime_T[i] * Y_3prime_T_sav[i] - uch_3prime_T[i] * Z_3prime_T_sav[i] - cat_3prime_D[i] * Z_3prime_T_sav[i])
    }

    # Michaelis_Menten equations for ribozyme reproduction cycle:
    for (k in 1:m) {
      F_[2 * k - 1] <- -a_[k] * R_sav[2 * k - 1] * T_sav + b_[k] * R_sav[2 * k]
      F_[2 * k] <- -F_[2 * k - 1]
    }

    for (i in 1:n) {
      tmp1 <- 0
      for (j in 1:n) {
        tmp1 <- tmp1 + off_prime[i, j] * Y_prime_sav[i, j] - on_prime[i, j] * R_sav[i] * R_sav[j] + off_prime[j, i] * Y_prime_sav[j, i] - on_prime[j, i] * R_sav[j] * R_sav[i]
        tmp1 <- tmp1 + off_2prime[i, j] * Y_2prime_sav[i, j] - on_2prime[i, j] * R_sav[i] * R_sav[j] + off_2prime[j, i] * Y_2prime_sav[j, i] - on_2prime[j, i] * R_sav[j] * R_sav[i]
      }
      cat_tmp1 = 0
      for (l in 1:n) {
        for (j in 1:n) {
          cat_tmp1 <- cat_tmp1 + cat_prime[i, l, j] * Z_prime_sav[l, j] + cat_prime[l, i, j] * Z_prime_sav[i, j]
          cat_tmp1 <- cat_tmp1 + cat_2prime[i, l, j] * Z_2prime_sav[l, j] + cat_2prime[l, i, j] * Z_2prime_sav[i, j] + cat_2prime[l, j, i] * Z_2prime_sav[j, i]
        }
      }
      tmp0 <- off_3prime_D[i] * Y_3prime_D_sav[i] - on_3prime_D[i] * R_sav[i] * D_sav + cat_3prime_T[i] * Z_3prime_D_sav[i]
      tmp0 <- tmp0 + off_3prime_T[i] * Y_3prime_T_sav[i] - on_3prime_T[i] * R_sav[i] * T_sav + cat_3prime_D[i] * Z_3prime_T_sav[i]
      R_[i] <- R_sav[i] + dt * (-d_R[i] * R_sav[i] + F_[i] + tmp0 + tmp1 + cat_tmp1)
    }
    for (i in 1:n) {
      for (j in 1:n) {
       cat_tmp1 <- 0
        for (l in 1:n) {
          cat_tmp1 <- cat_tmp1 + cat_prime[l, i, j] * Z_prime_sav[i, j]
        }
        Y_prime[i, j] <- Y_prime_sav[i, j] + dt * (-d_Y_prime[i, j] * Y_prime_sav[i, j] + on_prime[i, j] * R_sav[i] * R_sav[j] - (off_prime[i, j] + delta_ * ch_prime[i, j]) * Y_prime_sav[i, j] + uch_prime[i, j] * Z_prime_sav[i, j])
        Z_prime[i, j] <- Z_prime_sav[i, j] + dt * (-d_Z_prime[i, j] * Z_prime_sav[i, j] + delta_ * ch_prime[i, j] * Y_prime_sav[i, j] - uch_prime[i, j] * Z_prime_sav[i, j] - cat_tmp1)
      }
    }
    for (i in 1:n) {
      for (j in 1:n) {
       cat_tmp1 <- 0
        for (l in 1:n) {
          cat_tmp1 <- cat_tmp1 + cat_2prime[l, i, j] * Z_2prime_sav[i, j]
        }
        Y_2prime[i, j] <- Y_2prime_sav[i, j] + dt * (-d_Y_2prime[i, j] * Y_2prime_sav[i, j] + on_2prime[i, j] * R_sav[i] * R_sav[j] - (off_2prime[i, j] + delta_ * ch_2prime[i, j]) * Y_2prime_sav[i, j] + uch_2prime[i, j] * Z_2prime_sav[i, j])
        Z_2prime[i, j] <- Z_2prime_sav[i, j] + dt * (-d_Z_2prime[i, j] * Z_2prime_sav[i, j] + delta_ * ch_2prime[i, j] * Y_2prime_sav[i, j] - uch_2prime[i, j] * Z_2prime_sav[i, j] - cat_tmp1)
      }
    }

    for (i in 1:n) {
      tmp1 <- 0
      for (j in 1:n) {
        tmp1 <- tmp1 + Y_prime[i, j] + Y_prime[j, i] + Z_prime[i, j] + Z_prime[j, i]
        tmp1 <- tmp1 + Y_2prime[i, j] + Y_2prime[j, i] + Z_2prime[i, j] + Z_2prime[j, i]
      }
      R_tot[i] <- R_[i] + Y_3prime_D[i] + Z_3prime_D[i] + Y_3prime_T[i] + Z_3prime_T[i] + tmp1
    }
    tmp1 <- 0
    for (i in 1:n) {
      tmp1 <- tmp1 + Y_3prime_D[i] + Z_3prime_D[i]
    }
    D_tot <- D_ + tmp1
    tmp1 <- 0
    for (k in 1:m) {
      tmp1 <- tmp1 + R_tot[2 * k]
    }
    for (i in 1:n) {
      tmp1 <- tmp1 + Y_3prime_T[i] + Z_3prime_T[i]
    }
    T_tot <- T_ + tmp1

    T_demand <- 0
    for (i in 1:n) {
      for (j in 1:n) {
        T_demand <- T_demand + alpha_prime[i, j] * ch_prime[i, j] * Y_prime[i, j] + alpha_2prime[i, j] * ch_2prime[i, j] * Y_2prime[i, j]
      }
      T_demand <- T_demand + alpha_3prime_D[i] * ch_3prime_D[i] * Y_3prime_D[i] + alpha_3prime_T[i] * ch_3prime_T[i] * Y_3prime_T[i]
    }
    T_supply <- k_0 * T_
    for (i in 1:n) {
      T_supply <- T_supply + cat_3prime_D[i] * Z_3prime_T[i]
    }
    delta_   <- if (T_demand > T_supply) T_supply / T_demand else 1.0

    # Save to our history matrix (replaces writing to Excel Cells)
    history[t_1+1, ] <- c(R_, R_tot, D_, T_, D_tot, T_tot, delta_)
  } # End of t_1 loop

  # Convert history matrix to a clean Data Frame
  res <- as.data.frame(history)
  colnames(res) <- c(paste0("R_", 1:n), paste0("Rtot_", 1:n), "D_", "T_", "D_tot", "T_tot", "delta_")
  res$t <- 0:t_max

  # Save as TSV
  tsv_name <- paste0("data/Fig_", fig_num, ".tsv.gz")
  write_tsv(res, tsv_name)

  # Determine a scaling factor for delta_
  # Adjust 'scale_factor' based on the max concentration (e.g., if concentrations reach 10, use 10)
  scale_factor <- max(res[c("D_", "D_tot", "T_", "T_tot")])

  # Plot concentrations
  # Use a conditional filter: if fig_val is 1, remove delta_. Otherwise, keep everything.
  plot_data <- res %>%
    mutate(delta_scaled = delta_ * scale_factor) %>% # Scale delta_ to max concentration
    select(t, D_, D_tot, T_, T_tot, delta_ = delta_scaled) %>%
    pivot_longer(cols = -t, names_to = "Variable", values_to = "Value") %>%
    filter(!(fig_val == 1 & Variable == "delta_")) # <--- Trick to remove delta_ in figure 1

  if (fig_val == 1) {
    title <- paste("Figure", fig_num, "Evolution of ADP (D_) and ATP (T_)")
  } else {
    title <- paste("Figure", fig_num, "Evolution of ADP (D_), ATP (T_), and delta_ (rhs)")
  }
  # Create the base plot
  main_plot <- ggplot(plot_data, aes(x = t, y = Value, color = Variable, linetype = Variable)) +
    geom_line(aes(alpha = Variable), linewidth = 1) +
    scale_color_manual(values = custom_colors) +
    scale_linetype_manual(values = c("delta_" = "dashed", "D_" = "solid", "D_tot" = "dotted", "T_" = "solid", "T_tot" = "dotted")) +
    scale_alpha_manual(values = c("delta_" = 1.0, "D_" = 0.5, "D_tot" = 1.0, "T_" = 0.5, "T_tot" = 1.0)) +
    labs(title = title,
	 x = "Time Step (t)",
	 y = "Concentration") +
    theme_minimal()

  # Add the secondary axis ONLY if it's not Figure 1
  if (fig_val != 1) {
    main_plot <- main_plot + 
      scale_y_continuous(
	sec.axis = sec_axis(~ . / scale_factor, name = "Delta (0-1)")
      )
  }

  # Create a list to store the R plots
  r_plots <- list()

  for (i in 1:n) {
    # Dynamically pick the columns for Rn and R_totn
    r_col <- paste0("R_", i)
    rtot_col <- paste0("Rtot_", i)

    plot_data_r <- res %>%
      select(t, all_of(c(r_col, rtot_col))) %>%
      pivot_longer(cols = -t, names_to = "Variable", values_to = "Value")

    r_plots[[i]] <- ggplot(plot_data_r, aes(x = t, y = Value, color = Variable)) +
      geom_line(linewidth = 1) +
      labs(title = paste("Figure", fig_num, "Evolution of Species R", i),
	   x = "Time Step (t)",
	   y = "Concentration") +
      theme_minimal() +
      scale_color_manual(values = c("blue", "darkred")) # Specific colors for R vs Rtot
  }

  # Save Plots as PDF
  pdf_name <- paste0("plots/Fig_", fig_num, ".pdf")
  pdf(pdf_name, paper="a4r", width=0, height=0)

  print(main_plot) # Page 1: D, T, and Delta

  for (i in 1:n) {
    print(r_plots[[i]]) # Pages 2 to n+1: Individual R plots
  }

  dev.off()

  message("Processed Figure: ", fig_num)
}

###
