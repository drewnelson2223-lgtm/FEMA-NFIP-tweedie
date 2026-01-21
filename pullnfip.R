library(httr)
library(jsonlite)
library(dplyr)
library(future.apply)

# Parallel plan: adjust workers to your CPU (2–4 recommended)
plan(multisession, workers = 4)

pull_nfip_state_year_parallel <- function(
    start_year = 2000,
    end_year = as.integer(format(Sys.Date(), "%Y")),
    page_size = 20000,  # larger pages -> fewer API calls
    checkpoint_dir = "nfip_checkpoints"
) {
  
  base_url <- "https://www.fema.gov/api/open/v2/FimaNfipClaims"
  
  states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA",
              "HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
              "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
              "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC",
              "SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  
  # Create checkpoint directory
  if (!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir)
  
  # Function to pull a single state across all years
  pull_state <- function(st) {
    message("Starting state: ", st)
    state_results <- list()
    
    for (y in start_year:end_year) {
      skip <- 0
      
      repeat {
        filt <- paste0("(yearOfLoss eq ", y, ") and (state eq '", st, "')")
        
        resp <- GET(base_url, query = list(
          "$top" = page_size,
          "$skip" = skip,
          "$filter" = filt
        ))
        
        # HTTP error
        if (http_error(resp)) {
          warning("HTTP ", status_code(resp), " | year=", y, " state=", st)
          break
        }
        
        txt <- content(resp, "text", encoding = "UTF-8")
        if (nchar(txt) == 0) break
        
        parsed <- tryCatch(
          fromJSON(txt, flatten = TRUE),
          error = function(e) NULL
        )
        if (is.null(parsed) || !"FimaNfipClaims" %in% names(parsed)) break
        
        batch <- parsed$FimaNfipClaims
        
        # Safe exit condition
        if (is.null(batch) || !is.data.frame(batch) || nrow(batch) == 0) break
        
        # Summarize batch
        batch_summary <- batch %>%
          mutate(
            total_claim_paid =
              coalesce(as.numeric(amountPaidOnBuildingClaim), 0) +
              coalesce(as.numeric(amountPaidOnContentsClaim), 0) +
              coalesce(as.numeric(amountPaidOnIncreasedCostOfComplianceClaim), 0)
          ) %>%
          summarise(
            claim_count = n(),
            total_paid  = sum(total_claim_paid, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          mutate(
            state     = st,
            loss_year = y
          )
        
        state_results[[length(state_results)+1]] <- batch_summary
        skip <- skip + page_size
      }
      
      # Save checkpoint per state per year
      if (length(state_results) > 0) {
        saveRDS(
          bind_rows(state_results),
          file = file.path(checkpoint_dir, paste0("nfip_", st, ".rds"))
        )
      }
    }
    message("Finished state: ", st)
    return(bind_rows(state_results))
  }
  
  # Run all states in parallel
  all_states <- future_lapply(states, pull_state)
  
  # Combine into a single data frame
  results <- bind_rows(all_states) %>%
    group_by(state, loss_year) %>%
    summarise(
      claim_count = sum(claim_count),
      total_paid  = sum(total_paid),
      .groups = "drop"
    ) %>%
    mutate(
      avg_claim_amount = ifelse(claim_count > 0,
                                total_paid / claim_count,
                                NA_real_)
    )

  return(results)
}
