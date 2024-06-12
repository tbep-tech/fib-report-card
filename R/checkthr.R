definition_combos <- expand.grid(wet_threshold = seq(0.25, 3, by = 0.25), 
                                 temporal_threshold = 1:14)

outs_df <- list()
for(i in 1:nrow(definition_combos)){
    cat(i, '\t')
    wetthrs <- definition_combos$wet_threshold[i]
    tmpthrs <- definition_combos$temporal_threshold[i]
    
    fib_out <- anlz_fibwetdry(fibdat, prcpdat, 
                              temporal_window = tmpthrs, 
                              wet_threshold = wetthrs)
    
    
    # sample information
    # ------------------------------------------------------------------------
    # number wet samples
    nwet <- sum(fib_out$wet_sample, na.rm = TRUE)
    # number dry samples
    ndry <- sum(fib_out$wet_sample == FALSE, na.rm = TRUE)
    # number samples
    nsamps <- nrow(fib_out)
    # proportion of samples that are wet, out of those that could be ID'd:
    prop_wet <- nwet / (nwet + ndry)
    # percent wet
    pct_wet <- round(prop_wet*100, 1)
    # number stations
    nstns <- length(unique(fib_out$station))
    
    overall <- data.frame(
        wet_threshold = wetthrs,
        temporal_window = tmpthrs,
        nsamps,
        nstns,
        nwet,
        ndry,
        prop_wet,
        pct_wet
    )
    
    
    # concentration info
    # -------------------------------------------------------------------------
    conc_out <- fib_out |> 
        filter(!is.na(wet_sample),
               !is.na(FIBconc)) |> 
        summarize(.by = wet_sample,
                  nsamples = n(),
                  medianEntero = median(FIBconc),
                  IQREntero = IQR(FIBconc),
                  pct75Entero = quantile(FIBconc, 0.75),
                  geom_meanEntero = exp(mean(log(FIBconc + 0.001))),
                  quantilesEntero = list(quantile(FIBconc)),
                  nexceedances = sum(FIBconc > 70),
                  propExceedances = nexceedances / nsamples)
    
    
    # precip info
    # -------------------------------------------------------------------------
    
    
    comp2 <- conc_out |> 
        select(-IQREntero, -quantilesEntero, -nexceedances) |> 
        pivot_longer(-wet_sample,
                     names_to = "stat",
                     values_to = "value") |>
        mutate(wet_sample = case_when(wet_sample == TRUE ~ "wet",
                                      wet_sample == FALSE ~ "dry",
                                      .default = "somethingwrong")) |> 
        pivot_wider(names_from = c(stat, wet_sample),
                    values_from = value)
    
    if(sum(names(comp2) %in% c('nsamples_dry', 'nsamples_wet')) == 2)
        comp2 <- comp2 |> 
            mutate(
                meddiff = medianEntero_wet - medianEntero_dry,
            )
        
    outs_df[[i]] <- cbind(overall, comp2)
    
    
}


tmp <- do.call('bind_rows', outs_df)

ggplot(tmp, aes(x = factor(wet_threshold), y = factor(temporal_window))) +
    geom_tile(aes(fill = pct_wet), color = 'black') +
    geom_text(aes(label = paste0(round(pct_wet, 0), '%')), size = 3, color = 'white') +
    scale_fill_viridis_c() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
        y = "Temporal Window (days)",
        x = "Wet Threshold (inches)",
        fill = 'Percent samples "Wet"'
    )

ggplot(tmp, aes(x = factor(wet_threshold), y = factor(temporal_window))) +
    geom_tile(aes(fill = meddiff), color = 'black') +
    geom_text(aes(label = round(meddiff, 0)), size = 3, color = 'white') +
    scale_fill_viridis_c() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(
        y = "Temporal Window (days)",
        x = "Wet Threshold (inches)",
        fill = 'Diff. in median betweetn wet/dry'
    )

