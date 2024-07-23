library(here)
library(tidyverse)

##
# create samples with increasing number of exceedences
# increase sample sizes
# get grades

dat <- list(
    `0` = c(0, 5), 
    `0.20` = c(1, 5), 
    `0.40` = c(2, 5),
    `0.60` = c(3, 5), 
    `0.80` = c(4, 5),
    `1` = c(5, 5)
) %>% 
    tibble::enframe(name = 'propexceed') %>% 
    mutate(
        value = purrr::map(value, function(x){
            
            dat <- tibble(
                sumgt = x[1], 
                station_tot = x[2],
                mult = 1:30
            ) %>% 
                mutate(
                    sumgt = sumgt * mult, 
                    station_tot = station_tot * mult
                ) %>% 
                select(-mult) %>% 
                dplyr::mutate(
                    exceed_10_prob = pbinom(sumgt - 1, station_tot, 0.10, lower.tail = T),
                    exceed_30_prob = pbinom(sumgt - 1, station_tot, 0.30, lower.tail = T),
                    exceed_50_prob = pbinom(sumgt - 1, station_tot, 0.50, lower.tail = T),
                    exceed_75_prob = pbinom(sumgt - 1, station_tot, 0.75, lower.tail = T)
                ) 
            
            # Put stations into binomial test groups
            dat$MWQA <- NA
            dat$MWQA[dat$exceed_10_prob <= 0.90] <- 'A'
            dat$MWQA[dat$exceed_10_prob > 0.90 & dat$exceed_30_prob <= 0.90] <- 'B'
            dat$MWQA[dat$exceed_30_prob > 0.90 & dat$exceed_50_prob <= 0.90] <- 'C'
            dat$MWQA[dat$exceed_50_prob > 0.90 & dat$exceed_75_prob <= 0.90] <- 'D'
            dat$MWQA[dat$exceed_75_prob > 0.90] <- 'E'
            
            return(dat)
            
        })
    ) %>% 
    unnest('value')

##
# plot grades with increasing sample sizes

toplo1 <- dat %>% 
    select(-MWQA) %>% 
    pivot_longer(
        cols = c(exceed_10_prob, exceed_30_prob, exceed_50_prob, exceed_75_prob),
        names_to = 'threshold',
        values_to = 'prob'
    ) %>% 
    mutate(
        threshold = case_when(
            threshold == 'exceed_10_prob' ~ '10%',
            threshold == 'exceed_30_prob' ~ '30%',
            threshold == 'exceed_50_prob' ~ '50%',
            threshold == 'exceed_75_prob' ~ '75%'
        )
    )

toplo2 <- dat %>% 
    select(propexceed, station_tot, MWQA) %>% 
    summarise(
        xmin = min(station_tot), 
        xmax = max(station_tot), 
        .by = c(propexceed, MWQA)
    ) %>% 
    mutate(
        xmin = lag(xmax, default = 1), 
        .by = propexceed
    )

# fix propexceed as factor
propexceedlev <- unique(toplo1$propexceed) %>% 
    as.numeric() %>% 
    `*`(100) %>% 
    round(0) %>% 
    paste0('% samples > threshold')
toplo1 <- toplo1 %>%
    mutate(
        propexceed = paste0(round(100 * as.numeric(propexceed), 0), '% samples > threshold'),
        propexceed = factor(propexceed, levels = propexceedlev)
    )
toplo2 <- toplo2 %>% 
    mutate(
        propexceed = paste0(round(100 * as.numeric(propexceed), 0), '% samples > threshold'),
        propexceed = factor(propexceed, levels = propexceedlev)
    )

cols <- c('#2DC938', '#E9C318', '#EE7600', '#CC3231', '#800080')

p <- ggplot() +
    geom_rect(data = toplo2, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = MWQA), alpha = 0.9) +
    geom_line(data = toplo1, aes(x= station_tot, y = prob, group = threshold, color = threshold)) + 
    geom_point(data = toplo1, aes(x= station_tot, y = prob, group = threshold, color = threshold)) + 
    scale_fill_manual(values = cols) +
    facet_wrap(~propexceed) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(labels = scales::percent) +
    geom_hline(yintercept = 0.90, linetype = 'dashed') +
    theme_minimal() +
    theme(legend.position = 'top') + 
    labs(
        x = 'Sample size', 
        fill = 'Grade', 
        color = 'Estimated likelihood\nof exceeding', 
        y = 'Probability',
        caption = 'Likelihood of exceeding threshold > 130 CFU / 100 mL'
    )

png(here('graden.png'), width = 8, height = 6, units = 'in', res = 300)
print(p)
dev.off()


