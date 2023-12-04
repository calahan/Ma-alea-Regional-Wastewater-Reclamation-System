# The R package CalahanLab is loaded in .Rprofile

#  
# Briefing Settings
#

b_title <- 'BriefingTitle'
b_units <- 'US'
b_paper <- 'letter'
b_csl   <- file.path('Code', 'Rmd', '_Briefing.csl')
b_refs  <- file.path('Code', 'Rmd', '_References.bib')

authors <- list(
  c('First Author', 'First Author Affiliation'),
  c('Second Author', 'Second Author Affiliation')
)

b_author = paste0(
  '\n',
  paste0(
    unlist(lapply(1:length(authors), function(x) {
      paste0(
        '  - ',
        authors[[x]][[1]],
        '^[',
        authors[[x]][[2]],
        ']\n'
      )
    }
    )
    ), collapse = ''
  )
)


# Palette for the color vision deficient
#  ?scale_color_viridis_b
cvd_pal <- list(
  red     = '#F05039',
  brink   = '#E57A77', 
  pink    = '#EEBAB4', 
  blue    = '#1F449C', 
  ultra   = '#3D65A5',
  pwinkle = '#7CA1CC', 
  gray    = '#A8B6CC'
)

#' @name BriefingFigures
#' @title Build figures for a Fykor Briefing
#' @description Produces 6 figure panels related to the text generated from an
#'   ATS site definition by the Briefing Rmd files
#' @param lat latitude (deg)
#' @param area growth area (m^2)
#' @param start growth start day of year
#' @param end growth end day of year
#' @param prod_means vector of mean gross production for replete and limited (g m^-2 d^-2)
#' @param elem biomass elemental proportions (named list)
#' @return Side effect: six figure panel png and two figure png files created
#' @examples
#' \dontrun{
#' BriefingBuildFigures(
#'     lat   = 39,
#'     area  = 10000,
#'     start = 90,
#'     end   = 330,
#'     prod  = 15,
#'     elem  = list(C = 0.5, N = 0.05, P = 0.003)
#' )
#' }
#' @export
BriefingFigures <- function(
    lat = 20.79, year = 2023, zone = 'US/Hawaii', area = 1214, start_day = 1,
    end_day = 360, prod_names = c('C replete', 'C limited'),
    prod_means = c(15, 5), elem  = list(
      c(C = 0.5, N = 0.05, P = 0.003),
      c(C = 0.05, N = 0.05, P = 0.05))) {
  
  fig_path <- file.path('..', '..', 'Visual Elements', 'Figures')
  
  BriefingFigure1(fig_path)
  BriefingFigure2(
    lat        = lat,
    year       = year,
    zone       = zone,
    start_day  = start_day,
    end_day    = end_day,
    prod_names = prod_names,
    prod_means = prod_means,
    elem       = elem,
    area       = area,
    fig_path   = fig_path
  )
}

BriefingFigure1 <- function(fig_path) {
  # Panels are created by Code/sh/Fig1Panels.sh
  in_path <- file.path(fig_path, '1')
  out_fn  <- file.path(fig_path, 'Figure 1.png')
  
  infiles <- c(
    file.path(in_path, 'Site.png'),
    file.path(in_path, 'Head.png'),
    file.path(in_path, 'Tail.png')
  )
  
  AssemblePanels(
    out_fn,
    c(1, 2),
    infiles,
    3.625,
    0.0625,
    300,
    c('A', 'B', 'C'),
    rep('black', 3),
    cex = 0.75
  )
}

BriefingFigure2 <- function(
    lat, year, zone, start_day, end_day, prod_names, prod_means, elem, area,
    fig_path) {
  
  prod_df1 <- SimpleAnnualProd(
    lat       = lat,
    year      = year,
    zone      = zone,
    start_day = start_day,
    end_day   = end_day,
    prod_mean = prod_means[[1]]
  )
  prod_df1[prod_df1 == 0] <- NA
  
  prod_df2 <- SimpleAnnualProd(
    lat       = lat,
    year      = year,
    zone      = zone,
    start_day = start_day,
    end_day   = end_day,
    prod_mean = prod_means[[2]]
  )
  prod_df2[prod_df2 == 0] <- NA
  
  prod_df1 <- AnnualNutrientRemoval(prod_df1, elem[[1]])
  prod_df2 <- AnnualNutrientRemoval(prod_df2, elem[[2]])
  
  nut_df1  <- data.frame(
    nutrient = c('N', 'P'),
    removal  = c(sum(prod_df1$N, na.rm = TRUE), sum(prod_df1$P, na.rm = TRUE))
  )
  
  nut_df2  <- data.frame(
    nutrient = c('N', 'P'),
    removal  = c(sum(prod_df2$N, na.rm = TRUE), sum(prod_df2$P, na.rm = TRUE))
  )
  
  # econ_df <- data.frame(
  #   area      = area,
  #   cost      = 0.50,
  #   P_subsidy = 25 * 2.20462,
  #   N_subsidy = 15 * 2.20462
  # )
  
  BriefingFigure2A(prod_df1, prod_df2)
  BriefingFigure2B(nut_df1, nut_df2)
  BriefingFigure2C(prod_means[[1]], nrow(prod_df1))
  #BriefingFigure2C(prod_df, nut_df, econ_df)
  
  in_path <- file.path(fig_path, '2')
  out_fn  <- file.path(fig_path, 'Figure 2.png')
  
  infiles <- c(
    file.path(in_path, 'A.png'),
    file.path(in_path, 'B.png'),
    file.path(in_path, 'C.png')
  )
  
  AssemblePanels(
    out_fn,
    c(1, 2),
    infiles,
    3.625,
    0.0625,
    300,
    c('A', 'B', 'C'),
    rep('black', 3),
    cex = 0.75
  )
}

BriefingFigure2A <- function(prod_df1, prod_df2) {
  # Figure 2, Panel A, Biomass Productivity
  day_ct <- nrow(prod_df1)
  prod_df <- data.frame(
    date      = c(prod_df1$date, prod_df2$date),
    Condition = c(rep('C replete', day_ct), rep('C limited', day_ct)),
    prod      = c(prod_df1$prod, prod_df2$prod)
  )
  
  panel_A <- ggplot(prod_df, aes(date, prod, color = Condition)) +
    geom_line() +
    #scale_color_manual(values = c(cvd_pal$pwinkle, cvd_pal$ultra)) +
    scale_color_viridis_d() +
    scale_x_date(date_breaks = '1 month', date_labels = '%b') +
    scale_y_continuous(limits = c(0, 5 * ceiling(max(prod_df$prod) / 5))) +
    xlab('Date') +
    ylab(
      expression(
        paste('Productivity (', g, ' ', m^{-2}, ' ', d^{-1}, ')'))) +
    ThemeBriefing()
  
  suppressWarnings(
    ggsave(
      filename ='../../Visual Elements/Figures/2/A.png',
      plot     = panel_A,
      width    = 92.08,
      height   = 50.24,
      units    = 'mm',
      dpi      = 600
    )
  )
}

BriefingFigure2B <- function(nut_df1, nut_df2) {
  # Figure 2B: Nutrient Removal
  nut_df <- data.frame(
    Condition = c(rep('C replete', 2), rep('C limited', 2)),
    nutrient  = c('N', 'P', 'N', 'P'),
    removal   = c(nut_df1$removal, nut_df2$removal)
  )
  
  panel_B <- ggplot(nut_df,  aes(fill = Condition, x = nutrient, y = removal)) +
    geom_bar(width = 0.5, position = position_dodge(0.6), stat = 'identity') +
    #scale_fill_manual(values = c(cvd_pal$pwinkle, cvd_pal$brink)) +
    scale_fill_viridis_d() +
    scale_y_continuous(limits = c(0, 100 * ceiling(max(nut_df$removal) / 100))) +
    xlab('Nutrient') +
    ylab(
      expression(
        paste('Removal (', g, ' ', m^{-2}, ' ', yr^{-1}, ')'))) +
    ThemeBriefing()
  
  suppressWarnings(
    ggsave(
      filename ='../../Visual Elements/Figures/2/B.png',
      plot     = panel_B,
      width    = 48.92,
      height   = 40.24,
      units    = 'mm',
      dpi      = 600
    )
  )
}

#' Plot of daily volume processed along x, acres needed along y, with one line
#' plotted for each increment of polishing
#' @param prod_df productivity data frames for C replete, limited
#' @param max_flux influent flow rate, L d-1 * 1e+6
#' @param conc_N influent N concentration, mg L-1
#' @param bm_N biomass N proportion
#' @param inc inc number of polishing increments to plot
#' 15.14164 4 MGD = 4 MGD
BriefingFigure2C <- function(
    mean_prod, days, max_flux = 15.14164 / 8, conc_N = 5, bm_N = 0.03, inc = 5) {
  ann_prod <- mean_prod * days / 1000               # kg m-2 yr-1
  N_rem    <- seq(conc_N, 0, -conc_N / (inc))       # N removal per treatment level (mg L-1)
  tot_N    <- max_flux * N_rem * days * 1e+6 / 1e+6 # N removed (mg * 1e+6 = kg)
  tot_bm   <- tot_N / bm_N                          # kg
  tot_area <- (0.000247105) * tot_bm / ann_prod     # ac
  val_ct   <- 20
  vols     <- rep(seq(0, max_flux, max_flux / (val_ct - 1)), inc)
  vols_g   <- 0.264172 * vols
  levels   <- unlist(lapply((1:(inc)), function(x) { return(rep(N_rem[[x]], val_ct))}))
  vol_lev  <- vols * levels
  areas    <- max(tot_area) * vol_lev / max (vol_lev)
  
  area_df <- data.frame(
    vol     = vols_g,
    removal = as.character(levels),
    area    = areas
  )
  
  panel <- ggplot(area_df, aes(vol, area, color = removal)) +
    geom_line() +
    scale_color_viridis_d() +
    scale_y_continuous(limits = c(0, 5), n.breaks = 6) +
    xlab('R-1 Water Flow (MGD)') +
    ylab('Algal Growth Area (ac)') +
    labs(color = expression(N~Reduction~'('*mg~L^-1~')')) +
    geom_segment(aes(x = -0.01, xend = 0.3, y = 0.3, yend = 0.3), color = 'white') +
    ThemeBriefing()
  
  suppressWarnings(
    ggsave(
      filename ='../../Visual Elements/Figures/2/C.png',
      plot     = panel,
      width    = 41.55,
      height   = 40.24,
      units    = 'mm',
      dpi      = 600
    )
  )}

# BriefingFigure2C <- function(prod_df, nut_df, econ_df) {
#   area      <- econ_df$area
#   cost      <- econ_df$cost
#   prod_tot  <- area * sum(prod_df$prod, na.rm = TRUE) / 1e+3 # kg
#   N_tot     <- area * nut_df[which(nut_df$nutrient == 'N'),]$removal / 1e+3 # kg
#   P_tot     <- area * nut_df[which(nut_df$nutrient == 'P'),]$removal / 1e+3 # kg
#   prod_cost <- prod_tot * cost / 1000
#   N_income  <- N_tot * econ_df$N_subsidy / 1000
#   P_income  <- P_tot * econ_df$P_subsidy / 1000
#   
#   net_cost <- data.frame(
#     item = c('Costs', 'N', 'P', 'Net'),
#     value = c(
#       prod_cost = -prod_cost,
#       N_income  = N_income,
#       P_income  = P_income,
#       net       = N_income + P_income - prod_cost 
#     )
#   )
#   
#   panel_C <- ggplot(net_cost, aes(item, value)) +
#     geom_bar(stat = 'identity', aes(x = item, y = value, fill = item), show.legend = FALSE) +
#     scale_x_discrete(limits = c('Costs', 'N', 'P', 'Net')) +
#     scale_y_continuous(limits = c(-prod_cost, max(N_income, P_income))) +
#     scale_fill_manual(
#       values = c(cvd_pal$brink, cvd_pal$ultra, cvd_pal$pwinkle, cvd_pal$blue)) +
#     xlab('Item') +
#     ylab('US Dollars × 1,000') +
#     ThemeBriefing()
#   
#   suppressWarnings(
#     ggsave(
#       filename ='../../Visual Elements/Figures/2/C.png',
#       plot     = panel_C,
#       width    = 41.55,
#       height   = 40.24,
#       units    = 'mm',
#       dpi      = 600
#     )
#   )
# 
# }

ThemeBriefing <- function(font = 'Helvetica') {
  theme_minimal() %+replace%
    theme(
      plot.background  = element_rect(color = 'white'),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line        = element_line(color = 'black', size = 0.5),
      axis.ticks       = element_blank(),
      axis.title       = element_text(family = font, size = 4),
      axis.text        = element_text(family = font, size = 3),
      legend.title     = element_text(family = font, size = 4),
      legend.text      = element_text(family = font, size = 3),
      legend.key.size  = unit(2, 'mm'),
      legend.position  = 'top'
    )
}
