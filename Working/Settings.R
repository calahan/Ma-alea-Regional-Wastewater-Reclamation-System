# The R package CalahanLab is loaded in .Rprofile

#
# Site Settings
#
# From the plan: 6.46 cm = 80ft: 1 cm = 12.3839 ft, wid = 3.4 cm = 42 ft
# len = 24.2 cm = 300 ft
# fw_wid        <- 42
# fw_len        <- 300
# flow          <- 224843
# units(fw_wid) <- 'ft'
# units(fw_len) <- 'ft'
# units(fw_wid) <- 'm'
# units(fw_len) <- 'm'
# units(flow)   <- 'gallon/d'

{
  # Metadata
  b_title <- "MRWRS Algal Turf Floway"
  b_units <- 'US'
  b_paper <- 'letter'
  b_csl   <- file.path('Code', 'Rmd', '_Briefing.csl')
  b_refs  <- file.path('Code', 'Rmd', '_References.bib')
  
  authors <- list(
    c('Dean Calahan, Ph.D.', 'deanc@fykor.com'),
    c('Travis Liggett, M.S.', 'travis@reefpowermaui.com')
  )

  # Location
  s_lat         <- 20.809336
  s_lon         <- -156.490729
  
  # Assign values in SI for calculating
  s_vol         <- 3785412
  units(s_vol)  <- 'liter'
  fw_wid        <- 12.8016
  units(fw_wid) <- 'm'
  fw_len        <- 91.44
  units(fw_len) <- 'm'
  flow          <- 851123.4
  units(flow)   <- 'liter/d'
  lhlr          <- 120
  units(lhlr)   <- 'liter/min/m'
  
  # Total nutrients
  P_conc        <- 5
  units(P_conc) <- 'mg/L'
  units(P_conc) <- 'g/m^3'
  
  N_conc        <- 5
  units(N_conc) <- 'mg/L'
  units(N_conc) <- 'g/m^3'
  
  #P_in         <- P_conc * s_vol
  #units(P_tot)  <- 'kg'
  
  # N_tot         <- N_conc * s_vol
  # units(N_tot)  <- 'kg'
  
  prod         <- 10
  units(prod)  <- 'g/d/m2'
  bm_N_prop    <- 0.05
  bm_P_prop    <- 0.003

  #' Calculate useful production values \code{FlowayProduction} returns a named
  #' list containing useful values that might be needed when designing an ATS
  #' @param wid,len floway width and length
  #' @param lhlr linear hydraulic loading rate
  #' @param prod_d daily productivity
  #' @param N_pr,P_pr biomass N and P proportion
  #' @param vol_res volume of reservoir
  #' @param vol_d daily volume of influent
  FlowayProduction <-
    function(wid, len, lhlr, prod, N_pr, P_pr, N_c, P_c, vol_res, vol_d) {
    seven          <- 7
    units(seven)   <- 'd'
    one_day        <- 1
    units(one_day) <- 'd'
    
    area    <- wid * len
    hlr     <- wid * lhlr
    prod_d  <- prod * area * one_day
    prod_w  <- 7 * prod_d
    Nrem_d  <- N_pr * prod_d
    Prem_d  <- P_pr * prod_d
    Nrem_w  <- 7 * Nrem_d
    Prem_w  <- 7 * Prem_d
    N_d     <- vol_d * N_c
    P_d     <- vol_d * P_c
    N_w     <- seven * N_d
    P_w     <- seven * P_d
    
    # The following forces simplification (see stackoverflow etc.?)
    units(prod_d) <- 'kg'
    units(prod_w) <- 'kg'
    units(Nrem_d) <- 'kg'
    units(Prem_d) <- 'kg'
    units(Nrem_w) <- 'kg'
    units(Prem_w) <- 'kg'
    
    #units(prod_w) <- 'kg/week'
    #units(Nrem_d) <- 
    vol_to         <- hlr / vol_d
    vol_to         <- drop_units(vol_to) 
    units(vol_to) <- 'd-1'
    
    res_to         <- hlr / vol_res
    res_to         <- drop_units(res_to)
    units(res_to) <- 'd-1'    
    
    list(
      area    = area,
      hlr     = hlr,
      prod_d  = prod_d,
      prod_w  = prod_w,
      Nrem_d  = Nrem_d,
      Prem_d  = Prem_d,
      Nrem_w  = Nrem_w,
      Prem_w  = Prem_w,
      N_d     = N_d,
      P_d     = P_d,
      N_wk    = seven * N_d,
      P_wk    = seven * P_d
    )
  }
  prod_nums <- FlowayProduction(
    fw_wid, fw_len, lhlr, prod, bm_N_prop, bm_P_prop, N_conc, P_conc, s_vol, flow)
  # seven        <- 7
  # units(seven) <- 'd'
  # w_afdw       <- seven * fw_area * prod
  # w_Nrem       <- bm_N_prop * w_afdw
  # w_Prem       <- bm_P_prop * w_afdw
  # Nin          <- N_conc * flow
  # Pin          <- P_conc * flow
  # w_Nin        <- seven * Nin
  # w_Pin        <- seven * Pin
}

if(FALSE)
{
  # Convert to US units for display
  units(s_vol)   <- 'gallon'
  units(fw_area) <- 'acre'
  units(fw_wid)  <- 'ft'
  units(fw_len)  <- 'ft'
  units(lhlr)    <- 'gallon/min/ft'
  units(flow)    <- 'gallon/d'
  units(hlr)     <- 'gallon/d'
  units(w_afdw)  <- 'lb'
  units(w_Nin)   <- 'lb'
  units(w_Pin)   <- 'lb'
  units(w_Nrem)  <- 'lb'
  units(w_Prem)  <- 'lb'
} 

{
  #
  # Productivity
  #
  p_lhlr <- 120
  units(p_lhlr) <- 'L/min/m'
  units(p_lhlr) <- 'gallon/min/ft'
  
  p_afdw <- 10
  units(p_afdw) <- 'g/m^2/d'
  units(p_afdw) <- 'ton/acre/year'
}

# Convert to US units
# units(fw_wid)  <- 'ft'
# units(fw_len)  <- 'ft'
# units(fw_lhlr) <- 'gallon/min/ft'

# Turnover time
# ex_hl        <- fw_wid[[1]] * fw_lhlr[[1]]
# units(ex_hl) <- 'yard^3/min'
# ex_to        <- s_vol / ex_hl
# units(ex_to) <- 'day'

#  
# Briefing Settings
#
b_title   <- 'Maalea Regional Wastewater Reclamation System'
b_units   <- 'US'
b_paper   <- 'letter'
b_csl     <- file.path('Code', 'Rmd', '_Briefing.csl')
b_refs    <- file.path('Code', 'Rmd', '_References.bib')
b_fn      <- 'MRWRS'
b_panpath <- file.path('Research', 'Screen Shots', 'Crops')
b_figpath <- file.path('Visual Elements', 'Figures')
                       
authors <- list(
  c('Dean Calahan (Fykor LLC)', 'deanc@fykor.com'),
  c('Travis Ligget (Reef Power LLC)', 'travis@reefpowermaui.com')
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

# 

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

Briefing <- function() {
  RenderPub(title = b_fn)
}

# Figure 1
# In Research/Screen Shots
# 1A: 'Screen Shot 2023-12-13 at 3.35.26 PM' -> 'Crops/Millards Quarry Pond.png'
# 1B: 'Screen Shot 2023-12-13 at 3.31.59 PM' -> 'Crops/Retention Pond.png'
# 1C: 'Screen Shot 2023-12-13 at 4.19.56 PM' -> 'Crops/Fishing Preserve.png'
BriefingFigure1 <- function(fig_path) {
  out_fn  <- file.path(b_figpath, '1.png')
  if(file.exists(out_fn)) return()
  
  infiles <- c(
    file.path(b_panpath, 'Millards Quarry Pond.png'),
    file.path(b_panpath, 'Retention Pond.png'),
    file.path(b_panpath, 'Fishing Preserve.png')
  )
  
  AssemblePanels(
    out_fn,
    c(1, 2),
    infiles,
    3.625,
    0.0625,
    300,
    c('A', 'B', 'C'),
    rep('white', 3),
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

# Note, the following should be transitioned to CalahanLab and gotten working.
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
