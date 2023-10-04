fqdn <- function() {
  if (Sys.getenv("FQDN") == "") {
    fqdn <- as.character(Sys.info()["nodename"])
  } else {
    fqdn <- Sys.getenv("FQDN")
  }
  return(fqdn)
} # fqdn

ud_convert <- function(x, u1, u2) {
  stopifnot(units::ud_are_convertible(u1, u2))
  x1 <- units::set_units(x, value = u1, mode = "standard")
  x2 <- units::set_units(x1, value = u2, mode = "standard")

  units::drop_units(x2)
} #

days_in_year <- function(year, leap_year = TRUE) {
  if (any(year %% 1 != 0)) {
    stop(paste("Year must be integer. Given ", year, '.'))
  }
  ifelse( leap_year & lubridate::leap_year(year), yes = 366, no = 365)
}

seconds_in_year <- function(year, leap_year = TRUE, ...) {
  diy <- days_in_year(year, leap_year)
  siy <- ud_convert(diy, 'days', 'seconds')
  return(siy)
}


cos_solar_zenith_angle <- function(doy, lat, lon, dt, hr) {
  et <- equation_of_time(doy)
  merid  <- floor(lon / 15) * 15
  merid[merid < 0] <- merid[merid < 0] + 15
  lc     <- (lon - merid) * -4/60  ## longitude correction
  tz     <- merid / 360 * 24  ## time zone
  midbin <- 0.5 * dt / 86400 * 24  ## shift calc to middle of bin
  t0   <- 12 + lc - et - tz - midbin  ## solar time
  h    <- pi/12 * (hr - t0)  ## solar hour
  dec  <- -23.45 * pi / 180 * cos(2 * pi * (doy + 10) / 365)  ## declination
  cosz <- sin(lat * pi / 180) * sin(dec) + cos(lat * pi / 180) * cos(dec) * cos(h)
  cosz[cosz < 0] <- 0
  return(cosz)
}

equation_of_time <- function(doy) {
  stopifnot(doy <= 366)
  f      <- pi / 180 * (279.5 + 0.9856 * doy)
  et     <- (-104.7 * sin(f) + 596.2 * sin(2 * f) + 4.3 *
               sin(4 * f) - 429.3 * cos(f) - 2 *
               cos(2 * f) + 19.3 * cos(3 * f)) / 3600  # equation of time -> eccentricity and obliquity
  return(et)
}

check_ed_metheader <- function(ed_metheader, check_files = TRUE) {
  testthat::test_that(
    "ED met header object is a nested list",
    {
      testthat::expect_true(!is.null(names(ed_metheader[[1]])))
    }
  )
  .z <- lapply(ed_metheader, check_ed_metheader_format, check_files = check_files)
  invisible(TRUE)
}

check_ed_metheader_format <- function(ed_metheader_format, check_files = TRUE) {
  testthat::test_that(
    "Format has the correct names",
    {
      correct_names <- c("path_prefix", "nlon", "nlat", "dx", "dy", "xmin", "ymin", "variables")
      all(names(ed_metheader_format) %in% correct_names)
    }
  )
  testthat::test_that(
    "ED met header files exist and are not empty",
    {
      met_files <- PEcAn.utils::match_file(ed_metheader_format$path_prefix)
      testthat::expect_gte(length(met_files), 1)
      testthat::expect_true(all(file.exists(met_files)))
      testthat::expect_true(all(file.size(met_files) > 0))
    }
  )

  testthat::test_that(
    "Met header metadata fields are valid",
    {
      testthat::expect_true(is.numeric(ed_metheader_format$nlon))
      testthat::expect_true(is.numeric(ed_metheader_format$nlat))
      testthat::expect_true(is.numeric(ed_metheader_format$dx))
      testthat::expect_true(is.numeric(ed_metheader_format$dy))
      testthat::expect_true(is.numeric(ed_metheader_format$xmin))
      testthat::expect_true(is.numeric(ed_metheader_format$ymin))
      testthat::expect_is(ed_metheader_format$variables, "data.frame")
    }
  )

  if (check_files) {
    met_files <- PEcAn.utils::match_file(ed_metheader_format$path_prefix, suffix = "h5")
    .z <- lapply(met_files, check_ed_metfile, variables = ed_metheader_format$variables)
  }
}

check_ed_metfile <- function (metfile, variables)
{
  hfile <- hdf5r::H5File$new(metfile, mode = "r")
  variables <- variables[variables$flag != 4, ]
  testthat::test_that("All variables present in metfile", {
    testthat::expect_true(all(variables$variable %in% hfile$ls()$name))
  })
}

write_ed_metheader <- function(ed_metheader, filename,
                               header_line = shQuote("header")) {
  nformats <- length(ed_metheader)
  blocks <- vector("list", nformats)
  for (i in seq_len(nformats)) {
    metformat <- ed_metheader[[i]]
    block_lines <- character(6)
    prefix <- normalizePath(metformat$path_prefix, mustWork = FALSE)
    if (file.exists(prefix) && file.info(prefix)$isdir) {
      # ED doesn't treat directories specially.
      # Need to add trailing slash.
      prefix <- paste0(prefix, "/")
    }
    block_lines[1] <- prefix
    block_lines[2] <- paste(
      metformat$nlon,
      metformat$nlat,
      metformat$dx,
      metformat$dy,
      metformat$xmin,
      metformat$ymin
    )
    block_lines[3] <- nrow(metformat$variables)
    block_lines[4] <- paste(metformat$variables$variable, collapse = " ")
    block_lines[5] <- paste(metformat$variables$update_frequency, collapse = " ")
    block_lines[6] <- paste(metformat$variables$flag, collapse = " ")
    blocks[[i]] <- block_lines
  }
  file_lines <- c(header_line, as.character(nformats), Reduce(c, blocks))
  writeLines(file_lines, filename)
}

# TODO: First line does actually matter -- maybe a list of format names?
# Regardless, need to set it to something
