

library(dplyr)
library(magick)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# List of all country codes and the country name
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
js <- jsonlite::read_json("data-raw/country-flags/countries.json")
country_codes <- dplyr::tibble(ccode = tolower(names(js)), country = unname(unlist(js)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in all the PNG flags and get info about them
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pngs  <- magick::image_read(flagon::flags(flagon::country_codes$ccode))
png_info <- magick::image_info(pngs)
png_info <- png_info %>%
  select(-format, -colorspace, -matte, -density) %>%
  rename(png_width = width, png_height = height, png_filesize = filesize) %>%
  mutate(aspect_ratio = png_width/png_height) %>%
  mutate(ccode = flagon::country_codes$ccode)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in all the SVG flags and get info about them
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
svgs   <- magick::image_read(flagon::flags(flagon::country_codes$ccode, 'svg'))
svg_info <- magick::image_info(svgs)
svg_info <- svg_info %>%
  select(svg_filesize = filesize) %>%
  mutate(ccode = flagon::country_codes$ccode)


country_codes <- left_join(country_codes, png_info, by = 'ccode')
country_codes <- left_join(country_codes, svg_info, by = 'ccode')



usethis::use_data(country_codes, internal = FALSE, overwrite = TRUE)
