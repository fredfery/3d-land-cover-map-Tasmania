# 1. PACKAGES

libs <- c(
    "terra",
    "giscoR",
    "sf",
    "tidyverse",
    "ggtern",
    "elevatr",
    "png",
    "rayshader",
    "magick"
)

installed_libraries <- libs %in% rownames(installed.packages())

if(any(installed_libraries == FALSE)){
    install.packages(libs[!installed_libraries])
}

invisible(lapply(libs, library, character.only = TRUE))

# 2. COUNTRY BORDERS

# Get Australia's borders
country_sf <- giscoR::gisco_get_countries(
    country = "Australia",
    resolution = "1"
)

# Plot the geometry of Australia
png("australia-borders.png")
plot(sf::st_geometry(country_sf), main = "Australia Borders")
dev.off()

#filter to Tasmania only

libs <- c(
    "geodata",
    "tidyverse"
)

# install missing libraries
installed_libraries <-
    libs %in% rownames(
        installed.packages()
    )
if (
    any(
        installed_libraries
        == F
    )
) {
    install.packages(
        libs[
            !installed_libraries
        ],
       dependencies = T
    )
}

# load libraries
invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

# fetch australia admin. level 1 shapefile
australia_admin1 <- geodata::gadm(
        country = "AU",
        level = 1,
        path = getwd()
    ) |>
        sf::st_as_sf()

# filter Tasmania
lombardy <-  australia_admin1 |>
    dplyr::filter(
        NAME_1 == "Tasmania"
        ) |>
    sf::st_as_sf()



# 3. DOWNLOAD ESRI LAND COVER TILES

urls <- c(
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2023/55G_20230101-20240101.tif"
)

for(url in urls){
    download.file(
        url = url,
        destfile = basename(url),
        mode = "wb"
    )
}


# 4 LOAD TILES

raster_files <- list.files(
    path = getwd(),
    pattern = "55G_20230101-20240101.tif$",
    full.names = T
)

crs <- "EPSG:4326"

for(raster in raster_files){
    rasters <- terra::rast(raster)

    country <- country_sf |>
        sf::st_transform(
            crs = terra::crs(
                rasters
            )
        )

    land_cover <- terra::crop(
        rasters,
        terra::vect(
            country
        ),
        snap = "in",
        mask = T
    ) |>
    terra::aggregate(
        fact = 10,
        fun = "modal"
    ) |>
    terra::project(crs)

    terra::writeRaster(
        land_cover,
        paste0(
            raster,
            "_tasmania",
            ".tif"
        )
    )
}

# 5. LOAD VIRTUAL LAYER

r_list <- list.files(
    path = getwd(),
    pattern = "_australia.tif$",
    full.names = TRUE
)

land_cover_vrt <- terra::vrt(
    r_list,
    "australia_land_cover_vrt.vrt",
    overwrite = TRUE
)

# 6. FETCH ORIGINAL COLORS

ras <- terra::rast(raster_files[1])

raster_color_table <- do.call(data.frame, terra::coltab(ras))

hex_code <- ggtern::rgb2hex(
    r = raster_color_table[,2],
    g = raster_color_table[,3],
    b = raster_color_table[,4]
)

# 7. ASSIGN COLORS TO RASTER

cols <- hex_code[c(2:3, 5:6, 8:12)]

from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))

land_cover_australia <- terra::subst(
    land_cover_vrt,
    from = from,
    to = to,
    names = cols
)

terra::plotRGB(land_cover_australia)

# 8. DIGITAL ELEVATION MODEL

elev <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 9, clip = "locations"
)

crs_lambert <- "+proj=laea +lat_0=-42 +lon_0=147 +datum=WGS84 +units=m +no_defs"

land_cover_australia_resampled <- terra::resample(
    x = land_cover_australia,
    y = terra::rast(elev),
    method = "near"
) |>
terra::project(crs_lambert)

terra::plotRGB(land_cover_australia_resampled)

img_file <- "land_cover_australia.png"

terra::writeRaster(
    land_cover_australia_resampled,
    img_file,
    overwrite = TRUE,
    NAflag = 255
)

img <- png::readPNG(img_file)

# 9. RENDER SCENE

elev_lambert <- elev |>
    terra::rast() |>
    terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(elev_lambert)

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

elmat |>
    rayshader::height_shade(texture = colorRampPalette(cols[9])(256)) |>
    rayshader::add_overlay(img, alphalayer = 1) |>
    rayshader::plot_3d(
        elmat,
        zscale = 12,
        solid = FALSE,
        shadow = TRUE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(w / 5, h / 5),
        zoom = 0.5,
        phi = 85,
        theta = 0
    )

rayshader::render_camera(zoom = 0.58)

# 10. RENDER OBJECT

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"
hdri_file <- basename(u)

download.file(url = u, destfile = hdri_file, mode = "wb")

filename <- "3d_land_cover_australia-dark.png"

rayshader::render_highquality(
    filename = filename,
    preview = TRUE,
    light = FALSE,
    environment_light = hdri_file,
    intensity_env = 1,
    rotate_env = 90,
    interactive = FALSE,
    parallel = TRUE,
    width = w * 1.5,
    height = h * 1.5
)

# 11. PUT EVERYTHING TOGETHER

legend_name <- "land_cover_legend.png"
png(legend_name)
par(family = "mono")

plot(
    NULL,
    xaxt = "n",
    yaxt = "n",
    bty = "n",
    ylab = "",
    xlab = "",
    xlim = 0:1,
    ylim = 0:1,
    xaxs = "i",
    yaxs = "i"
)
legend(
    "center",
    legend = c("Water", "Trees", "Crops", "Built area", "Rangeland"),
    pch = 15,
    cex = 2,
    pt.cex = 1,
    bty = "n",
    col = cols[c(1:2, 4:5)],
    fill = cols[c(1:2, 4:5)],
    border = "grey20"
)
dev.off()

lc_img <- magick::image_read(filename)
my_legend <- magick::image_read(legend_name)

my_legend_scaled <- magick::image_scale(
    magick::image_background(my_legend, "none"), 2500
)

p <- magick::image_composite(
    magick::image_scale(lc_img, "x7000"),
    my_legend_scaled,
    gravity = "southwest",
    offset = "+100+0"
)

magick::image_write(p, "3d_australia_land_cover_final.png")