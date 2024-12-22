# from copilot - australia with titles
# 1. PACKAGES

libs <- c(
    "terra",
    "ozmaps",
    "sf",
    "tidyverse",
    "ggtern",
    "elevatr",
    "png",
    "rayshader",
    "magick"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libraries == FALSE)){
    install.packages(
        libs[!installed_libraries]
    )
}

invisible(
    lapply(
        libs, library, character.only = TRUE
    )
)

# 2. STATE BORDERS

# Get the state borders
state_sf <- ozmaps::ozmap_states

# Filter for Tasmania
tasmania_sf <- state_sf %>%
    dplyr::filter(NAME == "Tasmania")

# Debugging: Check the geometry of Tasmania
print(st_geometry(tasmania_sf))

plot(sf::st_geometry(tasmania_sf))

png("tasmania-borders.png")
plot(sf::st_geometry(tasmania_sf))
dev.off()

# 3 DOWNLOAD ESRI LAND COVER TILES

urls <- c(
    "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2022/55G_20220101-20230101.tif"
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
    pattern = "20230101.tif$",
    full.names = TRUE
)

crs <- "EPSG:4326"

for(raster in raster_files){
    rasters <- terra::rast(raster)

    state <- tasmania_sf %>%
        sf::st_transform(
            crs = terra::crs(
                rasters
            )
        )

    land_cover <- terra::crop(
        rasters,
        terra::vect(
            state
        ),
        snap = "in",
        mask = TRUE
    ) %>%
    terra::aggregate(
        fact = 5,
        fun = "modal"
    ) %>%
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

# 5 LOAD VIRTUAL LAYER

r_list <- list.files(
    path = getwd(),
    pattern = "_tasmania",
    full.names = TRUE
)

land_cover_vrt <- terra::vrt(
    r_list,
    "tasmania_land_cover_vrt.vrt",
    overwrite = TRUE
)

# 6 FETCH ORIGINAL COLORS

ras <- terra::rast(
    raster_files[[1]]
)

raster_color_table <- do.call(
    data.frame,
    terra::coltab(ras)
)

head(raster_color_table)

hex_code <- ggtern::rgb2hex(
    r = raster_color_table[,2],
    g = raster_color_table[,3],
    b = raster_color_table[,4]
)

# 7 ASSIGN COLORS TO RASTER

cols <- hex_code[c(2:3, 5:6, 8:12)]

from <- c(1:2, 4:5, 7:11)
to <- t(col2rgb(cols))
land_cover_vrt <- na.omit(land_cover_vrt)

land_cover_tasmania <- terra::subst(
    land_cover_vrt,
    from = from,
    to = to,
    names = cols
)

terra::plotRGB(land_cover_tasmania)

# 8 DIGITAL ELEVATION MODEL

# Reduce the resolution of the elevation model for faster processing
elev <- elevatr::get_elev_raster(
    locations = tasmania_sf,
    z = 9,  # Lower zoom level for lower resolution
    clip = "locations"
)

crs_lambert <-
    "+proj=laea +lat_0=-25 +lon_0=134 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

land_cover_tasmania_resampled <- terra::resample(
    x = land_cover_tasmania,
    y = terra::rast(elev),
    method = "near"
) %>%
terra::project(crs_lambert)

terra::plotRGB(land_cover_tasmania_resampled)

img_file <- "land_cover_tasmania.png"

terra::writeRaster(
    land_cover_tasmania_resampled,
    img_file,
    overwrite = TRUE,
    NAflag = 255
)

img <- png::readPNG(img_file)

# 9. RENDER SCENE
#----------------

elev_lambert <- elev %>%
    terra::rast() %>%
    terra::project(crs_lambert)

elmat <- rayshader::raster_to_matrix(
    elev_lambert
)

h <- nrow(elev_lambert)
w <- ncol(elev_lambert)

elmat %>%
    rayshader::height_shade(
        texture = colorRampPalette(
            cols[9]
        )(256)
    ) %>%
    rayshader::add_overlay(
        img,
        alphalayer = 1
    ) %>%
    rayshader::plot_3d(
        elmat,
        zscale = 12,
        solid = FALSE,
        shadow = TRUE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(
            w / 5, h / 5
        ),
        zoom = .5,
        phi = 85,
        theta = 0
    )

rayshader::render_camera(
    zoom = .58
)

print("Step 9 completed: Scene rendered")

# 10. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/air_museum_playground_4k.hdr"
hdri_file <- basename(u)

download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)

filename <- "3d_land_cover_tasmania-dark.png"

# Set the maximum width to 3000 pixels and adjust the height proportionally
max_width <- 3000
aspect_ratio <- h / w
width <- max_width
height <- max_width * aspect_ratio

rayshader::render_highquality(
    filename = filename,
    preview = TRUE,
    light = TRUE,
    environment_light = hdri_file,
    intensity_env = 1.5,  # Increase the intensity of the environment light
    rotate_env = 90,
    interactive = FALSE,
    parallel = TRUE,
    width = width,
    height = height,
    samples = 1000,  # Increase the number of samples for better quality
    sample_method = "sobol",  # Use a faster sampling method
    ambient_light = 0.3,  # Adjust the intensity of ambient light
    shadow_darkness = 0.5,  # Adjust the darkness of the shadows
    solid = TRUE  # Enable solid rendering of the base
)

print("Step 10 completed: Object rendered")

# 11. PUT EVERYTHING TOGETHER

c(
    "#419bdf", "#397d49", "#7a87c6", 
    "#e49635", "#c4281b", "#a59b8f", 
    "#a8ebff", "#616161", "#e3e2c3"
)

legend_name <- "land_cover_legend.png"
png(legend_name, bg = "transparent")
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
    legend = c(
        "Water",
        "Trees",
        "Crops",
        "Built area",
        "Rangeland"
    ),
    pch = 15,
    cex = 2,
    pt.cex = 1,
    bty = "n",
    col = c(cols[c(1:2, 4:5, 9)]),
    fill = c(cols[c(1:2, 4:5, 9)]),
    border = "grey20"
)
dev.off()

# filename <- "land-cover-tasmania-3d.png"

lc_img <- magick::image_read(
    filename
)

my_legend <- magick::image_read(
    legend_name
)

my_legend_scaled <- magick::image_scale(
    magick::image_background(
        my_legend, "none"
    ), 2500
)

# Add title and subtitle
title <- magick::image_blank(width = width, height = 600, color = "none") %>%
    magick::image_annotate(text = "Tasmania Land Cover", size = 300, font = "Cinzel Decorative", gravity = "north", location = "+0+50", color = "black") %>%
    magick::image_annotate(text = "Map by Frederic Fery | R tutorial by Milos Popovic | World Sentinel-2 Global Land Cover", size = 70, font = "Cinzel Decorative", gravity = "south", location = "+0+50", color = "black")

p <- magick::image_composite(
    magick::image_scale(
        lc_img, "x7000" 
    ),
    my_legend_scaled,
    gravity = "southwest",
    offset = "+100+0"
) %>%
magick::image_composite(title, gravity = "north")

magick::image_write(
    p, "3d_tasmania_land_cover_final.png"
)
