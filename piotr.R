download.file("https://origin.cpc.ncep.noaa.gov/products/people/mchen/CFSv2FCST/weekly/data/CFSv2.tmpsfc.0304.wkly.anom.nc", destfile="moj.nc")

library(magic)
library(raster)
library(ncdf4)
library(fields)
library(maps)

nc = nc_open("moj.nc")

anom = ncvar_get(nc, varid="anom")
lon = ncvar_get(nc, varid="LON")
lat = ncvar_get(nc, varid="LAT")

ind = c(which(lon>180), which(lon<=180))
lon2 = ifelse(lon>180, lon-360, lon)
lon2 = lon2[ind]


image.plot(lon2, lat, anom[ind,,1])

map('world', add=T)

rasteryzuj = function(nr_warstwy = 1){
r <-raster(
  vals = arot((anom[ind,,nr_warstwy]),1),
  nrows = length(lat), 
  ncols = length(lon),
  #xmn=range(lon2)[1], xmx=range(lon2)[2],
  #ymn=range(lat)[1], ymx=range(lat)[2],
  crs = CRS("+init=epsg:4326")
)
}

r = rasteryzuj(3)
res(r) # tu cos smierdzi
plot(r)
map('world', add=T)
