#' Create a DEM for use with `ClimateNA`
#'
#' **Windows only** due to limitations using `ClimateNA` with non-Windows
#' created DEMs.
#'
#' @param destinationPath the directory to save the output `.asc` raster
#' @param filename2 the name of the output DEM without the `.asc` suffix
#'
#' @return a DEM in ASCII format. The resolution is 30m and the studyArea is the Canadian boreal
#'
#' @author Tati Micheletti based on Ian Eddy's makeClimateDEM
#' @export
#' @importFrom reproducible prepInputs
#' @importFrom terra buffer compareGeom crop mask project resample same.crs trim vect writeRaster
#' @rdname makeClimateDEM_boreal
makeClimateDEM_boreal <- function(destinationPath, filename2 = "borealDEM") {
  if (!identical(tolower(.Platform$OS.type), "windows"))
    stop("ClimateNA will only accept .asc files created by Windows.")
  
  stopifnot(requireNamespace("googledrive", quietly = TRUE))
  
  if (arcSecRes[1] != arcSecRes[2]) {
    stop(".asc format requires x and y dimensions to be equal. Please adjust arcSecRes.")
  }
  
  if (!is(studyArea, "SpatVector")) {
    studyArea <- vect(studyArea)
  }
  
  # 30m = 1.9438444924196123 arcSeconds at 60o Latitude (average of the boreal forest)
  r <- 1.9438444924196123
  
  studyArea <- Cache(prepInputs, url = "https://drive.google.com/file/d/1of3bIlPnLMDmumerLX-thILHtAoJpeiC", 
                     targetFile = "studyArea.shp", 
                     archive = "studyArea.zip", 
                     alsoExtract = "similar", 
                     destinationPath = file.path(getwd(), "outputs/"), 
                     fun = "terra::vect",
                     userTags = c("objectName:studyArea",
                                  "goal:sA"),
                     omitArgs = c("destinationPath"))
  
  rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/file/d/1hSn2Rdiyou9znGRfmJlsUJ-p2fcHX6Uy",
                         targetFile = "processedRTM.tif",
                         fun = "terra::rast",
                         archive = "processedRTM.zip",
                         destinationPath = file.path(getwd(), "outputs/"),
                         userTags = c("goal:RTM", "objectName:rasterToMatch"),
                         omitArgs = "destinationPath")
  
  studyAreaLL <- reproducible::projectTo(studyArea, "epsg:4326")
  rasterToMatchLL <- reproducible::projectTo(from = rasterToMatch, 
                                             projectTo = studyAreaLL)
  
  gtopo30N <- Cache(prepInputs, url = "https://drive.google.com/file/d/14puAtns8oTZDtvWzpQ6_FgK4MbozGZFK/",
    rasterToMatch = rasterToMatchLL,
    studyArea = studyAreaLL,
    overwrite = TRUE,
    userTags = "objectName:gtopo30N",
    omitArgs = c("overwrite", "destinationPath"),
    targetFile = "gtopo30Canada.tif",
    archive = "gtopo30Canada.zip",
    destinationPath = destinationPath)

  return(gtopo30N)
}