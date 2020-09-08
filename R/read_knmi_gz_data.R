#' @name read_knmi_gz_data
#' @title Transform/Split KNMI gziped wind data file
#' @description Transform/Split KNMI gzipped wind observations date file to a dataframe/matrix
#' @param raw the imported gzipped file with only one column contains all varaible values
#' @return a splitted dataframe/matrix with multiple columns, each one stands for one variable
#' @export

# @examples
# knmiwind_each = read_knmi_gz_data( read.table(gzfile(knmi_each,'rt'),header=FALSE,sep=';') )

read_knmi_gz_data = function(raw)
{
  ######## new approach: try readr::read_fwf() function #####
  ###########################################################
  splited = matrix(nrow = nrow(raw), ncol = 18)
  colnames(splited) = c('DTG','LOCATION','NAME','LATITUDE','LONGITUDE','ALTITUDE','FF_10M_10','DD_10',
                        'DDN_10','DD_STD_10','DDX_10','FF_SENSOR_10','FF_10M_STD_10','FX_10M_10',
                        'FX_10M_MD_10','FX_SENSOR_10','FX_SENSOR_MD_10','SQUALL_10')

  for (i in 1 : nrow(raw) )
  {
    item = raw[i,]
    splited[i,1] = gsub('  ', '', str_sub(item, start = 1, end = 21) )
    splited[i,2] = gsub(' ', '', str_sub(item, start = 21, end = 41) )
    splited[i,3] = gsub('  ', '', str_sub(item, start = 41, end = 89) )
    splited[i,4] = gsub(' ', '', str_sub(item, start = 89, end = 109) )
    splited[i,5] = gsub(' ', '', str_sub(item, start = 109, end = 129) )
    splited[i,6] = gsub(' ', '', str_sub(item, start = 129, end = 149) )
    splited[i,7] = gsub(' ', '', str_sub(item, start = 149, end = 169) )
    splited[i,8] = gsub(' ', '', str_sub(item, start = 169, end = 189) )
    splited[i,9] = gsub(' ', '', str_sub(item, start = 189, end = 209) )
    splited[i,10] = gsub(' ', '', str_sub(item, start = 209, end = 229) )
    splited[i,11] = gsub(' ', '', str_sub(item, start = 229, end = 249) )
    splited[i,12] = gsub(' ', '', str_sub(item, start = 249, end = 269) )
    splited[i,13] = gsub(' ', '', str_sub(item, start = 269, end = 289) )
    splited[i,14] = gsub(' ', '', str_sub(item, start = 289, end = 309) )
    splited[i,15] = gsub(' ', '', str_sub(item, start = 309, end = 329) )
    splited[i,16] = gsub(' ', '', str_sub(item, start = 329, end = 349) )
    splited[i,17] = gsub(' ', '', str_sub(item, start = 349, end = 369) )
    splited[i,18] = gsub(' ', '', str_sub(item, start = 369, end = 390) )
  }
  return(splited)
}
