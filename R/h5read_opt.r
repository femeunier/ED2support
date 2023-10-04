h5read_opt <- function( h5File ) {
  
  dset_names <- h5ls(h5File, recursive = FALSE, datasetinfo = FALSE)$name
  
  fid <- H5Fopen(h5File)
  dapl <- H5Pcreate("H5P_DATASET_ACCESS")
  contents <- sapply(dset_names, FUN = function(fid, dset_name) {
    did <- .Call("_H5Dopen", fid@ID, dset_name, dapl@ID, PACKAGE='rhdf5')
    res <- .Call("_H5Dread", did, NULL, NULL, NULL,TRUE, 0L, FALSE, FALSE,PACKAGE='rhdf5')
    invisible(.Call("_H5Dclose", did, PACKAGE='rhdf5'))
    return(res)
  }, fid = fid)
  
  H5Pclose(dapl)
  H5Fclose(fid)
  
  return(contents)
}
