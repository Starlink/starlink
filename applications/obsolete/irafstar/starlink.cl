# IRAF package initialisation script for the Starlink package

package starlink

cl < "starlink$irafstar/zzsetenv.def"

#set kappa = starlink$kappa/
#task kappa = starlink$kappa/kappa.cl
#reset helpdb = (envget ("helpdb") // ",kappa$helpdb.mip")

#set figaro = starlink$figaro/
#task figaro = starlink$figaro/figaro.cl
#reset helpdb = (envget ("helpdb") // ",figaro$/helpdb.mip")

clbye()
