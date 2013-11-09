#
proc monitor_standard {} {

global nbid
global Xworld Yworld Ra Dec Pixel
global Sum Min Max Mean Merr
global Npix Nbad
global HelpString
global Cache Buffer

nbs stop
nbs clear
nbs monitor $nbid.pixel Pixel
nbs monitor $nbid.x Xworld
nbs monitor $nbid.y Yworld
nbs monitor $nbid.ra Ra
nbs monitor $nbid.dec Dec
nbs monitor $nbid.sum Sum
nbs monitor $nbid.min Min
nbs monitor $nbid.max Max
nbs monitor $nbid.mean Mean
nbs monitor $nbid.merr Merr
nbs monitor $nbid.region Region
nbs monitor $nbid.ngood Ngood
nbs monitor $nbid.nbad Nbad
nbs monitor $nbid.help HelpString
nbs monitor $nbid.cache Cache
nbs monitor $nbid.buffer Buffer

nbs start 200
}
#
proc monitor_browse {} {

global nbid
global Xworld Yworld Ra Dec Pixel
global HelpString

nbs stop
nbs clear
nbs monitor $nbid.help HelpString
nbs monitor $nbid.pixel Pixel
nbs monitor $nbid.x Xworld
nbs monitor $nbid.y Yworld
nbs monitor $nbid.ra Ra
nbs monitor $nbid.dec Dec
for {set j 1} {$j<=9} {incr j} {
  for {set i 1} {$i<=9} {incr i} {
    global data$i$j
    nbs monitor $nbid.data$i$j data$i$j
  }
}
nbs start 100
}


#
proc monitor_setpos {} {

global nbid
global Xworld Yworld Ra Dec Pixel
global HelpString

nbs stop
nbs clear
nbs monitor $nbid.help HelpString
nbs monitor $nbid.pixel Pixel
nbs monitor $nbid.x Xworld
nbs monitor $nbid.y Yworld
nbs monitor $nbid.ra Ra
nbs monitor $nbid.dec Dec
nbs start 100
}
#
