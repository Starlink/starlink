pack [canvas .c]
set rtd [image create rtdimage]
$rtd configure -file ngc1275.fits
set image [.c create image 0 0 -image $rtd -anchor nw]
