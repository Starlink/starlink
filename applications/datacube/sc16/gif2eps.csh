#!/bin/csh

# The Starlink CONVERT package is setup for use,  so unalias the
# "convert" command

unalias convert

# Do the image conversion, note the image shrinkage is different for each image

#
# version 1.0
#
convert -page A4 -geometry 100% sc16.htx/sc16_atv.gif eps:sc16_atv.eps
convert -page A4 -geometry 100% sc16.htx/sc16_compare.gif eps:sc16_compare.eps
convert -page A4 -geometry 100% sc16.htx/sc16_cover.gif eps:sc16_cover.eps
convert -page A4 -geometry 100% sc16.htx/sc16_gaia_cubetool.gif eps:sc16_gaia_cubetool.eps
convert -page A4 -geometry 100% sc16.htx/sc16_gaia_spectrum.gif eps:sc16_gaia_spectrum.eps
convert -page A4 -geometry 100% sc16.htx/sc16_mosaic.gif eps:sc16_mosaic.eps
convert -page A4 -geometry 100% sc16.htx/sc16_passband.gif eps:sc16_passband.eps
convert -page A4 -geometry 100% sc16.htx/sc16_peakmap.gif eps:sc16_peakmap.eps
convert -page A4 -geometry 100% sc16.htx/sc16_ripper.gif eps:sc16_ripper.eps
convert -page A4 -geometry 100% sc16.htx/sc16_slicer3_cut.gif eps:sc16_slicer3_cut.eps
convert -page A4 -geometry 100% sc16.htx/sc16_slicer3_probe.gif eps:sc16_slicer3_probe.eps
convert -page A4 -geometry 100% sc16.htx/sc16_squash.gif eps:sc16_squash.eps
convert -page A4 -geometry 100% sc16.htx/sc16_stacker.gif eps:sc16_stacker.eps
convert -page A4 -geometry 100% sc16.htx/sc16_step.gif eps:sc16_step.eps
convert -page A4 -geometry 100% sc16.htx/sc16_uist.gif eps:sc16_uist.eps
convert -page A4 -geometry 100% sc16.htx/sc16_velmap.gif eps:sc16_velmap.eps
convert -page A4 -geometry 100% sc16.htx/sc16_velmoment.gif eps:sc16_velmoment.eps
convert -page A4 -geometry 100% sc16.htx/sc16_xvelmap.gif eps:sc16_xvelmap.eps
convert -page A4 -geometry 100% sc16.htx/sc16_xvelmap2.gif eps:sc16_xvelmap2.eps

