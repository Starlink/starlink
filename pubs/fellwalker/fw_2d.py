#!/usr/bin/env python

#  Do "setenv PYTHONPATH $SMURF_DIR" before using this script.
#
#  This script generates artificial 2D clumps using cupid:makeclumps,
#  runs fellwalker on the data, and produces a table holding an analysis
#  of the fellwalker results compared to the known properties of the
#  original clumps. The results in file "table.asc" can be fiewed using
#  "topcat -f ascii".
#
#  Each row in table.asc describes the results of a single run of
#  fellwalker on a single set of artificial clumps. The clumps within
#  each set (produced by makeclumps) are all circular, all have the the
#  same peak value, all have the same size. Different sets of clumps have
#  different peak values and different mean separations between clumps
#  (although the clump positions are random).
#
#  The columns in table.asc are:
#
#  SNR: peak value divided by noise for the real clumps.
#  sep: mean separation between real clumps as a multiple of the size of the
#       clumps
#  ffound: The fraction of real clumps which fellwalker found within
#          half a clump width (0.5*FWHM) of their true position.
#  ffalse: The number of false clumps invented by fellwalker as a
#          fraction of the number of real clumps.
#  peak_mean: The mean clump peak value found by Fellwalker as a fraction
#             of the true peak value.
#  peak_sigma: The standard deviation of clump peak values found by
#              Fellwalker as a fraction of the true peak value.
#  size_mean: The mean clump width found by Fellwalker as a fraction
#             of the true clump width.
#  size_sigma: The standard deviation of clump widths found by
#              Fellwalker as a fraction of the true clump width.


#  STILL A LOT OF WORK TO BE DONE ON THIS SCRIPT !!!


import sys
import re
import os
import math
import starutil
from starutil import invoke
from starutil import NDG

starutil.ilevel=starutil.ATASK
starutil.glevel=starutil.ATASK

os.environ["AUTOPROV"] = "0"
os.environ["NDF_AUTO_HISTORY"] = "0"
os.environ["STAR_SEED"] = "4"

#  Open the text file to store the results as a TOPCAT ascii table
table = open( "table.asc", "w" )
table.write("# SNR sep ffound ffalse peak_mean peak_sigma size_mean size_sigma\n")

#  Fixed noise level
noise = 1.0

#  Target number of clumps
nclump_target = 500.0

#  Fixed clump size (FWHM in pixels on all axes)
clump_fwhm = 10

#  Initial mean clump separation in pixels
clump_separation = clump_fwhm/2.0

#  Do tests for 5 different separations
for isep in range(0, 1):

#  Initial peak value
   peak_value = noise*0.5

#  Do tests for 5 different peak values
   for ipeak in range(0, 1):
      starutil.msg_out( ">>> Doing sep={0} and peak={1}....".format(clump_separation,peak_value))

#  Get the dimensions of a square image that would be expected to
#  contain the target number of clumps at the current separation.
      npix = int( clump_separation*math.sqrt( nclump_target ) )

#  Create a temporary file containing circular clumps of constant size
#  and shape (except for the effects of noise).
      model = NDG(1)
      out = NDG(1)
      outcat = NDG.tempfile(".fit")
      invoke( "$CUPID_DIR/makeclumps angle=\[0,0\] beamfwhm=0 deconv=no "
              "fwhm1=\[{0},0\] fwhm2=\[{0},0\] lbnd=\[1,1\] ubnd=\[{1},{1}\] "
              "model={2} nclump={3} out={4} outcat={5} pardist=normal "
              "peak = \[{6},0\] rms={7} trunc=0.1".
               format(clump_fwhm,npix,model,nclump_target,out,outcat,
                      peak_value,noise) )

#  Run fellwalker on the data.
      mask = NDG(1)
      outcat_fw = NDG.tempfile(".fit")
      invoke( "$CUPID_DIR/findclumps config=def deconv=no in={0} "
              "method=fellwalker out={1} outcat={2} rms={3}".
               format(out,mask,outcat_fw,noise) )

# Get the number of clumps found by FellWalker.
      nfw = starutil.get_task_par( "nclumps", "findclumps" )
      if nfw > 0:

#  See how many of the clump peaks found by FellWalker match real clumps to
#  within 0.2 pixels.
         text = invoke( "stilts tmatch2 matcher=2d params={2} "
                        "in1={0} ifmt1=fits values1='Peak1 Peak2' "
                        "in2={1} ifmt2=fits values2='Peak1 Peak2' "
                        "scorecol=sep ocmd='keepcols sep' "
                        "omode=stats".format( outcat, outcat_fw,
                                              0.5*clump_fwhm ),aslist=True )

         ok = False
         for line in text:
            match = re.match( r'columns:\s*(\d+)\s*rows:\s*(\d+)', line )
            if match:
               nmatch = int( match.group(2) )
               ok = True

         if not ok:
            print"Cannot find rows/cols from stilts"
            sys.exit()


#  Get the fraction of real clumps that were found.
         ffound = nmatch/nclump_target

#  Get the fraction of false clump detections.
         ffalse = (nfw - nmatch)/nclump_target

#  Find the mean and sigma of the peak values found by fellwalker,
#  normalised to the true peak value.
         text = invoke( "stilts tpipe in={0} ifmt=fits omode=stats "
                        "cmd='keepcols Peak'".format( outcat_fw ),aslist=True )
         ok = False
         for line in text:
            match = re.match( r'\s*\|\s*Peak\s*\|\s*([0-9.+-E]+)\s*\|\s*([0-9.+-E]+)\s*\|', line )
            if match:
               mean = float( match.group(1) )
               sigma = float( match.group(2) )
               ok = True

         if not ok:
            print"Cannot find rows/cols from stilts"
            sys.exit()

         peak_mean = mean/peak_value
         peak_sigma = sigma/peak_value

#  Find the mean and sigma of the size values found by fellwalker,
#  normalised to the true size value.
         text = invoke( "stilts tpipe in={0} ifmt=fits omode=stats "
                        "cmd='addcol msize \"sqrt(Size1*Size2)\"' cmd='keepcols msize'"
                        .format( outcat_fw ),aslist=True )


         ok = False
         for line in text:
            match = re.match( r'\s*\|\s*msize\s*\|\s*([0-9.+-E]+)\s*\|\s*([0-9.+-E]+)\s*\|', line )
            if match:
               mean = float( match.group(1) )
               sigma = float( match.group(2) )
               ok = True

         if not ok:
            print"Cannot find rows/cols from stilts"
            sys.exit()

         size_mean = 2.355*mean/clump_fwhm
         size_sigma = 2.355*sigma/clump_fwhm


#  Write the values out to a text file in TOPCAT "ascii" format
         table.write("{0} {1} {2} {3} {4} {5} {6} {7}\n".
                     format(peak_value/noise,
                            clump_separation/clump_fwhm, ffound, ffalse,
                            peak_mean, peak_sigma, size_mean, size_sigma) )

# Update peak value for next test.
      peak_value *= 2


# Update clump separation for next test.
   clump_separation *= 2

#  Close the results file
table.close()

