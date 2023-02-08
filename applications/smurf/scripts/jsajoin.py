#!/usr/bin/env python3

'''
*+
*  Name:
*     JSAJOIN

*  Purpose:
*     Create a single tangent-plane NDF from a set of JSA tiles.

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     This script pastes together one or more JSA tiles and then
*     resamples the resulting montage onto a tangent-plane projection
*     with square pixels and celestial north upwards. It can also
*     restrict the output NDF to a specified sub-section of this montage.
*
*     By default, the output NDF contains all the data from all the tiles
*     specified by parameter TILES. Alternatively, an existing NDF can be
*     specified for parameter REGION, in which case the output NDF will
*     contain only the data that falls within the supplied NDF. Another
*     way to specify the bounds of the output NDF is to give the centre
*     and radius of a circle on the sky using parameters CENTRE1, CENTRE2,
*     RADIUS and SYSTEM.
*
*     The pixel size of the output may be specified by parameter PIXSIZE,
*     but defaults to the nominal pixel size of the JSA tiles.

*  Usage:
*     jsajoin tiles out [centre1] [centre2] [radius] [system] [region]
*             [pixsize] [retain]

*  ADAM Parameters:
*     CENTRE1 = LITERAL (Read)
*        The formatted RA or Galactic longitude at the centre of a circle
*        that defines the required extent of the output NDF. See also
*        CENTRE2 and RADIUS. The coordinate system is specified by parameter
*        SYSTEM. It is only accessed if a null (!) value is supplied for
*        parameter REGION. If a null value is supplied for both CENTRE1 and
*        REGION, the output NDF will encompass all the data specified by
*        parameter TILES. [!]
*     CENTRE2 = LITERAL (Read)
*        The formatted Dec or Galactic latitude at the centre of a circle
*        that defines the required extent of the output NDF. See also
*        CENTRE1 and RADIUS. The coordinate system is specified by parameter
*        SYSTEM. It is only accessed if a null (!) value is supplied for
*        parameter REGION and a non-null value is supplied for parameter
*        CENTRE1.
*     INSTRUMENT = LITERAL (Read)
*        Selects the tiling scheme to be used. The following instrument
*        names are recognised (unambiguous abbreviations may be supplied):
*        "SCUBA-2(450)", "SCUBA-2(850)", "ACSIS", "DAS". If the first input
*        NDF contains JCMT data, the default value for this parameter is
*        determined from the FITS headers in the input NDF. Otherwise,
*        there is no default and an explicit value must be supplied. []
*     OUT = NDF (Read)
*        The output NDF.
*     PIXSIZE = _REAL (Read)
*        The pixel size to use for the output NDF, in arc-seconds. Not
*        used if an NDF is supplied for parameter REGION. If a null (!)
*        value is supplied, a default pixel size is used equal to the
*        geometric mean of the pixel dimensions in the middle tile
*        specified by parameter TILES. [!]
*     RADIUS = _DOUBLE (Read)
*        The radius of the circle that defines the required extent of the
*        output NDF, in arc-minutes. See also CENTRE1 and CENTRE2. It is
*        only accessed if a null (!) value is supplied for parameter REGION
*        and a non-null value is supplied for parameter CENTRE1.
*     REGION = LITERAL (Read)
*        Specifies the required extent of the output NDF. It can be
*        either a text file holding an AST Region description, or an NDF.
*        If it is an NDF, it also defines the WCS and pixel grid of the
*        output NDF. If a null (!) value is supplied, the region is
*        specified using parameter CENTRE1, CENTRE2 and RADIUS. [!]
*     RETAIN = _LOGICAL (Read)
*        Should the temporary directory containing the intermediate files
*        created by this script be retained? If not, it will be deleted
*        before the script exits. If retained, a message will be
*        displayed at the end specifying the path to the directory. [FALSE]
*     SYSTEM = LITERAL (Read)
*        The celestial coordinate system used by the CENTRE1 and CENTRE2
*        parameters. It can be either "ICRS" or "Galactic". The output NDF
*        inherits this same system as its current WCS Frame. ["ICRS"]
*     TILES = NDF (Read)
*        A group of NDFs each of which corresponds to a JSA tile. They
*        should all relate to a single instrument.

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-DEC-2013 (DSB):
*        Original version.
*     18-DEC-2013 (DSB):
*        Use Gauss interpolation kernel if output pixels are larger than
*        input pixels.
*     27-JAN-2014 (DSB):
*        Fix mapping of non-NDF regions onto the reference image.
*     30-JAN-2014 (DSB):
*        - Fix problem invoking kappa:paste if only one tile has been
*        supplied.
*        - Change sincsinc kernel width from 2 to 3.
*     26-SEP-2014 (DSB):
*        Use smurf:jsapaster instead of kappa:paste to stick the tiles
*        together.
*     2-OCT-2014 (DSB):
*        Resample the SMURF extension NDFs in the same way as the main NDF.
*-
'''

import math
import starutil
from starutil import invoke
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out

#  Assume for the moment that we will not be retaining temporary files.
retain = 0

#  Do not create a log file by default. Setting parameter GLEVEL=ATASK
#  will cause a logfile to be produced.
starutil.glevel = starutil.NONE

#  A function to clean up before exiting. Delete all temporary NDFs etc,
#  unless the script's RETAIN parameter indicates that they are to be
#  retained. Also delete the script's temporary ADAM directory.
def cleanup():
   global retain
   ParSys.cleanup()
   if retain:
      msg_out( "Retaining temporary files in {0}".format(NDG.tempdir))
   else:
      NDG.cleanup()


#  Catch any exception so that we can always clean up, even if control-C
#  is pressed.
try:

#  Declare the script parameters. Their positions in this list define
#  their expected position on the script command line. They can also be
#  specified by keyword on the command line. No validation of default
#  values or values supplied on the command line is performed until the
#  parameter value is first accessed within the script, at which time the
#  user is prompted for a value if necessary. The parameters "MSG_FILTER",
#  "ILEVEL", "GLEVEL" and "LOGFILE" are added automatically by the ParSys
#  constructor.
   params = []

   params.append(starutil.ParNDG("TILES", "The available JSA tiles" ))

   params.append(starutil.ParNDG("OUT", "The output image", exists=False,
                                 maxsize=1 ))

   params.append(starutil.Par0S("CENTRE1", "Longitude at centre of required "
                                "circular sky region", None, noprompt=True ))

   params.append(starutil.Par0S("CENTRE2", "Latitude at centre of required "
                                "circular sky region" ))

   params.append(starutil.Par0F("RADIUS", "Radius of the required "
                                "circular region on the sky (arc-mins)" ))

   params.append(starutil.ParChoice("SYSTEM", ["ICRS","GALACTIC"],
                                    "System in which centre is defined",
                                    "ICRS", noprompt=True))

   params.append(starutil.Par0S("REGION", "An NDF or AST Region defining "
                                "the require region of the sky", None,
                                noprompt=True ))

   params.append(starutil.Par0F("PIXSIZE", "Output pixel size (arcsec)", None,
                                 maxval=1000, minval=0.01, noprompt=True))

   params.append(starutil.ParChoice("INSTRUMENT",
                                    ["SCUBA-2(450)", "SCUBA-2(850)", "ACSIS",
                                    "DAS"],
                                    "The JCMT instrument", "SCUBA-2(850)"))

   params.append(starutil.Par0L("RETAIN", "Retain temporary files?", False,
                                 noprompt=True))

#  Initialise the parameters to hold any values supplied on the command
#  line.
   parsys = ParSys( params )

#  See if temp files are to be retained.
   retain = parsys["RETAIN"].value

#  Get any supplied Region defining the required sky region.
   region = parsys["REGION"].value

#  Get the list of tile NDFs that are available.
   tiles = parsys["TILES"].value

#  Get the JCMT instrument to which the tiles relate by looking at the
#  "INSTRUME", "BACKEND" and "FILTER" FITS headers in the first tile. Check
#  all tiles have the same value.
   instrument0 = None
   for tile in tiles:
      instrument = None

      cval = starutil.get_fits_header( tile, "INSTRUME" )
      if cval == "SCUBA-2":
         cval = starutil.get_fits_header( tile, "FILTER" )

         if cval == "450":
            instrument = "SCUBA-2(450)"

         elif cval == "850":
            instrument = "SCUBA-2(850)"

      else:
         cval = starutil.get_fits_header( tile, "BACKEND" )

         if cval == "ACSIS":
            instrument = "ACSIS"

         elif cval == "DAS":
            instrument = "DAS"

      if instrument0 is None:
         if instrument is not None:
            parsys["INSTRUMENT"].default = instrument
            parsys["INSTRUMENT"].noprompt = True
         instrument = parsys["INSTRUMENT"].value
         instrument0 = instrument

      elif instrument is not None:
         if instrument0 != instrument:
            raise starutil.InvalidParameterError("Tile {0} is for instrument "
                     "{1} - others are for {2}.".format(tile,instrument,instrument0) )
      else:
         instrument = instrument0;

   instrument = starutil.shell_quote( instrument )

#  Assume the Region is not an NDF.
   region_is_ndf = False
   lz = None
   uz = None

#  If no region was supplied, get the centre and radius of a circular
#  area on the sky, and create an AST Region from them.
   cen1 = None
   cen2 = None
   if region is None :
      system = parsys["SYSTEM"].value
      if system == "ICRS" :
         parsys["CENTRE1"].prompt = "RA at centre of required circle"
         parsys["CENTRE2"].prompt = "Dec at centre of required circle"
      else:
         parsys["CENTRE1"].prompt = "Galactic longitude at centre of required circle"
         parsys["CENTRE2"].prompt = "Galactic latitude at centre of required circle"

      centre1 = parsys["CENTRE1"].value
      if centre1 is not None:
         centre2 = parsys["CENTRE2"].value
         radius = parsys["RADIUS"].value

         frame = NDG.tempfile()
         invoke( "$ATOOLS_DIR/astskyframe \"'system={0}'\" {1}".format(system,frame) )

         invoke( "$ATOOLS_DIR/astunformat {0} 1 {1}".format(frame,centre1) )
         cen1 = starutil.get_task_par( "DVAL", "astunformat" )
         invoke( "$ATOOLS_DIR/astunformat {0} 2 {1}".format(frame,centre2) )
         cen2 = starutil.get_task_par( "DVAL", "astunformat" )

         region = NDG.tempfile()
         invoke( "$ATOOLS_DIR/astcircle {0} 1 \[{1},{2}\] {3} ! ! {4}".
                 format(frame,cen1,cen2,math.radians(radius/60.0),region) )

#  If a Region was supplied ,not we do not yet have the coordinates of
#  the centre of the required region, and note if the Region is defined by
#  an NDF.
   else:
      try:
         invoke( "$KAPPA_DIR/ndftrace {0} quiet".format(region) )
         region_is_ndf = True
         ndim = int( starutil.get_task_par( "NDIM", "ndftrace" ) )
         if ndim > 2:
            lz = int( starutil.get_task_par( "LBOUND(3)", "ndftrace" ) )
            uz = int( starutil.get_task_par( "UBOUND(3)", "ndftrace" ) )
      except:
         pass

#  Get the name of the output NDF.
   outdata = parsys["OUT"].value

#  Get a list of indices for the JSA tiles that intersect the required
#  region. If region is none, we are pasting all tiles into the output
#  NDF.
   if region is not None:
      invoke( "$SMURF_DIR/jsatilelist in={0} instrument={1}".
              format(region,instrument) )
      jsatile_list = starutil.get_task_par( "TILES", "jsatilelist" )

#  Get a dictionary in which each key is a tile number and each value is
#  the path to the NDF holding the tile.
      tile_dict = {}
      for tile in tiles:
         jsatile = starutil.get_fits_header( tile, "TILENUM" )
         if jsatile is None:
            raise starutil.InvalidParameterError("Supplied tile '{0}' has no value for "
                                                 "the TILENUM header.".format(tile) )
         else:
            tile_dict[ jsatile ] = tile

#  Create a list holding the paths to the tile NDFs that intersect
#  the required region.
      ntile = 0
      used_tile_list = []
      for jsatile in jsatile_list:
         key = str(jsatile)
         if key in tile_dict and tile_dict[ key ]:
            used_tile_list.append( tile_dict[ key ] )
            ntile += 1

#  Create an NDG holding the group of tile NDFs.
      if ntile > 0:
         msg_out( "{0} of the supplied tiles intersect the requested region.".format(ntile) )
         used_tiles = NDG( used_tile_list )
      else:
         raise starutil.InvalidParameterError( "None of the supplied JSA tiles "
                                               "intersect the requested region" )

#  If we are using all tiles, just use the supplied group of tiles. Use
#  the middle supplied tile as the reference.
   else:
      used_tiles = tiles
      jsatile = int( len(tiles)/2 )
      jsatile = starutil.get_fits_header( tiles[ jsatile ], "TILENUM" )

#  Paste these tile NDFs into a single image. This image still uses the
#  JSA all-sky pixel grid. If we have only a single tile, then just use
#  it as it is.
   if len(used_tiles) > 1:
      temp = NDG(1)
      invoke( "$SMURF_DIR/jsapaster in={0} out={1}".format(used_tiles,temp) )
   else:
      temp = used_tiles

#  Strip any bad border from the montage.
   jsa_montage = NDG(1)
   invoke( "$KAPPA_DIR/ndfcopy in={0} out={1} trimbad".format(temp,jsa_montage) )

#  Get the nominal pixel size of the montage.
   invoke( "$KAPPA_DIR/ndftrace {0} quiet".format(jsa_montage) )
   pixsize1 = float( starutil.get_task_par( "FPIXSCALE(1)", "ndftrace" ) )
   pixsize2 = float( starutil.get_task_par( "FPIXSCALE(2)", "ndftrace" ) )
   pixsize_jsa = math.sqrt( pixsize1*pixsize2 )

#  If the Region was specified as an NDF, use the NDF as the reference
#  pixel grid to which the tile data will be aligned.
   if region_is_ndf:
      ref = region

#  The bounds of the output image equals the bounds of the supplied NDF.
      invoke( "$KAPPA_DIR/ndftrace {0} quiet".format(region) )
      lx = starutil.get_task_par( "LBOUND(1)", "ndftrace" )
      ly = starutil.get_task_par( "LBOUND(2)", "ndftrace" )
      ux = starutil.get_task_par( "UBOUND(1)", "ndftrace" )
      uy = starutil.get_task_par( "UBOUND(2)", "ndftrace" )

#  If the Region was not specified as an NDF, we need to create a
#  reference NDF.
   else:

#  Choose the celestial reference position. Use the circle centre if
#  supplied. Otherwise use the centre of the middle tile, converted into
#  the requested system.
      if cen1 is None:
         if system == "ICRS":
            invoke( "$SMURF_DIR/jsatileinfo itile={0} instrument={1}".
                    format(jsatile,instrument) )
            cen1 = starutil.get_task_par( "RACEN", "jsatileinfo" )
            cen2 = starutil.get_task_par( "DECCEN", "jsatileinfo" )
         else:
            tile_header = NDG.tempfile()
            invoke( "$SMURF_DIR/jsatileinfo itile={0} instrument={1} header={2}".
                    format(jsatile,instrument,tile_header) )
            lbnd = starutil.get_task_par( "LBND", "jsatileinfo" )
            ubnd = starutil.get_task_par( "UBND", "jsatileinfo" )

            ix = ( ubnd[ 0 ] - lbnd[ 0 ] + 1 )/2
            iy = ( ubnd[ 1 ] - lbnd[ 1 ] + 1 )/2

            tile_wcs = NDG.tempfile()
            invoke( "$ATOOLS_DIR/astset {0} system {1} {2}".
                    format(tile_header,system,tile_wcs) )
            invoke( "$ATOOLS_DIR/asttran2 {0} {1} {2} YES".
                    format(tile_wcs,ix,iy) )
            cen1 = starutil.get_task_par( "XVAL", "asttran2" )
            cen2 = starutil.get_task_par( "YVAL", "asttran2" )

#  Set the nominal tile pixel size as the default for parameter PIXSIZE
#  (the output pixel size), then get a value from the user.
      parsys["PIXSIZE"].default = pixsize_jsa
      pixsize = parsys["PIXSIZE"].value

#  Create a 1x1 NDF to act as the reference.
      ref = NDG(1)
      invoke( "$KAPPA_DIR/creframe mode=bl out={0} lbound=\[1,1\] "
              "ubound=\[1,1\]".format(ref) )

#  Store suitable FITS-WCS values in the FITS extension of the reference
#  NDF.
      invoke( "$KAPPA_DIR/fitsmod {0} edit=write position=! comment=! keyword=CRPIX1 value=0.5".
              format(ref) )
      invoke( "$KAPPA_DIR/fitsmod {0} edit=write position=! comment=! keyword=CRPIX2 value=0.5".
              format(ref) )
      invoke( "$KAPPA_DIR/fitsmod {0} edit=write position=! comment=! keyword=CRVAL1 value={1}".
              format(ref,math.degrees(cen1)) )
      invoke( "$KAPPA_DIR/fitsmod {0} edit=write position=! comment=! keyword=CRVAL2 value={1}".
              format(ref,math.degrees(cen2)) )
      invoke( "$KAPPA_DIR/fitsmod {0} edit=write position=! comment=! keyword=CDELT1 value={1}".
              format(ref,-pixsize/3600) )
      invoke( "$KAPPA_DIR/fitsmod {0} edit=write position=! comment=! keyword=CDELT2 value={1}".
              format(ref,pixsize/3600) )
      if system == "ICRS":
         invoke( "$KAPPA_DIR/fitsmod {0} edit=write position=! comment=! keyword=CTYPE1 value=RA---TAN".
                 format(ref) )
         invoke( "$KAPPA_DIR/fitsmod {0} edit=write position=! comment=! keyword=CTYPE2 value=DEC--TAN".
                 format(ref) )
         invoke( "$KAPPA_DIR/fitsmod {0} edit=write position=! comment=! keyword=RADESYS value=ICRS".
                 format(ref) )
      else:
         invoke( "$KAPPA_DIR/fitsmod {0} edit=write position=! comment=! keyword=CTYPE1 value=GLON-TAN".
                 format(ref) )
         invoke( "$KAPPA_DIR/fitsmod {0} edit=write position=! comment=! keyword=CTYPE2 value=GLAT-TAN".
                 format(ref) )

#  We now need to decide on the bounds of the output image. Remap the
#  supplied Region into the pixel coords of the reference image. If no
#  region was supplied, use the full JSA tile montage. e need to pick the
#  spatial axes of the region first.
      if region is not None:
         this_reg = region
      else:
         this_reg = jsa_montage

      reg = NDG.tempfile()
      invoke( "$ATOOLS_DIR/astcopy {0} {1} class=region".format(this_reg,reg) )

      reg_2d = NDG.tempfile()
      invoke( "$ATOOLS_DIR/astpickaxes {0} \[1,2\] ! {1}".format(reg,reg_2d) )

#  We want the mapping from the Region's frame (icrs,galatic,etc) to the
#  grid coordinate system of the reference image. We use astConvert for
#  this. But astConvert converts to the current Frame of the target, so we
#  need to invert the reference FrameSet first so that grid coords becomes
#  the current Frame, rather than the base Frame.
      ref_fs = NDG.tempfile()
      invoke( "$ATOOLS_DIR/astinvert {0} {1}".format(ref,ref_fs) )

#  Now use astConvert to get a FrameSet in which the base Frame is the
#  region's frame, the current Frame is the reference GRID system, and
#  the alignment occurs in sky coordinates.
      tmp_fs = NDG.tempfile()
      invoke( "$ATOOLS_DIR/astconvert {0} {1} SKY {2}".format(reg_2d,ref_fs,tmp_fs) )


#  Get the mapping from the region's frame to the reference GRID system.
      map = NDG.tempfile()
      invoke( "$ATOOLS_DIR/astgetmapping {0} AST__BASE AST__CURRENT {1}".
                 format(tmp_fs,map) )
      frm = NDG.tempfile()
      invoke( "$ATOOLS_DIR/astgetframe {0} AST__CURRENT {1}".format(tmp_fs,frm) )
      mapped_reg = NDG.tempfile()
      invoke( "$ATOOLS_DIR/astmapregion {0} {1} {2} {3}".
              format(reg_2d,map,frm,mapped_reg) )

#  Get the bounds of this region in the pixel coord system of the reference.
      invoke( "$ATOOLS_DIR/astgetregbounds {0}".format(mapped_reg) )
      lx = int( starutil.get_task_par( "LBND(1)", "astgetregbounds" ) )
      ly = int( starutil.get_task_par( "LBND(2)", "astgetregbounds" ) )
      ux = int( starutil.get_task_par( "UBND(1)", "astgetregbounds" ) )
      uy = int( starutil.get_task_par( "UBND(2)", "astgetregbounds" ) )

#  Get the nominal pixel size of the reference map.
   invoke( "$KAPPA_DIR/ndftrace {0} quiet".format(ref) )
   pixsize1 = float( starutil.get_task_par( "FPIXSCALE(1)", "ndftrace" ) )
   pixsize2 = float( starutil.get_task_par( "FPIXSCALE(2)", "ndftrace" ) )
   pixsize_ref = math.sqrt( pixsize1*pixsize2 )

#  If the output pixels are smaller than the tile pixels, we use a
#  SincSinc interpolation kernel with width equal to 3 JSA pixels.
   if pixsize_ref < 1.5*pixsize_jsa:
      method = "sincsinc"
      width = 3.0

#  If the output pixels are larger than the tile pixels, we use a
#  Gauss interpolation kernel with width equal to 0.8 output pixels (se we
#  need to convert 2 output pixels into the equivalent number of input
#  pixels, as required for wcsalign). Simulations seem to suggest 0.8 is
#  a good figure to use. See SCUBA-2 TRAC ticket #1333.
   else:
      method = "gauss"
      width = 0.8*pixsize_ref/pixsize_jsa

#  Create a group of NDFs that need to be resampled. This is the
#  combined NDF holding all tiles, plus any NDFs in its SMURF
#  extension.
   subndfs = NDG( "{0}.more.smurf".format(starutil.shell_quote(jsa_montage[0],True)) )
   ndfstoresample = NDG( [jsa_montage, subndfs ] )

#  Get a corresponding group of output NDFs.
   if region:
      out = outdata
   else:
      out = NDG(1)

   ndf_list = []
   for ndf in ndfstoresample:
      ndf_list.append( ndf.replace( jsa_montage[0], out[0] ) )
   resampledndfs = NDG( ndf_list )

#  Create the output NDF by resampling the combined NDF holding all
#  tiles. Do 2D and 3D separately.
   if lz is None:
      invoke( "$KAPPA_DIR/wcsalign in={0} out={1} ref={2} lbnd=\[{3},{4}\] "
              "ubnd=\[{5},{6}\] method={7} params=\[0,{8}\]".
              format(ndfstoresample,resampledndfs,ref,lx,ly,ux,uy,method,width) )
   else:
      invoke( "$KAPPA_DIR/wcsalign in={0} out={1} ref={2} lbnd=\[{3},{4},{5}\] "
              "ubnd=\[{6},{7},{8}\] method={9} params=\[0,{10}\]".
              format(ndfstoresample,resampledndfs,ref,lx,ly,lz,ux,uy,uz,method,width) )

#  If using all input tiles, strip any bad border from the output.
   if region is None:
      invoke( "$KAPPA_DIR/ndfcopy in={0} out={1} trimbad exten=yes".format(out,outdata) )

#  Remove temporary files.
   cleanup()

#  If an StarUtilError of any kind occurred, display the message but hide the
#  python traceback. To see the trace back, uncomment "raise" instead.
except starutil.StarUtilError as err:
#  raise
   print( err )
   cleanup()

# This is to trap control-C etc, so that we can clean up temp files.
except:
   cleanup()
   raise

