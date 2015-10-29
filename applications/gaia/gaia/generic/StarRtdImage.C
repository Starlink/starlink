/*+
 *  Name:
 *     StarRtdImage

 *  Language:
 *     C++

 *  Purpose:
 *     Defines the members of the StarRtdImage class.

 *  Authors:
 *     P.W. Draper (PWD)
 *     Allan Brighton, ESO (ALLAN)
 *     Tim Jenness, Cornell University (TIMJ)

 *  Copyright:
 *     Copyright (C) 1997-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006-2007 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2008-2009 Science and Technology Facilities Council.
 *     Copyright (C) 2014 Cornell University.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  History:
 *     15-FEB-1996 (PWD):
 *        Original version.
 *     4-APR-1996 (PWD):
 *        Added pix2screen command to convert image coordinates to
 *        screen coordinates.
 *     30-NOV-1996 (PWD):
 *        Added origin command and usingxshm.
 *     17-JAN-1996 (PWD):
 *        Changed loadImage command to clear image when file is "".
 *        Previously this deleted the image too (leaving none).
 *     19-AUG-1997 (PWD):
 *        Added plotgrid method.
 *     28-AUG-1997 (PWD):
 *        Converted to work with RTD 2.18.3. The shared memory copies
 *        of the headers and data no longer seem to be available.
 *     16-SEP-1997 (PWD):
 *        Added checks to deal with image/NDF slices.
 *     06-OCT-1997 (PWD):
 *        Modified to create FitsIO and NDFIO objects directly. This
 *        decouples the FitsIO classes and NDFIO classes from
 *        ImageData (and ImageIO) which allows us to subclass ImageIO
 *        and use the ImageData ABC without modifying the code.
 *        The loadImage member function becomes loadFile (which is now
 *        virtual) and we control configureImage completely at this
 *        level (this is needed to parse the options, plus the
 *        ast_tag item that we need). The RtdImage constructor is
 *        also changed, as we need to set the image name, the widget
 *        options and avoid some pitfalls with the RtdImage
 *        constructor calling loadFile (via the initImage member).
 *     17-OCT-1997 (PWD):
 *        Started changes to support the modification of Astrometry
 *        information in the WCS.
 *     16-Mar-1998 (ALLAN):
 *        Make use of new Skycat/Rtd features supporting subclassing,
 *        New constructor args to support subclassing,
 *        Changed image type from "starrtdimage" to "rtdimage",
 *        Added GAIA_CONFIG macro for Tk config options,
 *        Renamed OPTION() macro to GAIA_OPTION (Tk config options),
 *
 *        Renamed WCS to StarWCS, which is now a subclass of WCSRep,
 *        for compatibility with Rtd/Skycat, and added the method
 *        "getStarWCSPtr(image)" to access the StarWCS class ptr inside
 *        the WCS (reference counted) class.
 *
 *        Added getStarImage() method, to avoid duplicate code to read an
 *        image file and setup the right WCS subclass.
 *
 *        Added class StarFitsIO to replace GAIA version of FitsIO and
 *        remain compatible with the Skycat version.
 *     22-APR-1998 (PWD):
 *        Now subclasses Skycat rather than RtdImage. Changed
 *        draw_ellipse method to use rtd_ellipse.
 *     27-APR-1998 (ALLAN):
 *        Modified isNDFtype() and getStarImage() to check for all FITS
 *        suffixes (anything including "fit", such as fits.gz, fits.Z,
 *        cfits, gzfits, etc.).
 *     23-JUN-1998 (PWD):
 *        Added command for retrieving a file from a web server.
 *     16-JUL-1998 (PWD):
 *        Added slice command. This extends spectrum to also return
 *        the X and Y coordinates used as positions along the line.
 *        Added command to control the colour of blank pixels.
 *     04-NOV-1998 (PWD):
 *        Added override for get_compass member. This allows the
 *        images that have a WCS component to not use it when scaling
 *        and orienting the plotting symbols. This is much faster, and
 *        OK when dealing with detections for images that are
 *        displayed (i.e. the SExtractor toolbox). Also added -plotwcs
 *        configuration option to control this.
 *     13-NOV-1998 (PWD):
 *        Added rotbox to plotting symbols.
 *     13-JAN-1999 (PWD):
 *        Merged Allan Brighton's GAIA plugins changes (see history above).
 *     06-APR-1999 (PWD):
 *        Added contour command. Lots of restructuring of gridplot
 *        command.
 *     14-JUN-1999 (PWD):
 *        Added "hdu" command to stop use of this facility. There is a
 *        fundermental problem with memory mapped FITS files that
 *        stops the use of these commands (basically the disk file is
 *        not available to add/read HDUs). When time permits this
 *        problem should be worked around.
 *     12-JUL-1999 (PWD):
 *        Added changes to sliceCmd to replace blank pixels by the
 *        mean along the slice. Added percentiles command.
 *     14-JUL-1999 (PWD):
 *        Added gbandCmd to display image pixels and non-RA/Dec
 *        coordinates, otherwise this is exactly the same as
 *        RtdImage::mbandCmd.
 *     15-JUL-1999 (PWD):
 *        Changed sliceCmd to not flip the line ends when passing
 *        through horizonal and vertical.
 *     25-AUG-1999 (PWD):
 *        Added changes to support NDFs stored within container files.
 *     15-OCT-1999 (PWD):
 *        Added changes to astsystem to select a pixel coordinate system.
 *     17-NOV-1999 (PWD):
 *        Changed dumpCmd to attempt to write a native encoding of the
 *        WCS, if the default encoding isn't Native (this happens -
 *        sometimes - when the channel contains a known WCS that is
 *        illegal?).
 *     12-JAN-2000 (PWD):
 *        Returned to hdu command. Now enabled for FITS images and
 *        NDFs. NDFs do not support the full range of commands as no
 *        catalogue access is available. The fully qualified names of
 *        the displayed image is now returned via the "fullname"
 *        command, which should be used in preference to the "-file"
 *        option, which may just name the container file.
 *     01-FEB-2000 (PWD):
 *        Override RTD "remote" command so that we use GaiaRtdRemote
 *        instead of RtdImageRemote class. This cleans up the control
 *        of the ~/.rtd-remote file (which can become invalid when
 *        main windows are closed).
 *     16-MAR-2000 (PWD):
 *        Override "remotetcl" command. This adds error status return
 *        if command fails.
 *     27-MAR-2000 (PWD):
 *        Added globalstats command. Performs same job as statistics
 *        command except on a list of X,Y positions.
 *     08-MAY-2000 (PWD):
 *        Modified astsystem command to transform between current
 *        frame and new celestial coordinates. Also added asttran2
 *        command to transform coordinates between such frames.
 *     23-MAY-2000 (PWD):
 *        Added astwarnings command. These return the content of any
 *        ASTWARN cards produced when the WCS is set up.
 *     10-JUL-2000 (PWD):
 *        Added XY profile command.
 *     04-APR-2001 (PWD):
 *        Added astaddcolour command.
 *     15-MAY-2001 (PWD):
 *        Changed method for estimating contour region.
 *     08-AUG-2001 (PWD):
 *        Fixed problem with native objects being written at start of
 *        FITS headers (actually turned out to be an AST bug).
 *     27-JAN-2003 (PWD):
 *        Added facilities to support the display of coordinates at
 *        milli-arcsecond resolution.
 *     12-SEP-2003 (PWD):
 *        Added the slalib command. Currently only offers the ability
 *        to get a list of known observatories. A proper
 *        implementation would see this command refactored to a
 *        standalone Tcl command and more SLALIB facilities offered,
 *        but time presses.
 *     19-DEC-2003 (PWD):
 *        Added astcarlin command. Controls the CarLin attribute
 *        used when reading a FITS channel.
 *     16-FEB-2004 (PWD):
 *        Added astalwaysmerge command. Controls whether primary headers are
 *        merged with extension before looking for a WCS.
 *     28-OCT-2004 (PWD):
 *        Added direct NDF access commands. These are used to access cubes.
 *     14-DEC-2005 (PWD):
 *        Various update for Skycat 2.7.4.
 *     21-MAR-2006 (PWD):
 *        Removed direct NDF access commands. These are now available
 *        via the ndf:: commands. See gaiaNDFTcl.
 *     30-MAR-2006 (PWD):
 *        Added replaceImageDataCmd.
 *     26-APR-2006 (PWD):
 *        Added objectCmd and volatileCmd.
 *     08-JAN-2008 (PWD):
 *        Removed astaddcolour & astfontresize (moved into gaiautils::).
 *     08-APR-2008 (PWD):
 *        Add -pixel_indices support.
 *     09-JUN-2009 (PWD):
 *        Add imagedataCmd.
 *     19-JUN-2009 (PWD):
 *        Add blankvalueCmd.
 *     17-AUG-2009 (PWD):
 *        Added astgetcloneCmd.
 *     10-FEB-2010 (PWD):
 *        Added forcedegrees command. Sets whether to display decimal
 *        degrees for all celestial coordinate systems.
 *     02-SEP-2014 (TIMJ):
 *        Use DAT__FLEXT rather than explicit ".sdf"
 *-
 */
#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>
#include <cstdlib>
#include <csignal>
#include <cstdio>
#include <iostream>
#include <sstream>
#include <fstream>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/sem.h>
#include <cstring>
#include <fcntl.h>
#include <unistd.h>
#include <cmath>
#include <cassert>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include <float.h>
#include <tcl.h>
#include <tk.h>
#include <blt.h>
#include <bltVector.h>
#include <netinet/in.h>

#include "define.h"
#include "util.h"
#include "ImageIO.h"
#include "StarFitsIO.h"
#include "NDFIO.h"
#include "ImageData.h"
#include "StarRtdImage.h"
#include "Contour.h"
#include "XYProfile.h"
#include "XYHistogram.h"
#include "RegionStats.h"
#include <ast.h>
#include "grf_tkcan.h"
#include "tcl_err.h"
extern "C" {
#include "ast_tclerr.h"
#include "dat_par.h"
}
#include "rtdDtrn.h"
#include "GaiaRtdRemote.h"
#include "gaiaNDF.h"
#include "gaiaUtils.h"
#include <star/slalib.h>
#include "GaiaArray.h"

// Include any foreign commands. These are processed by the "foreign"
// member function when requested.
#include "StarRtdForeignCmds.h"

//  Size of buffer used temporary filenames.
static const int FILEBUFSIZE = 1024;

//  Number of characters in a FITS header card.
static const int FITSCARD = 80;

//  Trig conversion factors.
static const double pi_ = 3.14159265358979323846;
static const double r2d_ = 180.0/pi_;
static const double d2r_ = pi_/180.0;

//  StarRtdImageSubCmds
//
//  Declare a table of image subcommands and the methods that handle them.
//
//-
static class StarRtdImageSubCmds {
public:
    char const *name;                                  // Method name
    int (StarRtdImage::*fptr)(int argc, char* argv[]); // Ptr to method
    int min_args;                                      // Min number of args
    int max_args;                                      // Max number of args
} subcmds_[] = {
    { "astalwaysmerge",  &StarRtdImage::astalwaysmergeCmd,  1, 1 },
    { "astassign",       &StarRtdImage::astassignCmd,       7, 7 },
    { "astbootstats",    &StarRtdImage::astbootstatsCmd,    4, 4 },
    { "astcarlin",       &StarRtdImage::astcarlinCmd,       1, 1 },
    { "astcelestial",    &StarRtdImage::astcelestialCmd,    0, 0 },
    { "astcopy",         &StarRtdImage::astcopyCmd,         1, 1 },
    { "astcreate",       &StarRtdImage::astcreateCmd,       0, 0 },
    { "astcur2pix",      &StarRtdImage::astcur2pixCmd,      2, 3 },
    { "astdelete",       &StarRtdImage::astdeleteCmd,       1, 1 },
    { "astdomains",      &StarRtdImage::astdomainsCmd,      0, 1 },
    { "astfix",          &StarRtdImage::astfixCmd,          0, 0 },
    { "astget",          &StarRtdImage::astgetCmd,          1, 1 },
    { "astgetclone",     &StarRtdImage::astgetcloneCmd,     0, 0 },
    { "astmilli",        &StarRtdImage::astmilliCmd,        1, 1 },
    { "astpix2cur",      &StarRtdImage::astpix2curCmd,      2, 2 },
    { "astpix2wcs",      &StarRtdImage::astpix2wcsCmd,      2, 4 },
    { "astread",         &StarRtdImage::astreadCmd,         1, 1 },
    { "astrefine",       &StarRtdImage::astrefineCmd,       4, 4 },
    { "astreplace",      &StarRtdImage::astreplaceCmd,      0, 1 },
    { "astreset",        &StarRtdImage::astresetCmd,        1, 1 },
    { "astrestore",      &StarRtdImage::astrestoreCmd,      0, 1 },
    { "astset",          &StarRtdImage::astsetCmd,          2, 2 },
    { "aststore",        &StarRtdImage::aststoreCmd,        3, 5 },
    { "astsystem",       &StarRtdImage::astsystemCmd,       2, 4 },
    { "asttran2",        &StarRtdImage::asttran2Cmd,        2, 2 },
    { "astwarnings",     &StarRtdImage::astwarningsCmd,     0, 0 },
    { "astwcs2pix",      &StarRtdImage::astwcs2pixCmd,      2, 2 },
    { "astwrite",        &StarRtdImage::astwriteCmd,        1, 3 },
    { "autosetcutlevels",&StarRtdImage::autosetcutlevelsCmd,0, 0 },
    { "biasimage",       &StarRtdImage::biasimageCmd,       0, 3 },
    { "blankcolor",      &StarRtdImage::blankcolorCmd,      1, 1 },
    { "colorramp",       &StarRtdImage::colorrampCmd,       0, 2 },
    { "contour",         &StarRtdImage::contourCmd,         1, 7 },
    { "dump",            &StarRtdImage::dumpCmd,            1, 2 },
    { "forcedegrees",    &StarRtdImage::forcedegreesCmd,    1, 1 },
    { "foreign",         &StarRtdImage::foreignCmd,         2, 2 },
    { "fullname",        &StarRtdImage::fullNameCmd,        0, 0 },
    { "gband",           &StarRtdImage::gbandCmd,           6, 6 },
    { "globalstats",     &StarRtdImage::globalstatsCmd,     2, 2 },
    { "blankvalue",      &StarRtdImage::blankvalueCmd,      0, 1 },
    { "hdu",             &StarRtdImage::hduCmd,             0, 6 },
    { "imagedata",       &StarRtdImage::imageDataCmd,       0, 0 },
    { "iscompound",      &StarRtdImage::isCompoundCmd,      0, 0 },
    { "isfits",          &StarRtdImage::isfitsCmd,          0, 0 },
    { "origin",          &StarRtdImage::originCmd,          2, 3 },
    { "percentiles",     &StarRtdImage::percentCmd,         1, 1 },
    { "plotgrid",        &StarRtdImage::plotgridCmd,        0, 2 },
    { "readonly",        &StarRtdImage::readonlyCmd,        0, 1 },
    { "remote",          &StarRtdImage::remoteCmd,          0, 1 },
    { "remotetcl",       &StarRtdImage::remoteTclCmd,       1, 1 },
    { "object",          &StarRtdImage::objectCmd,          0, 1 },
    { "slice",           &StarRtdImage::sliceCmd,          11, 11},
    { "stcplot",         &StarRtdImage::stcplotCmd,         1, 2 },
    { "replaceimagedata",&StarRtdImage::replaceImageDataCmd, 1, 1 },
    { "usingxshm",       &StarRtdImage::usingxshmCmd,       0, 0 },
    { "volatile",        &StarRtdImage::volatileCmd,        0, 1 },
    { "xyprofile",       &StarRtdImage::xyProfileCmd,      14, 14},
    { "xyhistogram",     &StarRtdImage::xyHistogramCmd,    13, 13}
};


//+
//  starRtdImageType
//
//  Creation and initialization of the image control structure
//  with pointers to the handler functions. Registers the name of
//  the image creation command.
//
//-
static Tk_ImageType starRtdImageType = {
    "rtdimage",                          /* name */
    StarRtdImage::CreateImage,           /* createProc */
    TkImage::GetImage,                   /* getProc */
    TkImage::DisplayImage,               /* displayProc */
    TkImage::FreeImage,                  /* freeProc */
    TkImage::DeleteImage,                /* deleteProc */
    (Tk_ImagePostscriptProc *) NULL,     /* postscriptProc */
    (Tk_ImageType *) NULL,               /* nextPtr */
    (char *) NULL                        /* reserved */
};


//+
// configSpecs_
//
// Image config options - used to process command line options and for the
// image "configure" subcommand. (ALLAN)
//-
static Tk_ConfigSpec configSpecs_[] = {
    RTD_OPTIONS,                       // See RtdImage.h
    GAIA_OPTIONS,                      // See StarRtdImage.h
    {TK_CONFIG_END, NULL, NULL, NULL, NULL, 0, 0, NULL}
};

//  Function to use as source for an AST channel. See also the
//  channelData member.
static const char *ChannelSource()
{
    ChannelData *channelData = (ChannelData *) astChannelData;
    if ( channelData->read == 0 ) {
        channelData->read = 1;
        return channelData->content;
    }
    return NULL;
};

//+
//  Gaia_Init
//
//  Function to perform startup initialisation (called from tkAppInit).
//
//  Arguments:
//     Tcl_Interp *interp    - TCL interpreter
//
//  Return:
//     TCL status.
//-
extern "C" int StarRtd_Init( Tcl_Interp *interp )
{
#ifdef _DEBUG_
    cout << "Called StarRtd_Init" << endl;
#endif

    // Add our new images type.
    Tk_CreateImageType( &starRtdImageType );
    return TCL_OK;
}

//+
//  StarRtdImage::CreateImage
//
//  Function to create a new "rtdimage" Tk image with extensions for GAIA.
//  This function is called directly by Tk image code.
//
//  Arguments:
//     Tcl_Interp *interp,       - Tcl interpreter
//     char *name,               - Name of image
//     int argc,                 - Number of strings in argv
//     char *argv[],             - Image options
//     Tk_ImageType *typePtr,    - Pointer to type record
//     Tk_ImageMaster master,    - Image token, used to identify
//                                 this image back to Tk
//     ClientData *clientDataPtr - Pointer for image information
//
//  Return:
//    TCL status.
//-
int StarRtdImage::CreateImage( Tcl_Interp *interp,
                               char *name,
                               int argc,
                               Tcl_Obj *CONST objv[], // Argument objects for
                                                      // options (not
                                                      // including image name
                                                      // or type)
                               Tk_ImageType *typePtr,
                               Tk_ImageMaster master,
                               ClientData *clientDataPtr )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::CreateImage" << endl;
#endif

    // Generate an argv from the objv argument
    char* argv[64];  // there shouldn't be more than a few options...
    for(int i = 0; i < argc; i++)
        argv[i] = Tcl_GetString(objv[i]);
    argv[argc] = NULL;

    //  Now Create the image.
    StarRtdImage *im = new StarRtdImage( interp, name, argc, argv, master,
                                         starRtdImageType.name,
                                         configSpecs_,
                                         new StarRtdImageOptions() );
    if (im && im->status() == TCL_OK) {
        *clientDataPtr = (ClientData) im;
        return im->initImage(argc, argv);  // ALLAN: moved here from constructor
    }
    return TCL_ERROR;
}

//+
//  StarRtdImage::StarRtdImage
//
//  Constructor:
//     Also invokes RtdImage constructor for derived classes.
//
//  Arguments:
//     Tcl_Interp *interp           - TCL interpreter
//     StarRtdImageOptions *options - options structure, used to
//                                    override those defined in RtdImage
//     char *name,                  - Name of command
//     int argc,                    - Number of arguments
//     char *argv[],                - Command arguments
//     Tk_ImageMaster master        - Image master reference (passed back to TK).
//
//  Return:
//     Nothing.
//
//-
StarRtdImage::StarRtdImage(Tcl_Interp* interp, const char* instname,
                           int argc, char** argv,
                           Tk_ImageMaster master, const char* imageType,
                           Tk_ConfigSpec* specs, StarRtdImageOptions* options)
    : Skycat(interp, instname, argc, argv, master, imageType, specs, options),
      origset_(NULL),
      newset_(NULL),
      oldset_(NULL),
      stcMapping_(NULL)
{
#ifdef _DEBUG_
    cout << "Created StarRtdImage object " << std::endl;
#endif

    // Define the TCL interpreter for any AST errors.
    errTcl_Init( interp );

    // Initialise all the Channel slots.
    for ( int i=0; i < MAX_CHANNELS; i++ ) channels_[i] = NULL;

    // The image data is fixed at first.
    volatile_ = 0;
}


//+
//  StarRtdImage::~StarRtdImage
//
//  Destructor
//-
StarRtdImage::~StarRtdImage()
{
#ifdef _DEBUG_
    cout << "Destroying StarRtdImage object " << std::endl;
#endif
    //  Release any local FrameSets.
    if ( newset_ ) {
        newset_ = (AstFrameSet *) astAnnul( newset_ );
    }
    if ( oldset_ ) {
        oldset_ = (AstFrameSet *) astAnnul( oldset_ );
    }
    if ( origset_ ) {
        origset_ = (AstFrameSet *) astAnnul( origset_ );
    }
}

//+
//  StarRtdImage::call
//
//  Function to allow other functions to be called to implement the
//  image subcommands. If the requested subcommand is not defined at
//  this level then pass on the search to parent. Since this function
//  is virtual the call starts in the most specific class.
//
//  Arguments:
//     const char *name       - Name of subcommand
//     int len                - length of name
//     int argc               - Number of arguments to subcmd
//     char *argv[]           - Subcmd arguments
//
//
//  Return:
//     TCL status.
//-

int StarRtdImage::call( const char *name, int len, int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::call (" << name  << ")" << std::endl;
#endif
    for ( unsigned int i = 0;
          i < sizeof( subcmds_ ) / sizeof(*subcmds_); i++ ) {
        StarRtdImageSubCmds *t = &subcmds_[i];
        if ( strncmp( t->name, name, len ) == 0 ) {
            if ( check_args( name, argc, t->min_args, t->max_args )
                 != TCL_OK ) {
                return TCL_ERROR;
            }
            return ( this->*t->fptr )( argc, argv );
        }
    }
    // Not found at this scope, down to Skycat.
    return Skycat::call( name, len, argc, argv );
}

//+
// Load an image file and return a pointer to the ImageData object for it.
// The image will use the StarWCS class to manage WCS coordinates, instead
// of the default implementation (SAOWCS).
//
// The arguments are the filename, an optional slice, fits extension
// number and HDS path name string for NDFIO.  (17.03.98, ALLAN), note
// all images with slices, must be processed by the NDF library.
//-
ImageData* StarRtdImage::getStarImage( const char* filename,
                                       const char* fitsext,
                                       const char* slice,
                                       const char* path )
{
    // Check if the file extension is one known to the NDF library
    // (excluding FITS). If not then pass the image to be read using
    // the StarFitsIO or NDFIO classes as appropriate. These are then
    // passed to ImageData via which any further control is made.
    const char* type = fileSuffix( filename );
    ImageIO imio;

    //  ALLAN: fileSuffix(filename) might return "fits.Z" or "fits.gz"
    //  for a compressed FITS file, or "fts*"
    char* p = (char *) strchr( filename, '.' );
    int isfits = 1;
    if ( p && strstr( p, ".fit" ) == NULL && strstr( p, ".fts" ) == NULL
           && strstr( p, ".FIT" ) == NULL && strstr( p, ".FTS" ) == NULL ) {
        isfits = 0;
    }

    if ( ( !isfits && isNDFtype( type ) ) || slice || path ) {
        if ( slice || path || fitsext ) {

            //  Construct a complete name, including the slice, FITS
            //  extension and or path name. Note if a HDS path is
            //  given then we must avoid including the file extension.
            int len = strlen( filename ) + 1;
            if ( fitsext ) len += strlen( fitsext );
            if ( slice ) len += strlen( slice );
            if ( path ) len += strlen( path );
            char *fullname = new char[len];
            strcpy( fullname, filename );
            if ( path ) {
                p = strstr( fullname, type ) - 1;
                strcpy( p, path );
            }
            if ( fitsext ) strcat( fullname, fitsext );
            if ( slice ) strcat( fullname, slice );

            //  Open image.
            imio = NDFIO::read( fullname, component(), deep_search() );
            delete[] fullname;

        } else {

            //  Plain name so just open image.
            imio = NDFIO::read( filename, component(), deep_search() );
        }
    }  else {

        //  Note: Use special FITS class so that we can use AST for
        //  dealing with astrometry.
        imio = StarFitsIO::read( filename, Mem::FILE_PRIVATE | Mem::FILE_RDWR );
    }
    if ( imio.status() != 0 ) {
        return (ImageData *) NULL;
    }

    //  Return the new image.
    return makeImage( imio );
}

//+
//  StarRtdImage::makeImage
//
//  Make a new image from the given ImageIO object and return a
//  pointer to a derived class of ImageData specialized in that type
//  of image.
//
//  Note that pointers to ImageIORep subclasses, such as StarFitsIO
//  and NDFIO are automatically converted to an ImageIO object through
//  a special constructor.
//
//  This method overrides the parent version to make sure that the
//  GAIA features also work here. We need to make sure that the
//  correct classes are initialized, in particular, the StarWCS class.
//
//  (XXX what about byte swapping for shared memory images (rtdimage
//  mmap and shm commands, real-time interface)? Do we want to do that
//  here?).
//-
ImageData *StarRtdImage::makeImage( ImageIO imio )
{
    // Make sure that we use the StarWCS class and not the inherited default
    // SAOWCS class
    WCSRep* rep = imio.wcs().rep();
    if ( ! rep || strcmp( rep->classname(), "StarWCS" ) != 0 ) {
        imio.wcsinit();
    }

    // The image data is fixed at first.
    volatile_ = 0;

    return ImageData::makeImage( name(), imio, biasimage_->biasInfo(),
                                 verbose() );
}

//+
// Load an image file and display it.
//-
int StarRtdImage::loadFile()
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::loadFile (" << file() << ")" << std::endl;
#endif

    // Used to save and restore image transformation parameters.
    ImageDataParams p;

    //  Release any local FrameSets (these are associated with the
    //  previous image).
    if ( newset_ ) {
        newset_ = (AstFrameSet *) astAnnul( newset_ );
    }
    if ( oldset_ ) {
        oldset_ = (AstFrameSet *) astAnnul( oldset_ );
    }
    if ( origset_ ) {
        origset_ = (AstFrameSet *) astAnnul( origset_ );
    }

    //  -file may have been set to "", clear current image and just return.
    if (strlen(file()) == 0) {
        return clearCmd(0, NULL);
    }

    //  If we have an existing image then release it.
    if ( image_ ) {
        image_->saveParams(p);
        delete image_;
        image_ = (ImageData *) NULL;
        updateViews();
    }

    //  Check that the image exists and parse it.
    char *name;
    char *fitsext;
    char *slice;
    char *path;
    if ( parseName( file(), &name, &fitsext, &slice, &path ) != TCL_OK ) {
        if ( name ) delete[] name;
        if ( fitsext ) delete[] fitsext;
        if ( slice ) delete[] slice;
        if ( path ) delete[] path;
        return error( file(), " is not an image" );
    }

    //  Create the image.
    ImageData* image = getStarImage( name, fitsext, slice, path );

    //  Release resources and return an error if image couldn't be
    //  created for some reason.
    delete[] name;
    if ( fitsext ) delete[] fitsext;
    if ( slice ) delete[] slice;
    if ( path ) delete[] path;
    if (! image ) {
        return TCL_ERROR;
    }

    //  Store image reference.
    image_ = image;

    //  Restore transformations.
    image_->restoreParams( p, !autoSetCutLevels_ );
    filename(file());  // keep filename

    //  Initialise the new image.
    return initNewImage();
}

//+
// Return a pointer to the StarWCS object for the image, or NULL on error
//-
StarWCS* StarRtdImage::getStarWCSPtr( ImageData* image )
{
    if (!image) {
        image = image_;
    }
    WCSRep *p = image->wcs().rep();
    if ( p && strcmp( p->classname(), "StarWCS" ) == 0 ) {
        return (StarWCS *) p;
    }
    if ( p ) {
        error( "internal error: expected class StarWCS, not ",
               p->classname() );
    }
    else {
        error( "internal error: null StarWCS object" );
    }
    return NULL;
}

//+
//   StarRtdImage::replaceImageDataCmd.
//
//   Replace and update the image data. Requires the address of some
//   memory that contains data of exactly the same size and data type as that
//   already in use. If that's not true then undefined things will happen.
//
//   The only argument is a memory address stored in a long.
//-
int StarRtdImage::replaceImageDataCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::replaceImageDataCmd" << std::endl;
#endif
    long adr;
    if ( Tcl_ExprLong( interp_, argv[0], &adr ) != TCL_OK ) {
        return TCL_ERROR;
    }
    size_t size = image_->data().length();
    Mem data = Mem( (void *)adr, size, 0 );

    //  Record that the data associated with this image is now volatile (that
    //  is doesn't match the underlying data in any associated file).
    volatile_ = 1;

    //  And update data.
    return updateImageNewData( data );
}

//+
//   StarRtdImage::imageDataCmd.
//
//   Returns the memory address of the image data. This is for use
//   with fast updates (complements the shared memory access, but
//   for local process), clearly if the memory is to be modified
//   it must have been mapped in using an update access method.
//
//   Address is returned in a long.
//-
int StarRtdImage::imageDataCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::imageDataCmd" << std::endl;
#endif
    long adr = (long) image_->data().ptr();
    Tcl_SetObjResult( interp_, Tcl_NewLongObj( adr ) );
    return TCL_OK;
}

//+
//   StarRtdImage::volatileCmd
//
//   Purpose:
//      Return or set the volatility status. The behaviour depends on the
//      number of arguments, none to get the current value, 1 to set it to the
//      given value. Note this value will be reset when a new image is loaded.
//-
int StarRtdImage::volatileCmd( int argc, char *argv[] )
{
    if ( ! image_ ) {
        return TCL_OK;
    }
    if ( argc == 0 ) {
        return set_result( volatile_ );
    }
    if ( Tcl_GetBoolean( interp_, argv[0], &volatile_ ) != TCL_OK ) {
        return TCL_ERROR;
    }
    return TCL_OK;
}


//+
//   StarRtdImage::dumpCmd
//
//   Dump the displayed image to a file. This overrides the method
//   used by RtdImage, as we make sure that the current WCS is used as
//   part of the output FITS headers (overwriting what exists there
//   already) if the original WCS has been overwritten (a sign that at
//   least some WCS modification has been attempted).
//
//   Two encodings of the WCS can be written. The first is uses the default
//   encoding of a channel that contains the image FITS headers.  Normally
//   this is a native encoding which is accurate and can be output to an NDF
//   WCS component, unless the destination file is FITS, in which case
//   FITS-WCS overrides Native as the default (and also overrides the never a
//   standard FITS-PC). A second attempt to encode the WCS is made if a
//   suitable encoding form passed as an optional argument (normally this will
//   be FITS-WCS or DSS) and a successful FITS-WCS has not already been
//   written.
//
//   The return from this function is either TCL_OK for success
//   or TCL_ERROR for failure. Either way the return may have
//   diagnostic messages that should be shown to the user.
//
//-
int StarRtdImage::dumpCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::dumpCmd (" << argv[0] << ")" << std::endl;
#endif
    int native = 1;
    int saved = 1;
    int nwrite = 0;
    int nextra = 0;

    if ( image_ ) {

        //  Create a stream for any messages about the saving process.
        ostringstream message;
        if ( origset_ ) {

            //  WCS has been modified so we need to store this in the headers,
            //  before saving the file. The stratedy is to clear the headers
            //  of any existing WCS content and then write the new WCS.
            Mem oldhead = image_->header();
            int ncard = (int) oldhead.size() / FITSCARD;
            AstFitsChan *chan = NULL;
            gaiaUtilsGtFitsChan( (char *) oldhead.ptr(), ncard, &chan );

            //  Read the headers once, this determines the default encoding.
            astClear( chan, "Card" );
            const char *default_encoding = astGetC( chan, "Encoding" );

            //  The PC and CD forms are both permitted, use the one that the
            //  channel already contains.
            int cdmatrix = astGetI( chan, "CDMatrix" );

            //  Now destructively do the read.
            AstFrameSet *tmpset = (AstFrameSet *) astRead( chan );
            if ( !astOK ) {
                astClearStatus;
            }
            if ( tmpset != AST__NULL && astIsAFrameSet( tmpset ) ) {
                tmpset = (AstFrameSet *) astAnnul( tmpset );

                //  A successful initial default encoding is the permanent
                //  default for writing back. Now that FITS-PC is deprecated
                //  (and was never part of the standard) always write FITS-WCS.
                if ( strcmp( "FITS-PC", default_encoding ) == 0 ) {
                    astSet( chan, "Encoding=FITS-WCS" );
                    default_encoding = astGetC( chan, "Encoding" );
                }
                else {
                    astSet( chan, "Encoding=%s", default_encoding );
                }
            }
            else {
                //  Nothing read from the channel. So default encoding is
                //  untrustworthy. If this was a FITS file with no WCS then
                //  writing a Native encoding is bad, but is the correct thing
                //  to do for an NDF, but NDF channels should always read since
                //  they have PIXEL etc., so just need to test for FITS image.
                if ( isfits() ) {
                    astSet( chan, "Encoding=FITS-WCS" );
                    default_encoding = astGetC( chan, "Encoding" );
                }
            }

            //  Clear the channel of all existing WCS information.
            astPurgeWCS( chan );

            //  Reset the CDMatrix value back to that used initially.
            astSetI( chan, "CDMatrix", cdmatrix );

            //  If the card position is still at the beginning for any reason
            //  then we should move it past the standard headers, these are
            //  those up to the last NAXIS card.
            int startCard = astGetI( chan, "card" );
            char card[FITSCARD+1];
            if ( astGetI( chan, "Card" ) <= 1 ) {
                while( astFindFits( chan, "NAXIS%d", card, 1 ) ) {
                    startCard = astGetI( chan, "card" );
                }
                astSetI( chan, "Card", startCard );
            }

            //  Now we can try to add the WCS encoding. The encoding type of
            //  the channel is set to the type of the WCS object just read (or
            //  Native and FITS-WCS for NDFs and FITS). The first attempt to
            //  update will use whatever this is set to. If this fails
            //  (shouldn't now we clear the FITS headers of all WCS
            //  information) then an attempt to write a native encoding will
            //  be made, if the default type wasn't native already.
            StarWCS* wcsp = getStarWCSPtr();
            if ( !wcsp ) {
                return TCL_ERROR;
            }
            AstFrameSet *newwcs = wcsp->astWCSClone();
            nwrite = astWrite( chan, newwcs );
            if ( !astOK || nwrite == 0 ) {
                message << "WCS: failed to save WCS using the "
                        << "default encoding: " << default_encoding << std::endl;

                //  Failed to write WCS using default encoding. Try NATIVE.
                if ( ! astOK ) astClearStatus;
                if ( strcmp( "NATIVE", default_encoding ) != 0 ) {
                    astSet( chan, "Encoding=Native" );
                    nwrite = astWrite( chan, newwcs );
                    if ( astOK && nwrite != 0 ) {
                        message << "WCS: saved WCS as AST native "
                                << "encoding" << std::endl;
                        native = 1;
                        saved = 1;
                    } else {
                        message << "WCS: failed to save WCS "
                                << "using an AST native encoding" << std::endl;
                        //  Failed this time too.
                        native = 0;
                        saved = 0;
                    }
                } else {
                    native = 0;
                    saved = 0;
                }
            } else {

                //  Write succeeded using default encoding.
                message << "WCS: saved WCS using default encoding: "
                        << default_encoding << std::endl;
                saved = 1;
                if ( strcmp( "NATIVE", default_encoding ) == 0 ) {
                    native = 1;
                } else {
                    native = 0;
                }
            }

            //  Now try with the given encoding (which shouldn't be native),
            //  if this is the same as that just written, or is just another
            //  FITS-* encoding then do nothing. Too many FITS encodings are a
            //  bad things (mixes CD, PC etc. keywords which is difficult to
            //  maintain). Note DSS gets through.
            if ( argc == 2 ) {
                if ( strcmp( argv[1], default_encoding ) != 0 &&
                     ( ( strncmp( argv[1], "FITS-", 5 ) == 0 &&
                         strncmp( default_encoding, "FITS-", 5 ) != 0 ) ||
                       native )
                    ) {
                    astSet( chan, "Encoding=%s", argv[1] );
                    nextra = astWrite( chan, newwcs );

                    if ( astOK && nextra != 0 ) {
                        saved = 1;
                        message << "WCS: saved WCS using additional encoding: "
                                << argv[1] << std::endl;
                    }
                    else {
                        message << "WCS: failed to save WCS using "
                                << "additional encoding: "
                                << argv[1] << std::endl;
                    }
                    if ( !astOK ) astClearStatus;
                }
            }
            if ( nwrite != 0 || nextra != 0 ) {

                //  Write the FITS channel out to a Mem object and use
                //  it to replace the existing headers.
                ncard = astGetI( chan, "Ncard" );

                //  Allocate header. Need to use CNF when an NDF.
                Mem newhead;
                if ( isfits() ) {
                    newhead = Mem( FITSCARD * ( ncard + 1 ), 0 );
                }
                else {
                    size_t hsize = (size_t) FITSCARD * ( ncard + 1 );
                    void *hdata = cnfMalloc( hsize );
                    newhead = Mem( hdata, hsize, 0 );
                }
                char *newptr = (char *) newhead.ptr();

                astClear( chan, "Card" );
                int i;
                for ( i = 0; i < ncard; i++, newptr += FITSCARD ) {
                    astFindFits( chan, "%f", card, 1 );
                    memcpy( newptr, card, FITSCARD );
                }
                strcpy( newptr, "END" );
                newptr += 3;
                for ( i = 0; i < 77; i++, newptr++ ) *newptr = ' ';
                image_->header( newhead ); // XXX how is old header released?
            }
            chan = (AstFitsChan *) astAnnul( chan );
            newwcs = (AstFrameSet *) astAnnul( newwcs );
        }
        int result = image_->write( argv[0] );
        if ( result == TCL_OK ) {
            if ( origset_ ) {
                if ( saved && native && ( nextra == 0 ) ) {
                    if ( isfits() ) {
                        message << "WCS: warning, your modified WCS "
                                << "could only be saved as an AST native "
                                << "encoding, this will limit its "
                                << "usefulness to AST compatible programs "
                                << "(GAIA, KAPPA, EXTRACTOR etc.)"
                                << std::endl;
                        result = TCL_ERROR;
                    }
                    else {
                        //  Just native is fine for NDFs.
                        message << "WCS: your modified WCS was "
                                << "successfully saved" << std::endl;
                    }
                } else if ( !saved ) {
                    message << "WCS: error, failed to save your modified WCS "
                            << std::endl;
                    result = TCL_ERROR;
                }
                else {
                    message << "WCS: your modified WCS was "
                            << "successfully saved" << std::endl;
                }
            }
            else {
                message << "Image saved" << std::endl;
            }
            set_result( message.str().c_str() );
        }
        return result;
    }
    else {
        return TCL_OK;
    }
}

//+
//  StarRtdImage::configureImage
//
//  This procedure is called to process an argv/argc list, plus the Tk
//  option database, in order to configure (or reconfigure) a image.
//
//  Redefined here to check which changes were made and then pass
//  responsibility on to TkImage. Note that this method is a copy of
//  the body of RtdImage::configureImage with our modifications. We
//  need this to ensure that the correct configSpecs_ are used.
//-
int StarRtdImage::configureImage(int argc, char* argv[], int flags)
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::configureImage" << std::endl;
#endif
    if ( TkImage::configureImage(argc, argv, flags) != TCL_OK ) {
        return TCL_ERROR;
    }

    int status = TCL_OK;
    int reset = 0;

    // note if we are using X shared memory
    usingXShm_ = haveXShm_ && usexshm();

    // find out which options were specified and process them
    // if necessary (note: Tk sets a flag in the config entry when
    // the option is specified. We use the OFFSET macro defined above
    // as an efficient way to compare options)
    for ( Tk_ConfigSpec* p=configSpecs_; p->type != TK_CONFIG_END; p++ ) {
        if ( optionModified( argc, argv, p->argvName ) ) {
            switch(p->offset) {

            case RTD_OPTION(usexshm):
                if (initialized_) {
                    deleteXImage();
                    reset++;
                }
                break;

            case RTD_OPTION(usexsync):
                if (usingXSync_ && usexsync()) {
                    /*
                     * XSyncSetPriority() was commented out since it can block all
                     * other X-applications when fast image events are received-
                     */
                    // XSyncSetPriority(display_, None, 65535);
                    // fprintf(stderr, "Raising priority of client %s\n", name());
                }
                break;

            case RTD_OPTION(displaymode):
            case RTD_OPTION(shm_header):
            case RTD_OPTION(shm_data):
                if (initialized_)
                    reset++;
                break;
            case RTD_OPTION(verbose):
            case RTD_OPTION(debug):
                if (dbl_)
                    dbl_->setlog ((int)(debug() & verbose()));
                break;
            case RTD_OPTION(fitWidth):
            case RTD_OPTION(fitHeight):
                if (initialized_) {
                    if (image_ && fitWidth() && fitHeight()) {
                        image_->shrinkToFit(fitWidth(), fitHeight());
                    }
                    reset++;
                }
                break;

            case RTD_OPTION(fillWidth):
            case RTD_OPTION(fillHeight):
                if (initialized_) {
                    if (image_ && fillWidth() && fillHeight()) {
                        image_->fillToFit(fillWidth(), fillHeight());
                    }
                    reset++;
                }
                break;

            case RTD_OPTION(file):
                status = loadFile();
                break;

            case RTD_OPTION(sampmethod):
                if (initialized_ && image_) {
                    if (image_->sampmethod() != sampmethod()) {
                        image_->sampmethod(sampmethod());
                        reset++;
                    }
                }
                break;

            case RTD_OPTION(subsample):
                if (initialized_ && image_) {
                    if (image_->subsample() != subsample()) {
                        image_->subsample(subsample());
                        reset++;
                    }
                }
                break;
            }
        }
    }

    if ( reset ) {
        return resetImage();
    }
    return status;
}

//+
//   StarRtdImage::foreignCmd
//
//  Deal with request to run a "foreign" command.
//
//  This method provides direct access to commands that need it (such
//  as the patching command in GAIA).
//
//  Arguments:
//       int argc         - Number of arguments passed to command (2).
//       char *argv[]     - The arguments, name of the foreign command
//                          to run and a string with the foreign
//                          command qualifiers.
//    Return:
//       TCL status.
//
//-
int StarRtdImage::foreignCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::foreignCmd" << std::endl;
#endif

    // Stop if no image loaded.
    if ( !image_ ) {
        return TCL_OK;
    }

    // Check number of arguments.
    if ( argc != 2 ) {
        set_result( "number of arguments wrong, require 2: command qualifiers" );
        return TCL_ERROR;
    }

    // Now check for its existence and invoke it.
    for ( unsigned int i = 0; i < sizeof( foreigncmds_ ) / sizeof( *foreigncmds_); i++ ) {
        StarRtdForeignCmds *t = &foreigncmds_[i];
        if ( strcmp( t->name, argv[0] ) == 0 ) {

            //  Matched a command so construct the necessary image
            //  information structure and invoke the command.
            StarImageInfo *info = new StarImageInfo;
            ImageIO imio = image_->image();

            //  Look for NDF identifier.
            char *id = imio.get( "NDFID" );
            if ( id ) {
                info->NDFid = atoi( id );
            } else {
                info->NDFid = 0;
            }
            info->imageData = (void *) imio.dataPtr();
            info->type = (ImageDataType) image_->dataType();
            info->nx = (int) image_->width();
            info->ny = (int) image_->height();
            info->interp = interp_;

            //  If the image representation is byte swapped (i.e. FITS on
            //  some architectures) then inform foreign method that it
            //  needs to deal with this.
            info->swap = swapNeeded();

            //  Invoke foreign command.
            char *errStr;
            int result = ( *t->fptr )( info, argv[1], &errStr );
            delete info;
            if ( result ) {
                updateImage();
                return TCL_OK;
            } else {
                set_result( errStr );
                free( (void *) errStr );
                return TCL_ERROR;
            }
        }
    }

    // Foreign command not found.
    return error( "unknown foreign command");
}

//+
//   StarRtdImage::originCmd
//
//   Purpose:
//      Returns the effective NDF origin information. The effective origins
//      are those of the significant dimensions.
//
//   Arguments:
//       int argc         - Number of arguments passed to command.
//       char *argv[]     - The arguments, names of the variables
//                          to hold origin information. Note there
//                          can be more than two of these. If more are
//                          supplied than necessary then the
//                          extra ones will be set to 1.
//    Return:
//       TCL status.
//
//-
int StarRtdImage::originCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::originCmd" << std::endl;
#endif

    if (!image_) {
        return error("no image loaded");
    }
    char *out_name;
    char *value;
    char name[10];
    const char *base;

    // If any ELBOUND cards are defined we use them, otherwise LBOUND (of
    // which there should only ever be two).
    if( image_->image().get( "ELBOUND1" ) != NULL ) {
        base = "ELBOUND";
    }
    else {
        base = "LBOUND";
    }

    for ( int i = 0; i < argc; i++ ) {
        out_name = argv[i];
        sprintf( name, "%s%d", base, i + 1 );
        value = image_->image().get( name );
        if ( ! value ) {
            value = "1";
        }
        Tcl_SetVar(interp_, out_name, value, 0);
    }
    return TCL_OK;
}

//+
//   StarRtdImage::usinghxshmCmd
//
//   Purpose:
//      Returns whether we're actually using X shared memory.
//
//   Arguments:
//      None
//
//    Return:
//       TCL status and result.
//
//-
int StarRtdImage::usingxshmCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::usingxshmCmd" << std::endl;
#endif

    if (!image_) {
        return error("no image loaded");
    }
    if ( haveXShm_ && usexshm() ) {
        set_result( 1 );
    } else {
        set_result( 0 );
    }
    return TCL_OK;
}

//+
//   StarRtdImage::plotgridCmd
//
//   Purpose:
//      Plot an AST grid
//
//    Return:
//       TCL status and result. Plots a grid.
//
//    Notes:
//       The first parameter to this routine is a list that contains
//       all of the grid attributes (see the AST documentation) in a
//       pre-formatted manner (such as can be used by as astSet
//       routine). This allows maximum flexibility in the options
//       that can be set, but imposes an obligation on the user of
//       this member function to format and control the attributes
//       correctly. Special attributes can be passed that describe
//       a new sky-frame, rather than the plot. These should be:
//
//          epoch=value
//          equinox=value
//          system=value
//
//       where value are possible specifiers to a skyframe.
//
//       The gap(1) and gap(2) attributes are also special. This are
//       sense inverted in that the default gap is scaled by the
//       inverse of these numbers.
//
//       System can take a value "pixels" which is taken to mean that
//       a grid showing pixel coordinates should be drawn.
//
//       The second parameter passed to this member should be a list of
//       the bounds, in canvas coordinates, of the region to be
//       drawn. If NULL then the whole canvas is used.
//
//-
int StarRtdImage::plotgridCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::plotgridCmd" << std::endl;
#endif

    int inerror = 0;
    if (!image_) {
        return error("no image loaded");
    }

    // Check the list of attributes for the special values pixel,
    // epoch, equinox, system, gap(1) and gap(2). This section also
    // does the initial split of the input list. Note we do these
    // always so that we can check for request for a pixel based
    // system. This is always available regardless of the existence of
    // a proper WCS system.
    char *equinox = NULL;
    char *epoch = NULL;
    char *system = NULL;
    char *gap1 = NULL;
    char *gap2 = NULL;
    char **listArgv;
    int listArgc = 0;
    char **coordArgv;
    int coordArgc = 0;
    double region[4];
    int showpixels = 0;
    if ( argc != 0 ) {
        if ( argc > 2 ) {
            error( "wrong # args: plotgrid options_list canvas_area" );
            inerror = 1;
        } else {
            if ( Tcl_SplitList( interp_, argv[0], &listArgc, &listArgv )
                 != TCL_OK ) {
                error( "sorry: failed to decode plotting options "
                       "(check format)" );
                inerror = 1;
            } else {
                char *string = NULL;
                for ( int index = 0; index < listArgc; index++ ) {
                    string = listArgv[index];

                    //  Check for "epoch", "equinox" or "system" and
                    //  record the value. Note assumes lowercase and
                    //  no leading blanks.
                    if ( strncmp( string, "equinox=", 8 ) == 0 ) {
                        equinox = string;
                    } else if ( strncmp( string, "epoch=", 6 ) == 0 ) {
                        epoch = string;
                    } else if ( strncmp( string, "system=", 7 ) == 0 ) {
                        system = string;
                        if ( strncmp( system+7, "pixels", 6 ) == 0 ) {
                            // This is a special case, want just pixels
                            // displayed.
                            showpixels = 1;
                        }
                    } else if ( strncmp( string, "gap(1)=", 7 ) == 0 ) {
                        gap1 = string;
                    } else if ( strncmp( string, "gap(2)=", 7 ) == 0 ) {
                        gap2 = string;
                    }
                }
            }

            //  Get the canvas coordinate display range if given.
            if ( argc == 2 ) {
                if ( Tcl_SplitList( interp_, argv[1], &coordArgc,
                                    &coordArgv ) != TCL_OK ) {
                    error( "sorry: failed to decode region of image to grid" );
                    coordArgc = 0;
                    inerror = 1;
                } else {
                    if ( coordArgc != 4 ) {
                        error( "wrong # of args, should be 4 canvas "
                               "coordinates" );
                        inerror = 1;
                    } else {
                        for ( int index = 0; index < coordArgc; index++ ) {
                            if ( Tcl_GetDouble(interp_, coordArgv[index],
                                               &region[index] ) != TCL_OK ) {
                                error( coordArgv[index],
                                       "is not a valid number");
                                inerror = 1;
                                break;
                            }
                        }

                        //  Reorganise these coordinates to the expected order
                        //  (bottom-left, top-right of screen, not canvas).
                        swap( region[1], region[3] );
                    }
                }
            }
        }
    }

    //  If just showing pixels, then just get a suitable FrameSet.
    AstFrameSet *wcs = (AstFrameSet *) NULL;
    if ( showpixels ) {
        wcs = makeGridWCS();
    } else {

        //  See if a copy of the current WCS frameset is available (we
        //  use a copy so we can  modify without changing any other elements).
        StarWCS* wcsp = getStarWCSPtr();
        if ( ! wcsp ) {
            return TCL_ERROR;
        }
        wcs = wcsp->astWCSCopy();
        if ( wcs == (AstFrameSet *) NULL ) {

            //  No WCS, give up.
            error( "sorry: unable to plot a grid as no world coordinates"
                   " are available, select the system 'pixels' or add a WCS" );
            inerror = 1;
        }
    }
    if ( wcs != (AstFrameSet *) NULL ) {

        //  If we have an equinox, epoch or system then create a skyframe
        //  and set this up as the current system (rather than the wcs
        //  system). Note we create a new skyframe so that we can ensure
        //  the correct defaults, i.e. anything not set equals the default
        //  for that system, rather than the existing value.
        //
        //  However for channel map WCS's this strategy does not work and we
        //  must fall back to setting the attributes of all the ROI frames and
        //  clearing those not set (but things like formatting remain a
        //  problem).  Note this also happens to the WCS, not the Plot.
        AstSkyFrame *newsky = NULL;
        if ( ( equinox || epoch || system ) && ! showpixels ) {
            const char *ident = astGetC( wcs, "Ident" );
            if ( ident != NULL && strncmp( "ROI", ident, 3 ) == 0  ) {
                int icur = astGetI( wcs, "Current" );
                int nframe = astGetI( wcs, "nframe" );
                for ( int i = 1; i <= nframe; i++ ) {
                    astSetI( wcs, "Current", i );
                    if ( strncmp( "ROI", astGetC( wcs, "Ident" ), 3 ) == 0 ){
                        if ( equinox ) {
                            astSet( wcs, equinox, " " );
                        }
                        else {
                            astClear( wcs, "Equinox" );
                        }
                        if ( epoch ) {
                            astSet( wcs, epoch, " " );
                        }
                        else {
                            astClear( wcs, "Epoch" );
                        }
                        if ( system ) {
                            astSet( wcs, system, " " );
                        }
                        else {
                            astClear( wcs, "System" );
                        }
                    }
                    if ( ! astOK ) astClearStatus;
                }
                astSetI( wcs, "Current", icur );
            }
            else {
                newsky = astSkyFrame( " " );
                if ( equinox ) {
                    astSet( newsky, equinox, " " );
                }
                if ( epoch ) {
                    astSet( newsky, epoch, " " );
                }
                if ( system ) {
                    astSet( newsky, system, " " );
                }
            }
        }

        //  Create an AstPlot that selects the displayed region, or the
        //  whole of the image. It also incorporates the additional
        //  SkyFrame that describes a system we want to add. Note wcs is
        //  inverted so that the conversion goes through the appropriate
        //  route (SKY to SKY to GRAPHICS, without inversion we just get
        //  SKY to SKY, as conversion is between the current frames).
        if ( newsky != (AstSkyFrame *) NULL ) {
            astInvert( wcs );
        }
        AstPlot *plot = createPlot( wcs, (AstFrameSet *) newsky, NULL,
                                    (coordArgc == 0), 0, region, 0 );
        inerror = ( inerror || plot == (AstPlot *) NULL );
        if ( ! inerror ) {

            //  Initialise the interpreter and canvas name for the Tk plotting
            //  routines.
            astTk_Init( interp_, canvasName_ );

            //  Define a tag for all items created in the plot.
            astTk_Tag( ast_tag() );

            //  If we have the values gap1 and gap2 then these are
            //  actually divisors of the default value which we can only
            //  actually find out now.
            float fact;
            float value;
            if ( gap1 != NULL ) {
                sscanf( gap1, "gap(1)=%f", &fact );
                if ( fact < FLT_EPSILON ) fact = 1.0f;
                value = astGetF( plot, "gap(1)" );
                if ( astOK ) {
                    value /= fact;
                    astSetF( plot, "gap(1)", value );
                }
                if ( !astOK ) astClearStatus;
            }
            if ( gap2 != NULL ) {
                sscanf( gap2, "gap(2)=%f", &fact );
                if ( fact < FLT_EPSILON ) fact = 1.0f;
                value = astGetF( plot, "gap(2)" );
                if ( astOK ) {
                    value /= fact;
                    astSetF( plot, "gap(2)", value );
                }
                if ( !astOK ) astClearStatus;
            }

            // Unset the number of digits used in the plot. These are set
            // elsewhere in GAIA (for WCS decoding) and are not sensible
            // for this occasion (too much precision leads to bad default
            // labelling).
            astClear( plot, "digits(1)" );
            astClear( plot, "digits(2)" );
            if ( ! astOK ) astClearStatus;

            // Set all plot attributes to control the plot appearance.
            if ( listArgc > 0 ) {
                char *string = NULL;
                int failed = 0;
                for ( int index = 0; index < listArgc; index++ ) {
                    string = listArgv[index];

                    //  Check for "epoch", "equinox", "system", "gap(1)" and
                    //  "gap(2)" and ignore these.
                    if ( string != equinox && string != epoch &&
                         string != system && string != gap1 &&
                         string != gap2 ) {
                        astSet( plot, string, " " );
                        if ( !astOK ) {
                            cerr << "plot options: " << string <<
                                " is not a valid attribute (" <<
                                astStatus << ")" << std::endl;
                            failed++;
                            //  Try to succeed by using any acceptable options.
                            astClearStatus;
                        }
                    }
                }
                if ( failed == listArgc ) {
                    inerror = 1;
                    error( "Failed to set any plot attributes" );
                }
            }

            //  Final job before plotting the grid is to look for any ROIs.
            //  These require a Plot each. ROI are marked with Ident values
            //  of ROI<n> (by atlAxTrm).
            if ( astOK && ! inerror ) {
                const char *ident = astGetC( plot, "Ident" );
                if ( ident != NULL && strlen( ident ) > 3 &&
                     ident[0] == 'R' && ident[1] == 'O' && ident[2] == 'I' ) {
                    AstKeyMap *rplots;
                    char *error_mess;
                    AstPlot *roiPlot;
                    if ( gaiaUtilsAtlPlROI( plot, &rplots, &error_mess ) ) {
                        int size = astMapSize( rplots );
                        for ( int i = 0; i < size; i++ ) {
                            char const *key = astMapKey( rplots, i );
                            astMapGet0A( rplots, key, &roiPlot );
                            astGrid( roiPlot );
                            if ( ! astOK ) {
                                astClearStatus;
                            }
                            roiPlot = (AstPlot *) astAnnul( roiPlot );
                        }
                        rplots = (AstKeyMap *) astAnnul( rplots );
                    }
                    else {
                        inerror = 1;
                        error( error_mess );
                        free( error_mess );
                    }
                }
                else {
                    //  No ROIs.
                    astGrid( plot );
                }
            }

            //  Free the plot.
            plot = (AstPlot *) astAnnul( plot );

            //  Reset the tag associated with AST grid items.
            astTk_Tag( NULL );
        }

        // Free the WCS copy.
        wcs = (AstFrameSet *) astAnnul( wcs );
    }

    // Free the lists.
    if ( listArgc > 0 ) {
        Tcl_Free( (char *) listArgv );
    }
    if ( coordArgc > 0 ) {
        Tcl_Free( (char *) coordArgv );
    }

    //  Do not exit with AST still in error.
    if ( inerror || ! astOK ) {
        if ( !astOK ) {
            astClearStatus;
        }
        return TCL_ERROR;
    }
    return TCL_OK;
}

//+
//   StarRtdImage::astcelestialCmd
//
//   Return if coordinate system is celestial or not.
//-
int StarRtdImage::astcelestialCmd( int argc, char *argv[] )
{
    if ( isCelestial() ) {
        set_result( 1 );
    }
    else {
        set_result( 0 );
    }
    return TCL_OK;
}

//+
//   StarRtdImage::isCelestial
//
//   Return if coordinate system is celestial.
//-
int StarRtdImage::isCelestial()
{
    StarWCS* wcsp = getStarWCSPtr();
    if ( wcsp ) {
        return wcsp->isCelestial();
    }
    else {
        return 0;
    }
}

//+
//   StarRtdImage::astgetCmd
//
//   Purpose:
//      Returns the value of an AST attribute.
//
//    Return:
//       TCL status and result.
//
//    Notes:
//       The input string should be the name of a known
//       AST attribute. The AST object which is queried
//       is the current WCS object, so any attributes should be
//       limited to applicable values.
//
//-
int StarRtdImage::astgetCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astgetCmd" << std::endl;
#endif
    if (!image_) {
        return error("no image loaded");
    }

    //  Get the value.
    StarWCS* wcsp = getStarWCSPtr();
    if ( !wcsp ) {
        return TCL_ERROR;
    }
    const char *result = wcsp->astGetAttrib( argv[0] );
    set_result( result );
    return TCL_OK;
}

//+
//   StarRtdImage::astsetCmd
//
//   Purpose:
//      Set the value of an AST attribute.
//
//    Return:
//       TCL status and result.
//
//    Notes:
//       The two input string should be the name of a known
//       AST attribute and the new value. The AST object which is queried
//       is the current WCS object, so any attributes should be
//       limited to applicable values.
//
//-
int StarRtdImage::astsetCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astsetCmd" << std::endl;
#endif
    if (!image_) {
        return error("no image loaded");
    }

    //  Get the value.
    StarWCS* wcsp = getStarWCSPtr();
    if (!wcsp) {
        return TCL_ERROR;
    }
    if ( wcsp->astSetAttrib( argv[0], argv[1] ) ) {
        return TCL_OK;
    }

    return error( "Failed to set:" , argv[0] );
}

//+
//   StarRtdImage::aststoreCmd
//
//   Purpose:
//      Stores a value and keyword in a FITS channel.
//      The channel can later be used to create an AST FrameSet
//      that describes the WCS system. It is the responsibility
//      of the caller to ensure that a FITS channel contains the
//      correct information.
//
//    Notes:
//      The arguments are the channel number, keyword, value,
//      comment and if the value is a string. The comment and
//      type are optional, unless the value is a string.
//
//    Return:
//       TCL status and result.
//
//-
int StarRtdImage::aststoreCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::aststoreCmd" << std::endl;
#endif

    //  Extract the identifier of the FITS channel that we are to use.
    int slot = 0;
    if ( Tcl_GetInt( interp_, argv[0], &slot ) != TCL_OK ) {
        return error( argv[0], " is not an integer");
    }
    else {
        char *comment = NULL;
        int isString = 0;
        if ( argc > 3 ) {
            comment = argv[3];
            if ( argc > 4 ) {
                Tcl_GetBoolean( interp_, argv[4], &isString );
            }
        }

        // Enter the value into the channel.
        slot--;
        storeCard( channels_[slot], argv[1], argv[2], comment, isString );
        if ( astOK ) {
            return TCL_OK;
        }
        astClearStatus;
        return more_error( "failed to enter card into FITS channel" );
    }
}

//+
//   StarRtdImage::storeCard
//
//   Purpose:
//      Creates and stores a FITS card in a FITS channel given the
//      keyword name, value and comment. If this is a string then
//      the value will be placed in quotes.
//-
void StarRtdImage::storeCard( AstFitsChan *channel, const char *keyword,
                              const char *value, const char *comment,
                              int isString, int overwrite )
{
    char card[FITSCARD];
    const char *dummy = "No comment";
    if ( comment == NULL ) comment = dummy;
    if ( isString ) {
        if ( strlen(value) > 19 ) {
            sprintf( card, "%-8.8s='%s' /%s", keyword, value, comment );
        }
        else {
            sprintf( card, "%-8.8s='%19.19s' /%s", keyword, value, comment );
        }
    }
    else {
        if ( strlen(value) > 21 ) {
            sprintf( card, "%-8.8s=%s /%s", keyword, value, comment );
        }
        else {
            sprintf( card, "%-8.8s=%21.21s /%s", keyword, value, comment );
        }
    }
    astPutFits( channel, card, overwrite );
}

//+
//   StarRtdImage::astresetCmd
//
//   Purpose:
//      Clears a FITS channel of all its cards.
//
//    Return:
//       TCL status and result.
//
//-
int StarRtdImage::astresetCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astresetCmd" << endl;
#endif

    //  Extract the identifier of the FITS channel that we are to use.
    int slot = 0;
    if ( Tcl_GetInt( interp_, argv[0], &slot ) != TCL_OK ) {
        return error( argv[0], " is not an integer");
    } else {

        // Now clear the channel. Do this by deleting it and creating a
        // new one.
        slot--;
        channels_[slot] = (AstFitsChan *) astAnnul( channels_[slot] );
        initChannel( slot );
    }
    return TCL_OK;
}

//+
//   StarRtdImage::astdeleteCmd
//
//   Purpose:
//      Delete a FITS channel.
//
//    Return:
//       TCL status and result.
//
//-
int StarRtdImage::astdeleteCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astdeleteCmd" << std::endl;
#endif

    //  Extract the identifier of the FITS channel that we are to
    //  delete.
    int slot = 0;
    if ( Tcl_GetInt( interp_, argv[0], &slot ) != TCL_OK ) {
        return error( argv[0], " is not an integer" );
    } else {
        slot--;
        channels_[slot] = (AstFitsChan *) astAnnul( channels_[slot] );
    }
    return TCL_OK;
}

//+
//  StarRtdImage::astcreateCmd
//
//  Purpose:
//     Creates a FITS channel that can be used to enter suitable FITS
//     cards for establishing a FITS WCS system of some kind.
//
//     The return value is a handle for the channel to be used
//     when accessing it in future calls. This is zero if no channel
//     was available.
//
//    Return:
//       TCL status and result.
//-
int StarRtdImage::astcreateCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astcreateCmd" << std::endl;
#endif
    if (!image_) {
        set_result(0);
        return error("no image loaded");
    }

    //  Check we have a slot for a new channel.
    int freeslot = -1;
    for ( int i = 0; i < MAX_CHANNELS; i++) {
        if ( !channels_[i] ) {
            freeslot = i;
            break;
        }
    }
    if ( freeslot == -1 ) {
        set_result(0);
        return error("no more channels available");
    }

    //  Create a new FITS channel.
    initChannel( freeslot );

    //  The handle is one more than freeslot.
    set_result(++freeslot);
    return TCL_OK;
}

//+
//   StarRtdImage::initChannel
//
//   Purpose:
//      Initialises a FITS channel and sets the keywords that we can
//      construct by default.
//-
void StarRtdImage::initChannel( int slot )
{
    char buffer[FITSCARD];
    channels_[slot] = astFitsChan( NULL, NULL, " " );
    storeCard( channels_[slot], "NAXIS", "2", "Number of axes", 0 );
    sprintf( buffer, "%d", image_->width() );
    storeCard( channels_[slot], "NAXIS1", buffer, "Dimension 1", 0 );
    sprintf( buffer, "%d", image_->height() );
    storeCard( channels_[slot], "NAXIS2", buffer, "Dimension 2", 0 );
}

//+
//   StarRtdImage::astreadCmd
//
//   Purpose:
//      Reads a FITS channel and attempts to convert it into
//      a WCS FrameSet that can be used to define the WCS of an
//      image.
//
//   Notes:
//      If successful then the FrameSet will become the "local" FrameSet
//      which can be attached to the real WCS by a call to the command
//      astreplace. Only one FrameSet can be local at any time,
//      previous local FrameSets are released when new ones are
//      created. The FITS channel used by this command will remain
//      unchanged after the read (contrary to the usual AST
//      behaviour).
//
//-
int StarRtdImage::astreadCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astreadCmd" << std::endl;
#endif

    //  Extract the identifier of the FITS channel that we are to use.
    int slot = 0;
    if ( Tcl_GetInt( interp_, argv[0], &slot ) != TCL_OK ) {
        return error( argv[0], " is not an integer");
    } else {
        slot--;

        //  Make a copy of the FITS channel so we can modify it without
        //  having to totally recreate it.
        AstFitsChan *copy = (AstFitsChan *) astCopy( channels_[slot] );

        //  Attempt to read the channel and create a FrameSet.
        astClear( channels_[slot], "Card" );
        AstFrameSet *set = (AstFrameSet *) astRead( channels_[slot] );
        if ( set == AST__NULL ) {

            //  Read failed for some reason, so back out of changes and make
            //  an error report.
            if ( !astOK ) astClearStatus;
            copy = (AstFitsChan *) astAnnul( copy );
            return more_error("failed to read WCS system");
        } else {

            //  Read succeeded. Release the old FITS channel and replace it
            //  with the copy. The new object becomes the current newset_.
            channels_[slot] = (AstFitsChan *) astAnnul( channels_[slot] );
            channels_[slot] = copy;
            if ( newset_ ) newset_ = (AstFrameSet *) astAnnul( newset_ );
            newset_ = set;
        }
    }
    return TCL_OK;
}

//+
//  StarRtdImage::astreplaceCmd
//
//  Purpose:
//     Replaces the existing WCS AstFrameSet with a new version. This can be
//     by passing in a memory address (as a long) of a new FrameSet, or, (when
//     no arguments are given) by using one that has been created by other
//     members in this class (i.e. astread or astrefine, this will be
//     currently pointed at by newset_). A clone of the existing WCS system is
//     retained so that the changes can be undone (at least until up to the
//     next invocation of astread or astrefine, or the next call to this
//     function) using astrestore.
//
//     If not already taken a clone of what is reckoned to be the original WCS
//     associated with the image is also made. This can be restored using the
//     "astrestore original" command.
//-
int StarRtdImage::astreplaceCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astreplaceCmd" << std::endl;
#endif
    if ( !image_ ) {
        return error( "no image loaded" );
    }
    if ( !newset_ && argc == 0 ) {
        return error( "no new WCS system is available" );
    }

    //  Get a pointer to the internal Starlink version of the WCS class.
    StarWCS* wcsp = getStarWCSPtr();
    if ( !wcsp ) {
        return TCL_ERROR;
    }

    //  If given access the memory address of the FrameSet.
    if ( argc == 1 ) {
        long adr;
        if ( Tcl_ExprLong( interp_, argv[0], &adr ) != TCL_OK ) {
            return TCL_ERROR;
        }
        newset_ = (AstFrameSet *) adr;
    }

    //  If the original WCS hasn't been cloned yet then take one to keep
    //  so that we can always get back to where we started. This will go
    //  wrong when the image didn't have a WCS, but that shouldn't cause
    //  any real harm.
    if ( !origset_ ) {
        origset_ = wcsp->astWCSClone();
    }

    //  Take a clone of the existing WCS system so we can back out of
    //  these changes.
    if ( oldset_ ) {
        oldset_ = (AstFrameSet *) astAnnul( oldset_ );
    }
    oldset_ = wcsp->astWCSClone();

    //  And make the new FrameSet current.
    wcsp->astWCSReplace( newset_ );

    //  Update any views to use this information.
    if ( updateViews( 2 ) != TCL_OK ) {
        return TCL_ERROR;
    }
    else {
        return TCL_OK;
    }
}

//+
//  StarRtdImage::astgetcloneCmd
//
//  Purpose:
//     Returns a clone of the current AST frameset.
//     Annul this after use.
//-

int StarRtdImage::astgetcloneCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astgetwcsCmd" << std::endl;
#endif
    if ( !image_ ) {
        return error( "no image loaded" );
    }

    //  Get a pointer to the internal Starlink version of the WCS class.
    StarWCS* wcsp = getStarWCSPtr();
    if ( !wcsp ) {
        return TCL_ERROR;
    }

    //  Get a clone of the WCS.
    AstFrameSet *frmset = wcsp->astWCSClone();

    //  And export it.
    Tcl_SetObjResult( interp_, Tcl_NewLongObj( (long) frmset ) );
    return TCL_OK;
}

//+
//  StarRtdImage::astrestoreCmd
//
//  Purpose:
//     Restores an old WCS AstFrameSet. This should have been cloned
//     using astreplaceCmd, or be the original WCS that was associated
//     with the image. To restore the original WCS pass the parameter
//     "original".
//-
int StarRtdImage::astrestoreCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astrestoreCmd" << std::endl;
#endif
    if (!image_) {
        return error("no image loaded");
    }

    StarWCS* wcsp = getStarWCSPtr();
    if (!wcsp)
        return TCL_ERROR;

    //  Check for a single parameter. This signifies that we should
    //  restore the original FrameSet, not the last one.
    if ( argc == 1 ) {
        if ( *argv[0] == 'o' ) {
            if ( origset_ ) {
                wcsp->astWCSReplace( origset_ );

                //  Update any views to use this information.
                if ( updateViews( 2 ) != TCL_OK ) {
                    return TCL_ERROR;
                } else {
                    return TCL_OK;
                }
            } else {
                return error("no original WCS system available");
            }
        } else {
            // Unidentified argument.
            return error( argv[0], " unknown parameter, should be original");
        }
    } else {
        if (!oldset_) {
            return error("no old WCS system is available (see astreplace)");
        } else {

            //  Restore the old WCS FrameSet. Annul the pointer to
            //  register that we're no longer interested.
            wcsp->astWCSReplace( oldset_ );
            oldset_ = (AstFrameSet *) astAnnul( oldset_ );

            //  Update any views to use this information.
            if ( updateViews( 2 ) != TCL_OK ) {
                return TCL_ERROR;
            }
        }
    }
    return TCL_OK;
}

//+
//  StarRtdImage::astrefineCmd
//
//  Purpose:
//     Refines an existing FrameSet by adding a mapping that
//     transforms the old image coordinates to a new set of
//     coordinates. The mappings are restricted to combinations of
//     MatrixMaps and WinMaps. These in combination allow the solution
//     of offset, scale, shear and rotation terms.
//
//  Arguments:
//     argv, argc strings.
//
//     The arguments to this command should be a string an integer and two
//     lists. The string should be either "image" or "local", the
//     integer a value in the range 1 to 6, and the lists two sets of
//     coordinates. The first list are the existing coordinates and
//     the second the new coordinates (to which the first are to be
//     refined).
//
//     The integer defines the type of refinement to be used:
//
//        1 - just use a shift
//        2 - shift and rotation
//        3 - shift and scale
//        4 - shift, rotation and scale
//        5 - shift, rotation, scale and shear
//        6 - same as 4, except no test for reflection.
//
//  Result:
//     The result of this command is to change the newset_ WCS system
//     and also to return the RMS difference in the fit, plus the
//     transformation coefficients and a decoded version of these
//     (xshift, yshift, xscale, yscale, non-perpendicularity and the
//     rotation angle).
//
//  Notes:
//     This member works on one of two FrameSets, either the one that
//     is currently associated with the image, or one that has been
//     created by the astcreate command. This is determined by the
//     first argument which should be "image" or "local". If "image"
//     is chosen then a copy of the existing system associated with
//     the image will be made  and will become the local one (and will
//     be refined). To use this system with the image you must use the
//     command "astreplace". Note also that if you choose "image" then any
//     existing local systems will be annulled.
//-
int StarRtdImage::astrefineCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astrefineCmd" << std::endl;
#endif
    if (!image_) {
        return error("no image loaded");
    }

    StarWCS* wcsp = getStarWCSPtr();
    if (!wcsp)
        return TCL_ERROR;

    //  Decode the first argument. This should be the source of the
    //  FrameSet that we are to refine.
    char *source = argv[0];
    switch ( source[0] ) {
    case 'i': {
        //  FrameSet from image, so take a copy of it to modify
        //  non-destructively.
        if ( newset_ ) newset_ = (AstFrameSet *) astAnnul( newset_ );
        newset_ = wcsp->astWCSCopy();
    }
    break;
    case 'l': {
        //  Local, so make sure that astcreate has already been called
        //  (or this function once before).
        if ( !newset_ ) {
            return error("cannot use a local WCS system as none is "
                         "available (see astcreate, or use this command "
                         "once previously with source 'image')");
        }
    }
    break;
    default: {
        return error( source,
                      ": unknown WCS source, should be 'image' or 'local'");
    }
    }

    //  Next argument should be the type of transformation to use.
    int fittype = 1;
    if ( Tcl_GetInt( interp_, argv[1], &fittype ) != TCL_OK ) {
        return error( argv[1], " is not an integer" );
    }
    fittype = max( 1, min( 6, fittype ) );

    //  And the next two should be lists of coordinates that define the
    //  existing positions and the new positions. These should have the
    //  same number of values and be a multiple of 2.
    char **listArgv1;
    int listArgc1 = 0;
    if ( Tcl_SplitList( interp_, argv[2], &listArgc1, &listArgv1 ) != TCL_OK ) {
        return error( "failed to interpret current positions list" );
    }
    char **listArgv2;
    int listArgc2 = 0;
    if ( Tcl_SplitList( interp_, argv[3], &listArgc2, &listArgv2 ) != TCL_OK ) {
        Tcl_Free( (char *) listArgv1 );
        return error( "failed to interpret new positions list" );
    }
    if ( listArgc1 != listArgc2 ) {
        Tcl_Free( (char *) listArgv1 );
        Tcl_Free( (char *) listArgv2 );
        return error("coordinate lists contain different numbers of positions");
    }
    int npoints = listArgc1 / 2;
    if ( npoints * 2 != listArgc1 ) {
        Tcl_Free( (char *) listArgv1 );
        Tcl_Free( (char *) listArgv2 );
        return error("coordinate lists contain a odd number of values");
    }

    // Now attempt to translate all these values into doubles.
    double *xold = new double[npoints];
    double *yold = new double[npoints];
    int i, j;
    for ( i = 0, j = 0; i < listArgc1; i += 2, j++ ) {
        if ( Tcl_GetDouble( interp_, listArgv1[i], &xold[j] ) != TCL_OK ||
             Tcl_GetDouble( interp_, listArgv1[i+1], &yold[j] ) != TCL_OK ) {
            delete [] xold;
            delete [] yold;
            return error( listArgv1[i], " is not a number" );
        }
    }
    double *xnew = new double[npoints];
    double *ynew = new double[npoints];
    for ( i = 0, j = 0; i < listArgc2; i += 2, j++ ) {
        if ( Tcl_GetDouble( interp_, listArgv2[i], &xnew[j] ) != TCL_OK ||
             Tcl_GetDouble( interp_, listArgv2[i+1], &ynew[j] ) != TCL_OK ) {
            delete [] xold;
            delete [] yold;
            delete [] xnew;
            delete [] ynew;
            return error( listArgv2[i], " is not a number" );
        }
    }

    //  Now create a mapping that transforms between these positions.
    int ok = mapPositions( AST__BASE, newset_, fittype, xold, yold,
                           xnew, ynew, npoints );
    delete [] xold;
    delete [] yold;
    delete [] xnew;
    delete [] ynew;
    Tcl_Free( (char *) listArgv1 );
    Tcl_Free( (char *) listArgv2 );
    if ( ! ok ) {
        return error("failed to determine new transformation from the "
                     "given positions");
    }
    return TCL_OK;
}

//+
//   StarRtdImage::mapPositions
//
//   Purpose:
//      Determine an AST mapping that goes between two sets of
//      positions, and append it to a Frame in a FrameSet.
//
//-
int StarRtdImage::mapPositions( int iframe, AstFrameSet *fset,
                                int fittype, double *xold,
                                double *yold, double *xnew,
                                double *ynew, int npoints )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::mapPositions" << std::endl;
#endif

    //  Determine the linear transformation between the data points. if
    //  this fails then return an error.
    double tr[6];
    double resid;
    if ( ! rtdDtrn( fittype, xnew, ynew, xold, yold, npoints, tr, &resid ) ) {
        return 0;
    }

    //  Now transform this linear transform into AST mappings.
    if ( addLinear( iframe, fset, tr, fittype ) ) {

        //  Return the fit parameters as the value from this function (via
        //  Tcl). Note also includes the decoding of the fit.
        char result[TCL_DOUBLE_SPACE*13 + 14];
        double xz, yz, xs, ys, perp, orient;
        decodeLinear( tr, xz, yz, xs, ys, perp, orient );
        sprintf( result, "%f %f %f %f %f %f %f %f %f %f %f %f %f",
                 resid, tr[0], tr[1], tr[2], tr[3], tr[4], tr[5],
                 xz, yz, xs, ys, perp, orient );
        set_result( result );
        return 1;
    } else {
        return 0;
    }
}

//+
//   StarRtdImage::addLinear
//
//   Purpose:
//      Add a series of frames that describe a linear mapping to a
//      Frame in a FrameSet.
//
//-
int StarRtdImage::addLinear( int iframe, AstFrameSet *fset,
                             double *tr, int fittype )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::addLinear" << std::endl;
#endif

    //  The offset part is characterised using a WinMap, the other terms
    //  via a MatrixMap, MatrixMap first.
    if ( fittype != 1 ) {

        //  Should have some rotation, scale and shear terms.
        double matrix[4];
        matrix[0] = tr[1];
        matrix[1] = tr[2];
        matrix[2] = tr[4];
        matrix[3] = tr[5];
        AstMapping *matrixmap = (AstMapping *) astMatrixMap( 2, 2, 0,
                                                             matrix, " " );
        astRemapFrame( fset, iframe, matrixmap );
        matrixmap = (AstMapping *) astAnnul( matrixmap );
    }

    double ina[2], inb[2], outa[2], outb[2];
    ina[0] = ina[1] = 0.0;
    inb[0] = inb[1] = 100.0;
    outa[0] = ina[0] + tr[0];
    outa[1] = ina[1] + tr[3];
    outb[0] = inb[0] + tr[0];
    outb[1] = inb[1] + tr[3];
    AstMapping *winmap = (AstMapping *) astWinMap( 2, ina, inb, outa,
                                                   outb, " " );

    //  And append it to required frame of the FrameSet.
    astRemapFrame( fset, iframe, winmap );
    winmap = (AstMapping *) astAnnul( winmap );

    //  If things have gone badly tidy the error up.
    if ( !astOK ) {
        astClearStatus;
        return 0;
    }
    return 1;
}

//+
//  StarRtdImage::astassignCmd
//
//  Purpose:
//     Adds a linear transform to a WCS. The linear transform is given
//     as a set of 6 coefficients. The source of the WCS is given as
//     the first parameter and should be either local or image as in
//     astrefine.
//-
int StarRtdImage::astassignCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astassignCmd" << endl;
#endif
    if (!image_) {
        return error("no image loaded");
    }

    StarWCS* wcsp = getStarWCSPtr();
    if (!wcsp)
        return TCL_ERROR;

    //  Decode the first argument. This should be the source of the
    //  FrameSet that we are to modify.
    char *source = argv[0];
    switch ( source[0] ) {
       case 'i': {
           //  FrameSet from image, so take a copy of it to modify
           //  non-destructively.
           if ( newset_ ) newset_ = (AstFrameSet *) astAnnul( newset_ );
           newset_ = wcsp->astWCSCopy();
       }
       break;
       case 'l': {
           //  Local, so make sure that astcreate has already been called
           //  (or this function once before).
           if ( !newset_ ) {
               return error("cannot use a local WCS system as none is "
                            "available (see astcreate, or use this command "
                            "once previously with source 'image')");
           }
       }
       break;
       default: {
           return error( source,
                         ": unknown WCS source, should be 'image' or 'local'");
       }
    }

    //  Decode the parameters which should be the transformation
    //  coefficients.
    double tr[6];
    for ( int i = 1; i < 7; i++ ) {
        if ( Tcl_GetDouble( interp_, argv[i], &tr[i-1] ) != TCL_OK ) {
            return error( argv[i], " is not a number" );
        }
    }

    //  Now set the transform.
    if ( ! addLinear( AST__BASE, newset_, tr ) ) {
        return error( "failed to add linear transform to WCS");
    }
    return TCL_OK;
}

//+
//   StarRtdImage::astwriteCmd
//
//   Purpose:
//      Debugging procedure. Writes either the current local or the
//      image WCS system out to terminal using the given encoding,
//      plus a FITS channel if requested.
//
//   Arguments:
//      Either 'image' or 'local' possibly followed by an encoding.
//-
extern "C" {
    static void write_out( const char *card )
    {
        cout << card << std::endl;
    }
}

int StarRtdImage::astwriteCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astwriteCmd" << std::endl;
#endif

    // See if we are to write out the image or local WCS.
    AstFrameSet *astPtr = (AstFrameSet *) NULL;
    switch ( *argv[0] ) {
       case 'i': {

           //  FrameSet from image, so take a copy of it to modify
           //  non-destructively.
           StarWCS* wcsp = getStarWCSPtr();
           if (!wcsp)
               return TCL_ERROR;
           astPtr = wcsp->astWCSClone();
       }
       break;
       case 'l': {
           //  Local, so make sure that astcreate has already been called
           //  (or this function once before).
           if ( !newset_ ) {
               return error("cannot use a local WCS system as none is "
                            "available (see astcreate, or use this command "
                            "once previously with source 'image')");
           }
           astPtr = (AstFrameSet *) astClone( newset_ );
       }
       break;
       default: {
           return error( argv[0],
                         ": unknown WCS source, should be 'image' or 'local'");
       }
    }

    if ( astPtr ) {
        astShow( astPtr );
        (void) astAnnul( astPtr );
        if ( argc >= 2 ) {
            AstFitsChan *chan = (AstFitsChan *)
                astFitsChan( NULL, &write_out, "Encoding=%s", argv[1] );
            int nwrite = astWrite( chan, newset_ );
            if ( !astOK || nwrite == 0 ) {
                cerr << "Failed to write object via FITS channel" << std::endl;
            }
            chan = (AstFitsChan *) astAnnul( chan );

            //  If requested also show the contents of a channel.
            if ( argc == 3 ) {
                int slot = 0;
                if ( Tcl_GetInt( interp_, argv[2], &slot ) != TCL_OK ) {
                    cout << argv[1] << " is not an integer" << std::endl;
                } else {
                    slot--;
                    astShow( channels_[slot] ) ;
                }
            }
        }
    } else {
        cout << "Sorry no FrameSets are available" << std::endl;
    }
    return TCL_OK;
}

//+
//  StarRtdImage::astcopyCmd
//
//  Purpose:
//     Opens a given file and takes a copy of the WCS, if found. This
//     becomes the new FrameSet and is then available to be set as the
//     new WCS.
//-
int StarRtdImage::astcopyCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astcopyCmd" << std::endl;
#endif
    if (!image_) {
        return error("no image loaded");
    }

    //  Parse the input image name.
    char *name;
    char *fitsext;
    char *slice;
    char *path;
    if ( parseName( argv[0], &name, &fitsext, &slice, &path ) != TCL_OK ) {
        if ( name ) delete[] name;
        if ( fitsext ) delete[] fitsext;
        if ( slice ) delete[] slice;
        if ( path ) delete[] path;
        return error( argv[0], " is not an image" );
    }

    // Open the image to get at the WCS information.
    // XXX this is a very inefficient way to get at WCS information, but
    // at least it's general.
    ImageData* newimage = getStarImage( name, fitsext, slice, path );
    delete[] name;
    if ( fitsext ) delete[] fitsext;
    if ( slice ) delete[] slice;
    if ( path ) delete[] path;
    if ( ! newimage ) {
        return TCL_ERROR;
    }

    // Ok managed to read the file. Now get a copy of the WCS that comes
    // with it.
    if ( newset_ ) newset_ = (AstFrameSet *) astAnnul( newset_ );

    StarWCS* wcsp = getStarWCSPtr( newimage );
    if (!wcsp)
        return TCL_ERROR;
    newset_ = wcsp->astWCSCopy();

    // Release the newimage now we don't need it.
    delete newimage;
    return TCL_OK;
}

//+
//  StarRtdImage::astfixCmd
//
//  Purpose:
//    Makes the action of the last astreplace look like a permanent
//    change. This is performed by annuling the oldset_ and origset_
//    WCS systems and making the image WCS the new origset_ (this
//    makes any future restore commands use this instead of the first
//    image WCS).
//-
int StarRtdImage::astfixCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astfixCmd" << std::endl;
#endif
    if (!image_) {
        return error("no image loaded");
    }

    StarWCS* wcsp = getStarWCSPtr();
    if (!wcsp)
        return TCL_ERROR;

    if ( oldset_ ) {
        oldset_ = (AstFrameSet *) astAnnul( oldset_ );
    }
    if ( origset_ ) {
        origset_ = (AstFrameSet *) astAnnul( origset_ );
    }
    origset_ = wcsp->astWCSClone();
    return TCL_OK;
}

//+
//  StarRtdImage::astwcs2pixCmd
//
//  Purpose:
//     Given an RA/Dec position in the system, equinox and epoch of
//     the image WCS, return the equivalent X and Y position.
//
//  Notes:
//     This provides some of the functionality of the convert command,
//     but avoids any issues to do with equinoxes, celestial
//     coordinates systems (FK5/FK4 etc.) and epochs, which are
//     assumed to be correct in the image WCS and in the values that
//     are given.
//-
int StarRtdImage::astwcs2pixCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astwcs2pixCmd" << std::endl;
#endif

    // Convert input position, which may be in H:M:S format, to
    // degrees.  (note values may only be in HH:MM:SS/DD:MM:SS or
    // DD.ddd/DD.ddd no HH.hh, 2000.0 mean values are not converted to
    // another equinox)
    WorldCoords wcs( argv[0], argv[1], 2000.0, 1);
    if ( wcs.status() != TCL_OK ) {
        return TCL_ERROR;
    }
    double x = wcs.ra_deg();
    double y = wcs.dec_deg();

    //  Convert from RA/Dec to x,y.
    worldToImageCoords(x, y, 0);

    //  And return the values as the result of this command.
    char *buffer = Tcl_Alloc( TCL_DOUBLE_SPACE * 2 + 2 );
    sprintf( buffer, "%g %g", x, y );
    set_result( buffer );
    return TCL_OK;
}

//+
//  StarRtdImage::astpix2wcsCmd
//
//  Purpose:
//     Given an image X and Y position return the equivalent RA/Dec
//     position (and image equinox) in the system, equinox and epoch of
//     the image WCS system.
//
//  Input:
//     X and Y position followed by an optional third parameter indicating if
//     the result should be reported even if outside the image bounds and a
//     fourth optional parameter indicating if the result should be formatted
//     (otherwise decimal degrees are returned). If the fourth parameter is
//     needed then the third is not optional.
//
//  Result:
//     RA and Dec either in degrees or formatted as the the related axes
//     (plus a trailing equinox).
//
//  Notes:
//     This provides some of the functionality of the convert command,
//     but avoids any issues to do with equinoxes, celestial
//     coordinates systems (FK5/FK4 etc.) and epochs.
//-
int StarRtdImage::astpix2wcsCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astpix2wcsCmd" << std::endl;
#endif

    //  Extract the input X and Y positions.
    double x;
    double y;
    if (Tcl_GetDouble( interp_, argv[0], &x ) != TCL_OK
        || Tcl_GetDouble( interp_, argv[1], &y ) != TCL_OK) {
        return TCL_ERROR;
    }

    int notbound = 0;
    if ( argc >= 3 ) {
        if ( *argv[2] != '0' ) {
            notbound = 1;
        }
    }

    int formatted = 0;
    if ( argc == 4 ) {
        if ( *argv[3] != '0' ) {
            formatted = 1;
        }
    }

    //  Convert to a RA/Dec in degrees (plus an unwanted equinox, which
    //  is left in place for convenience).
    char buffer[80];
    StarWCS* wcsp = getStarWCSPtr();
    wcsp->pix2wcs( x, y, notbound, buffer, 80, formatted );

    //  Set the result and return.
    set_result( buffer );
    return TCL_OK;
}

//+
//  StarRtdImage::astpix2curCmd
//
//  Purpose:
//     Given an image X and Y position return a lis of coordinates that they
//     transform into in the current frame.
//
//  Input:
//     X and Y position
//
//  Result:
//     Formatted positions transformed into the current frame (may be more
//     than 2 values).
//
//  Notes:
//     This provides some of the functionality of the convert command,
//     but avoids any issues to do with equinoxes, celestial
//     coordinates systems (FK5/FK4 etc.) and epochs and dimensionalities.
//-
int StarRtdImage::astpix2curCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astpix2curCmd" << std::endl;
#endif

    //  Extract the input X and Y positions.
    double x;
    double y;
    if (Tcl_GetDouble( interp_, argv[0], &x ) != TCL_OK
        || Tcl_GetDouble( interp_, argv[1], &y ) != TCL_OK) {
        return TCL_ERROR;
    }

    //  Convert to current coordinates.
    double wcs[7];
    int dims;
    StarWCS* wcsp = getStarWCSPtr();
    if ( wcsp->pix2wcs( x, y, wcs, dims ) == 0 ) {

        //  Format the values according to the current frame.
        AstFrameSet *iwcs = wcsp->astWCSClone();
        reset_result();
        for ( int i = 0; i < dims; i++ ) {
            append_element( astFormat( iwcs, i+1, wcs[i] ) );
        }
        (void) astAnnul( iwcs );
        return TCL_OK;
    }
    else {
        return error( "failed to transform positions" );
    }
}

//+
//  StarRtdImage::astcur2pixCmd
//
//  Purpose:
//     Given a position in current coordinates (double precision)
//     convert it to pixel coordinates.
//
//  Input:
//     Pair of double precision coordinates, plus option flag. The
//     flag if set, indicates that the coordinates should not be considered
//     as possibly celestial (input in degrees).
//
//  Result:
//     Pair of pixel coordinates.
//
//  Notes:
//     This provides some of the functionality of the convert command,
//     but avoids any issues to do with equinoxes, celestial
//     coordinates systems (FK5/FK4 etc.) and epochs.
//-
int StarRtdImage::astcur2pixCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astcur2pixCmd" << std::endl;
#endif

    //  Extract the input positions.
    double inx;
    double iny;
    if (Tcl_GetDouble( interp_, argv[0], &inx ) != TCL_OK
        || Tcl_GetDouble( interp_, argv[1], &iny ) != TCL_OK) {
        return TCL_ERROR;
    }

    //  Check for do not assumed celestial coordinates flag.
    int notcelestial = 0;
    if ( argc == 3 ) {
        if ( *argv[2] == '1' ) {
            notcelestial = 1;
        }
    }

    //  Convert to pixel coordinates.
    StarWCS* wcsp = getStarWCSPtr();
    double outx;
    double outy;
    if ( wcsp->anyWcs2pix( inx, iny, notcelestial, outx, outy ) == 0 ) {

        //  Set the result and return.
        set_result( outx, outy );
        return TCL_OK;
    }
    else {
        return error( "can't convert to image coordinates" );
    }
}

//+
//   StarRtdImage::decodeLinear
//
//   Purpose:
//      Decodes a linear transformation into a set of parameters
//      following the method used in sla_dcmpf.
//
//-
void StarRtdImage::decodeLinear( double tr[6], double &xz, double &yz,
                                 double &xs, double &ys, double &perp,
                                 double &orient )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::decodeLinear" << std::endl;
#endif
    double a = tr[0];
    double b = tr[1];
    double c = tr[2];
    double d = tr[3];
    double e = tr[4];
    double f = tr[5];

    //  Derive the scale factors.
    double rb2e2 = sqrt( b * b + e * e );
    double rc2f2 = sqrt( c * c + f * f );
    double xsc = 1.0;
    double ysc = 1.0;
    if ( ( b * f - c * e ) > 0.0 ) {
        xsc = rb2e2;
    } else {
        b = -b;
        c = -c;
        xsc = -rb2e2;
    }
    ysc = rc2f2;

    // The non-perpendicularity.
    double p1 = 0.0;
    double p2 = 0.0;
    if (  c != 0.0 || f != 0.0 ) {
        p1 = atan2( c, f );
    }
    if ( e != 0.0 || b != 0.0 ) {
        p2 = atan2( e, b );
    }
    double p = p1 + p2;

    //  Orientation.
    double ws = c * rb2e2 - e * rc2f2;
    double wc = b * rc2f2 + f * rb2e2;
    double wor = 0.0;
    if ( ws != 0.0 || wc != 0.0 ) {
        wor = atan2( ws, wc );
    }

    //  Offsets.
    double hp = p / 2.0;
    double shp = sin( hp );
    double chp = cos( hp );
    double sor = sin( wor );
    double cor = cos( wor );
    double det = xsc * ysc * ( chp + shp ) * ( chp - shp );
    double x0 = 0.0;
    double y0 = 0.0;
    if ( fabs( det ) > 0.0 ) {
        x0 = ysc*(a*(chp*cor-shp*sor)-d*(chp*sor+shp*cor))/det;
        y0 = xsc*(a*(chp*sor-shp*cor)+d*(chp*cor+shp*sor))/det;
    }

    //  Set output variables.
    xz = x0;
    yz = y0;
    xs = xsc;
    ys = ysc;
    perp = p * r2d_;
    orient = wor * r2d_;
}

//+
//  StarRtdImage::astbootstats
//
//  Purpose:
//     Given a list of RA/Dec, X,Y pairs derive some useful
//     statistics. These are intended for use when bootstrapping WCS
//     systems from reference positions (and no existing WCS FrameSet
//     is available for calculating reliable distances etc.).
//
//  Arguments:
//
//        1) list of set of RA/Dec pairs.
//        2) list "corresponding" X,Y postions.
//        3) X reference position.
//        4) Y reference position.
//
//
//   Result:
//      A list of the following values:
//
//         RA, Dec of X,Y reference position     (degrees)
//         X image scale                         (degrees/pixel)
//         Y image scale                         (degrees/pixel)
//
//-
int StarRtdImage::astbootstatsCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astbootstatsCmd" << std::endl;
#endif

    //  Extract the values from the lists.
    char **listArgv1;
    int listArgc1 = 0;
    if ( Tcl_SplitList( interp_, argv[0], &listArgc1, &listArgv1 ) != TCL_OK ) {
        return error( "failed to interpret RA/Dec positions list" );
    }
    char **listArgv2;
    int listArgc2 = 0;
    if ( Tcl_SplitList( interp_, argv[1], &listArgc2, &listArgv2 ) != TCL_OK ) {
        Tcl_Free( (char *) listArgv1 );
        return error( "failed to interpret X,Y positions list" );
    }
    if ( listArgc1 != listArgc2 ) {
        Tcl_Free( (char *) listArgv1 );
        Tcl_Free( (char *) listArgv2 );
        return error("coordinate lists contain different numbers of positions");
    }
    int npoints = listArgc1 / 2;
    if ( npoints * 2 != listArgc1 ) {
        Tcl_Free( (char *) listArgv1 );
        Tcl_Free( (char *) listArgv2 );
        return error("coordinate lists contain a odd number of values");
    }

    // Check have at least two positions.
    if ( listArgc1 < 2 ) {
        Tcl_Free( (char *) listArgv1 );
        Tcl_Free( (char *) listArgv2 );
        return error("need at least two positions");
    }

    // Now attempt to translate all these values into doubles.
    double *ra = new double[npoints];
    double *dec = new double[npoints];
    int i, j;
    for ( i = 0, j = 0; i < listArgc1; i += 2, j++ ) {

        //  These should either be in degrees or H/D:M:S strings
        // (note values may only be in HH:MM:SS/DD:MM:SS or DD.ddd/DD.ddd
        //  no HH.hh, 2000.0 mean values are not converted to another equinox)
        WorldCoords wcs( listArgv1[i], listArgv1[i+1], 2000, 1);
        if ( wcs.status() != TCL_OK ) {
            delete [] ra;
            delete [] dec;
            return error( listArgv1[i], " is not a celestial coordinate value" );
        }
        ra[j] = wcs.ra_deg();
        dec[j] = wcs.dec_deg();
    }
    double *x = new double[npoints];
    double *y = new double[npoints];
    for ( i = 0, j = 0; i < listArgc2; i += 2, j++ ) {
        if ( Tcl_GetDouble( interp_, listArgv2[i], &x[j] ) != TCL_OK ||
             Tcl_GetDouble( interp_, listArgv2[i+1], &y[j] ) != TCL_OK ) {
            delete [] ra;
            delete [] dec;
            delete [] x;
            delete [] y;
            return error( listArgv2[i], " is not a number" );
        }
    }

    // Translate the X,Y reference positions into doubles.
    double xref = 0.0;
    double yref = 0.0;
    if ( Tcl_GetDouble( interp_, argv[2], &xref ) != TCL_OK ||
         Tcl_GetDouble( interp_, argv[3], &yref ) != TCL_OK ) {
        delete [] ra;
        delete [] dec;
        delete [] x;
        delete [] y;
        return error( "cannot understand x or y reference position" );
    }

    // Find the minimum RA and Dec.
    double ra_ref = ra[0];
    double dec_ref = dec[0];
    int ra_index = 0;
    int dec_index = 0;
    for ( i = 1; i < npoints; i++ ) {
        if ( ra[i] < ra_ref ) {
            ra_ref = ra[i];
            ra_index = i;
        }
        if ( dec[i] < dec_ref ) {
            dec_ref = dec[i];
            dec_index = i;
        }
    }

    // Derive the displacements in RA and Dec from the smallest RA and
    // Dec position (use distance to avoid discontinuity problems!). Also
    // avoid the zero displacement position which causes occasional
    // numeric errors.
    for ( i = 0; i < npoints; i++ ) {
        if ( i != ra_index ) {
            ra[i] = WorldCoords::dist( ra_ref, dec_ref, ra[i], dec_ref ) /60.0;
        }
        if ( i != dec_index ) {
            dec[i] = WorldCoords::dist( ra_ref, dec_ref, ra_ref, dec[i] ) / 60.0;
        }
    }
    ra[ra_index] = 0.0;
    dec[dec_index] = 0.0;

    //  Now use the X,Y positions and the displacements to do a linear
    //  fit (the fittype should be reduced until appropriate for the
    //  number of positions given).
    int fittype = 5;
    double resid;
    double tr[6];
    double meanra;
    double meandec;
    double xscale;
    double yscale;
    if ( rtdDtrn( fittype, ra, dec, x, y, npoints, tr, &resid ) ) {

        //  Use this fit to derive the required values.
        double xz, yz, perp, orient;
        decodeLinear( tr, xz, yz, xscale, yscale, perp, orient );
        meanra = ra_ref + tr[0] + xref*tr[1] + yref*tr[2];
        meandec = dec_ref + tr[3] + xref*tr[4] + yref*tr[5];
    } else {

        // No fit, just make up some values.
        // Mean RA and Dec.
        meanra = 0.0;
        meandec = 0.0;
        for ( i = 0; i < npoints; i++ ) {
            meanra += ra[i];
            meandec += dec[i];
        }
        meanra = ra_ref + meanra / double(npoints);
        meandec = dec_ref + meandec / double(npoints);

        //  Image scales.
        double xmax = -DBL_MAX;
        double xmin = DBL_MAX;
        int max_index = -1;
        int min_index = -1;
        for ( i = 0; i < npoints; i++ ) {
            if ( x[i] > xmax ) {
                xmax = x[i];
                max_index = i;
            }
            if ( x[i] < xmin ) {
                xmin = x[i];
                min_index = i;
            }
        }
        xscale = (ra[max_index] - ra[min_index]) / (xmax - xmin);

        //  Image scales.
        double ymax = -DBL_MAX;
        double ymin = DBL_MAX;
        max_index = -1;
        min_index = -1;
        for ( i = 0; i < npoints; i++ ) {
            if ( y[i] > ymax ) {
                ymax = y[i];
                max_index = i;
            }
            if ( y[i] < ymin ) {
                ymin = y[i];
                min_index = i;
            }
        }
        yscale = (ra[max_index] - ra[min_index]) / (ymax - ymin);
    }

    //  Construct a return list of the values.
    char result[TCL_DOUBLE_SPACE*5 + 5];
    sprintf( result, "%f %f %f %f", meanra, meandec, xscale, yscale );
    set_result( result );

    //  Release memory.
    Tcl_Free( (char *) listArgv1 );
    Tcl_Free( (char *) listArgv2 );
    delete [] ra;
    delete [] dec;
    delete [] x;
    delete [] y;
    return TCL_OK;
}

//+
//   StarRtdImage::astsystemCmd
//
//   Purpose:
//      Modifies the WCS system with new celestial/pixel coordinates.
//
//    Notes:
//       The system attributes are passed in as the second element in a
//       pre-formatted manner (such as can be used by as astSet
//       routine, i.e. system=newsystem epoch=epoch equinox=equinox).
//       The first value is the source of the WCS to modify and should
//       be image or local (as in astassign and astrefine).
//
//       The third (optional) argument determines whether we get a
//       FrameSet describing the transformation from the current base
//       frame to this new system, or from the current current
//       frame. The default is to transform from the base frame (thus
//       retaining a connection with the image pixels) and is
//       indicated by the value 0. This option only applies when
//       the fourth option is set to 1.
//
//       The fourth (optional) argument determines if the new system
//       should be clean, that is a new skyframe, or be just a modified copy
//       of the existing one. This latter option keeps all related metadata,
//       like skyref etc., but has the side-effect of usually having the
//       wrong equinox and epochs (when say changing from FK5 to FK4).
//
//       If system is set to "pixel" then a pixel coordinate system is
//       used, rather than sky coordinates.
//-
int StarRtdImage::astsystemCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astsystemCmd" << std::endl;
#endif
    if (!image_) {
        return error("no image loaded");
    }

    //  Decode the first argument. This should be the source of the
    //  FrameSet that we are to modify.
    char *source = argv[0];
    switch ( source[0] ) {
       case 'i': {
           //  FrameSet from image, so take a copy of it to modify
           //  non-destructively.
           if ( newset_ ) newset_ = (AstFrameSet *) astAnnul( newset_ );

           StarWCS* wcsp = getStarWCSPtr();
           if ( !wcsp ) {
               return TCL_ERROR;
           }
           newset_ = wcsp->astWCSCopy();
       }
       break;
       case 'l': {
           //  Local, so make sure that astcreate has already been called
           //  (or this function once before).
           if ( !newset_ ) {
               return error("cannot use a local WCS system as none is "
                            "available (see astcreate, or use this command "
                            "once previously with source 'image')");
           }
       }
       break;
       default: {
           return error( source,
                         ": unknown WCS source, should be 'image' or 'local'");
       }
    }

    AstFrame *newfrm = NULL;
    if ( strstr( argv[1], "pixel" ) == NULL ) {

        //  Ok now create a new SkyFrame with the options we have been given.
        if ( argc <= 3 || ( argc == 4 && *argv[3] == '1' ) ) {
            newfrm = (AstFrame *) astSkyFrame( argv[1], " " );
        }
        else {
            //  Just apply attributes to the FrameSet copy.
            astSet( newset_, argv[1], " " );
            if ( !astOK ) {
                astClearStatus;
                return error( "failed to modify coordinate system" );
            }
            return TCL_OK;
        }
    }
    else {
        //  Want a pixel coordinate system (only sensible for NDFs when
        //  pixel coordinates are different from grid coordinates).
        newfrm = (AstFrame *) makeGridWCS( );
    }
    if ( !astOK ) {

        //  If any of the above failed, then report the error.
        astClearStatus;
        return error ( "failed to establish new system coordinates system");
    }
    else {
        //  Get a mapping to convert to the new system and add this
        //  to the current frameset. Note we convert from BASE frame to
        //  the new skyframe to force AST to retain all the current
        //  frameset mappings or just get the current-current mapping
        //  (useful for converting between celestial coordinates).
        if ( argc == 2 || ( argc >= 3 && *argv[2] == '0' ) ) {
            astSetI( newset_, "Current", AST__BASE );
        }
        AstFrameSet *cvt = (AstFrameSet *) astConvert( newset_, newfrm, "" );
        newfrm = (AstFrame *) astAnnul( newfrm );
        if ( astOK ) {
            newset_ = (AstFrameSet *) astAnnul( newset_ );
            newset_ = cvt;
        }
        else {
            cvt = (AstFrameSet *) astAnnul( cvt );
            astClearStatus;
            return error ( "failed to convert from existing system "
                           "to new system");
        }
    }
    return TCL_OK;
}

//
//  Draw a symbol on the image with the given shape at the given coordinates
//  (in the given x,y units), with the given radius (in radius_units),
//  bg and fg color, canvas tags list, x/y ratio and rotation angle.
//
//  shape may be one of "circle", "square", "plus", "cross", "triangle",
//  "diamond", "ellipse", "compass", "line", "arrow", "rotbox", "rectangle"
//  and "stcshape".
//
//  x and y are the coordinates "xy_units", which is one of the units
//  accepted by the Rtd commands (canvas, image, screen, "wcs $equinox",
//  "deg $equinox"), or if the shape is "stcshape" the STC description
//  which describes the shape and its units, for stcshape many of the
//  other arguments are ignored, but x and y should still be an indicative
//  position if possible (for clipping etc.).
//
//  The radius value is interpreted in radius_units.
//
//  bg and fg are X color names for the symbol (may be the same).
//
//  symbol_tags should be a Tcl list of canvas tags for the symbol.
//
//  ratio and angle are used to stretch/shrink and rotate the symbol.
//
//  label is an optional text for a label to place near the symbol.
//
//  label_tags should be a Tcl list of canvas tags for the label, or
//  an empty or null string, if there is no label.
//
//  Returns an error if the coordinates or part of the symbol are off
//  the image.
//
//  Uses world coordinates, if available, for the rotation and orientation,
//  for symbols that support it (i.e.: rotation is relative to WCS north).
//
//  Override Skycat equivalent to add rtd_ellipse, rtd_rotbox and
//  rectangle.
//
int StarRtdImage::draw_symbol( const char *shape,
                               double x, double y, const char *xy_units,
                               double radius, const char *radius_units,
                               const char *bg, const char *fg,
                               const char *symbol_tags,
                               double ratio, double angle,
                               const char *label, const char *label_tags )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::draw_symbol: " << shape << std::endl;
#endif
    static struct SymbolTab {
        // symbol name
        const char* name;
        // ptr to method to draw the symbol
        int (StarRtdImage::*fptr)(double x, double y, const char *xy_units,
                                  double radius, const char *radius_units,
                                  const char *bg, const char *fg,
                                  const char *symbol_tags,
                                  double ratio, double angle,
                                  const char *label, const char *label_tags);
    } symbols[] = {
        {"arrow", &Skycat::draw_arrow},
        {"circle", &Skycat::draw_circle},
        {"compass", &Skycat::draw_compass},
        {"cross", &Skycat::draw_cross},
        {"diamond", &Skycat::draw_diamond},
        {"ellipse", &StarRtdImage::draw_ellipse},
        {"line", &Skycat::draw_line},
        {"plus", &Skycat::draw_plus},
        {"rectangle", &StarRtdImage::draw_rectangle},
        {"rotbox", &StarRtdImage::draw_rotbox},
        {"square", &Skycat::draw_square},
        {"stcshape", &StarRtdImage::draw_stcshape},
        {"triangle", &Skycat::draw_triangle}
    };
    static int nsymbols = sizeof(symbols)/sizeof(SymbolTab);

    // Symbol shape
    for (int i = 0; i < nsymbols; i++) {
        if (strcmp(shape, symbols[i].name) == 0) {
            return (this->*symbols[i].fptr)( x, y, xy_units, radius,
                                             radius_units, bg, fg,
                                             symbol_tags, ratio, angle,
                                             label, label_tags );
        }
    }
    return error( "invalid plot symbol (%s)", shape );
}

//
//  Draw an ellipse at the given coords.
//  See draw_symbol for a description of the arguments.
//
//  This method overrides Skycat version to use local rtd_ellipse,
//  rather than a smoother polygon.
//
int StarRtdImage::draw_ellipse( double x, double y, const char *xy_units,
                                double radius, const char *radius_units,
                                const char *bg, const char *fg,
                                const char *symbol_tags, double ratio,
                                double angle, const char *label,
                                const char *label_tags )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::draw_ellipse" << std::endl;
#endif
    double cx, cy, nx, ny, ex, ey;
    if ( get_compass( x, y, xy_units, radius, radius_units, ratio, angle,
                      cx, cy, nx, ny, ex, ey ) != TCL_OK ) {
        reset_result(); // ignore off scale symbols
        return TCL_OK;
    }

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if ( strcmp( fg, bg ) != 0 ) {
        os << canvasName_ << " create rtd_ellipse "
           << cx << " " << cy << " "
           << ex << " " << ey << " "
           << nx << " " << ny
           << " -outline " << bg
           << " -fill " << bg
           << " -width 3 -stipple pat7 -tags {" << symbol_tags << "}"
           << std::endl;
    }
    os << canvasName_ << " create rtd_ellipse "
       << cx << " " << cy << " "
       << ex << " " << ey << " "
       << nx << " " << ny
       << " -outline " << fg
       << " -fill " << fg
       << " -width 1 -stipple pat7 -tags {" << symbol_tags << "}"
       << std::endl;

    if ( label && strlen( label ) ) {
        make_label( os, label, cx, cy, label_tags, fg );
    }

    int result = eval( os.str().c_str() );
    return result;
}

//
//  Draw an ellipse at the given coords, rtd coords version.
//  The x and y coordinates are the position of the centre, the end of the
//  semi-major axis and the semi-minor.
//
int StarRtdImage::draw_rtdellipse( double *x, double *y, const char *xy_units,
                                   const char *bg, const char *fg,
                                   const char *symbol_tags, const char *label,
                                   const char *label_tags )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::draw_ellipse" << std::endl;
#endif
    //  Convert given units into canvas coordinates.
    for ( int i = 0; i < 3; i++ ) {
        if ( convertCoords( 0, x[i], y[i], xy_units, "canvas" ) != TCL_OK ) {
            x[i] = 0.0;
            y[i] = 0.0;
        }
    }

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if ( strcmp( fg, bg ) != 0 ) {
        os << canvasName_ << " create rtd_ellipse "
           << x[0] << " " << y[0] << " "
           << x[1] << " " << y[1] << " "
           << x[2] << " " << y[2]
           << " -outline " << bg
           << " -fill " << bg
           << " -width 3 -stipple pat7 -tags {" << symbol_tags << "}"
           << std::endl;
    }
    os << canvasName_ << " create rtd_ellipse "
       << x[0] << " " << y[0] << " "
       << x[1] << " " << y[1] << " "
       << x[2] << " " << y[2]
       << " -outline " << fg
       << " -fill " << fg
       << " -width 1 -stipple pat7 -tags {" << symbol_tags << "}"
       << std::endl;

    if ( label && strlen( label ) ) {
        make_label( os, label, x[0], y[0], label_tags, fg );
    }
    return eval( os.str().c_str() );
}

//
//  Draw an reorientable box at the given coords.
//  See draw_symbol for a description of the arguments.
//
int StarRtdImage::draw_rotbox(double x, double y, const char *xy_units,
                              double radius, const char *radius_units,
                              const char *bg, const char *fg,
                              const char *symbol_tags, double ratio,
                              double angle, const char *label,
                              const char *label_tags)
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::draw_rotbox" << std::endl;
#endif
    double cx, cy, nx, ny, ex, ey;
    if (get_compass(x, y, xy_units, radius, radius_units, ratio, angle,
                    cx, cy, nx, ny, ex, ey) != TCL_OK) {
        reset_result(); // ignore off scale symbols
        return TCL_OK;
    }

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
        os << canvasName_ << " create rtd_rotbox "
           << cx << " " << cy << " "
           << ex << " " << ey << " "
           << nx << " " << ny
           << " -outline " << bg
           << " -fill " << bg
           << " -width 3 -stipple pat7 -tags {" << symbol_tags << "}"
           << std::endl;
    }
    os << canvasName_ << " create rtd_rotbox "
       << cx << " " << cy << " "
       << ex << " " << ey << " "
       << nx << " " << ny
       << " -outline " << fg
       << " -fill " << fg
       << " -width 1 -stipple pat7 -tags {" << symbol_tags <<  "}"
       << std::endl;

    if (label && strlen(label))
        make_label(os, label, cx, cy, label_tags, fg);

    int result = eval( os.str().c_str() );
    return result;
}

//
//  Draw an axis-aligned rectangle at the given coords.
//  See draw_symbol for a description of the arguments.
//
int StarRtdImage::draw_rectangle( double x, double y, const char *xy_units,
                                  double radius, const char *radius_units,
                                  const char *bg, const char *fg,
                                  const char *symbol_tags, double ratio,
                                  double angle, const char *label,
                                  const char *label_tags )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::draw_rectangle" << std::endl;
#endif
    double rx = radius, ry = radius * ratio;
    if (convertCoords(0, x, y, xy_units, "canvas") != TCL_OK
        || convertCoords(1, rx, ry, radius_units, "canvas") != TCL_OK) {
        reset_result(); // ignore off scale symbols
        return TCL_OK;
    }
    double x0 = x - rx;
    double y0 = y - ry;
    double x1 = x + rx;
    double y1 = y + ry;

    // if using 2 colors, draw 2 symbols, for visibility, one thicker
    std::ostringstream os;
    if (strcmp(fg, bg) != 0) {
        os << canvasName_ << " create rect "
           << x0-1 << ' ' << y0-1 << ' ' << x1+1 << ' ' << y1+1
           << " -outline " << bg
           << " -fill " << bg
           << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}"
           << std::endl;
    }
    os << canvasName_ << " create rect "
       << x0 << ' ' << y0 << ' ' << x1 << ' ' << y1
       << " -outline " << fg
       << " -fill " << fg
       << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}"
       << std::endl;

    if (label && strlen(label))
        make_label(os, label, x, y, label_tags, fg);

    return eval(os.str().c_str());

}

//
//  Draw a polygon. Like the other shapes, except only acceptable as
//  an STC shape. Coordinates are in degrees.
//
int StarRtdImage::draw_polygon( int npoint, double *x, double *y,
                                const char *xy_units,
                                const char *bg, const char *fg,
                                const char *symbol_tags, const char *label,
                                const char *label_tags )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::draw_polygon" << std::endl;
#endif
    //  Convert given units into canvas coordinates.
    for ( int i = 0; i < npoint; i++ ) {
        if ( convertCoords( 0, x[i], y[i], xy_units, "canvas" ) != TCL_OK ) {
            x[i] = 0.0;
            y[i] = 0.0;
        }
    }

    //  If using 2 colors, draw 2 symbols, for visibility, one thicker
    //  If needed look into using rtd_polygon for speed.
    std::ostringstream os;
    if ( strcmp( fg, bg ) != 0 ) {
        os << canvasName_ << " create polygon ";
        for ( int i = 0; i < npoint; i++ ) {
            os << x[i] << ' ' << y[i] << ' ';
        }
        os << " -outline " << bg
           << " -fill " << bg
           << " -width 2 -stipple pat7 -tags " << "{" << symbol_tags << "}"
           << std::endl;
    }
    os << canvasName_ << " create polygon ";
    for ( int i = 0; i < npoint; i++ ) {
        os << x[i] << ' ' << y[i] << ' ';
    }
    os << " -outline " << fg
       << " -fill " << fg
       << " -width 1 -stipple pat7 -tags " << "{" << symbol_tags << "}"
       << std::endl;

    //  Where to position label. Just next to first point.
    if ( label && strlen( label ) ) {
        make_label( os, label, x[0], y[0], label_tags, fg );
    }
    return eval( os.str().c_str() );

}

//
//  Draw an STC-S encoded shape, if possible. The shape must parse
//  into one of the supported symbol types.
//
int StarRtdImage::draw_stcshape( double x, double y, const char *stc_shape,
                                 double radius, const char *radius_units,
                                 const char *bg, const char *fg,
                                 const char *symbol_tags, double ratio,
                                 double angle, const char *label,
                                 const char *label_tags )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::draw_stcshape" << std::endl;
#endif

    //  Get the actual shape.
    AstStcsChan *chan = astStcsChan( ChannelSource, NULL, " " );
    channelData_.read = 0;
    channelData_.content = stc_shape;
    astPutChannelData( chan, &channelData_ );
    AstRegion *region = (AstRegion *) astRead( chan );

    //  Create the mapping from region coordinates to image coordinates.
    //  Need to do this as the symbol plotting applies further transformations
    //  (the skycat equinox method) that we should avoid. The mapping is
    //  cached so that we do not astConvert every time, use the
    //  clearStcMapping method to reset this for new catalogues, or when the
    //  catalogue is known to have mixed coordinate systems in the regions.
    if ( region != NULL && stcMapping_ == NULL ) {
        StarWCS* wcsp = getStarWCSPtr();
        AstFrameSet *wcs = wcsp->astWCSClone();

        //  Need base to become current, so we get a transform to GRID.
        int base = astGetI( wcs, "Base" );
        int current = astGetI( wcs, "Current" );
        astSetI( wcs, "Current", base );

        AstFrameSet *fs = (AstFrameSet *) astConvert( wcs, region, " " );
        astSetI( wcs, "Current", current );
        astSetI( wcs, "Base", base );

        if ( fs != NULL ) {
            AstMapping *map = (AstMapping *) astGetMapping( fs, AST__BASE,
                                                            AST__CURRENT );

            //  Removal all region effects (otherwise BAD coordinates outside
            //  the region).
            stcMapping_ = (AstMapping *) astRemoveRegions( map );

            (void) astAnnul( map );
            (void) astAnnul( fs );
        }
        (void) astAnnul( wcs );
    }
    if ( !astOK ) {
        astClearStatus;
    }

    if ( astIsAEllipse( region ) ) {

        //  Get the parameters. Use this rather than astGetRegionPoints
        //  as we get the end points of the ellipse axes.
        double centre[2];
        double a;
        double b;
        double angle;
        double p1[2];
        double p2[2];
        astEllipsePars( (AstEllipse *)region, centre, &a, &b, &angle, p1, p2 );

        if ( stcMapping_ != NULL ) {
            //  Transform to image coordinates.
            double xin[3];
            double yin[3];
            double xout[3];
            double yout[3];
            xin[0] = centre[0];
            yin[0] = centre[1];
            xin[1] = p1[0];
            yin[1] = p1[1];
            xin[2] = p2[0];
            yin[2] = p2[1];
            astTran2( stcMapping_, 3, xin, yin, 0, xout, yout );
            if ( !astOK ) {
                astClearStatus;
            }
            return draw_rtdellipse( xout, yout, "image", bg, fg, symbol_tags,
                                    label, label_tags );
        }
        else {
            //  Assume we're in deg J2000 and proceed.
            centre[0] *= r2d_;
            centre[1] *= r2d_;
            a *= r2d_;
            b *= r2d_;

            //  To correct orientation (Y through X).
            angle = angle * r2d_ + 90.0;

            return draw_ellipse( centre[0], centre[1], "deg", b, "deg", bg, fg,
                                 symbol_tags, a/b, angle, label, label_tags );
        }
    }
    else if ( astIsAPolygon( region ) ) {

        //  Transform the region into the coordinates of the image WCS,
        //  which needs to be a celestial system, by matching the system
        //  epoch and equinox.
        int npoint = 0;
        double *points;
        astGetRegionPoints( region, 0, 0, &npoint, points );
        if ( npoint > 0 ) {
            points = new double[npoint*2];
            astGetRegionPoints( region, npoint, 2, &npoint, points );

            double *x = new double[npoint];
            double *y = new double[npoint];
            const char *units;

            //  Transform to GRID, if no mapping just push on assuming degrees.
            if ( stcMapping_ != NULL ) {
                double *xin = new double[npoint];
                double *yin = new double[npoint];
                for ( int i = 0; i < npoint; i++ ) {
                    xin[i] = points[i];
                }
                for ( int i = npoint, j = 0; i < npoint*2; i++, j++ ) {
                    yin[j] = points[i];
                }
                units = "image";
                astTran2( stcMapping_, npoint, xin, yin, 0, x, y );
                delete[] xin;
                delete[] yin;

            }
            else {
                //  Radians to degrees.
                units = "deg";
                for ( int i = 0; i < npoint; i++ ) {
                    x[i] = points[i] * r2d_;
                }
                for ( int i = npoint, j = 0; i < npoint*2; i++, j++ ) {
                    y[j] = points[i] * r2d_;
                }
            }
            int result = draw_polygon( npoint, x, y, units, bg, fg,
                                       symbol_tags, label, label_tags );
            delete[] points;
            delete[] x;
            delete[] y;
            return result;
        }
        if ( !astOK ) {
            astClearStatus;
        }
        return error( "no points in STC region" );
    }

    //  Don't exit with AST still in error.
    if ( !astOK ) {
        astClearStatus;
    }
    return error( "Unknown STC shape" );
}

//+
//   StarRtdImage::blankcolor
//
//   Purpose:
//       Sets the colour of any blank pixels.
//
//    Notes:
//       The colour given should be recognisably by Tk.
//-
int StarRtdImage::blankcolorCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::blankcolorCmd (" << argv[0] << ")" << std::endl;
#endif

    //  Decode the colour into an XColor structure.
    XColor *xcolor = Tk_GetColor( interp_, tkwin_, argv[0] );
    if ( xcolor == NULL ) {
        return error( "failed to interpret colour:", argv[0] );
    } else {
        //  Pass on pixel value to ImageColor as background value.
        colors_->setBackground( xcolor->pixel );

        //  Force update (same as if color table had changed).
        colorUpdate( 1 );
    }
    Tk_FreeColor( xcolor );
    return TCL_OK;
}

//+
//   StarRtdImage::biasimage
//
//   Purpose:
//       Configure the bias images.
//
//    Notes:
//       See RtdImage version. Overridden to support NDFs.
//-
int StarRtdImage::biasimageCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::biasimageCmd ("<< argv[0] << ")"<< std::endl;
#endif

    // Do nothing just yet, unless it's a FITS image.
    if ( ! isfits() ) {
        return TCL_OK;
    }
    return RtdImage::biasimageCmd( argc, argv );
}


//+
//   StarRtdImage::sliceCmd
//
//   Purpose:
//      Implements an enhanced version of the "spectrum" command.
//      This version also offers the ability to have the real X or Y
//      coordinates returned with the slice index and data values.
//      These are expected to be used to display the original
//      coordinates.
//
//   Arguments:
//
//      <bltGraph> is the path name of a BLT graph widget to display
//                  the plot of the pixel intensities along the line
//
//      <bltElem>  is the name of the element in the graph that should
//                 receive the data
//
//      x0, y0,    are the end points of a line in the image in the
//      x1, y1     given coordinate system (canvas, image, screen,
//                 wcs, deg).
//
//      xy_units   units of the line coordinates.
//
//      iVector    (returned) name of a BLT vector to receive the X
//                 axes indices.
//
//      vVector    (returned) name of a BLT vector to receive the
//                 data values (Y axis).
//
//      xVector    (returned) name of a BLT vector to receive the X
//                 coordinate values.
//
//      yVector    (returned) name of a BLT y vector to receive the Y
//                 coordinate values.
//
//   Return:
//      The number of positions written to the vectors.
//-

int StarRtdImage::sliceCmd(int argc, char *argv[])
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::sliceCmd (" << argc << ")" << std::endl;
#endif
    if (!image_) {
        return TCL_OK;
    }

    //  Convert line extent to image coords.
    double rx0, ry0, rx1, ry1;
    if (convertCoordsStr( 0, argv[2], argv[3], NULL, NULL,
                          rx0, ry0, argv[6], "image") != TCL_OK
        || convertCoordsStr( 0, argv[4], argv[5], NULL, NULL,
                             rx1, ry1, argv[6], "image") != TCL_OK ) {
        return TCL_ERROR;
    }

    //  Get distance between endpoints (add a little to be safe).
    //  Round to nearest integer to respect pixel boundaries.
    int x0 = int(rx0+0.5);
    int y0 = int(ry0+0.5);
    int x1 = int(rx1+0.5);
    int y1 = int(ry1+0.5);
    int w = abs(x1-x0) + 1;
    int h = abs(y1-y0) + 1;
    int dist = (int)sqrt((double)w*w + (double)h*h) + 4;

    double* ivvalues = new double[dist*2];
    double* xyvalues = new double[dist*2];

    //  Fill the xyvalues array and set numValues to the actual number
    //  of points
    int numValues = image_->getSpectrum( ivvalues, x0, y0, x1, y1 );

    //  Convert the slice indices to the equivalent X and Y coordinates.
    //  To do this we reproduce the algorithm of image->getSpectrum.
    int i = 0;
    if ( y1 == y0 ) {

        //  Horizontal line (modified to run the direction as supplied).
        if ( x0 < x1 ) {
            for ( int x = x0; x <= x1; x++ ) {
                xyvalues[i*2] = x;
                xyvalues[i*2+1] = y0;
                i++;
            }
        } else {
            for ( int x = x0; x >= x1; x-- ) {
                xyvalues[i*2] = x;
                xyvalues[i*2+1] = y0;
                i++;
            }

            //  The data values are also reversed in this case.
            int j = numValues - 1;
            for ( i = 0; i < numValues/2; i++ ) {
                swap( ivvalues[i*2+1], ivvalues[j*2+1] );
                j--;
            }
        }
    } else if  ( x1 == x0 ) {

        //  Vertical line (modified to run the direction as supplied).
        if ( y0 < y1 ) {
            for ( int y = y0; y <= y1; y++ ) {
                xyvalues[i*2] = x0;
                xyvalues[i*2+1] = y;
                i++;
            }
        } else {
            for ( int y = y0; y >= y1; y-- ) {
                xyvalues[i*2] = x0;
                xyvalues[i*2+1] = y;
                i++;
            }

            //  The data values are also reversed in this case.
            int j = numValues - 1;
            for ( i = 0; i < numValues/2; i++ ) {
                swap( ivvalues[i*2+1], ivvalues[j*2+1] );
                j--;
            }
        }
    } else {

        // sloped line
        // use Bresenham midpoint line scan-conversion algorithm
        // see: Computer Graphics Princ. a. Pract., 2nd Ed., p. 78
        // also see x11r5/mit/server/ddx/cfb/cfbline.c, cfbbres.c

        int x = x0;
        int y = y0;
        int e, e1, e2, e3;        // bresenham error and increments
        int len;                  // length of segment
        int adx = x1 - x0;        // abs values of dx and dy
        int ady = y1 - y0;
        int signdx = 1;           // sign of dx and dy
        int signdy = 1;

        if (adx < 0) {
            adx = -adx;
            signdx = -1;
        }
        if (ady < 0) {
            ady = -ady;
            signdy = -1;
        }

        // start pixel
        xyvalues[i*2] = x;
        xyvalues[i*2+1] = y;
        i++;

        if (adx > ady) {
            // X major axis;
            e1 = ady << 1;
            e2 = e1 - (adx << 1);
            e3 = e2 - e1;
            e = -adx;
            len = adx;
            while (len--) {
                e += e1;
                x += signdx;
                if (e >= 0) {
                    y += signdy;
                    e += e3;
                }
                xyvalues[i*2] = x;
                xyvalues[i*2+1] = y;
                i++;
            }
        } else {
            // Y major axis
            e1 = adx << 1;
            e2 = e1 - (ady << 1);
            e3 = e2 - e1;
            e = -ady;
            len = ady;
            while(len--) {
                e += e1;
                y += signdy;
                if (e >= 0) {
                    x += signdx;
                    e += e3;
                }
                xyvalues[i*2] = x;
                xyvalues[i*2+1] = y;
                i++;
            }
        }
    }

    //  Strip out any blank values, if needed. Do this now to preserve
    //  X-Y correspondence. The replacement value is 0.0, which is as
    //  unlikely to occur as any other value and will still look odd
    //  when a long run of values occur.
    if ( image_->haveBlank() ) {
        double blank = image_->getBlank();
        int inblank = 0;
        int start = 0;
        double fill = 0.0;
        for ( i = 0; i < numValues; i++ ) {
            if ( inblank ) {
                //  In a region of blanks. If this pixel is blank
                //  continue, otherwise it is the end of the region
                //  and need to fill the blank segment with the mean
                //  of the end points.
                if ( ivvalues[i*2+1] != blank ) {
                    inblank = 0;

                    //  Trap start of line was blank.
                    if ( start == -1 ) {
                        fill = ivvalues[i*2+1];
                        start = 0;
                    } else {
                        fill = ( ivvalues[start*2+1] + ivvalues[i*2+1] ) * 0.5;
                    }
                    int j;
                    for ( j = start; j <= i; j++ ) {
                        ivvalues[j*2+1] = fill;
                    }
                }
            } else if ( ivvalues[i*2+1] == blank ) {

                //  Start of blank region.
                inblank = 1;
                start = i - 1;

                //  Trap end of line is only blank.
                if ( i == numValues - 1 ) {
                    ivvalues[i*2+1] = ivvalues[(i-1)*2+1] ;
                }
            }
        }

        //  If still inblank at end of line use start value, unless this
        //  is also blank, in which case use 0.
        if ( inblank ) {
            if ( start == -1 ) {
                start = 0;
            }
            if ( ivvalues[start*2+1] == blank ) {
                fill = 0.0;
            } else {
                fill = ivvalues[start*2+1] == blank;
            }
            for ( i = start; i < numValues; i++ ) {
                ivvalues[i*2+1] = fill;
            }
        }
    }

    //  Convert the index/value and x/y pairs into Blt vectors.
    if ( Blt_GraphElement( interp_, argv[0], argv[1], numValues*2,
                           ivvalues, argv[7], argv[8]) != TCL_OK ) {
        delete[] xyvalues;
        delete[] ivvalues;
        return TCL_ERROR;
    }
    if ( Blt_GraphElement( interp_, argv[0], argv[1], numValues*2,
                           xyvalues, argv[9], argv[10] ) != TCL_OK ) {
        delete[] xyvalues;
        delete[] ivvalues;
        return TCL_ERROR;
    }
    delete[] xyvalues;
    delete[] ivvalues;

    return set_result(numValues);
}

//
//   Override the Skycat::get_compass member. This just allows the
//   pixel coordinate system to be used when scaling and orienting the
//   plot symbols, regardless of whether the image has a WCS system or
//   not. This is much faster than going via sky coordinates and is OK
//   when catalogues of positions are created from a displayed image
//   (i.e. the SExtractor toolbox), rather than from a general
//   catalogue.
//
int StarRtdImage::get_compass( double x, double y, const char* xy_units,
                               double radius, const char* radius_units,
                               double ratio, double angle,
                               double& cx, double& cy, double& nx, double& ny,
                               double& ex, double& ey )
{
    double rx = radius, ry = radius;
    cx = x;
    cy = y;

    if ( isCelestial() && plot_wcs() ) {

        //  Get center and radius in deg 2000.
        if (convertCoords(0, cx, cy, *xy_units, 'd') != TCL_OK
            || convertCoords(1, rx, ry, *radius_units, 'd') != TCL_OK) {
            return TCL_ERROR;
        }

        //  Adjust the radius by the ratio.
        if (ratio < 1.)
            ry *= 1.0/ratio;
        else if (ratio > 1.)
            rx *= ratio;

        //  (cx,cy) is center, (nx,ny) is north, (ex,ey) is east, in world
        //  coords deg.
        ex = fmod(cx+fabs(rx)/cos((cy/180.)*pi_),360.);
        ey = cy;
        if (ex < 0.)
            ex += 360;

        nx = cx;
        ny = cy + fabs(ry);
        if (ny >= 90.)
            ny = 180.-ny;
        else if (ny <= -90.)
            ny = -180.-ny;

        //  Convert to canvas coords.
        if (convertCoords(0, nx, ny, 'd', 'c') != TCL_OK
            || convertCoords(0, ex, ey, 'd', 'c') != TCL_OK
            || convertCoords(0, cx, cy, 'd', 'c') != TCL_OK) {
            return TCL_ERROR;
        }
    }
    else {
        //  Not using world coords, go straight to canvas coords.
        if (convertCoords(0, cx, cy, *xy_units, 'c') != TCL_OK
            || convertCoords(1, rx, ry, *radius_units, 'c') != TCL_OK) {
            return TCL_ERROR;
        }

        //  Adjust the radius by the ratio.
        if (ratio < 1.)
            ry *= 1.0/ratio;
        else if (ratio > 1.)
            rx *= ratio;

        ex = cx-rx;
        ey = cy;
        nx = cx;
        ny = cy-ry;
    }

    //  Rotate by angle.
    if (angle) {
        rotate_point(nx, ny, cx, cy, angle);
        rotate_point(ex, ey, cx, cy, angle);
    }

    return TCL_OK;
}

//+
//   StarRtdImage::contourCmd
//
//   Purpose:
//      Contour an image
//
//    Return:
//       TCL status and result (the number of points drawn).
//       Plots contours on canvas.
//
//    Notes:
//       The first parameter passed to this routine should be a list
//       of contour levels, i.e. {1.0 2.0 3.0 4.0 5.0}.
//
//       The second parameter is some reference to an image that is
//       displayed elsewhere, and which will actually be contoured.
//       If not given (shown by its absence or by being set to "")
//       then the image associated with this object will be contoured.
//
//       The third parameter is a boolean indicating whether to use
//       the careful drawing scheme (using geodesics, essential for
//       complex astrometries) or not.
//
//       The fourth parameter is a boolean indicating whether to use
//       smooth polylines (note this is completementary to parameter
//       three) or not.
//
//       The fifth parameter to this routine is a list that contains
//       strings of the curve attributes (see the AST documentation) in
//       a pre-formatted manner (such as can be passed directly to the
//       astSet routine). This allows maximum flexibility in the
//       options that can be set, but imposes an obligation on the
//       user of this member function to format and control the
//       attributes correctly. If the number of elements of this list
//       are less than the number of contour levels then the first
//       element is used for all remaining contours. If omitted then
//       the default preferences are used. A typical response to this
//       parameter might be:
//
//         { {colour(curve)=2,width(curve)=0.015}
//           {colour(curve)=3,width(curve)=0.012}
//           {colour(curve)=4,width(curve)=0.010}
//           {colour(curve)=5,width(curve)=0.008}
//           {colour(curve)=6,width(curve)=0.005} }
//
//       For 5 contours. Note that the style attribute is currently
//       ignored by the Tk canvas AST interface.
//
//       The sixth parameter passed to this member should be a list of
//       the bounds, in canvas coordinates, of the region to be
//       drawn. If NULL then the whole canvas is used.
//
//       The seventh command is a boolean, of true a report is made about the
//       coordinate system alignment, if used.
//-
int StarRtdImage::contourCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::contourCmd" << std::endl;
#endif

    int inerror = 0;
    if ( !image_ ) {
        return error( "no image loaded" );
    }

    // Do the initial split of the input lists.
    char **levelsArgv;
    int nlevels = 0;
    double *levels;
    if ( argc > 7 || argc == 0 ) {
        error( "wrong # args: contour levels [rtdimage] [careful] [smooth] [options_list] [canvas_area]" );
        inerror = 1;
    } else {
        if ( Tcl_SplitList( interp_, argv[0], &nlevels, &levelsArgv ) != TCL_OK ) {
            error( "sorry: failed to decode the contour levels (check format)" );
            inerror = 1;
        }

        //  Convert these into doubles.
        levels = new double[nlevels];
        for ( int index = 0; index < nlevels; index++ ) {
            if ( Tcl_GetDouble( interp_, levelsArgv[index],
                                &levels[index] ) != TCL_OK ) {
                error( levelsArgv[index], "is not a valid number");
                inerror = 1;
                break;
            }
        }
    }

    //  Get the rtdimage that contains the image we want to contour
    //  really.
    StarRtdImage *rtdimage = (StarRtdImage *) NULL;
    if ( argc >= 2 ) {
        if ( *argv[1] != '\0' ) {
            rtdimage = (StarRtdImage *) getView( argv[1] );
            if ( rtdimage == (StarRtdImage *) NULL ) {
                more_error( "; cannot find the contour image -- is it still displayed?" );
                inerror = 1;
            }
        }
    }

    //  Get whether plotting is careful, or fast.
    int careful = 1;
    if ( argc >= 3 ) {
        if ( Tcl_GetBoolean(interp_, argv[2], &careful)  != TCL_OK ) {
            error( "sorry: failed to decode careful preference" );
            inerror = 1;
        }
    }

    //  Get whether using smooth polylines (must be fast).
    int smooth = 0;
    if ( argc >= 4 ) {
        if ( Tcl_GetBoolean(interp_, argv[3], &smooth)  != TCL_OK ) {
            error( "sorry: failed to decode smooth preference" );
            inerror = 1;
        }
    }

    //  Get the preferences, if given
    char **prefs;
    int nprefs = 0;
    if ( argc >= 5 ) {
        if ( Tcl_SplitList( interp_, argv[4], &nprefs, &prefs ) != TCL_OK ) {
            error( "sorry: failed to decode line attributes" );
            nprefs = 0;
            inerror = 1;
        }
    }

    //  Get the region coordinates. Note just use Contour native
    //  facilities for this, not the plot (this is potentially much
    //  more efficient). These may be given as "" in which case they are
    //  ignored.
    char **coordArgv;
    int ncoords = 0;
    double region[4];
    if ( argc >= 6 ) {
        if ( *argv[5] != '\0' ) {
            if ( Tcl_SplitList( interp_, argv[5], &ncoords, &coordArgv ) != TCL_OK ) {
                error( "sorry: failed to decode region of image to contour" );
                ncoords = 0;
                inerror = 1;
            } else {
                if ( ncoords != 4 ) {
                    error( "wrong # of args, should be 4 canvas coordinates"
                           " for contouring region " );
                    inerror = 1;
                } else {
                    for ( int index = 0; index < ncoords; index++ ) {
                        if ( Tcl_GetDouble(interp_, coordArgv[index],
                                           &region[index] ) != TCL_OK ) {
                            error( coordArgv[index], "is not a valid number");
                            inerror = 1;
                            break;
                        }
                    }

                    //  Transform these positions into image coordinates.
                    canvasToImageCoords( region[0], region[1], 0 );
                    canvasToImageCoords( region[2], region[3], 0 );

                    //  Correct for any flips etc. by re-picking the bounds.
                    double temp[4];
                    temp[0] = region[0];
                    temp[1] = region[1];
                    temp[2] = region[2];
                    temp[3] = region[3];
                    region[0] = min( temp[0], temp[2] );
                    region[2] = max( temp[0], temp[2] );
                    region[1] = min( temp[1], temp[3] );
                    region[3] = max( temp[1], temp[3] );
                }
            }
        }
    }

    //  Get whether to report coordinate matching.
    int report = 0;
    if ( argc == 7 ) {
        if ( Tcl_GetBoolean( interp_, argv[6], &report )  != TCL_OK ) {
            error( "sorry: failed to decode reporting value" );
            inerror = 1;
        }
    }

    //  Now see if a copy of the current WCS frameset is available (we
    //  use a copy so we can  modify without changing any other elements).
    StarWCS* wcsp = getStarWCSPtr();
    if ( ! wcsp ) {
        return TCL_ERROR;
    }
    AstFrameSet *wcs = wcsp->astWCSCopy();
    if ( wcs == (AstFrameSet *) NULL ) {

        //  If no WCS is available then we need to create a suitable AST
        //  frameset. This just maps GRID coordinates to themselves, or to
        //  pixel coordinates.
        wcs = makeGridWCS();
    }

    //  See if another image is to be used for contouring. If so then
    //  this is related to the image displayed by some AST based
    //  transformation (which may be pixel coordinates or a sky-based
    //  system). This is also the image we want to contour, so get its
    //  ImageIO object.
    ImageIO imageIO = image_->image();
    AstFrameSet *farwcs = NULL;
    if ( rtdimage != (StarRtdImage *) NULL ) {

        //  OK, attempt to locate a WCS for this image.
        ImageData *farimagedata = rtdimage->image();
        imageIO = farimagedata->image();
        StarWCS* wcsp = getStarWCSPtr( farimagedata );
        if ( ! wcsp ) {
            return TCL_ERROR;
        }
        farwcs = wcsp->astWCSCopy();
        if ( farwcs == (AstFrameSet *) NULL ) {

            //  If no WCS is available then we need to create a suitable AST
            //  frameset. This just maps GRID coordinates to themselves, or to
            //  pixel coordinates.
            farwcs = rtdimage->makeGridWCS();
        }
    }
    if ( ! inerror && wcs != (AstFrameSet *) NULL ) {

        //  Current frame is the GRID coordinates of the image to be
        //  contoured. Get domain as this is the route we'd like to connect
        //  through.
        const char *domain = astGetC( wcs, "Domain" );
        astSetI( wcs, "Current", AST__BASE );

        //  Create an AstPlot that incorporates an additional FrameSet
        //  that describes an system we want to add.
        AstPlot *plot = createPlot( wcs, farwcs, domain, 1, ncoords != 0,
                                    region, report );
        inerror = ( inerror || plot == (AstPlot *) NULL );

        //  Initialise the interpreter and canvas name for the Tk plotting
        //  routines.
        astTk_Init( interp_, canvasName_ );

        //  We want to draw polylines, not line segments. Polylines may be
        //  smooth.
        astTk_LineType( 0, smooth );

        //  Define a tag for all items created in the plot.
        astTk_Tag( ast_tag() );

        if ( astOK && !inerror ) {

            //  Create a contour object, setting the contour levels, and
            //  line attributes.
            Contour contour( imageIO, plot, levels, nlevels,
                             (const char **) prefs, nprefs );

            //  Establish if plotting is careful or fast.
            contour.setCareful( careful );

            //  Tell the contour object if it needs to byte swap the image
            //  data. This is only necessary if the image being
            //  contoured is FITS on a non bigendian machine.
            contour.setSwap( swapNeeded( imageIO ) );

            //  Tell contour object if this is FITS. Need to handle NaN
            //  values.
            contour.setIsFITS( isfits() );

            //  Set the region of image to contour (tuned to match grid plots).
            if ( ncoords > 0 ) {
                contour.setRegion( (int)(region[0] + 1), (int)(region[1] + 1),
                                   (int)(region[2] - region[0] + 1 ),
                                   (int)(region[3] - region[1] + 1 ) );
            }

            //  Draw the contour. Only result in drawn or not.
            int ndrawn = contour.drawContours();
            set_result( ndrawn );
        }

        //  Free the plot.
        plot = (AstPlot *) astAnnul( plot );

        //  Reset the tag associated with AST grid items.
        astTk_Tag( NULL );

        //  Switch line type back to default.
        astTk_LineType( 1, 0 );
    }

    //  Free the lists.
    if ( ncoords > 0 ) {
        Tcl_Free( (char *) coordArgv );
    }
    if ( nprefs > 0 ) {
        Tcl_Free( (char *) prefs );
    }
    if ( nlevels > 0 ) {
        Tcl_Free( (char *) levelsArgv );
    }

    //  Free the WCS copy.
    wcs = (AstFrameSet *) astAnnul( wcs );

    //  Free the contours.
    if ( nlevels > 0 ) {
        delete [] levels;
    }

    //  Tidy up.
    if ( inerror || ! astOK ) {
        if ( !astOK ) {
            astClearStatus;
        }
        return TCL_ERROR;
    }
    return TCL_OK;
}

//+
//   StarRtdImage::stcplotCmd
//
//   Purpose:
//      Draw an STC region over the displayed image.
//
//    Return:
//      TCL status
//
//    Notes:
//      The first parameter passed to this command should be an STC-S
//      description and the second any attributes for the plot (line
//      colours etc. as for an AstPlot). STC-S is documented at:
//
//           http://www.ivoa.net/Documents/Notes/STC-S
//
//
int StarRtdImage::stcplotCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::stcCmd" << std::endl;
#endif

    if ( !image_ ) {
        return error( "no image loaded" );
    }
    const char *errmsg = NULL;


    //  Read the STC-S description and create the AST region.
    AstStcsChan *chan = astStcsChan( ChannelSource, NULL, " " );
    channelData_.read = 0;
    channelData_.content = argv[0];
    astPutChannelData( chan, &channelData_ );
    AstRegion *region = (AstRegion *) astRead( chan );

    //  Create the mapping from region coordinates to the image WCS.
    if ( region != NULL ) {
        StarWCS* wcsp = getStarWCSPtr();
        if ( wcsp != NULL ) {
            AstFrameSet *wcs = wcsp->astWCSCopy();

            //  Get alignment between the coordinate systems.
            int base = astGetI( wcs, "Base" );
            AstFrameSet *fs = (AstFrameSet *) astConvert( region, wcs, " " );
            astSetI( wcs, "Base", base );

            //  Get Region in coordinates of the image.
            AstRegion *wcsreg = (AstRegion *) astMapRegion( region, fs, fs );

            //  Create an AstPlot based on the full image WCS.
            AstPlot *plot = createPlot( wcs, NULL, NULL, 1, 0, NULL, 1 );

            //  Set any attributes.
            astSet( plot, argv[1], " " );

            //  Initialise the interpreter and canvas name for the Tk plotting
            //  routines.
            astTk_Init( interp_, canvasName_ );

            //  Define a tag for all items created in the plot.
            astTk_Tag( ast_tag() );

            //  Add the STC-S region to the plot.
            astAddFrame( plot, AST__CURRENT, astUnitMap( 2, " " ), wcsreg );

            // Now draw the border round the STC-S Region (outside coordinates
            // are BAD so this defines the border).
            astBorder( plot );

            //  Free the plot etc,
            plot = (AstPlot *) astAnnul( plot );
            wcsreg = (AstRegion *) astAnnul( wcsreg );
            wcs = (AstFrameSet *) astAnnul( wcs );
            fs = (AstFrameSet *) astAnnul( fs );

            //  Reset the tag associated with AST grid items.
            astTk_Tag( NULL );
        }
        else {
            //  No WCS available for image, STC-S requires this.
            errmsg = "no WCS available";
        }
    }
    else {
        if ( ! astOK ) {
            //  Get informative error message.
            int status_check;
            errTcl_LastError( &status_check, &errmsg );
        }
        else {
            errmsg = "not a valid STC-S region";
        }
    }

    //  Tidy up.
    if ( region != NULL ) {
        region = (AstRegion *) astAnnul( region );
    }
    if ( chan != NULL ) {
        chan = (AstStcsChan *) astAnnul( chan );
    }
    if ( !astOK || errmsg != NULL ) {
        if ( ! astOK ) {
            astClearStatus;
        }
        if ( errmsg != NULL ) {
            return error( errmsg );
        }
        return TCL_ERROR;
    }
    return TCL_OK;
}

//+
//   StarRtdImage::makeGridWCS
//
//   Purpose:
//       Create a pseudo WCS that describes the GRID coordinates of
//       an image.
//
//    Return:
//       Basic AstFrameSet.
//
//    Notes:
//       If the image is derived from an NDF then a PIXELl coordinates
//       Frame will be added.
//
//-
AstFrameSet* StarRtdImage::makeGridWCS( ImageData *image )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::makeGridWCS" << std::endl;
#endif

    //  If no image is given then use the default.
    if ( ! image ) {
        image = image_;
    }

    //  Create the GRID domain Frame.
    AstFrame *grid = astFrame( 2, "Domain=GRID,Title=Grid Coordinates" );

    //  Create the FrameSet, adding the GRID domain.
    AstFrameSet *set = astFrameSet( grid, " " );
    grid = (AstFrame *) astAnnul( grid );

    //  Need to add a pixel coordinates based Frame.
    AstFrame *coordfrm  = astFrame( 2,
                                    "Domain=PIXEL,Title=Pixel Coordinates" );
    double ina[2], inb[2], outa[2], outb[2];
    double width = image_->width();
    double height = image_->height();

    //  Define limits of window in pixel indices.
    ina[0] = ina[1] = 1.0;
    inb[0] = width + 1.0;
    inb[1] = height + 1.0;

    //  Get the NDF origin information and set up the limits of the
    //  same window as above in pixel coordinates.
    char *xori = image_->image().get("LBOUND1");
    if ( xori ) {
        outa[0] = atof( xori );
    } else {
        outa[0] = 1;
    }
    outa[0] -= 0.5;
    outb[0] = outa[0] + width;
    char *yori = image_->image().get("LBOUND2");
    if ( yori ) {
        outa[1] = atof( yori );
    } else {
        outa[1] = 1;
    }
    outa[1] -= 0.5;
    outb[1] = outa[1] + height;

    //  Create the pixel indices to pixel coordinates mapping.
    AstMapping *pixmap = (AstMapping *) astWinMap( 2, ina, inb, outa, outb, " " );

    //  Add all these to the FrameSet. Assumes that the current
    //  base frame is the pixel index frame.
    astAddFrame( set, AST__BASE, pixmap, coordfrm );
    coordfrm = (AstFrame *) astAnnul( coordfrm );
    pixmap = (AstMapping *) astAnnul( pixmap );

    return set;
}

//+
//   StarRtdImage::createPlot
//
//   Purpose:
//       Create an AST plot.
//
//   Description:
//       This member creates an AST plot that reflects the current
//       image orientation, is either the whole or a displayed
//       part and that adds in an additional FrameSet (or Frame) to
//       the plot.
//
//       The part of the plot that is valid is determined by the
//       "region" argument. If full is false, then this should be an
//       array of 4 canvas positions that define the actual area to be
//       drawn, rather than the full canvas, otherwise this parameter
//       will not be used.
//
//       If full is true then the region array can be used to
//       transform image coordinates from the original WCS system into
//       the new coordinates (this supplies a region of coordinates in
//       the new image that correspond to a region in the old image,
//       useful for clipping).
//
//       If given domain is a domain that should be used to route the
//       connection between the framesets. If that fails then a default list
//       of expected domains is tried.
//
//    Return:
//       An AstPlot, or NULL if failed.
//
//
//-
AstPlot* StarRtdImage::createPlot( AstFrameSet *wcs,
                                   AstFrameSet *extraset,
                                   const char *domain,
                                   int full, int image,
                                   double region[],
                                   int report )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::createPlot" << std::endl;
#endif
    //  Define the limits of the canvas plotting "area" in canvas
    //  coordinates and current coordinates (the same positions). Use
    //  the image position on the canvas for this. Note that this also
    //  reorients the image to the canvas (so any scales, flips,
    //  offset and interchange are accounted).
    float gbox[4];
    double pbox[4];
    if ( full ) {

        //  Using whole of canvas/image.
        double rw = reqWidth_, rh = reqHeight_;
        if ( rw == 0 ) rw = (double) image_->width();  // Zero when whole
        if ( rh == 0 ) rh = (double) image_->height(); // image is displayed.
        doTrans( rw, rh, 1 );
        gbox[0] = pbox[0] = 0.0;
        gbox[1] = pbox[1] = rh;
        gbox[2] = pbox[2] = rw;
        gbox[3] = pbox[3] = 0.0;
    }
    else {
        //  Just using part of the canvas.
        gbox[0] = pbox[0] = region[0];
        gbox[1] = pbox[1] = region[1];
        gbox[2] = pbox[2] = region[2];
        gbox[3] = pbox[3] = region[3];
    }
    int rotated = image_->rotate();             // Record if rotated
    if ( rotated ) {                            // and switch off while
        image_->rotate(0);                      // getting graphics mapping
    }
    canvasToImageCoords( pbox[0], pbox[1], 0 );
    canvasToImageCoords( pbox[2], pbox[3], 0 );
    if ( rotated ) {
        image_->rotate(1);                        // Restore rotation.
    }

    //  If an extra FrameSet has been given then try to get a conversion
    //  which goes from its CURRENT frame to the BASE frame of the WCS
    //  system. For this reason we need to invert the new FrameSet.
    AstFrameSet *plotset = NULL;
    if ( extraset != (AstFrameSet *) NULL ) {
        astInvert( extraset );

        //  Test various ways of converting. Prefer the domain of the current
        //  system if given.
        int presetused = 0;
        if ( domain != NULL ) {
            plotset = (AstFrameSet *) astConvert( wcs, extraset, domain );

            //  If this domain isn't the one we requested try with the default
            //  list.
            if ( plotset != (AstFrameSet *) NULL && astOK ) {
                AstFrame *base = (AstFrame *) astGetFrame( wcs, AST__BASE );
                if ( strcasecmp( astGetC( base , "Domain" ), domain ) != 0 ) {
                    plotset = (AstFrameSet *) astAnnul( plotset );
                }
                (void) astAnnul( base );
            }
        }
        if ( ! astOK || plotset == (AstFrameSet *) NULL ) {
            //  Attempt a list of preset domains expected in images.
            plotset = (AstFrameSet *) astConvert( wcs, extraset,
                                                  "SKY,AXIS,PIXEL,GRID,," );
            presetused = 1;
        }

        astInvert( extraset );
        if ( ! astOK || plotset == (AstFrameSet *) NULL ) {
            more_error( "sorry: cannot find a way to convert your existing "
                        "coordinates to the requested system");
            if ( plotset != (AstFrameSet *) NULL ) {
                plotset = (AstFrameSet *) astAnnul( plotset );
            }
            return (AstPlot *) NULL;
        }
        if ( report ) {
            AstFrame *base = (AstFrame *) astGetFrame( wcs, AST__BASE );
            cerr << "INFO: coordinates matched using " << astGetC( base, "Domain" );
            if ( presetused ) {
                cerr << " (tried: ";
                if ( domain != NULL && strcmp( domain, "SKY" ) != 0 ) {
                    cerr << domain << ",";
                }
                cerr << "SKY,AXIS,PIXEL,GRID,any";
            }
            cerr << endl;
        }

        //  Transform region from old image to new image if asked.
        if ( image ) {
            //  Transform the centre and the sides of the region. Note
            //  that this transformation performs slightly better than
            //  just using the bounding box of the region as the size
            //  is less dependent on the rotation.
            double xin[4], yin[4];
            double xout[4], yout[4];
            xin[0] = region[0];
            yin[0] = region[1];

            xin[1] = region[2];
            yin[1] = region[1];

            xin[2] = region[2];
            yin[2] = region[3];

            xin[3] = region[0] + ( region[2] - region[0] ) * 0.5;
            yin[3] = region[1] + ( region[3] - region[1] ) * 0.5;

            astTran2( plotset, 4, xin, yin, 1, xout, yout );

            double dx = xout[1] - xout[0];
            double dy = yout[1] - yout[0];
            double xdist = sqrt( dx * dx + dy * dy );

            dx = xout[1] - xout[2];
            dy = yout[1] - yout[2];
            double ydist = sqrt( dx * dx + dy * dy );

            region[0] = xout[3] - xdist * 0.5;
            region[1] = yout[3] - ydist * 0.5;
            region[2] = xout[3] + xdist * 0.5;
            region[3] = yout[3] + ydist * 0.5;
        }
    }
    else {

        //  Just using plain WCS.
        plotset = wcs;
    }

    //  Create the plot frameset.
    AstPlot *plot = astPlot( plotset, gbox, pbox, " " );
    if ( extraset != (AstFrameSet *) NULL ) {
        plotset = (AstFrameSet *) astAnnul( plotset );
    }

    //  If the X and Y axes are interchanged then we need to
    //  correct the mapping between the pixel frame (image
    //  coordinates) and the graphics frame.
    if ( image_->rotate() ) {
        int inperm[] = {2,1};
        int outperm[] = {2,1};
        AstPermMap *perm = astPermMap( 2, inperm, 2, outperm,
                                       (double *) NULL, " " );
        astRemapFrame( plot, AST__BASE, perm );
        perm = (AstPermMap *) astAnnul( perm );
    }
    if ( astOK ) {
        return plot;
    }
    else {
        return (AstPlot *) NULL;
    }
}

//+
//   StarRtdImage::hduCmd
//
//   Purpose:
//      Overrides the "hdu" command of Skycat to add in NDF support.
//      See the Skycat documentation for what this does.
//
//-
int StarRtdImage::hduCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::hduCmd" << std::endl;
#endif
    if ( ! image_ ) {
        return TCL_OK;
    }

    //  Check that we have a known data representation.
    ImageIO imio = image_->image();
    if ( ! imio.rep() ) {
        return error( "unknown data representation, "
                      "\"hdu\" command only available for FITS and NDF" );
    }

    if ( isfits() ) {
        return fitsHduCmd( imio, argc, argv );
    }
    return ndfHduCmd( imio, argc, argv );
}

//+
//   StarRtdImage::fitsHduCmd
//
//   Implement the hdu command for FITS files.
//
//-
int StarRtdImage::fitsHduCmd( const ImageIO &imio, int argc, char *argv[] )
{
    //  Cannot pass FITS files directly to Skycat as it only works with
    //  FitsIO objects, not StarFitsIO, so repeat Skycat functionality.
    FitsIO *fits = (FitsIO *) imio.rep();

    // <path> hdu  (return the current HDU number)
    if ( argc == 0 ) {
        return set_result( fits->getHDUNum() );
    }

    // <path> hdu count
    if ( strcmp(argv[0], "count" ) == 0 ) {
        return set_result( fits->getNumHDUs() );
    }

    // <path> hdu type ?number?
    if ( strcmp(argv[0], "type") == 0 ) {
        return hduCmdType( argc, argv, fits );
    }

    // <path> hdu listheadings
    // (return a list of table headings matching the "hdu list" output)
    if ( strcmp(argv[0], "listheadings") == 0 ) {
        return set_result( "HDU Type ExtName NAXIS NAXIS1 NAXIS2 NAXIS3"
                           " CRPIX1 CRPIX2" );
    }

    // <path> hdu headings ?$number?
    if ( strcmp(argv[0], "headings") == 0 ) {
        return hduCmdHeadings( argc, argv, fits );
    }

    // <path> hdu get ?number? ?$filename? ?$entry?"
    if ( strcmp(argv[0], "get") == 0 ) {
        return hduCmdGet( argc, argv, fits );
    }

    // <path> hdu create $type $extname $headings $tform $data
    if (strcmp(argv[0], "create") == 0) {
        return hduCmdCreate( argc, argv, fits );
    }

    // <path> hdu delete $number
    if ( strcmp(argv[0], "delete" ) == 0 ) {
        return hduCmdDelete( argc, argv, fits );
    }

    // <path> hdu fits ?$number?
    if (strcmp(argv[0], "fits") == 0)
        return hduCmdFits(argc, argv, fits);

    // <path> hdu list, extended for compressed images
    if ( strcmp(argv[0], "list" ) == 0 ) {
        int ret = hduCmdCompList( argc, argv, fits );
        return ret;
    }

    // <path> hdu set $number
    if (strcmp( argv[0], "set" ) == 0 ) {
        return hduCmdSet( argc, argv, fits );
    }

    // <path> hdu display, as one image.
    if (strcmp( argv[0], "display" ) == 0 ) {
        return hduCmdDisplay( argc, argv, fits );
    }

    // <path> hdu $number (Set the current HDU)
    return hduCmdSet( argc, argv, fits );
}

//+
//  StarRtdImage::hduCmdCompList
//
//  Purpose:
//     Implements the "hdu list" command. Note that if an extension contains
//     a compressed image the string "COMPRESSED_IMAGE" will be part of
//     the extname.
//-
int StarRtdImage::hduCmdCompList( int argc, char** argv, FitsIO* fits )
{
    //  Return a list of HDUs.
    int numHDUs = fits->getNumHDUs();
    if ( numHDUs <= 0 ) {
        return TCL_OK;  // empty return list
    }

    //  Save current HDU, then loop through all HDUs to get info.
    int curHDU = fits->getHDUNum();
    ostringstream os;
    int status = 0;
    int count = 0;
    for ( int i = 1; i <= numHDUs; i++ ) {
        if ( fits->setHDU( i ) != 0 ) {
            status++;
            break;
        }
        const char* type = fits->getHDUType();
        if ( !type ) {
            status++;
            break;
        }

        //  Get these keyword values and default to "".
        char extName[80], naxis[32], naxis1[32], naxis2[32], naxis3[32];
        char crpix1[32], crpix2[32];
        fits->get( "EXTNAME", extName, sizeof(extName) );
        fits->get( "NAXIS", naxis, sizeof(naxis) );
        fits->get( "NAXIS1", naxis1, sizeof(naxis1) );
        fits->get( "NAXIS2", naxis2, sizeof(naxis2) );
        fits->get( "NAXIS3", naxis3, sizeof(naxis3) );
        fits->get( "CRPIX1", crpix1, sizeof(crpix1) );
        fits->get( "CRPIX2", crpix2, sizeof(crpix2) );

        //  Is this compressed? ZIMAGE will be T or extname already contains
        //  the COMPRESSED_IMAGE string.
        if ( strstr( extName, "COMPRESSED_IMAGE" ) == NULL ) {
            int zimage = 0;
            fits->get( "ZIMAGE", zimage );
            if ( zimage ) {
                strcat( extName, "(COMPRESSED_IMAGE)" );
            }
        }

        // Try avoiding long fractional strings
        if (strlen(crpix1) != 0 &&  strlen(crpix2) != 0) {
            double dcrpix1, dcrpix2;
            fits->get("CRPIX1", dcrpix1);
            fits->get("CRPIX2", dcrpix2);
            os << "{"
               << i
               << " " << type
               << " {" << extName << "}"
               << " {" << naxis << "}"
               << " {" << naxis1 << "}"
               << " {" << naxis2 << "}"
               << " {" << naxis3 << "}"
               << " {" << dcrpix1 << "}"
               << " {" << dcrpix2 << "}"
               << "} ";
        }
        else {
            os << "{"
               << i
               << " " << type
               << " {" << extName << "}"
               << " {" << naxis << "}"
               << " {" << naxis1 << "}"
               << " {" << naxis2 << "}"
               << " {" << naxis3 << "}"
               << " {" << crpix1 << "}"
               << " {" << crpix2 << "}"
               << "} ";
        }
        count++;
    }
    if (count) {
        if (status == TCL_OK) {
            set_result(os.str().c_str());
        }
        fits->setHDU(curHDU);
    }
    return status;
}

//+
//   StarRtdImage::ndfHduCmd
//
//   Implement the hdu command for NDF container files.
//
//   Notes:
//      For NDFIO data, we will map hdu commands to work with a single
//      HDS container file. This may be a single NDF with image
//      components (variance and quality), or a series of NDFs. Only
//      NDFs at the level of the HDS path name given are used (no over
//      the top searching of container file is used).
//-
int StarRtdImage::ndfHduCmd( const ImageIO &imio, int argc, char *argv[] )
{
    NDFIO *ndf = (NDFIO *) imio.rep();

    // <path> hdu  (return the current HDU number)
    if ( argc == 0 ) {
        return set_result( ndf->getNDFNum() );
    }

    // <path> hdu count
    if ( strcmp(argv[0], "count" ) == 0 ) {
        return set_result( ndf->getNumNDFs() );
    }

    // <path> hdu type ?number? Only type available are NDFs.
    if ( strcmp(argv[0], "type") == 0 ) {
        return set_result( "NDF" );
    }

    // <path> hdu listheadings
    // (return a list of table headings matching the "hdu list" output)
    if ( strcmp(argv[0], "listheadings") == 0 ) {
        return set_result( "number name naxis1 naxis2 hasvar hasqual" );
    }

    // <path> hdu headings ?$number?  Table command not implemented
    if ( strcmp(argv[0], "headings") == 0 ) {
        return error( "table commands not supported for NDFs" );
    }

    // <path> hdu get ?number? ?$filename? ?$entry?"
    if ( strcmp(argv[0], "get") == 0 ) {
        return error( "table commands not supported for NDFs" );
    }

    // <path> hdu create $type $extname $headings $tform $data
    if (strcmp(argv[0], "create") == 0) {
        return error( "table commands not supported for NDFs" );
    }

    // <path> hdu delete $number
    if ( strcmp(argv[0], "delete" ) == 0 ) {
        return error( "table commands not supported for NDFs" );
    }

    // <path> hdu fits ?$number?
    if (strcmp(argv[0], "fits") == 0) {
        return ndfCmdFits( argc, argv, ndf );
    }

    // <path> hdu list
    if ( strcmp(argv[0], "list" ) == 0 ) {
        return ndfCmdList( argc, argv, ndf );
    }

    // <path> hdu set $number
    if (strcmp( argv[0], "set" ) == 0 ) {
        return ndfCmdSet( argc, argv, ndf );
    }

    // <path> hdu display, as one image.
    if (strcmp( argv[0], "display" ) == 0 ) {
        return ndfCmdDisplay( argc, argv, ndf );
    }

    // <path> hdu $number (Set the current HDU)
    return ndfCmdSet( argc, argv, ndf );
}

//+
//  StarRtdImage::ndfCmdSet
//
//  Purpose:
//     Implements the NDF equivalent of the "hdu set" command.
//     This displays the required NDF and its component from the list
//     of NDFs available in the current container file.
//-
int StarRtdImage::ndfCmdSet( int argc, char** argv, NDFIO* ndf )
{
    //  First argument is an optional "set".
    if (strcmp(argv[0], "set") == 0) {
        argc--;
        argv++;
    }

    //  Expect a number identifying the NDF and the data component.
    if (argc != 2) {
        return error(
            "wrong number of args: expected NDF_number NDF_component" );
    }

    //  Get a (reference counted) copy of the image
    ImageIO imio = image_->image();

    //  Extract the arguments and set the NDFIO to this NDF/component.
    int num = 0;
    if ( Tcl_GetInt( interp_, argv[0], &num ) != TCL_OK ) {
        return TCL_ERROR;
    }
    if ( ! ndf->setDisplayable( num, argv[1] ) ) {
        return TCL_ERROR;
    }

    //  Save image transformation parameters to restore later.
    ImageDataParams p;
    image_->saveParams(p);

    //  Delete old image.
    delete image_;
    image_ = NULL;
    updateViews();

    //  Re-initialize the image from the given NDF/component
    ImageData* im = makeImage( imio );
    if ( ! im ) {
        return TCL_ERROR;
    }
    image_ = im;

    //  The WCS info will be different in this NDF.
    imio.wcsinit();

    //  Restore transformations.
    // image_->restoreParams( p, !autoSetCutLevels_ );
    image_->restoreParams( p );

    //  Update the display.
    return initNewImage();
}

//+
//  StarRtdImage::ndfCmdFits
//
//  Purpose:
//     Implements the NDF equivalent of the "hdu fits" command.
//     This returns the FITS headers.
//-
int StarRtdImage::ndfCmdFits( int argc, char** argv, NDFIO* ndf )
{
    int hdu = ndf->getNDFNum();
    int saved_hdu = hdu;
    int numNDFs = ndf->getNumNDFs();
    const char *component = ndf->component();
    int status = TCL_OK;

    // Check for the optional hdu arg, otherwise use current
    if ( argc >= 2 && sscanf( argv[1], "%d", &hdu ) == 1 ) {
        if ( hdu != saved_hdu ) {
            if ( hdu < 1 || hdu > numNDFs ) {
                return fmt_error( "HDU number %d out of range (max %d)",
                                  hdu, numNDFs );
            }
            else {
                // Switch to the given HDU, but restore the original before
                // returning, so use status to fall through...
                if ( ! ndf->setDisplayable( hdu, "data" ) ) {
                    set_result( "Failed to set displayable" );
                    status = TCL_ERROR;
                }
            }
        }
    }

    // Get the FITS header and return it as the result.
    if ( status == TCL_OK ) {
        ostringstream os;
        ndf->getFitsHeader( os );
        set_result( os.str().c_str() );
    }

    // Restore the original HDU before returning.
    if ( hdu != saved_hdu &&
         ! ndf->setDisplayable( saved_hdu, component ) ) {
        if ( status != TCL_ERROR ) {
            status = TCL_ERROR;
            set_result( "Failed to set displayable" );
        }
    }
    return status;
}

//+
//  StarRtdImage::ndfCmdList
//
//  Purpose:
//     Implement NDF equivalent of the "hdu list" command. This
//     returns a list of NDF names, their dimensions and whether
//     they have variance and quality components.
//
//-
int StarRtdImage::ndfCmdList( int argc, char *argv[], NDFIO *ndf )
{
    int numNDFs = ndf->getNumNDFs();
    if ( numNDFs <= 0 ) {
        return TCL_OK;  // empty return list
    }

    //  Loop though all NDFs getting the required information.
    std::ostringstream os;
    for ( int i = 1; i <= numNDFs; i++ ) {
        char name[MAXNDFNAME], naxis1[32], naxis2[32], hasvar[32], hasqual[32];
        ndf->getNDFInfo( i, name, naxis1, naxis2, hasvar, hasqual );
        os << "{"
           << i
           << " {" << name    << "}"
           << " {" << naxis1  << "}"
           << " {" << naxis2  << "}"
           << " {" << hasvar  << "}"
           << " {" << hasqual << "}"
           << "} ";
    }
    set_result( os.str().c_str() );
    return TCL_OK;
}

//+
//  StarRtdImage::ndfCmdDisplay
//
//  Purpose:
//     Implement NDF equivalent of the "hdu display" command.
//     This displays all the NDFs in a container file as a single
//     image.
//
//-
int StarRtdImage::ndfCmdDisplay( int argc, char *argv[], NDFIO *ndf )
{
    int ndfList[256];
    int numNDFs = 0;

    if ( ! image_ ) {
        return error( "No images to display" );
    }

    if ( argc == 2 ) {
        //  Parse list of indices.
        char** indices = NULL;
        if ( Tcl_SplitList(interp_, argv[0], &numNDFs, &indices ) != TCL_OK ) {
            return TCL_ERROR;
        }

        if ( numNDFs > (int) ( sizeof( ndfList ) / sizeof( int ) ) ) {
            return fmt_error( "StarRtdImage::ndfCmdDisplay: too many "
                              "NDFs: %d (max 256)", numNDFs );
        }
        if ( numNDFs == 0 ) {
            return error( "No NDFs were specified" );
        }

        for ( int i = 0; i < numNDFs; i++ ) {
            if ( Tcl_GetInt( interp_, indices[1], &ndfList[i] ) != TCL_OK ) {
                Tcl_Free( *indices );
                return TCL_ERROR;
            }
        }
        Tcl_Free( *indices );
    }
    else {
        // Use all images
        numNDFs = ndf->getNumNDFs();
        for ( int i = 0; i < numNDFs; i++ ) {
            ndfList[i] = i + 1;
        }
    }

    // Get a (reference counted) copy of the image
    ImageIO imio = image_->image();

    // Save and restore image transformation parameters
    ImageDataParams p;
    image_->saveParams(p);

    // Delete old image
    delete image_;
    image_ = NULL;
    updateViews();

    // Create an image composed of all of the requested image extensions.
    // Note the copy of imio created here is really a container-file per NDF
    // (see gaiaCloneMNDF). That's probably not optimal, but is reasonably
    // light-weight.
    image_ = ImageData::makeCompoundImage( name(), imio, ndfList, numNDFs,
                                           biasimage_->biasInfo(), verbose() );
    if ( ! image_) {
        return error( "Failed to display NDFs" );
    }

    // Restore transformations
    image_->restoreParams( p, !autoSetCutLevels_ );

    return initNewImage();
}

/*
 * Re-implement the FITS hdu get subcommand:
 *
 *    <path> hdu get ?$number? ?$filename? ?$entry?
 *
 * See skycat for details. This method is overridden so we can trap binary
 * tables that are really compressed images and handle them appropriately.
 */
int StarRtdImage::hduCmdGet( int argc, char** argv, FitsIO* fits )
{
    StarFitsIO *sfits = (StarFitsIO *) fits;
    int hdu = fits->getHDUNum();
    int saved_hdu = hdu;
    int numHDUs = fits->getNumHDUs();
    int status = TCL_OK;

    //  Check for the optional hdu arg, otherwise use current
    if ( argc >= 2 && sscanf( argv[1], "%d", &hdu ) == 1 ) {
        argc--;
        argv++;
        if ( hdu != saved_hdu ) {
            if ( hdu < 1 || hdu > numHDUs ) {
                return fmt_error( "HDU number %d out of range (max %d)", hdu,
                                  numHDUs );
            }

            //  Switch to the given HDU, but restore the original before
            //  returning.
            if ( fits->setHDU( hdu ) != 0 ) {
                return TCL_ERROR;
            }
        }
    }

    //  Check for the filename arg, needed for compressed image support.
    char* filename = NULL;
    if ( argc >= 2 ) {
        filename = argv[1];
    }

    //  Check if this is a compressed image and handle.
    if ( sfits->isCompressedImage() ) {
        if ( argc >= 2 ) {
            //  Create an object name of the filename plus HDU.
            char objname[1024];
            sprintf( objname, "%s[%d]", filename_, hdu );
            status = sfits->saveCompressedImage( filename, objname );
        }
        else {
            status = error( "Saving compressed image requires a filename" );
        }
    }
    else {
        //  Normal table save, check for the entry arg.
        char* entry = NULL;
        if ( argc >= 3 ) {
            entry = argv[2];
        }

        //  Get the info and catch any errors
        status = getHDU( fits, filename, entry );
    }

    //  Restore the original HDU before returning.
    if ( hdu != saved_hdu && fits->setHDU( saved_hdu ) != 0 ) {
        status = TCL_ERROR;
    }
    return status;
}

//+
//   StarRtdImage::colorrampCmd
//
//   Purpose:
//      Extends the standard colorramp command to add a pseudo AST WCS
//      which describes the image in terms of the current intensity range.
//
//      The reason for doing this is so that the image can have a AST grid
//      drawn around it, so that it may be annotated and have axes which
//      make the colour correspondence clear.
//
//      We also reproduce the full colorramp command so that a StarFitsIO
//      instance can be used, this is necessary for the AST facility to work.
//
//      Note that with no arguments the standard "colorramp" command is
//      invoked.  With the arguments "setwcs" the WCS system is added based on
//      the image whose name is given as the second argument.
//-
int StarRtdImage::colorrampCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::colorrampCmd" << std::endl;
#endif
    if ( argc == 0 ) {
        //  Cut and paste of RtdImage::colorrampCmd with StarFitsIO
        //  replacement.

        int w = Tk_Width( tkwin_ );
        int h = Tk_Height( tkwin_ );
        if ( w == 1 && h == 1 ) {
            // Waiting for resize event on image window.
            return TCL_OK;
        }

        Mem data( w * h, 0 ), header;
        if ( data.status() != 0 ) {
            return TCL_ERROR;
        }

        // Create the ramp data.
        double scale = 255.0 / w;
        char* p = (char*) data.ptr();
        for ( int i = 0; i < w; i++ ) {
            p[i] = (int)( i * scale );
        }
        for ( int j = 0; j < h; j++ ) {
            memmove( p+(j*w), p, w );
        }

        //  And the image. Note will associate to the main image as a
        //  viewmaster. See Tcl wrapper code.
        if ( image_ ) {
            delete image_;
        }
        StarFitsIO* fits = new StarFitsIO( w, h, BYTE_IMAGE, 0.0, 1.0,
                                           header, data, NULL );
        if ( fits ) {
            image_ = makeImage( fits );
            image_->name( "Ramp" );
            return initNewImage();
        }
        return ERROR;
    }
    else {

        //  Get the image, should be named as second argument.
        StarRtdImage *rtdimage = (StarRtdImage *) getView( argv[1] );
        if ( rtdimage == (StarRtdImage *) NULL ) {
            return error( "cannot access main image" );
        }
        ImageData *rtdimagedata = rtdimage->image();

        //  Create a frameset that maps from GRID to intensity.
        AstFrame *base = (AstFrame *) astFrame( 2, "Domain=GRID" );
        AstFrameSet *fset = (AstFrameSet *) astFrameSet( base, " " );
        AstMapping *map = (AstMapping *) astUnitMap( 2, " " );
        AstFrame *current = (AstFrame *) astFrame( 2, "Domain=PIXEL" );
        astAddFrame( fset, 1, map, current );

        // Add a mapping that transforms from the base map to the
        // current as intensity along the X axis.
        double low = rtdimagedata->lowCut();
        double high = rtdimagedata->highCut();
        double width = image_->width();

        double ina[2], inb[2], outa[2], outb[2];
        ina[0] = 0.0;
        ina[1] = 0.0;
        inb[0] = width;
        inb[1] = 1.0;

        outa[0] = low;
        outa[1] = 0.0;
        outb[0] = high;
        outb[1] = 1.0;
        AstMapping *winmap = (AstMapping *) astWinMap( 2, ina, inb, outa,
                                                       outb, " " );
        astRemapFrame( fset, 2, winmap );

        //  And make the new FrameSet current.
        StarWCS* wcsp = getStarWCSPtr();
        wcsp->astWCSReplace( fset );

        (void) astAnnul( base );
        (void) astAnnul( current );
        (void) astAnnul( fset );
        (void) astAnnul( map );
        (void) astAnnul( winmap );
    }
    return TCL_OK;
}

//+
//   StarRtdImage::percentCmd
//
//   Purpose:
//     Returns image values that correspond to certain percentile
//     limits.
//
//   Notes:
//     Accepts one parameter. This is a list of percentiles.
//
//   Return:
//     A list of data values corresponding to the given percentiles.
//
//-
int StarRtdImage::percentCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::percentCmd" << std::endl;
#endif

    //  No image, no values.
    if ( ! image_ ) {
        return error( "no image loaded" );
    }

    //  Split the list and convert into doubles.
    char **levelsArgv;
    int nlevels = 0;
    if ( Tcl_SplitList( interp_, argv[0], &nlevels, &levelsArgv ) != TCL_OK ) {
        return error(
            "sorry: failed to decode the percentile levels (check format)" );
    }
    if ( nlevels == 0 ) {
        return error( "must give some percentiles" );
    }

    //  Convert these into doubles.
    double *levels = new double[nlevels];
    for ( int index = 0; index < nlevels; index++ ) {
        if ( Tcl_GetDouble( interp_, levelsArgv[index],
                            &levels[index] ) != TCL_OK ) {
            Tcl_Free( (char *) levelsArgv );
            delete [] levels;
            return error( levelsArgv[index], "is not a valid number");
        }
    }

    //  Get the histogram of data values.
    int numValues = 2048;
    double xyvalues[2048*2];
    image_->getDist( numValues, xyvalues );

    //  Find out how many pixels we actually counted (may be significant
    //  numbers of blanks)
    int npixels = 0;
    int i;
    for ( i = 0 ; i < numValues; i++ ) {
        npixels += (int)(xyvalues[i*2+1]);
    }
    if ( npixels == 0 ) {
        delete [] levels;
        Tcl_Free( (char *) levelsArgv );
        return error( "image contains no valid pixels" );
    }

    //  Now get percentiles. Assume ordered randomly.
    for ( i = 0; i < nlevels; i++ ) {

        //  Change percentile to pixel count.
        int cutoff = (int) ( (double) npixels * ( levels[i] / 100.0 ) );

        //  Run over histogram until pixel count exceeded, then
        //  interpolate to get better estimate.
        int count = 0;
        int j;
        int prev = 0;
        for ( j = 0; j < numValues; j++ ) {
            prev = count;
            count += (int)(xyvalues[j*2+1]);
            if ( count >= cutoff) {
                levels[i] = xyvalues[j*2];
                if ( j != numValues - 1 ) {
                    double interp = 1.0 - ( double(cutoff) - double(prev) ) /
                        ( double(count) - double(prev) );
                    levels[i] = xyvalues[(j+1)*2] -
                        ( xyvalues[(j+1)*2] - levels[i] ) * interp;
                }
                break;
            }
        }
    }

    //  Construct the result.
    char buf[TCL_DOUBLE_SPACE];
    for ( i = 0; i < nlevels; i++ ) {
        sprintf( buf, "%f", levels[i] );
        append_element( buf );
    }

    //  Free workspace and return.
    Tcl_Free( (char *) levelsArgv );
    delete [] levels;
    return TCL_OK;
}

//
// clip x to withing range x0 .. x1
//
static void myClip(double& x, double x0, double x1)
{
    if (x0 < x1) {
        if (x < x0)
            x = x0;
        else if (x > x1)
            x = x1;
    } else {
        if (x > x0)
            x = x0;
        else if (x < x1)
            x = x1;
    }
}

//+
//   StarRtdImage::gbandCmd
//
//   Purpose:
//      Do same job as RtdImage::mbandCmd, except deal with the case
//      when there is no WCS or the WCS is not a celestial coordinate
//      system.
//
//   Notes:
//      See mband command for arguments.
//
//   Return:
//      TCL_OK.
//
//-
int StarRtdImage::gbandCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::gbandCmd" << std::endl;
#endif

    //  If WCS is available see if it is celestial.
    StarWCS *wcsp = (StarWCS *) NULL;
    if ( isWcs() && isCelestial() ) {
        return RtdImage::mbandCmd( argc, argv );
    }

    //  Define buffers for width and height strings.
    char widthStr[TCL_DOUBLE_SPACE];
    char heightStr[TCL_DOUBLE_SPACE];
    char distStr[TCL_DOUBLE_SPACE];

    //  Extract the arguments.
    double x0, y0, x1, y1;
    int show_angle;
    char* from_type = argv[4];
    const char* to_type = "canvas";
    char buf[1024];

    if (Tcl_GetInt(interp_, argv[5], &show_angle) != TCL_OK) {
        return TCL_OK;
    }

    //  Convert input coordinates to canvas coords. Use canvas coords as
    //  will plot graphics in these.
    if (convertCoordsStr(0, argv[0], argv[1], NULL, NULL, x0, y0,
                         from_type, to_type) != TCL_OK
        || convertCoordsStr(0, argv[2], argv[3], NULL, NULL, x1, y1,
                            from_type, to_type) != TCL_OK) {
        return TCL_OK;
    }

    //  Clip to image bounds.
    double ix0 = 0.5;
    double iy0 = 0.5;
    double ix1 = 0.5 + image_->width();
    double iy1 = 0.5 + image_->height();
    if (   imageToCanvasCoords(ix0, iy0, 0) != TCL_OK
           || imageToCanvasCoords(ix1, iy1, 0) != TCL_OK) {
        return TCL_OK;
    }
    myClip(x0, ix0, ix1);
    myClip(x1, ix0, ix1);
    myClip(y0, iy0, iy1);
    myClip(y1, iy0, iy1);

    if ( wcsp != (StarWCS *)NULL ) {
        //  Non-celestial coordinates, need to transform without
        //  prejudice about hh/dd:mm:ss, so convert back to native
        //  from "degrees"
        double ra0 = x0, dec0 = y0, ra1 = x1, dec1 = y1, ra2 = x1, dec2 = y0;
        if (   canvasToWorldCoords(ra0, dec0, 0) != TCL_OK
               || canvasToWorldCoords(ra1, dec1, 0) != TCL_OK
               || canvasToWorldCoords(ra2, dec2, 0) != TCL_OK ) {
            return TCL_OK;
        }
        ra0 *= r2d_;
        ra1 *= r2d_;
        ra2 *= r2d_;
        dec0 *= r2d_;
        dec1 *= r2d_;
        dec2 *= r2d_;

        //  Get distances in world coords.
        double dist = wcsp->plaindist( ra0, dec0, ra1, dec1 );
        double width, height;
        sprintf( distStr, "%g", dist );
        if (show_angle) {
            width = wcsp->plaindist( ra0, dec0, ra2, dec2 );
            sprintf( widthStr, "%g", width );
            height = wcsp->plaindist( ra2, dec2, ra1, dec1 );
            sprintf( heightStr, "%g", height );
        }
    } else {

        //  Just image coordinates.
        double ra0 = x0, dec0 = y0, ra1 = x1, dec1 = y1;
        if ( canvasToImageCoords( ra0, dec0, 0 ) != TCL_OK
             || canvasToImageCoords( ra1, dec1, 0 ) != TCL_OK ) {
            return TCL_OK;
        }
        double dist = sqrt(( ra1 - ra0 ) * ( ra1 - ra0 ) +
                           ( dec1 - dec0 ) * ( dec1 - dec0 ));
        sprintf( distStr, "%g", dist );
        if (show_angle) {
            double width = fabs( ra1 - ra0 );
            sprintf( widthStr, "%g", width );
            double height = fabs( dec1 - dec0 );
            sprintf( heightStr, "%g", height );
        }
    }

    //  Code from mband.
    //  Calculate canvas coords for lines and labels and
    //  try to keep the labels out of the way so they don't block anything
    double mx = (x0 + x1)/2;
    double my = (y0 + y1)/2;
    int offset = 10;               // offset of labels from lines

    const char* diag_anchor = "c";       // label anchors
    const char* width_anchor = "c";
    const char* height_anchor = "c";

    int diag_xoffset = 0,          // x,y offsets for labels
        diag_yoffset = 0,
        width_yoffset = 0,
        height_xoffset = 0;

    if (fabs(y0 - y1) < 5) {
        diag_anchor = "s";
        diag_yoffset = offset;
        show_angle = 0;
    }
    else if (y0 < y1) {
        width_anchor = "s";
        width_yoffset = -offset;
    }
    else {
        width_anchor = "n";
        width_yoffset = offset;
    }

    if (fabs(x0 - x1) < 5) {
        diag_anchor  = "w";
        diag_xoffset = offset;
        diag_yoffset = 0;
        show_angle = 0;
    } else if (x0 < x1) {
        diag_anchor = "se";
        diag_xoffset = -offset;
        diag_yoffset = offset;
        height_anchor = "w";
        height_xoffset = offset;
    }
    else {
        diag_anchor = "nw";
        diag_xoffset = offset;
        diag_yoffset = -offset;
        height_anchor = "e";
        height_xoffset = -offset;
    }

    //  Evaluate Tk canvas commands in the image's canvas
    const char* canvas = canvasName_;

    //  Set diagonal line coords
    sprintf( buf, "%s coords mband_line %g %g %g %g\n",
             canvas, x0, y0, x1, y1 );
    Tcl_Eval( interp_, buf );

    //  Adjust labels
    sprintf( buf, "%s coords mband_diag_text %g %g\n",
             canvas, mx+diag_xoffset, my+diag_yoffset );
    Tcl_Eval( interp_, buf );

    sprintf( buf, "%s itemconfig mband_diag_text -text %s -anchor %s\n",
             canvas, distStr, diag_anchor );
    Tcl_Eval( interp_, buf );

    sprintf( buf, "%s bbox mband_diag_text\n", canvas );
    Tcl_Eval( interp_, buf );

    double rx0, ry0, rx1, ry1;
    if ( sscanf( Tcl_GetStringResult( interp_ ), "%lf %lf %lf %lf",
                 &rx0, &ry0, &rx1, &ry1) != 4 ) {
        return TCL_OK;
    }

    sprintf( buf, "%s coords mband_diag_rect %g %g %g %g\n",
             canvas,  rx0, ry0, rx1, ry1);
    Tcl_Eval( interp_, buf );

    if ( show_angle ) {
        //  Set angle line coords
        sprintf(buf, "%s coords mband_angle %g %g %g %g %g %g\n",
                canvas, x0, y0, x1, y0, x1, y1);
        Tcl_Eval(interp_, buf);

        sprintf(buf, "%s coords mband_width_text %g %g\n",
                canvas, mx, y0+width_yoffset);
        Tcl_Eval(interp_, buf);

        sprintf(buf, "%s itemconfig mband_width_text -text %s -anchor %s\n",
                canvas, widthStr, width_anchor);
        Tcl_Eval(interp_, buf);

        sprintf(buf, "%s bbox mband_width_text\n", canvas);
        Tcl_Eval(interp_, buf);

        if (sscanf( Tcl_GetStringResult( interp_ ), "%lf %lf %lf %lf", &rx0, &ry0, &rx1, &ry1) != 4)
            return TCL_OK;
        sprintf(buf, "%s coords mband_width_rect %g %g %g %g\n",
                canvas,  rx0, ry0, rx1, ry1);
        Tcl_Eval(interp_, buf);

        sprintf(buf, "%s coords mband_height_text %g %g\n",
            canvas, x1+height_xoffset, my);
        Tcl_Eval(interp_, buf);

        sprintf(buf, "%s itemconfig mband_height_text -text %s -anchor %s\n",
                canvas, heightStr, height_anchor);
        Tcl_Eval(interp_, buf);

        sprintf(buf, "%s bbox mband_height_text\n", canvas);
        Tcl_Eval(interp_, buf);

        if (sscanf(Tcl_GetStringResult( interp_ ), "%lf %lf %lf %lf", &rx0, &ry0, &rx1, &ry1) != 4)
            return TCL_OK;
        sprintf(buf, "%s coords mband_height_rect %g %g %g %g\n",
                canvas,  rx0, ry0, rx1, ry1);
        Tcl_Eval(interp_, buf);
    }
    else {

        //  Hide the width and height labels and lines
        x1 = x0 + 1;
        y1 = y0 + 1;
        sprintf(buf, "%s coords mband_angle %g %g %g %g\n", canvas, x0, y0, x1, y1);
        Tcl_Eval(interp_, buf);

        sprintf(buf, "%s itemconfig mband_width_text -text {}\n", canvas);
        Tcl_Eval(interp_, buf);

        sprintf(buf, "%s coords mband_width_rect %g %g %g %g\n", canvas, x0, y0, x1, y1);
        Tcl_Eval(interp_, buf);

        sprintf(buf, "%s itemconfig mband_height_text -text {}\n", canvas);
        Tcl_Eval(interp_, buf);

        sprintf(buf, "%s coords mband_height_rect %g %g %g %g", canvas, x0, y0, x1, y1);
        Tcl_Eval(interp_, buf);
    }
    return TCL_OK;
}

//+
//   StarRtdImage::parseName
//
//   Purpose:
//      Parses an image name into a filename, plus optional components
//      of a FITS extension, NDF slice and HDS component path.
//
//   Notes:
//      The given name is checked to see if it corresponds to a known
//      NDF name, or the name of a file that the NDF library can
//      process (such as a foreign format). If this is the case then
//      the existence of the file is checked. If it doesn't exist then
//      an NDF with the basename is check instead, if this exists then
//      it is assumed that the file "extension" corresponds to an NDF
//      within that container file.
//
//      FITS files are checked for existence, if these are not found
//      then no further action is taken, but note that the extension
//      specifier ([number]) and slice can also be valid.
//
//      The fullname, slice and path strings are set to a copy of the
//      appropriate parts of imagename. These should be deleted when no
//      longer needed.
//
//   Return:
//      TCL_OK if file exists, TCL_ERROR otherwise.
//
//-
int StarRtdImage::parseName( const char *imagename, char **fullname,
                             char **fitsext, char **slice, char **path )
{
    //  Create the output strings.
    int namelen = strlen( imagename ) + 1;
    *fullname = new char[namelen];
    *slice = new char[namelen];
    *path = new char[namelen];
    *fitsext = new char[namelen];
    strcpy( *fullname, imagename );

    //  Does the name as given exist as a file? If so we don't need to
    //  check for slices & paths.
    int exists = fileExists( *fullname );

    //  Look for a slice at the end of the file name. This needs to be
    //  removed while we do other tests.
    char *left = strrchr( *fullname, '(');
    char *right = strrchr( *fullname, ')');
    int haveslice = 0;
    if ( left && right && ! exists ) {
        strcpy( *slice, left );
        *left = '\0';
        haveslice = 1;
    }

    //  Look for a FITS extension, this should be before the slice,
    //  but after the file type (probably .fits).
    left = strrchr( *fullname, '[');
    right = strrchr( *fullname, ']');
    int havefitsext = 0;
    if ( left && right && ! exists ) {
        strcpy( *fitsext, left );
        *left = '\0';
        havefitsext = 1;
    }

    //  See if fullname is now just an existing file known to NDF.
    const char* type = fileSuffix( *fullname );
    if ( isNDFtype( type ) ) {

        //  Check that name is a file, if so nothing to do except to check
        //  that it is a regular file. Note fullname now has any slice removed.
        exists = fileExists( *fullname );
        if ( ! exists ) {
            delete[] *fullname;
            *fullname = NULL;
            delete[] *slice;
            *slice = NULL;
            delete[] *path;
            *path = NULL;
            return error( imagename, " cannot be accessed" );
        } else {
            if ( ! haveslice ) {
                delete[] *slice;
                *slice = NULL;
            }
            if ( ! havefitsext ) {
                delete[] *fitsext;
                *fitsext = NULL;
            }
            delete[] *path;
            *path = (char *) NULL;
            return TCL_OK;
        }
    } else {

        //  Could be FITS file. This needs periods and existence.
        char* p = strchr( *fullname, '.' );
        if ( p && exists ) {

            //  Assume non-NDF existent files are possible FITS.
            if ( ! haveslice ) {
                delete[] *slice;
                *slice = NULL;
            }
            if ( ! havefitsext ) {
                delete[] *fitsext;
                *fitsext = NULL;
            }
            delete[] *path;
            *path = NULL;
            return TCL_OK;
        }
    }

    //  OK. File as given doesn't correspond to a disk file. Either the
    //  name is wrong, or we may have a path to a HDS component. Look
    //  for a component name which should be after the ".sdf" string,
    //  but before the slice.
    int found = 0;
    char *sdf = strstr( *fullname, DAT__FLEXT );
    if ( sdf ) {
        sdf += DAT__SZFLX;
        if ( *sdf == '.' ) {
            strcpy( *path, sdf );
            *sdf = '\0';

            //  OK fullname should be a filename.
            found = fileExists( *fullname );
        }
    }
    if ( found ) {
        delete[] *fitsext;
        *fitsext = NULL;
        if ( ! haveslice ) {
            delete[] *slice;
            *slice = NULL;
        }
        return TCL_OK;
    } else {
        delete[] *fullname;
        *fullname = NULL;
        delete[] *fitsext;
        *fitsext = NULL;
        delete[] *slice;
        *slice = NULL;
        delete[] *path;
        *path = NULL;
        return error( imagename, " cannot be accessed" );
    }
}

//+
//   StarRtdImage::isNDFtype
//
//   Purpose:
//      Determines if the given string is a type that can be
//      interpreted by the NDF library (excluding FITS).
//
//    Return:
//      int, 1 if type is known to the NDF system.
//-
int StarRtdImage::isNDFtype( const char *type )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::isNDFType" << std::endl;
#endif

    // If the type is already known do things quickly.
    if ( strstr( type, "fit" ) ) {  // allan: 27.4.98: check all FITS types
        return 0;                     // including fits.gz, gzfits, fit, fits, cfits...
    } else if ( strcmp( type, "sdf" ) == 0 ) {
        return 1;
    } else {

        //  Check if the type is present in the NDF_FORMATS_IN environment
        //  variable.
        const char *ndf_formats = getenv( "NDF_FORMATS_IN" );
        if ( ndf_formats ) {
            if ( strstr( ndf_formats, type ) != 0 ) {
                return 1;
            } else {
                return 0;
            }
        } else {
            return 0;
        }
    }
}

//+
//   StarRtdImage::fileExists
//
//   Purpose:
//     Checks if a given file exists and is a regular file.
//
//   Return:
//      1 if file exists, 0 otherwise.
//-
int StarRtdImage::fileExists( const char *filename )
{
    struct stat buf;
    if ( stat( filename, &buf ) == 0 ) {
        if ( S_ISREG( buf.st_mode ) != 0 ) {
            return 1;
        }
    }
    return 0;
}

//+
//   StarRtdImage::objectCmd
//
//   Purpose:
//      Return or set the OBJECT value used. The behaviour depends on the
//      number of arguments, none to get the value, 1 to set it to the given
//      value. Note this value will be reset when a new image is loaded.
//-
int StarRtdImage::objectCmd( int argc, char *argv[] )
{
    if ( ! image_ ) {
        return TCL_OK;
    }
    if ( argc == 0 ) {
        return set_result( image_->object() );
    }
    image_->object( argv[0] );
    return TCL_OK;
}

//+
//   StarRtdImage::blankvalueCmd
//
//   Purpose:
//      Set the blank pixel value for an image. This may be necessary
//      if an image has BLANK values introduced. By default the NDF BAD
//      value for the current data type is used. If not arguments are
//      given the result is whether a blank value is already set or not.
//-
int StarRtdImage::blankvalueCmd( int argc, char *argv[] )
{
    if ( ! image_ ) {
        return TCL_OK;
    }

    //  Request for is the blank value set?
    if ( argc == 0 ) {
        return set_result( image_->haveBlank() );
    }

    //  Set the override for the blank value used by the image.
    image_->setBlank( gaiaArrayFITSBlankValue( image_->dataType() ) );

    //  Reinitialise blank value.
    image_->initBlankPixel();
    return TCL_OK;
}

//+
//   StarRtdImage::fullNameCmd
//
//
//   Purpose:
//      Returns the fullname of the currently displayed image.
//      This is different to the configuration option "-file" in that
//      the FITS HDU or NDF extension is also folded into the returned
//      value. This makes the name suitable for passing to external
//      routines that are required to access the same image (rather
//      than just the same file).
//-
int StarRtdImage::fullNameCmd( int argc, char *argv[] )
{
    ImageIO imio = image_->image();

    if ( ! isfits() ) {
        //  Parse the filename into bits
        char *name;
        char *fitsext;
        char *slice;
        char *path;
        if ( parseName( file(), &name, &fitsext, &slice, &path ) != TCL_OK ) {
            if ( name ) delete [] name;
            if ( fitsext ) delete [] fitsext;
            if ( slice ) delete [] slice;
            if ( path ) delete [] path;
            return error( file(), " is not a known NDF (internal error)" );
        }

        //  Get "HDU" number and construct the fullname.
        NDFIO *ndf = (NDFIO *) imio.rep();
        int nndf = ndf->getNDFNum();

        //  Need NDF path
        char ndfpath[FILEBUFSIZE], naxis1[32], naxis2[32],
            hasvar[32], hasqual[32];
        ndf->getNDFInfo( nndf, ndfpath, naxis1, naxis2, hasvar, hasqual );

        if ( ndfpath[0] == '.' && ndfpath[1] == '\0' ) {
            //  Main NDF, just return name.
            set_result( file() );
        } else {
            //  Sub-NDF, so need path (which has slice information
            //  appended already).
            char buffer[FILEBUFSIZE];
            strcpy( buffer, name );
            char *start = strstr( buffer, ndfpath );
            if ( start != NULL ) {
                *start ='\0';
            }
            strcat( buffer, ndfpath );
            set_result( buffer );
        }
        if ( name ) delete [] name;
        if ( slice ) delete [] slice;
        if ( path ) delete [] path;
    } else {

        //  FITS file, just need the name and the HDU number (if any).
        FitsIO *fits = (FitsIO *) imio.rep();
        int hdu = fits->getHDUNum();
        if ( hdu == 1 ) {
            set_result( file() );
        } else {
            char buffer[FILEBUFSIZE];
            sprintf( buffer, "%s[%d]", file(), hdu );
            set_result( buffer );
        }
    }
    return TCL_OK;
}

//+
//   StarRtdImage::isfitsCmd
//
//   Purpose:
//      Returns whether the displayed image is a FITS file or not.
//-
int StarRtdImage::isfitsCmd( int argc, char *argv[] )
{
    return set_result( isfits() );
}

//+
//   StarRtdImage::isfits
//
//   Purpose:
//      Returns whether the displayed image is a FITS file or not.
//-
int StarRtdImage::isfits()
{
    //  See what kind of image type we have.
    ImageIO imio = image_->image();
    int isfits = 0;
    if ( strcmp( imio.rep()->classname(), "StarFitsIO" ) == 0 ||
         strcmp( imio.rep()->classname(), "FitsIO" ) == 0 ) {
        isfits = 1;
    }
    else {
        isfits = 0;
    }
    return isfits;
}

//+
//   StarRtdImage::isCompoundCmd
//
//   Purpose:
//      Returns whether the displayed image is a CompoundImageData.
//-
int StarRtdImage::isCompoundCmd( int argc, char *argv[] )
{
    ImageIO imio = image_->image();
    if ( strcmp( image_->classname(), "CompoundImageData" ) == 0 ) {
        return set_result( 1 );
    }
    return set_result( 0 );
}


//+
//   StarRtdImage::remoteCmd
//
//   Purpose:
//     Override "remote" command so that the GaiaRtdRemote class is
//     used to present the remote control interface. This has better
//     facilities for keeping the ~/.rtd-remote interface up to date.
//
//-
int StarRtdImage::remoteCmd( int argc, char *argv[] )
{
    //  If no arguments return the current port number.
    if ( argc == 0 ) {
        if ( remote_ ) {
            return set_result( remote_->port() );
        }
        return TCL_OK;
    }

    //  Get port number (0 means that one is chosen for us).
    int port = 0;
    if ( Tcl_GetInt(interp_, argv[0], &port) == TCL_ERROR ) {
        return TCL_ERROR;
    }

    //  Delete existing remote object.
    if ( remote_ ) {
        delete remote_;
    }

    //  Create control object with the given port (if 0 the port number
    //  is available as remote_->port().
    remote_ = new GaiaRtdRemote( this, port );
    if ( remote_ ) {
        return remote_->status();
    }
    return TCL_ERROR;
}

//+
//   StarRtdImage::readonlyCmd
//
//   Purpose:
//      Implement the "readonly" command. This allows a readonly
//      displayed NDF to be switched to writable mode and the current
//      readonly status of the image to be determined.
//
//      FITS images are always fixed readonly or readwrite so the
//      result is just readonly status.
//
//-
int StarRtdImage::readonlyCmd( int argc, char *argv[] )
{
    int readonly = 1;
    int isndf = 0;
    NDFIO *ndf;
    StarFitsIO *fits;
    ImageIO imio = image_->image();

    //  Inquire image readonly status.
    if ( strcmp( imio.rep()->classname(), "NDFIO" ) == 0 ) {
        ndf = (NDFIO *) imio.rep();
        readonly = ndf->getReadonly();
        isndf = 1;
    } else {
        fits = (StarFitsIO *) imio.rep();
        readonly = fits->getReadonly();
        isndf = 0;
    }

    //  If just inquiring or this is a FITS image then return value.
    if ( argc == 0 || ! isndf ) {
        return set_result( readonly );
    }

    //  Get new value.
    int value;
    if ( Tcl_GetInt( interp_, argv[0], &value ) != TCL_OK ) {
        return error( argv[0], " is not an integer");
    }

    //  If readonly status changed then re-read image.
    if ( value != readonly && isndf ) {
        ndf->setReadonly( value );
        ndf->resetDisplayable();
    }
    return set_result( value );
}

//+
//  StarRtdImage::remoteTclCmd
//
//  Purpose:
//     Override the "remotetcl" subcommand to evaluate a Tcl command
//     in the RTD Tcl interpreter. Returns TCL_ERROR if eval fails.
//
//     usage: $image remotetcl $command
//-
int StarRtdImage::remoteTclCmd( int argc, char* argv[] )
{
    return Tcl_Eval( interp_, argv[0] );
}

//+
//  StarRtdImage::globalstatsCmd
//
//  Purpose:
//     Calculate global statistics for a list of object positions
//     using the IQE routine of RTD.
//
//  Description:
//     This command accepts a list of positions and a related size
//     parameter. The size parameter determines the region about each
//     position that will be used in the IQE parameter estimation (IQE
//     fits a 2D gaussian, i.e.:
//
//        {x1 y1 x2 y2 ...} box_size
//
//  Return:
//     The return from this routine is a list consisting of the
//     following mean values:
//
//        fwhmX          = FWHM in X
//        rangeFwhmX     = Range in fwhmX
//        fwhmY          = FWHM in Y
//        rangeFwhmY     = Range in fwhmY
//        angle          = angle of major axis, degrees, along X
//        objectPeak     = peak value of object above background
//        meanBackground = mean background level
//        nused          = number of positions used
//
//     These are for use in estimating seeing and focus parameters
//     across a whole image, rather than just for one object.
//
//-
int StarRtdImage::globalstatsCmd( int argc, char *argv[] )
{
    if ( !image_ ) {
        return error( "no image loaded" );
    }

    //  Get the size of the region to use around each object.
    int size = 5;
    if ( Tcl_GetInt( interp_, argv[1], &size ) != TCL_OK ) {
        return error( argv[1], "is not a valid size" );
    }

    //  Split the input list of positions into individual X and Y's.
    char **listArgv;
    int listArgc = 0;
    if ( Tcl_SplitList( interp_, argv[0], &listArgc, &listArgv ) != TCL_OK ) {
        return error( "failed to interpret X Y positions list" );
    }
    if ( listArgc <= 1 ) {
        return error( "must supply pairs of object positions" );
    }

    //  Now attempt to translate all values into doubles.
    int npoints = listArgc/2;
    double *x = new double[npoints];
    double *y = new double[npoints];
    int i, j;
    for ( i = 0, j = 0; i < listArgc; i += 2, j++ ) {
        if ( Tcl_GetDouble( interp_, listArgv[i], &x[j] ) != TCL_OK ||
             Tcl_GetDouble( interp_, listArgv[i+1], &y[j] ) != TCL_OK ) {
            delete [] x;
            delete [] y;
            Tcl_Free( (char *) listArgv );
            return error( listArgv[i], " is not a number" );
        }
    }

    //  Variables for sums and final results.
    double meanX = 0.0;
    double meanY = 0.0;
    double minFwhmX = DBL_MAX, maxFwhmX = -DBL_MAX;
    double minFwhmY = DBL_MAX, maxFwhmY = -DBL_MIN;
    double fwhmX = 0.0, sumFwhmX = 0.0;
    double fwhmY = 0.0, sumFwhmY = 0.0;
    double angle = 0.0;
    double objectPeak = 0.0, sumObjectPeak = 0.0;
    double meanBackground = 0.0, sumMeanBackground = 0.0;
    double *angles = new double[npoints];

    //  And get the image statistics sums.
    int ncount = 0;
    double newx, newy;
    double hw = 0.5 * (double) size;
    for ( i = 0; i < npoints; i++ ) {
        newx = x[i] - hw;
        newy = y[i] - hw;

        if ( image_->getStatistics( newx, newy, size, size, meanX, meanY,
                                    fwhmX, fwhmY, angle, objectPeak,
                                    meanBackground ) == 0 ) {
            ncount++;
            sumFwhmX += fwhmX;
            sumFwhmY += fwhmY;
            angles[i] = angle;                  //  Record angles.
            sumObjectPeak += objectPeak;
            sumMeanBackground += meanBackground;
            minFwhmX = min( minFwhmX, fwhmX );
            maxFwhmX = max( maxFwhmX, fwhmX );
            minFwhmY = min( minFwhmY, fwhmY );
            maxFwhmY = max( maxFwhmY, fwhmY );
        }
    }

    //  Construct return;
    if ( ncount == 0 ) {
        delete [] x;
        delete [] y;
        delete [] angles;
        Tcl_Free( (char *) listArgv );
        return error( "failed to get statistics for any objects" );
    }

    //  Angles need special handling, since a straight average doesn't make a
    //  lot of sense. Form the mean difference from the first angle,
    //  normalised as a PA, that's in the range +/-90.
    double diffsum = 0.0;
    double diff = 0.0;
    for ( int i = 1; i < npoints; i++ ) {

        //  Difference in angle from first position, in range +/- 90.0.
        diff = fmod( ( angles[i] - angles[0] ), 180.0 );
        if ( diff >= 90.0 ) {
            diff -= 180.0;
        }
        else if ( diff <= -90.0 ) {
            diff += 180.0;
        }
        diffsum += diff;
    }
    double meanAngle = angles[0] + ( diffsum /(double) ncount );

    //  Normalize this as well.
    if ( meanAngle >= 180.0 ) {
        meanAngle -= 180.0;
    }
    else if ( meanAngle <= -180.0 ) {
        meanAngle += 180.0;
    }

    char result[TCL_DOUBLE_SPACE*5 + 6];
    sprintf( result, "%f %f %f %f %f %f %f %d",
             sumFwhmX / (double) ncount,
             (maxFwhmX - minFwhmX),
             sumFwhmY / (double) ncount,
             (maxFwhmY - minFwhmY),
             meanAngle,
             sumObjectPeak / (double) ncount,
             sumMeanBackground / (double) ncount,
             ncount );

    set_result( result );
    return TCL_OK;
}

//+
//   StarRtdImage::asttran2Cmd
//
//   Purpose:
//       Transform two dimensional coordinates using either the image
//       WCS or a locally derived frameset.
//
//  Arguments:
//     argv, argc strings.
//
//     The first argument to this command is either "image" or "local"
//     and describes the source of the frameset to use during the
//     transformation. If "local" then a suitable astxxx command must
//     have been called to create it.
//
//     The second argument is a list consisting of pairs of
//     coordinates to be transformed.
//
//     The result of this command is the new set of coordinates, if
//     successful.
//-
int StarRtdImage::asttran2Cmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::asttran2Cmd" << std::endl;
#endif
    if (!image_) {
        return error("no image loaded");
    }

    //  Decode the first argument. This should be the source of the
    //  FrameSet that we are to use.
    char *source = argv[0];
    AstFrameSet *wcs = (AstFrameSet *) NULL;
    switch ( source[0] ) {
       case 'i': {
           //  FrameSet from image, need a clone.
           if ( newset_ ) newset_ = (AstFrameSet *) astAnnul( newset_ );
           StarWCS* wcsp = getStarWCSPtr();
           if ( !wcsp ) {
               return TCL_ERROR;
           }
           wcs = wcsp->astWCSClone();
       }
       break;
       case 'l': {
           //  Local, so make sure that astcreate has already been called
           //  (or this function once before).
           if ( !newset_ ) {
               return error("cannot use a local WCS system as none is "
                            "available (see astcreate, or use this command "
                            "once previously with source 'image')");
           }
           wcs = newset_;
       }
       break;
       default: {
           return error( source,
                         ": unknown WCS source, should be 'image' or 'local'");
       }
    }

    //  Extract the coordinates from the input list.
    char **listArgv;
    int listArgc = 0;
    if ( Tcl_SplitList( interp_, argv[1], &listArgc, &listArgv ) != TCL_OK ) {
        return error( "failed to interpret RA/Dec positions list" );
    }
    int npoints = listArgc / 2;
    if ( npoints * 2 != listArgc ) {
        Tcl_Free( (char *) listArgv );
        return error("coordinate lists contain a odd number of values");
    }

    // Check have at least two positions.
    if ( listArgc < 2 ) {
        Tcl_Free( (char *) listArgv );
        return error("need at least two positions");
    }

    // Now attempt to translate all these values into doubles.
    double *oldra = new double[npoints];
    double *olddec = new double[npoints];
    int i, j;
    for ( i = 0, j = 0; i < listArgc; i += 2, j++ ) {

        //  These should either be in degrees or H/D:M:S strings
        // (note values may only be in HH:MM:SS/DD:MM:SS or DD.ddd/DD.ddd
        //  no HH.hh, 2000.0 mean values are not converted to another equinox)
        WorldCoords wcs( listArgv[i], listArgv[i+1], 2000, 1);
        if ( wcs.status() != TCL_OK ) {
            delete [] oldra;
            delete [] olddec;
            return error( listArgv[i], " is not a celestial coordinate value" );
        }
        oldra[j] = wcs.ra_deg() * d2r_;
        olddec[j] = wcs.dec_deg() * d2r_;
    }
    double *newra = new double[npoints];
    double *newdec = new double[npoints];

    //  Now do the transformation.
    astTran2( wcs, npoints, oldra, olddec, 0, newra, newdec );
    if ( !astOK ) {
        astClearStatus;
        Tcl_Free( (char *) listArgv );
        delete [] oldra;
        delete [] olddec;
        delete [] newra;
        delete [] newdec;
        return error( "failed to transform positions" );
    }

    //  Construct a return list of the values. Note we want the
    //  formatting to match the base frame (where we're going).
    int base = astGetI( wcs, "Base" );
    int current = astGetI( wcs, "Current" );
    astSetI( wcs, "Current", base );
    astSetI( wcs, "Base", current );
    double point[2];
    reset_result();
    for ( i = 0; i < npoints; i++ ) {

        //  Normalize and format the ra,dec position arguments using the
        //  base frame default formatting.
        point[0] = newra[i];
        point[1] = newdec[i];
        astNorm( wcs, point );
        const char *ra_buf = astFormat( wcs, 1, point[0] );
        const char *dec_buf = astFormat( wcs, 2, point[1] );
        append_element( ra_buf );
        append_element( dec_buf );
    }
    astSetI( wcs, "Current", current );
    astSetI( wcs, "Base", base );

    //  Release memory.
    Tcl_Free( (char *) listArgv );
    delete [] oldra;
    delete [] olddec;
    delete [] newra;
    delete [] newdec;
    return TCL_OK;
}

//+
//   StarRtdImage::astwarningsCmd
//
//   Purpose:
//      Return the contents of any ASTWARN cards
//
//   Arguments:
//      None.
//
//   Result:
//      Either a formatted string containing the ASTWARN cards or
//      a blank string.
//-
int StarRtdImage::astwarningsCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astwarningsCmd" << std::endl;
#endif
    if ( image_ ) {
        StarWCS *wcs = getStarWCSPtr( image_ );
        if ( wcs ) {
            const char *warnings = wcs->getWarning();
            if ( warnings ) {
                set_result( warnings );
                }
            }
        }
    return TCL_OK;
}

//+
//   StarRtdImage::xyProfileCmd
//
//   Purpose:
//      Updates BLT vectors to contain the average values along the
//      X and Y directions of a rectangular region of the current
//      image. Essentially the X and Y profile equivalent of the
//      interactive "slice" command.
//
//   Arguments:
//
//      <bltGraph> is the path name of a BLT graph widget to display
//                 the X average pixel intensities.
//
//      <bltGraph> is the path name of a BLT graph widget to display
//                 the Y average pixel intensities.
//
//      <bltElem>  is the name of the element used in the graphs for
//                 that should receive the data.
//
//      x0, y0,    are the end points of a rectangle in the
//      x1, y1     given coordinate system (canvas, image, screen,
//                 wcs, deg).
//
//      xy_units   units of the rectangle coordinates.
//
//      xxVector   (returned) name of a BLT vector to receive the X
//                 profile X axes indices.
//
//      xyVector   (returned) name of a BLT vector to receive the X
//                 profile data values.
//
//      yxVector   (returned) name of a BLT vector to receive the Y
//                 profile X axes indices.
//
//      yyVector   (returned) name of a BLT vector to receive the Y
//                 profile data values.
//
//   Return:
//      A list of two values, the number of positions written to the X
//      and Y profiles.
//-

int StarRtdImage::xyProfileCmd(int argc, char *argv[])
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::xyProfileCmd (" << argc << ")" << std::endl;
#endif
    if ( !image_ ) {
        return TCL_OK;
    }

    //  Convert extent to image coords.
    double rx0, ry0, rx1, ry1;
    if (convertCoordsStr(0, argv[3], argv[4], NULL, NULL,
                         rx0, ry0, argv[7], "image") != TCL_OK
        || convertCoordsStr(0, argv[5], argv[6], NULL, NULL,
                            rx1, ry1, argv[7], "image") != TCL_OK) {
        return TCL_ERROR;
    }

    //  Get X and Y dimensions.
    int x0 = int( rx0 ), y0 = int( ry0 ), x1 = int( rx1 ), y1 = int( ry1 );
    int w = abs( x1 - x0 ) + 1;
    int h = abs( y1 - y0 ) + 1;

    //  Allocate space for results.
    double* xcoords = new double[w];
    double* xvalues = new double[w*2];
    double* ycoords = new double[h];
    double* yvalues = new double[h*2];

    //  And get the profile information. Do this by creating a suitable
    //  profiling object and passing it a reference to the image data
    //  values.
    ImageIO imageIO = image_->image();
    XYProfile xyProfile( imageIO );

    //  Tell the profile object if it needs to byte swap the image
    //  data. This is only necessary if the image is FITS on a non
    //  bigendian machine.
    xyProfile.setSwap( swapNeeded() );

    //  Set the region of image to extract the profiles from.
    xyProfile.setRegion( x0, y0, x1, y1 );

    //  Get the profiles.
    int numValues[2];
    xyProfile.extractProfiles( xcoords, xvalues, ycoords, yvalues, numValues );

    //  Copy into BLT vectors.
    int status = TCL_OK;
    if ( numValues[0] > 0 && numValues[1] > 0 ) {

        //  Transfer X index and data value array into two BLT vectors.
        status = Blt_GraphElement( interp_, argv[0], argv[2],
                                   numValues[0]*2, xvalues, argv[9],
                                   argv[10] );

        //  Transfer X coordinates into BLT vector.
        if ( status == TCL_OK ) {
            status = resetBltVector( numValues[0], xcoords, argv[8] );
        }

        //  Transfer Y index and data value array into two BLT vectors.
        if ( status == TCL_OK ) {
            status = Blt_GraphElement( interp_, argv[1], argv[2],
                                       numValues[1]*2, yvalues,
                                       argv[12], argv[13] );
        }

        //  Transfer Y coordinates into BLT vector.
        if ( status == TCL_OK ) {
           status = resetBltVector( numValues[1], ycoords, argv[11] );
        }
    }

    delete[] xcoords;
    delete[] ycoords;
    delete[] xvalues;
    delete[] yvalues;

    set_result( numValues[0] );
    append_element( numValues[1] );
    return status;
}


//+
//   StarRtdImage::xyHistogramCmd
//
//   Purpose:
//      Updates BLT vectors with a histogram of a rectangular region of the
//      current image.
//
//   Arguments:
//
//      <bltGraph> is the path name of a BLT graph widget
//
//      <bltElem>  is the name of the element used in graph.
//
//      x0, y0,    are the end points of a rectangle in the
//      x1, y1     given coordinate system (canvas, image, screen,
//                 wcs, deg).
//
//      xy_units   units of the rectangle coordinates.
//
//      datalimits whether to use the image low/high cuts to limit
//                 data range.
//
//      factor     binning factor, value in range 0 to 1.
//
//      xVector   (returned) name of a BLT vector to receive the
//                histogram coordinates.
//
//      yVector   (returned) name of a BLT vector to receive the
//                histogram counts.
//
//      gxVector  (returned) name of a BLT vector to receive the
//                gaussian fit coordinates.
//
//      gyVector  (returned) name of a BLT vector to receive the
//                gaussian fit counts.
//
//   Return:
//      A list containing the number of positions written to the vectors,
//      and estimates of:
//         peak position
//         count at peak position
//         peak position from parabolic fit
//         count at parabolic peak position
//         FWHM from parabolic fit
//         peak position from gaussian fit
//         error in peak position from gaussian fit
//         sigma from gaussian fit
//         error in sigma position from gaussian fit
//-

int StarRtdImage::xyHistogramCmd(int argc, char *argv[])
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::xyHistogramCmd (" << argc << ")" << std::endl;
#endif
    if ( !image_ ) {
        return TCL_OK;
    }

    //  Convert extent to image coords.
    double rx0, ry0, rx1, ry1;
    if (convertCoordsStr(0, argv[2], argv[3], NULL, NULL,
                         rx0, ry0, argv[6], "image") != TCL_OK
        || convertCoordsStr(0, argv[4], argv[5], NULL, NULL,
                            rx1, ry1, argv[6], "image") != TCL_OK) {
        return TCL_ERROR;
    }

    //  Get X and Y dimensions.
    int x0 = int( rx0 ), y0 = int( ry0 ), x1 = int( rx1 ), y1 = int( ry1 );

    //  And get the histogram. Do this by creating a suitable histogram object
    //  and passing it a reference to the image data values.
    ImageIO imageIO = image_->image();
    XYHistogram xyHistogram( imageIO );

    //  Tell the histogram object if it needs to byte swap the image
    //  data. This is only necessary if the image is FITS on a non
    //  bigendian machine.
    xyHistogram.setSwap( swapNeeded() );

    //  Set the region of image to use.
    xyHistogram.setRegion( x0, y0, x1, y1 );

    //  Whether to use the ImageIO data limits.
    xyHistogram.setDataLimits( image_->lowCut(), image_->highCut() );
    if ( *argv[7] == '0' ) {
        xyHistogram.setUseDataLimits( 0 );
    }
    else {
        xyHistogram.setUseDataLimits( 1 );
    }

    //  Set the binning factor.
    double factor;
    int status = TCL_OK;
    if ( Tcl_GetDouble(interp_, argv[8], &factor ) != TCL_OK ) {
        status = TCL_ERROR;
    }
    else {
        xyHistogram.setBinningFactor( factor );

        //  Get the histogram.
        Histogram histogram;
        xyHistogram.extractHistogram( &histogram );

        //  Copy into BLT vectors.
        if ( histogram.nbin > 0 ) {

            //  Transfer coords and counts into two BLT vectors.
            double *values = new double[histogram.nbin*2];
            for ( int i = 0, j = 0; i < histogram.nbin; i++, j += 2 ) {
                values[j] = i * histogram.width + histogram.zero;
                values[j+1] = histogram.hist[i];
            }
            status = Blt_GraphElement( interp_, argv[0], argv[1],
                                       histogram.nbin*2, values,
                                       argv[9], argv[10] );

            if ( status == TCL_OK ) {
                //  Same for gaussian fit.
                for ( int i = 0, j = 0; i < histogram.nbin; i++, j += 2 ) {
                    values[j] = i * histogram.width + histogram.zero;
                    values[j+1] = histogram.ghist[i];
                }
                status = Blt_GraphElement( interp_, argv[0], argv[1],
                                           histogram.nbin*2, values,
                                           argv[11], argv[12] );
            }
            delete[] values;
        }

        set_result( histogram.nbin );
        append_element( histogram.mode * histogram.width + histogram.zero );
        append_element( histogram.hist[histogram.mode] );

        append_element( histogram.ppeak * histogram.width + histogram.zero );
        append_element( histogram.hist[(int)round(histogram.ppeak)] );
        append_element( histogram.pfwhm * histogram.width );

        append_element( histogram.gpeak * histogram.width + histogram.zero );
        append_element( histogram.gdpeak* histogram.width );
        append_element( histogram.gsd * histogram.width );
        append_element( histogram.gdsd * histogram.width );

    }
    return status;
}


//+
//  Name:
//     StarRtdImage::resetBltVector
//
//  Purpose:
//     Reset data of a BLT vector to new values.
//
//  Arguments:
//     num = number of values in new vector data.
//
//     valueArr = pointer to new vector data.
//
//     vecName = name of BLT vector to modify.
//-
int StarRtdImage::resetBltVector( const int num, const double *valueArr,
                                  char *vecName )
{
    int i;
    int nbytes = sizeof(double) * num;

    /*  Note: Blt_Vector::arraySize is the number of bytes! */
    Blt_Vector *vecPtr;
    double *vecArray;

    /*  Get pointer to existing data area */
    if ( Blt_GetVector( interp_, vecName, &vecPtr) != 0 ) {
        return TCL_ERROR;
    }

    /*  Allocate space for the new vector, if needed */
    if ( vecPtr->arraySize < nbytes ) {
        vecArray = (double *) Tcl_Alloc( nbytes );
        if ( vecArray == NULL ) {
            return error( "failed to allocate memory" );
        }
    } else {

        /*  Reuse existing array. */
        vecArray = vecPtr->valueArr;
        nbytes = vecPtr->arraySize;
    }

    /*  Write the data into the vector */
    for ( i = 0; i < num; i++ ) {
        vecArray[i] = valueArr[i];
    }

    if ( Blt_ResetVector( vecPtr, vecArray, num, nbytes, TCL_DYNAMIC )
         != TCL_OK ) {
        return TCL_ERROR;
    }
    return TCL_OK;
}

//+
//   StarRtdImage::swapNeeded
//
//   Purpose:
//      Test if the data of current image needs to be byte swapped.
//
//   Return:
//      1 if swap is need, 0 otherwise.
//-
int StarRtdImage::swapNeeded()
{
    ImageIO imio = image_->image();
    return swapNeeded( imio );
}

//+
//   StarRtdImage::swapNeeded
//
//   Purpose:
//      Test if the data of a given image needs to be byte swapped.
//
//   Return:
//      1 if swap is need, 0 otherwise.
//-
int StarRtdImage::swapNeeded( ImageIO imio )
{
    //  Check the ImageIO format. Don't need a swap if the data is already in
    //  network byte order on a bigendian machine, or vice-versa.
    ImageIO *imptr = &imio;
    int usingNBO = imptr->usingNetBO();
    if ( BIGENDIAN ) {
        return ( !usingNBO );
    }
    return usingNBO;
}

//+
//   StarRtdImage::astdomainsCmd
//
//   Purpose:
//      Return a list of all the domain names in the current AST
//      frameset. If an argument is also given then the dimensionality of the
//      domain is also returned.
//
//   Result:
//      A tcl list of the names, paired with the dimensionality when
//      requested.
//-
int StarRtdImage::astdomainsCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astdomainsCmd" << std::endl;
#endif
    if (!image_) {
        return error("no image loaded");
    }
    StarWCS* wcsp = getStarWCSPtr();
    if ( !wcsp ) {
        return TCL_ERROR;
    }

    char *result = wcsp->getDomains( argc == 1 );
    set_result( result );
    free( result );
    return TCL_OK;
}

//+
//   StarRtdImage::astmilliCmd
//
//   Purpose:
//      Whether precision of displayed coordinates (RA and Dec)
//      should be shown to milli-arcsecond resolution.
//
//   Note this is a method, rather than a configuration option as we
//   need to do some work
//
//   Result:
//      A tcl list of the names.
//-
int StarRtdImage::astmilliCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astmilliCmd" << std::endl;
#endif
    if (!image_) {
        return error("no image loaded");
    }
    StarWCS* wcsp = getStarWCSPtr();
    if ( !wcsp ) {
        return TCL_ERROR;
    }
    if ( *argv[0] == '0' ) {
        wcsp->extraPrecision( 0 );
    }
    else {
        wcsp->extraPrecision( 1 );
    }
    return TCL_OK;
}

//+
//   StarRtdImage::astcarlinCmd
//
//   Purpose:
//      Set the value used for the CarLin attribute when reading
//      the WCS from a FITS channel. The value should be 0 or 1.
//-
int StarRtdImage::astcarlinCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astcarlinCmd" << std::endl;
#endif
    if ( *argv[0] == '0' ) {
        StarWCS::setCarLin( 0 );
    }
    else {
        StarWCS::setCarLin( 1 );
    }
    return TCL_OK;
}

//+
//   StarRtdImage::forcedegreesCmd
//
//   Purpose:
//      Set the value used for the forced degrees attribute.
//      The value should be 0 or 1.
//-
int StarRtdImage::forcedegreesCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::forcedegreesCmd" << std::endl;
#endif
    if ( *argv[0] == '0' ) {
        StarWCS::setForceDegrees( 0 );
    }
    else {
        StarWCS::setForceDegrees( 1 );
    }
    return TCL_OK;
}

//+
//   StarRtdImage::astalwaysmergeCmd
//
//   Purpose:
//      Set whether extension headers cards should be merge with
//      the ones from the primary HDU. True always merges the headers,
//      but false is just a suggestion.
//-
int StarRtdImage::astalwaysmergeCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::astalwaysmergeCmd" << std::endl;
#endif
    if ( *argv[0] == '0' ) {
        StarFitsIO::setAlwaysMerge( 0 );
    }
    else {
        StarFitsIO::setAlwaysMerge( 1 );
    }
    return TCL_OK;
}

//+
//   StarRtdImage::autosetcutlevels
//
//   Purpose:
//      Return the current setting of the autoSetCutLevels_ member.
//      When unset this stops the application of autocuts and
//      the user defined limits are honoured.
//-
int StarRtdImage::autosetcutlevelsCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
    cout << "Called StarRtdImage::autosetcutlevelsCmd" << std::endl;
#endif
    set_result( autoSetCutLevels_ );
    return TCL_OK;
}

//  ==================================
//  UKIRT quick look member functions.
//  ==================================

/*
 * This method is called from the RtdImageCamera class to display the
 * image from shared memory.  "info" contains all we need to know
 * about the image and "data" gives access to the shared memory for
 * the image.
 *
 * This is re-implemented so that we can retain the UKIRT quick look
 * statistics region. The mapping of this extra information is:
 *
 *    Lower X coordinate: ql_x0 = info.reserved[0]
 *    Lower Y coordinate: ql_y0 = info.reserved[1]
 *    Upper X coordinate: ql_x1 = info.reserved[2]
 *    Upper Y coordinate: ql_y1 = info.reserved[3]
 *    Rowcut (?):         ql_rowcut = info.reserved[4]
 *
 * Which makes use of 5 of the 9 reserved integers in the
 * rtdIMAGE_INFO structure.
 *
 * For the UKIRT XY profile extensions these members are reused with
 * much the same meaning. However, these are updated for each realtime
 * event and do not also require a motion event (not sure why the
 * old ones do either).
 */
int StarRtdImage::displayImageEvent( const rtdIMAGE_INFO& info,
                                     const Mem& data )
{
    //  Extract the UKIRT quick look region. These are passed in the
    //  reserved region so that we can retain the rtdIMAGE_INFO size
    //  at the current values.
    if ( ukirt_ql() || ukirt_xy() ) {
        ql_x0 = info.reserved[0];
        ql_y0 = info.reserved[1];
        ql_x1 = info.reserved[2];
        ql_y1 = info.reserved[3];
        ql_rowcut = info.reserved[4];
        if ( ukirt_xy() ) {

            //  Write out the data limits now to variables var(X0),
            //  var(X1), var(Y0), var(Y1) and var(ROWCUT).
            char *var = ( viewMaster_ ? viewMaster_->name() : instname_ );
            char buffer[36];
            sprintf( buffer, "%d", ql_x0 );
            Tcl_SetVar2( interp_, var, "X0", buffer, TCL_GLOBAL_ONLY );
            sprintf( buffer, "%d", ql_x1 );
            Tcl_SetVar2( interp_, var, "X1", buffer, TCL_GLOBAL_ONLY );
            sprintf( buffer, "%d", ql_y0 );
            Tcl_SetVar2( interp_, var, "Y0", buffer, TCL_GLOBAL_ONLY );
            sprintf( buffer, "%d", ql_y1 );
            Tcl_SetVar2( interp_, var, "Y1", buffer, TCL_GLOBAL_ONLY );
            sprintf( buffer, "%d", ql_rowcut );
            Tcl_SetVar2( interp_, var, "ROWCUT", buffer, TCL_GLOBAL_ONLY );
        }
    }
    return RtdImage::displayImageEvent( info, data );
}

/*
 * This virtual method is called for motion events to update the zoom
 * window and set trace variables based on the current mouse position.
 * We need to re-implement so that the UKIRT look quick statistics can
 * be made available as global variables.
 */
void StarRtdImage::processMotionEvent()
{
    RtdImage::processMotionEvent();

    //  Report NDF integer pixel indices rather than GRID coordinates.
    if ( pixel_indices() ) {

        //  Get the NDF origin information. Values default to 1. Note we
        //  always do this, once requested we need the PX and PY values to be
        //  set.
        char *xori = image_->image().get("LBOUND1");
        int xo = 1;
        if ( xori ) {
            xo = atoi( xori );
        }
        char *yori = image_->image().get("LBOUND2");
        int yo = 1;
        if ( yori ) {
            yo = atoi( yori );
        }

        //  The global variables var(X) and var(Y) have already been set
        //  to the GRID coordinates. Use those values to set related
        //  globals. Note do not reuse the var(X) and var(Y) these
        //  values are used to also pick values.
        char *var = ( viewMaster_ ? viewMaster_->name() : instname_ );
        char buf[100];
        int nb;
        nb = sprintf( buf, "set %s(PX) [expr round($%s(X))+(%d)-1]",
                      var, var, xo );
        Tcl_EvalEx( interp_, buf, nb, TCL_EVAL_GLOBAL );
        nb = sprintf( buf, "set %s(PY) [expr round($%s(Y))+(%d)-1]",
                      var, var, yo );
        Tcl_EvalEx( interp_, buf, nb, TCL_EVAL_GLOBAL );
    }

    //  Need the UKIRT quick look statistics box updates. These are originally
    //  by Min Tan (mt@roe.ac.uk).
    if ( ukirt_ql() ) {
        if( ( ql_x1 - ql_x0 ) > 1024 || ( ql_y1 - ql_x0 ) > 1024 ||
            ( ql_x1 - ql_x0 ) < 0 || ( ql_y1 - ql_y0 ) < 0 ) {
            //  Do nothing, if out of bounds?
        }
        else {
            //  Write out the data limits.
            char *var = ( viewMaster_ ? viewMaster_->name() : instname_ );
            char buffer[36];
            sprintf( buffer, "%d, %d", ql_x0, ql_y0 );
            Tcl_SetVar2( interp_, var, "XY1", buffer, TCL_GLOBAL_ONLY );

            sprintf( buffer, "%d, %d", ql_x1, ql_y1 );
            Tcl_SetVar2( interp_, var, "XY2", buffer, TCL_GLOBAL_ONLY );

            sprintf( buffer, "%d", ql_rowcut );
            Tcl_SetVar2( interp_, var, "ROWCUT", buffer, TCL_GLOBAL_ONLY );

            //  Derive the stats for our current data type.
            RegionStats sBox( image_->image() );
            sBox.setSwap( swapNeeded() );
            sBox.setRegion( ql_x0, ql_y0, ql_x1, ql_y1 );
            sBox.calc();

            //  Export the statistical values as TCL global variables.
            sprintf( buffer, "%ld", (long) sBox.getArea() );
            Tcl_SetVar2( interp_, var, "PIXELS", buffer, TCL_GLOBAL_ONLY );

            sprintf( buffer, "%d", (int) sBox.getMin() );
            Tcl_SetVar2( interp_, var, "SMIN", buffer, TCL_GLOBAL_ONLY );

            sprintf( buffer, "%d", (int) sBox.getMax() );
            Tcl_SetVar2( interp_, var, "SMAX", buffer, TCL_GLOBAL_ONLY );

            sprintf( buffer, "%ld", (long) sBox.getTotal() );
            Tcl_SetVar2( interp_, var, "TOTAL", buffer, TCL_GLOBAL_ONLY );

            sprintf( buffer, "%f", (float) sBox.getMean() );
            Tcl_SetVar2( interp_, var, "MEAN", buffer, TCL_GLOBAL_ONLY );

            sprintf( buffer, "%f", (float) sBox.getStd() );
            Tcl_SetVar2( interp_, var, "STD", buffer, TCL_GLOBAL_ONLY );
        }
    }
}
