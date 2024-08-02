//  Avoid inclusion into files more than once.
#ifndef _StarRtdImage_
#define _StarRtdImage_

/*+
 *  Name:
 *     StarRtdImage

 *  Purpose:
 *     Include file that defines the class that extends Skycat to add
 *     features required by the Starlink implementation.

 *  Authors:
 *     P.W. Draper (PWD)

 *  Copyright:
 *     Copyright (C) 1997-2005 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2007-2009 Science and Technology Facilities Council.
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
 *     12-JUN-1996 (PWD):
 *        Added foreign command.
 *     04-SEP-1997 (PWD):
 *        Added plotgrid method.
 *     08-OCT-1997 (PWD):
 *        loadImage now called loadFile and becomes virtual.
 *     16-Mar-1998 (Allan Brighton, ESO):
 *        Use class Skycat as the base class instead of Rtd.
 *        Make use of new Skycat/Rtd features supporting subclassing,
 *        new constructor args, GAIA_CONFIG macro,
 *        Added getStarImage(), getStarWCSPtr()
 *     22-APR-1998 (PWD):
 *        Now subclasses Skycat rather than RtdImage.
 *     13-JAN-1999 (PWD):
 *        Merged in changes from Allan Brighton's GAIA plugin (see
 *        history above).
 *     14-MAR-1999 (PWD):
 *        Added contour command and related changes.
 *     04-APR-2001 (PWD):
 *        Added the astaddcolourCmd member.
 *     16-JUL-2001 (PWD):
 *        Added UKIRT quick look code. Also needed processMotionEvent.
 *        The quick look is off by default.
 *     22-JAN-2003 (PWD):
 *        Added astmilliCmd to switch on milli-arcsec resolution.
 *     16-FEB-2004 (PWD):
 *        Added astalwaysmergeCmd.
 *     14-DEC-2005 (PWD):
 *        Added isCompoundCmd.
 *     26-APR-2006 (PWD):
 *        Added updateImageCmd and volatility marking.
 *     02-OCT-2008 (PWD):
 *        Added autosetcutlevelsCmd.
 *     17-AUG-2009 (PWD):
 *        Added astgetcloneCmd.
 *     09-JUN-2012 (PWD):
 *        Added stcplotCmd.
 *-
 */

#include "Skycat.h"
#include "StarWCS.h"
#include "ImageColor.h"
#include "NDFIO.h"
extern "C" {
#include "ast.h"
}

//
//  Image options used for image configuration. Note inheriting this struct
//  means that it is no longer POD (in the C++ sense), so cannot be used with
//  the offsetof macro. To work around that we implement a runtime function
//  that determines the offset from an actual instance (note we need to extend
//  Rtd_Options as RtdImageOptions requires that type).
//
typedef struct Gaia_Options : Rtd_Options
{
    char *ast_tag;      // Canvas tag for all ast graphics
    char *component;    // NDF component to display
    int deep_search;    // Look for NDFs in extensions
    int plot_wcs;       // Scale and orient plot symbols using WCS system if available
    int ukirt_ql;       // Whether ukirt quick look stats are enabled.
    int ukirt_xy;       // Whether ukirt quick look XY profiles are enabled.
    int pixel_indices;  // Whether X,Y coordinates are pixel indices.
} Gaia_Options;

//
//  Support for reading a string into an AST channel.
//
typedef struct ChannelData {
    const char *content;        //  The string to read into channel
    int read;                   //  true when string has been read,
                                //  set this initially to false
} ChannelData;

class StarRtdImageOptions : public RtdImageOptions
{
  public:

    struct Gaia_Options gaia_options_;

    StarRtdImageOptions() :
        RtdImageOptions( &gaia_options_ )
    {
        memset( &gaia_options_, '\0', sizeof( Gaia_Options ) );

        // Repeat RtdImageOptions initialisations. Probably better way to do
        // this.
        gaia_options_.displaymode = 1;
        gaia_options_.usexshm = 1;
        gaia_options_.usexsync = 1;
        gaia_options_.min_colors = 30;
        gaia_options_.max_colors = 60;

        gaia_options_.ast_tag = NULL;
        gaia_options_.component= NULL;
        gaia_options_.deep_search = 1;
        gaia_options_.plot_wcs = 1;
        gaia_options_.ukirt_ql = 0;
        gaia_options_.ukirt_xy = 0;
        gaia_options_.pixel_indices = 0;
    }
};

#if __GNUC__
//  Define an in-line function that returns the offset of a known member in
//  the Gaia_Options struct. Use runtime implementation to avoid POD issues
//  with offsetof macro.
static Gaia_Options gaia_options;
static int Gaia_Options_Offset( const char* member )
{
    static Gaia_Options *ptr = &gaia_options;
    if ( member[0] == 'a' && member[1] == 's' ) {
        return (char *)&ptr->ast_tag - (char *)ptr;
    }
    if ( member[0] == 'c' && member[1] == 'o' ) {
        return (char *)&ptr->component - (char *)ptr;
    }
    if ( member[0] == 'd' && member[1] == 'e' ) {
        return (char *)&ptr->deep_search - (char *)ptr;
    }
    if ( member[0] == 'p' && member[1] == 'l' ) {
        return (char *)&ptr->plot_wcs - (char *)ptr;
    }
    if ( member[0] == 'u' && member[1] == 'k' ) {
        if ( member[6] == 'x' ) {
            return (char *)&ptr->ukirt_xy - (char *)ptr;
        }
        return (char *)&ptr->ukirt_ql - (char *)ptr;
    }
    if ( member[0] == 'p' && member[1] == 'i' ) {
        return (char *)&ptr->pixel_indices - (char *)ptr;
    }
    return 0;
}

//  Define the Tk config options specific to GAIA.
#define GAIA_OPTION(x) (Gaia_Options_Offset(x))
#define GAIA_OPTIONS \
{TK_CONFIG_STRING, "-ast_tag",        NULL, NULL, "ast_element", GAIA_OPTION("ast_tag"),       0, NULL}, \
{TK_CONFIG_STRING, "-component",      NULL, NULL, "data",        GAIA_OPTION("component"),     0, NULL}, \
{TK_CONFIG_INT,    "-deep_search",    NULL, NULL, "1",           GAIA_OPTION("deep_search"),   0, NULL}, \
{TK_CONFIG_INT,    "-plot_wcs",       NULL, NULL, "1",           GAIA_OPTION("plot_wcs"),      0, NULL}, \
{TK_CONFIG_INT,    "-ukirt_ql",       NULL, NULL, "0",           GAIA_OPTION("ukirt_ql"),      0, NULL}, \
{TK_CONFIG_INT,    "-ukirt_xy",       NULL, NULL, "0",           GAIA_OPTION("ukirt_xy"),      0, NULL}, \
{TK_CONFIG_INT,    "-pixel_indices",  NULL, NULL, "0",           GAIA_OPTION("pixel_indices"), 0, NULL}
#else

//  Above only known to work with GCC, let other compilers complain (Solaris CC doesn't work).
#define GAIA_OPTION(x) Tk_Offset(Gaia_Options,x)
#define GAIA_OPTIONS \
{TK_CONFIG_STRING, "-ast_tag",        NULL, NULL, "ast_element", GAIA_OPTION(ast_tag),       0, NULL}, \
{TK_CONFIG_STRING, "-component",      NULL, NULL, "data",        GAIA_OPTION(component),     0, NULL}, \
{TK_CONFIG_INT,    "-deep_search",    NULL, NULL, "1",           GAIA_OPTION(deep_search),   0, NULL}, \
{TK_CONFIG_INT,    "-plot_wcs",       NULL, NULL, "1",           GAIA_OPTION(plot_wcs),      0, NULL}, \
{TK_CONFIG_INT,    "-ukirt_ql",       NULL, NULL, "0",           GAIA_OPTION(ukirt_ql),      0, NULL}, \
{TK_CONFIG_INT,    "-ukirt_xy",       NULL, NULL, "0",           GAIA_OPTION(ukirt_xy),      0, NULL}, \
{TK_CONFIG_INT,    "-pixel_indices",  NULL, NULL, "0",           GAIA_OPTION(pixel_indices), 0, NULL}
#endif

class StarRtdImage : public Skycat
{
  public:

   //  Constructor
   StarRtdImage( Tcl_Interp *interp, const char *instname, int argc,
                 char **argv, Tk_ImageMaster master, const char *imageType,
                 Tk_ConfigSpec *specs = (Tk_ConfigSpec *)NULL,
                 StarRtdImageOptions *options = (StarRtdImageOptions *)NULL );

   //  Destructor
   ~StarRtdImage();

   //  Entry point from tcl to create a image.
   static int CreateImage( Tcl_Interp *interp, char *name, int argc,
                           Tcl_Obj *CONST objv[], Tk_ImageType *typePtr,
                           Tk_ImageMaster master, ClientData *clientDataPtr );

   //  Replace the image data.
   int replaceImageDataCmd( int argc, char *argv[] );

   //  Get the image data.
   int imageDataCmd( int argc, char *argv[] );

   //  Run a foreign procedure.
   int foreignCmd( int argc, char *argv[] );

   //  Get the NDF origin information.
   int originCmd( int argc, char *argv[] );

   //  Plot an astrometry grid.
   int plotgridCmd( int argc, char *argv[] );

   //  Return if we are really using X shared memory
   int usingxshmCmd( int argc, char *argv[] );

   //  Return the value of an AST attribute of the main AstFrameSet.
   int astgetCmd( int argc, char *argv[] );

   //  Set the value of an AST attribute of the main AstFrameSet.
   int astsetCmd( int argc, char *argv[] );

   //  Create a new FITS channel
   int astcreateCmd( int argc, char *argv[] );

   // Return true if there is an image and it has a celestial
   // coordinate system.
   int astcelestialCmd( int argc, char *argv[] );

   //  Replace the existing WCS FrameSet.
   int astreplaceCmd( int argc, char *argv[] );

   //  Return a clone of the main AstFrameSet.
   int astgetcloneCmd( int argc, char *argv[] );

   //  Restore an old WCS FrameSet.
   int astrestoreCmd( int argc, char *argv[] );

   //  Delete a FITS channel.
   int astdeleteCmd( int argc, char *argv[] );

   //  Store a FITS card in a FITS channel.
   int aststoreCmd( int argc, char *argv[] );

   //  Clear a FITS channel.
   int astresetCmd( int argc, char *argv[] );

   //  Read a FITS channel.
   int astreadCmd( int argc, char *argv[] );

   //  Refine the image mapping to include a transform to a new set of
   //  positions.
   int astrefineCmd( int argc, char *argv[] );

   //  Write out local FrameSet using given encoding.
   int astwriteCmd( int argc, char *argv[] );

   //  Get a copy of the WCS from another file.
   int astcopyCmd( int argc, char *argv[] );

   //  Assign a linear transform to a WCS.
   int astassignCmd( int argc, char *argv[] );

   //  Fix the original WCS to be the current image WCS (which may
   //  be a replacement one).
   int astfixCmd( int argc, char *argv[] );

   //  Straight-forward conversion from WCS position to X,Y.
   int astwcs2pixCmd( int argc, char *argv[] );

   //  Straight-forward conversion from X,Y position to WCS.
   int astpix2wcsCmd( int argc, char *argv[] );

   //  Straight-forward conversion from X,Y position to current coordinates.
   int astpix2curCmd( int argc, char *argv[] );

   //  Straight-forward conversion from current coordinates (proper
   //  units) to image coords.
   int astcur2pixCmd( int argc, char *argv[] );

   //  Derive simple statistics for creating WCS systems from scratch.
   int astbootstatsCmd( int argc, char *argv[] );

   //  Set the celestial coordinate system.
   int astsystemCmd( int argc, char *argv[] );

   //  Transform celestial coordinates.
   int asttran2Cmd( int argc, char *argv[] );

   //  Get a list of the available domains.
   int astdomainsCmd( int argc, char *argv[] );

   //  Write the current image and if new WCS (if modified) to a file.
   int dumpCmd( int argc, char *argv[] );

   //  Colorramp command, with pseudo WCS.
   int colorrampCmd( int argc, char *argv[] );

   //  List of percentiles.
   int percentCmd( int argc, char *argv[] );

   //  Enable remote control interface.
   int remoteCmd( int argc, char *argv[] );

   //  Draw a Skycat-like ellipse, rotbox and rectangle.
   virtual int draw_symbol(const char *shape,
                           double x, double y, const char *xy_units,
                           double radius, const char *radius_units,
                           const char *bg, const char *fg,
                           const char *symbol_tags,
                           double ratio = 1., double angle = 0.,
                           const char *label = NULL,
                           const char *label_tags = NULL);
   virtual int draw_ellipse(double x, double y, const char *xy_units,
                            double radius, const char *radius_units,
                            const char *bg, const char *fg,
                            const char *symbol_tags,
                            double ratio = 1., double angle = 0.0,
                            const char *label = NULL,
                            const char *label_tags = NULL);
   virtual int draw_rtdellipse( double *x, double *y, const char *xy_units,
                                const char *bg, const char *fg,
                                const char *symbol_tags, const char *label,
                                const char *label_tags );
   virtual int draw_rotbox(double x, double y, const char *xy_units,
                           double radius, const char *radius_units,
                           const char *bg, const char *fg,
                           const char *symbol_tags,
                           double ratio = 1., double angle = 0.0,
                           const char *label = NULL,
                           const char *label_tags = NULL);
   virtual int draw_rectangle(double x, double y, const char *xy_units,
                              double radius, const char *radius_units,
                              const char *bg, const char *fg,
                              const char *symbol_tags,
                              double ratio = 1., double angle = 0.0,
                              const char *label = NULL, const
                              char *label_tags = NULL);
   virtual int draw_polygon( int npoint, double *x, double *y,
                             const char *xy_units,
                             const char *bg, const char *fg,
                             const char *symbol_tags, const char *label,
                             const char *label_tags );

   virtual int draw_stcshape(double x, double y, const char *xy_units,
                             double radius, const char *radius_units,
                             const char *bg, const char *fg,
                             const char *symbol_tags,
                             double ratio = 1., double angle = 0.0,
                             const char *label = NULL, const
                             char *label_tags = NULL);

   //  Set the colour of blank pixels.
   int blankcolorCmd( int argc, char *argv[] );

   //  Slice command (X and Y coordinates).
   int sliceCmd( int argc, char *argv[] );

   //  Contour command.
   int contourCmd( int argc, char *argv[] );

   //  STC-S command.
   int stcplotCmd( int argc, char *argv[] );

   //  MOC command.
   int mocplotCmd( int argc, char *argv[] );

   //  HDU command.
   int hduCmd( int argc, char *argv[] );

   //  Replacement mband command, also allows for pixels and non-celestial.
   int gbandCmd( int argc, char *argv[] );

   //  Fullname command.
   int fullNameCmd( int argc, char *argv[] );

   //  Object command.
   int objectCmd( int argc, char *argv[] );

   //  "isfits" command.
   int isfitsCmd( int argc, char *argv[] );

   //  "iscompound" command.
   int isCompoundCmd( int argc, char *argv[] );

   //  Readonly command (used to make NDF writeable).
   int readonlyCmd( int argc, char *argv[] );

   //  Evaluate command from remote client.
   int remoteTclCmd( int argc, char* argv[] );

   //  Get global statistics for a list of objects.
   int globalstatsCmd( int argc, char *argv[] );

   //  Get any AST warning issued when image WCS was read.
   int astwarningsCmd( int argc, char *argv[] );

   //  Create X and Y profiles of rectangular region.
   int xyProfileCmd( int argc, char *argv[] );

   //  Create histogram of rectangular region.
   int xyHistogramCmd( int argc, char *argv[] );

   //  Enable readouts etc. to show milli-arcsec precision.
   int astmilliCmd( int argc, char *argv[] );

   //  Set the CarLin attribute of FITS channels.
   int astcarlinCmd( int argc, char *argv[] );

   //  Set the forced degrees attribute.
   int forcedegreesCmd( int argc, char *argv[] );

   //  Set the always merge attribute.
   int astalwaysmergeCmd( int argc, char *argv[] );

   //  Configure the bias images.
   int biasimageCmd( int argc, char *argv[] );

   // Command to get or set the volatility status. Note this happens
   // automatically when replaceImageDataCmd is called.
   int volatileCmd( int argc, char *argv[] );

   // Command to get the value of the autoSetCutLevels_ member.
   int autosetcutlevelsCmd( int argc, char *argv[] );

   //  Clear the cached STC mapping. Do this for new catalogues.
   void clearStcMapping() {
       if ( stcMapping_ != NULL ) {
           stcMapping_ = (AstMapping *) astAnnul( stcMapping_ );
       }
   };

   // Set the blank pixel value to that appropriate for data type,
   // or return if it is set.
   int blankvalueCmd( int argc, char *argv[] );

 protected:

   //  Redefined from parent class to check configuration options.
   virtual int configureImage(int argc, char* argv[], int flags);

   //  Call a named member function.
   virtual int call( const char*, int, int, char** );

   //  Return an ImageData object, given an ImageIO object reference.
   virtual ImageData *makeImage(ImageIO);

   //  Load an image file and return a pointer to the ImageData object for it.
   virtual ImageData *getStarImage( const char *filename,
                                    const char *fitsext,
                                    const char *slice,
                                    const char *path );

   //  Load an image.
   virtual int loadFile();

   //  Return the AST graphics item tag.
   char *ast_tag() const {
       return ((StarRtdImageOptions* )options_)->gaia_options_.ast_tag;
   }

   //  Return the NDF component displayed.
   char *component() const {
       return ((StarRtdImageOptions* )options_)->gaia_options_.component;
   }

   //  Return whether we're checking for related NDFs in extensions.
   int deep_search() const {
       return ((StarRtdImageOptions* )options_)->gaia_options_.deep_search;
   }

   //  Return whether to scale and orient plotting symbols using the WCS
   int plot_wcs() const {
       return ((StarRtdImageOptions* )options_)->gaia_options_.plot_wcs;
   }

   //  Return whether UKIRT quick look statistics are available.
   int ukirt_ql() const {
       return ((StarRtdImageOptions* )options_)->gaia_options_.ukirt_ql;
   }

   //  Return whether UKIRT quick look XY profiles are being used.
   int ukirt_xy() const {
       return ((StarRtdImageOptions* )options_)->gaia_options_.ukirt_xy;
   }

   //  Return whether pixel indices are calculated by processMotionEvent.
   int pixel_indices() const {
       return ((StarRtdImageOptions* )options_)->gaia_options_.pixel_indices;
   }

   //  Test if file extension is known to NDF.
   int isNDFtype( const char *);

   //  Test if file is FITS or not.
   int isfits();

   //  Parse and test image name.
   int parseName( const char *imagename, char **fullname,
                  char **fitsext, char **slice, char **path );

   //  Check if the named file exists.
   int fileExists( const char *filename );

   //  Pointer to the original FrameSet.
   AstFrameSet *origset_;

   //  Pointer to a new FrameSet (during manipulation).
   AstFrameSet *newset_;

   //  Pointer to the old WCS FrameSet (used to restore existing system).
   AstFrameSet *oldset_;

   //  Pointers to available FITS channels.
   enum {MAX_CHANNELS = 8};
   AstFitsChan *channels_[MAX_CHANNELS];

   //  Determine a linear mapping between two sets of positions.
   int mapPositions( int iframe, AstFrameSet *fset,
                     int fittype, double *xold,
                     double *yold, double *xnew,
                     double *ynew, int npoints );

   //  Add a linear mapping to a Frame in a FrameSet
   int addLinear( int iframe, AstFrameSet *fset, double *tr, int fittype=5 );

   //  Create and store a FITS card in a FITS channel.
   void storeCard( AstFitsChan *channel, const char *keyword, const char *value,
                   const char *comment, int isString, int overwrite = 1 );

   //  Create and initialise a new FITS channel.
   void initChannel( int slot );

   //  Decode a linear transformation into parts.
   void decodeLinear( double tr[6], double &xz, double &yz,
                      double &xs, double &ys, double &perp,
                      double &orient );

   //  Get canvas positions of 3 points -- centre, north and east.
   int get_compass(double x, double y, const char* xy_units,
                   double radius, const char* radius_units,
                   double ratio, double angle,
                   double& cx, double& cy, double& nx, double& ny,
                   double& ex, double& ey);

   //  Return a pointer to the StarWCS object for the image, or NULL on error
   StarWCS* getStarWCSPtr(ImageData* image = (ImageData*)NULL);

   //  Create a basic GRID/PIXEL coordinates domain FrameSet.
   AstFrameSet* makeGridWCS( ImageData *image = NULL );

   //  Create AST plot for drawing grids and contours to canvas.
   AstPlot* createPlot( AstFrameSet *wcs, AstFrameSet *extraset,
                        const char *domain, int full, int image,
                        double region[], int report, AstRegion **bounds );

   //  Implementation of hdu command for FITS files.
   int fitsHduCmd( const ImageIO &imio, int argc, char *argv[] );

   //  Implementation of "hdu list" command.
   int hduCmdCompList( int argc, char** argv, FitsIO* fits );

   //  Implementation of hdu command for NDF files.
   int ndfHduCmd( const ImageIO &imio, int argc, char *argv[] );

   //  Change the displayed NDF to another (or another component).
   int ndfCmdSet( int argc, char *argv[], NDFIO *ndf );

   //  Get list of NDF properties.
   int ndfCmdList( int argc, char *argv[], NDFIO *ndf );

   //  Display NDFs as single image.
   int ndfCmdDisplay( int argc, char *argv[], NDFIO *ndf );

   //  Get the FITS headers of the NDF.
   int ndfCmdFits( int argc, char *argv[], NDFIO *ndf );

   //  Get a FITS HDU and write to a disk file.
   int hduCmdGet( int argc, char** argv, FitsIO* fits );

   //  Change the data of a BLT vector.
   int resetBltVector( const int num, const double *valueArr,
                       char *vecName );

   //  Whether associated image data is byte swapped.
   int swapNeeded();

   //  Whether an image has byte swapped data.
   int swapNeeded( ImageIO imio );

   //  UKIRT quick look members.
   //  Deal with motion event in main window. Updates trace variables
   //  for real-time readouts.
   void processMotionEvent();

   //  Called from the Camera class to display image from shared memory
   virtual int displayImageEvent( const rtdIMAGE_INFO&,
                                  const Mem& data );

   //  UKIRT Quick Look parameters.
   int ql_x0;
   int ql_y0;
   int ql_x1;
   int ql_y1;
   int ql_rowcut;

   // Flag indicating if image data has been updated externally.
   int volatile_;

   // Member to pass channel data string.
   ChannelData channelData_;

   //  Cached mapping used when plotting STC regions. Don't
   //  want to connect every region to the WCS.
   AstMapping *stcMapping_;

   // Return true if there is an image and it has a celestial
   // coordinate system.
   int isCelestial();

 private:

   // Copy constructor -- not defined.
   StarRtdImage( const StarRtdImage&) ;
};

#endif // StarRtdImage
