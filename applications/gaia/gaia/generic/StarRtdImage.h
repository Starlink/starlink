//  Avoid inclusion into files more than once.
#ifndef _StarRtdImage_
#define _StarRtdImage_

//
//+
// Name:
//    StarRtdImage
//
// Purpose:
//    Include file that defines the class that extends Skycat to add
//    features required by the Starlink implementation.
//
// Authors:
//    P.W. Draper (PWD)
//
// Copyright:
//    Copyright (C) 1997-2001 Central Laboratory of the Research Councils
//
// History:
//    15-FEB-1996 (PWD):
//       Original version.
//    12-JUN-1996 (PWD):
//       Added foreign command.
//    04-SEP-1997 (PWD):
//       Added plotgrid method.
//    08-OCT-1997 (PWD):
//       loadImage now called loadFile and becomes virtual.
//    16-Mar-1998 (Allan Brighton, ESO):
//       Use class Skycat as the base class instead of Rtd.
//       Make use of new Skycat/Rtd features supporting subclassing,
//       new constructor args, GAIA_CONFIG macro,
//       Added getStarImage(), getStarWCSPtr()
//    22-APR-1998 (PWD):
//       Now subclasses Skycat rather than RtdImage.
//    13-JAN-1999 (PWD):
//       Merged in changes from Allan Brighton's GAIA plugin (see
//       history above).
//    14-MAR-1999 (PWD):
//       Added contour command and related changes.
//    04-APR-2001 (PWD):
//       Added the astaddcolourCmd member.
//    16-JUL-2001 (PWD):
//       Added UKIRT quick look code. Also needed processMotionEvent.
//       The quick look is off by default.
//    22-JAN-2003 (PWD):
//       Added astmilliCmd to switch on milli-arcsec resolution.
//    16-FEB-2004 (PWD):
//       Added astalwaysmergeCmd.
//    14-DEC-2005 (PWD):
//       Added isCompoundCmd.
//
//-

#include "Skycat.h"
#include "StarWCS.h"
#include "ImageColor.h"
extern "C" {
#include "ast.h"
}

//
//  Image options (used for image configuration). Note inheriting this struct
//  seems to cause the "invalid access to non-static data member" warning
//  from g++ (using the plain struct in RTD doesn't do that).
//
typedef struct Gaia_Options : Rtd_Options
{
    char *ast_tag;      // Canvas tag for all ast graphics
    char *component;    // NDF component to display
    int plot_wcs;       // Scale and orient plot symbols using WCS system if available
    int ukirt_ql;       // Whether ukirt quick look stats are enabled.
} Gaia_Options;

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
        gaia_options_.plot_wcs = 1;
        gaia_options_.ukirt_ql = 0;
    }
};

//  Define the Tk config options specific to GAIA.
#define GAIA_OPTION(x) Tk_Offset(Gaia_Options, x)
#define GAIA_OPTIONS \
{TK_CONFIG_STRING, "-ast_tag",   NULL, NULL, "ast_element", GAIA_OPTION(ast_tag),   0}, \
{TK_CONFIG_STRING, "-component", NULL, NULL, "data",        GAIA_OPTION(component), 0}, \
{TK_CONFIG_INT,    "-plot_wcs",  NULL, NULL, "1",           GAIA_OPTION(plot_wcs),  0}, \
{TK_CONFIG_INT,    "-ukirt_ql",  NULL, NULL, "0",           GAIA_OPTION(ukirt_ql),  0}

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
    static int CreateImage( Tcl_Interp*, char *name, int argc,
#if TCL_MAJOR_VERSION >= 8 && TCL_MINOR_VERSION >= 3
                            Tcl_Obj *CONST objv[],
#else
                            char **argv,
#endif
                            Tk_ImageType*, Tk_ImageMaster, ClientData* );

   //  Run a foreign procedure.
   int foreignCmd( int argc, char *argv[] );

   //  Get the NDF origin information.
   int originCmd( int argc, char *argv[] );

   //  Plot an astrometry grid.
   int plotgridCmd( int argc, char *argv[] );

   //  Return if we are really using X shared memory
   int usingxshmCmd( int argc, char *argv[] );

   //  Add a colour to extend the default set.
   int astaddcolourCmd( int argc, char *argv[] );

   //  Set whether fonts should be resized with canvas.
   int astfontresizeCmd( int argc, char *argv[] );

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

   //  Draw a Skycat-like ellipse and rotbox
   virtual int draw_symbol(const char *shape,
                           double x, double y, const char *xy_units,
                           double radius, const char *radius_units,
                           const char *bg, const char *fg,
                           const char *symbol_tags,
                           double ratio = 1., double angle = 0.,
                           const char *label = NULL, const char *label_tags = NULL);
   virtual int draw_ellipse(double x, double y, const char *xy_units,
                            double radius, const char *radius_units,
                            const char *bg, const char *fg,
                            const char *symbol_tags,
                            double ratio = 1., double angle = 0.0,
                            const char *label = NULL, const char *label_tags = NULL);
   virtual int draw_rotbox(double x, double y, const char *xy_units,
                           double radius, const char *radius_units,
                           const char *bg, const char *fg,
                           const char *symbol_tags,
                           double ratio = 1., double angle = 0.0,
                           const char *label = NULL, const char *label_tags = NULL);

   //  Get a file from the WWW.
   int urlgetCmd( int argc, char *argv[] );

   //  Set the colour of blank pixels.
   int blankcolorCmd( int argc, char *argv[] );

   //  Slice command (X and Y coordinates).
   int sliceCmd( int argc, char *argv[] );

   //  Contour command.
   int contourCmd( int argc, char *argv[] );

   //  HDU command.
   int hduCmd( int argc, char *argv[] );

   //  Replacement mband command, also allows for pixels and non-celestial.
   int gbandCmd( int argc, char *argv[] );

   //  Fullname command.
   int fullNameCmd( int argc, char *argv[] );

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

   //  Enable readouts etc. to show milli-arcsec precision.
   int astmilliCmd( int argc, char *argv[] );

   //  Access to some SLALIB routines.
   int slalibCmd( int argc, char *argv[] );

   //  Set the CarLin attribute of FITS channels.
   int astcarlinCmd( int argc, char *argv[] );

   //  Set the always merge attribute.
   int astalwaysmergeCmd( int argc, char *argv[] );

   //  Direct NDF access.
   int ndfCmd( int argc, char *argv[] );

   //  Configure the bias images.
   int biasimageCmd( int argc, char *argv[] );

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

   //  Return whether to scale and orient plotting symbols using the WCS
   int plot_wcs() const {
       return ((StarRtdImageOptions* )options_)->gaia_options_.plot_wcs;
   }

   //  Return whether UKIRT quick look statistics are available.
   int ukirt_ql() const {
       return ((StarRtdImageOptions* )options_)->gaia_options_.ukirt_ql;
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
                   const char *comment, int overwrite = 1 );

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
                        int full, int image, double region[] );

   //  Implementation of hdu command for FITS files.
   int fitsHduCmd( const ImageIO &imio, int argc, char *argv[] );

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

 private:

   // Copy constructor -- not defined.
   StarRtdImage( const StarRtdImage&) ;

   // Return true if there is an image and it has a celestial
   // coordinate system.
   int isCelestial();
};

#endif // StarRtdImage
