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
//    Copyright (C) 1997-1998 Central Laboratory of the Research Councils
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
//    14-PAR-1999 (PWD):
//       Added contour command and related changes.
//
//-

#include "Skycat.h"
#include "StarWCS.h"
#include "ImageColor.h"
extern "C" {
#include "ast.h"
}

//
//  Image options (used for image configuration).
//
class StarRtdImageOptions : public RtdImageOptions {
public:
  char *ast_tag;                // canvas tag for all ast graphics
  char *component;              // NDF component to display
  int plot_wcs;                 // Scale and orient plot symbols using
                                // WCS system if available

  StarRtdImageOptions()
    : ast_tag(NULL), component(NULL), plot_wcs(1)
    {}
};

//  Define the Tk config options here so that derived classes can add
//  more options. The definition "inherits" the Rtd options (skycat
//  does not define any new options) (allan).
#define GAIA_OPTION(x) Tk_Offset(StarRtdImageOptions, x)
#define GAIA_OPTIONS \
  RTD_OPTIONS, \
    {TK_CONFIG_STRING, "-ast_tag",   NULL, NULL, "ast_element", GAIA_OPTION(ast_tag), 0}, \
    {TK_CONFIG_STRING, "-component", NULL, NULL, "data", GAIA_OPTION(component), 0}, \
    {TK_CONFIG_INT,    "-plot_wcs",  NULL, NULL, "1", GAIA_OPTION(plot_wcs), 0}


class StarRtdImage : public Skycat {

public:

  //  Constructor
  StarRtdImage(Tcl_Interp*, const char* instname, int argc, char** argv,
	     Tk_ImageMaster master, const char* imageType,
	     Tk_ConfigSpec* specs = (Tk_ConfigSpec*)NULL,
	     RtdImageOptions* options = (RtdImageOptions*)NULL);

  //  Destructor
  ~StarRtdImage();

  //  Entry point from tcl to create a image.
  static int CreateImage( Tcl_Interp *, char *, int, char **, Tk_ImageType *,
                          Tk_ImageMaster, ClientData * );

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

  //  Create a new FITS channel
  int astcreateCmd( int argc, char *argv[] );

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

  //  Derive simple statistics for creating WCS systems from scratch.
  int astbootstatsCmd( int argc, char *argv[] );

  //  Set the celestial coordinate system.
  int astsystemCmd( int argc, char *argv[] );

  //  Write the current image and if new WCS (if modified) to a file.
  int dumpCmd( int argc, char *argv[] );

  //  Colorramp command, with pseudo WCS.
  int colorrampCmd( int argc, char *argv[] );

  //  List of percentiles.
  int percentCmd( int argc, char *argv[] );

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
  gbandCmd( int argc, char *argv[] );

protected:

  //  Pointer to structure that holds the image configuration options.
  StarRtdImageOptions *staroptionsPtr_;

  //  Redefined from parent class to check configuration options.
  virtual int configureImage(int argc, char* argv[], int flags);

  //  Call a named member function.
  virtual int call( const char*, int, int, char** );

  //  Return an ImageData object, given an ImageIO object reference.
  virtual ImageData* makeImage(ImageIO);

  //  Load an image file and return a pointer to the ImageData object for it.
  virtual ImageData* getStarImage(const char* filename, 
				  const char* slice,
				  const char* path);

  //  Load an image.
  virtual int loadFile();

  //  Return the AST graphics item tag.
  char *ast_tag() const { return staroptionsPtr_->ast_tag; }

  //  Return the NDF component displayed.
  char *component() const { return staroptionsPtr_->component; }

  //  Return whether to scale and orient plotting symbols using the WCS
  int plot_wcs() const { return staroptionsPtr_->plot_wcs; }

  //  Test if file extension is known to NDF.
  int isNDFtype( const char *);

  //  Parse and test image name.
  int parseName( const char *imagename, char **fullname,
		 char **slice, char **path );

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

  //  Create a basic GRID-Pixel coordinates domain FrameSet.
  AstFrameSet* StarRtdImage::makePixelWCS( ImageData *image = NULL );

  //  Create AST plot for drawing grids and contours to canvas.
  AstPlot* createPlot( AstFrameSet *wcs, AstFrameSet *extraset,
                       int full, int image, double region[] );
 private:

  // Copy constructor -- not defined.
  StarRtdImage( const StarRtdImage&) ;

};


#endif // StarRtdImage
