//+
//  Name:
//     StarRtdImage
//
//  Language:
//     C++
//
//  Purpose:
//     Defines the members of the StarRtdImage class.
//
//  Authors:
//     P.W. Draper (PWD)
//     Allan Brighton, ESO (ALLAN)
//
//  Copyright:
//     Copyright (C) 1997-1999 Central Laboratory of the Research Councils
//
//  History:
//     15-FEB-1996 (PWD):
//        Original version.
//     4-APR-1996 (PWD):
//        Added pix2screen command to convert image coordinates to
//        screen coordinates.
//     30-NOV-1996 (PWD):
//        Added origin command and usingxshm.
//     17-JAN-1996 (PWD):
//        Changed loadImage command to clear image when file is "".
//        Previously this deleted the image too (leaving none).
//     19-AUG-1997 (PWD):
//        Added plotgrid method.
//     28-AUG-1997 (PWD):
//        Converted to work with RTD 2.18.3. The shared memory copies
//        of the headers and data no longer seem to be available.
//     16-SEP-1997 (PWD):
//        Added checks to deal with image/NDF slices.
//     06-OCT-1997 (PWD):
//        Modified to create FitsIO and NDFIO objects directly. This
//        decouples the FitsIO classes and NDFIO classes from
//        ImageData (and ImageIO) which allows us to subclass ImageIO
//        and use the ImageData ABC without modifying the code.
//        The loadImage member function becomes loadFile (which is now
//        virtual) and we control configureImage completely at this
//        level (this is needed to parse the options, plus the
//        ast_tag item that we need). The RtdImage constructor is
//        also changed, as we need to set the image name, the widget
//        options and avoid some pitfalls with the RtdImage
//        constructor calling loadFile (via the initImage member).
//     17-OCT-1997 (PWD):
//        Started changes to support the modification of Astrometry
//        information in the WCS.
//     16-Mar-1998 (ALLAN):
//        Make use of new Skycat/Rtd features supporting subclassing,
//        New constructor args to support subclassing,
//        Changed image type from "starrtdimage" to "rtdimage",
//        Added GAIA_CONFIG macro for Tk config options,
//        Renamed OPTION() macro to GAIA_OPTION (Tk config options),
//
//        Renamed WCS to StarWCS, which is now a subclass of WCSRep,
//        for compatibility with Rtd/Skycat, and added the method
//        "getStarWCSPtr(image)" to access the StarWCS class ptr inside
//        the WCS (reference counted) class.
//
//        Added getStarImage() method, to avoid duplicate code to read an
//        image file and setup the right WCS subclass.
//
//        Added class StarFitsIO to replace GAIA version of FitsIO and
//        remain compatible with the Skycat version.
//     22-APR-1998 (PWD):
//        Now subclasses Skycat rather than RtdImage. Changed
//        draw_ellipse method to use rtd_ellipse.
//     27-APR-1998 (ALLAN):
//        Modified isNDFType() and getStarImage() to check for all FITS
//        suffixes (anything including "fit", such as fits.gz, fits.Z,
//        cfits, gzfits, etc.).
//     23-JUN-1998 (PWD):
//        Added command for retrieving a file from a web server.
//     16-JUL-1998 (PWD):
//        Added slice command. This extends spectrum to also return
//        the X and Y coordinates used as positions along the line.
//        Added command to control the colour of blank pixels.
//     04-NOV-1998 (PWD):
//        Added override for get_compass member. This allows the
//        images that have a WCS component to not use it when scaling
//        and orienting the plotting symbols. This is much faster, and
//        OK when dealing with detections for images that are
//        displayed (i.e. the SExtractor toolbox). Also added -plotwcs
//        configuration option to control this.
//     13-NOV-1998 (PWD):
//        Added rotbox to plotting symbols.
//     13-JAN-1999 (PWD):
//        Merged Allan Brighton's GAIA plugins changes (see history above).
//     06-APR-1999 (PWD):
//        Added contour command. Lots of restructuring of gridplot
//        command.
//     14-JUN-1999 (PWD):
//        Added "hdu" command to stop use of this facility. There is a 
//        fundermental problem with memory mapped FITS files that
//        stops the use of these commands (basically the disk file is
//        not available to add/read HDUs). When time permits this
//        problem should be worked around.
//-

#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <iostream.h>
#include <strstream.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <float.h>
#include "tcl.h"

#include "define.h"
#include "util.h"
#include "ImageIO.h"
#include "StarFitsIO.h"
#include "NDFIO.h"
#include "ImageData.h"
#include "StarRtdImage.h"
#include "Contour.h"
#include "HTTP.h"
#include "ast.h"
#include "grf_tkcan.h"
#include "tcl_err.h"
#include "rtdDtrn.h"

// Include any foreign commands. These are processed by the "foreign"
// member function when requested.
#include "StarRtdForeignCmds.h"

//  Size of buffer used for Tcl commands.
static const eval_buf_size_ = 1024;

//  Trig conversion factors.
static const double pi_ = 3.14159265358979323846;
static const double rad_ = pi_/180.;

//  StarRtdImageSubCmds
//
//  Declare a table of image subcommands and the methods that handle them.
//
//-
static class StarRtdImageSubCmds {
public:
  char const *name;                                  // Method name
  int (StarRtdImage::*fptr)(int argc, char* argv[]); // Ptr to method
  int min_args;                                      // Minimum number of args
  int max_args;                                      // Maximum number of args
} subcmds_[] = {
  { "astassign",     &StarRtdImage::astassignCmd,    7, 7 },
  { "astbootstats",  &StarRtdImage::astbootstatsCmd, 4, 4 },
  { "astcopy",       &StarRtdImage::astcopyCmd,      1, 1 },
  { "astcreate",     &StarRtdImage::astcreateCmd,    0, 0 },
  { "astdelete",     &StarRtdImage::astdeleteCmd,    1, 1 },
  { "astfix",        &StarRtdImage::astfixCmd,       0, 0 },
  { "astget",        &StarRtdImage::astgetCmd,       1, 1 },
  { "astpix2wcs",    &StarRtdImage::astpix2wcsCmd,   2, 2 },
  { "astread",       &StarRtdImage::astreadCmd,      1, 1 },
  { "astrefine",     &StarRtdImage::astrefineCmd,    4, 4 },
  { "astreplace",    &StarRtdImage::astreplaceCmd,   0, 0 },
  { "astreset",      &StarRtdImage::astresetCmd,     1, 1 },
  { "astrestore",    &StarRtdImage::astrestoreCmd,   0, 1 },
  { "aststore",      &StarRtdImage::aststoreCmd,     2, 4 },
  { "astsystem",     &StarRtdImage::astsystemCmd,    2, 2 },
  { "astwcs2pix",    &StarRtdImage::astwcs2pixCmd,   2, 2 },
  { "astwrite",      &StarRtdImage::astwriteCmd,     1, 2 },
  { "blankcolor",    &StarRtdImage::blankcolorCmd,   1, 1 },
  { "contour",       &StarRtdImage::contourCmd,      1, 6 },
  { "dump",          &StarRtdImage::dumpCmd,         1, 2 },
  { "foreign",       &StarRtdImage::foreignCmd,      2, 2 },
  { "hdu",           &StarRtdImage::hduCmd,          0,  6},
  { "origin",        &StarRtdImage::originCmd,       2, 2 },
  { "plotgrid",      &StarRtdImage::plotgridCmd,     0, 2 },
  { "slice",         &StarRtdImage::sliceCmd,       11, 11},
  { "urlget",        &StarRtdImage::urlgetCmd,       1, 1 },
  { "usingxshm",     &StarRtdImage::usingxshmCmd,    0, 0 }
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
  "rtdimage",
  StarRtdImage::CreateImage,
  TkImage::GetImage,
  TkImage::DisplayImage,
  TkImage::FreeImage,
  TkImage::DeleteImage,
  (Tk_ImageType *) NULL       /* nextPtr */
};


//+
// configSpecs_
//
// Image config options - used to process command line options and for the
// image "configure" subcommand. (ALLAN)
//-
static Tk_ConfigSpec configSpecs_[] = {
    GAIA_OPTIONS,		// See Gaia.h: defines option list
    {TK_CONFIG_END,     NULL,           NULL, NULL, NULL, 0,                  0}
};

// From BLT package (now locally because this routine was deleted with blt2.1).
extern "C" int Blt_GraphElement(
    Tcl_Interp *interp,         /* Interpreter of the graph widget */
    char *pathName,             /* Path name of the graph widget */
    char *elemName,             /* Name of the element to reset */
    int numValues,              /* Number of values in array */
    double *valueArr,           /* Array of x,y coordinate pairs */
    char *xVector,              /* Name of x array */
    char *yVector);             /* Name of y array */

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
int StarRtdImage::CreateImage( Tcl_Interp *interp, char *name, int argc,
                               char *argv[], Tk_ImageType *typePtr,
                               Tk_ImageMaster master,
                               ClientData *clientDataPtr )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::CreateImage" << endl;
#endif

  //  Create a configuration options structure (this is passed to
  //  RtdImage and from there to TkImage overriding the options
  //  defined by RtdImage).
  StarRtdImageOptions *options = new StarRtdImageOptions();

  //  Now Create the image.
  StarRtdImage *im = new StarRtdImage( interp, name, argc, argv, master,
				       starRtdImageType.name,
				       configSpecs_, options);
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
StarRtdImage::StarRtdImage(Tcl_Interp* interp, const char* instname, int argc, char** argv,
			   Tk_ImageMaster master, const char* imageType,
			   Tk_ConfigSpec* specs, RtdImageOptions* options)
    : Skycat(interp, instname, argc, argv, master, imageType, specs, options),
    staroptionsPtr_((StarRtdImageOptions*)options),
    origset_(NULL),
    newset_(NULL),
    oldset_(NULL)
{
#ifdef _DEBUG_
  cout << "Created StarRtdImage object " << endl;
#endif

  // Define the TCL interpreter for any AST errors.
  errTcl_Init( interp );

  // Initialise all the Channel slots.
  for ( int i=0; i < MAX_CHANNELS; i++ ) channels_[i] = NULL;
}


//+
//  StarRtdImage::~StarRtdImage
//
//  Destructor
//-
StarRtdImage::~StarRtdImage()
{
#ifdef _DEBUG_
  cout << "Destroying StarRtdImage object " << endl;
#endif
  // XXX Do not delete this as base classes need the reference.
  // if ( staroptionsPtr_ ) {
  //    delete staroptionsPtr_;
  // }

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

int StarRtdImage::call ( const char *name, int len, int argc, char *argv[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::call (" << name  << ")" << endl;
#endif
  for ( unsigned int i = 0; i < sizeof( subcmds_ ) / sizeof( *subcmds_ ); i++ ) {
    StarRtdImageSubCmds *t = &subcmds_[i];
    if ( strncmp( t->name, name, len ) == 0 ) {
      if ( check_args( name, argc, t->min_args, t->max_args ) != TCL_OK ) {
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
// The arguments are the filename and an optional slice string for NDFIO.
// (17.03.98, ALLAN), note all image with slices, must be processed by
// the NDF library.
//-
ImageData* StarRtdImage::getStarImage(const char* filename, const char* slice)
{
  // Check if the file extension is one known to the NDF library
  // (excluding FITS). If not then pass the image to be read using
  // the FitsIO or NDFIO classes as appropriate. These are then passed
  // to ImageData via which any further control is made.
  const char* type = fileSuffix( filename );

  ImageIO imio;

  //  ALLAN: fileSuffix(filename) might return just "Z" or "gz" for
  //  a compressed FITS file.
  char* p = strchr( filename, '.' );
  int isfits = 1;
  if ( p && ! strstr(p, ".fit") ) {
      isfits = 0;
  }

  if ( ( !isfits && isNDFtype( type ) ) || slice ) {
    if ( slice ) {

      //  Construct a complete name, including the slice.
      char *fullname = new char[strlen(filename)+strlen(slice)+1];
      strcpy( fullname, filename );
      strcat( fullname, slice );
      imio = NDFIO::read( fullname, component() );
      delete fullname;
    } else {
      imio = NDFIO::read( filename, component() );
    }
  }  else {
    //  Note: since some Starlink routines access the raw data and
    //  expect it to be already byte-swapped, if needed, we have to
    //  use a special class that makes a memory copy of the image, if
    //  needed.
    imio = StarFitsIO::read( filename, Mem::FILE_PRIVATE | Mem::FILE_RDWR );
  }
  if ( imio.status() != 0 ) {
    return (ImageData *) NULL;
  }

  //  Return the new image.
  return makeImage( imio );
}


//+
// Make a new image from the given ImageIO object and return a pointer to
// a derived class of ImageData specialized in that type of image.
//
// Note that pointers to ImageIORep subclasses, such as FitsIO, StarFitsIO
// and NDFIO are automatically converted to an ImageIO object through a special
// constructor.
//
// This method overrides the parent version to make sure that the
// GAIA features also work here. We need to make sure that the correct
// classes are initialized, in particular, the StarWCS class.
//
// (XXX what about byte swapping for shared memory images (rtdimage mmap
//  and shm commands, real-time interface) ? Do we want to do that here?).
//-
ImageData* StarRtdImage::makeImage(ImageIO imio)
{
  // Make sure that we use the StarWCS class and not the inherited default
  // SAOWCS class
  WCSRep* rep = imio.wcs().rep();
  if (! rep || strcmp(rep->classname(), "StarWCS") != 0) {
      const char* header = "";
      if (! imio.isclear())  // if image is not cleared, set WCS header
	  header = (const char*)imio.header().ptr();
      WCS wcs(new StarWCS(header));
      if (wcs.status() != 0)
	  return (ImageData*)NULL;
      imio.wcs(wcs);  // this sets the WCS object to use for this image
  }

  return ImageData::makeImage(name(), imio, verbose());
}


//+
// Load an image file and display it.
//-
int StarRtdImage::loadFile()
{
  // Used to save and restore image transformation parameters.
  ImageDataParams p;

#ifdef _DEBUG_
  cout << "Called StarRtdImage::loadFile (" << file() << ")" << endl;
#endif

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

  // -file may have been set to "", clear current image and just return.
  if (strlen(file()) == 0) {
    return clearCmd(0, NULL);
  }

  // If we have an existing image then release it.
  if ( image_ ) {
    image_->saveParams(p);
    delete image_;
    image_ = (ImageData *) NULL;
    updateViews();
  }

  // Get a local copy of the image string so that we may modify it as
  // required.
  int namelen = strlen(file()) + 1;
  char *name = new char[namelen];
  char *slice = new char[namelen];
  strcpy( name, file() );

  // Look for a slice at the end of the file name. This needs to be
  // removed while we do other tests.
  char *left = strrchr( name, '(');
  char *right = strrchr( name, ')');
  if ( left && right ) {
    strcpy( slice, left );
    *left = '\0';
  } else {
    delete slice;
    slice = NULL;
  }

  // Check that name is a file.
  struct stat buf;
  if ( stat( name, &buf ) != 0 || S_ISREG( buf.st_mode ) == 0 ) {
    return error( file(), " is not an image" );
  }

  ImageData* image = getStarImage( name, slice );
  delete name;
  delete slice;

  if (! image)
    return TCL_ERROR;
  image_ = image;

  // Restore transformations.
  image_->restoreParams(p, !autoSetCutLevels_);

  return initNewImage();
}

//+
// Return a pointer to the StarWCS object for the image, or NULL on error
//-
StarWCS* StarRtdImage::getStarWCSPtr(ImageData* image)
{
  if (!image) {
    image = image_;
  }
  WCSRep* p = image->wcs().rep();
  if (p && strcmp(p->classname(), "StarWCS") == 0) {
    return (StarWCS*)p;
  }
  error("internal error: expected class StarWCS, not ", p->classname());
  return NULL;
}


//+
// Dump the displayed image to a file. This overrides the method used
// by RtdImage, as we make sure that the current WCS is used as part
// of the output FITS headers (overwriting what exists there
// already) if the original WCS has been overwritten (a sign that at
// least some WCS modification has been attempted).
//
// Two encodings can be written. A native one is always written (as
// this is accurate and can be output to an NDF WCS component) and one
// other (presumably FITS-WCS) which is passed as an optional
// argument. If the selected encoding fails an error message is
// printed, but is not considered fatal (as the native encoding will
// always work).
//-
int StarRtdImage::dumpCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::dumpCmd (" << argv[0] << ")" << endl;
#endif
  int native = 1;
  int saved = 1;
  if ( image_ ) {
    if ( origset_ ) {
      // WCS has been played with. Attempt to write the current WCS
      // out to the FITS headers. We do this by reading the existing
      // FITS headers into a channel, attempting to repeat the process
      // of reading an object (thus removing the associated FITS
      // headers) and then attempt to write the new WCS object, plus
      // a native object (as a backup in case other system cannot be
      // written).
      AstFitsChan *chan = (AstFitsChan *)astFitsChan( NULL, NULL, "" );

      // Now add all the current FITS headers to this channel.
      Mem oldhead = image_->header();
      char *oldptr = (char *) oldhead.ptr();
      int oldsize = oldhead.size();
      char card[81];
      int ncard = oldsize / 80;
      for ( int i = 0 ; i < ncard; i++, oldptr += 80 ) {
        memcpy( card, (void *)oldptr, (size_t) 80 );
        card[80] = '\0';

        //  Read all cards up to, but not including, the END card.
        if ( ! ( card[0] == 'E' && card[1] == 'N' && card[2] == 'D'
                 && ( card[3] == '\0' || card[3] == ' ' ) ) ) {
          astPutFits( chan, card, 0 );
          if ( !astOK ) {

            //  If an error occurs with a card, just continue, it's
            //  almost certainly something trivial like a formatting
            //  problem.
            astClearStatus;
          }
        } else {
          break;
        }
      }

      // Read a WCS system from the channel to simulate the initial
      // read (which removed an object).
      astClear( chan, "Card" );
      AstFrameSet *tmpset = (AstFrameSet *) astRead( chan );
      if ( !astOK ) astClearStatus;
      if ( tmpset != AST__NULL && astIsAFrameSet( tmpset ) ) {
        tmpset = (AstFrameSet *) astAnnul( tmpset );
      }

      // Now we can try to add the WCS object. If we've been given a
      // preferred encoding then use this as well as a native encoding.
      //      astSet( chan, "Encoding=Native" );
      StarWCS* wcsp = getStarWCSPtr();
      if (!wcsp)
	  return TCL_ERROR;
      AstFrameSet *newwcs = wcsp->astWCSClone();
      int nwrite = astWrite( chan, newwcs );
      if ( !astOK ) {
        astClearStatus;
        native = 0;
      } else {
        native = 1;
      }

      // Now try with the given encoding (which shouldn't be native).
      int nextra = 0;
      if ( argc == 2 ) {
	astSet( chan, "Encoding=%s", argv[1] );
	nextra = astWrite( chan, newwcs );
	if ( !astOK || nextra == 0 ) {
	  if ( !astOK ) astClearStatus;
          saved = 0;
	} else {
          saved = 1;
        }
      }
      if ( nwrite != 0 || nextra != 0 ) {

	// Write the FITS channel out to a Mem object and use it to
	// replace the existing headers.
	ncard = astGetI( chan, "Ncard" );
	Mem newhead = Mem( 80 * ( ncard + 1 ), 0 );
	char *newptr = (char *) newhead.ptr();
	astClear( chan, "Card" );
	int i;
	for ( i = 0; i < ncard; i++, newptr += 80 ) {
	  astFindFits( chan, "%f", card, 1 );
	  memcpy( newptr, card, 80 );
	}
	strcpy( newptr, "END" );
	newptr += 3;
	for ( i = 0; i < 77; i++, newptr++ ) *newptr = ' ';
	image_->header( newhead ); // XXX how is old header released?
      }
      chan = (AstFitsChan *) astAnnul( chan );
      newwcs = (AstFrameSet *) astAnnul( newwcs );
    }
    int result = image_->write(argv[0]);
    if ( result == TCL_OK ) {
      if ( ! saved && native ) {
        return error( "information: WCS could only be saved"
                      " as an AST native representation");
      } else if ( !saved && !native ) {
        return error( "Failed to save WCS information");
      }
      return TCL_OK;
    } else {
      return TCL_ERROR;
    }
  } else {
    return TCL_OK;
  }
}

//+
//  StarRtdImage::configureImage
//
// This procedure is called to process an argv/argc list, plus
// the Tk option database, in order to configure (or reconfigure)
// a image.
//
// Redefined here to check which changes were made and then
// pass responsibility on to TkImage. Note that this method
// is a copy of the body of RtdImage::configureImage with our
// modifications. We need this to ensure that the correct
// configSpecs_ are used.
//-
int StarRtdImage::configureImage(int argc, char* argv[], int flags)
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::configureImage" << endl;
#endif
  if (TkImage::configureImage(argc, argv, flags) != TCL_OK)
    return TCL_ERROR;

  int status = TCL_OK;
  int reset = 0;

  // Note if we are using X shared memory.
  usingXShm_ = haveXShm_ && usexshm();

  // Find out which options were specified and process them
  // if necessary (note: Tk sets a flag in the config entry when
  // the option is specified. We use the OFFSET macro defined above
  // as an efficient way to compare options).
  for (Tk_ConfigSpec* p=configSpecs_; p->type != TK_CONFIG_END; p++) {
    if (p->specFlags & TK_CONFIG_OPTION_SPECIFIED) {
      switch(p->offset) {

      case GAIA_OPTION(usexshm):
        if (initialized_) {
          deleteXImage();
          reset++;
        }
        break;

      case GAIA_OPTION(displaymode):
      case GAIA_OPTION(shm_header):
      case GAIA_OPTION(shm_data):
        if (initialized_)
          reset++;
        break;

      case GAIA_OPTION(fitWidth):
      case GAIA_OPTION(fitHeight):
        if (initialized_) {
          if (image_ && fitWidth() && fitHeight()) {
            image_->shrinkToFit(fitWidth(), fitHeight());
          }
          reset++;
        }
        break;

      case GAIA_OPTION(file):
        status = loadFile();
        break;
      }
    }
  }

  if (reset)
    return resetImage();
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
  cout << "Called StarRtdImage::foreignCmd" << endl;
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
  for ( unsigned int i = 0; i < sizeof( foreigncmds_ ) / sizeof( *foreigncmds_)
          ; i++ ) {
    StarRtdForeignCmds *t = &foreigncmds_[i];
    if ( strcmp( t->name, argv[0] ) == 0 ) {
      //  Matched a command so construct the image information and
      // invoke it.
      StarImageInfo *info = new StarImageInfo;
      char *id = image_->image().get("NDFID");
      if ( id ) {
	info->NDFid = atoi( id );
      } else {
	info->NDFid = 0;
      }
      ImageIO imageIO = image_->image();
      info->imageData = (void *) imageIO.dataPtr();
      info->type = (ImageDataType) image_->dataType();
      info->nx = (int) image_->width();
      info->ny = (int) image_->height();
      info->interp = interp_;

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
//      Returns the NDF origin information.
//
//   Arguments:
//       int argc         - Number of arguments passed to command (2).
//       char *argv[]     - The arguments, names of the variables
//                          to hold origin information. If not given
//                          then return is a list.
//    Return:
//       TCL status.
//
//-
int StarRtdImage::originCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::originCmd" << endl;
#endif

  if (!image_) {
    return error("no image loaded");
  }
  char* outx_name = argv[0];
  char* outy_name = argv[1];

  // Set the origin information if available.
  Tcl_ResetResult(interp_);
  char *x = image_->image().get("LBOUND1");
  if ( ! x ) x = "1";
  Tcl_SetVar(interp_, outx_name, x, 0);

  char *y = image_->image().get("LBOUND2");
  if ( ! y ) y = "1";
  Tcl_SetVar(interp_, outy_name, y, 0);
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
  cout << "Called StarRtdImage::usingxshmCmd" << endl;
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
  cout << "Called StarRtdImage::plotgridCmd" << endl;
#endif

  int inerror = 0;
  if (!image_) {
    return error("no image loaded");
  }

  // Check the list of attributes for the special values pixel, epoch,
  // equinox, system, gap(1) and gap(2). This section also does the
  // initial split of the input list. Note we do these always so that
  // we can check for request for a pixel based system. This is always
  // available regardless of the existence of a proper WCS system.
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
      if ( Tcl_SplitList( interp_, argv[0], &listArgc, &listArgv ) != TCL_OK ) {
        error( "sorry: failed to decode plotting options (check format)" );
        inerror = 1;
      } else {
        char *string = NULL;
        for ( int index = 0; index < listArgc; index++ ) {
          string = listArgv[index];

          //  Check for "epoch", "equinox" or "system" and record
          //  the value. Note assumes lowercase and no leading blanks.
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
        if ( Tcl_SplitList( interp_, argv[1], &coordArgc, &coordArgv ) != TCL_OK ) {
          error( "sorry: failed to decode region of image to grid" );
          coordArgc = 0;
          inerror = 1;
        } else {
          if ( coordArgc != 4 ) {
            error( "wrong # of args, should be 4 canvas coordinates" );
            inerror = 1;
          } else {
            for ( int index = 0; index < coordArgc; index++ ) {
              if ( Tcl_GetDouble(interp_, coordArgv[index],
                                 &region[index] ) != TCL_OK ) {
                error( coordArgv[index], "is not a valid number");
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
    wcs = makePixelWCS();
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
    AstSkyFrame *newsky = NULL;
    if ( equinox || epoch || system ) {
      if ( ! showpixels ) {
        newsky = astSkyFrame( "" );
        if ( equinox ) {
          astSet( newsky, equinox );
        }
        if ( epoch ) {
          astSet( newsky, epoch );
        }
        if ( system ) {
          astSet( newsky, system) ;
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
    AstPlot *plot = createPlot( wcs, (AstFrameSet *) newsky,
                                (coordArgc == 0), 0, region );
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
      }
      if ( gap2 != NULL ) {
        sscanf( gap2, "gap(2)=%f", &fact );
        if ( fact < FLT_EPSILON ) fact = 1.0f;
        value = astGetF( plot, "gap(2)" );
        if ( astOK ) {
          value /= fact;
          astSetF( plot, "gap(2)", value );
        }
      }

      // Unset the number of digits used in the plot. These are set
      // elsewhere in GAIA (for WCS decoding) and are not sensible
      // for this occasion (too much precision leads to bad default
      // labelling).
      astClear( plot, "digits(1)" );
      astClear( plot, "digits(2)" );

      // Set all plot attributes to control the plot appearance.
      if ( listArgc > 0 ) {
        char *string = NULL;
        for ( int index = 0; index < listArgc; index++ ) {
          string = listArgv[index];

          //  Check for "epoch", "equinox", "system", "gap(1)" and
          //  "gap(2)" and ignore these.
          if ( string != equinox && string != epoch &&
               string != system && string != gap1 &&
               string != gap2 ) {
            astSet( plot, string );
            if ( !astOK ) {
              (void) error( string, " is not a valid attribute "
                                    "(check format)" );
              inerror = 1;
              break;
            }
          }
        }
      }

      //  Draw the grid.
      if ( astOK && ! inerror ) {
        astGrid( plot );
      }

      //  Free the plot.
      plot = (AstPlot *) astAnnul( plot );

      //  Reset the tag associated with AST grid items.
      astTk_Tag( NULL );
    }

    // Free the list.
    if ( listArgc > 0 ) {
      Tcl_Free( (char *) listArgv );
    }

    // Free the WCS copy.
    wcs = (AstFrameSet *) astAnnul( wcs );
  }

  if ( inerror || ! astOK ) {
    if ( !astOK ) {
      astClearStatus;
    }
    return TCL_ERROR;
  }
  return TCL_OK;
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
  cout << "Called StarRtdImage::astgetCmd" << endl;
#endif
  if (!image_) {
    return error("no image loaded");
  }

  //  Get the value.
  StarWCS* wcsp = getStarWCSPtr();
  if (!wcsp)
      return TCL_ERROR;
  const char *result = wcsp->astGet( argv[0] );
  Tcl_SetResult( interp_, (char *)result, TCL_VOLATILE );
  return TCL_OK;
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
  cout << "Called StarRtdImage::isNDFType" << endl;
#endif

  // If the type is already known do things quickly.
  if ( strstr( type, "fit" ) ) {  // allan: 27.4.98: check all FITS types
      return 0;                   // including fits.gz, gzfits, fit, fits, cfits...
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
//   StarRtdImage::aststoreCmd
//
//   Purpose:
//      Stores a value and keyword in a FITS channel. The channel
//      can later be used to create an AST FrameSet that describes
//      the WCS system. It is the responsibility of the caller to
//      ensure that a FITS channel contains the correct information.
//
//    Return:
//       TCL status and result.
//
//-
int StarRtdImage::aststoreCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::aststoreCmd" << endl;
#endif

  //  Extract the identifier of the FITS channel that we are to use.
  int slot = 0;
  if ( Tcl_GetInt( interp_, argv[0], &slot ) != TCL_OK ) {
    return error( argv[0], " is not an integer");
  } else {

    // Enter the value into the channel.
    slot--;
    storeCard( channels_[slot], argv[1], argv[2], argv[3] );
    if ( astOK ) {
      return TCL_OK;
    }
    astClearStatus;
    return more_error("failed to enter card into FITS channel");
  }
}

//+
//   StarRtdImage::storeCard
//
//   Purpose:
//      Creates and stores a FITS card in a FITS channel given the
//      keyword name, value and comment.
//-
void StarRtdImage::storeCard( AstFitsChan *channel, const char *keyword,
                              const char *value, const char *comment, 
                              int overwrite )
{
  char card[80];
  const char *dummy = "No comment";
  if ( comment == NULL ) comment = dummy;
  if ( strlen(value) > 21 ) {
    sprintf( card, "%-8.8s=%s /%s", keyword, value, comment );
  } else {
    sprintf( card, "%-8.8s=%21.21s /%s", keyword, value, comment );
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
  cout << "Called StarRtdImage::astdeleteCmd" << endl;
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
  cout << "Called StarRtdImage::astcreateCmd" << endl;
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
  char buffer[80];
  channels_[slot] = astFitsChan( NULL, NULL, "" );
  storeCard( channels_[slot], "NAXIS", "2", "Number of axes" );
  sprintf( buffer, "%d", image_->width() );
  storeCard( channels_[slot], "NAXIS1", buffer, "Dimension 1" );
  sprintf( buffer, "%d", image_->height() );
  storeCard( channels_[slot], "NAXIS2", buffer, "Dimension 2" );
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
  cout << "Called StarRtdImage::astreadCmd" << endl;
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
//     Replaces the existing WCS AstFrameSet with a new version that
//     has been created by other members in this class
//     (i.e. astread or astrefine). A clone of the existing WCS system
//     is retained so that the changes can be undone (at least until
//     up to the next invocation of astread or astrefine) using
//     astrestore.
//
//     If not already taken a clone of what is reckoned to
//     be the original WCS associated with the image is also
//     made. This can be restored using the "astrestore original"
//     command.
//-
int StarRtdImage::astreplaceCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::astreplaceCmd" << endl;
#endif
  if (!image_) {
    return error("no image loaded");
  }
  if (!newset_) {
    return error("no new WCS system is available");
  }

  // get a pointer to the internal Starlink version of the WCS class
  StarWCS* wcsp = getStarWCSPtr();
  if (!wcsp)
      return TCL_ERROR;

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
  if (updateViews(2) != TCL_OK) {
    return TCL_ERROR;
  } else {
    return TCL_OK;
  }
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
  cout << "Called StarRtdImage::astrestoreCmd" << endl;
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
	if (updateViews(2) != TCL_OK) {
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

      //  Restore the old WCS FrameSet. Annul the pointer to register that
      //  we're no longer interested.
      wcsp->astWCSReplace( oldset_ );
      oldset_ = (AstFrameSet *) astAnnul( oldset_ );

      //  Update any views to use this information.
      if (updateViews(2) != TCL_OK)
	return TCL_ERROR;
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
  cout << "Called StarRtdImage::astrefineCmd" << endl;
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
  cout << "Called StarRtdImage::mapPositions" << endl;
#endif
  double tr[6];

  //  Determine the linear transformation between the data points. if
  //  this fails then return an error.
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
  cout << "Called StarRtdImage::addLinear" << endl;
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
    AstMapping *matrixmap = (AstMapping *) astMatrixMap( 2, 2, 0, matrix, "");
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
  AstMapping *winmap = (AstMapping *) astWinMap( 2, ina, inb, outa, outb, "" );

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
//     Adds a linear transform to a WCS. The linear
//     transform is given as a set of 6 coefficients. The source of
//     the WCS is given as the first parameter and should be either
//     local or image as in astrefine.
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
//      image  WCS system out to terminal using the given encoding,
//      plus a FITS channel if requested.
//
//   Arguments:
//      Either 'image' or 'local' possibly followed by an encoding.
//-
extern "C" {
  static void write_out( const char *card )
  {
    cout << card << endl;
  }
}

int StarRtdImage::astwriteCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::astwriteCmd" << endl;
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
    astAnnul( astPtr );
    if ( argc == 2 ) {
      AstFitsChan *chan = (AstFitsChan *)
        astFitsChan( NULL, &write_out, "Encoding=%s", argv[1] );
      int nwrite = astWrite( chan, newset_ );
      if ( !astOK || nwrite == 0 ) {
        cerr << "Failed to write object via FITS channel" << endl;
      }
      chan = (AstFitsChan *) astAnnul( chan );

      //  If requested also show the contents of a channel.
      if ( argc == 3 ) {
        int slot = 0;
        if ( Tcl_GetInt( interp_, argv[2], &slot ) != TCL_OK ) {
          cout << argv[1] << " is not an integer" << endl;
        } else {
          slot--;
          astShow( channels_[slot] ) ;
        }
      }
    }
  } else {
    cout << "Sorry no FrameSets are available" << endl;
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
  cout << "Called StarRtdImage::astcopyCmd" << endl;
#endif
  if (!image_) {
    return error("no image loaded");
  }

  // Get a local copy of the image string so that we may modify it as
  // required.
  int namelen = strlen(argv[0]) + 1;
  char *name = new char[namelen];
  char *slice = new char[namelen];
  strcpy( name, argv[0] );

  // Look for a slice at the end of the file name. This needs to be
  // removed while we do other tests.
  char *left = strrchr( name, '(');
  char *right = strrchr( name, ')');
  if ( left && right ) {
    strcpy( slice, left );
    *left = '\0';
  } else {
    delete slice;
    slice = NULL;
  }

  // Check that name is a file.
  struct stat buf;
  if ( stat(name, &buf) != 0 || S_ISREG(buf.st_mode) == 0 ) {
    return error( argv[0], " is not an image" );
  }

  // Check if the file extension is one known to the NDF library
  // (excluding FITS). If not then pass the image to be read using
  // the FitsIO or NDFIO classes as appropriate. These are then passed
  // to ImageData via which any further control is made.
  //
  // XXX this is a very inefficient way to get at WCS information, but
  // at least it's general.
  ImageData* newimage = getStarImage(name, slice);
  delete name;
  delete slice;
  if ( ! newimage ) {
    return TCL_ERROR;
  }

  // Ok managed to read the file. Now get a copy of the WCS that comes
  // with it.
  if ( newset_ ) newset_ = (AstFrameSet *) astAnnul( newset_ );

  StarWCS* wcsp = getStarWCSPtr(newimage);
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
  cout << "Called StarRtdImage::astfixCmd" << endl;
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
  cout << "Called StarRtdImage::astwcs2pixCmd" << endl;
#endif

  // Convert input position, which may be in H:M:S format, to degrees.
  // (note values may only be in HH:MM:SS/DD:MM:SS or DD.ddd/DD.ddd
  //  no HH.hh, 2000.0 mean values are not converted to another equinox)
  WorldCoords wcs( argv[0], argv[1], 2000.0, 1);
  if ( wcs.status() != TCL_OK ) {
    return TCL_ERROR;
  }
  double x = wcs.ra_deg();
  double y = wcs.dec_deg();

  //  Convert from RA/Dec to x,y.
  worldToImageCoords(x, y, 0);

  //  And return the values as the result of this command.
  Tcl_ResetResult( interp_ );
  char *buffer = Tcl_Alloc( TCL_DOUBLE_SPACE * 2 + 2 );
  sprintf( buffer, "%g %g", x, y );
  Tcl_SetResult( interp_, buffer, TCL_DYNAMIC );
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
//     X and Y position
//
//  Result:
//     RA and Dec in degrees (plus trailing equinox).
//
//  Notes:
//     This provides some of the functionality of the convert command,
//     but avoids any issues to do with equinoxes, celestial
//     coordinates systems (FK5/FK4 etc.) and epochs.
//-
int StarRtdImage::astpix2wcsCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::astpix2wcsCmd" << endl;
#endif

  //  Extract the input X and Y positions.
  double x;
  double y;
  if (Tcl_GetDouble( interp_, argv[0], &x ) != TCL_OK
      || Tcl_GetDouble( interp_, argv[1], &y ) != TCL_OK) {
    return TCL_ERROR;
  }

  //  Convert to a RA/Dec in degrees (plus an unwanted equinox, which
  //  is left in place for convenience).
  char buffer[80];
  image_->wcs().pix2wcs( x, y, buffer, 80, 0);

  //  Set the result and return.
  Tcl_SetResult( interp_, buffer, TCL_VOLATILE );
  return TCL_OK;
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
  cout << "Called StarRtdImage::decodeLinear" << endl;
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
  perp = p * R2D;
  orient = wor * R2D;
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
  cout << "Called StarRtdImage::astbootstatsCmd" << endl;
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
  Tcl_Free( (char *) listArgv1 );
  Tcl_Free( (char *) listArgv2 );
  return TCL_OK;
}

//+
//   StarRtdImage::astsystemCmd
//
//   Purpose:
//       Creates a WCS system with new celestial coordinates.
//
//    Notes:
//       The system attributes are passed in the argv[1] element
//       in a pre-formatted manner (such as can be used by as astSet
//       routine, i.e. system=newsystem epoch=epoch equinox=equinox).
//       The first value is the source of the WCS to modify and
//       should be image or local (as in astassign and astrefine).
//-
int StarRtdImage::astsystemCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::astsystemCmd" << endl;
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
      if (!wcsp)
	  return TCL_ERROR;

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

  // Ok now create a new SkyFrame with the options we have been given.
  AstSkyFrame *newsky = astSkyFrame( argv[1] );
  if ( !astOK ) {

    // If any of the above failed, then report the error.
    return error ( "failed to establish new system coordinates system");
  } else {

    // Get a mapping to convert to the new system and add this
    // to the current frameset. Note we convert from BASE frame to
    // the new skyframe to force AST to retain all the current
    // frameset mappings.
    astSetI( newset_, "Current", AST__BASE );
    AstFrameSet *cvt = (AstFrameSet *) astConvert( newset_, newsky, "" );
    newsky = (AstSkyFrame *) astAnnul( newsky );
    if ( astOK ) {
      newset_ = (AstFrameSet *) astAnnul( newset_ );
      newset_ = cvt;
    } else {
      cvt = (AstFrameSet *) astAnnul( cvt );
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
//  "diamond", "ellipse", "compass", "line", "arrow" and "rotbox".
//
//  x and y are the coordinates "xy_units", which is one of the units
//  accepted by the Rtd commands (canvas, image, screen, "wcs $equinox",
//  "deg $equinox").
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
//  Override Skycat equivalent to add rtd_ellipse and rtd_rotbox.
//
int StarRtdImage::draw_symbol(const char *shape,
                              double x, double y, const char *xy_units,
                              double radius, const char *radius_units,
                              const char *bg, const char *fg,
                              const char *symbol_tags,
                              double ratio, double angle,
                              const char *label, const char *label_tags)
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::draw_symbol" << endl;
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
    {"circle", &Skycat::draw_circle},
    {"square", &Skycat::draw_square},
    {"plus", &Skycat::draw_plus},
    {"cross", &Skycat::draw_cross},
    {"triangle", &Skycat::draw_triangle},
    {"diamond", &Skycat::draw_diamond},
    {"ellipse", &StarRtdImage::draw_ellipse},
    {"rotbox", &StarRtdImage::draw_rotbox},
    {"compass", &Skycat::draw_compass},
    {"line", &Skycat::draw_line},
    {"arrow", &Skycat::draw_arrow}
  };
  static int nsymbols = sizeof(symbols)/sizeof(SymbolTab);

  // symbol shape
  for (int i = 0; i < nsymbols; i++) {
    if (strcmp(shape, symbols[i].name) == 0) {
      return (this->*symbols[i].fptr)(x, y, xy_units, radius, radius_units,
                                      bg, fg, symbol_tags, ratio, angle,
                                      label, label_tags);
    }
  }
  return error("invalid plot symbol");
}

//
//  Draw an ellipse at the given coords.
//  See draw_symbol for a description of the arguments.
//
//  This method overrides Skycat version to use local rtd_ellipse,
//  rather than a smoother polygon.
//
int StarRtdImage::draw_ellipse(double x, double y, const char *xy_units,
                               double radius, const char *radius_units,
                               const char *bg, const char *fg,
                               const char *symbol_tags, double ratio,
                               double angle, const char *label,
                               const char *label_tags)
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::draw_ellipse" << endl;
#endif
  double cx, cy, nx, ny, ex, ey;
  if (get_compass(x, y, xy_units, radius, radius_units, ratio, angle,
                  cx, cy, nx, ny, ex, ey) != TCL_OK) {
    reset_result(); // ignore off scale symbols
    return TCL_OK;
  }


  // if using 2 colors, draw 2 symbols, for visibility, one thicker
  char buf[eval_buf_size_*4];
  ostrstream os(buf, sizeof(buf));
  if (strcmp(fg, bg) != 0) {
    os << canvasName_ << " create rtd_ellipse "
       << cx << " " << cy << " "
       << ex << " " << ey << " "
       << nx << " " << ny
       << " -outline " << bg
       << " -fill " << bg
       << " -width 2 -stipple pat7 -tags {" << symbol_tags << "}"
       << endl;
  }
  os << canvasName_ << " create rtd_ellipse "
     << cx << " " << cy << " "
     << ex << " " << ey << " "
     << nx << " " << ny
     << " -outline " << bg
     << " -fill " << fg
     << " -width 1 -stipple pat7 -tags {" << symbol_tags << "}"
     << endl;

  if (label && strlen(label))
    make_label(os, label, cx, cy, label_tags, fg);

  os << ends;
  return eval(os.str());
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
  cout << "Called StarRtdImage::draw_rotbox" << endl;
#endif
  double cx, cy, nx, ny, ex, ey;
  if (get_compass(x, y, xy_units, radius, radius_units, ratio, angle,
                  cx, cy, nx, ny, ex, ey) != TCL_OK) {
    reset_result(); // ignore off scale symbols
    return TCL_OK;
  }

  // if using 2 colors, draw 2 symbols, for visibility, one thicker
  char buf[eval_buf_size_*4];
  ostrstream os(buf, sizeof(buf));
  if (strcmp(fg, bg) != 0) {
    os << canvasName_ << " create rtd_rotbox "
       << cx << " " << cy << " "
       << ex << " " << ey << " "
       << nx << " " << ny
       << " -outline " << bg
       << " -fill " << bg
       << " -width 2 -stipple pat7 -tags {" << symbol_tags << "}"
       << endl;
  }
  os << canvasName_ << " create rtd_rotbox "
     << cx << " " << cy << " "
     << ex << " " << ey << " "
     << nx << " " << ny
     << " -outline " << bg
     << " -fill " << fg
     << " -width 1 -stipple pat7 -tags {" << symbol_tags <<  "}"
     << endl;

  if (label && strlen(label))
    make_label(os, label, cx, cy, label_tags, fg);

  os << ends;
  return eval(os.str());
}

//+
//   StarRtdImage::urlgetCmd
//
//   Purpose:
//       Gets a file from a specified URL and returns it as
//       a result.
//
//    Notes:
//       This command is provided to allow access to the HTTP class
//       (which for instance supports proxies), which does not have a
//       Tcl interface.
//-
int StarRtdImage::urlgetCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::urlgetCmd (" << argv[0] << ")" << endl;
#endif

  //  Create a HTTP object to do the transaction.
  HTTP http;

  //  Now get the file.
  int nlines = 0;
  char *result = http.get( argv[0], nlines, 1 );

  //  And return its content.
  if ( result != NULL ) {
    set_result( result );
    return TCL_OK;
  } else {
    return TCL_ERROR;
  }
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
  cout << "Called StarRtdImage::blankcolorCmd (" << argv[0] << ")" << endl;
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
  cout << "Called StarRtdImage::sliceCmd (" << argc << ")" << endl;
#endif
  if (!image_)
    return TCL_OK;

  //  Convert line extent to image coords.
  double rx0, ry0, rx1, ry1;
  if (convertCoordsStr(0, argv[2], argv[3], NULL, NULL,
                       rx0, ry0, argv[6], "image") != TCL_OK
      || convertCoordsStr(0, argv[4], argv[5], NULL, NULL,
                          rx1, ry1, argv[6], "image") != TCL_OK) {
    return TCL_ERROR;
  }

  //  Get distance between endpoints (add a little to be safe).
  int x0 = int(rx0), y0 = int(ry0), x1 = int(rx1), y1 = int(ry1);
  int w = abs(x1-x0) + 1;
  int h = abs(y1-y0) + 1;
  int dist = (int)sqrt(w*w + h*h) + 4;

  double* ivvalues = new double[dist*2];
  double* xyvalues = new double[dist*2];

  // Fill the xyvalues array and set numValues to the actual number of points
  int numValues = image_->getSpectrum(ivvalues, x0, y0, x1, y1);

  //  Convert the slice indices to the equivalent X and Y coordinates.
  //  To do this we reproduce the algorithm of image->getSpectrum.
  int i = 0;
  if (y1 == y0) {
    // horizontal line
    int startx = min(x0, x1);
    int endx = max(x0, x1);
    for (int x = startx; x <= endx; x++) {
      xyvalues[i*2] = x;
      xyvalues[i*2+1] = y0;
      i++;
    }
  } else if (x1 == x0) {
    // vertical line
    int starty = min(y0, y1);
    int endy = max(y0, y1);
    for (int y = starty; y <= endy; y++) {
      xyvalues[i*2] = x0;
      xyvalues[i*2+1] = y;
      i++;
    }
  } else {
    // sloped line
    // use Bresenham midpoint line scan-conversion algorithm
    // see: Computer Graphics Princ. a. Pract., 2nd Ed., p. 78
    // also see x11r5/mit/server/ddx/cfb/cfbline.c, cfbbres.c

    int x = x0;
    int y = y0;
    int e, e1, e2, e3;		// bresenham error and increments
    int len;			// length of segment
    int adx = x1 - x0;		// abs values of dx and dy
    int ady = y1 - y0;
    int signdx = 1;		// sign of dx and dy
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

  //  Convert the index/value and x/y pairs into Blyt vectors.
  if (Blt_GraphElement(interp_, argv[0], argv[1], numValues*2,
                       ivvalues, argv[7], argv[8]) != TCL_OK) {
    delete xyvalues;
    delete ivvalues;
    return TCL_ERROR;
  }
  if (Blt_GraphElement(interp_, argv[0], argv[1], numValues*2,
                       xyvalues, argv[9], argv[10]) != TCL_OK) {
    delete xyvalues;
    delete ivvalues;
    return TCL_ERROR;
  }
  delete xyvalues;
  delete ivvalues;

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
int StarRtdImage::get_compass(double x, double y, const char* xy_units,
                              double radius, const char* radius_units,
                              double ratio, double angle,
                              double& cx, double& cy, double& nx, double& ny,
                              double& ex, double& ey)
{
  double rx = radius, ry = radius;
  cx = x;
  cy = y;

  if ( isWcs() && plot_wcs() ) {

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
    //  Not using world coords, go right to canvas coords.
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
//	   { {colour(curve)=2,width(curve)=0.015}
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
//-
int StarRtdImage::contourCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::contourCmd" << endl;
#endif

  int inerror = 0;
  if ( !image_ ) {
    return error("no image loaded");
  }

  // Do the initial split of the input lists.
  char **levelsArgv;
  int nlevels = 0;
  double *levels;
  if ( argc > 6 || argc == 0 ) {
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
  if ( argc == 6 ) {
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
    wcs = makePixelWCS();
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
      farwcs = rtdimage->makePixelWCS();
    }
  }
  if ( ! inerror && wcs != (AstFrameSet *) NULL ) {

    //  Current frame is the GRID coordinates of the image to be
    //  contoured.
    astSetI( wcs, "Current", AST__BASE );

    //  Create an AstPlot that incorporates an additional FrameSet
    //  that describes an system we want to add.
    AstPlot *plot = createPlot( wcs, (AstFrameSet *) farwcs, 1, 1, region );
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
                       (const char **)prefs, nprefs );

      //  Establish if plotting is careful or fast.
      contour.setCareful( careful );

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
//   StarRtdImage::makePixelWCS
//
//   Purpose:
//       Create a pseudo WCS that describes the GRID coordinates of
//       an image.
//
//    Return:
//       Basic AstFrameSet.
//
//    Notes:
//       If the image is derived from an NDF then a pixel coordinates
//       Frame will be added.
//
//-
AstFrameSet* StarRtdImage::makePixelWCS( ImageData *image )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::makePixelWCS" << endl;
#endif

  //  If no image is given then use the default.
  if ( ! image ) {
    image = image_;
  }

  //  Create the GRID domain Frame.
  AstFrame *grid = astFrame( 2, "Domain=GRID,Title=Grid Coordinates" );

  //  Create the FrameSet, adding the GRID domain.
  AstFrameSet *set = astFrameSet( grid, "" );
  grid = (AstFrame *) astAnnul( grid );

  //  Need to add a pixel coordinates based Frame.
  AstFrame *coordfrm  = astFrame( 2, "Domain=PIXEL,Title=Pixel Coordinates" );
  double ina[2], inb[2], outa[2], outb[2];
  double width = image_->width();
  double height = image_->height();

  //  Define limits of window in pixel indices.
  ina[0] = ina[1] = 1.0;
  inb[0] = width;
  inb[1] = height;

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
  AstMapping *pixmap = (AstMapping *)
                           astWinMap( 2, ina, inb, outa, outb,"");

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
//    Return:
//       An AstPlot, or NULL if failed.
//
//
//-
AstPlot* StarRtdImage::createPlot( AstFrameSet *wcs,
                                   AstFrameSet *extraset,
                                   int full, int image,
                                   double region[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::createPlot" << endl;
#endif
  //  Define the limits of the canvas plotting "area" in canvas
  //  coordinates and current coordinates (the same positions). Use
  //  the image position on the canvas for this. Note that this also
  //  reorients the image to the canvas (so any scales, flips, offset
  //  and interchange are accounted).
  float gbox[4];
  double pbox[4];
  if ( full ) {

    //  Using whole of canvas/image.
    double rw = reqWidth_, rh = reqHeight_;
    if ( rw == 0 ) rw = (double) image_->width();  // Zero when whole image is
    if ( rh == 0 ) rh = (double) image_->height(); // displayed.
    doTrans( rw, rh, 1 );
    gbox[0] = pbox[0] = 0.0;
    gbox[1] = pbox[1] = rh;
    gbox[2] = pbox[2] = rw;
    gbox[3] = pbox[3] = 0.0;
  } else {

    //  Just using part of the canvas.
    gbox[0] = pbox[0] = region[0];
    gbox[1] = pbox[1] = region[1];
    gbox[2] = pbox[2] = region[2];
    gbox[3] = pbox[3] = region[3];
  }
  int rotated = image_->rotate();             // Record if rotated
  if ( rotated ) {                            // and switch off while
    image_->rotate(0);                        // getting graphics mapping
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
    plotset = (AstFrameSet *) astConvert( wcs, extraset, "SKY,AXIS,PIXEL,GRID,," );
    astInvert( extraset );
    if ( ! astOK || plotset == (AstFrameSet *) NULL ) {
      more_error ( "sorry: cannot find a way to convert your existing "
                   "coordinates to the requested system");
      if ( plotset != (AstFrameSet *) NULL ) {
        plotset = (AstFrameSet *) astAnnul( plotset );
      }
      return (AstPlot *) NULL;
    }

    //  Transform region from old image to new image if asked.
    if ( image ) {
      double xin[2], yin[2];
      double xout[2], yout[2];
      xin[0] = region[0];
      yin[0] = region[1];
      xin[1] = region[2];
      yin[1] = region[3];
      astTran2( plotset, 2, xin, yin, 1, xout, yout );
      region[0] = xout[0];
      region[1] = yout[0];
      region[2] = xout[1];
      region[3] = yout[1];
    }
  } else {

    //  Just using plain WCS.
    plotset = wcs;
  }

  //  Create the plot frameset.
  AstPlot *plot = astPlot( plotset, gbox, pbox, "" );
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
                                   (double *) NULL, "" );
    astRemapFrame( plot, AST__BASE, perm );
    perm = (AstPermMap *) astAnnul( perm );
  }
  if ( astOK ) {
    return plot;
  } else {
    return (AstPlot *) NULL;
  }
}

//+
//   StarRtdImage::hduCmd
//
//   Purpose:
//      Overrides the "hdu" command of Skycat. None of this
//      functionality is implemented in GAIA, and at present cannot be 
//      (because we byte swap FITS files these are memory mapped and
//      not available for access by the FitsIO library).
//
//-
int StarRtdImage::hduCmd( int argc, char *argv[] )
{
#ifdef _DEBUG_
  cout << "Called StarRtdImage::hduCmd" << endl;
#endif
  return error( "Sorry the HDU commands are not available" );
}
