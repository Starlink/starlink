/* Name:
      stcschan-demo3.c

   Purpose:
      A demonstration of the facilities provided by the AST library 
      for reading STC metadata encoded using the STC-S linear string 
      format.

   Description:
      This program reads an STC-S description from a text file, and also
      reads a set of 2-D spatial FITS-WCS headers from another (text) file. 
      It then opens a specified graphics device, and displays an annotated 
      coordinate grid covering the region described by the FITS headers.
      Finally, it draws the outline of the spatial extent of the STC-S 
      description over the top of the annotated coordinate grid.

   Usage:
      % stcschan-demo3 <stcs-file> <header-file> <device>

      <stcs-file>: The path to a text file containing the STC-S
      description.

      <header-file>: The path to a text file containing a set of FITS-WCS
      headers.

      <device>: The name of an available PGPLOT graphics device. If not
      supplied, the available device names will be listed and the program
      will then exit.

   Example:
      % stcschan-demo3 m31.stcs andromeda.head /xserve

   To compile and link:
      Assuming your starlink distribution is in "/star":

      % gcc -o stcschan-demo3 stcschan-demo3.c -g -L/star/lib \
             -I/star/include `ast_link -pgplot` -lcpgplot

*/

/* Include system headers. */
#include <stdio.h>
#include <string.h>

/* Include the PGPLOT header. */
#include "cpgplot.h"

/* Include the AST library header. */
#include "ast.h"

/* Maximum allowed length for a single line of text from the disk file. */
#define MAX_LINE_LEN 100

/* Prototypes */
const char *source( void );
AstFrameSet *ReadFitsHeaders( const char *, int *, int * );
AstRegion *ReadStcs( const char * );


int main( int argc, char **argv ){

/* Local variables: */
   AstBox *pixbox;   
   AstFrame *pixfrm;
   AstFrameSet *align_fs = NULL;
   AstFrameSet *fset = NULL;
   AstPlot *plot;
   AstRegion *reg = NULL;
   AstRegion *wcsbox = NULL;
   AstRegion *wcsreg = NULL;
   const char *dev;
   double bbox[ 4 ];
   float gbox[ 4 ];
   int overlap_flag, status, naxis1, naxis2, ibase;
   
/* Initialise the returned system status to indicate failure. */
   status = 1;

/* Start an AST object context. This means we do not need to annull 
   each AST Object individually. Instead, all Objects created within 
   this context will be annulled automatically by the corresponding
   invocation of astEnd. */
   astBegin;

/* Check there are enough command line arguments. */
   if( argc < 3 ) {
      printf( "Usage: stcschan-demo3 <stcs-file> <header-file> <device>\n" );

/* If so, attempt to read the STC-S description, creating a corresponding 
   AST Region. If this is successful, attempt to read the FITS header file 
   and create an equivalent AST FrameSet. */
   } else {
      reg = ReadStcs( argv[ 1 ] );
      if( reg ) fset = ReadFitsHeaders( argv[ 2 ], &naxis1, &naxis2 );
   }

/* Check we obtained a Region and a FrameSet successfully. */
   if( reg && fset ){

/* Check that we can align the FITS WCS with the spatial axes in the
   STC-S description. AST contains various built-in conversions between 
   standard celestial coordinate system. The necessary conversion will be
   identified and used automatically if necessary to achieve alignment. 
   The returned object "align_fs" is a FrameSet that encapsulates the
   the FITS WCS coordinate system, the STC-S spatial coordinate system and
   the Mapping between them. A NULL pointer is returned if alignment is
   not possible. Note, the astConvert method changes the base Frame in
   any supplied FrameSet to indicate which coordinate frame was used for
   alignment. So we first note the original base Frame index and then
   re-instate it afterwards. */
      ibase = astGetI( fset, "Base" );
      align_fs = astConvert( reg, fset, " " ); 
      astSetI( fset, "Base", ibase );

      if( !align_fs ) {
         printf( "Could not align the FITS WCS with the spatial axes "
                 "in the STC-S\n" );

/* If alignment was possible, use the alignment FrameSet to create a new 
   Region representing the same region as the supplied STC-S, but 
   expressed in the coordinate system of the FITS WCS. The FrameSet class
   inherits from both Frame and Mapping, and so the "align_fs" FrameSet can 
   be used both as the Mapping in this call, and as the Frame. When used
   as a Mapping, a FrameSet represents the transformation between its base
   and current Frame. When used as a Frame, a FrameSet represents its
   current Frame. */
      } else {
         wcsreg = astMapRegion( reg, align_fs, align_fs );

/* It would be nice to warn the user if the STC-S AstroCoordsArea does
   not overlap the FITS grid. To do so we need a Region representing the
   FITS grid. Create one now (an AST Box). First store the bounds of the 
   FITS grid, in pixel coordinates (i.e. a system in which the centre of 
   the bottom left pixel is at (1.0,1.0) ). */
         bbox[ 0 ] = 0.5;
         bbox[ 1 ] = 0.5;
         bbox[ 2 ] = (double) naxis1 + 0.5;
         bbox[ 3 ] = (double) naxis2 + 0.5;

/* Get a pointer to the Frame describing the FITS pixel coordinate system
   (the base Frame in the FITS FrameSet). */
         pixfrm = astGetFrame( fset, AST__BASE );

/* Create a Box that encompasses the required range of axis values within
   the pixel coordinate Frame. */
         pixbox = astBox( pixfrm, 1, bbox, bbox + 2, NULL, " " );

/* Create another Region that represents the same area, but in the FITS
   WCS. */
         wcsbox = astMapRegion( pixbox, fset, fset );

/* If the previous step failed, it probably means the FITS header covers
   the entire sky, resulting the corners of the pixel grid having invalid
   sky positions. So cancel the error and omit the overlap test. */
         if( !wcsbox ) {
            astClearStatus;
            printf("\nContinuing, but omitting overlap test...\n\n");
                
/* Now see if the Region representing the FITS grid overlaps the region 
   read from the STC-S description.*/
         } else {
            overlap_flag = astOverlap( wcsreg, wcsbox );

            if( overlap_flag == 1 || overlap_flag == 6 ) {
               printf( "\nThere is no overlap between the FITS grid and the "
                       "STC-S AstroCoordsArea\n\n" );
   
            } else if( overlap_flag == 3 || overlap_flag == 5 ) {
               printf( "\nThe FITS grid is completely contained within the "
                       "STC-S AstroCoordsArea\n\n" );
            }

         }
      }
   }

/* Check we obtained a FITS-WCS to STC-S Mapping, and that no error has
   occurred in AST. */
   if( wcsreg && astOK ){

/* Open PGPLOT using the specified device. Prompt the user for a device if
   none was supplied on the command line. */
      if( argc < 4 ) {
         dev = "?";
      } else {
         dev = argv[ 3 ];
      }
      if( cpgbeg( 0, dev, 1, 1 ) == 1 ) {

/* Clear the screen. */
         cpgpage();

/* Ensure the graphics window has equal scales on both axes. */
         cpgwnad( 0.0f, 1.0f, 0.0f, 1.0f );

/* Find the extent of the graphics window, and store in an array suitable
   for passing to the astPlot function. */
         cpgqwin( gbox, gbox + 2, gbox + 1, gbox + 3 );

/* Create an AST Plot. This is a special sort of FrameSet in which the
   base Frame corresponds to graphics coordinates. All the coordinate
   Frames and Mappings read from the FITS-WCS headers are added into the
   Plot so that graphics can be drawn in any coordinate system. The extent 
   of the FITS array in pixel coordinates is mapped onto the extent of the 
   graphics device as returned above by cpgqwin. The AST library comes with
   a driver module that provides primitive drawing functions by calling
   appropriate PGFPLOT functions. It is simple to write driver modules
   for other graphics systems such as Tcl/Tk, Java/Swing, etc. Set a few
   graphics attributes to show the sort of thing that can be done. */
         plot = astPlot( fset, gbox, bbox, "Colour(border)=2,Colour(ticks)=2,"
                                       "Colour(axes)=2,Grid=1,Colour(grid)=3,"
                                       "Style(grid)=4" );

/* Draw a set of annotated coordinate axes labelling the FITS WCS axes. */
         astGrid( plot );

/* If there is any overlap (or if the overlap test could not be performed), 
   add the STC-S Region into the Plot. We use a UnitMap to connect it to 
   the current Frame (the FITS WCS frame). */
         if( 1 || overlap_flag == 2 || overlap_flag == 4 || !wcsbox ) {
            astAddFrame( plot, AST__CURRENT, astUnitMap( 2, " " ), wcsreg );

/* Now draw the border round the STC-S Region. A Region is a sub-class of
   Mapping and so can be used to transform positions. When a Region is used 
   as a Mapping, positions that are inside the Region are left unchanged
   by the transformation, and positions that are outside the Region are
   transformed into "bad" positions (i.e. every axis value has the nmagic
   value AST__BAD indicating that the axis value is undefined). The
   astBorder method is a generic function that will outline the areas
   within the current coordinate Frame of the Plot that correspond to 
   valid (i.e. non-bad) positions. Set the colour and line thickness first
   to emphasise the border. */
            astSet( plot, "Colour(border)=4,Width(border)=8" );
            (void) astBorder( plot );
         }

/* Set the returned system status to indicate success. */
         status = 0;

/* Close down PGPLOT. */
         cpgend();
      }
   }

/* End the AST Object context. All Objects created since the
   corresponding invocation of astbegin will be annulled automatically. */
   astEnd;

/* If an error occurred in the AST library, set the returned system
   status non-zero. */
   if( !astOK ) status = 1;
   return status;
}







/* -------------------------------------------------------------------
 * This is a function that reads a line of text from the disk file and
 * returns it to the AST library. It is called from within the astRead
 * function. 
*/

const char *source( void ){
   static char buffer[ MAX_LINE_LEN + 2 ];
   FILE *fd = astChannelData;
   return fgets( buffer, MAX_LINE_LEN + 2, fd );
}



/* -------------------------------------------------------------------
 * This function reads a set of FITS-WCS headers from a given text file,
 * and attempts to convert them into an AST FrameSet. If successful, a
 * pointer to the FrameSet is returned. A NULL pointer is returned if
 * anything goes wrong, or if the WCS is not 2-dimensional. The values of 
 * the NAXIS1 and NAXIS2 headers are returned in "*naxis1" and "*naxis2". 
*/

AstFrameSet *ReadFitsHeaders( const char *file, int *naxis1, int *naxis2 ){
   AstFitsChan *chan;
   AstFrameSet *result;
   AstObject *object;
   FILE *fd;

/* Initialise the returned pointer to indicate that no FrameSet has yet
   been read. */
   result = NULL;

/* Attempt to open the FITS header file */
   fd = fopen( file, "r" );
   if( !fd ) {
      printf("Failed to open FITS header file '%s'.\n", file );

/* If successful, create a FitsChan. This is the object that converts 
   external FITS headers into corresponding AST Objects. Tell it to use 
   the "source" function for obtaining lines of text from the disk file. */
   } else {
      chan = astFitsChan( source, NULL, " " );

/* Associate the descriptor for the input disk file with the StcsChan.
   This makes it available to the "source" function. Since this
   application is single threaded, we could instead have made "fd" a 
   global variable, but the ChannelData facility is used here to illustrate 
   how to pass data to a source or sink function safely in a multi-threaded 
   application. */
      astPutChannelData( chan, fd );

/* Attempt to read the FITS heades and convert them into an AST FrameSet. */
      object = astRead( chan );
      
/* The astRead function is a generic function and so returns a generic
   AstObject pointer. Check an Object was created successfully. */
      if( !object ) {
         printf( "Failed to read an AST Object from FITS header file '%s'.\n", 
                 file );

/* Now check that the object read is actually an AST FrameSet, rather than
   some other class of AST Object. */
      } else if( !astIsAFrameSet( object ) ) {      
         printf( "Expected a FrameSet but read a %s from FITS header "
                 "file '%s'.\n", astGetC( object, "Class" ), file );

/* If the Object is a FrameSet, return the FrameSet pointer. */
      } else {
         result = (AstFrameSet *) object;

/* Check the WCS is 2-dimensional. If not, report an error and set the
   returned pointer to NULL. The memory used to store the FrameSet will
   be released when the current AST object context is ended (by calling
   astEnd). */
        if( astGetI( result, "Naxes" ) != 2 ) {
           printf( "The FITS WCS is not 2-dimensional.\n");
           result = NULL;

/* If it is 2-dimensional, get the NAXIS1 and NAXIS2 keyword values. */
        } else {

            if( !astGetFitsI( chan, "NAXIS1", naxis1 ) ){
               printf("Keyword 'NAXIS1' not found in header\n" );
               result = NULL;
            } 

            if( !astGetFitsI( chan, "NAXIS2", naxis2 ) ){
               printf("Keyword 'NAXIS2' not found in header\n" );
               result = NULL;
            } 
       
         }
      }

/* Close the file. */
      fclose( fd );
   }

   return result;
}


/* -------------------------------------------------------------------
 * This function reads an STC-S description from a given text file,
 * and attempts to convert them into an AST Region. If successful, a
 * pointer to the Region is returned. A NULL pointer is returned if
 * anything goes wrong. 
*/

AstRegion *ReadStcs( const char *file ){
   AstStcsChan *chan;
   AstRegion *result;
   AstObject *object;
   FILE *fd;

/* Initialise the returned pointer to indicate that no Region has yet
   been read. */
   result = NULL;

/* Attempt to open the STC-S file */
   fd = fopen( file, "r" );
   if( !fd ) {
      printf("Failed to open STC-S descrption file '%s'.\n", file );

/* If successful, create an StcsChan. This is the object that converts 
   external STC-S descriptions into corresponding AST Objects. Tell it to 
   use the "source" function for obtaining lines of text from the disk 
   file. */
   } else {
      chan = astStcsChan( source, NULL, " " );

/* Associate the descriptor for the input disk file with the StcsChan.
   This makes it available to the "source" function. Since this
   application is single threaded, we could instead have made "fd" a 
   global variable, but the ChannelData facility is used here to illustrate 
   how to pass data to a source or sink function safely in a multi-threaded 
   application. */
      astPutChannelData( chan, fd );

/* The default behaviour of the astRead function when used on an StcsChan is 
   to read and return the AstroCoordArea as an AST Region. This behaviour
   can be changed by assigning appropriate values to the StcsChan attributes
   "StcsArea", "StcsCoords" and "StcsProps". Options exist to return the 
   AstroCoords as an AST PointList, and/or to return the individual
   property values read from the STC-S text in the form of an AST KeyMap
   (a sort of hashmap). For now, just take the default action of reading the 
   AstroCoordsArea. */
      object = astRead( chan );
      
/* The astRead function is a generic function and so returns a generic
   AstObject pointer. Check an Object was created successfully. */
      if( !object ) {
         printf( "Failed to read an AST Object from STC-S description "
                 "file '%s'.\n", file );

/* Now check that the object read is actually an AST Region, rather than
   some other class of AST Object. */
      } else if( !astIsARegion( object ) ) {      
         printf( "Expected a Region but read a %s from STC-S description "
                 "file '%s'.\n", astGetC( object, "Class" ), file );

/* If the Object is a Region, return the Region pointer. */
      } else {
         result = (AstRegion *) object;
      }

/* Close the file. */
      fclose( fd );
   }

   return result;
}



