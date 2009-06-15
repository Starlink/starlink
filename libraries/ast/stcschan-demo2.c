/* Name:
      stcschan-demo2.c

   Purpose:
      A demonstration of the facilities provided by the AST library 
      for reading STC metadata encoded using the STC-S linear string 
      format.

   Description:
      This program reads a set of FITS-WCS headers from a text file, and
      writes an STC-S description of the region covered by the FITS
      file to standard output.

   Notes:
      - The simple approach used in this demonstration will report an
      error if the FITS headers describe a WCS in which some pixels have
      invalid WCS axis values (such as most all sky maps, for instance).

   Usage:
      % stcschan-demo2 <header-file> 

      <header-file>: The path to a text file containing a set of FITS-WCS
      headers.

   Example:
      % stcschan-demo2 m31.head

   To compile and link:
      Assuming your starlink distribution is in "/star":

      % gcc -o stcschan-demo2 stcschan-demo2.c -L/star/lib \
            -I/star/include `ast_link`

*/

/* Include system headers. */
#include <stdio.h>
#include <string.h>

/* Include the AST library header. */
#include "ast.h"

/* Maximum number of axes in an STC-S AstroCoordSystem. */
#define MAX_AXES 5

/* Maximum allowed length for a single line of text from the disk file. */
#define MAX_LINE_LEN 100

/* Prototype for the function that reads text from the disk file. */
const char *source( void );



int main( int argc, char **argv ){

/* Local variables: */
   AstBox *pixbox;
   AstFitsChan *fchan;
   AstFrame *pixfrm;
   AstFrame *wcsfrm;
   AstFrameSet *frameset;
   AstKeyMap *warnings;
   AstMapping *pix2wcs;
   AstObject *object;
   AstRegion *wcsbox;
   AstStcsChan *schan;
   FILE *fd;
   char key[ 15 ];
   char keyword[ 9 ];
   const char *message;
   double p1[ MAX_AXES ];
   double p2[ MAX_AXES ];
   int axis;
   int iwarn;
   int naxis;
   int status;

/* Initialised the returned system status to indicate success. */
   status = 0;

/* Check a file was specified on the command line, and attempt to open it
   for read access. */
   if( argc < 2 ) {
      printf( "Usage: stcschan-demo2 <header-file>\n" );
      status = 1;
   } else {
      fd = fopen( argv[ 1 ], "r" );
      if( !fd ) {
         printf("Failed to open input file '%s'.\n", argv[ 1 ] );
         status = 1;
      }
   }

/* If a disk file was opened successfully... */
   if( !status ) {

/* Start an AST object context. This means we do not need to annull 
   each AST Object individually. Instead, all Objects created within 
   this context will be annulled automatically by the corresponding
   invocation of astEnd. */
      astBegin;

/* Create a FitsChan. This is the object that converts external FITS
   headers into corresponding AST Objects. Tell it to use the "source" 
   function for obtaining lines of text from the disk file. */
      fchan = astFitsChan( source, NULL, " " );

/* Associate the descriptor for the input disk file with the StcsChan.
   This makes it available to the "source" function. Since this
   application is single threaded, we could instead have made "fd" a 
   global variable, but the ChannelData facility is used here to illustrate 
   how to pass data to a source or sink function safely in a multi-threaded 
   application. */
      astPutChannelData( fchan, fd );

/* Attempt to read the FITS heades and convert them into an AST FrameSet. */
      object = astRead( fchan );
      
/* The astRead function is a generic function and so returns a generic
   AstObject pointer. Check an Object was created successfully. */
      if( !object ) {
         printf( "Failed to read an AST Object from file '%s'.\n", 
                 argv[ 1 ] );
         status = 1;

/* Now check that the object read is actually an AST FrameSet, rather than
   some other class of AST Object. */
      } else if( !astIsAFrameSet( object ) ) {      
         printf( "Expected a FrameSet but read a %s from file '%s'.\n", 
                 astGetC( object, "Class" ), argv[ 1 ] );
         status = 1;

/* We now know we have a FrameSet so it is safe to use the pointer
   returned by astRead as a FrameSet pointer. Do the cast now to avoid
   repeated casting in future. */
      } else {
         frameset = (AstFrameSet *) object;      

         wcsfrm = astGetFrame( frameset, AST__CURRENT );
         pixfrm = astGetFrame( frameset, AST__BASE );
         pix2wcs = astGetMapping( frameset, AST__BASE, AST__CURRENT );

/* Get the number of pixel axes. */
         naxis = astGetI( pixfrm, "Naxes" );

         for( axis = 0; axis < naxis; axis++ ) { 
            sprintf( keyword, "NAXIS%d", axis + 1 );
            p1[ axis ] = 0.5;
            if( !astGetFitsF( fchan, keyword, p2 + axis ) ){
               printf("Keyword '%s' not found in header\n", keyword );
               status = 1;
               break;
            } else {
               p2[ axis ] += 0.5;
            }
         }
      }




      if( !status ) {
         pixbox = astBox( pixfrm, 1, p1, p2, NULL, " " );
         wcsbox = astMapRegion( pixbox, pix2wcs, wcsfrm );
         wcsbox = astSimplify( wcsbox );


/* Create an StcsChan. This is the object that converts external STC-S
   descriptions into corresponding AST Objects. Tell it to use the
   "source" function for obtaining lines of text from the disk file. Also
   tell it to store all warnings generated by the conversion for later
   use. Other attributes of the StcsChan class retain their default
   values. */
         schan = astStcsChan( NULL, NULL, "ReportLevel=3" );
         if( ! astWrite( schan, wcsbox ) && astOK ) {
            printf( "Failed to convert the Region into an STC-S "
                    "description.\n" );
         }
      }

/* We asked the StcsChan to record any warnings that were generated
   whilst converting the STC-S description into a corresponding AST
   Object (a Region in this case). We now see if any such warnings were
   generated by the earlier call to astRead. */
      warnings = astWarnings( schan );

/* If any warnings were generated, and if no other error has occurred so
   far, display the warnings. */
      if( warnings && !status && astOK ) {
         printf( "\nThe following warnings were issued:\n" );

/* The warnings are stored in an AST KeyMap (a sort of hashmap). Each
   warning message is associated with a key of the form "Warning_1",
   "Warning_2", etc. Loop round successive keys, obtaining a value for
   each key from the warnings KeyMap, and displaying it. */
         iwarn = 1;
         while( astOK ) {
            sprintf( key, "Warning_%d", iwarn++ );
            if( astMapGet0C( warnings, key, &message ) ) {
               printf( "\n- %s\n", message );
            } else {
               break;
            }
         }             
      }

/* End the AST Object context. All Objects created since the
   corresponding invocation of astbegin will be annulled automatically. */
      astEnd;

/* Close the disk file. */
      (void) fclose( fd );
   }

/* If an error occurred in the AST library, set the retiurns system
   status non-zero. */
   if( !astOK ) status = 1;
   return status;
}







/* This is a function that reads a line of text from the disk file and
   returns it to the AST library. It is called from within the astRead
   function. */
const char *source( void ){
   static char buffer[ MAX_LINE_LEN + 2 ];
   FILE *fd = astChannelData;
   return fgets( buffer, MAX_LINE_LEN + 2, fd );
}





