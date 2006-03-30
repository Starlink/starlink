#include "sae_par.h"
#include "star/hds.h"
#include "star/kaplibs.h"
#include "par.h"
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "cupid.h"
#include "cupid.h"
#include <string.h>
#include <stdio.h>

/* Local Constants: */
#define MAXCAT   50   /* Max length of catalogue name */

void cupidStoreClumps( const char *param, HDSLoc *xloc, HDSLoc *obj, 
                       int ndim, double beamcorr[ 3 ], const char *ttl ){
/*
*  Name:
*     cupidStoreClumps

*  Purpose:
*     Store properties of all clumps found by the CLUMPS command.

*  Synopsis:
*     void cupidStoreClumps( const char *param, HDSLoc *xloc, HDSLoc *obj, 
*                            int ndim, double beamcorr[ 3 ], const char *ttl )

*  Description:
*     This function optionally saves the clump properties in an output
*     catalogue, and then copies the NDF describing the found clumps into 
*     the supplied CUPID extension.

*  Parameters:
*     param
*        The ADAM parameter to associate with the output catalogue.
*     xloc
*        HDS locator for the CUPID extension of the NDF in which to store
*        the clump properties. May be NULL.
*     obj
*        A locator for an HDS array the clump NDF structures.
*     ndim
*        The number of pixel axes in the data.
*     beamcorr
*        An array holding the FWHM (in pixels) describing the instrumental 
*        smoothing along each pixel axis. The clump widths stored in the 
*        output catalogue are reduced to correct for this smoothing.
*     ttl
*        The title for the output catalogue (if any).

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     10-NOV-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   AstFrame *frm1;              /* Frame describing clump parameters */
   AstFrame *frm2;              /* Frame describing clump centres */
   AstFrameSet *iwcs;           /* FrameSet to be stored in output catalogue */
   AstMapping *map;             /* Mapping from "frm1" to "frm2" */
   HDSLoc *aloc;                /* Locator for array of Clump structures */
   HDSLoc *ncloc;               /* Locator for array cell */
   HDSLoc *cloc;                /* Locator for array cell */
   HDSLoc *dloc;                /* Locator for cell value */
   char attr[ 15 ];             /* AST attribute name */
   char cat[ MAXCAT + 1 ];      /* Catalogue name */
   const char **names;          /* Component names */
   double *cpars;               /* Array of parameters for a single clump */
   double *t;                   /* Pointer to next table value */
   double *tab;                 /* Pointer to catalogue table */
   int i;                       /* Index of next locator */
   int icol;                    /* Zero based column index */
   int indf;                    /* Identifier for supplied NDF */
   int indf2;                   /* Identifier for copied NDF */
   int irow;                    /* One-based row index */
   int ncpar;                   /* Number of clump parameters */
   int nndf;                    /* Total number of NDFs */
   int place;                   /* Place holder for copied NDF */
   int there;                   /* Does component exist?*/

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Get the total number of NDFs supplied. */
   datSize( obj, (size_t *) &nndf, status );

/* If we are writing the information to an NDF extension, create an array 
   of "nndf" Clump structures in the extension, and get a locator to it. */
   if( xloc ) {
      aloc = NULL;
      datThere( xloc, "CLUMPS", &there, status );
      if( there ) datErase( xloc, "CLUMPS", status );
      datNew( xloc, "CLUMPS", "CLUMP", 1, &nndf, status );
      datFind( xloc, "CLUMPS", &aloc, status );
   } else {
      aloc = NULL;
   }

/* Indicate that no memory has yet been allocated to store the parameters
   for a single clump. */
   cpars = NULL;

/* Indicate that no memory has yet been allocated to store the full table
   of parameters for all clumps. */
   tab = NULL;

/* Loop round the non-null identifiers, keeping track of the one-based row 
   number corresponding to each one. */
   irow = 0;
   for( i = 1; i <= nndf && *status == SAI__OK; i++ ) {
      ncloc = NULL;
      datCell( obj, 1, &i, &ncloc, status );

      errBegin( status );
      ndfFind( ncloc, " ", &indf, status );
      errEnd( status );

      datAnnul( &ncloc,status );
      if( indf != NDF__NOID ) {
         irow++;

/* Calculate the clump parameters from the clump data values stored in the 
   NDF. This allocates memory if needed, and also returns some global
   information which is the same for every clump (the parameter names, the 
   indices of the parameters holding the clump central position, and the 
   number of parameters). */
         cpars = cupidClumpDesc( indf, beamcorr, cpars, &names, &ncpar );

/* If we have not yet done so, allocate memory to hold a table of clump 
   parameters. In this table, all the values for column 1 come first, 
   followed by all the values for column 2, etc (this is the format required 
   by KPG1_WRLST). */ 
         if( !tab ) tab = astMalloc( sizeof(double)*nndf*ncpar );
         if( tab ) {

/* Put the clump parameters into the table. */
            t = tab + irow - 1;
            for( icol = 0; icol < ncpar; icol++ ) {
               *t = cpars[ icol ];
               t += nndf;
            }

/* If required, put the clump parameters into the current CLUMP structure. */
            if( aloc ) {

/* Get an HDS locator for the next cell in the array of CLUMP structures. */
               cloc = NULL;
               datCell( aloc, 1, &irow, &cloc, status );

/* Store each clump parameter in a component of this CLUMP structure. */
               dloc = NULL;
               for( icol = 0; icol < ncpar; icol++ ) {
                  datNew( cloc, names[ icol ], "_DOUBLE", 0, NULL, status );
                  datFind( cloc, names[ icol ], &dloc, status );
                  datPutD( dloc, 0, NULL, cpars + icol, status );
                  datAnnul( &dloc, status );
               }

/* Store the supplied NDF in a component called "MODEL" of the CLUMP
   structure. */
               ndfPlace( cloc, "MODEL", &place, status );
               ndfCopy( indf, &place, &indf2, status );
               ndfAnnul( &indf2, status );

/* Free the locator to the CLUMP structure. */
               datAnnul( &cloc, status );
            }
         }
         ndfAnnul( &indf, status );
      }
   }

/* See if an output catalogue is to be created. If not, annull the null
   parameter error. */
   parGet0c( param, cat, MAXCAT, status );
   if( *status == PAR__NULL ) {
      errAnnul( status );
  
/* Otherwise create the catalogue. */
   } else if( tab && *status == SAI__OK ) {

/* Start an AST context. */
      astBegin;
   
/* Create a Frame with "ncpar" axes describing the table columns. Set the
   axis Symbols to the column names. */
      frm1 = astFrame( ncpar, "Domain=PARAMETERS,Title=Clump parameters" );
      for( icol = 0; icol < ncpar; icol++ ) {
         sprintf( attr, "Symbol(%d)", icol + 1 );
         astSetC( frm1, attr, names[ icol ] );
      }
   
/* Create a Frame with "ndim" axes describing the pixel coords at the
   clump centre. */
      frm2 = astFrame( ndim, "Domain=PIXEL,Title=Pixel coordinates" );
      astSetC( frm2, "Symbol(1)", "P1" );
      if( ndim > 1 ) {
         astSetC( frm2, "Symbol(2)", "P2" );
         if( ndim > 2 ) astSetC( frm2, "Symbol(3)", "P3" );
      }
   
/* Create a Mapping (a PermMap) from the Frame representing the "ncpar" clump
   parameters, to the "ndim" Frame representing clump centre positions. The
   inverse transformation supplies bad values for the other parameters. */
      map = (AstMapping *) astPermMap( ncpar, NULL, ndim, NULL, NULL, "" );
   
/* Create a FrameSet to store in the output catalogue. It has two Frames,
   the base Frame has "ncpar" axes - each axis describes one of the table
   columns. The current Frame has 2 axes and describes the clump (x,y)
   position. The ID value of FIXED_BASE is a special value recognised by 
   kpg1Wrlst. */
      iwcs = astFrameSet( frm1, "ID=FIXED_BASE" );
      astAddFrame( iwcs, AST__BASE, map, frm2 );
      astSetI( iwcs, "CURRENT", 1 );
   
/* Create the output catalogue */
      kpg1Wrlst( param, nndf, nndf, ncpar, tab, AST__BASE, iwcs,
                 ttl, 1, NULL, 1, status );
   
/* End the AST context. */
      astEnd;
   }

/* If required, annul the locator for the array of CLUMP structures. */
   if( aloc ) datAnnul( &aloc, status );

/* Free resources. */
   tab = astFree( tab );
   cpars = astFree( cpars );

}
