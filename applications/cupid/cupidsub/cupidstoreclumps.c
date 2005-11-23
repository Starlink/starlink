#include "sae_par.h"
#include "star/hds.h"
#include "star/kaplibs.h"
#include "par.h"
#include "ast.h"
#include "mers.h"
#include "cupid.h"
#include "cupid.h"
#include <string.h>
#include <stdio.h>

/* Local Constants: */
#define MAXCAT   50   /* Max length of catalogue name */

void cupidStoreClumps( const char *param, HDSLoc *xloc, HDSLoc **clist, 
                       int nclump, int ndim, const char *ttl ){
/*
*  Name:
*     cupidStoreClumps

*  Purpose:
*     Store properties of all clumps found by the CLUMPS command.

*  Synopsis:
*     void cupidStoreClumps( const char *param, HDSLoc *xloc, HDSLoc **clist, 
*                            int nclump, int ndim, const char *ttl )

*  Description:
*     This function optionally saves the clump properties in an output
*     catalogue, and then copies the HDS structures describing the found 
*     clumps into the supplied CUPID extension, and then annuls the locators
*     in "clist".

*  Parameters:
*     param
*        The ADAM parameter to associate with the output catalogue.
*     xloc
*        HDS locator for the CUPID extension. May be NULL.
*     clist
*        A pointer to an array of "nclump" HDS locators. These locators will 
*        be annulled before returning.
*     nclump
*        The number of locators in "clist".
*     ndim
*        The number of pixel axes in the data.
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
   AstKeyMap *cols;             /* KeyMap holding column names and numbers */
   AstFrame *frm1;              /* Frame describing clump parameters */
   AstFrame *frm2;              /* Frame describing clump centres */
   AstFrameSet *iwcs;           /* FrameSet to be stored in output catalogue */
   AstMapping *map;             /* Mapping from "frm1" to "frm2" */
   HDSLoc *aloc;                /* Locator for array of Clump structures */
   char attr[ 15 ];             /* AST attribute name */
   char cat[ MAXCAT + 1 ];      /* Catalogue name */
   HDSLoc *cloc;                /* Locator for array cell */
   char name[ DAT__SZNAM + 1 ]; /* Component name */
   const char *key;             /* Pointer to entry key */
   double *t;                   /* Pointer to next table value */
   double *tab;                 /* Pointer to catalogue table */
   int i;                       /* Index of next locator */
   int inperm[ 100 ];           /* Input axis permutation array */
   int j;                       /* Loop index */
   int ncol;                    /* number of catalogue columns */
   int ok;                      /* Found columns holding Centre coords? */
   int outperm[ 3 ];            /* Output axis permutation array */
   int icol;                    /* Zero based column index */
   int ncomp;                   /* Number of components in supplied structure */
   int prim;                    /* Is component primitive? */
   int centre1;                 /* Column number for CENTRE1 */
   int centre2;                 /* Column number for CENTRE2 */
   int centre3;                 /* Column number for CENTRE3 */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Initialise all HDS locator pointers to NULL since HDS now objects if it
   receives an uninitialised pointer. */
   aloc = NULL;
   cloc = NULL;

/* See if an output catalogue is to be created. If not, annul the null
   parameter error. */
   parGet0c( param, cat, MAXCAT, status );
   if( *status == PAR__NULL ) {
      errAnnul( status );

/* Otherwise create the catalogue. */
   } else if( *status == SAI__OK ) {

/* Create an AstKeyMap to hold column names and numbers. */
      cols = astKeyMap( "" );

/* First we find out how many rows and columns the table needs. Loop
   round every supplied structure. */
      ncol = 0;
      centre1 = -1;
      centre2 = -1;
      centre3 = -1;
      for( i = 0; i < nclump; i++ ) {

/* Loop round every component in this clump. */
         datNcomp( clist[ i ], &ncomp, status );
         for( j = 0; j < ncomp; j++ ) {
            datIndex( clist[ i ], j + 1, &cloc, status );

/* Pass over non-primitive components. */
            datPrim( cloc, &prim, status );
            if( prim ){

/* Get the name of the component and store the column name and
   corresponding column number in an AstKeyMap (but only if it is not
   already there). */
               datName( cloc, name, status );
               if( !astMapHasKey( cols, name ) ) {
                  astMapPut0I( cols, name, ncol++, NULL );

/* Note the indices of the CENTRE1, CENTRE2 and CENTRE3 columns. */
                  if( !strcmp( name, "CENTRE1" ) ) {
                     centre1 = ncol - 1;
                  } else if( !strcmp( name, "CENTRE2" ) ) {
                     centre2 = ncol - 1;
                  } else if( !strcmp( name, "CENTRE3" ) ) {
                     centre3 = ncol - 1;
                  }
               }
            }

/* Annul the component locator. */
            datAnnul( &cloc, status );

         }
      }

/* Get memory to hold a table of clump parameters. */
      tab = astMalloc( sizeof(double)*nclump*ncol );

/* Loop round all the supplied locators again. */
      for( i = 0; i < nclump && *status == SAI__OK; i++ ) {

/* Loop round every column name */
         for( j = 0; j < ncol; j++ ) {
            key = astMapKey( cols, j );

/* Get the corresponding column number. */
            astMapGet0I( cols, key, &icol );

/* Get the address of the table cell to receive this value. */
            t = tab + icol*nclump + i;

/* Get the value for this column for this clump and store in the table. */
            datFind( clist[ i ], (char *) key, &cloc, status );
            datGetD( cloc, 0, NULL, t, status );

/* Annul the component locator. */
            datAnnul( &cloc, status );

         }
      }

/* Create the catalogue. */
      if( *status == SAI__OK ) {
   
/* Start an AST context. */
         astBegin;
   
/* Create a Frame with "ncol" axes describing the table columns. Set the
   axis Symbols to the column names. */
         frm1 = astFrame( ncol, "Domain=PARAMETERS,Title=Clump parameters" );
         for( j = 0; j < ncol; j++ ) {
            key = astMapKey( cols, j );
            astMapGet0I( cols, key, &icol );
            sprintf( attr, "Symbol(%d)", icol + 1 );
            astSetC( frm1, attr, key );
         }
   
/* Create a Frame with "ndim" axes describing the pixel coords at the
   clump centre. */
         frm2 = astFrame( ndim, "Domain=PIXEL,Title=Pixel coordinates" );
         astSetC( frm2, "Symbol(1)", "P1" );
         if( ndim > 1 ) {
            astSetC( frm2, "Symbol(2)", "P2" );
            if( ndim > 2 ) astSetC( frm2, "Symbol(3)", "P3" );
         }
   
/* Create a Mapping (a PermMap) from the Frame representing the "ncol" clump
   parameters, to the "ndim" Frame representing clump centre positions. The
   inverse transformation supplies bad values for the other parameters. */
         for( j = 0; j < ncol; j++ ) inperm[ j ] = 0;
   

         if( centre1 < 0 ){
            ok = 0;
         } else {
            ok = 1;
            inperm[ centre1 ] = 1;
            outperm[ 0 ] = centre1 + 1;
            if( ndim > 1 ) {
               if( centre2 < 0 ){
                  ok = 0;
               } else {
                  inperm[ centre2 ] = 2;
                  outperm[ 1 ] = centre2 + 1;
                  if( ndim > 2 ) {
                     if( centre3 < 0 ){
                        ok = 0;
                     } else {
                        inperm[ centre3 ] = 3;
                        outperm[ 2 ] = centre3 + 1;
                     }
                  }   
               }
            }   
         }

         if( ok ) {
            map = (AstMapping *) astPermMap( ncol, inperm, ndim, outperm, NULL, "" );

         } else if( *status == SAI__OK ) {
            *status = SAI__ERROR;
            errRep( "CUPIDSTORECLUMPS_ERR1", "cupidStoreClumps: Clump "
                    "centre unspeciified (internal CUPID prigramiing error).", 
                     status );
         }
   
/* Create a FrameSet to store in the output catalogue. It has two Frames,
   the base Frame has "ncol" axes - each axis describes one of the table
   columns. The current Frame has 2 axes and describes the clump (x,y)
   position. */
         iwcs = astFrameSet( frm1, "ID=FIXED_BASE" );
         astAddFrame( iwcs, AST__BASE, map, frm2 );
         astSetI( iwcs, "CURRENT", 1 );
   
/* Create the output catalogue */
         kpg1Wrlst( param, nclump, nclump, ncol, tab, AST__BASE, iwcs,
                    ttl, 1, NULL, 1, status );
   
/* End the AST context. */
         astEnd;
   
      }

/* Free resources */
      tab = astFree( tab );
   }
 
/* If required, store information in the NDF extension */
   if( xloc ) {

/* Create an array of "nclump" Clump structures in the extension, and get
   a locator to it. */
      datNew( xloc, "CLUMPS", "CLUMP", 1, &nclump, status );
      datFind( xloc, "CLUMPS", &aloc, status );

/* Loop round all the supplied locators. */
      for( i = 1; i <= nclump; i++ ) {

/* Get a locator for the cell of the array. */
         datCell( aloc, 1, &i, &cloc, status );

/* Copy the Clump object located by the next element in the "clist" array
   into the current cell. */
         cupidDatCopy( clist[ i - 1 ], cloc );

/* Annul the cell locator. */
         datAnnul( &cloc, status );
      }

/* Annul the locator to the array within the extension */   
      datAnnul( &aloc, status );
   }

/* Loop round all the supplied locators, annulling them. */
   for( i = 0; i < nclump; i++ ) datAnnul( clist + i, status );

}
