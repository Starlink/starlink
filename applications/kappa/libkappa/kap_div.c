/* Indicate that we want to use the 8-byte NDF interface */
#define NDF_I8 1

#include "f77.h"
#include "mers.h"
#include "ndf.h"
#include "prm.h"
#include "ast.h"
#include "kaplibs.h"
#include "sae_par.h"
#include "star/lpg.h"
#include <string.h>
#include "star/thr.h"


F77_SUBROUTINE(kap_div)( INTEGER(STATUS) ){
/*
*+
*  Name:
*     div

*  Purpose:
*     Divides one NDF data structure by another.

*  Type of Module:
*     ADAM A-task

*  Synopsis:
*     void kap_div( int *status )

*  Parameters:
*     *status
*        The global status.

*  Description:
*     This function divides one NDF data structure by another pixel-by-
*     pixel to produce a new NDF.

*  Usage:
*     div in1 in2 out

*  ADAM Parameters:
*     IN1 = NDF (Read)
*        First NDF, to be divided by the second NDF.
*     IN2 = NDF (Read)
*        Second NDF, to be divided into the first NDF.
*     OUT = NDF (Write)
*        Output NDF to contain the ratio of the two input NDFs.
*     TITLE = LITERAL (Read)
*        Value for the title of the output NDF.  A null value will cause
*        the title of the NDF supplied for parameter IN1 to be used
*        instead. [!]

*  Examples:
*     div a b c
*        This divides the NDF called a by the NDF called b, to make the NDF
*        called c.  NDF c inherits its title from a.
*     div out=c in1=a in2=b title="Normalised data"
*        This divides the NDF called a by the NDF called b, to make the NDF
*        called c.  NDF c has the title "Normalised data".

*  Notes:
*     - If the two input NDFs have different pixel-index bounds, then
*     they will be trimmed to match before being divided. An error
*     will result if they have no pixels in common.

*  Related Applications:
*     KAPPA: ADD, CADD, CDIV, CMULT, CSUB, DIV, MATHS, MULT, SUB.

*  Implementation Status:
*     -  This function correctly processes the WCS, AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, HISTORY, WCS and VARIANCE components of an
*     NDF data structure and propagates all extensions.
*     -  The UNITS component is propagated only if it has the same value in
*     both input NDFs.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.
*     Calculations will be performed using either real or double
*     precision arithmetic, whichever is more appropriate.  If the
*     input NDF structures contain values with other data types, then
*     conversion will be performed as necessary.
*     -  Huge NDFs are supported.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either Version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful, but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S. Berry (EAO)

*  History:
*     22-SEP-2020 (DSB):
*        Original C version, based on equivalent Fortran function by RFWS
*        et al.

*-
*/
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   AstFrame *tmpfrm;     /* An AST Frame used to convert units */
   ThrWorkForce *wf;     /* Pointer to pool of worker threads */
   const char *newun;    /* New Units component */
   const char *comp;     /* NDF component list to be mapped */
   char dtype[ NDF__SZFTP + 1 ];   /* Data type for output components */
   char form[ NDF__SZFRM + 1 ];    /* Form of the ARRAY */
   char itype[ NDF__SZTYP + 1 ];   /* Data type for processing */
   char unit1[ 31 ];     /* Units string from NDF1 */
   char unit2[ 31 ];     /* Units string from NDF2 */
   const char *clist;    /* List of NDF components to copy */
   int bad;              /* Need to check for bad pixels? */
   int iat;              /* String length */
   int iw;               /* Index of worker thread */
   int ndf1;             /* Identifier for 1st NDF (input) */
   int ndf2;             /* Identifier for 2nd NDF (input) */
   int ndf3;             /* Identifier for 3rd NDF (output) */
   int nw;               /* Number of worker threads to use */
   int var1;             /* Variance component in 1st input NDF? */
   int var2;             /* Variance component in 2nd input NDF? */
   int var;              /* Process variance? */
   size_t el;            /* Number of mapped elements */
   size_t nerr;          /* Number of errors */
   size_t step;          /* Number of pixels per worker thread */
   void *pntr1[ 2 ];     /* Pointers to 1st NDF mapped arrays */
   void *pntr2[ 2 ];     /* Pointers to 2nd NDF mapped arrays */
   void *pntr3[ 2 ];     /* Pointers to 3rd NDF mapped arrays */

/* Check inherited global status. */
   if( *STATUS != SAI__OK ) return;

/* Begin an NDF context. */
   ndfBegin();

/* Obtain identifiers for the two input NDFs. */
   lpgAssoc( "IN1", "READ", &ndf1, STATUS );
   lpgAssoc( "IN2", "READ", &ndf2, STATUS );

/* Trim the input pixel-index bounds to match. */
   ndfMbnd( "TRIM", &ndf1, &ndf2, STATUS );

/* Get the input Units. Use "1" as the default unit. This is interpreted
   as "dimensionless" units by AST. */
   strcpy( unit1, "1" );
   ndfCget( ndf1, "Unit", unit1, sizeof(unit1), STATUS );
   strcpy( unit2, "1" );
   ndfCget( ndf2, "Unit", unit2, sizeof(unit2), STATUS );

/* If nether NDF had any units, then neither does the output. */
   if( !strcmp( unit1, "1" ) && !strcmp( unit2, "1" ) ) {
      newun = " ";

/* Otherwise, combine the input NDF units. */
   } else {

/* Create an AST Frame with Unit set to the ratio of the two supplied
   Units. */
      iat = 0;
      newun = astAppendStringf( NULL, &iat, "(%s)/(%s)", unit1, unit2 );
      tmpfrm = astFrame( 1, " " );
      astSetC( tmpfrm, "Unit(1)", newun );
      newun = astFree( (void *) newun );

/* Retrieve the normalised Unit string, and free the Frame. */
      newun = astGetC( tmpfrm, "NormUnit(1)" );
      tmpfrm = astAnnul( tmpfrm );
   }

/* Set the list of components to be propagated from NDF1. */
   clist = "WCS,Axis,Quality";

/* Create a new output NDF based on the first input NDF. Propagate the
   components listed above. */
   lpgProp( ndf1, clist, "OUT", &ndf3, STATUS );

/* Set the output Unit component, if the units are defined. */
   if(  astChrLen( newun ) > 0 ) ndfCput( newun, ndf3, "Unit", STATUS );

/* See whether a variance component is defined in both the input NDFs
   and set the list of components to be processed accordingly. */
   ndfState( ndf1, "Variance", &var1, STATUS );
   ndfState( ndf2, "Variance", &var2, STATUS );
   var = ( var1 && var2 );
   if( var ) {
      comp = "Data,Variance";
   } else {
      comp = "Data";
   }

/* Determine which data type to use to process the input arrays and set
   an appropriate data type in the output NDF. */
   ndfMtype( "_REAL,_DOUBLE", ndf1, ndf2, comp, itype, sizeof(itype),
             dtype, sizeof(dtype), STATUS );
   ndfStype( dtype, ndf3, comp, STATUS );

/* Map the input and output arrays. */
   ndfMap( ndf1, comp, itype, "READ", pntr1, &el, STATUS );
   ndfMap( ndf2, comp, itype, "READ", pntr2, &el, STATUS );
   ndfMap( ndf3, comp, itype, "WRITE", pntr3, &el, STATUS );

/* Merge the bad pixel flag values for the input arrays to see if
   checks for bad pixels are needed. */
   ndfMbad( 1, ndf1, ndf2, comp, 0, &bad, STATUS );

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( "KAPPA_THREADS", STATUS ), STATUS );

/* Select the appropriate function for the data type being processed and
   divide the arrays. */

/* Real data... */
   if( !strcmp( itype, "_REAL" ) ) {
      kpgDivF( wf, bad, var, el, pntr1[ 0 ], pntr1[ 1 ], pntr2[ 0 ],
               pntr2[ 1 ], pntr3[ 0 ], pntr3[ 1 ], &nerr, STATUS );

/* Double precision data... */
   } else if( !strcmp( itype, "_DOUBLE" ) ) {
      kpgDivD( wf, bad, var, el, pntr1[ 0 ], pntr1[ 1 ], pntr2[ 0 ],
               pntr2[ 1 ], pntr3[ 0 ], pntr3[ 1 ], &nerr, STATUS );
   }

/* See if there may be bad pixels in the output arrays and set the
   output bad pixel flag value accordingly unless the output NDF is
   primitive. */
   bad = ( bad || ( nerr != 0 ) );
   ndfForm( ndf3, "Data", form, sizeof(form), STATUS );
   if( strcmp( form, "PRIMITIVE" ) ) ndfSbad( bad, ndf3, "Data", STATUS );

/* Obtain a title for the output NDF. */
   ndfCinp( "TITLE", ndf3, "Title", STATUS );

/* End the NDF context. */
   ndfEnd( STATUS );

/* If an error occurred, then report context information. */
   if( *STATUS != SAI__OK ) errRep( " ", "DIV: Error dividing two NDF "
                                    "data structures.", STATUS );
}

