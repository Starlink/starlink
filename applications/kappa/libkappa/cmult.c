/* Indicate that we want to use the 8-byte NDF interface */
#define NDF_I8 1

#include "f77.h"
#include "mers.h"
#include "ndf.h"
#include "prm.h"
#include "par.h"
#include "sae_par.h"
#include "star/util.h"
#include "star/lpg.h"
#include <string.h>
#include "star/thr.h"
#include "kaplibs.h"


F77_SUBROUTINE(cmult)( INTEGER(STATUS) ){
/*

*+
*  Name:
*     CMULT

*  Purpose:
*     Multiplies an NDF by a scalar.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CMULT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application multiplies each pixel of an NDF by a scalar
*     (constant) value to produce a new NDF.

*  Usage:
*     cmult in scalar out

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDF structure whose pixels are to be multiplied by a
*        scalar.
*     OUT = NDF (Write)
*        Output NDF structure.
*     SCALAR = _DOUBLE (Read)
*        The value by which the NDF's pixels are to be multiplied.
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null value will cause the title
*        of the NDF supplied for Parameter IN to be used instead.
*        [!]

*  Examples:
*     cmult a 12.5 b
*        Multiplies all the pixels in the NDF called a by the constant
*        value 12.5 to produce a new NDF called b.
*     cmult in=rawdata out=newdata scalar=-19
*        Multiplies all the pixels in the NDF called rawdata by -19 to
*        give newdata.

*  Related Applications:
*     KAPPA: ADD, CADD, CDIV, CSUB, DIV, MATHS, MULT, SUB.

*  Implementation Status:
*     -  This routine correctly processes the AXIS, DATA, QUALITY,
*     LABEL, TITLE, UNITS, HISTORY, WCS and VARIANCE components of an NDF
*     data structure and propagates all extensions.
*     -  Processing of bad pixels and automatic quality masking are
*     supported.
*     -  All non-complex numeric data types can be handled.  Arithmetic
*     is carried out using the appropriate floating-point type, but the
*     numeric type of the input pixels is preserved in the output NDF.
*     -  Huge NDFs are supported.

*  Copyright:
*     Copyright (C) 2021 East Asian Observatory
*     Council.  All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-MAY-2021 (DSB):
*        Original C version, based on equivalent Fortran function by RFWS
*        et al.
*     12-JUL-2021 (DSB):
*        Multiple Variance arrays by the square of the supplied constant.
*        This code was accidentally omitted when converting from Fortran to C.
*     {enter_further_changes_here}

*-
*/
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   ThrWorkForce *wf;     /* Pointer to pool of worker threads */
   const char *comp;     /* Component list */
   char form[ NDF__SZFRM + 1 ];    /* Form of the ARRAY */
   char dtype[ NDF__SZFTP + 1 ];   /* Output data type */
   char itype[ NDF__SZTYP + 1 ];   /* Data type for processing */
   double cons;          /* Constant to be used. */
   int bad;              /* Need to check for bad pixels? */
   int ndf1;             /* Identifier for 1st NDF (input) */
   int ndf2;             /* Identifier for 2nd NDF (input) */
   int var;              /* Variance component present? */
   size_t el;            /* Number of mapped elements */
   size_t nerr;          /* Number of numerical errors */
   void *pntr1[2];       /* Pointers to 1st NDF mapped arrays */
   void *pntr2[2];       /* Pointers to 2nd NDF mapped arrays */

/* Check inherited global status. */
   if( *STATUS != SAI__OK ) return;

/* Begin an NDF context. */
   ndfBegin();

/* Obtain an identifier for the input NDF. */
   lpgAssoc( "IN", "READ", &ndf1, STATUS );

/* Obtain the scalar value to be used. */
   parGet0d( "SCALAR", &cons, STATUS );

/* Create a new output NDF based on the input NDF. Propagate the WCS, axis,
   quality, units and variance components. */
   lpgProp( ndf1, "WCS,Axis,Quality,Units,Variance", "OUT", &ndf2, STATUS );

/* See if the input NDF has a variance component and set the list of
   components to process accordingly. */
   ndfState( ndf1, "Variance", &var, STATUS );
   if( var ) {
      comp = "Data,Variance";
   } else {
      comp = "Data";
   }

/* Determine the data type to use for processing and set the output data
   type accordingly. */
   ndfMtype( "_BYTE,_UBYTE,_WORD,_UWORD,_INTEGER,_INT64,_REAL,_DOUBLE",
             ndf1, ndf1, comp, itype, sizeof(itype), dtype, sizeof(dtype),
             STATUS );
   ndfStype( dtype, ndf2, comp, STATUS );

/* Map the input and output data arrays. */
   ndfMap( ndf1, comp, itype, "READ", pntr1, &el, STATUS );
   ndfMap( ndf2, comp, itype, "WRITE", pntr2, &el, STATUS );

/* See if checks for bad pixels are needed when processing the NDF's
   data array. */
   ndfBad( ndf1, "Data", 0, &bad, STATUS );

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( "KAPPA_THREADS", STATUS ), STATUS );

/* Select the appropriate routine for the data type being processed and
   multiply the data array by the constant. */
   if( !strcmp( itype, "_BYTE" ) ) {
      kpgCmultB( wf, bad, el, pntr1[0], cons, pntr2[0], &(nerr), STATUS );

   } else if( !strcmp( itype, "_UBYTE" ) ) {
      kpgCmultUB( wf, bad, el, pntr1[0], cons, pntr2[0], &(nerr), STATUS );

   } else if( !strcmp( itype, "_DOUBLE" ) ) {
      kpgCmultD( wf, bad, el, pntr1[0], cons, pntr2[0], &(nerr), STATUS );

   } else if( !strcmp( itype, "_INTEGER" ) ) {
      kpgCmultI( wf, bad, el, pntr1[0], cons, pntr2[0], &(nerr), STATUS );

   } else if( !strcmp( itype, "_INT64" ) ) {
      kpgCmultK( wf, bad, el, pntr1[0], cons, pntr2[0], &(nerr), STATUS );

   } else if( !strcmp( itype, "_REAL" ) ) {
      kpgCmultF( wf, bad, el, pntr1[0], cons, pntr2[0], &(nerr), STATUS );

   } else if( !strcmp( itype, "_WORD" ) ) {
      kpgCmultW( wf, bad, el, pntr1[0], cons, pntr2[0], &(nerr), STATUS );

   } else if( !strcmp( itype, "_UWORD" ) ) {
      kpgCmultUW( wf, bad, el, pntr1[0], cons, pntr2[0], &(nerr), STATUS );

   } else if( *STATUS == SAI__OK ){
      *STATUS = SAI__ERROR;
      errRepf( " ", "Unsupported data type'%s'.", STATUS, itype );
   }

/* See if there may be bad pixels in the output data array and set the
   output bad pixel flag value accordingly unless the output NDF is
   primitive. */
   if( nerr > 0 ) bad = 1;
   ndfForm( ndf2, "Data", form, sizeof(form), STATUS );
   if( strcmp( form, "PRIMITIVE" ) ) ndfSbad( bad, ndf2, "Data", STATUS );

/* If there is a variance component to be processed, then square the
   constant to be used for multiplication. */
   if( var ){
      cons *= cons;

/* See if checks for bad pixels are needed when processing the NDF's
   variance array. */
      ndfBad( ndf1, "Variance", 0, &bad, STATUS );

/* Select the appropriate routine for the data type being processed and
   multiply the variance array by the squared constant. */
      if( !strcmp( itype, "_BYTE" ) ) {
         kpgCmultB( wf, bad, el, pntr1[1], cons, pntr2[1], &(nerr), STATUS );

      } else if( !strcmp( itype, "_UBYTE" ) ) {
         kpgCmultUB( wf, bad, el, pntr1[1], cons, pntr2[1], &(nerr), STATUS );

      } else if( !strcmp( itype, "_DOUBLE" ) ) {
         kpgCmultD( wf, bad, el, pntr1[1], cons, pntr2[1], &(nerr), STATUS );

      } else if( !strcmp( itype, "_INTEGER" ) ) {
         kpgCmultI( wf, bad, el, pntr1[1], cons, pntr2[1], &(nerr), STATUS );

      } else if( !strcmp( itype, "_INT64" ) ) {
         kpgCmultK( wf, bad, el, pntr1[1], cons, pntr2[1], &(nerr), STATUS );

      } else if( !strcmp( itype, "_REAL" ) ) {
         kpgCmultF( wf, bad, el, pntr1[1], cons, pntr2[1], &(nerr), STATUS );

      } else if( !strcmp( itype, "_WORD" ) ) {
         kpgCmultW( wf, bad, el, pntr1[1], cons, pntr2[1], &(nerr), STATUS );

      } else if( !strcmp( itype, "_UWORD" ) ) {
         kpgCmultUW( wf, bad, el, pntr1[1], cons, pntr2[1], &(nerr), STATUS );

      } else if( *STATUS == SAI__OK ){
         *STATUS = SAI__ERROR;
         errRepf( " ", "Unsupported data type'%s'.", STATUS, itype );
      }

/* See if there may be bad pixels in the output variance array and set the
   output bad pixel flag value accordingly unless the output NDF is
   primitive. */
      if( nerr > 0 ) bad = 1;
      ndfForm( ndf2, "Variance", form, sizeof(form), STATUS );
      if( strcmp( form, "PRIMITIVE" ) ) ndfSbad( bad, ndf2, "Variance", STATUS );
   }

/* Obtain a new title for the output NDF. */
   ndfCinp( "TITLE", ndf2, "Title", STATUS );

/* End the NDF context. */
   ndfEnd( STATUS );

/* If an error occurred, then report context information. */
   if( *STATUS != SAI__OK ) errRep( " ", "CMULT: Error multiplying an NDF "
                                    "data structure by a scalar.", STATUS );
}

