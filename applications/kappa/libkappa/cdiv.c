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


F77_SUBROUTINE(cdiv)( INTEGER(STATUS) ){
/*
*+
*  Name:
*     CDIV

*  Purpose:
*     Divides an NDF by a scalar.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CDIV( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application divides each pixel of an NDF by a scalar
*     (constant) value to produce a new NDF.

*  Usage:
*     cdiv in scalar out

*  ADAM Parameters:
*     IN = NDF (Read)
*        Input NDF structure whose pixels are to be divided by a
*        scalar.
*     OUT = NDF (Write)
*        Output NDF structure.
*     SCALAR = _DOUBLE (Read)
*        The value by which the NDF's pixels are to be divided.
*     TITLE = LITERAL (Read)
*        A title for the output NDF.  A null value will cause the title
*        of the NDF supplied for parameter IN to be used instead.
*        [!]

*  Examples:
*     cdiv a 100.0 b
*        Divides all the pixels in the NDF called a by the constant
*        value 100.0 to produce a new NDF called b.
*     cdiv in=data1 out=data2 scalar=-38
*        Divides all the pixels in the NDF called data1 by -38 to give
*        data2.

*  Related Applications:
*     KAPPA: ADD, CADD, CMULT, CSUB, DIV, MATHS, MULT, SUB.

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
*     {enter_further_changes_here}

*-
*/
   GENPTR_INTEGER(STATUS)

/* Local Variables: */
   ThrWorkForce *wf;     /* Pointer to pool of worker threads */
   char form[ NDF__SZFRM + 1 ];    /* Form of the ARRAY */
   char itype[ NDF__SZTYP + 1 ];   /* Data type for processing */
   double cons;          /* Constant to be used. */
   int bad;              /* Need to check for bad pixels? */
   int ndf1;             /* Identifier for 1st NDF (input) */
   int ndf2;             /* Identifier for 2nd NDF (input) */
   size_t el;            /* Number of mapped elements */
   size_t nerr;          /* Number of numerical errors */
   void *pntr1;          /* Pointer to 1st NDF mapped array */
   void *pntr2;          /* Pointer to 2nd NDF mapped array */

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

/* Determine which data type to use to process the input data array. */
   ndfType( ndf1, "Data", itype, sizeof(itype), STATUS );

/* Map the input and output data arrays. */
   ndfMap( ndf1, "Data", itype, "READ", &pntr1, &el, STATUS );
   ndfMap( ndf2, "Data", itype, "WRITE", &pntr2, &el, STATUS );

/* See if checks for bad pixels are needed. */
   ndfBad( ndf1, "Data", 0, &bad, STATUS );

/* Find the number of cores/processors available and create a pool of
   threads of the same size. */
   wf = thrGetWorkforce( thrGetNThread( "KAPPA_THREADS", STATUS ), STATUS );

/* Select the appropriate function for the data type being processed and
   do the arithmetic. */
   if( !strcmp( itype, "_BYTE" ) ) {
      kpgCdivB( wf, bad, el, pntr1, cons, pntr2, &(nerr), STATUS );

   } else if( !strcmp( itype, "_UBYTE" ) ) {
      kpgCdivUB( wf, bad, el, pntr1, cons, pntr2, &(nerr), STATUS );

   } else if( !strcmp( itype, "_DOUBLE" ) ) {
      kpgCdivD( wf, bad, el, pntr1, cons, pntr2, &(nerr), STATUS );

   } else if( !strcmp( itype, "_INTEGER" ) ) {
      kpgCdivI( wf, bad, el, pntr1, cons, pntr2, &(nerr), STATUS );

   } else if( !strcmp( itype, "_INT64" ) ) {
      kpgCdivK( wf, bad, el, pntr1, cons, pntr2, &(nerr), STATUS );

   } else if( !strcmp( itype, "_REAL" ) ) {
      kpgCdivF( wf, bad, el, pntr1, cons, pntr2, &(nerr), STATUS );

   } else if( !strcmp( itype, "_WORD" ) ) {
      kpgCdivW( wf, bad, el, pntr1, cons, pntr2, &(nerr), STATUS );

   } else if( !strcmp( itype, "_UWORD" ) ) {
      kpgCdivUW( wf, bad, el, pntr1, cons, pntr2, &(nerr), STATUS );

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

/* Unmap the data arrays. */
   ndfUnmap( ndf1, "Data", STATUS );
   ndfUnmap( ndf2, "Data", STATUS );

/* Obtain a new title for the output NDF. */
   ndfCinp( "TITLE", ndf2, "Title", STATUS );

/* End the NDF context. */
   ndfEnd( STATUS );

/* If an error occurred, then report context information. */
   if( *STATUS != SAI__OK ) errRep( " ", "CDIV: Error dividing an NDF "
                                    "data structure by a scalar.", STATUS );
}

