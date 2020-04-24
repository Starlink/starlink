#include "sae_par.h"
#include "prm_par.h"
#include "kaplibs.h"
#include "mers.h"
#include <string.h>
#include <stdint.h>

void kpg1Badbits( const char *type, char buf[8], size_t *nb, int *status ){
/*
*+
*  Name:
*     kpg1Badbits

*  Purpose:
*     Returns the bit pattern for the bad value of a given HDS data type

*  Language:
*     C.

*  Invocation:
*     void kpg1Badbits( const char *type, char buf[8], size_t *nb,
*                       int *status )

*  Description:
*     This function returns the bit pattern corresponding to the bad
*     value for a given HDS data type.

*  Arguments:
*     type
*        The HDS data type.
*     buf
*        A buffer in which to return the bit pattern. The number of
*        elements required to store the bad value is returned in "*nb".
*        Any remaining (unused) elements are filled with zero.
*     nb
*        Returned holding the number of elements actually used in "buf".
*     status
*        The inherited status.

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     22-APR-2020 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   char bval;
   double dval;
   float rval;
   int ival;
   int64_t kval;
   short int wval;
   unsigned char ubval;
   unsigned short int uwval;
   void *pval;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

   if( !strcmp( type, "_DOUBLE" ) ) {
      *nb = VAL__NBD;
      dval = VAL__BADD;
      pval = &dval;

   } else if( !strcmp( type, "_REAL" ) ) {
      *nb = VAL__NBR;
      rval = VAL__BADR;
      pval = &rval;

   } else if( !strcmp( type, "_INTEGER" ) ) {
      *nb = VAL__NBI;
      ival = VAL__BADI;
      pval = &ival;

   } else if( !strcmp( type, "_INT64" ) ) {
      *nb = VAL__NBK;
      kval = VAL__BADK;
      pval = &kval;

   } else if( !strcmp( type, "_WORD" ) ) {
      *nb = VAL__NBW;
      wval = VAL__BADW;
      pval = &wval;

   } else if( !strcmp( type, "_UWORD" ) ) {
      *nb = VAL__NBUW;
      uwval = VAL__BADUW;
      pval = &uwval;

   } else if( !strcmp( type, "_BYTE" ) ) {
      *nb = VAL__NBB;
      bval = VAL__BADB;
      pval = &bval;

   } else if( !strcmp( type, "_UBYTE" ) ) {
      *nb = VAL__NBUB;
      ubval = VAL__BADUB;
      pval = &ubval;

   } else if( *status == SAI__OK ) {
      pval = NULL;
      msgSetc( "T", type );
      errRep( "", "kpg1Badbits: Unknown NDF data type '^T' - programming "
              "error.", status );
   }

   if( pval ) memcpy( buf, pval, *nb );

}
