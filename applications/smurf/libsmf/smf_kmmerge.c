/*
*+
*  Name:
*     smf_kmmerge

*  Purpose:
*     Merge the contents of two time slices in an AST KeyMap holding 
*     time-indexed extension items in an NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_kmmerge( const char *xname, AstKeyMap *keymap, int from,
*                       int into, int ndet, int *mask, int nts, 
*                       int rts_num, int *status )

*  Arguments:
*     xname = const char * (Given)
*        The name of the NDF extension to use.
*     keymap = AstKeyMap * (Given)
*        An AST keyMap holding the primitive array values copied from the
*        NDF extension (see smf_ext2km). Only time-indexed extension items 
*        are stored in this KeyMap.
*     from = int (Given)
*        The zero-based input time slice index for the values that are to 
*        be read. The contents of this time slice are unchanged.   
*     into = int (Given)
*        The zero-based index for the input time slice in to which the read 
*        values are to be merged. 
*     ndet = int (Given)
*        The number of detectors. 
*     mask = int * (Given)
*        Pointer to an array with "ndet" elements. Each element is a flag 
*        indicating if the are any good data values in the input spectrum
*        from the corresponding detector for the "from" time slice.
*     nts = int (Given)
*        The total number of input time slices described by the keyMap.
*     rts_num = int (Given)
*        The RTS_NUM value associated with both time slices.
*     status = int * (Given and Returned)
*        Inherited status value. 

*  Description:
*     This function merges the extension values stored for an input 
*     time slice with the extension values for another time slice. 
*     If an extension item contains only a scalar value for each time 
*     slice, then an error is reported if there is any difference between
*     the scalar value in time slices "from" and "into". If an extension
*     item contains a vector (indexed by detector number) for each time 
*     slice, then any good detector values in "from" are used to over-write 
*     any corresponding bad values in "into" (an error is reported if a
*     detector has good - but different - values in both time slices). 
*     Detector are skipped if they have a zero value in the "mask" array.
*
*     It is assumed that the time slice axis is the last axis in the
*     extension array, and the detector axis (if it exists) is the last
*     but one axis in the extension array. Note, the inpuyt KeyMap only
*     contains entries for arrays that are index by time slice.

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     14-APR-2008 (DSB):
*        Initial version.
*     17-APR-2008 (DSB):
*        Correct error message text and handle cases where a time slice
*        is being merged with itself (this happens for the first time
*        slice encountered for a given RTS_NUM value).
*     16-MAY-2008 (DSB):
*        Ignore discrepancies in ENVIRO_ items.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "prm_par.h"
#include "mers.h"

/* System includes */
#include <string.h>


/* Define a macro to do the work for a given data type. */
#define DOTYPE(Type,Sym1,Sym2,Sym3) \
\
/* Allocate a buffer to hold the values and then get them. */ \
            values##Sym3 = astGrow( values##Sym3, veclen, sizeof( Type ) ); \
            (void) astMapGet1##Sym1( keymap, key, veclen, &veclen, values##Sym3 ); \
            if( astOK ) { \
\
/* If the vector length equals the number of time slices, just report an \
   error if the value in the two time slices differ. */ \
               if( veclen == nts ) { \
                  if( values##Sym3[ from ] != values##Sym3[ into ] ) { \
                     if( ! ignore ) { \
                        *status = SAI__ERROR; \
                        msgSetc( "X", xname ); \
                        msgSetc( "N", key ); \
                        msgSeti( "R", rts_num ); \
                        msgSet##Sym2( "V1", values##Sym3[ from ] ); \
                        msgSet##Sym2( "V2", values##Sym3[ into ] ); \
                        errRep( "", "Differing values (^V1 and ^V2) found for " \
                                "item ^X.^N when RTS_NUM=^R.", status ); \
                     } \
                  } \
\
/* If the vector length is multiple of the number of time slices... */ \
               } else if( veclen % nts == 0 ) { \
\
/* Each contiguous section of the vector is assumed to refer to a single \
   detector. Check the length of each section is a multiple of the number \
   of detectors. */ \
                  seclen = veclen/nts; \
                  if( seclen % ndet == 0 ) { \
\
/* Find the number of values per detector. */ \
                     vpd = seclen/ndet; \
\
/* Initialise potiners to the start of the detector data for each time  \
   slice. */ \
                     pfrom##Sym3 = values##Sym3 + from*seclen; \
                     pinto##Sym3 = values##Sym3 + into*seclen; \
\
/* Loop round each detector. */ \
                     for( idet = 0; idet < ndet && *status == SAI__OK; idet++ ){ \
\
/* Skip detectors that have no good data in the "from" time slice. */ \
                        if( mask [ idet ] ) { \
\
/* Loop round each value for this detector. */ \
                           for( j = 0; j < vpd; j++ ){ \
\
/* If the "from" value is bad, leave the "into" value unchanged. */ \
                              if( pfrom##Sym3[ j ] != VAL__BAD##Sym3 ) { \
\
/* If the "into" value is bad, replace it with the "from" value. */ \
                                 if( pinto##Sym3[ j ] == VAL__BAD##Sym3 ) { \
                                    pinto##Sym3[ j ] = pfrom##Sym3[ j ]; \
\
/* Set a flag indicating that the contents of the KeyMap entry have  \
   been changed. */ \
                                    changed = 1; \
\
/* If the "into" value is not bad, and differs from the "from" value, \
   report an error. */ \
                                 } else if( pinto##Sym3[ j ] != pfrom##Sym3[ j ] ) { \
                                    if( ! ignore ) { \
                                       *status = SAI__ERROR; \
                                       msgSetc( "X", xname ); \
                                       msgSetc( "N", key ); \
                                       msgSeti( "R", rts_num ); \
                                       msgSeti( "D", idet + 1 ); \
                                       msgSet##Sym2( "V1", pinto##Sym3[ j ] ); \
                                       msgSet##Sym2( "V2", pfrom##Sym3[ j ] ); \
                                       errRep( "", "Detector ^D has differing values " \
                                               "(^V1 and ^V2) for item ^X.^N when " \
                                               "RTS_NUM=^R.", status ); \
                                       break; \
                                    } \
                                 } \
                              } \
                           } \
\
/* If we are merging a time slice with itself (i.e. if this is the first \
   time slice for the RTS_NUM value), set metatdata bad for detectors \
   that have no good spectral data values. */ \
                        } else if( from == into ){ \
                           for( j = 0; j < vpd; j++ ){ \
                              if( pfrom##Sym3[ j ] != VAL__BAD##Sym3 ) { \
                                 pfrom##Sym3[ j ] = VAL__BAD##Sym3; \
                                 changed = 1; \
                              } \
                           } \
                        } \
\
/* Move pointers on to the start of the data for the next detector. */ \
                        pfrom##Sym3 += vpd; \
                        pinto##Sym3 += vpd; \
                     } \
                  } \
               } \
            } \
 \
/* If the KeyMap entry has changed, save its new values. */ \
            if( changed ) astMapPut1##Sym1( keymap, key, veclen, values##Sym3, NULL ); 











void smf_kmmerge( const char *xname, AstKeyMap *keymap, int from, int into, 
                  int ndet, int *mask, int nts, int rts_num, int *status ){

/* Local Variables */
   const char *key = NULL;
   double *pfromD = NULL;
   double *pintoD = NULL;
   double *valuesD = NULL;
   float *pfromR = NULL;
   float *pintoR = NULL;
   float *valuesR = NULL;
   int *pfromI = NULL;
   int *pintoI = NULL;
   int *valuesI = NULL;
   int changed;
   int i;
   int idet;
   int ignore;
   int j;
   int nentry;
   int seclen;
   int type;
   int veclen;
   int vpd;           

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Loop round every entry in the KeyMap. */
   nentry = astMapSize( keymap );
   for( i = 0; i < nentry; i++ ) {
      key = astMapKey( keymap, i );

/* Ignore discrepancies in ENVIRO_ items. */
      ignore = key || strncmp( key, "ENVIRO_", 7 );

/* Get the vector length of the entry. */
      veclen = astMapLength( keymap, key );

/* Set a flag indicating that the contents of the KeyMap entry have not
   been changed. */
      changed = 0;

/* Invoke a macro to handle the data type. */
      type = astMapType( keymap, key );
      if( type == AST__INTTYPE ){
         DOTYPE(int,I,i,I);

      } else if( type == AST__FLOATTYPE ){
         DOTYPE(float,F,r,R);

      } else if( type == AST__DOUBLETYPE ){
         DOTYPE(double,D,d,D);

      } 
   }

/* Free resources. */
   valuesD = astFree( valuesD );
   valuesR = astFree( valuesR );
   valuesI = astFree( valuesI );
}

