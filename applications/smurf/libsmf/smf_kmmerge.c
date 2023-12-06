/*
*+
*  Name:
*     smf_kmmerge

*  Purpose:
*     Merge the contents of adjacent time slices in an AST KeyMap holding
*     time-indexed extension items in an NDF.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_kmmerge( const char *xname, AstKeyMap *keymap, dim_t *index,
*                       int ndet, int *mask, dim_t nts, int *rts, dim_t j0,
*                       dim_t j1, int *status )

*  Arguments:
*     xname = const char * (Given)
*        The name of the NDF extension to use.
*     keymap = AstKeyMap * (Given)
*        An AST keyMap holding the primitive array values copied from the
*        NDF extension (see smf_ext2km). Only time-indexed extension items
*        are stored in this KeyMap.
*     index = dim_t * (Given)
*        A pointer to an  array of integers with "nts" element (i.e. an
*        element for each input time slice). The value held in an element
*        of this array is an index into the "rts" array. Traversing this
*        array from start to end, and using each element as an index into
*        the "rts" array, causes the "rts" array to be traversed in order
*        of increasing RTS_NUM value.
*     ndet = int (Given)
*        The number of detectors.
*     mask = int * (Given)
*        Pointer to an array with "nts" elements. Each element is a bit
*        mask in which each bit is set if there are any good data values in
*        the data from the corresponding detector.
*     nts = dim_t (Given)
*        The total number of input time slices described by the keyMap.
*        (i.e. the total number of time slices read from all input NDFs).
*     rts = int * (Given)
*        The RTS_NUM value associated with each input time slice. The length
*        of this array should be "nts".
*     j0 = dim_t (Given)
*        The index of the first time slice to use.
*     j1 = dim_t (Given)
*        The index of the last time slice to use.
*     status = int * (Given and Returned)
*        Inherited status value.

*  Description:
*     This function merges the extension values stored for an input
*     time slice with the extension values for all subsequent time slices
*     that have the same RTS_NUM value.
*
*     If an extension item contains only a scalar value for each time
*     slice, then an error is reported if there is any difference between
*     the scalar value within each group of merged time slices. If an
*     extension item contains a vector (indexed by detector number) for each
*     time slice, then any good detector values in subsequent time slices
*     are used to over-write any corresponding bad values in earlier time
*     slices (an error is reported if a detector has good - but different -
*     values in two time slices). For any time slice, detectors are
*     skipped if they have a zero bit in the element of the "mask" array
*     corresponding to the time slice.
*
*     It is assumed that the time slice axis is the last axis in the
*     extension array, and the detector axis (if it exists) is the last
*     but one axis in the extension array. Note, the inpuyt KeyMap only
*     contains entries for arrays that are index by time slice.

*  Authors:
*     David S Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
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
*     30-JUN-2009 (DSB):
*        Re-written to process all time slices in a single call. Processing
*        each time slice in a separate call was hugely expensive in terms
*        of time spent accessing elements of KeyMap vector entries.
*     2010-10-04 (TIMJ):
*        Add support for short/word
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2010 Science & Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "sae_par.h"
#include "ast.h"
#include "prm_par.h"
#include "mers.h"
#include "smf.h"

/* System includes */
#include <string.h>


/* Define a macro to do the work for a given data type. */
#define DOTYPE(Type,Sym1,Sym2,Sym3) \
\
/* Allocate a buffer to hold the values and then get them. */ \
   values##Sym3 = astGrow( values##Sym3, (size_t) veclen, sizeof( Type ) ); \
   (void) astMapGet1##Sym1( keymap, key, veclen, &veclen, values##Sym3 ); \
   if( astOK ) { \
\
/* If the vector length equals the number of time slices, just report an \
   error if the value in the two time slices differ. */ \
      if( (dim_t) veclen == nts ) { \
\
/* Get the index of the element corresponding to the lowest RTS_NUM \
   value. If any subsequent input time slices refer to the same RTS_NUM \
   value we will check vector elements are equal. */ \
         into = index[ j0 ]; \
         rts_num0 = rts[ into ]; \
\
/* Loop round all other time slices. */ \
         for( j = j0 + 1; j <= j1; j++ ) { \
\
/* Get the index of the element with the next higher (or equal) RTS_NUM \
   value. */ \
            from = index[ j ]; \
            rts_num = rts[ from ]; \
\
/* If the two entries have the same RTS_NUM values, check the vector \
   elements are equal. */ \
            if( rts_num == rts_num0 ) { \
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
/* If the two entries have differnt RTS_NUM values, do not check the vector \
   elements are equal, but store the new RTS_NUM value. */ \
            } else { \
               into = from; \
               rts_num0 = rts_num; \
            } \
         } \
\
/* If the vector length is multiple of the number of time slices... */ \
      } else if( veclen % nts == 0 ) { \
\
/* Each contiguous section of the vector is assumed to refer to a single \
   detector. Check the length of each section is a multiple of the number \
   of detectors. */ \
         seclen = (int)( veclen/nts ); \
         if( seclen % ndet == 0 ) { \
\
/* Find the number of values per detector. */ \
            vpd = seclen/ndet; \
\
/* Get the index of the element corresponding to the lowest RTS_NUM \
   value. If any subsequent input time slices refer to the saem RTS_NUM \
   value we will merge them into this time slice. */ \
            into = index[ 0 ]; \
            rts_num0 = rts[ into ]; \
\
/* Loop round all time slices. */ \
            for( j = j0; j <= j1; j++ ) { \
\
/* Get the index of the element with the next higher (or equal) RTS_NUM \
   value. */ \
               from = index[ j ]; \
               rts_num = rts[ from ]; \
\
/* If this RTS_NUM value is the same as the previous one, we merge the \
   contents of the "from" time slice into the "into" time slice. If the \
   new RTS_NUM value is different to the previous RTS_NUM value, we start \
   a new block by merging the time slice with itself. */ \
               if( rts_num != rts_num0 ) { \
                  into = from; \
                  rts_num0 = rts_num; \
               } \
\
/* Initialise potiners to the start of the detector data for each time  \
   slice. */ \
               pfrom##Sym3 = values##Sym3 + from*seclen; \
               pinto##Sym3 = values##Sym3 + into*seclen; \
\
/* Loop round each detector. */ \
               detbit = 1; \
               for( idet = 0; idet < ndet && *status == SAI__OK; idet++ ){ \
\
/* Skip detectors that have no good data in the "from" time slice. */ \
                  if( mask[ from ] & detbit ) { \
\
/* Loop round each value for this detector. */ \
                     for( k = 0; k < vpd; k++ ){ \
\
/* If the "from" value is bad, leave the "into" value unchanged. */ \
                        if( pfrom##Sym3[ k ] != VAL__BAD##Sym3 ) { \
\
/* If the "into" value is bad, replace it with the "from" value. */ \
                           if( pinto##Sym3[ k ] == VAL__BAD##Sym3 ) { \
                              pinto##Sym3[ k ] = pfrom##Sym3[ k ]; \
\
/* Set a flag indicating that the contents of the KeyMap entry have  \
   been changed. */ \
                              changed = 1; \
\
/* If the "into" value is not bad, and differs from the "from" value, \
   report an error. */ \
                           } else if( pinto##Sym3[ k ] != pfrom##Sym3[ k ] ) { \
                              if( ! ignore ) { \
                                 *status = SAI__ERROR; \
                                 msgSetc( "X", xname ); \
                                 msgSetc( "N", key ); \
                                 msgSeti( "R", rts_num ); \
                                 msgSeti( "D", idet + 1 ); \
                                 msgSet##Sym2( "V1", pinto##Sym3[ k ] ); \
                                 msgSet##Sym2( "V2", pfrom##Sym3[ k ] ); \
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
                     for( k = 0; k < vpd; k++ ){ \
                        if( pfrom##Sym3[ k ] != VAL__BAD##Sym3 ) { \
                           pfrom##Sym3[ k ] = VAL__BAD##Sym3; \
                           changed = 1; \
                        } \
                     } \
                  } \
\
/* Move pointers on to the start of the data for the next detector. */ \
                  pfrom##Sym3 += vpd; \
                  pinto##Sym3 += vpd; \
\
/* Modify the bit mask so that it contains all zeros except for a 1 at \
   the bit corresponding to the next detector. */ \
                  detbit <<= 1; \
               } \
            } \
\
/* We should never get here. */ \
         } else { \
            msgSetc( "K", key ); \
            msgSeti( "V", veclen ); \
            msgSeti( "N", ndet ); \
            msgOut( " ", "Unexpected vector length (^V) for ^K - it is not a " \
                    "multiple of the number of detectors (^N) (possible " \
                    "programming error).", status ); \
         } \
\
/* We should never get here. */ \
      } else { \
         msgSetc( "K", key ); \
         msgSeti( "V", veclen ); \
         msgSetk( "N", nts ); \
         msgOut( " ", "Unexpected vector length (^V) for ^K - it is not a " \
                 "multiple of the number of input time slices (^N) (possible " \
                 "programming error).", status ); \
      } \
   } \
\
/* If the KeyMap entry has changed, save its new values. */ \
   if( changed ) astMapPut1##Sym1( keymap, key, veclen, values##Sym3, NULL );











void smf_kmmerge( const char *xname, AstKeyMap *keymap, dim_t *index,
                  int ndet, int *mask, dim_t nts, int *rts, dim_t j0,
                  dim_t j1, int *status ){

/* Local Variables */
   const char *key = NULL;
   double *pfromD = NULL;
   double *pintoD = NULL;
   double *valuesD = NULL;
   float *pfromR = NULL;
   float *pintoR = NULL;
   float *valuesR = NULL;
   short *pfromW = NULL;
   short *pintoW = NULL;
   short *valuesW = NULL;
   int *pfromI = NULL;
   int *pintoI = NULL;
   int *valuesI = NULL;
   int changed;
   int detbit;
   dim_t from;
   int i;
   int idet;
   int ignore;
   dim_t into;
   dim_t j;
   int k;
   int nentry;
   int rts_num0;
   int rts_num;
   int seclen;
   int type;
   int vpd;
   int veclen;

/* Check the inherited status */
   if( *status != SAI__OK ) return;

/* Loop round every entry in the KeyMap. */
   nentry = astMapSize( keymap );
   for( i = 0; i < nentry; i++ ) {
      key = astMapKey( keymap, i );

/* Ignore discrepancies in ENVIRO_ items. */
      ignore = ! (key && strncmp( key, "ENVIRO_", 7 ));

/* Get the length of the vector of values in the KeyMap. */
      veclen = astMapLength( keymap, key );

/* Set a flag indicating that the contents of the KeyMap entry have not
   been changed. */
      changed = 0;

/* Invoke a macro to handle the data type. */
      type = astMapType( keymap, key );
      if( type == AST__INTTYPE ){
         DOTYPE(int,I,i,I);

      } else if( type == AST__SINTTYPE ){
         DOTYPE(short,S,i,W);

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
   valuesW = astFree( valuesW );
}

