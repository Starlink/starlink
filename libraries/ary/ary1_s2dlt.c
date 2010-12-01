#include "sae_par.h"
#include "f77.h"
#include "mers.h"
#include "star/hds.h"
#include "star/hds_fortran.h"
#include "prm_par.h"
#include "ary_par.h"
#include "ary_err.h"
#include "ary_dlt.h"

/* Macros */
/* ------ */

/* Length of VARIANT string. */
#define VARIANT_LEN 20

/* Returns the number of bytes in an HDS type, or -1 if the HDS type is
   not supported */
#define SIZEOF(thistype) ( \
   !strcmp( thistype, "_INTEGER" ) ? VAL__NBI : ( \
   !strcmp( thistype, "_WORD" ) ? VAL__NBW : ( \
   !strcmp( thistype, "_UWORD" ) ? VAL__NBUW : ( \
   !strcmp( thistype,"_UBYTE") ? VAL__NBUB : ( \
   !strcmp( thistype,"_BYTE") ? VAL__NBB : -1 )))))



/* Type definitions. */
/* ----------------- */
/* Type for the compression and check functions that accepts void * pointers
   for arrays. */
typedef  void (*delt_fun_type)( void *, int, size_t, void *, void *, void *,
                                int *, int *, int *, int * );

/* Type for the check function that accepts void * pointers for
   arrays. */
typedef  void (*check_fun_type)( void *, int, size_t, int *, int *, int *,
                                 int *, int * );



/* Prototypes for private functions defined within this file. */
/* ---------------------------------------------------------- */

/* Use macros to define the functions required for all the combinations
   of input and output data types. Floating point values cannot be compressed
   using delta compression, so exclude _DOUBLE and _REAL from the list of
   supported input data types. Also, the compressed values must be
   integers and must be signed (since we need to be able to store negative
   deltas). So the list of output data tpes is just _INTEGER, _WORD and
   _BYTE. */

#define MAKE_PROTOA(incode,intype,outcode,outtype) \
   static void ary1Delt##incode##outcode( intype *pindata, int nel, \
                                          size_t stride, outtype *poutdata, \
                                          intype *pvalue, int *prepeat, \
                                          int *ndata, int *nvalue, \
                                          int *nrepeat, int *status ); \
\
   static void ary1Check##incode##outcode( intype *pindata, int nel, \
                                           size_t stride, int *ndata, \
                                           int *nvalue, int *nrepeat, \
                                           int *max_repeat, int *status );

#define MAKE_PROTOB(outcode,outtype) \
   MAKE_PROTOA(I,int,outcode,outtype) \
   MAKE_PROTOA(W,short int,outcode,outtype) \
   MAKE_PROTOA(UW,unsigned short int,outcode,outtype) \
   MAKE_PROTOA(B,char,outcode,outtype) \
   MAKE_PROTOA(UB,unsigned char,outcode,outtype)

MAKE_PROTOB(I,int)
MAKE_PROTOB(W,short int)
MAKE_PROTOB(B,char)

#undef MAKE_PROTOA
#undef MAKE_PROTOB







F77_SUBROUTINE(ary1_s2dlt)( CHARACTER(LOC1), INTEGER(ZAXIS), CHARACTER(TYPE),
                            CHARACTER(LOC2), REAL(ZRATIO), INTEGER(STATUS)
                            TRAIL(LOC1) TRAIL(TYPE) TRAIL(LOC2) ) {
/*
*+
*  Name:
*     ARY1_S2DLT

*  Purpose:
*     Convert a simple, scaled or primitive array to a delta compressed array.

*  Language:
*     ANSI C

*  Invocation:
*     CALL ARY1_S2DLT( LOC1, ZAXIS, TYPE, LOC2, ZRATIO, STATUS )

*  Description:
*     This routine compresses a simple, scaled or primtive array to produce
*     a corresponding delta compressed array, stored within a supplied output
*     structure.
*
*     See the inverse function, ary1_dlt2s.c, for a description of the
*     structure of a delta compressed array.

*  Arguments:
*     LOC1 = CHARACTER * ( DAT__SZLOC ) (Given)
*        An HDS locator for a structure holding a simple, scaled or
*        primitive array. The data type of this array must be an integer
*        type (signed or unsigned), otherwise an error will be reported.
*     ZAXIS = INTEGER (Given)
*        The one-based index of the pixel axis within the input along
*        which the compression is to occur. An error is reported if the
*        specified axis spans only a single pixel.
*     TYPE = CHARACTER * ( DAT__SZTYP ) (Given)
*        The HDS data type of the compressed array. Must be one of
*        _INTEGER, _WORD or _BYTE, otherwise an error is reported.
*     LOC2 = CHARACTER * ( DAT__SZLOC ) (Given)
*        An HDS locator for a structure into which the components of
*        the output delta compressed array will be written. If this is
*        DAT__NOLOC, then no array is created, but SIZE is still returned.
*     ZRATIO = REAL (Returned)
*        The compresison ratio - the ratio of the uncompressed array size to
*        the compressed array size. If LOC2 is DAT__NOLOC, a value will
*        still be returned but will be estimated by looking at only 20% of
*        the input data. This approximation is done to save time.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     19-OCT-2010 (DSB):
*        Original version.
*     2010-11-30 (TIMJ):
*        Avoid dereferencing off the end of an array in the check functions
*     1-DEC-2010 (DSB):
*        Take care with casting of size_t values used in floating point calculations. 
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Arguments Given: */
   GENPTR_CHARACTER(LOC1)
   GENPTR_INTEGER(ZAXIS)
   GENPTR_CHARACTER(TYPE)
   GENPTR_CHARACTER(LOC2)

/* Arguments Returned: */
   GENPTR_REAL(ZRATIO)

/* Status: */
   GENPTR_INTEGER(STATUS)

/* Local Variables; */
   HDSLoc *loc1 = NULL;
   HDSLoc *loc2 = NULL;
   HDSLoc *loc_data = NULL;
   HDSLoc *loc_firstd = NULL;
   HDSLoc *loc_firstr = NULL;
   HDSLoc *loc_firstv = NULL;
   HDSLoc *loc_indata = NULL;
   HDSLoc *loc_repeat = NULL;
   HDSLoc *loc_temp = NULL;
   HDSLoc *loc_value = NULL;
   char *ptr_data = NULL;
   char *ptr_indata = NULL;
   char *ptr_value = NULL;
   char type[DAT__SZTYP + 1];
   char type_indata[DAT__SZTYP + 1];
   char variant[ VARIANT_LEN + 1 ];
   check_fun_type check_fun = NULL;
   const char *type_temp;
   delt_fun_type delt_fun = NULL;
   hdsdim dims_cindata[ ARY__MXDIM ];
   hdsdim dims_first[ ARY__MXDIM - 1 ];
   hdsdim dims_indata[ ARY__MXDIM ];
   hdsdim start[ ARY__MXDIM ];
   int *ptr_firstd = NULL;
   int *ptr_firstr = NULL;
   int *ptr_firstv = NULL;
   int *ptr_repeat = NULL;
   int idata;
   int idim;
   int irepeat;
   int isprim;
   int ivalue;
   int max_repeat;
   int ndata;
   int ndim;
   int nrepeat;
   int nvalue;
   int row_inc;
   int size_temp;
   int there;
   int zaxis;
   int zdim;
   size_t bsize;
   size_t div_indata[ ARY__MXDIM ];
   size_t irow;
   size_t iv_indata;
   size_t nel_data;
   size_t nel_first;
   size_t nel_firstd;
   size_t nel_firstr;
   size_t nel_firstv;
   size_t nel_indata;
   size_t nel_repeat;
   size_t nel_value;
   size_t next_row;
   size_t ntest_row;
   size_t size_intype;
   size_t size_outtype;
   size_t stride_indata[ ARY__MXDIM ];
   size_t zstride;

/* Initialise */
   *ZRATIO = 1.0;

/* Check inherited status. */
   if ( *STATUS != SAI__OK ) return;



/* The next section verifies the input. */
/* ------------------------------------ */

/* Import the Fortran TYPE string. */
   cnfImpn( TYPE, TYPE_length, DAT__SZTYP, type );

/* Report an error if the output (compressed) data type is not a signed
   integer type. */
   if( strcmp( type, "_INTEGER" ) && strcmp( type, "_WORD" ) &&
       strcmp( type, "_BYTE" ) && *STATUS == SAI__OK ) {
      *STATUS = ARY__TYPIN;
      msgSetc( "T", type );
      errRep( "", "Cannot create a ^T array using delta compression.",
              STATUS );
      goto L999;
   }

/* Import the supplied F77 locators. */
   datImportFloc( LOC1, LOC1_length, &loc1, STATUS );
   datImportFloc( LOC2, LOC2_length, &loc2, STATUS );

/* Get a locator for the input DATA array. If the input is a primitive array,
   then just clone the supplied locator. */
   datPrim( loc1, &isprim, STATUS );
   if( isprim ) {
      datClone( loc1, &loc_indata, STATUS );
   } else {
      datFind( loc1, "DATA", &loc_indata, STATUS );
   }

/* Finds the dimensions of the array. */
   datShape( loc_indata, ARY__MXDIM, dims_indata, &ndim, STATUS );

/* Get its type. */
   datType( loc_indata, type_indata, STATUS );

/* Report an error if the input data type is not an integer type. */
   if( ( !strcmp( type_indata, "_DOUBLE" ) ||
         !strcmp( type_indata, "_REAL" ) ) && *STATUS == SAI__OK ) {
      *STATUS = ARY__TYPIN;
      msgSetc( "T", type_indata );
      errRep( "", "Cannot apply delta compression to a ^T array.",
              STATUS );
      goto L999;
   }

/* Report an error if the compression axis is out of bounds. */
   if( ( *ZAXIS < 1 || *ZAXIS > ndim ) && *STATUS == SAI__OK ) {
      *STATUS = ARY__AXINV;
      msgSeti( "Z", *ZAXIS );
      msgSeti( "N", ndim );
      errRep( "", "Cannot compress a ^N-D array along axis ^Z.",
              STATUS );
      goto L999;
   }

/* Get the zero-based compression axis index. */
   zaxis = *ZAXIS - 1;

/* Report an error if the compression axis spans only a single pixel. */
   zdim =  (int) dims_indata[ zaxis ];
   if( zdim == 1 && *STATUS == SAI__OK ) {
      *STATUS = ARY__DIMIN;
      msgSeti( "I", *ZAXIS );
      errRep( "", "Cannot compress along axis ^I because it spans only a "
              "single pixel.", STATUS );
      goto L999;
   }

/* Get the number of bytes in one element of the input DATA array. */
   size_intype = SIZEOF( type_indata );

/* Get the number of bytes in one element of the output DATA array. */
   size_outtype = SIZEOF( type );

/* Map the input DATA array. */
   datMapV( loc_indata, type_indata, "READ", (void **) &ptr_indata,
            &nel_indata, STATUS );



/* The next section puts the simple components into the output. */
/* ------------------------------------------------------------ */

/* Do not create anything if we are just evaluating the size of the
   compressed arary. */
   if( loc2 ) {

/* If the input has an ORIGIN component, copy it to the output. */
      if( !isprim ) {
         datThere( loc1, "ORIGIN", &there, STATUS );
         if( there ) {
            datFind( loc1, "ORIGIN", &loc_temp, STATUS );
            datCopy( loc_temp, loc2, "ORIGIN", STATUS );
            datAnnul( &loc_temp, STATUS );
         }

/* If the input is a SCALED array, copy the SCALE and ZERO components
   to the output. Report an error if either does not exist. */
         datThere( loc1, "VARIANT", &there, STATUS );
         if( there ) {
            datFind( loc1, "VARIANT", &loc_temp, STATUS );
            datGet0C( loc_temp, variant, VARIANT_LEN, STATUS );
            datAnnul( &loc_temp, STATUS );

            if( !strcmp( variant, "SCALED" ) ) {

               datThere( loc1, "SCALE", &there, STATUS );
               if( there ) {
                  datFind( loc1, "SCALE", &loc_temp, STATUS );
                  datCopy( loc_temp, loc2, "SCALE", STATUS );
                  datAnnul( &loc_temp, STATUS );
               } else if( *STATUS == SAI__OK ) {
                  *STATUS = ARY__SCLIN;
                  datMsg( "A", loc1 );
                  errRep( "", "The SCALE component is missing from the "
                          "scaled array structure '^A'.", STATUS );
                  goto L999;
               }

               datThere( loc1, "ZERO", &there, STATUS );
               if( there ) {
                  datFind( loc1, "ZERO", &loc_temp, STATUS );
                  datCopy( loc_temp, loc2, "ZERO", STATUS );
                  datAnnul( &loc_temp, STATUS );
               } else if( *STATUS == SAI__OK ) {
                  *STATUS = ARY__SCLIN;
                  datMsg( "A", loc1 );
                  errRep( "", "The ZERO component is missing from the "
                          "scaled array structure '^A'.", STATUS );
                  goto L999;
               }

            }
         }
      }

/* Put the VARIANT component into the output delta compressed array */
      datNew0C( loc2, "VARIANT", 5, STATUS );
      datFind( loc2, "VARIANT", &loc_temp, STATUS );
      datPut0C( loc_temp, "DELTA", STATUS );
      datAnnul( &loc_temp, STATUS );

/* Put the ZAXIS and ZDIM components into the output delta compressed
   array */
      datNew0I( loc2, "ZAXIS", STATUS );
      datFind( loc2, "ZAXIS", &loc_temp, STATUS );
      datPut0I( loc_temp, *ZAXIS, STATUS );
      datAnnul( &loc_temp, STATUS );

      datNew0I( loc2, "ZDIM", STATUS );
      datFind( loc2, "ZDIM", &loc_temp, STATUS );
      datPut0I( loc_temp, zdim, STATUS );
      datAnnul( &loc_temp, STATUS );
   }



/* The next section gets pointers to the functions that do the work
   for the required combination of input and output data type. */
/* ---------------------------------------------------------------- */
#define ASSIGN_FUNS(incode,outcode) \
            delt_fun = (delt_fun_type) ary1Delt##incode##outcode; \
            check_fun = (check_fun_type) ary1Check##incode##outcode;

#define CHOOSE_FUNS(outcode) \
      if( !strcmp( type_indata, "_INTEGER" ) ) { \
         ASSIGN_FUNS(I,outcode) \
\
      } else if( !strcmp( type_indata, "_WORD" ) ) { \
         ASSIGN_FUNS(W,outcode) \
\
      } else if( !strcmp( type_indata, "_UWORD" ) ) { \
         ASSIGN_FUNS(UW,outcode) \
\
      } else if( !strcmp( type_indata, "_BYTE" ) ) { \
         ASSIGN_FUNS(B,outcode) \
\
      } else if( !strcmp( type_indata, "_UBYTE" ) ) { \
         ASSIGN_FUNS(UB,outcode) \
\
      } else if( *STATUS == SAI__OK ) { \
         *STATUS = ARY__FATIN; \
         msgSetc( "T", type_indata ); \
         errRep( "", "ARY1_S2DLT: Unsupported input data type '^T' " \
                 "(programming error).", STATUS ); \
         goto L999; \
      }

   if( !strcmp( type, "_INTEGER" ) ) {
      CHOOSE_FUNS(I)

   } else if( !strcmp( type, "_WORD" ) ) {
      CHOOSE_FUNS(W)

   } else if( !strcmp( type, "_BYTE" ) ) {
      CHOOSE_FUNS(B)

   } else if( *STATUS == SAI__OK ) {
      *STATUS = ARY__FATIN;
      msgSetc( "T", type );
      errRep( "", "ARY1_S2DLT: Unsupported output data type '^T' "
              "(programming error).", STATUS );
      goto L999;
   }

/* Delete the macro */
#undef ASSIGN_FUNS
#undef CHOOSE_FUNS



/* The next section determines the sizes for the output arrays. */
/* ------------------------------------------------------------ */

/* Determine the bounds and size for the FIRST_xxx output arrays. These
   arrays are shaped like a single slice from the input data array,
   perpendicular to the compression axis. */
   nel_first = 1;
   for( idim = 0; idim < zaxis; idim++ ) {
      nel_first *= dims_indata[ idim ];
      dims_first[ idim ] = dims_indata[ idim ];
   }

   for( idim = zaxis + 1; idim < ndim; idim++ ) {
      nel_first *= dims_indata[ idim ];
      dims_first[ idim - 1 ] = dims_indata[ idim ];
   }

/* Initialise the pixel indices at the start of the first row of input pixels
   to be checked (each row is parallel to the compression axis). Also get a
   copy of the dimensions collaped to 1 along the compression axis. */
   for( idim = 0; idim < ndim; idim++ ) {
      start[ idim ] = 1;
      dims_cindata[ idim ] = dims_indata[ idim ];
   }
   dims_cindata[ zaxis ] = 1;

/* Find the stride between adjacent samples on each axis of the input
   array, in units of bytes. */
   stride_indata[ 0 ] = size_intype;
   for( idim = 1; idim < ndim; idim++ ) {
      stride_indata[ idim ] = stride_indata[ idim - 1 ]*dims_indata[ idim - 1 ];
   }

/* Get the stride on the intput array compression axis in units of input
   array elements. */
   zstride = stride_indata[ zaxis ]/size_intype;

/* Evaluate constants to avoid repeated calculation of them in the following
   loop. These are the change in vector index needed when moving from
   dimension "idim" to dimension "idim+1" when looping round all the row
   starts, in units of bytes. */
   for( idim = 0; idim < ndim - 1; idim++ ) {
      div_indata[ idim ] = stride_indata[ idim + 1 ]
                           - stride_indata[ idim ]*dims_cindata[ idim ];
   }
   div_indata[ idim ] = 1;

/* Initialise the vector index (into the input array) at the start of the
   current row of input pixels parallel to the compression axis. */
   iv_indata = 0;

/* Initialise indices into the DATA, VALUE and REPEAT arrays. */
   idata = 0;
   ivalue = 0;
   irepeat = 0;

/* Initialise the largest number of repeated values (i.e. the largest
   value that must be stored in REPEAT). */
   max_repeat = 0;

/* Abort if an error has occurred. */
   if( *STATUS != SAI__OK ) goto L999;

/* Check the first row to see how many elements are required to
   describe it within the DATA, VALUE and REPEAT arrays. */
   (*check_fun)( ptr_indata + iv_indata, zdim, zstride, &ndata, &nvalue,
                 &nrepeat, &max_repeat, STATUS );

/* If we are not creating an output array, we can base the returned
   compression estimate on a subset of the data, on the assumption that
   the statistical properties of the data will be more or less uniform
   across the data set. This speeds things up. Find the increment in row
   number that will mean about 20% of the data is tested, and find the
   resulting number of test rows. */
   if( !loc2 && nel_first > 5 ) {
      row_inc = 5;
   } else {
      row_inc = 1;
   }

/* Get the index of the next row to be checked. */
   next_row = row_inc;
   ntest_row = 1;

/* Loop round all remaining rows of pixels that are parallel to the
   compression axis. */
   for( irow = 1; irow < nel_first; irow++ ) {

/* Increment the number of DATA, VALUE and REPEAT values required so far. */
      idata += ndata;
      ivalue += nvalue;
      irepeat += nrepeat;

/* Update the pixel indices at the start of the row so that they refer
   to the next row of the input array. Also update the vector index into
   the input array at which the row starts. */
      idim = 0;
      iv_indata += stride_indata[ idim ];
      while( ++start[ idim ] > dims_cindata[ idim ] ) {
         iv_indata += div_indata[ idim ];
         start[ idim ] = 1;
         idim++;
      }

/* Check the next row to see how many elements are required to describe it
   within the DATA, VALUE and REPEAT arrays. */
      if( irow == next_row ) {
         (*check_fun)( ptr_indata + iv_indata, zdim, zstride, &ndata,
                       &nvalue, &nrepeat, &max_repeat, STATUS );
         next_row += row_inc;
         ntest_row++;
      } else {
         ndata = 0;
         nvalue = 0;
         nrepeat = 0;
      }
   }


/* Finalise the number of DATA, VALUE and REPEAT values required. */
   idata += ndata;
   ivalue += nvalue;
   irepeat += nrepeat;



/* The next section creates the output arrays. */
/* ------------------------------------------- */

/* If we are creating an output, create the required output arrays, find
   them and map them. We find the size of the output, even if we do not
   create it. */
   if( loc2 ) {
      datNew( loc2, "DATA", type, 1, &idata, STATUS );
      datFind( loc2, "DATA", &loc_data, STATUS );
      datMapV( loc_data, type, "WRITE", (void **) &ptr_data, &nel_data,
               STATUS );
   }
   bsize = idata*size_outtype;

   if( loc2 ) {
      datNew( loc2, "FIRST_DATA", "_INTEGER", ndim - 1, dims_first, STATUS );
      datFind( loc2, "FIRST_DATA", &loc_firstd, STATUS );
      datMapV( loc_firstd, "_INTEGER", "WRITE", (void **) &ptr_firstd,
               &nel_firstd, STATUS );
   }
   bsize += ntest_row*VAL__NBI;

   if( loc2 ) {
      datNew( loc2, "VALUE", type_indata, 1, &ivalue, STATUS );
      datFind( loc2, "VALUE", &loc_value, STATUS );
      datMapV( loc_value, type_indata, "WRITE", (void **) &ptr_value,
               &nel_value, STATUS );
   }
   bsize += ivalue*size_intype;

   if( ivalue > VAL__MAXUW ) {
      type_temp = "_INTEGER";
      size_temp = VAL__NBI;
   } else if( ivalue > VAL__MAXUB ) {
      type_temp = "_UWORD";
      size_temp = VAL__NBUW;
   } else {
      type_temp = "_UBYTE";
      size_temp = VAL__NBUB;
   }

   if( loc2 ) {
      datNew( loc2, "FIRST_VALUE", type_temp, ndim - 1, dims_first, STATUS );
      datFind( loc2, "FIRST_VALUE", &loc_firstv, STATUS );
      datMapV( loc_firstv, "_INTEGER", "WRITE", (void **) &ptr_firstv,
               &nel_firstv, STATUS );
   }
   bsize += ntest_row*size_temp;

   if( irepeat ) {

      if( max_repeat > VAL__MAXUW ) {
         type_temp = "_INTEGER";
         size_temp = VAL__NBI;
      } else if( max_repeat > VAL__MAXUB ) {
         type_temp = "_UWORD";
         size_temp = VAL__NBUW;
      } else {
         type_temp = "_UBYTE";
         size_temp = VAL__NBUB;
      }

      if( loc2 ) {
         datNew( loc2, "REPEAT", type_temp, 1, &irepeat, STATUS );
         datFind( loc2, "REPEAT", &loc_repeat, STATUS );
         datMapV( loc_repeat, "_INTEGER", "WRITE", (void **) &ptr_repeat,
                  &nel_repeat, STATUS );
      }
      bsize += irepeat*size_temp;

      if( irepeat > VAL__MAXUW ) {
         type_temp = "_INTEGER";
         size_temp = VAL__NBI;
      } else if( irepeat > VAL__MAXUB ) {
         type_temp = "_UWORD";
         size_temp = VAL__NBUW;
      } else {
         type_temp = "_UBYTE";
         size_temp = VAL__NBUB;
      }

      if( loc2 ) {
         datNew( loc2, "FIRST_REPEAT", type_temp, ndim - 1, dims_first, STATUS );
         datFind( loc2, "FIRST_REPEAT", &loc_firstr, STATUS );
         datMapV( loc_firstr, "_INTEGER", "WRITE", (void **) &ptr_firstr,
                  &nel_firstr, STATUS );
      }
      bsize += ntest_row*size_temp;
   }

/* Calculate the compressed ratio and return it in argument ZRATIO. */
   *ZRATIO = ( (float)nel_indata*(float)size_intype )/
             ( 2*VAL__NBI + ((float)bsize*(float)nel_first)/(float)ntest_row );


/* The next section stores the compressed values in the output arrays. */
/* ------------------------------------------------------------------- */

/* We can skip this section if we are not actually creating an output
   compressed array. */
   if( loc2 && *STATUS == SAI__OK ) {

/* Initialise the vector index (into the input array) at the start of the
   current row of pixels parallel to the compression axis. */
      iv_indata = 0;

/* Initialise the pixel indices at the start of the first row of input pixels
   to be compressed (each row is parallel to the compression axis). */
      for( idim = 0; idim < ndim; idim++ ) start[ idim ] = 1;

/* Initialise indices into the DATA, VALUE and REPEAT arrays. */
      *ptr_firstd = idata = 0;
      *ptr_firstv = ivalue = 0;
      if( ptr_firstr ) *ptr_firstr = irepeat = 0;

/* Compress the first row, putting the compressed values into the
   output array. */
      (*delt_fun)( ptr_indata + iv_indata, zdim, zstride, ptr_data, ptr_value,
                   ptr_repeat, &ndata, &nvalue, &nrepeat, STATUS );

/* Loop round all remaining rows of input pixels that are parallel to the
   compression axis. */
      for( irow = 1; irow < nel_first; irow++ ) {

/* Increment the number of DATA, VALUE and REPEAT values stored so far. */
         idata += ndata;
         ivalue += nvalue;
         irepeat += nrepeat;

/* Increment the corresponding pointers (declared as "char *" rather than
   "void *" to avoid compiler warnings about pointer arithmetic). */
         ptr_data += ndata*size_outtype;
         ptr_value += nvalue*size_intype;
         ptr_repeat += nrepeat;

/* Store the index within DATA, VALUE and REPEAT of the first element needed
   to uncompress the next row. */
         *(++ptr_firstd) = idata;
         *(++ptr_firstv) = ivalue;
         if( ptr_firstr ) *(++ptr_firstr) = irepeat;

/* Update the pixel indices at the start of the row so that they refer
   to the next row. Also update the vector index into the input array at
   which the row starts. */
         idim = 0;
         iv_indata += stride_indata[ idim ];
         while( ++start[ idim ] > dims_cindata[ idim ] ) {
            iv_indata += div_indata[ idim ];
            start[ idim ] = 1;
            idim++;
         }

/* Compress the next row, putting the compressed values into the output
   array. */
         (*delt_fun)( ptr_indata + iv_indata, zdim, zstride, ptr_data,
                      ptr_value, ptr_repeat, &ndata, &nvalue, &nrepeat,
                      STATUS );
      }

/* Store the compressed ratio in the DELTA array. */
      datNew0R( loc2, "ZRATIO", STATUS );
      datFind( loc2, "ZRATIO", &loc_temp, STATUS );
      datPut0R( loc_temp, *ZRATIO, STATUS );
      datAnnul( &loc_temp, STATUS );
   }

/* The next section tidies up. */
/* --------------------------- */

L999:

/* Annul all local locators. */
   datAnnul( &loc_indata, STATUS );
   if( loc_data ) datAnnul( &loc_data, STATUS );
   if( loc_value ) datAnnul( &loc_value, STATUS );
   if( loc_repeat ) datAnnul( &loc_repeat, STATUS );
   if( loc_firstd ) datAnnul( &loc_firstd, STATUS );
   if( loc_firstv ) datAnnul( &loc_firstv, STATUS );
   if( loc_firstr ) datAnnul( &loc_firstr, STATUS );

/* We need to free the HDSLoc structures created at the start by
   datImportFloc. The only way to do this seems to be to export the locators
   back to Fortran, even though they have not been changed. */
   datExportFloc( &loc1, 1, LOC1_length, LOC1, STATUS );
   if( loc2 ) datExportFloc( &loc2, 1, LOC2_length, LOC2, STATUS );

/* Report a context error if anything went wrong. */
   if( *STATUS != SAI__OK ) {
      errRep( "", "ARY1_S2DLT: Failed to convert a simple array into a delta "
              "compressed array.", STATUS );
   }
}






/*
*  Name:
*     ary1Delt<TIN><TOUT>

*  Purpose:
*     Delta compress a row of pixel values.

*  Invocation:
*     void ary1Delt<TIN><TOUT>( <TIN> *pindata, int nel, size_t stride,
*                               <TOUT> *poutdata, <TIN> *pvalue,
*                               int *prepeat, int *ndata, int *nvalue,
*                               int *nrepeat, int *status )

*  Description:
*     This family of functions delta compresses a row of input pixel
*     values. Each member of the family handles a different combination
*     of input and output data type. The output compressed values
*     are contiguous. The input uncompressed values will only be contiguous
*     if the compression axis is the first axis.

*  Arguments:
*     pindata = <TIN> *
*        Pointer to the first value to be compressed.
*     nel = int
*        The number of values to read from "pindata".
*     stride = size_t
*        The stride between adjacent values to be compressed within the
*        "pindata" array.
*     poutdata = <TOUT> *
*        Array in which to place the compressed values.
*     pvalue = <TIN> *
*        Array in which to store individual uncompressed values.
*     prepeat = int *
*        Array in which to store the number of repeats for each value in
*        "pvalue" that is flagged as REPEAT_GOOD or REPEAT_BAD in the
*        compressed DATA array.
*     ndata = int *
*        Pointer to an int in which to return the number of values stored
*        in the "poutdata" array.
*     nvalue = int *
*        Pointer to an int in which to return the number of values stored
*        in the "pvalue" array.
*     nrepeat = int *
*        Pointer to an int in which to return the number of values stored
*        in the "prepeat" array.
*     status
*        Pointer to the global status.

*/

/* Macro to push any deferred SINGLE_GOOD values onto the end of poutdata.
   If there are three or more of them, we can save space by replacing
   them with a single MULTI_GOOD value. */
#define CHECK_SINGLE(outcode) \
         if( nsingle > 2 ) { \
            *(poutdata++) = MULTI_GOOD##outcode; \
            *(prepeat++) = nsingle; \
            nsingle = 0; \
         } else if( nsingle ) { \
            *(poutdata++) = SINGLE_GOOD##outcode; \
            if( nsingle == 2 ) *(poutdata++) = SINGLE_GOOD##outcode; \
            nsingle = 0; \
         }

#define MAKE_FUNA(incode,intype,outcode,outtype) \
\
static void ary1Delt##incode##outcode( intype *pindata, int nel, \
                                       size_t stride, outtype *poutdata, \
                                       intype *pvalue, int *prepeat, \
                                       int *ndata, int *nvalue, \
                                       int *nrepeat, int *status ){ \
\
/* Local Variables: */ \
   int *prepeat0; \
   int iel; \
   int nbad_repeat; \
   int ngood_repeat; \
   int nsingle; \
   intype *pvalue0; \
   intype vdata; \
   intype vnext; \
   intype vprev2 = 0; \
   intype vprev = 0; \
   long int delta; \
   outtype *poutdata0; \
\
/* Initialise */ \
   *ndata = 0; \
   *nvalue = 0; \
   *nrepeat = 0; \
   nbad_repeat = 0; \
   ngood_repeat = 0; \
   nsingle = 0; \
\
/* Check inherited status. */ \
   if( *status != SAI__OK ) return; \
\
/* Record the supplied pointers */ \
   poutdata0 = poutdata; \
   pvalue0 = pvalue; \
   prepeat0 = prepeat; \
\
/* Get the first input value. */ \
   vdata = *pindata; \
\
/* Increment the pointer and get the second input value. */ \
   pindata += stride; \
   vnext = *pindata; \
\
/* If the first input pixel is bad, set the number of bad pixels in the \
   current run of bad pixels to one. We do nothing else until we reach the \
   end of the run. */ \
   if( vdata == VAL__BAD##incode ){ \
      nbad_repeat = 1; \
\
/* If the first input pixel is good, and is equal to the next input pixel, \
   we are starting a run of equal good pixel qvalues, so set the count of \
   pixels in the current run to one . We do nothing else until we reach the \
   end of the run. */ \
   } else if( vdata == vnext ) { \
      ngood_repeat = 1; \
\
/* If the first input pixel is good, and is not equal to the second input \
   pixel, flag it as SINGLE_GOOD and add it to the VALUE array. We do not \
   actually store the SINGLE_GOOD valuein the DATA array yet, as there are \
   often many SINGLE_GOOD values next to each other, which we can combine \
   into a MULTI_GOOD value. So for the moment, just count how many \
   SINGLE_GOOD values there are. */ \
   } else { \
      nsingle = 1; \
      *(pvalue++) = vdata; \
   } \
\
/* Check all the elements in the supplied row from the second element to \
   the penultimate element. We exclude first and last elements from this \
   loop as checking each element will in general require access to both the \
   previous and the following elements. Note, each time we push something \
   onto the end of poutdata, we need to check first to see if there are any \
   single values waiting to be pushed first. We defer pushing of single \
   values because they commonly occur in groups, and defering them means \
   we can group them together into a single MULTI_GOOD element, thus saving \
   space. */ \
   nel--; \
   for( iel = 1; iel < nel; iel++ ) { \
\
/* Move on to next element ("vdata" holds the next element, \
   "vprev" holds the previous and "vnext" holds the third). */ \
      pindata += stride; \
      vprev = vdata; \
      vdata = vnext; \
      vnext = *pindata; \
\
/* If this input pixel is bad, increment the number of bad pixels in the \
   current run of bad pixels. We do nothing else until we reach the end \
   of the run. */ \
      if( vdata == VAL__BAD##incode ){ \
         nbad_repeat++; \
\
/* If this input pixel is good, and it is the first good pixel following \
   a run of just one bad pixel, flag it as SINGLE_BAD and add it to the \
   VALUES array. Reset the count of previous bad pixels. We first push \
   any outstanding SINGLE_GOOD values onto the output. */ \
      } else if( nbad_repeat == 1 ) { \
         CHECK_SINGLE(outcode) \
         *(poutdata++) = SINGLE_BAD##outcode; \
         *(pvalue++) = vdata; \
         nbad_repeat = 0; \
\
/* If this input pixel is good, and it is the first good pixel following \
   a run of more than one bad pixel, flag it as REPEAT_BAD, and add it to \
   the VALUES array. update "max_repeat" and reset the count of previous \
   bad pixels. */ \
      } else if( nbad_repeat > 1 ) { \
         CHECK_SINGLE(outcode) \
         *(poutdata++) = REPEAT_BAD##outcode; \
         *(pvalue++) = vdata; \
         *(prepeat++) = nbad_repeat; \
         nbad_repeat = 0; \
\
/* If the current and previous input pixel are both good, and the current \
   input pixel is equal to the next input pixel, we are in a run of equal \
   good pixel values, so increment the count of pixels in the current run. \
   We do nothing else until we reach the end of the run. */ \
      } else if( vdata == vnext ) { \
         ngood_repeat++; \
\
/* If the current and previous input pixel are both good, and the current \
   input pixel is not equal to the following input pixel, but is equal to \
   the previous input pixel, we may have reached the end of the current run \
   of good values. If the run is at least three pixels long, flag it as \
   REPEAT_GOOD, otherwise add appropriate delta values to the DATA \
   array (since the overheads of using REPEAT_GOOD will be larger than the \
   saving for runs of only two values). Then update max_repeat and reset \
   the count of values in the current run. */ \
      } else if( vdata == vprev && ngood_repeat > 0 ) { \
         ngood_repeat++; \
         if( ngood_repeat > 2 ) { \
            CHECK_SINGLE(outcode) \
            *(poutdata++) = REPEAT_GOOD##outcode; \
            *(pvalue++) = vdata; \
            *(prepeat++) = ngood_repeat; \
\
         } else if( iel > 1 ) { \
            delta = vprev - vprev2; \
            if( delta < MIN_DELTA##outcode || \
                delta > MAX_DELTA##outcode ) { \
               nsingle++; \
               *(pvalue++) = vprev; \
            } else { \
               CHECK_SINGLE(outcode) \
               *(poutdata++) = delta; \
            } \
            CHECK_SINGLE(outcode) \
            *(poutdata++) = 0; \
\
/* iel is 1 so vprev2 is not defined and and nsingle must be zero */ \
         } else { \
            *(pvalue++) = vprev; \
            *(poutdata++) = SINGLE_GOOD##outcode; \
            *(poutdata++) = 0; \
         } \
         ngood_repeat = 0; \
\
/* If the current and previous input pixel are both good, and the current \
   input pixel is not equal to either of its neighbours, we will store the \
   increment from previous to current value in the DATA array so long as \
   the increment does not exceed the data range of DATA. If it does, then \
   the current value is flagged as SINGLE_GOOD and added to the VALUES list. */ \
      } else { \
         delta = vdata - vprev;     /* delta is a "long int" so the \
                                             difference will not overflow */ \
         if( delta < MIN_DELTA##outcode || \
             delta > MAX_DELTA##outcode ) { \
            nsingle++; \
            *(pvalue++) = vdata; \
          } else { \
            CHECK_SINGLE(outcode) \
            *(poutdata++) = delta; \
          } \
      } \
\
/* Store previous previous */\
      vprev2 = vprev; \
   } \
\
/* Update value of vdata and vprev for end of loop */\
   vprev = vdata;\
   vdata = vnext;\
\
/* The "v..." variables now refer to the last pixel in the input array. If \
   the last pixel is bad, we must treat VAL__BAD as a special sort of good \
   value, since the SINGLE_BAD and REPEAT_BAD flags imply that there are \
   more values to follow. */ \
   if( vdata == VAL__BAD##incode ){ \
\
/* If the previous value was good, flag the last value as SINGLE_GOOD \
   and store VAL__BAD in the VALUES list. */ \
      if( !nbad_repeat ) { \
         nsingle++; \
         *(pvalue++) = VAL__BAD##incode; \
\
/* If the last value is bad and is the end of a run of two bad values, \
   flag the last value as SINGLE_BAD and store VAL__BAD in the VALUES list. */ \
      } else if( nbad_repeat == 1 ) { \
         CHECK_SINGLE(outcode) \
         *(poutdata++) = SINGLE_BAD##outcode; \
         *(pvalue++) = VAL__BAD##incode; \
\
/* If the last value is bad and is the end of a run of more than two bad \
   values, flag the last value as REPEAT_BAD and store VAL__BAD in the \
   VALUES list. */ \
      } else { \
         CHECK_SINGLE(outcode) \
         *(poutdata++) = REPEAT_BAD##outcode; \
         *(pvalue++) = VAL__BAD##incode; \
         *(prepeat++) = nbad_repeat; \
      } \
\
/* If the last pixel is good, and is the first good value following a run \
   of just one bad value, flag it as SINGLE_BAD, and add it to the VALUES \
   array. */ \
   } else if( nbad_repeat == 1 ) { \
      CHECK_SINGLE(outcode) \
      *(poutdata++) = SINGLE_BAD##outcode; \
      *(pvalue++) = vdata; \
\
/* If the last pixel is good, and it is the first good pixel following a \
   run of more than one bad pixel, flag it as REPEAT_BAD, and add it to \
   the VALUES array. */ \
   } else if( nbad_repeat > 1 ) { \
      CHECK_SINGLE(outcode) \
      *(poutdata++) = REPEAT_BAD##outcode; \
      *(pvalue++) = vdata; \
      *(prepeat++) = nbad_repeat; \
\
/* If the last and previous input pixel are both good, and are equal, \
   we have reached the end of a run of equal good values, so flag the last \
   value as REPEAT_GOOD, and add it to the VALUES list. But if the run \
   only contains two values it is cheaper to store them as single values. */ \
   } else if( vdata == vprev ) { \
      ngood_repeat++; \
      if( ngood_repeat > 2 ) { \
         CHECK_SINGLE(outcode) \
         *(poutdata++) = REPEAT_GOOD##outcode; \
         *(pvalue++) = vdata; \
         *(prepeat++) = ngood_repeat; \
\
      } else { \
          delta = vprev - vprev2; \
          if( delta < MIN_DELTA##outcode || \
              delta > MAX_DELTA##outcode ) { \
            nsingle++; \
            *(pvalue++) = vprev; \
          } else { \
            CHECK_SINGLE(outcode) \
            *(poutdata++) = delta; \
          } \
          CHECK_SINGLE(outcode) \
          *(poutdata++) = 0; \
      } \
\
/* If the last and previous input pixel are both good, but are not equal, \
   then store a delta value in DATA if it is within range, or flag the \
   value as SINGLE_GOOD otherwise. */ \
   } else { \
      delta = vdata - vprev;  /* delta is a "long int" so the \
                                 difference will not overflow */ \
      if( delta < MIN_DELTA##outcode || \
          delta > MAX_DELTA##outcode ) { \
         nsingle++; \
         *(pvalue++) = vdata; \
      } else { \
         CHECK_SINGLE(outcode) \
         *(poutdata++) = delta; \
      } \
   } \
\
/* Do a final check for deferred single values. */ \
   CHECK_SINGLE(outcode) \
\
/* Return the number of values added into each array. */ \
   *ndata = poutdata - poutdata0; \
   *nvalue = pvalue - pvalue0; \
   *nrepeat = prepeat - prepeat0; \
\
}

/* Use the above macro to define the functions for all required
   combinations on input and output data type. */
#define MAKE_FUNB(outcode,outtype) \
   MAKE_FUNA(I,int,outcode,outtype) \
   MAKE_FUNA(W,short int,outcode,outtype) \
   MAKE_FUNA(UW,unsigned short int,outcode,outtype) \
   MAKE_FUNA(B,char,outcode,outtype) \
   MAKE_FUNA(UB,unsigned char,outcode,outtype)

MAKE_FUNB(I,int)
MAKE_FUNB(W,short int)
MAKE_FUNB(B,char)

#undef MAKE_FUNA
#undef MAKE_FUNB
#undef CHECK_SINGLE







/*
*  Name:
*     ary1Check<TIN><TOUT>

*  Purpose:
*     Check how much data is needed to describe a row of pixel values.

*  Invocation:
*     void ary1Check<TIN><TOUT>( <TIN> *pindata, int nel, size_t stride,
*                                int *ndata, int *nvalue, int *nrepeat,
*                                int *max_repeat, int *status );

*  Description:
*     This family of functions check how much data is needed to store a
*     compressed row of input pixel values. Each member of the family handles
*     a different combination of input and output data type.

*  Arguments:
*     pindata = <TIN> *
*        Pointer to the first value to be compressed.
*     nel = int
*        The number of values to read from "pindata".
*     stride = size_t
*        The stride between adjacent values read from "pindata".
*     ndata = int *
*        Pointer to an int in which to return the number of values that
*        must be added to the compressed DATA array to describe the row.
*     nvalue = int *
*        Pointer to an int in which to return the number of values that
*        must be added to the VALUES array to describe the row.
*     nrepeat = int *
*        Pointer to an int in which to return the number of values that
*        must be added to the REPEAT array to describe the row.
*     max_repeat = int *
*        Pointer to an int in which to return the maximum value to be
*        stored in the REPEAT array. The supplied value is left unchanged
*        if the row does not require a larger REPEAT value.
*     status
*        Pointer to the global status.

*/

/* Macro to push any deferred SINGLE_GOOD values onto the end of poutdata.
   If there are three or more of them, we can save space by replacing
   them with a single MULTI_GOOD value. */
#define CHECK_SINGLE \
         if( nsingle > 2 ) { \
            (*ndata)++; \
            (*nrepeat)++; \
            if( nsingle > *max_repeat ) *max_repeat = nsingle; \
            nsingle = 0; \
         } else if( nsingle ) { \
            (*ndata) += nsingle; \
            nsingle = 0; \
         }

#define MAKE_FUNA(incode,intype,outcode,outtype) \
\
static void ary1Check##incode##outcode( intype *pindata, int nel, \
                                        size_t stride, int *ndata, \
                                        int *nvalue, int *nrepeat, \
                                        int *max_repeat, int *status ){ \
\
/* Local Variables: */ \
   int iel; \
   int nbad_repeat; \
   int ngood_repeat; \
   int nsingle; \
   intype vdata; \
   intype vnext; \
   intype vprev2 = 0; \
   intype vprev = 0; \
   long int delta; \
\
/* Initialise */ \
   *ndata = 0; \
   *nvalue = 0; \
   *nrepeat = 0; \
   nbad_repeat = 0; \
   ngood_repeat = 0; \
   nsingle = 0; \
\
/* Check inherited status. */ \
   if( *status != SAI__OK ) return; \
\
/* Get the first input value. */ \
   vdata = *pindata; \
\
/* Increment the pointer and get the second input value. */ \
   pindata += stride; \
   vnext = *pindata; \
\
/* If the first input pixel is bad, set the number of bad pixels in the \
   current run of bad pixels to one. We do nothing else until we reach the end \
   of the run. */ \
   if( vdata == VAL__BAD##incode ){ \
      nbad_repeat = 1; \
\
/* If the first input pixel is good, and is equal to the next input pixel, \
   we are starting a run of equal good pixel values, so set the count of \
   pixels in the current run to one . We do nothing else until we reach the \
   end of the run. */ \
   } else if( vdata == vnext ) { \
      ngood_repeat = 1; \
\
/* If the first input pixel is good, and is not equal to the second input \
   pixel, flag it as SINGLE_GOOD and add it to the VALUE array. We do not \
   actually store the SINGLE_GOOD valuein the DATA array yet, as there are \
   often many SINGLE_GOOD values next to each other, which we can combine \
   into a MULTI_GOOD value. So for the moment, just count how many \
   SINGLE_GOOD values there are. */ \
   } else { \
      nsingle = 1; \
      (*nvalue)++; \
   } \
\
/* Check all the elements in the supplied row from the second element to \
   the penultimate element. We exclude first and last elements from this \
   loop as checking each element will in general require access to both the \
   previous and the following elements. */ \
   nel--; \
   for( iel = 1; iel < nel; iel++ ) { \
\
/* Move on to next element ("vdata" holds the next element, \
   "vprev" holds the previous and "vnext" holds the third). */ \
      pindata += stride; \
      vprev = vdata; \
      vdata = vnext; \
      vnext = *pindata; \
\
/* If this input pixel is bad, increment the number of bad pixels in the \
   current run of bad pixels. We do nothing else until we reach the end \
   of the run. */ \
      if( vdata == VAL__BAD##incode ){ \
         nbad_repeat++; \
\
/* If this input pixel is good, and it is the first good pixel following \
   a run of just one bad pixel, flag it as SINGLE_BAD and add it to the \
   VALUES array. Reset the count of previous bad pixels. */ \
      } else if( nbad_repeat == 1 ) { \
         CHECK_SINGLE \
         (*ndata)++; \
         (*nvalue)++; \
         nbad_repeat = 0; \
\
/* If this input pixel is good, and it is the first good pixel following \
   a run of more than one bad pixel, flag it as REPEAT_BAD, and add it to \
   the VALUES array. Update "max_repeat" and reset the count of previous \
   bad pixels. */ \
      } else if( nbad_repeat > 1 ) { \
         CHECK_SINGLE \
         (*ndata)++; \
         (*nvalue)++; \
         (*nrepeat)++; \
         if( nbad_repeat > *max_repeat ) *max_repeat = nbad_repeat; \
         nbad_repeat = 0; \
\
/* If the current and previous input pixel are both good, and the current \
   input pixel is equal to the next input pixel, we are in a run of equal \
   good pixel values, so increment the count of pixels in the current run. \
   We do nothing else until we reach the end of the run. */ \
      } else if( vdata == vnext ) { \
         ngood_repeat++; \
\
/* If the current and previous input pixel are both good, and the current \
   input pixel is not equal to the following input pixel, but is equal to \
   the previous input pixel, we may have reached the end of the current run \
   of good values. If the run is at least three pixels long, flag it as \
   REPEAT_GOOD, otherwise add appropriate delta values to the DATA \
   array (since the overheads of using REPEAT_GOOD will be larger than the \
   saving for runs of only two values). Then update max_repeat and reset \
   the count of values in the current run. */ \
      } else if( vdata == vprev && ngood_repeat > 0 ) { \
         ngood_repeat++; \
         if( ngood_repeat > 2 ) { \
            CHECK_SINGLE \
            (*ndata)++; \
            (*nvalue)++; \
            (*nrepeat)++; \
            if( ngood_repeat > *max_repeat ) *max_repeat = ngood_repeat; \
\
         } else if( iel > 1 ) { \
            delta = vprev - vprev2; \
            if( delta < MIN_DELTA##outcode || \
                delta > MAX_DELTA##outcode ) { \
               nsingle++; \
               (*nvalue)++; \
            } else { \
               CHECK_SINGLE \
               (*ndata)++; \
            } \
            CHECK_SINGLE \
            (*ndata)++; \
\
/* iel is 1 so vprev2 is not defined and and nsingle must be zero */ \
         } else { \
            *ndata += 2; \
            (*nvalue)++; \
         } \
         ngood_repeat = 0; \
\
/* If the current and previous input pixel are both good, and the current \
   input pixel is not equal to either of its neighbours, we will store the \
   increment from previous to current value in the DATA array so long as \
   the increment does not exceed the data range of DATA. If it does, then \
   the current value is flagged as SINGLE_GOOD and added to the VALUES list. */ \
      } else { \
         delta = vdata - vprev;  /* delta is a "long int" so the \
                                    difference will not overflow */ \
         if( delta < MIN_DELTA##outcode || \
             delta > MAX_DELTA##outcode ) { \
            nsingle++; \
            (*nvalue)++; \
         } else { \
            CHECK_SINGLE \
            (*ndata)++; \
         } \
      } \
\
/* Store previous previous element. */ \
      vprev2 = vprev; \
   } \
\
/* Update value of vdata and vprev for end of loop */\
   vprev = vdata;\
   vdata = vnext;\
\
/* The "v..." variables now refer to the last pixel in the input array. If \
   the last pixel is bad, we must treat VAL__BAD as a special sort of good \
   value, since the SINGLE_BAD and REPEAT_BAD flags imply that there are \
   more values to follow. */ \
   if( vdata == VAL__BAD##incode ){ \
\
/* If the previous value was good, flag the last value as SINGLE_GOOD \
   and store VAL__BAD in the VALUES list. */ \
      if( !nbad_repeat ) { \
         nsingle++; \
         (*nvalue)++; \
\
/* If the last value is bad and is the end of a run of two bad values, \
   flag the last value as SINGLE_BAD and store VAL__BAD in the VALUES list. */ \
      } else if( nbad_repeat == 1 ) { \
         CHECK_SINGLE \
         (*ndata)++; \
         (*nvalue)++; \
\
/* If the last value is bad and is the end of a run of more than two bad \
   values, flag the last value as REPEAT_BAD and store VAL__BAD in the \
   VALUES list. */ \
      } else { \
         CHECK_SINGLE \
         (*ndata)++; \
         (*nvalue)++; \
         (*nrepeat)++; \
         if( nbad_repeat > *max_repeat ) *max_repeat = nbad_repeat; \
      } \
\
/* If the last pixel is good, and is the first good value following a run \
   of just one bad value, flag it as SINGLE_BAD, and add it to the VALUES \
   array. */ \
   } else if( nbad_repeat == 1 ) { \
      CHECK_SINGLE \
      (*ndata)++; \
      (*nvalue)++; \
\
/* If the last pixel is good, and it is the first good pixel following a \
   run of more than one bad pixel, flag it as REPEAT_BAD, and add it to \
   the VALUES array. */ \
   } else if( nbad_repeat > 1 ) { \
      CHECK_SINGLE \
      (*ndata)++; \
      (*nvalue)++; \
      (*nrepeat)++; \
      if( nbad_repeat > *max_repeat ) *max_repeat = nbad_repeat; \
\
/* If the last and previous input pixel are both good, and are equal, \
   we have reached the end of a run of equal good values, so flag the last \
   value as REPEAT_GOOD, and add it to the VALUES list. But if the run \
   only contains two values it is cheaper to store them as single values. */ \
   } else if( vdata == vprev ) { \
      ngood_repeat++; \
      if( ngood_repeat > 2 ) { \
         CHECK_SINGLE \
         (*ndata)++; \
         (*nvalue)++; \
         (*nrepeat)++; \
         if( ngood_repeat > *max_repeat ) *max_repeat = ngood_repeat; \
      } else { \
         delta = vprev - vprev2; \
         if( delta < MIN_DELTA##outcode || \
             delta > MAX_DELTA##outcode ) { \
            nsingle++; \
            (*nvalue)++; \
         } else { \
            CHECK_SINGLE \
            (*ndata)++; \
         } \
         CHECK_SINGLE \
         (*ndata)++; \
      } \
      ngood_repeat = 0; \
\
/* If the last and previous input pixel are both good, but are not equal, \
   then store a delta value in DATA if it is within range, or flag the \
   value as SINGLE_GOOD otherwise. */ \
   } else { \
      delta = vdata - vprev; \
      if( delta < MIN_DELTA##outcode || \
          delta > MAX_DELTA##outcode ) { \
         nsingle++; \
         (*nvalue)++; \
      } else { \
         CHECK_SINGLE \
         (*ndata)++; \
      } \
   } \
\
/* Do a final check for deferred single values. */ \
   CHECK_SINGLE \
}



/* Use the above macro to define the functions for all required
   combinations on input and output data type. */
#define MAKE_FUNB(outcode,outtype) \
   MAKE_FUNA(I,int,outcode,outtype) \
   MAKE_FUNA(W,short int,outcode,outtype) \
   MAKE_FUNA(UW,unsigned short int,outcode,outtype) \
   MAKE_FUNA(B,char,outcode,outtype) \
   MAKE_FUNA(UB,unsigned char,outcode,outtype)

MAKE_FUNB(I,int)
MAKE_FUNB(W,short int)
MAKE_FUNB(B,char)

#undef MAKE_FUNA
#undef MAKE_FUNB
#undef CHECK_SINGLE




