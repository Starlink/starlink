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

/* Returns the number of bytes in an HDS type, or -1 if the HDS type is
   not supported */
#define SIZEOF(thistype) ( \
   !strcmp( thistype, "_DOUBLE" ) ? VAL__NBD : ( \
   !strcmp( thistype, "_REAL" ) ? VAL__NBR : ( \
   !strcmp( thistype, "_INTEGER" ) ? VAL__NBI : ( \
   !strcmp( thistype, "_WORD" ) ? VAL__NBW : ( \
   !strcmp( thistype, "_UWORD" ) ? VAL__NBUW : ( \
   !strcmp( thistype,"_UBYTE") ? VAL__NBUB : ( \
   !strcmp( thistype,"_BYTE") ? VAL__NBB : -1 )))))))



/* Type definitions. */
/* ----------------- */
/* Type for the compression function that accepts void * pointers for
   arrays. */
typedef  void (*undelt_fun_type)( void *, size_t, size_t, void *, void *,
                                  void *, int *, void *, size_t, int *, int * );


/* Prototypes for private functions defined within this file. */
/* ---------------------------------------------------------- */

/* Use macros to define the functions required for all the combinations of
   input and output data types. The compressed values must be integers and
   must be signed (since we need to be able to store negative deltas). So
   the list of input data tpes is just _INTEGER, _WORD and _BYTE. We also
   define separate functions for scaled and unscaled arrays. */

#define MAKE_PROTOA(incode,intype,outcode,outtype,vcode,vtype,scaled) \
   static void ary1Undelt##incode##outcode##vcode##scaled( intype *pdata, \
                                            size_t row_lbnd, \
                                            size_t row_ubnd, outtype *scale, \
                                            outtype *zero, vtype *pvalue, \
                                            int *prepeat, outtype *pout, \
                                            size_t stride, int *bad, \
                                            int *status );

/* Use the above macro to define the functions for both scaled and
   unscaled versions for a specific input and output data type. */
#define MAKE_PROTOB(incode,intype,outcode,outtype,vcode,vtype) \
   MAKE_PROTOA(incode,intype,outcode,outtype,vcode,vtype,0) \
   MAKE_PROTOA(incode,intype,outcode,outtype,vcode,vtype,1)

#define MAKE_PROTOC(outcode,outtype,vcode,vtype) \
   MAKE_PROTOB(I,int,outcode,outtype,vcode,vtype) \
   MAKE_PROTOB(W,short int,outcode,outtype,vcode,vtype) \
   MAKE_PROTOB(B,char,outcode,outtype,vcode,vtype)

#define MAKE_PROTOD(vcode,vtype) \
   MAKE_PROTOC(D,double,vcode,vtype) \
   MAKE_PROTOC(R,float,vcode,vtype) \
   MAKE_PROTOC(I,int,vcode,vtype) \
   MAKE_PROTOC(W,short int,vcode,vtype) \
   MAKE_PROTOC(UW,unsigned short int,vcode,vtype) \
   MAKE_PROTOC(B,char,vcode,vtype) \
   MAKE_PROTOC(UB,unsigned char,vcode,vtype)

MAKE_PROTOD(I,int)
MAKE_PROTOD(W,short int)
MAKE_PROTOD(UW,unsigned short int)
MAKE_PROTOD(B,char)
MAKE_PROTOD(UB,unsigned char)

#undef MAKE_PROTOA
#undef MAKE_PROTOB
#undef MAKE_PROTOC
#undef MAKE_PROTOD








F77_SUBROUTINE(ary1_undlt)( CHARACTER(LOC1), INTEGER(NDIM),
                            INTEGER_ARRAY(LBND), INTEGER_ARRAY(UBND),
                            POINTER(PNTR), LOGICAL(BAD), INTEGER(STATUS)
                            TRAIL(LOC1) TRAIL(TYPE) ) {
/*
*+
*  Name:
*     ARY1_UNDLT

*  Purpose:
*     Uncompress a section of a delta compressed array.

*  Language:
*     ANSI C

*  Invocation:
*     CALL ARY1_UNDLT( LOC1, NDIM, LBND, UBND, PNTR, BAD,
*                      STATUS )

*  Description:
*     This routine uncompresses a section of a delta compressed array,
*     storing the results in a supplied array. The section to be copied
*     must not extend outside the bounds of the uncompressed array, and
*     must have the same number of dimensions as the uncompressed array.

*  Arguments:
*     LOC1 = CHARACTER * ( DAT__SZLOC ) (Given)
*        An HDS locator for a structure holding a delta compressed array.
*        See "The DELTA array struture" below.
*     NDIM = INTEGER (Given)
*        The size of the LBND and UBND arrays. This should be equal to
*        the number of axes in the uncompressed array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower bounds of the HDS section to be copied. These must fall
*        within the bounds of the delta array. Note, these are bounds within
*        the HDS array and so are relative to a fixed origin of (1,1,1,...).
*        The supplied LBND values should therefore be no less than 1 on
*        each axis.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper bounds of the section to be copied. These must fall
*        within the bounds of the delta array. Note, these are bounds within
*        the HDS array and so are relative to a fixed origin of (1,1,1,...).
*     PNTR = INTEGER (Given)
*        A CNF pointer to the array in which to store the uncompressed
*        data values. The data type of this array should match the data
*        type of the VALUES component within the delta compressed array
*        structure. The number of elements in this array should equal the
*        product of the dimensions implied by LBND and UBND.
*     BAD = LOGICAL (Returned)
*        .TRUE. if any bad values are present in the output uncompressed
*        array, and .FALSE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  The DELTA array structure:
*     The HDS structure of a DELTA array is similar to the SIMPLE array,
*     in that it will contain VARIANT, DATA and ORIGIN components. In
*     addition they can contain SCALE and ZERO terms, which, if present,
*     are used to scale the uncompressed integers as in a SCALED array.
*     Uncompression happens first, producing an array of uncompressed
*     integers, which are then unscaled if required using SCALE and ZERO to
*     produce the final uncompressed, unscaled, array.
*
*     DELTA arrays cannot be used to hold complex values and so no
*     IMAGINARY_DATA component will be present. Also, DELTA arrays have an
*     implicit value of .TRUE. for their bad pixel flags, and so no BAD_PIXEL
*     component will be present in the HDS structure.
*
*     The DATA array holds the differences between adjacent values in
*     the uncompressed integer array. The differences are taken along
*     a specific pixel axis, which is recorded in the ZAXIS component.
*     These differences usually have a more restricted range than the
*     full data values, and so can be stored in an integer array with
*     shorter word length. This is where (most of) the compression comes
*     from. However, it could well be that some fraction of the
*     differences exceed the range of the data type chosen for the DATA
*     array. The full uncompressed value of such pixels are stored in a
*     separate array, and the DATA array then contains a flag saying that
*     the full value should be read from the separate array.
*
*     In addition, if many adjacent pixels have the same uncompressed
*     value, then this value is stored in another array, together with a
*     count of how many times the value occurs. A flag in then included
*     in the DATA array indicating that the repeated value should be
*     obtained from the separate array and inserted into the uncompressed
*     array.
*
*     Sometimes, the user may request access to a subsection of the
*     uncompressed array rather than the whole array. To avoid the
*     overhead of uncompressing the whole array in such cases, the array
*     is compressed line by line, so that a single line can be
*     uncompressed by itself. Here, a "line" is a row of pixels, parallel
*     to the ZAXIS axis,  that extends across the full width of the
*     uncompressed array.
*
*     The following components are allowed to appear in the HDS structure:
*
*     DATA: A 1D vector of _INTEGER, _WORD or _BYTE values. Each value is
*        either the increment in value from the previous element in the
*        uncompressed array [so VAL(I) = VAL(I-1) + DATA(I) ] (which is
*        why unsigned integer types cannot be used), or one of the following
*        special flag values (see ary_dlt.h):
*
*        - SINGLE_BAD<X>: the next element of the uncompressed array
*        is bad, but the following element is good and its full uncompressed
*        value should be read from the next element of the VALUE array.
*
*        - SINGLE_GOOD<X>: the next element of the uncompressed array
*        is good, but cannot be expressed as a difference from the previous
*        element because the difference would not fit into the available data
*        range of the DATA array. Instead, the full uncompressed value should
*        be read from the next element of the VALUE array.
*
*        - REPEAT_GOOD<X>: the next element of the uncompressed array is
*        good and is exactly equal to the following "N-1" elements. The full
*        uncompressed value should be read from the next element of the
*        VALUE array. The value of N should be read from the next element
*        of the REPEAT array.
*
*        - REPEAT_BAD<X>: the next element of the uncompressed array is bad,
*        as are the following "N-1" elements. The full uncompressed value
*        of the next good value should be read from the next element of the
*        VALUE array. The value of N should be read from the next element of
*        the REPEAT array.
*
*        - MULTI_GOOD<X>: the next "N" elements of the uncompressed array
*        are good but cannot be expressed as differences from the previous
*        element because the differences would not fit into the available
*        data range of the DATA array. Instead, the full uncompressed
*        values should be read from the next "N" elements of the VALUE array.
*        The value of N should be read from the next element of the REPEAT
*        array.
*
*        Note 1), the "available data range" in DATA is reduced to leave room
*        for these flags.
*
*        Note 2), the first element in each "hyper-row" of pixels parallel
*        to ZAXIS is always represented using one of these flag values, so
*        that the whole hyper-row of pixel values can be uncompressed without
*        reference to any earlier values. See FIRST_DATA below.
*
*        Note 3), repeated runs of good or bad value are always contained
*        within a single hyper-row. Runs of repeated values that cross the
*        boundary between adjacent hyper-rows are split into two repeated
*        runs - one for each hyper-row.
*
*     ORIGIN: The pixel indices of the first element of the uncompressed array.
*        Assumed to be [1,1,1...] if not present.
*
*     SCALE: An optional component giving a scale factor to apply to the
*        uncompressed integer values. It can be of any data type. If
*        present the uncompressed array is treated like a SCALED array.
*        In particular, the data type of the uncompressed array will be
*        the same as the data type of the SCALE component, if present. If
*        not present, the data type of the uncompressed array is given by
*        the data type of the VALUE array.
*
*     ZERO: An optional component giving a zero offset to add to the
*        uncompressed integer values. It can be of any data type. If
*        present the uncompressed array is treated like a SCALED array.
*
*     ZAXIS: _INTEGER scalar, giving the pixel axis index within the
*        uncompressed array along which differences were taken. Care should be
*        taken in the choice of ZAXIS since it can affect the degree of
*        compresion achieved. If ZAXIS is not specified when compressing an
*        array, it should default to the axis that gives the greatest
*        compression. Note, the ZAXIS value is one-based, not zero-based.
*
*     ZDIM: _INTEGER scalar holding the length of the ZAXIS axis of the
*        uncompressed array. The other dimensions of the uncompressed
*        array are given by the shape of the FIRST_DATA array.
*
*     VALUE: 1D vector with the same data type as the uncompressed array
*        (_INTEGER, _WORD, _UWORD, _BYTE or _UBYTE) prior to scaling by
*        SCALE and ZERO. It holds full uncompressed integer values for the
*        elements that are flagged with any of the special values listed
*        under "DATA" above. VALUE will always be present in the structure
*        since it will at least be needed to hold the values at the start
*        of each hyper-row parallel to ZAXIS. Note, if SCALE and ZERO
*        components are present in the DELTA array, the VALUE array holds
*        internal scaled values, rather than external unscaled values.
*
*     REPEAT: 1D integer vector holding the number of repetitions for each
*        value associated with an occurrence of REPEAT_GOOD or REPEAT_BAD in
*        the DATA array. The data type of this array will be an integer
*        type just large enough to hold the largest value. This array will
*        not be present if there are no runs in the uncompressed data
*        array.
*
*     FIRST_DATA: _INTEGER array with NDIM-1 axes in the same order as the
*        axes of the uncompressed array, but omitting the ZAXIS axis. It
*        holds the zero-based index into the DATA array at which the first
*        element of the corresponding hyper-row (i.e. the row of values
*        parallel to ZAXIS) is stored.
*
*     FIRST_VALUE: integer (of any type) array with NDIM-1 axes in the same
*        order as the axes of the uncompressed array, but omitting the ZAXIS
*        axis. It holds the zero-based index of the first element of the
*        VALUE array to be used when uncompressing the corresponding
*        hyper-row.
*
*     FIRST_REPEAT: integer (of any type) array with NDIM-1 axes in the same
*        order as the axes of the uncompressed array, but omitting the ZAXIS
*        axis. It holds the zero-based index of the first element of the
*        REPEAT array to be used when uncompressing the corresponding
*        hyper-row. This array should be present if and only if the REPEAT
*        array is present.
*
*     ZRATIO: A _REAL value giving the compression factor - that is, the
*        ratio of the uncompressed array size to the compressed array size.
*        This is approximate as it does not include the effects of the
*        metadata needed to describe the extra components of a DELTA array
*        (i.e. the space needed to hold the component names, types,
*        dimensions, etc).

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-OCT-2010 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Arguments Given: */
   GENPTR_CHARACTER(LOC1)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(LBND)
   GENPTR_INTEGER_ARRAY(UBND)
   GENPTR_POINTER(PNTR)

/* Arguments Returned: */
   GENPTR_LOGICAL(BAD)

/* Status: */
   GENPTR_INTEGER(STATUS)

/* Local Variables; */
   HDSLoc *loc1 = NULL;
   HDSLoc *loc_badpixel = NULL;
   HDSLoc *loc_data = NULL;
   HDSLoc *loc_firstd = NULL;
   HDSLoc *loc_firstr = NULL;
   HDSLoc *loc_firstv = NULL;
   HDSLoc *loc_repeat = NULL;
   HDSLoc *loc_scale = NULL;
   HDSLoc *loc_slice = NULL;
   HDSLoc *loc_value = NULL;
   HDSLoc *loc_zaxis = NULL;
   HDSLoc *loc_zdim = NULL;
   HDSLoc *loc_zero = NULL;
   char *pdata;
   char *ptr_data = NULL;
   char *ptr_out = NULL;
   char *ptr_value = NULL;
   char *pvalue;
   char scale_buf[ 20 ];
   char type[DAT__SZTYP + 1];
   char type_data[DAT__SZTYP + 1];
   char type_value[DAT__SZTYP + 1];
   char zero_buf[ 20 ];
   hdsdim dims[ ARY__MXDIM ];
   hdsdim dims_csection[ ARY__MXDIM ];
   hdsdim dims_cwhole[ ARY__MXDIM ];
   hdsdim dims_firstd[ ARY__MXDIM - 1 ];
   hdsdim dims_firstr[ ARY__MXDIM - 1 ];
   hdsdim dims_firstv[ ARY__MXDIM - 1 ];
   hdsdim dims_out[ ARY__MXDIM ];
   hdsdim dims_section[ ARY__MXDIM ];
   hdsdim dlb;
   hdsdim dub;
   hdsdim lbnd_csection[ ARY__MXDIM ];
   hdsdim lbnd_cwhole[ ARY__MXDIM ];
   hdsdim start[ ARY__MXDIM + 1 ];
   hdsdim ubnd_csection[ ARY__MXDIM + 1 ];
   int *prepeat;
   int *ptr_firstd = NULL;
   int *ptr_firstr = NULL;
   int *ptr_firstv = NULL;
   int *ptr_repeat = NULL;
   int bad;
   int idim;
   int ndim;
   int ndim_firstr;
   int ndim_firstv;
   int there;
   int whole;
   int zaxis;
   int zdim;
   size_t div_cwhole[ ARY__MXDIM ];
   size_t div_section[ ARY__MXDIM ];
   size_t iv_csection;
   size_t iv_cwhole;
   size_t iv_section;
   size_t last_csection;
   size_t ndata;
   size_t nel_csection;
   size_t nel_data;
   size_t nel_firstd;
   size_t nel_firstr;
   size_t nel_firstv;
   size_t nel_out;
   size_t nel_repeat;
   size_t nel_value;
   size_t offset;
   size_t size_intype;
   size_t size_outtype;
   size_t size_vtype;
   size_t stride_cwhole[ ARY__MXDIM ];
   size_t stride_section[ ARY__MXDIM ];
   size_t zstride;
   undelt_fun_type undelt_fun;

/* Initialise */
   *BAD = F77_FALSE;

/* Check inherited status. */
   if ( *STATUS != SAI__OK ) return;

/* Import the supplied F77 locator. */
   datImportFloc( LOC1, LOC1_length, &loc1, STATUS );

/* Get a C pointer from the supplied Fortran pointer.*/
   ptr_out = cnfCptr( *PNTR );

/* Get locators to components of the supplied input structure that will
   always exist and will definitely be needed. */
   datFind( loc1, "ZAXIS", &loc_zaxis, STATUS );
   datFind( loc1, "ZDIM", &loc_zdim, STATUS );
   datFind( loc1, "FIRST_DATA", &loc_firstd, STATUS );
   datFind( loc1, "DATA", &loc_data, STATUS );
   datFind( loc1, "VALUE", &loc_value, STATUS );

/* Get the data type of the input (compressed) DATA array, and get the
   number of bytes per DATA value (i.e.the number of bytes used to store the
   difference between adjacent uncompressed integer values). */
   datType( loc_data, type_data, STATUS );
   size_intype = SIZEOF( type_data );

/* If the SCALE and ZERO terms exist, we will definitely need them, so
   get locators to them, and copy their values into generic buffers. */
   datThere( loc1, "SCALE", &there, STATUS );
   if( there ) {
      datFind( loc1, "SCALE", &loc_scale, STATUS );
      datType( loc_scale, type, STATUS );
      datGet( loc_scale, type, 0, NULL, scale_buf, STATUS );

      datThere( loc1, "ZERO", &there, STATUS );
      if( there ) {
         datFind( loc1, "ZERO", &loc_zero, STATUS );
         datGet( loc_zero, type, 0, NULL, zero_buf, STATUS );

      } else if( *STATUS == SAI__OK ) {
         *STATUS = ARY__DLTIN;
         datMsg( "A", loc1 );
         errRep( "", "The compressed array '^A' is invalid - the ZERO "
                 "component is mising.", STATUS );
         goto L999;
      }

   } else {
      datThere( loc1, "ZERO", &there, STATUS );
      if( there && *STATUS == SAI__OK ) {
         *STATUS = ARY__DLTIN;
         datMsg( "A", loc1 );
         errRep( "", "The compressed array '^A' is invalid - the SCALE "
                 "component is mising.", STATUS );
         goto L999;
      }
   }

/* Get the data type of the output (uncompressed) array. This is given by
   the data type of the SCALE component if present, and by the data type
   of the VALUE component otherwise. */
   datType( loc_value, type_value, STATUS );
   if( !loc_scale ) strcpy( type, type_value );

/* Map the VALUE array since we know we will need it. */
   datMapV( loc_value, type_value, "READ", (void **) &ptr_value, &nel_value, STATUS );

/* Get the size in bytes of a single uncompressed integer value prior to
   any additional scaling. */
   size_vtype = SIZEOF( type_value );

/* Get the size in bytes of a single uncompressed value including any additional
   scaling. */
   size_outtype = SIZEOF( type );

/* If the REPEAT array exists, we will definitely need it. So find it and
   map it. */
   datThere( loc1, "REPEAT", &there, STATUS );
   if( there ) {
      datFind( loc1, "REPEAT", &loc_repeat, STATUS );
      datMapV( loc_repeat, "_INTEGER", "READ", (void **) &ptr_repeat,
               &nel_repeat, STATUS );
   }

/* Get the index of the compressed axis, and convert to a zero-based
   index. */
   datGet0I( loc_zaxis, &zaxis, STATUS );
   zaxis--;

/* Get the size of the uncompressed array along the compression axis, and
   report an error if is is not positive. */
   datGet0I( loc_zdim, &zdim, STATUS );
   if( zdim < 1 && *STATUS == SAI__OK ) {
      *STATUS = ARY__DLTIN;
      datMsg( "A", loc1 );
      msgSeti( "I", zdim );
      errRep( "", "The compressed array '^A' is invalid - the ZDIM "
              "value (^I) is invalid.", STATUS );
      goto L999;
   }

/* Get the number and lengths of the other axes. These are defined by the
   FIRST_DATA array. */
   datShape( loc_firstd, ARY__MXDIM - 1, dims_firstd, &ndim, STATUS );

/* Increment the number of axes in FIRST_DATA to get the number of axes in
   the uncompressed array. */
   ndim++;

/* Report an error if the dimensionality of the supplied bounds is
   different. */
   if( ndim != *NDIM && *STATUS == SAI__OK ) {
      *STATUS = ARY__FATIN;
      datMsg( "A", loc1 );
      msgSeti( "N", *NDIM );
      msgSeti( "M", ndim );
      errRep( "", "ARY1_UNDLT: error uncompressed array '^A' - the "
              "supplied bounds have ^N elements but the array has "
              "^M axes (internal programming error).", STATUS );
      goto L999;
   }

/* Check the compression axis index is within the range of axis indices
   allowed by the value of "ndim". Note, zaxis is zero-based at this point. */
   if( ( zaxis < 0 || zaxis >= ndim ) && *STATUS == SAI__OK ) {
      *STATUS = ARY__DLTIN;
      datMsg( "A", loc1 );
      msgSeti( "I", zaxis + 1 );
      errRep( "", "The compressed array '^A' is invalid - the ZAXIS "
              "value (^I) is invalid.", STATUS );
      goto L999;
   }

/* Abort if an error has occurred. */
   if( *STATUS != SAI__OK ) goto L999;

/* Get the bounds and dimensionality of the uncompressed array. The
   length of the ZAXIS is given by ZDIM. The lengths of the other axes
   are given by the shape of the FIRST_DATA array. */
   for( idim = 0; idim < zaxis; idim++ ) dims[ idim ] = dims_firstd[ idim ];
   dims[ zaxis ] = zdim;
   for( idim = zaxis + 1; idim < ndim; idim++ ) dims[ idim ] = dims_firstd[ idim - 1 ];

/* Set a flag indicating if the entire array is being uncompressed, in
   which case we can do some optimisations. */
   whole = 1;
   for( idim = 0; idim < ndim; idim++ ) {
      if( LBND[ idim ] != 1 || UBND[ idim ] != dims[ idim ] ) {
         whole = 0;
         break;
      }
   }

/* If we are copying the whole array, map the whole input DATA array.
   Otherwise, we will map slices from it as needed later on. Get the size
   anyway since it will always be needed. */
   if( whole ) {
      datMapV( loc_data, type_data, "READ", (void **) &ptr_data,
               &nel_data, STATUS );
   } else {
      datSize( loc_data, &nel_data, STATUS );
   }

/* Get the dimensions of the section being copied. Report an error if the
   section extends outside the area of the uncompressed array. Also, get
   the number of elements. */
   nel_out = 1;
   for( idim = 0; idim < ndim && *STATUS == SAI__OK; idim++ ) {

      if( LBND[ idim ] < 1 || UBND[ idim ] > dims[ idim ] ) {
         *STATUS = ARY__FATIN;
         datMsg( "A", loc1 );
         errRep( "", "ARY1_UNDLT: error uncompressed array '^A' - the "
                 "requested section extends outside the array (internal "
                 "programming error).", STATUS );
         goto L999;

      } else if( LBND[ idim ] > UBND[ idim ] ) {
         *STATUS = ARY__FATIN;
         datMsg( "A", loc1 );
         msgSeti( "B", LBND[ idim ] );
         msgSeti( "C", idim + 1 );
         msgSeti( "D", UBND[ idim ] );
         errRep( "", "ARY1_UNDLT: error uncompressed array '^A' - the "
                 "requested lower bound (^B) on axis ^C is greater than "
                 "the requested upper bound (^D) (programming error).",
                 STATUS );
         goto L999;

      } else {
         dims_out[ idim ] = UBND[ idim ] - LBND[ idim ] + 1;
         nel_out *= dims_out[ idim ];
      }
   }

/* Create a suitable DATA component in the output simple array structure,
   and map it. This will receive the uncompressed data values. */

/* Indicate we have not yet put any bad values into the output array. */
   bad = 0;

/* Get a pointer to the function that uncompresses a single contiguous
   run of compressed data values, for the required combination of input,
   output and value data types and scaling. Use macros to shorten this
   code. */
#define ASSIGN_FUN(incode,outcode,vcode,scale) \
               undelt_fun = (undelt_fun_type) ary1Undelt##incode##outcode##vcode##scale;

#define CHOOSE_FUNA(incode,outcode,vcode) \
            if( loc_scale ) { \
               ASSIGN_FUN(incode,outcode,vcode,1) \
            } else { \
               ASSIGN_FUN(incode,outcode,vcode,0) \
            }

#define CHOOSE_FUNB(outcode,vcode) \
         if( !strcmp( type_data, "_INTEGER" ) ) { \
            CHOOSE_FUNA(I,outcode,vcode) \
         } else if( !strcmp( type_data, "_WORD" ) ) { \
            CHOOSE_FUNA(W,outcode,vcode) \
         } else if( !strcmp( type_data, "_BYTE" ) ) { \
            CHOOSE_FUNA(B,outcode,vcode) \
         } else if( *STATUS == SAI__OK ) { \
            *STATUS = ARY__FATIN; \
            msgSetc( "T", type_data ); \
            errRep( "", "ARY1_UNDLT: Unsupported input data type '^T' " \
                    "(programming error).", STATUS ); \
            goto L999; \
         }

#define CHOOSE_FUNC(vcode) \
      if( !strcmp( type, "_DOUBLE" ) ) { \
         CHOOSE_FUNB(D,vcode) \
      } else if( !strcmp( type, "_REAL" ) ) { \
         CHOOSE_FUNB(R,vcode) \
      } else if( !strcmp( type, "_INTEGER" ) ) { \
         CHOOSE_FUNB(I,vcode) \
      } else if( !strcmp( type, "_WORD" ) ) { \
         CHOOSE_FUNB(W,vcode) \
      } else if( !strcmp( type, "_UWORD" ) ) { \
         CHOOSE_FUNB(UW,vcode) \
      } else if( !strcmp( type, "_BYTE" ) ) { \
         CHOOSE_FUNB(B,vcode) \
      } else if( !strcmp( type, "_UBYTE" ) ) { \
         CHOOSE_FUNB(UB,vcode) \
      } else if( *STATUS == SAI__OK ) { \
         *STATUS = ARY__FATIN; \
         msgSetc( "T", type ); \
         errRep( "", "ARY1_UNDLT: Unsupported output data type '^T' " \
                 "(programming error).", STATUS ); \
         goto L999; \
      }

   if( !strcmp( type_value, "_INTEGER" ) ) {
      CHOOSE_FUNC(I)
   } else if( !strcmp( type_value, "_WORD" ) ) {
      CHOOSE_FUNC(W)
   } else if( !strcmp( type_value, "_UWORD" ) ) {
      CHOOSE_FUNC(UW)
   } else if( !strcmp( type_value, "_BYTE" ) ) {
      CHOOSE_FUNC(B)
   } else if( !strcmp( type_value, "_UBYTE" ) ) {
      CHOOSE_FUNC(UB)
   } else if( *STATUS == SAI__OK ) {
      *STATUS = ARY__FATIN;
      msgSetc( "T", type );
      errRep( "", "ARY1_UNDLT: Unsupported VALUE data type '^T' "
              "(programming error).", STATUS );
      goto L999;
   }

/* Delete the macros */
#undef ASSIGN_FUN
#undef CHOOSE_FUNA
#undef CHOOSE_FUNB
#undef CHOOSE_FUNC

/* If the whole array is being uncompressed, and the compression axis
   is the first axis (so that no jumping around is required within the
   output array), we can disregard the three FIRST_xxx arrays. Uncompress all
   pixels, from first to last, in a single call, placing the uncompressed
   values in the output DATA array. */
   if( whole && zaxis == 0 ) {
      (*undelt_fun)( ptr_data, 0, nel_out - 1, scale_buf, zero_buf, ptr_value,
                     ptr_repeat, ptr_out, 1, &bad, STATUS );

/* If only part of the array is being uncompressed, we decompress just
   those hyper-rows that pass through the required section of the
   uncompressed array. */
   } else {

/* We now know we need to map the FIRST_DATA array. */
      datMapV( loc_firstd, "_INTEGER", "READ", (void **) &ptr_firstd,
               &nel_firstd, STATUS );

/* We also know we need the FIRST_VALUE array, so find it, map it, and
   report an error if it has a different shape to the FIRST_DATA array. */
      datFind( loc1, "FIRST_VALUE", &loc_firstv, STATUS );
      datMapV( loc_firstv, "_INTEGER", "READ", (void **) &ptr_firstv,
               &nel_firstv, STATUS );

      datShape( loc_firstv, ARY__MXDIM - 1, dims_firstv, &ndim_firstv,
                STATUS );
      if( ndim_firstv != ndim - 1 && *STATUS == SAI__OK ) {
         *STATUS = ARY__DLTIN;
         datMsg( "A", loc1 );
         errRep( "", "The compressed array '^A' is invalid - the "
                 "FIRST_VALUE and FIRST_DATA arrays have different "
                 "numbers of dimensions.", STATUS );
         goto L999;
      }

      for( idim = 0; idim < ndim_firstv && *STATUS == SAI__OK; idim++ ) {
         if( dims_firstv[ idim ] != dims_firstd[ idim ] ) {
            *STATUS = ARY__DLTIN;
            datMsg( "A", loc1 );
            errRep( "", "The compressed array '^A' is invalid - the "
                    "FIRST_VALUE and FIRST_DATA arrays have different "
                    "shapes.", STATUS );
            goto L999;
         }
      }

/* We also need the FIRST_REPEAT array but only if the REPEAT array exist.
   Find it and map it, and report an error if it has different dimensions
   to the FIRST_DATA array. */
      if( loc_repeat ) {
         datFind( loc1, "FIRST_REPEAT", &loc_firstr, STATUS );
         datMapV( loc_firstr, "_INTEGER", "READ", (void **) &ptr_firstr,
                  &nel_firstr, STATUS );

         datShape( loc_firstr, ARY__MXDIM - 1, dims_firstr, &ndim_firstr,
                   STATUS );
         if( ndim_firstr != ndim - 1 && *STATUS == SAI__OK ) {
            *STATUS = ARY__DLTIN;
            datMsg( "A", loc1 );
            errRep( "", "The compressed array '^A' is invalid - the "
                    "FIRST_REPEAT and FIRST_DATA arrays have different "
                    "numbers of dimensions.", STATUS );
            goto L999;
         }

         for( idim = 0; idim < ndim_firstr && *STATUS == SAI__OK; idim++ ) {
            if( dims_firstr[ idim ] != dims_firstd[ idim ] ) {
               *STATUS = ARY__DLTIN;
               datMsg( "A", loc1 );
               errRep( "", "The compressed array '^A' is invalid - the "
                       "FIRST_REPEAT and FIRST_DATA arrays have different "
                       "shapes.", STATUS );
               goto L999;
            }
         }

/* Report an error if the FIRST_REPEAT array exists but the REPEAT array
   does not exist. */
      } else {
         datThere( loc1, "FIRST_REPEAT", &there, STATUS );
         if( there && *STATUS == SAI__OK ) {
            *STATUS = ARY__DLTIN;
            datMsg( "A", loc1 );
            errRep( "", "The compressed array '^A' is invalid - the "
                    "REPEAT array is missing.", STATUS );
            goto L999;
         }
      }

/* We will loop round, uncompressing the required section of the whole
   array row-by-row (where each row is parallel to the compression axis),
   and copying the required part of each uncompressed row into the output
   array. We only uncompress rows that intersect the required section specified
   by LBND and UBND. */

/* Collapse the section bounds to its lower bound on the compression
   axis. Also get the dimensions of the whole uncompressed array, collapsed
   along the compression axis (this is congruent with the FIRST_xxx arrays,
   except that it has the full "ndim" axes, rather than "ndim-1", with the
   compression axis being a degenerate axis that spans only a single
   pixel). Also find the number of elements in the collapsed section,
   and initialise the pixel indicies of the current pixel (the "current
   pixel" is the pixel at the start of the row which is currently being
   uncompressed). */
      nel_csection = 1;
      for( idim = 0; idim < ndim; idim++ ) {
         start[ idim ] = lbnd_csection[ idim ] = LBND[ idim ];
         dims_section[ idim ] = UBND[ idim ] - LBND[ idim ] + 1;
         if( idim != zaxis ) {
            ubnd_csection[ idim ] = UBND[ idim ];
            lbnd_cwhole[ idim ] = LBND[ idim ];
            nel_csection *= dims_section[ idim ];
            dims_cwhole[ idim ] = dims[ idim ];
            dims_csection[ idim ] = dims_section[ idim ];
         } else {
            ubnd_csection[ idim ] = LBND[ idim ];
            lbnd_cwhole[ idim ] = 1;
            dims_cwhole[ zaxis ] = 1;
            dims_csection[ idim ] = 1;
         }
      }

/* To avoid valgrind warnings caused by the fact that the loop over
   rows advances to read one step beyond the last row (but doesn't write
   anything because the loop then terminates). */
      start[ idim ] = -1;
      ubnd_csection[ idim ] = 1;

/* Initialise the zero-based vector index of the current pixel into the
   collapsed whole array. Also find the strides between adjacent elements
   on each axis of the collapsed whole array and the uncollapsed section. */
      stride_cwhole[ 0 ] = 1;               /* Units of array elements */
      stride_section[ 0 ] = size_outtype;   /* Units of bytes (actually "chars") */
      iv_cwhole = lbnd_cwhole[ 0 ] - 1;
      for( idim = 1; idim < ndim; idim++ ) {
         stride_cwhole[ idim ] = stride_cwhole[ idim - 1 ]*dims_cwhole[ idim - 1 ];
         stride_section[ idim ] = stride_section[ idim - 1 ]*dims_section[ idim - 1 ];
         iv_cwhole += ( lbnd_cwhole[ idim ] - 1 )*stride_cwhole[ idim ];
      }

/* Note the compression axis stride in units of elements rather than bytes. */
      zstride = stride_section[ zaxis ]/size_outtype;

/* Initialise the vector index into the full (i.e. not collapsed) required
   section, at the point where the current row intersects the lower
   bounds of the section. This is an index into the returned output array. */
      iv_section = 0;

/* Evaluate constants to avoid repeated calculation of them in the
   following loop. First, the change in vector index into the uncollapsed
   section that occurs when moving from one dimension to the next. Then
   the change in vector index into collapsed whole array that occurs when
   moving from one dimension to the next. Both of these consist of two
   parts; a negative one that moves the vector back to the start of the
   old axis, plus a positive one that steps up to the start of the next
   axis. */
      for( idim = 0; idim < ndim - 1; idim++ ) {
         div_section[ idim ] = stride_section[ idim + 1 ]
                                - stride_section[ idim ]*dims_csection[ idim ];
         div_cwhole[ idim ] = stride_cwhole[ idim + 1 ]
                               - stride_cwhole[ idim ]*dims_csection[ idim ];
      }
      div_section[ idim ] = 1;
      div_cwhole[ idim ] = 1;

/* Abort if an error has occurred. */
      if( *STATUS != SAI__OK ) goto L999;

/*  Loop round all pixels in the collapsed section. The "start" array
    holds the pixel indices of the pixel where the row that is currently
    being uncompressed intersects the lower bounds of the required section. */
      last_csection = nel_csection - 1;
      for( iv_csection = 0; iv_csection < nel_csection; iv_csection++ ) {

/* If we are copying the whole data array, we will already have mapped
   the DATA array, so just get a pointer into it that points to the first
   element needed for the current row. */
         if( whole ) {
            offset = ptr_firstd[ iv_cwhole ];
            pdata = ptr_data + offset*size_intype;

/* Otherwise, we do not need to map the whole of the input DATA array, so
   we map each slice as needed. */
         } else {

/* Get the one-based indicies of the first and last required element of the
   DATA array. */
            dlb = ptr_firstd[ iv_cwhole ] + 1;
            if( iv_csection < last_csection ) {
               dub = ptr_firstd[ iv_cwhole + 1 ];
            } else {
               dub = nel_data;
            }

/* Get the required slice of the input DATA array and map it. */
            datSlice( loc_data, 1, &dlb, &dub, &loc_slice, STATUS );
            datMapV( loc_slice, type_data, "READ", (void **) &pdata, &ndata,
                     STATUS );
         }

/* Get a pointer to the first element of the VALUE array that holds data
   for the current row. */
         offset = ptr_firstv[ iv_cwhole ];
         pvalue = ptr_value + offset*size_vtype;

/* Get pointers to the first element of the REPEAT array that holds data for
   the current row. */
         if( ptr_repeat ) {
            offset = ptr_firstr[ iv_cwhole ];
            prepeat = ptr_repeat + offset;
         } else {
            prepeat = NULL;
         }

/* Uncompress the required part of the current row, putting the
   uncompressed values into the required bit of the output array. */
         (*undelt_fun)( pdata, (size_t) LBND[ zaxis ] - 1,
                        (size_t) UBND[ zaxis ] - 1, scale_buf, zero_buf,
                        pvalue, prepeat, ptr_out + iv_section, zstride,
                        &bad, STATUS );

/* Annul any DATA slice locator. */
         if( loc_slice ) datAnnul( &loc_slice, STATUS );

/* Update the pixel indices at the start of the row so that they refer
   to the next row. Also update the vector indicies into the full (i.e. not
   collapsed) section, and into the compressed whole array, at the start
   of the next row. */
         idim = 0;
         iv_section += stride_section[ idim ];
         iv_cwhole += stride_cwhole[ idim ];
         while( ++start[ idim ] > ubnd_csection[ idim ] ) {
            iv_section += div_section[ idim ];
            iv_cwhole += div_cwhole[ idim ];
            start[ idim ] = lbnd_csection[ idim ];
            idim++;
         }
      }
   }

/* If there are no bad pixels in the output, indicate this by storing a
   FALSE value for BAD_PIXEL in the output. */
   *BAD = bad ? F77_TRUE : F77_FALSE;

L999:

/* Annul all local locators. */
   datAnnul( &loc_data, STATUS );
   datAnnul( &loc_zaxis, STATUS );
   datAnnul( &loc_zdim, STATUS );
   datAnnul( &loc_firstd, STATUS );
   datAnnul( &loc_value, STATUS );
   if( loc_repeat ) datAnnul( &loc_repeat, STATUS );
   if( loc_firstr ) datAnnul( &loc_firstr, STATUS );
   if( loc_firstv ) datAnnul( &loc_firstv, STATUS );
   if( loc_scale ) datAnnul( &loc_scale, STATUS );
   if( loc_zero ) datAnnul( &loc_zero, STATUS );

/* We need to free the HDSLoc structure created at the start by
   datImportFloc. The only way to do this seems to be to export the locator
   back to Fortran, even though it has not been changed. */
   datExportFloc( &loc1, 1, LOC1_length, LOC1, STATUS );

/* Report a context error if anything went wrong. */
   if( *STATUS != SAI__OK ) {
      errRep( "", "ARY1_UNDLT: Failed to uncompress a section of a "
              "delta compressed array.", STATUS );
   }
}






/*
*  Name:
*     ary1Undelt<TIN><TOUT><TVAL><SCALE>

*  Purpose:
*     Uncompress a continuous run of delta compressed values.

*  Invocation:
*     void ary1Undelt<TIN><TOUT><TVAL><SCALE>( <TIN> *pindata, size_t row_lbnd,
*                                        size_t row_ubnd, <TOUT> *pscale,
*                                        <TOUT> *pzero, <TVAL> *pvalue,
*                                        int *prepeat, <TOUT> *poutdata,
*                                        size_t stride, int *bad, int *status )

*  Description:
*     This family of functions uncompresses a continuous run of delta
*     compressed values. Each member of the family handles a different
*     combination of input and output data type. The input compressed values
*     are contiguous. The output uncompressed values will only be contiguous
*     if the compression axis is the first axis. The <SCALE> part of the
*     function name is either "1" or "0" and is a boolean indicating if
*     the pscale and pzero values should be used or not. This allows the
*     compiler optimisation to remove the test on scale (since the scale
*     value is a literal constant), and so speed things up.

*  Arguments:
*     pindata = <TIN> *
*        Pointer to the first compressed value to be uncompressed. The other
*        values to be uncompressed should follow in a contiguous block. This
*        array holds differences between uncompressed values along the
*        compression axis (zaxis) and flags identifying values that could
*        not be compressed.
*     lbnd = size_t
*        The zero-based index of the first uncompressed value that is to be
*        returned in "poutdata".
*     ubnd = size_t
*        The zero-based index of the last uncompressed value that is to be
*        returned in "poutdata".
*     pscale = <TOUT> *
*        Pointer to the scale factor to be applied to the output values,
*        or NULL if no scaling is needed. Only used if <SCALE> is 1.
*     pzero = <TOUT> *
*        Pointer to the zero offset to be applied to the output values,
*        or NULL if no offset is needed. Only used if <SCALE> is 1.
*     pvalue = <TVAL> *
*        Array of uncompressed values that should be used to replace
*        flagged values in "pindata".
*     prepeat = int *
*        Array holding the number of repeats for each repeated value in
*        "pvalue".
*     poutdata = <TOUT> *
*        Pointer to the location at which the first uncompressed value
*        should be placed.
*     stride = size_t
*        The number of array elements between adjacent uncompressed data
*        values in the output array.
*     bad = int *
*        A pointer to an int in which to return a flag indicating if any
*        VAL__BAD<TOUT> values were stored in the output array.
*     status
*        Pointer to the global status.

*/

/* Define a macro that pushes a value onto the end of the output array,
   scaling it if required. Optimisation during compilation should remove
   the unrequired parts since the "scale" value will be a literal
   constant - 0 or 1. Note, the "value" argument may be evaluated twice
   and so should have no side effects. */
#define PUSH(value,outcode,vcode,scale) \
   if( (value) != VAL__BAD##vcode ) { \
      *poutdata = ( scale ? (*pscale)*(value)+(*pzero) : (value) ); \
   } else { \
      *poutdata =  VAL__BAD##outcode; \
   } \
   poutdata += stride;

#define PUSHBAD(outcode) \
   *poutdata = VAL__BAD##outcode; \
   poutdata += stride;

#define MAKE_FUNA(incode,intype,outcode,outtype,vcode,vtype,scale) \
\
static void ary1Undelt##incode##outcode##vcode##scale( intype *pindata, size_t row_lbnd, \
                                            size_t row_ubnd, outtype *pscale, \
                                            outtype *pzero, vtype *pvalue, \
                                            int *prepeat, outtype *poutdata, \
                                            size_t stride, int *bad, \
                                            int *status ){ \
\
/* Local Variables: */ \
   int dim; \
   int nleft;  \
   vtype temp_val; \
   vtype previous_val; \
   size_t irepeat; \
   size_t nrepeat; \
   size_t nwrite;  \
\
/* Check inherited status. */ \
   if( *status != SAI__OK ) return; \
\
/* Store the number of uncompressed values required along the output \
   compression axis. */ \
   dim = row_ubnd - row_lbnd + 1; \
\
/* Indicate we do not currently know the value of the previous uncompressed \
   output value. */ \
   previous_val = VAL__BAD##vcode; \
\
/* First uncompress (but do not store) the data up to the start of the \
   required section. */ \
   nwrite = 0; \
   while( nwrite < row_lbnd ) { \
\
/* If the current input value is SINGLE_BAD, the current output value is \
   bad, but the following output value is good and is given by the next \
   element in the VALUE array. This generates two uncompressed values. */ \
      if( *pindata == SINGLE_BAD##incode ) { \
         previous_val = *(pvalue++); \
         nwrite += 2; \
\
/* If the current input value is SINGLE_GOOD, the current output value \
   is good and is given by the next element in the VALUE array. This \
   generates one uncompressed value. */ \
      } else if( *pindata == SINGLE_GOOD##incode ) { \
         previous_val = *(pvalue++); \
         nwrite++; \
\
/* If the current input value is REPEAT_BAD, the next N output values \
   (starting with the current output value) are bad, where "N" is given by \
   the next value in the REPEAT array. The (N+1)'th output value is good \
   and is given by the next element of the VALUE array. This generates \
   (N+1) uncompressed values. */ \
      } else if( *pindata == REPEAT_BAD##incode ) { \
         previous_val = *(pvalue++); \
         nwrite += *(prepeat++) + 1; \
\
/* If the current input value is REPEAT_GOOD, the next N output values \
   (starting with the current output value) are all good and identically \
   equal to the next element of the VALUE array. This generates N \
   uncompressed values. */ \
      } else if( *pindata == REPEAT_GOOD##incode ) { \
         previous_val = *(pvalue++); \
         nwrite += *(prepeat++); \
\
/* If the current input value is MULTI_GOOD, the next N output values \
   (starting with the current output value) are all good and should be \
   read from the next N elements of the VALUE array. This generates N \
   uncompressed values. */ \
      } else if( *pindata == MULTI_GOOD##incode ) { \
         nrepeat = *(prepeat++); \
         pvalue += nrepeat; \
         previous_val = pvalue[ -1 ]; \
         nwrite += nrepeat; \
\
/* If the current input value is none of the above special cases, then \
   it is the increment which must be added to the previous good value in \
   order to re-create the current output value. */ \
      } else if( previous_val != VAL__BAD##vcode ) { \
         previous_val = previous_val + ((outtype) *pindata); \
         nwrite++; \
\
/* Report an error if the previous value is unknown. */ \
      } else { \
         *status = ARY__FATIN; \
         errRep( "", "ARY1_UNDLT: First compressed value is a delta " \
                 "(programming error).", status ); \
         return;   /* Naughty but fast */ \
      } \
\
/* Look at the next input value. */ \
      pindata++; \
   } \
\
/* If we have over-shot the first required output value, because it was in \
   the middle of a repeat section, store the missed bad or good values at \
   the start of the output array. */ \
   if( nwrite > row_lbnd ) { \
\
      if( pindata[ -1 ] == SINGLE_BAD##incode ) { \
         PUSH(previous_val,outcode,vcode,scale) \
\
      } else if( pindata[ -1 ] == REPEAT_BAD##incode ) { \
         nrepeat = nwrite - row_lbnd - 1; \
         if( nrepeat > dim ) nrepeat = dim; \
         for( irepeat = 0; irepeat < nrepeat; irepeat++ ) { \
            PUSHBAD(outcode) \
         } \
         *bad = 1; \
         if( nrepeat < dim ) { \
            PUSH(previous_val,outcode,vcode,scale) \
         } \
\
      } else if( pindata[ -1 ] == REPEAT_GOOD##incode ) { \
         nrepeat = nwrite - row_lbnd; \
         if( nrepeat > dim ) nrepeat = dim; \
         for( irepeat = 0; irepeat < nrepeat; irepeat++ ) { \
            PUSH(previous_val,outcode,vcode,scale) \
         } \
\
      } else if( pindata[ -1 ] == MULTI_GOOD##incode ) { \
         nrepeat = nwrite - row_lbnd; \
         pvalue -= nrepeat; \
         if( nrepeat > dim ) nrepeat = dim; \
         for( irepeat = 0; irepeat < nrepeat; irepeat++ ) { \
            temp_val = *(pvalue++); \
            PUSH(temp_val,outcode,vcode,scale) \
         } \
\
      } else { \
         *status = ARY__FATIN; \
         errRep( "", "ARY1_UNDLT: Inconsistent over-shoot (programming " \
                 "error).", status ); \
         return; \
      } \
   } \
\
/* Now loop round until we have read the required number of uncompressed \
   values. */ \
   nleft = row_ubnd - nwrite + 1; \
   while( nleft > 0 ) { \
\
/* If the current input value is SINGLE_BAD, the current output value is \
   bad, but the following output value is good and is given by the next \
   element in the VALUE array. Store the bad value in the output, \
   increment the pointer to the next output value, get the good value \
   from the VALUE array and record it. */ \
      if( *pindata == SINGLE_BAD##incode ) { \
         PUSHBAD(outcode) \
         nleft--; \
\
         if( nleft > 0 ) { \
            previous_val = *(pvalue++); \
            PUSH(previous_val,outcode,vcode,scale) \
            nleft--; \
         } \
\
         *bad = 1; \
\
/* If the current input value is SINGLE_GOOD, the current output value is \
   good and is given by the next element in the VALUE array. Record the \
   good value. */ \
      } else if( *pindata == SINGLE_GOOD##incode ) { \
         previous_val = *(pvalue++); \
         PUSH(previous_val,outcode,vcode,scale) \
         nleft--; \
\
/* If the current input value is REPEAT_BAD, the next N output values \
   (starting with the current output value) are bad, where "N" is given by \
   the next value in the REPEAT array. The (N+1)'th output value is good \
   and is given by the next element of the VALUE array. Record the good \
   value. */ \
      } else if( *pindata == REPEAT_BAD##incode ) { \
         nrepeat = *(prepeat++); \
         if( nrepeat > nleft ) nrepeat = nleft; \
\
         for( irepeat = 0; irepeat < nrepeat; irepeat++ ) { \
            PUSHBAD(outcode) \
         } \
         nleft -= nrepeat; \
         *bad = 1; \
\
         if( nleft > 0 ) { \
            previous_val = *(pvalue++); \
            PUSH(previous_val,outcode,vcode,scale) \
            nleft--; \
         } \
\
/* If the current input value is REPEAT_GOOD, the next N output values \
   (starting with the current output value) are all good and identically \
   equal to the next element of the VALUE array. */ \
      } else if( *pindata == REPEAT_GOOD##incode ) { \
         nrepeat = *(prepeat++); \
         if( nrepeat > nleft ) nrepeat = nleft; \
\
         previous_val = *(pvalue++); \
         for( irepeat = 0; irepeat < nrepeat; irepeat++ ) { \
            PUSH(previous_val,outcode,vcode,scale) \
         } \
\
         nleft -= nrepeat; \
\
/* If the current input value is MULTI_GOOD, the next N output values \
   (starting with the current output value) are all good and are read \
   from the next N values of the VALUE array. */ \
      } else if( *pindata == MULTI_GOOD##incode ) { \
         nrepeat = *(prepeat++); \
         if( nrepeat > nleft ) nrepeat = nleft; \
\
         for( irepeat = 0; irepeat < nrepeat; irepeat++ ) { \
            temp_val = *(pvalue++); \
            PUSH(temp_val,outcode,vcode,scale) \
         } \
         previous_val = pvalue[ -1 ]; \
\
         nleft -= nrepeat; \
\
/* If the current input value is none of the above special cases, then \
   it is the increment which must be added to the previous good value in \
   order to re-create the current output value. */ \
      } else if( previous_val != VAL__BAD##vcode ) { \
         previous_val += ((outtype) *pindata); \
         PUSH(previous_val,outcode,vcode,scale) \
         nleft--; \
\
/* Report an error if the previous value is unknown. */ \
      } else { \
         *status = ARY__FATIN; \
         errRep( "", "ARY1_UNDLT: First compressed value is a delta " \
                 "(programming error).", status ); \
         break; \
      } \
\
/* Move on to read the next compressed input value */ \
      pindata++; \
   } \
}

/* Use the above macro to define the functions for both scaled and
   unscaled versions for a specific combination of input, output and value
   data type. */
#define MAKE_FUNB(incode,intype,outcode,outtype,vcode,vtype) \
   MAKE_FUNA(incode,intype,outcode,outtype,vcode,vtype,0) \
   MAKE_FUNA(incode,intype,outcode,outtype,vcode,vtype,1)

/* Use the above macro to define the functions for all required
   input data types. */
#define MAKE_FUNC(outcode,outtype,vcode,vtype) \
   MAKE_FUNB(I,int,outcode,outtype,vcode,vtype) \
   MAKE_FUNB(W,short int,outcode,outtype,vcode,vtype) \
   MAKE_FUNB(B,char,outcode,outtype,vcode,vtype)

/* Use the above macro to define the functions for all required
   output data types. */
#define MAKE_FUND(vcode,vtype) \
   MAKE_FUNC(D,double,vcode,vtype) \
   MAKE_FUNC(R,float,vcode,vtype) \
   MAKE_FUNC(I,int,vcode,vtype) \
   MAKE_FUNC(W,short int,vcode,vtype) \
   MAKE_FUNC(UW,unsigned short int,vcode,vtype) \
   MAKE_FUNC(B,char,vcode,vtype) \
   MAKE_FUNC(UB,unsigned char,vcode,vtype)

/* Use the above macro to define the functions for all required
   value data types. */
MAKE_FUND(I,int)
MAKE_FUND(W,short int)
MAKE_FUND(UW,unsigned short int)
MAKE_FUND(B,char)
MAKE_FUND(UB,unsigned char)

/* Delete the macros */
#undef MAKE_FUNA
#undef MAKE_FUNB
#undef MAKE_FUNC
#undef MAKE_FUND
#undef PUSH
#undef PUSHBAD

