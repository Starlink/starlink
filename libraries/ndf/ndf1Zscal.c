#include "sae_par.h"		 /* Standard SAE constants */
#include "mers.h"		 /* Message and error system */
#include "ndf1_types.h"		 /* Internal NDF constants */
#include "ndf_err.h"		 /* NDF error constants */
#include "prm_par.h"		 /* NDF error constants */

/* Prototypes for local functions. */
/* =============================== */

/* Macro defining a prototype for a single combination of data types. */
#define PROTOTYPE(incode,outcode,intype,outtype) \
   static void ndf1Zscal##incode##outcode( intype *pin, int el, double *scale, \
                                           double *zero, outtype *pout, \
                                           int *status );

/* Macro defining prototypes for all function with a given input data type. */
#define PROTOTYPES(incode,intype) \
   PROTOTYPE(incode,B,intype,char) \
   PROTOTYPE(incode,D,intype,double) \
   PROTOTYPE(incode,I,intype,int) \
   PROTOTYPE(incode,R,intype,float) \
   PROTOTYPE(incode,UB,intype,unsigned char) \
   PROTOTYPE(incode,UW,intype,unsigned short int) \
   PROTOTYPE(incode,W,intype,short int)

/* Use the above macros to define all prototypes. */
PROTOTYPES(B,char)
PROTOTYPES(UB,unsigned char)
PROTOTYPES(W,short int)
PROTOTYPES(UW,unsigned short int)
PROTOTYPES(I,int)
PROTOTYPES(R,float)
PROTOTYPES(D,double)





void ndf1Zscal( int intype, void *pin, size_t el, int outtype,
                double *scale, double *zero, void *pout, int *status ) {
/*
*+
*  Name:
*     ndf1Zscal

*  Purpose:
*     Convert an array to SCALED form.

*  Synopsis:
*     void ndf1Zscal( int intype, void *pin, size_t el, int outtype,
*                     double *scale, double *zero, void *pout, int *status )

*  Description:
*     This routine copies the input array to the output array,
*     compressing it using the SCALED algorithm. Each output value is
*     related to each input value as follows:
*
*        output_value = ( input_value - ZERO )/SCALE
*
*     Output array values that exceed the range of values that can be
*     stored using the output data type are set bad.

*  Arguments:
*     intype
*        The data type of the input array.
*     pin
*        A pointer to the input array.
*     el
*        The number of elements in the array.
*     outtype
*        The data type of the output array. An error will be reported if
*        this is _DOUBLE.
*     *scale
*        The scale factor that converts the output values into the input
*        values. If a value of VAL__BADD is supplied, a default value
*        will be calculated, and returned on exit. The default value is
*        the ratio of the data range of the values in the input array to
*        the data range that can be stored in the output array. The
*        supplied value must not be zero. On exit, any supplied value
*        will be rounded to a value that can be represented accurately
*        using the input data type.
*     *zero
*        The zero offset that converts the output values into the input
*        values. If a value of VAL__BADD is supplied, a default value
*        will be calculated, and returned on exit. The default value
*        maps the centre of the data range of the values in the input array
*        onto the centre of the data range that can be stored in the output
*        array, using either the supplied or default SCALE value. On exit,
*        any supplied value will be rounded to a value that can be
*        represented accurately using the input data type.
*     pout
*        A pointer to the output array.
*     *status
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-OCT-2010 (DSB):
*        Original version.
*     1-AUG-2018 (DSB):
*        Modified to be called from C and renamed to ndf1Zscal.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables; */
   int ok;                    /* Is the output data type legal? */

/* Check inherited status. */
   if ( *status != SAI__OK ) return;

/* Set a flag indicating the output data type is legal. */
   ok = 1;

/* A macro that expands to an invocation of the function that does the
   work for a specified combination of input and output data types. */
#define CALLER1(incode,outcode,intype,outtype) \
      ndf1Zscal##incode##outcode( (intype *) pin, el, scale, zero, \
                                  (outtype *) pout, status );

/* A macro that expands to an invocation of the functions that do the
   work for a specified input data type. */
#define CALLER2(incode,intype) \
      if( outtype == NDF__TYPB ) { \
         CALLER1(incode,B,intype,char) \
\
      } else if( outtype == NDF__TYPD ) { \
         CALLER1(incode,D,intype,double) \
\
      } else if( outtype == NDF__TYPI ) { \
         CALLER1(incode,I,intype,int) \
\
      } else if( outtype == NDF__TYPR ) { \
         CALLER1(incode,R,intype,float) \
\
      } else if( outtype == NDF__TYPUB ) { \
         CALLER1(incode,UB,intype,unsigned char) \
\
      } else if( outtype == NDF__TYPUW ) { \
         CALLER1(incode,UW,intype,unsigned short int) \
\
      } else if( outtype == NDF__TYPW ) { \
         CALLER1(incode,W,intype,short int) \
\
      } else { \
         ok = 0; \
      }

/* Use the above macros to invoke the correct function for the combination of
   input and output data types. */
   if( intype == NDF__TYPB ) {
      CALLER2(B,char)

   } else if( intype == NDF__TYPUB ) {
      CALLER2(UB,unsigned char)

   } else if( intype == NDF__TYPW ) {
      CALLER2(W,short int)

   } else if( intype == NDF__TYPUW ) {
      CALLER2(UW,unsigned short int)

   } else if( intype == NDF__TYPI ) {
      CALLER2(I,int)

   } else if( intype == NDF__TYPR ) {
      CALLER2(R,float)

   } else if( intype == NDF__TYPD ) {
      CALLER2(D,double)

   } else if( *status == SAI__OK ){
      *status = NDF__FATIN;
       msgSeti( "I", intype );
       errRep( "", "NDF1_ZSCAL: Illegal input data type code ^I (internal "
               "programming error).", status );
   }

/* Report an error if the input data type was bad. */
   if( !ok && *status == SAI__OK ){
      *status = NDF__FATIN;
       msgSeti( "I", outtype );
       errRep( "", "NDF1_ZSCAL: Illegal output data type code ^I (internal "
               "programming error).", status );
   }

/* Undefine the macros defined in this function. */
#undef CALLER1
#undef CALLER2

}

/* Use a macro to define a function that does the work for a single
   combination of input and output data types. */
#define FUNCTION(incode,outcode,intype,outtype) \
static void ndf1Zscal##incode##outcode( intype *pin, int el, double *scale, \
                                        double *zero, outtype *pout, \
                                        int *status ){ \
\
/* Local Variables; */ \
   double outeps;   /* Min increment in output value */ \
   double outmax;   /* Max output value */ \
   double outmin;   /* Min output value */ \
   double outval;   /* Output array value */ \
   int iel;         /* Index of current array element */ \
   intype *p1;      /* Pointer to next input array value */ \
   intype inmax;    /* Maximum input array value */ \
   intype inmin;    /* Minimum input array value */ \
   intype inval;    /* Input array value */ \
   outtype *p2;     /* Pointer to next output array value */ \
\
/* Check inherited status */ \
   if( *status != SAI__OK ) return; \
\
/* Report an error if the output data type is _DOUBLE */ \
   if( VAL__BAD##outcode == VAL__BADD && *status == SAI__OK ) { \
      *status = NDF__BADCP; \
      errRep( " ", "Cannot compressed data to _DOUBLE data type.", \
              status ); \
   } \
\
/* Constants defining the output data type. We leave a big safety margin \
   if compressing to floating point, and a small one if compressing to \
   integers. */ \
   if( VAL__BAD##outcode == VAL__BADR ) { \
      outmax = 0.001*( (double) VAL__MAX##outcode ); \
      outmin = 0.001*( (double) VAL__MIN##outcode ); \
   } else { \
      outmax = 0.98*( (double) VAL__MAX##outcode ); \
      outmin = 0.98*( (double) VAL__MIN##outcode ); \
   } \
   outeps = 0.5*( (double) VAL__EPS##outcode ); \
\
/* If either of the scale or zero have not been supplied, we need to find \
   default values. */ \
   if( *scale == VAL__BADD || *zero == VAL__BADD ) { \
\
/* Scan the input array to find the max and min values in the array. */ \
      p1 = pin; \
      inmin = VAL__MAX##incode; \
      inmax = VAL__MIN##incode; \
      for( iel = 0; iel < el; iel++ ) { \
         inval = *(p1++); \
         if( inval != VAL__BAD##incode ){ \
            if( inval < inmin ) inmin = inval; \
            if( inval > inmax ) inmax = inval; \
         } \
      } \
\
/* If no SCALE has been provided, use a SCALE that maps the input data \
   range onto the output data range. Since it will be stored in the NDF \
   using the data type of the input array, we constrain it to be well \
   within the data range of the input data type. */ \
      if( *scale == VAL__BADD ) { \
         *scale = ( ( (double) inmax - (double) inmin ) )/ \
                  ( outmax - outmin ); \
\
         if( *scale < 10.0*VAL__SML##incode ) { \
            *scale = 10.0*VAL__SML##incode; \
         } else if( *scale > 0.1*VAL__MAX##incode ) { \
            *scale = 0.1*VAL__MAX##incode; \
         } \
\
      } \
\
/* If no ZERO has been provided, use a ZERO that maps the centre of the \
   input data range onto the centre of the output data range. Constrain \
   it to be well within the data range of input data type. */ \
      if( *zero == VAL__BADD ) { \
         *zero = 0.5*( ( ( (double) inmax + (double) inmin ) ) - \
                     (*scale)*( ( outmax + outmin ) ) ); \
\
         if( *zero < 0.1*VAL__MIN##incode ) { \
            *zero = 0.1*VAL__MIN##incode; \
         } else if( *zero > 0.1*VAL__MAX##incode ) { \
            *zero = 0.1*VAL__MAX##incode; \
         } \
\
      } \
   } \
\
/* Report an error if the scale is zero or negative. */ \
   if( *scale <= 0.0 && *status == SAI__OK ) { \
      *status = NDF__ZERSC; \
      msgSetd( "S", *scale ); \
      errRep( " ", "Invalid zero or negative scale value (^S) supplied " \
              "when converting an array to scaled storage form.", status ); \
\
/* Otherwise, we need to modify the scale and zero so that they can be stored \
   exactly in the input data type (since that is the type in which they \
   will be store din the NDF. */ \
   } else { \
      *scale = (intype)( *scale + 0.5*( (double) VAL__EPS##incode ) ); \
      *zero = (intype)( *zero + 0.5*( (double) VAL__EPS##incode ) ); \
\
/* Scale all values, setting out-of-range input values bad in the  \
   output array, and rounding to the nearest output value. */ \
      p1 = pin; \
      p2 = pout; \
      for( iel = 0; iel < el; iel++ ) { \
         inval = *(p1++); \
         if( inval != VAL__BAD##incode ){ \
\
            outval = ( (double) inval - (*zero) )/(*scale); \
             if( outval > 0.0 ) { \
               outval += outeps; \
            } else if( outval < 0.0 ) { \
               outval -= outeps; \
            } \
\
            if( outval > VAL__MAX##outcode || outval < VAL__MIN##outcode ) { \
               *(p2++) = VAL__BAD##outcode; \
            } else { \
               *(p2++) = outval;      \
            } \
         } else { \
            *(p2++) = VAL__BAD##outcode; \
         } \
      } \
   } \
}


/* Use the above macro to define all functions. */
#define FUNCTIONS(incode,intype) \
   FUNCTION(incode,B,intype,char) \
   FUNCTION(incode,D,intype,double) \
   FUNCTION(incode,I,intype,int) \
   FUNCTION(incode,R,intype,float) \
   FUNCTION(incode,UB,intype,unsigned char) \
   FUNCTION(incode,UW,intype,unsigned short int) \
   FUNCTION(incode,W,intype,short int) \

FUNCTIONS(B,char)
FUNCTIONS(UB,unsigned char)
FUNCTIONS(W,short int)
FUNCTIONS(UW,unsigned short int)
FUNCTIONS(I,int)
FUNCTIONS(R,float)
FUNCTIONS(D,double)



