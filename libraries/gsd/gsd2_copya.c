/*+
 * Name:
 *    gsd2_copya

 * Purpose:
 *    Copy array with type conversion.

 * Language:
 *    ANSI C

 * Type of Module:
 *    C function.

 * Prototype:
 *    (available via #include "gsd1.h")
 *    int gsd2_copya( enum type_tag itype, enum type_tag otype,
 *       int size, unsigned char *in, unsigned char *out );

 * Description:
 *    This routine converts an array from the given data type to the same or a
 *    different one. At any rate the array will be copied to a different
 *    buffer. In type conversion bad values turn into bad values (or false for
 *    logical output). Input values that cannot represented in the output type
 *    also become bad values (or false for logical output).
 *
 *    Either both types must be numeric (including byte and logical), or both
 *    must be character.
 *

 * Arguments:
 *    enum type_tag itype (Given)
 *       The data type code for the given array.
 *    enum type_tag otype (Given)
 *       The data type code for the returned array.
 *    int size (Given)
 *       The size of the arrays. This is the number of elements in the array,
 *       _not_ the size of the in and out buffers in bytes.
 *    unsigned char *in (Given)
 *       The array to be converted. This need not be aligned, since it is
 *       treated only as a byte array.
 *    unsigned char *out (Returned)
 *       The converted array. This buffer need not be aligned, since it is
 *       treated only as a byte array. The calling routine must provide
 *       sufficient memory under the given pointer, taking into account the
 *       size of the output array elements.

 * Returned Value:
 *    int gsd2_copya();
 *       Status.
 *       -1: Conversion between these types not possible.
 *        N: Otherwise. The returned value is the number of output elements
 *           that are bad because the input elements could not be represented
 *           in the output type.

 * Algorithm:
 *    The heart of this routine is the type conversion. If both types are the
 *    same, we can just perform a byte-wise copy between the buffers. If one
 *    type is character (typ_char) and the other is not then conversion is
 *    flatly refused.
 *
 *    For the remaining conversions, integer types (b,w,i) are cast, floating
 *    point types (r,d) are cast. Conversion from integer types to floating
 *    types are cast, too. Conversions from floating types to integer types are
 *    rounded to the nearest integer (in double type) and then cast.
 *
 *    A logical false (true) becomes an integer or floating zero (one). A
 *    logical false (true) results from a zero (non-zero) integer or floating.
 *
 *    Some operations, including the cast, can be done only with properly
 *    aligned buffers. Thus each array element may have to be copied to/from
 *    local buffers that are aligned like doubles.
 *
 *    Before any conversion is attempted, the input is checked for being the
 *    bad value. In that case the output also is simply the bad value (or 0 for
 *    logical). This operation can be done on the un-aligned bytes and is done
 *    before the element is copied to the aligned buffer.
 *
 *    For a number of conversions the output type has a smaller allowed value
 *    range than the input type. In those cases a check has to be made after
 *    copying to the aligned buffer, but before doing the actual conversion. If
 *    conversion is not possible the aligned buffer is abandones and the output
 *    bytes are set to the bad value (or 0 for logical). The error count
 *    (return value) is also incremented.
 *
 *    The allowed maxima are from <limits.h> and <float.h>. The minima are the
 *    PRIMDAT bad values from "gsd2.h":
 *
 *      b VAL__BADB SCHAR_MAX
 *      w VAL__BADW  SHRT_MAX
 *      i VAL__BADI   INT_MAX
 *      r VAL__BADR   FLT_MAX

 * Implementation Status:
 *    If the input is logical, then its char values are converted to the new
 *    type. The output array does thus not necessarily contain only 0 or 1.
 *    Under normal circumstances a logical array never contains values other
 *    than 0 or 1, so there should be no problem.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    12 Dec 1994 (hme):
 *       Original.
 *-
 */

#include <float.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include "gsd1.h"
#include "gsd2.h"

/*:
 */

int gsd2_copya( enum type_tag itype, enum type_tag otype,
   int size, unsigned char *in, unsigned char *out )
{
   const int gsd_byte[GSD_NTYPES] =  /* Size for each type. */
   {  GSD_SZBYTE, GSD_SZLOGICAL, GSD_SZWORD, GSD_SZINTEGER,
      GSD_SZREAL, GSD_SZDOUBLE,  GSD_SZCHAR
   };

   int status;      /* Counts conversion errors */
   int i, j, k, l;  /* Count bytes in steps of 1, icell, ocell, 1 */

   double       dibuf[1];  /* Buffer for input element */
   void        *vibuf =        (void *) dibuf;
   signed char *bibuf = (signed char *) dibuf;
   char        *libuf =        (char *) dibuf;
   short int   *wibuf =   (short int *) dibuf;
   int         *iibuf =         (int *) dibuf;
   float       *ribuf =       (float *) dibuf;

   double       dobuf[1];  /* Buffer for output element */
   void        *vobuf =        (void *) dobuf;
   signed char *bobuf = (signed char *) dobuf;
   char        *lobuf =        (char *) dobuf;
   short int   *wobuf =   (short int *) dobuf;
   int         *iobuf =         (int *) dobuf;
   float       *robuf =       (float *) dobuf;

   size_t icell, ocell;  /* Size of element in bytes, input/output */

/*.
 */


/* Simple cases.
 * =============
 */

/* If both types are character, just copy the bytes and return zero.
 */
   if ( itype == typ_char && otype == typ_char )
   {  (void) memcpy( out, in, (size_t) (GSD_SZCHAR*size) );
      return 0;
   }

/* Else if one type is character (and the other one isn't), return with error.
 */
   else if ( itype == typ_char || otype == typ_char )
   {  return -1;
   }

/* Else if both types are equal (and numeric), just copy the bytes and return
 * zero.
 */
   else if ( itype == otype )
   {  (void) memcpy( out, in, (size_t) (gsd_byte[itype-1]*size) );
      return 0;
   }


/* Complex cases.
 * ==============
 */

/* Still here? Must be two different numeric types then.
 * Nested switch case for each pair of types.
 */
   status = 0;
   icell = gsd_byte[itype-1];
   ocell = gsd_byte[otype-1];
   switch (itype)
   {  case typ_byte:
         switch (otype)
         {  case typ_logical:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if      ( in[j] == val__badb[0] ) out[k] = 0;
                  else if ( in[j] )                 out[k] = 1;
                  else                              out[k] = 0;
               }
               break;
            case typ_word:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if ( in[j] == val__badb[0] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badw[l];
                  }
                  else
                  {  (void) memcpy( vibuf,  in+j, icell );
                     *wobuf = (short) *bibuf;
                     (void) memcpy( out+k, vobuf, ocell );
                  }
               }
               break;
            case typ_int:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if ( in[j] == val__badb[0] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badi[l];
                  }
                  else
                  {  (void) memcpy( vibuf,  in+j, icell );
                     *iobuf = (int) *bibuf;
                     (void) memcpy( out+k, vobuf, ocell );
                  }
               }
               break;
            case typ_real:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if ( in[j] == val__badb[0] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badr[l];
                  }
                  else
                  {  (void) memcpy( vibuf,  in+j, icell );
                     *robuf = (float) *bibuf;
                     (void) memcpy( out+k, vobuf, ocell );
                  }
               }
               break;
            case typ_double:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if ( in[j] == val__badb[0] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badd[l];
                  }
                  else
                  {  (void) memcpy( vibuf,  in+j, icell );
                     *dobuf = (double) *bibuf;
                     (void) memcpy( out+k, vobuf, ocell );
                  }
               }
               break;
            default:
               return -1;
               break;
         }
         break;
      case typ_logical:
         switch (otype)
         {  case typ_byte:
               (void) memcpy( out, in, icell*size );
               break;
            case typ_word:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  (void) memcpy( vibuf,  in+j, icell );
                  *wobuf = (short) *libuf;
                  (void) memcpy( out+k, vobuf, ocell );
               }
               break;
            case typ_int:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  (void) memcpy( vibuf,  in+j, icell );
                  *iobuf = (int) *libuf;
                  (void) memcpy( out+k, vobuf, ocell );
               }
               break;
            case typ_real:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  (void) memcpy( vibuf,  in+j, icell );
                  *robuf = (float) *libuf;
                  (void) memcpy( out+k, vobuf, ocell );
               }
               break;
            case typ_double:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  (void) memcpy( vibuf,  in+j, icell );
                  *dobuf = (double) *libuf;
                  (void) memcpy( out+k, vobuf, ocell );
               }
               break;
            default:
               return -1;
               break;
         }
         break;
      case typ_word:
         switch (otype)
         {  case typ_byte:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if ( in[j] == val__badw[0] && in[j+1] == val__badw[1] )
                  {  out[k] = val__badb[0];
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     if ( *wibuf <= VAL__BADB || *wibuf > SCHAR_MAX )
                     {  out[k] = val__badb[0];
                        status++;
                     }
                     else
                     {  *bobuf = (signed char) *wibuf;
                        (void) memcpy( out+k, vobuf, ocell );
                     }
                  }
               }
               break;
            case typ_logical:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if ( in[j] == val__badw[0] && in[j+1] == val__badw[1] )
                  {  out[k] = 0;
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     if ( *wibuf ) out[k] = 1; else out[k] = 0;
                  }
               }
               break;
            case typ_int:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if ( in[j] == val__badw[0] && in[j+1] == val__badw[1] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badi[l];
                  }
                  else
                  {  (void) memcpy( vibuf,  in+j, icell );
                     *iobuf = (int) *wibuf;
                     (void) memcpy( out+k, vobuf, ocell );
                  }
               }
               break;
            case typ_real:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if ( in[j] == val__badw[0] && in[j+1] == val__badw[1] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badr[l];
                  }
                  else
                  {  (void) memcpy( vibuf,  in+j, icell );
                     *robuf = (float) *wibuf;
                     (void) memcpy( out+k, vobuf, ocell );
                  }
               }
               break;
            case typ_double:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if ( in[j] == val__badw[0] && in[j+1] == val__badw[1] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badd[l];
                  }
                  else
                  {  (void) memcpy( vibuf,  in+j, icell );
                     *dobuf = (double) *wibuf;
                     (void) memcpy( out+k, vobuf, ocell );
                  }
               }
               break;
            default:
               return -1;
               break;
         }
         break;
      case typ_int:
         switch (otype)
         {  case typ_byte:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badi[0] && in[j+1] == val__badi[1]
                     && in[j+2] == val__badi[2] && in[j+3] == val__badi[3] )
                  {  out[k] = val__badb[0];
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     if ( *iibuf <= VAL__BADB || *iibuf > SCHAR_MAX )
                     {  out[k] = val__badb[0];
                        status++;
                     }
                     else
                     {  *bobuf = (signed char) *iibuf;
                        (void) memcpy( out+k, vobuf, ocell );
                     }
                  }
               }
               break;
            case typ_logical:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badi[0] && in[j+1] == val__badi[1]
                     && in[j+2] == val__badi[2] && in[j+3] == val__badi[3] )
                  {  out[k] = 0;
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     if ( *iibuf ) out[k] = 1; else out[k] = 0;
                  }
               }
               break;
            case typ_word:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badi[0] && in[j+1] == val__badi[1]
                     && in[j+2] == val__badi[2] && in[j+3] == val__badi[3] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badw[l];
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     if ( *iibuf <= VAL__BADW || *iibuf > SHRT_MAX )
                     {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badw[l];
                        status++;
                     }
                     else
                     {  *wobuf = (short) *iibuf;
                        (void) memcpy( out+k, vobuf, ocell );
                     }
                  }
               }
               break;
            case typ_real:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badi[0] && in[j+1] == val__badi[1]
                     && in[j+2] == val__badi[2] && in[j+3] == val__badi[3] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badr[l];
                  }
                  else
                  {  (void) memcpy( vibuf,  in+j, icell );
                     *robuf = (float) *iibuf;
                     (void) memcpy( out+k, vobuf, ocell );
                  }
               }
               break;
            case typ_double:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badi[0] && in[j+1] == val__badi[1]
                     && in[j+2] == val__badi[2] && in[j+3] == val__badi[3] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badd[l];
                  }
                  else
                  {  (void) memcpy( vibuf,  in+j, icell );
                     *dobuf = (double) *iibuf;
                     (void) memcpy( out+k, vobuf, ocell );
                  }
               }
               break;
            default:
               return -1;
               break;
         }
         break;
      case typ_real:
         switch (otype)
         {  case typ_byte:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badr[0] && in[j+1] == val__badr[1]
                     && in[j+2] == val__badr[2] && in[j+3] == val__badr[3] )
                  {  out[k] = val__badb[0];
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     *ribuf = (float) floor( (double)(*ribuf+0.5) );
                     if ( *ribuf <= VAL__BADB || *ribuf > SCHAR_MAX )
                     {  out[k] = val__badb[0];
                        status++;
                     }
                     else
                     {  *bobuf = (signed char) *ribuf;
                        (void) memcpy( out+k, vobuf, ocell );
                     }
                  }
               }
               break;
            case typ_logical:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badr[0] && in[j+1] == val__badr[1]
                     && in[j+2] == val__badr[2] && in[j+3] == val__badr[3] )
                  {  out[k] = 0;
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     if ( *ribuf != 0. ) out[k] = 1; else out[k] = 0;
                  }
               }
               break;
            case typ_word:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badr[0] && in[j+1] == val__badr[1]
                     && in[j+2] == val__badr[2] && in[j+3] == val__badr[3] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badw[l];
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     *ribuf = (float) floor( (double)(*ribuf+0.5) );
                     if ( *ribuf <= VAL__BADW || *ribuf > SHRT_MAX )
                     {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badw[l];
                        status++;
                     }
                     else
                     {  *wobuf = (short) *ribuf;
                        (void) memcpy( out+k, vobuf, ocell );
                     }
                  }
               }
               break;
            case typ_int:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badr[0] && in[j+1] == val__badr[1]
                     && in[j+2] == val__badr[2] && in[j+3] == val__badr[3] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badi[l];
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     *ribuf = (float) floor( (double)(*ribuf+0.5) );
                     if ( *ribuf <= VAL__BADI || *ribuf > INT_MAX )
                     {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badi[l];
                        status++;
                     }
                     else
                     {  *iobuf = (int) *ribuf;
                        (void) memcpy( out+k, vobuf, ocell );
                     }
                  }
               }
               break;
            case typ_double:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badr[0] && in[j+1] == val__badr[1]
                     && in[j+2] == val__badr[2] && in[j+3] == val__badr[3] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badd[l];
                  }
                  else
                  {  (void) memcpy( vibuf,  in+j, icell );
                     *dobuf = (double) *ribuf;
                     (void) memcpy( out+k, vobuf, ocell );
                  }
               }
               break;
            default:
               return -1;
               break;
         }
         break;
      case typ_double:
         switch (otype)
         {  case typ_byte:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badd[0] && in[j+1] == val__badd[1]
                     && in[j+2] == val__badd[2] && in[j+3] == val__badd[3]
                     && in[j+4] == val__badd[4] && in[j+5] == val__badd[5]
                     && in[j+6] == val__badd[6] && in[j+7] == val__badd[7] )
                  {  out[k] = val__badb[0];
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     *dibuf = floor( *dibuf+0.5 );
                     if ( *dibuf <= VAL__BADB || *dibuf > SCHAR_MAX )
                     {  out[k] = val__badb[0];
                        status++;
                     }
                     else
                     {  *bobuf = (signed char) *dibuf;
                        (void) memcpy( out+k, vobuf, ocell );
                     }
                  }
               }
               break;
            case typ_logical:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badd[0] && in[j+1] == val__badd[1]
                     && in[j+2] == val__badd[2] && in[j+3] == val__badd[3]
                     && in[j+4] == val__badd[4] && in[j+5] == val__badd[5]
                     && in[j+6] == val__badd[6] && in[j+7] == val__badd[7] )
                  {  out[k] = 0;
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     if ( *dibuf != 0. ) out[k] = 1; else out[k] = 0;
                  }
               }
               break;
            case typ_word:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badd[0] && in[j+1] == val__badd[1]
                     && in[j+2] == val__badd[2] && in[j+3] == val__badd[3]
                     && in[j+4] == val__badd[4] && in[j+5] == val__badd[5]
                     && in[j+6] == val__badd[6] && in[j+7] == val__badd[7] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badw[l];
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     *dibuf = floor( *dibuf+0.5 );
                     if ( *dibuf <= VAL__BADW || *dibuf > SHRT_MAX )
                     {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badw[l];
                        status++;
                     }
                     else
                     {  *wobuf = (short) *dibuf;
                        (void) memcpy( out+k, vobuf, ocell );
                     }
                  }
               }
               break;
            case typ_int:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badd[0] && in[j+1] == val__badd[1]
                     && in[j+2] == val__badd[2] && in[j+3] == val__badd[3]
                     && in[j+4] == val__badd[4] && in[j+5] == val__badd[5]
                     && in[j+6] == val__badd[6] && in[j+7] == val__badd[7] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badi[l];
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     *dibuf = floor( *dibuf+0.5 );
                     if ( *dibuf <= VAL__BADI || *dibuf > INT_MAX )
                     {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badi[l];
                        status++;
                     }
                     else
                     {  *iobuf = (int) *dibuf;
                        (void) memcpy( out+k, vobuf, ocell );
                     }
                  }
               }
               break;
            case typ_real:
               for ( i=0, j=0, k=0; i < size; i++, j += icell, k += ocell )
               {  if (  in[j]   == val__badd[0] && in[j+1] == val__badd[1]
                     && in[j+2] == val__badd[2] && in[j+3] == val__badd[3]
                     && in[j+4] == val__badd[4] && in[j+5] == val__badd[5]
                     && in[j+6] == val__badd[6] && in[j+7] == val__badd[7] )
                  {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badr[l];
                  }
                  else
                  {  (void) memcpy( vibuf, in+j, icell );
                     if ( *dibuf <= VAL__BADR || *dibuf > FLT_MAX )
                     {  for ( l = 0; l < ocell; l++ ) out[k+l] = val__badr[l];
                        status++;
                     }
                     else
                     {  *robuf = (float) *dibuf;
                        (void) memcpy( out+k, vobuf, ocell );
                     }
                  }
               }
               break;
            default:
               return -1;
               break;
         }
         break;
      default:
         return -1;
         break;
   }

   return status;
}

/* Test programme.
 * ===============

hashinclude <stdio.h>
hashinclude "gsd2.h"

void main( void )
{
   double      darray[20];
   float       farray[20];
   int         iarray[20];
   short       warray[20];
   char        larray[20];
   signed char barray[20];
   int         i, status;

   darray[0]  =   0.;
   darray[1]  =  12.3456;
   darray[2]  = -78.9012;
   darray[3]  =  VAL__BADB;
   darray[4]  =  VAL__BADW;
   darray[5]  =  VAL__BADI;
   darray[6]  =  VAL__BADR;
   darray[7]  =  VAL__BADD;
   darray[8]  =  234.567;
   darray[9]  =  567.89;
   darray[10] =  34567.8;
   darray[11] =  67890.1;
   darray[12] =  2.34567e9;
   darray[13] =  4.56789e38;
   darray[14] = -234.567;
   darray[15] = -567.89;
   darray[16] = -34567.8;
   darray[17] = -67890.1;
   darray[18] = -2.34567e9;
   darray[19] = -4.56789e38;

   status = gsd2_copya( 6, 5, 20,
      (unsigned char *) darray, (unsigned char *) farray );
   (void) printf( "%2d conversion errors D to R\n", status );

   status = gsd2_copya( 6, 4, 20,
      (unsigned char *) darray, (unsigned char *) iarray );
   (void) printf( "%2d conversion errors D to I\n", status );

   status = gsd2_copya( 6, 3, 20,
      (unsigned char *) darray, (unsigned char *) warray );
   (void) printf( "%2d conversion errors D to W\n", status );

   status = gsd2_copya( 6, 2, 20,
      (unsigned char *) darray, (unsigned char *) larray );
   (void) printf( "%2d conversion errors D to L\n", status );

   status = gsd2_copya( 6, 1, 20,
      (unsigned char *) darray, (unsigned char *) barray );
   (void) printf( "%2d conversion errors D to B\n", status );

   for ( i = 0; i < 20; i++ )
      (void) printf( "%2d %22.15g %14.8g %11d %6d %1d %4d\n",
         i, darray[i], farray[i], iarray[i], warray[i], larray[i], barray[i] );

   farray[0]  =   0.;
   farray[1]  =  12.3456;
   farray[2]  = -78.9012;
   farray[3]  =  VAL__BADB;
   farray[4]  =  VAL__BADW;
   farray[5]  =  VAL__BADI;
   farray[6]  =  VAL__BADR;
   farray[7]  =  VAL__BADR;
   farray[8]  =  234.567;
   farray[9]  =  567.89;
   farray[10] =  34567.8;
   farray[11] =  67890.1;
   farray[12] =  2.34567e9;
   farray[13] =  VAL__BADR;
   farray[14] = -234.567;
   farray[15] = -567.89;
   farray[16] = -34567.8;
   farray[17] = -67890.1;
   farray[18] = -2.34567e9;
   farray[19] =  VAL__BADR;

   status = gsd2_copya( 5, 6, 20,
      (unsigned char *) farray, (unsigned char *) darray );
   (void) printf( "%2d conversion errors R to D\n", status );

   status = gsd2_copya( 5, 4, 20,
      (unsigned char *) farray, (unsigned char *) iarray );
   (void) printf( "%2d conversion errors R to I\n", status );

   status = gsd2_copya( 5, 3, 20,
      (unsigned char *) farray, (unsigned char *) warray );
   (void) printf( "%2d conversion errors R to W\n", status );

   status = gsd2_copya( 5, 2, 20,
      (unsigned char *) farray, (unsigned char *) larray );
   (void) printf( "%2d conversion errors R to L\n", status );

   status = gsd2_copya( 5, 1, 20,
      (unsigned char *) farray, (unsigned char *) barray );
   (void) printf( "%2d conversion errors R to B\n", status );

   for ( i = 0; i < 20; i++ )
      (void) printf( "%2d %22.15g %14.8g %11d %6d %1d %4d\n",
         i, darray[i], farray[i], iarray[i], warray[i], larray[i], barray[i] );

   iarray[0]  =   0.;
   iarray[1]  =  12.3456;
   iarray[2]  = -78.9012;
   iarray[3]  =  VAL__BADB;
   iarray[4]  =  VAL__BADW;
   iarray[5]  =  VAL__BADI;
   iarray[6]  =  VAL__BADI;
   iarray[7]  =  VAL__BADI;
   iarray[8]  =  234.567;
   iarray[9]  =  567.89;
   iarray[10] =  34567.8;
   iarray[11] =  67890.1;
   iarray[12] =  VAL__BADI;
   iarray[13] =  VAL__BADI;
   iarray[14] = -234.567;
   iarray[15] = -567.89;
   iarray[16] = -34567.8;
   iarray[17] = -67890.1;
   iarray[18] =  VAL__BADI;
   iarray[19] =  VAL__BADI;

   status = gsd2_copya( 4, 6, 20,
      (unsigned char *) iarray, (unsigned char *) darray );
   (void) printf( "%2d conversion errors I to D\n", status );

   status = gsd2_copya( 4, 5, 20,
      (unsigned char *) iarray, (unsigned char *) farray );
   (void) printf( "%2d conversion errors I to R\n", status );

   status = gsd2_copya( 4, 3, 20,
      (unsigned char *) iarray, (unsigned char *) warray );
   (void) printf( "%2d conversion errors I to W\n", status );

   status = gsd2_copya( 4, 2, 20,
      (unsigned char *) iarray, (unsigned char *) larray );
   (void) printf( "%2d conversion errors I to L\n", status );

   status = gsd2_copya( 4, 1, 20,
      (unsigned char *) iarray, (unsigned char *) barray );
   (void) printf( "%2d conversion errors I to B\n", status );

   for ( i = 0; i < 20; i++ )
      (void) printf( "%2d %22.15g %14.8g %11d %6d %1d %4d\n",
         i, darray[i], farray[i], iarray[i], warray[i], larray[i], barray[i] );

   warray[0]  =   0.;
   warray[1]  =  12.3456;
   warray[2]  = -78.9012;
   warray[3]  =  VAL__BADB;
   warray[4]  =  VAL__BADW;
   warray[5]  =  VAL__BADW;
   warray[6]  =  VAL__BADW;
   warray[7]  =  VAL__BADW;
   warray[8]  =  234.567;
   warray[9]  =  567.89;
   warray[10] =  VAL__BADW;
   warray[11] =  VAL__BADW;
   warray[12] =  VAL__BADW;
   warray[13] =  VAL__BADW;
   warray[14] = -234.567;
   warray[15] = -567.89;
   warray[16] =  VAL__BADW;
   warray[17] =  VAL__BADW;
   warray[18] =  VAL__BADW;
   warray[19] =  VAL__BADW;

   status = gsd2_copya( 3, 6, 20,
      (unsigned char *) warray, (unsigned char *) darray );
   (void) printf( "%2d conversion errors W to D\n", status );

   status = gsd2_copya( 3, 5, 20,
      (unsigned char *) warray, (unsigned char *) farray );
   (void) printf( "%2d conversion errors W to R\n", status );

   status = gsd2_copya( 3, 4, 20,
      (unsigned char *) warray, (unsigned char *) iarray );
   (void) printf( "%2d conversion errors W to I\n", status );

   status = gsd2_copya( 3, 2, 20,
      (unsigned char *) warray, (unsigned char *) larray );
   (void) printf( "%2d conversion errors W to L\n", status );

   status = gsd2_copya( 3, 1, 20,
      (unsigned char *) warray, (unsigned char *) barray );
   (void) printf( "%2d conversion errors W to B\n", status );

   for ( i = 0; i < 20; i++ )
      (void) printf( "%2d %22.15g %14.8g %11d %6d %1d %4d\n",
         i, darray[i], farray[i], iarray[i], warray[i], larray[i], barray[i] );

   barray[0]  =   0.;
   barray[1]  =  12.3456;
   barray[2]  = -78.9012;
   barray[3]  =  VAL__BADB;
   barray[4]  =  VAL__BADB;
   barray[5]  =  VAL__BADB;
   barray[6]  =  VAL__BADB;
   barray[7]  =  VAL__BADB;
   barray[8]  =  VAL__BADB;
   barray[9]  =  VAL__BADB;
   barray[10] =  VAL__BADB;
   barray[11] =  VAL__BADB;
   barray[12] =  VAL__BADB;
   barray[13] =  VAL__BADB;
   barray[14] =  VAL__BADB;
   barray[15] =  VAL__BADB;
   barray[16] =  VAL__BADB;
   barray[17] =  VAL__BADB;
   barray[18] =  VAL__BADB;
   barray[19] =  VAL__BADB;

   status = gsd2_copya( 1, 6, 20,
      (unsigned char *) barray, (unsigned char *) darray );
   (void) printf( "%2d conversion errors B to D\n", status );

   status = gsd2_copya( 1, 5, 20,
      (unsigned char *) barray, (unsigned char *) farray );
   (void) printf( "%2d conversion errors B to R\n", status );

   status = gsd2_copya( 1, 4, 20,
      (unsigned char *) barray, (unsigned char *) iarray );
   (void) printf( "%2d conversion errors B to I\n", status );

   status = gsd2_copya( 1, 2, 20,
      (unsigned char *) barray, (unsigned char *) larray );
   (void) printf( "%2d conversion errors B to L\n", status );

   status = gsd2_copya( 1, 3, 20,
      (unsigned char *) barray, (unsigned char *) warray );
   (void) printf( "%2d conversion errors B to W\n", status );

   for ( i = 0; i < 20; i++ )
      (void) printf( "%2d %22.15g %14.8g %11d %6d %1d %4d\n",
         i, darray[i], farray[i], iarray[i], warray[i], larray[i], barray[i] );

   larray[0]  =   0.;
   larray[1]  =  12.3456;
   larray[2]  = -78.9012;
   larray[3]  =  0;
   larray[4]  =  0;
   larray[5]  =  0;
   larray[6]  =  0;
   larray[7]  =  0;
   larray[8]  =  1;
   larray[9]  =  1;
   larray[10] =  1;
   larray[11] =  1;
   larray[12] =  1;
   larray[13] =  1;
   larray[14] =  1;
   larray[15] =  1;
   larray[16] =  1;
   larray[17] =  1;
   larray[18] =  1;
   larray[19] =  1;

   status = gsd2_copya( 2, 6, 20,
      (unsigned char *) larray, (unsigned char *) darray );
   (void) printf( "%2d conversion errors L to D\n", status );

   status = gsd2_copya( 2, 5, 20,
      (unsigned char *) larray, (unsigned char *) farray );
   (void) printf( "%2d conversion errors L to R\n", status );

   status = gsd2_copya( 2, 4, 20,
      (unsigned char *) larray, (unsigned char *) iarray );
   (void) printf( "%2d conversion errors L to I\n", status );

   status = gsd2_copya( 2, 3, 20,
      (unsigned char *) larray, (unsigned char *) warray );
   (void) printf( "%2d conversion errors L to W\n", status );

   status = gsd2_copya( 2, 1, 20,
      (unsigned char *) larray, (unsigned char *) barray );
   (void) printf( "%2d conversion errors L to B\n", status );

   for ( i = 0; i < 20; i++ )
      (void) printf( "%2d %22.15g %14.8g %11d %6d %1d %4d\n",
         i, darray[i], farray[i], iarray[i], warray[i], larray[i], barray[i] );

   return;
}
 */
