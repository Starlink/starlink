/*+
 * Name:
 *    gsd2_nativ<t>

 * Purpose:
 *    Convert VAX to local bit patterns.

 * Language:
 *    ANSI C

 * Type of Module:
 *    C function.

 * Prototype:
 *    (available via #include "gsd1.h")
 *    void gsd2_nativ{blwirdc}( unsigned char *bytes );
 *    void gsd2_nativa( char *ch_ptr, enum type_tag d_type, int d_length );

 * Description:
 *    These routines convert the given VAX bytes into bytes that on the local
 *    machine represent the same value in the equivalent type.
 *
 *    <t> <type>     Fortran       GSD
 *     b   char      byte          byte
 *     l   char      logical*1     logical
 *     w   short     integer*2     word
 *     i   int       integer*4     integer
 *     r   float     real*4        real
 *     d   double    real*8        double
 *     c   char[16]  character*16  char
 *
 *    These routines will convert bad values according to the old VAX GSD
 *    library into bad values according to PRIMDAT on the local machine.
 *
 *    The routine gsd2_nativa wraps up the other routines for an array of any
 *    type.

 * Arguments:
 *    unsigned char *bytes (Given and Returned)
 *       The bytes to be converted. These are replaced in situ. Their alignment
 *       is not critical, since they are only treated as byte arrays.
 *    char *ch_ptr (Given and Returned)
 *       The array to be converted. This need not be aligned, since it is
 *       treated only as a byte array.
 *    enum type_tag d_type (Given)
 *       The data type code for the array to be converted.
 *    int d_length (Given)
 *       The size in bytes (!) of the array to be converted.

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)

 * History:
 *    08 Dec 1994 (hme):
 *       Original.
 *-
 */

#include "gsd2.h"

/*:
 */

void gsd2_nativb( unsigned char *bytes )
{
   if ( bytes[0] == gsd__bbad[0] ) bytes[0] = val__badb[0];
   return;
}

/*:
 */

void gsd2_nativl( unsigned char *bytes )
{
   bytes[0] &= 0x1;
   return;
}

/*:
 */

void gsd2_nativw( unsigned char *bytes )
{
   unsigned char temp;

/* If bad, replace with bad.
 */
   if ( bytes[0] == gsd__wbad[0] && bytes[1] == gsd__wbad[1] )
   {    bytes[0] =  val__badw[0];   bytes[1] =  val__badw[1];
   }

/* Else swap to local byte order.
 */
   else if ( host_order[2] == BIGEND )
   {  temp = bytes[0]; bytes[0] = bytes[1]; bytes[1] = temp;
   }

   return;
}

/*:
 */

void gsd2_nativi( unsigned char *bytes )
{
   unsigned char temp;

/* If bad, replace with bad.
 */
   if ( bytes[0] == gsd__ibad[0] && bytes[1] == gsd__ibad[1] &&
        bytes[2] == gsd__ibad[2] && bytes[3] == gsd__ibad[3]    )
   {    bytes[0] =  val__badi[0];   bytes[1] =  val__badi[1];
        bytes[2] =  val__badi[2];   bytes[3] =  val__badi[3];
   }

/* Else swap to local byte order.
 */
   else if ( host_order[3] == BIGEND )
   {
      temp = bytes[0]; bytes[0] = bytes[3]; bytes[3] = temp;
      temp = bytes[1]; bytes[1] = bytes[2]; bytes[2] = temp;
   }

   return;
}

/*:
 */

void gsd2_nativr( unsigned char *bytes )
{
   unsigned char temp;
   unsigned char tmp[4];
   unsigned int  e, f;
   int           i;

/* If bad value.
 */
   if ( bytes[0] == gsd__rbad[0] && bytes[1] == gsd__rbad[1] &&
        bytes[2] == gsd__rbad[2] && bytes[3] == gsd__rbad[3]    )
   {  for ( i = 0; i < 4; i++ ) bytes[i] = val__badr[i];
   }

/* Else if IEEE format (with any byte order) required.
 */
   else if ( host_order[4] != VAXF )
   {
/*    Extract the exponent.
 */
      e = ( ( bytes[1] << 1 ) & 0xfe ) | ( ( bytes[0] >> 7 ) & 0x1 );

/*    If the (biased) exponent is greater than 2, then the VAXF number can be
 *    represented in IEEE_S form as a normalised number. Decrement the
 *    exponent by 2. This allows for a difference of 1 between the exponent
 *    bias values of the two number formats and a further difference of one in
 *    the assumed position of the binary radix point.
 */
      if ( e > 2 )
      {  e -= 2;

/*       Construct the resulting IEEE_S number, using the appropriate bytes of
 *       the VAXF number but replacing the exponent field with its modified
 *       value.
 */
         tmp[0] = ( bytes[1] & 0x80 ) | ( ( e >> 1 ) & 0x7f );
         tmp[1] = ( bytes[0] & 0x7f ) | ( ( e << 7 ) & 0x80 );
         tmp[2] = bytes[3]; tmp[3] = bytes[2];
      }

/*    If the (biased) VAXF exponent is zero, then the resulting IEEE_S value
 *    is zero (or we have a VAX reserved operand, but we assume that can't
 *    happen).
 */
      else if ( e == 0 )
      {  for ( i = 0; i < 4; i++ ) tmp[i] = 0;
      }

/*    Otherwise, if the (biased) exponent is 2 or less, then the IEEE_S
 *    equivalent will be a denormalised number, so the fraction field must be
 *    modified.  Extract all the bits of the VAXF fraction field into a single
 *    integer (remember we can't assume what order the integer's bytes are
 *    stored in).  Also add the (normally omitted) leading 1.
 */
      else
      {  f =     bytes[2]
           |   ( bytes[3]          <<  8 )
           | ( ( bytes[0] & 0x7f ) << 16 )
           |   ( 0x1               << 23 );

/*       Shift the fraction bits to account for the limited range of the
 *       exponent. Then pack the fraction field into the IEEE_S number. Retain
 *       the VAXF sign bit, but set the exponent field to zero (indicating a
 *       denormalised number).
 */
         f = f >> ( 3 - e );
         tmp[0] =   bytes[1]  & 0x80;
         tmp[1] = ( f >> 16 ) & 0x7f;
         tmp[2] = ( f >>  8 ) & 0xff;
         tmp[3] =   f         & 0xff;
      }

/*    If IEEEBS byte order required.
 *    This is the Alpha. We assume that we have to swap. But that may be wrong.
 */
      if ( host_order[4] == IEEEBS )
      {  for( i = 0; i < 4; i++ ) bytes[i] = tmp[3-i];
      }

/*    Else (IEEE without BS) byte order required.
 *    This is the Sun. We assume that we need not swap. But that may be wrong.
 */
      else
      {  for( i = 0; i < 4; i++ ) bytes[i] = tmp[i];
      }

   }  /* End of "IEEE any order" block */

   return;
}

/*:
 */

void gsd2_nativd( unsigned char *bytes )
{
   unsigned char temp;
   unsigned char tmp[8];
   unsigned int  e;
   int           i;

/* If bad value.
 */
   if ( bytes[0] == gsd__dbad[0] && bytes[1] == gsd__dbad[1] &&
        bytes[2] == gsd__dbad[2] && bytes[3] == gsd__dbad[3] &&
        bytes[4] == gsd__dbad[4] && bytes[5] == gsd__dbad[5] &&
        bytes[6] == gsd__dbad[6] && bytes[7] == gsd__dbad[7]    )
   {  for ( i = 0; i < 8; i++ ) bytes[i] = val__badd[i];
   }

/* Else if IEEE format (with any byte order) required.
 */
   else if ( host_order[5] != VAXF )
   {
/*    Extract the exponent.
 */
      e = ( ( bytes[1] << 1 ) & 0xfe ) | ( ( bytes[0] >> 7 ) & 0x1 );

/*    If the (biased) exponent is non-zero, then the VAXD number can be
 *    represented in IEEE_D form as a normalised number. Increment the
 *    exponent by 894. This allows for a difference of 895 between the exponent
 *    bias values of the two number formats and a further difference of one
 *    (acting in the opposite direction) in the assumed position of the binary
 *    radix point.
 */
      if ( e != 0 )
      {  e += 894;

/*       Construct the resulting IEEE_D number, using the appropriate bytes of
 *       the VAXD number but replacing the exponent field with its modified
 *       value. The input fraction bytes have to be split between the output
 *       bytes with a displacement of 3 bits and the final 3 least significant
 *       input bits are lost (note they are simply truncated; no rounding is
 *       attempted).
 */
         tmp[0] =   ( bytes[1]        & 0x80 ) | ( ( e >> 4 ) & 0x7f );
         tmp[1] = ( ( bytes[0] >> 3 ) & 0xf  ) | ( ( e << 4 ) & 0xf0 );
         tmp[2] = ( ( bytes[0] << 5 ) & 0xe0 ) | ( ( bytes[3] >> 3 ) & 0x1f );
         tmp[3] = ( ( bytes[3] << 5 ) & 0xe0 ) | ( ( bytes[2] >> 3 ) & 0x1f );
         tmp[4] = ( ( bytes[2] << 5 ) & 0xe0 ) | ( ( bytes[5] >> 3 ) & 0x1f );
         tmp[5] = ( ( bytes[5] << 5 ) & 0xe0 ) | ( ( bytes[4] >> 3 ) & 0x1f );
         tmp[6] = ( ( bytes[4] << 5 ) & 0xe0 ) | ( ( bytes[7] >> 3 ) & 0x1f );
         tmp[7] = ( ( bytes[7] << 5 ) & 0xe0 ) | ( ( bytes[6] >> 3 ) & 0x1f );
      }

/*    If the (biased) VAXD exponent is zero, then the resulting IEEE_D value
 *    is zero (or we have a VAX reserved operand, but we assume that can't
 *    happen).
 */
      else
      {  for ( i = 0; i < 8; i++ ) tmp[i] = 0;
      }

/*    If IEEEBS byte order required.
 *    This is the Alpha. We assume that we have to swap. But that may be wrong.
 */
      if ( host_order[5] == IEEEBS )
      {  for( i = 0; i < 8; i++ ) bytes[i] = tmp[7-i];
      }

/*    Else (IEEE without BS) byte order required.
 *    This is the Sun. We assume that we need not swap. But that may be wrong.
 */
      else
      {  for( i = 0; i < 8; i++ ) bytes[i] = tmp[i];
      }

   }  /* End of "IEEE any order" block */

   return;
}

/*:
 */

void gsd2_nativc( unsigned char *bytes ) { return; }

/*:
 */

void gsd2_nativa( char *ch_ptr, enum type_tag d_type, int d_length )
{
   unsigned char *ch;
   int            i, k, num;

/*.
 */

/* Cast the given char pointer to unsigned.
 */
   ch = (unsigned char *) ch_ptr;

/* For each array element make the appropriate call to the nativx routine.
 * We save ourselves the calls for char[16] arrays, since nothing is going to
 * happen in that case.
 */
   switch (d_type)
   {  case typ_byte:
         num = d_length / GSD_SZBYTE;
         for ( k = 0; k < num; k++, ch += GSD_SZBYTE )
            (void) gsd2_nativb( ch );
         break;
      case typ_logical:
         num = d_length / GSD_SZLOGICAL;
         for ( k = 0; k < num; k++, ch += GSD_SZLOGICAL )
            (void) gsd2_nativl( ch );
         break;
      case typ_word:
         num = d_length / GSD_SZWORD;
         for ( k = 0; k < num; k++, ch += GSD_SZWORD )
            (void) gsd2_nativw( ch );
         break;
      case typ_int:
         num = d_length / GSD_SZINTEGER;
         for ( k = 0; k < num; k++, ch += GSD_SZINTEGER )
            (void) gsd2_nativi( ch );
         break;
      case typ_real:
         num = d_length / GSD_SZREAL;
         for ( k = 0; k < num; k++, ch += GSD_SZREAL )
            (void) gsd2_nativr( ch );
         break;
      case typ_double:
         num = d_length / GSD_SZDOUBLE;
         for ( k = 0; k < num; k++, ch += GSD_SZDOUBLE )
            (void) gsd2_nativd( ch );
         break;
      case typ_char:
         break;
      default:
         break;
   }

   return;
}

/* Test programme.
 * ===============


 * The test programme uses the file TEST_ARRAY.DAT that is supposed to have
 * been written with WRITE_TEST_ARRAY.FOR on a VAX.

hashinclude <stdio.h>

void main( void )
{
   FILE       *fptr;
   double      darray[20];
   float       farray[20];
   int         iarray[20];
   short       warray[20];
   signed char barray[20];
   int         i;

   fptr = fopen( "TEST_ARRAY.DAT", "rb" );

   (void) fread( darray, sizeof(double),      20, fptr );
   (void) fread( farray, sizeof(float),       20, fptr );
   (void) fread( iarray, sizeof(int),         20, fptr );
   (void) fread( warray, sizeof(short),       20, fptr );
   (void) fread( barray, sizeof(signed char), 20, fptr );

   (void) gsd2_nativa( (char *) darray, 6, 160 );
   (void) gsd2_nativa( (char *) farray, 5,  80 );
   (void) gsd2_nativa( (char *) iarray, 4,  80 );
   (void) gsd2_nativa( (char *) warray, 3,  40 );
   (void) gsd2_nativa( (char *) barray, 1,  20 );

   for ( i = 0; i < 20; i++ )
      (void) printf( "%2d %22.15g %14.8g %11d %6d %4d\n",
         i, darray[i], farray[i], iarray[i], warray[i], barray[i] );

   (void) fclose( fptr );
   return;
}
 */
