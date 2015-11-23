#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void dat1_cvt_format( int bad, UINT_BIG nval, const struct PDD *imp,
                         struct PDD *exp, int *nbad, int *status )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_cvt_format                                                       */

/* Purpose:                                                                 */
/*    Convert between different machine number formats.                     */

/* Invocation:                                                              */
/*    dat1_cvt_format( bad, nval, imp, exp, nbad, status )                  */

/* Description:                                                             */
/*    This function converts a stream of primitive values between the       */
/*    internal number formats used by the machines on which HDS is          */
/*    implemented.                                                          */

/* Parameters:                                                              */
/*    int bad                                                               */
/*       If this value is non-zero, then the function will check for (and   */
/*       propagate) "bad" data values. Otherwise, no bad-value propagation  */
/*       will occur.                                                        */
/*    UINT_BIG nval                                                         */
/*       Number of data elements to be processed.                           */
/*    const struct PDD *imp                                                 */
/*       Pointer to a PDD descriptor for the array of input data.           */
/*    struct PDD *exp                                                       */
/*       Pointer to a PDD descriptor for the array to receive the output    */
/*       data.                                                              */
/*    int *nbad                                                             */
/*       Pointer to an integer which will be set to the number of new "bad" */
/*       data values introduced by conversion errors (e.g. overflow).       */
/*    int *status                                                           */
/*       The inherited global status.                                       */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    -  This function will execute if *status is OK on input or if it is   */
/*    set to DAT__CONER (indicating a previous conversion error).  However, */
/*    if it is not OK in input, then no further error reports associated    */
/*    with conversion errors will be made.                                  */
/*    -  This function is written so as to execute in a machine-independent */
/*    manner, regardless of whether the number formats being converted are  */
/*    actually recognised by the host machine.  Conversion is therefore     */
/*    performed by explicit bit manipulation.                               */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    24-JUL-1991 (RFWS):                                                   */
/*       Original version.                                                  */
/*    26-JUL-1991 (RFWS):                                                   */
/*       Added logical type conversions and error reporting. Allow the      */
/*       routine to execute if *status is set to DAT__CONER. Added bad      */
/*       value propagation for logical data formats.                        */
/*    10-SEP-1992 (RFWS):                                                   */
/*       Corrected wrongly-set bit in bad IEEE double value.                */
/*    20-NOV-2015 (DSB):                                                    */
/*       Change nval from int to UNIT_BIG.                                  */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Structure Definitions:                                             */
      union I4                   /* 4-byte integer                          */
      {
         unsigned char byte[ 4 ];
         unsigned int value;
      };
      union I8                   /* 8-byte integer                          */
      {
         unsigned char byte[ 8 ];
         unsigned int value[ 2 ];
      };

/* Local Variables:                                                         */
      UINT_BIG i;                /* Loop counter for data elements          */
      static int init = 0;       /* Initialisation performed?               */
      static union I4 bad_ieee_s;/* IEEE_S bad value                        */
      static union I4 bad_vaxf;  /* VAXF bad value                          */
      static union I8 bad_ieee_d;/* IEEE_D bad value                        */
      static union I8 bad_vaxd;  /* VAXD bad value                          */
      union I4 ieee_s;           /* IEEE_S number                           */
      union I4 mask;             /* Bit mask for logical values             */
      union I4 true;             /* TRUE value for logical results          */
      union I4 vaxf;             /* VAXF number                             */
      union I8 ieee_d;           /* IEEE_D number                           */
      union I8 vaxd;             /* VAXD number                             */
      unsigned char tmp;         /* Temporary byte storage                  */
      unsigned int *in;          /* Pointer to input array                  */
      unsigned int *out;         /* Pointer to output array                 */
      unsigned int e;            /* Exponent value                          */
      unsigned int f;            /* Fraction value                          */

/*.                                                                         */

/* Check the inherited global status. Allow the routine to execute if       */
/* status is set to DAT__CONER indicating a previous conversion error.      */
      if ( !( _ok( *status ) || ( *status == DAT__CONER ) ) ) return;

/* On the first invocation of this routine, initialise the bad values for   */
/* each type of number. This should really be done on the declarations      */
/* above, but the VAX C compiler does not support initialisation of unions  */
/* at the time of writing.                                                  */
      if ( !init )
      {
         init = 1;

         bad_vaxf.byte[ 0 ] = 0xff;
         bad_vaxf.byte[ 1 ] = 0xff;
         bad_vaxf.byte[ 2 ] = 0xff;
         bad_vaxf.byte[ 3 ] = 0xff;

         bad_vaxd.byte[ 0 ] = 0xff;
         bad_vaxd.byte[ 1 ] = 0xff;
         bad_vaxd.byte[ 2 ] = 0xff;
         bad_vaxd.byte[ 3 ] = 0xff;
         bad_vaxd.byte[ 4 ] = 0xff;
         bad_vaxd.byte[ 5 ] = 0xff;
         bad_vaxd.byte[ 6 ] = 0xff;
         bad_vaxd.byte[ 7 ] = 0xff;

         bad_ieee_s.byte[ 0 ] = 0xff;
         bad_ieee_s.byte[ 1 ] = 0x7f;
         bad_ieee_s.byte[ 2 ] = 0xff;
         bad_ieee_s.byte[ 3 ] = 0xff;

         bad_ieee_d.byte[ 0 ] = 0xff;
         bad_ieee_d.byte[ 1 ] = 0xef;
         bad_ieee_d.byte[ 2 ] = 0xff;
         bad_ieee_d.byte[ 3 ] = 0xff;
         bad_ieee_d.byte[ 4 ] = 0xff;
         bad_ieee_d.byte[ 5 ] = 0xff;
         bad_ieee_d.byte[ 6 ] = 0xff;
         bad_ieee_d.byte[ 7 ] = 0xff;
      }

/* Check that the input and output primitive data types match. Report an    */
/* error if they do not.                                                    */
      if ( imp->dtype != exp->dtype )
      {
         *status = DAT__FATAL;
         emsRep( "DAT1_CVT_FORMAT_1",
                    "Routine DAT1_CVT_FORMAT called with non-matching \
primitive data types (internal programming error).",
                    status );
      }

/* Initialise, and obtain pointers to the input and output arrays.          */
      else
      {
         *nbad = 0;
         in = (unsigned int *) imp->body;
         out = (unsigned int *) exp->body;

/* Test for recognised input and output number formats.                     */
         switch ( imp->format )
         {

/* Convert from VAX/VMS single precision (F) floating point...              */
/* ===========================================================              */
            case DAT__VAXF:
            {
               switch ( exp->format )
               {

/* ...to IEEE single precision floating point.                              */
/* -------------------------------------------                              */
                  case DAT__IEEE_S:
                  {

/* Loop through the input and output arrays in groups of 4 bytes.           */
                     for ( i = 0; i < nval; i++ )
                     {

/* Extract the next VAXF number and reverse its byte order if it is not     */
/* stored in the conventional way (i.e. with the most significant part of   */
/* the fraction stored first). This should never actually happen.           */
                        vaxf.value = in[ i ];
                        if ( imp->order != DAT__MSB )
                        {
                           tmp = vaxf.byte[ 0 ];
                           vaxf.byte[ 0 ] = vaxf.byte[ 3 ];
                           vaxf.byte[ 3 ] = tmp;
                           tmp = vaxf.byte[ 1 ];
                           vaxf.byte[ 1 ] = vaxf.byte[ 2 ];
                           vaxf.byte[ 2 ] = tmp;
                        }

/* If necessary, check if this is a bad value. If so, then assign the       */
/* corresponding bad value to the IEEE_S result.                            */
                        if ( bad && ( vaxf.value == bad_vaxf.value ) )
                        {
                           ieee_s.value = bad_ieee_s.value;
                        }

/* Extract the value of the exponent.                                       */
                        else
                        {
                           e = ( ( vaxf.byte[ 1 ] << 1 ) & 0xfe ) |
                               ( ( vaxf.byte[ 0 ] >> 7 ) & 0x1 );

/* If the (biased) exponent is greater than 2, then the VAXF number can be  */
/* represented in IEEE_S form as a normalised number. Decrement the         */
/* exponent by 2. This allows for a difference of 1 between the exponent    */
/* bias values of the two number formats and a further difference of one in */
/* the assumed position of the binary radix point.                          */
                           if ( e > 2 )
                           {
                              e -= 2;

/* Construct the resulting IEEE_S number, using the appropriate bytes of    */
/* the VAXF number but replacing the exponent field with its modified       */
/* value.                                                                   */
                              ieee_s.byte[ 0 ] = ( vaxf.byte[ 1 ] & 0x80 ) |
                                                 ( ( e >> 1 ) & 0x7f );
                              ieee_s.byte[ 1 ] = ( vaxf.byte[ 0 ] & 0x7f ) |
                                                 ( ( e << 7 ) & 0x80 );
                              ieee_s.byte[ 2 ] = vaxf.byte[ 3 ];
                              ieee_s.byte[ 3 ] = vaxf.byte[ 2 ];
                           }

/* If the (biased) VAXF exponent is zero, then the resulting IEEE_S value   */
/* is zero (or we have a VAX reserved operand, but we assume that can't     */
/* happen).                                                                 */
                           else if ( e == 0 )
                           {
                              ieee_s.value = 0;
                           }

/* Otherwise, if the (biased) exponent is 2 or less, then the IEEE_S        */
/* equivalent will be a denormalised number, so the fraction field must be  */
/* modified.  Extract all the bits of the VAXF fraction field into a single */
/* integer (remember we can't assume what order the integer's bytes are     */
/* stored in).  Also add the (normally omitted) leading 1.                  */
                           else
                           {
                              f = vaxf.byte[ 2 ] |
                                  ( vaxf.byte[ 3 ] << 8 ) |
                                  ( ( vaxf.byte[ 0 ] & 0x7f ) << 16 ) |
                                  ( 0x1 << 23 );

/* Shift the fraction bits to account for the limited range of the          */
/* exponent. Then pack the fraction field into the IEEE_S number. Retain    */
/* the VAXF sign bit, but set the exponent field to zero (indicating a      */
/* denormalised number).                                                    */
                              f = f >> ( 3 - e );
                              ieee_s.byte[ 0 ] = vaxf.byte[ 1 ] & 0x80;
                              ieee_s.byte[ 1 ] = ( f >> 16 ) & 0x7f;
                              ieee_s.byte[ 2 ] = ( f >> 8 ) & 0xff;
                              ieee_s.byte[ 3 ] = f & 0xff;
                           }
                        }

/* If the required output storage order is not with the most significant    */
/* part of the fraction first, then reverse the order.                      */
                        if ( exp->order != DAT__MSB )
                        {
                           tmp = ieee_s.byte[ 0 ];
                           ieee_s.byte[ 0 ] = ieee_s.byte[ 3 ];
                           ieee_s.byte[ 3 ] = tmp;
                           tmp = ieee_s.byte[ 1 ];
                           ieee_s.byte[ 1 ] = ieee_s.byte[ 2 ];
                           ieee_s.byte[ 2 ] = tmp;
                        }

/* Enter the IEEE_S result into the output array.                           */
                        out[ i ] = ieee_s.value;
                     }
                     break;
                  }

/* Report an error if the destination format is unknown in this context.    */
                  default:
                  {
                     *status = DAT__FATAL;
                     emsSeti( "DTYPE", exp->format );
                     emsRep( "DAT1_CVT_FORMAT_2",
                                "Attempt to convert VAX/VMS single \
precision (F) floating point to unknown format; DTYPE code=^DTYPE (internal \
programming error).",
                                status );
                     break;
                  }
               }
               break;
            }

/* Convert from IEEE single precision floating point...                     */
/* ====================================================                     */
            case DAT__IEEE_S:
            {
               switch ( exp->format )
               {

/* ...to VAX/VMS single precision (F) floating point.                       */
/* --------------------------------------------------                       */
                  case DAT__VAXF:
                  {

/* Loop through the input and output arrays in groups of four bytes.        */
                     for ( i = 0; i < nval; i++ )
                     {

/* Extract the next IEEE_S number and reverse its byte order if it is not   */
/* stored with the most significant part of the fraction first.             */
                        ieee_s.value = in[ i ];
                        if ( imp->order != DAT__MSB )
                        {
                           tmp = ieee_s.byte[ 0 ];
                           ieee_s.byte[ 0 ] = ieee_s.byte[ 3 ];
                           ieee_s.byte[ 3 ] = tmp;
                           tmp = ieee_s.byte[ 1 ];
                           ieee_s.byte[ 1 ] = ieee_s.byte[ 2 ];
                           ieee_s.byte[ 2 ] = tmp;
                        }

/* If necessary, check if this is a bad value. If so, then assign the       */
/* corresponding bad value to the VAXF result.                              */
                        if ( bad  && ( ieee_s.value == bad_ieee_s.value ) )
                        {
                           vaxf.value = bad_vaxf.value;
                        }

/* Extract the value of the exponent.                                       */
                        else
                        {
                           e = ( ( ieee_s.byte[ 0 ] << 1 ) & 0xfe ) |
                               ( ( ieee_s.byte[ 1 ] >> 7 ) & 0x1 );

/* If the (biased) exponent is 255, then the IEEE_S number is either a NaN  */
/* (not a number) or an infinity, neither of which can be represented in    */
/* VAXF format. Also, if the (biased) exponent is 254, then it cannot be    */
/* incremented by 2 (as required below) without overflowing. This           */
/* corresponds to IEEE_S numbers which are too large to be represented in   */
/* VAXF format. In all these cases, assign a bad value to the VAXF result.  */
                           if ( e >= 254 )
                           {
                              vaxf.value = bad_vaxf.value;

/* Increment the conversion error count and make an error report if         */
/* appropriate.                                                             */
                              (*nbad)++;
                              if ( _ok( *status ) )
                              {
                                 *status = DAT__CONER;
                                 emsRep( "DAT1_CVT_FORMAT_3",
                                            "Overflow occurred while \
converting an IEEE single precision number to VAX F floating point format.",
                                            status );
                              }
                           }

/* Otherwise, if the (biased) exponent is not zero, then we have a          */
/* normalised IEEE_S number.  Increment the exponent by 2. This allows for  */
/* a difference of 1 between the exponent bias values of the two number     */
/* formats and a further difference of one in the assumed position of the   */
/* binary radix point.                                                      */
                           else if ( e != 0 )
                           {
                              e += 2;

/* Construct the resulting VAXF number, using the appropriate bytes of the  */
/* IEEE_S number but replacing the exponent field with its modified value.  */
                              vaxf.byte[ 0 ] = ( ieee_s.byte[ 1 ] & 0x7f ) |
                                               ( ( e << 7 ) & 0x80 );
                              vaxf.byte[ 1 ] = ( ieee_s.byte[ 0 ] & 0x80 ) |
                                               ( ( e >> 1 ) & 0x7f );
                              vaxf.byte[ 2 ] = ieee_s.byte[ 3 ];
                              vaxf.byte[ 3 ] = ieee_s.byte[ 2 ];
                           }

/* If the exponent is zero, then we either have a (possibly signed) value   */
/* of zero, or a denormalised IEEE_S number. Check to see if either of the  */
/* two most significant bits of the fraction are non-zero. If not, then we  */
/* have a zero or a denormalised number which is too small to be            */
/* represented in VAXF format. In either case, assign a zero to the VAXF    */
/* result.                                                                  */
                           else
                           {
                              if ( ( ieee_s.byte[ 1 ] & 0x60 ) == 0 )
                              {
                                 vaxf.value = 0;
                              }

/* Otherwise we have a denormalised number which is still large enough to   */
/* be converted.  Extract all the bits of the IEEE_F fraction field into a  */
/* single integer (remember we can't assume what order the integer's bytes  */
/* are stored in).                                                          */
                              else
                              {
                                 f = ieee_s.byte[ 3 ] |
                                     ( ieee_s.byte[ 2 ] << 8 ) |
                                     ( ( ieee_s.byte[ 1 ] & 0x7f ) << 16 );

/* The VAXF exponent will be 1 or 2, depending on how many bit shifts are   */
/* required to normalise the fraction (i.e. to bring a 1 into the most      */
/* significant bit position). Test the most significant bit to determine    */
/* the new exponent value and apply the corresponding shift to normalise    */
/* it.                                                                      */
                                 e = ( ieee_s.byte[ 1 ] & 0x40 ) ? 2 : 1;
                                 f = f << ( 3 - e );

/* Construct the resulting VAXF value, retaining the IEEE_S sign bit, but   */
/* building the rest from the new exponent and fraction values. Note that   */
/* the most significant bit of the new fraction (always a 1) is not stored. */
                                 vaxf.byte[ 0 ] = ( ( f >> 16 ) & 0x7f ) |
                                                  ( ( e << 7 ) & 0x80 );
                                 vaxf.byte[ 1 ] = ( ieee_s.byte[ 0 ] & 0x80 ) |
                                                  ( ( e >> 1 ) & 0x7f );
                                 vaxf.byte[ 2 ] = f & 0xff;
                                 vaxf.byte[ 3 ] = ( f >> 8 ) & 0xff;
                              }
                           }
                        }

/* If the required output storage order is not with the most significant    */
/* part of the fraction first, then reverse the order.                      */
                        if ( exp->order != DAT__MSB )
                        {
                           tmp = vaxf.byte[ 0 ];
                           vaxf.byte[ 0 ] = vaxf.byte[ 3 ];
                           vaxf.byte[ 3 ] = tmp;
                           tmp = vaxf.byte[ 1 ];
                           vaxf.byte[ 1 ] = vaxf.byte[ 2 ];
                           vaxf.byte[ 2 ] = tmp;
                        }

/* Enter the VAXF result into the output array.                             */
                        out[ i ] = vaxf.value;
                     }
                     break;
                  }

/* Report an error if the destination format is unknown in this context.    */
                  default:
                  {
                     *status = DAT__FATAL;
                     emsSeti( "DTYPE", exp->format );
                     emsRep( "DAT1_CVT_FORMAT_4",
                                "Attempt to convert IEEE single \
precision floating point to unknown format; DTYPE code=^DTYPE (internal \
programming error).",
                                status );
                     break;
                  }
               }
               break;
            }

/* Convert from VAX/VMS double precision (D) floating point format...       */
/* ==================================================================       */
            case DAT__VAXD:
            {
               switch ( exp->format )
               {

/* ...to IEEE double precision floating point.                              */
/* -------------------------------------------                              */
                  case DAT__IEEE_D:
                  {

/* Loop through the input and output arrays in groups of 8 bytes.           */
                     for ( i = 0; i < nval * 2; i += 2 )
                     {

/* Extract the next VAXD number and reverse its byte order if it is not     */
/* stored in the conventional way (i.e. with the most significant part of   */
/* the fraction stored first). This should never actually happen.           */
                        vaxd.value[ 0 ] = in[ i ];
                        vaxd.value[ 1 ] = in[ i + 1 ];
                        if ( imp->order != DAT__MSB )
                        {
                           tmp = vaxd.byte[ 0 ];
                           vaxd.byte[ 0 ] = vaxd.byte[ 7 ];
                           vaxd.byte[ 7 ] = tmp;
                           tmp = vaxd.byte[ 1 ];
                           vaxd.byte[ 1 ] = vaxd.byte[ 6 ];
                           vaxd.byte[ 6 ] = tmp;
                           tmp = vaxd.byte[ 2 ];
                           vaxd.byte[ 2 ] = vaxd.byte[ 5 ];
                           vaxd.byte[ 5 ] = tmp;
                           tmp = vaxd.byte[ 3 ];
                           vaxd.byte[ 3 ] = vaxd.byte[ 4 ];
                           vaxd.byte[ 4 ] = tmp;
                        }

/* If necessary, check if this is a bad value. If so, then assign the       */
/* corresponding bad value to the IEEE_D result.                            */
                        if ( bad &&
                             ( ( vaxd.value[ 0 ] == bad_vaxd.value[ 0 ] ) &&
                               ( vaxd.value[ 1 ] == bad_vaxd.value[ 1 ] ) ) )
                        {
                           ieee_d.value[ 0 ] = bad_ieee_d.value[ 0 ];
                           ieee_d.value[ 1 ] = bad_ieee_d.value[ 1 ];
                        }

/* Extract the value of the exponent.                                       */
                        else
                        {
                           e = ( ( vaxd.byte[ 1 ] << 1 ) & 0xfe ) |
                               ( ( vaxd.byte[ 0 ] >> 7 ) & 0x1 );

/* If the (biased) exponent is non-zero, then the VAXD number can be        */
/* represented in IEEE_D form as a normalised number. Increment the         */
/* exponent by 894. This allows for a difference of 895 between the         */
/* exponent bias values of the two number formats and a difference of one   */
/* (acting in the opposite direction) in the assumed position of the binary */
/* radix point.                                                             */
                           if ( e != 0 )
                           {
                              e += 894;

/* Construct the resulting IEEE_D number, using the appropriate bytes of    */
/* the VAXD number but replacing the exponent field with the modified       */
/* value. The input fraction bytes have to be split between the output      */
/* bytes with a displacement of 3 bits and the final 3 least significant    */
/* input bits are lost (note they are simply truncated; no rounding is      */
/* attempted).                                                              */
                              ieee_d.byte[ 0 ] =
                                 ( vaxd.byte[ 1 ] & 0x80 ) |
                                 ( ( e >> 4 ) & 0x7f );
                              ieee_d.byte[ 1 ] =
                                 ( ( vaxd.byte[ 0 ] >> 3 ) & 0xf ) |
                                 ( ( e << 4 ) & 0xf0 );
                              ieee_d.byte[ 2 ] =
                                 ( ( vaxd.byte[ 0 ] << 5 ) & 0xe0 ) |
                                 ( ( vaxd.byte[ 3 ] >> 3 ) & 0x1f );
                              ieee_d.byte[ 3 ] =
                                 ( ( vaxd.byte[ 3 ] << 5 ) & 0xe0 ) |
                                 ( ( vaxd.byte[ 2 ] >> 3 ) & 0x1f );
                              ieee_d.byte[ 4 ] =
                                 ( ( vaxd.byte[ 2 ] << 5 ) & 0xe0 ) |
                                 ( ( vaxd.byte[ 5 ] >> 3 ) & 0x1f );
                              ieee_d.byte[ 5 ] =
                                 ( ( vaxd.byte[ 5 ] << 5 ) & 0xe0 ) |
                                 ( ( vaxd.byte[ 4 ] >> 3 ) & 0x1f );
                              ieee_d.byte[ 6 ] =
                                 ( ( vaxd.byte[ 4 ] << 5 ) & 0xe0 ) |
                                 ( ( vaxd.byte[ 7 ] >> 3 ) & 0x1f );
                              ieee_d.byte[ 7 ] =
                                 ( ( vaxd.byte[ 7 ] << 5 ) & 0xe0 ) |
                                 ( ( vaxd.byte[ 6 ] >> 3 ) & 0x1f );
                           }

/* If the (biased) VAXD exponent is zero, then the resulting IEEE_D value   */
/* is zero (or we have a VAX reserved operand, but we assume that can't     */
/* happen).                                                                 */
                           else
                           {
                              ieee_d.value[ 0 ] = 0;
                              ieee_d.value[ 1 ] = 0;
                           }
                        }

/* If the required output storage order is not with the most significant    */
/* part of the fraction first, then reverse the order.                      */
                        if ( exp->order != DAT__MSB )
                        {
                           tmp = ieee_d.byte[ 0 ];
                           ieee_d.byte[ 0 ] = ieee_d.byte[ 7 ];
                           ieee_d.byte[ 7 ] = tmp;
                           tmp = ieee_d.byte[ 1 ];
                           ieee_d.byte[ 1 ] = ieee_d.byte[ 6 ];
                           ieee_d.byte[ 6 ] = tmp;
                           tmp = ieee_d.byte[ 2 ];
                           ieee_d.byte[ 2 ] = ieee_d.byte[ 5 ];
                           ieee_d.byte[ 5 ] = tmp;
                           tmp = ieee_d.byte[ 3 ];
                           ieee_d.byte[ 3 ] = ieee_d.byte[ 4 ];
                           ieee_d.byte[ 4 ] = tmp;
                        }

/* Enter the IEEE_D result into the output array.                           */
                        out[ i ] = ieee_d.value[ 0 ];
                        out[ i + 1 ] = ieee_d.value[ 1 ];
                     }
                     break;
                  }

/* Report an error if the destination format is unknown in this context.    */
                  default:
                  {
                     *status = DAT__FATAL;
                     emsSeti( "DTYPE", exp->format );
                     emsRep( "DAT1_CVT_FORMAT_5",
                                "Attempt to convert VAX/VMS double \
precision (D) floating point to unknown format; DTYPE code=^DTYPE (internal \
programming error).",
                                status );
                     break;
                  }
               }
               break;
            }

/* Convert from IEEE double precision floating point...                     */
/* ====================================================                     */
            case DAT__IEEE_D:
            {
               switch ( exp->format )
               {

/* ...to VAX/VMS double precision (D) floating point.                       */
/* --------------------------------------------------                       */
                  case DAT__VAXD:
                  {

/* Loop through the input and output arrays in groups of four bytes.        */
                     for ( i = 0; i < nval * 2; i += 2 )
                     {

/* Extract the next IEEE_D number and reverse its byte order if it is not   */
/* stored with the most significant part of the fraction first.             */
                        ieee_d.value[ 0 ] = in[ i ];
                        ieee_d.value[ 1 ] = in[ i + 1 ];
                        if ( imp->order != DAT__MSB )
                        {
                           tmp = ieee_d.byte[ 0 ];
                           ieee_d.byte[ 0 ] = ieee_d.byte[ 7 ];
                           ieee_d.byte[ 7 ] = tmp;
                           tmp = ieee_d.byte[ 1 ];
                           ieee_d.byte[ 1 ] = ieee_d.byte[ 6 ];
                           ieee_d.byte[ 6 ] = tmp;
                           tmp = ieee_d.byte[ 2 ];
                           ieee_d.byte[ 2 ] = ieee_d.byte[ 5 ];
                           ieee_d.byte[ 5 ] = tmp;
                           tmp = ieee_d.byte[ 3 ];
                           ieee_d.byte[ 3 ] = ieee_d.byte[ 4 ];
                           ieee_d.byte[ 4 ] = tmp;
                        }

/* If necessary, check if this is a bad value. If so, then assign the       */
/* corresponding bad value to the VAXD result.                              */
                        if ( bad &&
                             ( ( ieee_d.value[ 0 ] ==
                                 bad_ieee_d.value[ 0 ] ) &&
                               ( ieee_d.value[ 1 ] ==
                                 bad_ieee_d.value[ 1 ] ) ) )
                        {
                           vaxd.value[ 0 ] = bad_vaxd.value[ 0 ];
                           vaxd.value[ 1 ] = bad_vaxd.value[ 1 ];
                        }

/* Extract the value of the exponent.                                       */
                        else
                        {
                           e = ( ( ieee_d.byte[ 0 ] & 0x7f ) << 4 ) |
                               ( ( ieee_d.byte[ 1 ] >> 4 ) & 0xf );

/* If the (biased) exponent is 2047, then the IEEE_D number is either a NaN */
/* (not a number) or an infinity, neither of which can be represented in    */
/* VAXD format. Also, if the (biased) exponent is 1150 or above, then it    */
/* will exceed 255 when decremented by 894 (as required below) and cannot   */
/* then be represented in the more restricted VAXD exponent range.  This    */
/* corresponds to IEEE_D numbers which are too large to be represented in   */
/* VAXD format. In all these cases, assign a bad value to the VAXD result.  */
                           if ( e >= 1150 )
                           {
                              vaxd.value[ 0 ] = bad_vaxd.value[ 0 ];
                              vaxd.value[ 1 ] = bad_vaxd.value[ 1 ];

/* Increment the conversion error count and make an error report if         */
/* appropriate.                                                             */
                              (*nbad)++;
                              if ( _ok( *status ) )
                              {
                                 *status = DAT__CONER;
                                 emsRep( "DAT1_CVT_FORMAT_6",
                                            "Overflow occurred while \
converting an IEEE double precision number to VAX D floating point format.",
                                            status );
                              }
                           }

/* If the (biased) exponent is 894 or less, then we either have a (possibly */
/* signed) value of zero, or a denormalised IEEE_D number, or a normalised  */
/* number which is too small to be represented in VAXD format.  In all      */
/* these cases, we assign a value of zero to the VAXD result.               */
                           else if ( e <= 894 )
                           {
                              vaxd.value[ 0 ] = 0;
                              vaxd.value[ 1 ] = 0;
                           }

/* In all other cases the value can be represented as a normalised VAXD     */
/* number. Decrement the exponent by 894. This allows for a difference of   */
/* 895 between the exponent bias values of the two number formats and a     */
/* difference of one (acting in the opposite direction) in the assumed      */
/* position of the binary radix point.                                      */
                           else
                           {
                              e -= 894;

/* Construct the resulting VAXD number, using the appropriate bytes of the  */
/* IEEE_D number but replacing the exponent field with the modified value.  */
/* The input fraction bytes have to be split between the output bytes with  */
/* a displacement of 3 bits and the final 3 least significant output bits   */
/* are set to zero, being unavailable in the input format.                  */
                              vaxd.byte[ 0 ] =
                                 ( ( e << 7 ) & 0x80 ) |
                                 ( ( ieee_d.byte[ 1 ] & 0xf ) << 3 ) |
                                 ( ( ieee_d.byte[ 2 ] >> 5 ) & 0x7 );
                              vaxd.byte[ 1 ] =
                                 ( ( e >> 1 ) & 0x7f ) |
                                 ( ieee_d.byte[ 0 ] & 0x80 );
                              vaxd.byte[ 2 ] =
                                 ( ( ieee_d.byte[ 3 ] << 3 ) & 0xf8 ) |
                                 ( ( ieee_d.byte[ 4 ] >> 5 ) & 0x7 );
                              vaxd.byte[ 3 ] =
                                 ( ( ieee_d.byte[ 2 ] << 3 ) & 0xf8 ) |
                                 ( ( ieee_d.byte[ 3 ] >> 5 ) & 0x7 );
                              vaxd.byte[ 4 ] =
                                 ( ( ieee_d.byte[ 5 ] << 3 ) & 0xf8 ) |
                                 ( ( ieee_d.byte[ 6 ] >> 5 ) & 0x7 );
                              vaxd.byte[ 5 ] =
                                 ( ( ieee_d.byte[ 4 ] << 3 ) & 0xf8 ) |
                                 ( ( ieee_d.byte[ 5 ] >> 5 ) & 0x7 );
                              vaxd.byte[ 6 ] =
                                 ( ieee_d.byte[ 7 ] << 3 ) & 0xf8;
                              vaxd.byte[ 7 ] =
                                 ( ( ieee_d.byte[ 6 ] << 3 ) & 0xf8 ) |
                                 ( ( ieee_d.byte[ 7 ] >> 5 ) & 0x7 );
                           }
                        }

/* If the required output storage order is not with the most significant    */
/* part of the fraction first, then reverse the order.                      */
                        if ( exp->order != DAT__MSB )
                        {
                           tmp = vaxd.byte[ 0 ];
                           vaxd.byte[ 0 ] = vaxd.byte[ 7 ];
                           vaxd.byte[ 7 ] = tmp;
                           tmp = vaxd.byte[ 1 ];
                           vaxd.byte[ 1 ] = vaxd.byte[ 6 ];
                           vaxd.byte[ 6 ] = tmp;
                           tmp = vaxd.byte[ 2 ];
                           vaxd.byte[ 2 ] = vaxd.byte[ 5 ];
                           vaxd.byte[ 5 ] = tmp;
                           tmp = vaxd.byte[ 3 ];
                           vaxd.byte[ 3 ] = vaxd.byte[ 4 ];
                           vaxd.byte[ 4 ] = tmp;
                        }

/* Enter the VAXD result into the output array.                             */
                        out[ i ] = vaxd.value[ 0 ];
                        out[ i + 1 ] = vaxd.value[ 1 ];
                     }
                     break;
                  }

/* Report an error if the destination format is unknown in this context.    */
                  default:
                  {
                     *status = DAT__FATAL;
                     emsSeti( "DTYPE", exp->format );
                     emsRep( "DAT1_CVT_FORMAT_7",
                                "Attempt to convert IEEE double \
precision floating point to unknown format; DTYPE code=^DTYPE (internal \
programming error).",
                                status );
                     break;
                  }
               }
               break;
            }

/* Convert from logical BIT0 format (bit 0 holds the value)...              */
/* ===========================================================              */
            case DAT__BIT0:
            {
               switch ( exp->format )
               {

/* ...to logical NZ format (non-zero represents TRUE).                      */
/* ---------------------------------------------------                      */
                  case DAT__NZ:
                  {

/* Create a mask value with just bit zero of the least significant byte set */
/* to one, where the bytes are considered stored in the input data order.   */
                     mask.value = 0;
                     mask.byte[ ( imp->order == DAT__MSB ) ? 3 : 0 ] = 0x1;

/* Create a similar value to represent a TRUE result, this time using the   */
/* output byte storage order (this gives "nice" values of 0 or 1 for the    */
/* result).                                                                 */
                     true.value = 0;
                     true.byte[ ( exp->order == DAT__MSB ) ? 3 : 0 ] = 0x1;

/* Loop to process the input array (groups of 4 bytes are assumed), masking */
/* to select input bit 0 and assigning the appropriate result.              */
                     if ( !bad )
                     {
                        for ( i = 0; i < nval; i++ )
                        {
                           out[ i ] = ( in[ i ] & mask.value ) ?
                                      true.value : 0;
                        }
                     }

/* If necessary, also perform checking for bad values and propagate these   */
/* to the output array. Note that the bad value is the same for both        */
/* logical formats and is also palindromic, so we do not need to take       */
/* account of the data storage order.                                       */
                     else
                     {
                        for ( i = 0; i < nval; i++ )
                        {
                           if ( in[ i ] == dat_gl_ndr[ DAT__L ].bad.L )
                           {
                              out[ i ] = in[ i ];
                           }
                           else
                           {
                              out[ i ] = ( in[ i ] & mask.value ) ?
                                         true.value : 0 ;
                           }
                        }
                     }
                     break;
                  }

/* Report an error if the destination format is unknown in this context.    */
                  default:
                  {
                     *status = DAT__FATAL;
                     emsSeti( "DTYPE", exp->format );
                     emsRep( "DAT1_CVT_FORMAT_8",
                                "Attempt to convert BIT0 format logical values \
to unknown format; DTYPE code=^DTYPE (internal programming error).",
                                status );
                     break;
                  }
               }
               break;
            }

/* Convert from logical NZ format (non-zero represents TRUE)...             */
/* ============================================================             */
            case DAT__NZ:
            {
               switch ( exp->format )
               {

/* ...to logical BIT0 format (bit zero holds the value).                    */
/* -----------------------------------------------------                    */
                  case DAT__BIT0:
                  {

/* Create a value to represent a TRUE result with just bit zero of the      */
/* least significant byte set to 1, where the bytes are assumed stored in   */
/* the output data order (this gives "nice" values of 0 or 1 for the        */
/* result).                                                                 */
                     true.value = 0;
                     true.byte[ ( exp->order == DAT__MSB ) ? 3 : 0 ] = 0x1;

/* Loop to process the input array (groups of 4 bytes are assumed),         */
/* assigning the appropriate result.                                        */
                     if ( !bad )
                     {
                        for ( i = 0; i < nval; i++ )
                        {
                           out[ i ] = in[ i ] ? true.value : 0;
                        }
                     }

/* If necessary, also perform checking for bad values and propagate these   */
/* to the output array. Note that the bad value is the same for both        */
/* logical formats and is also palindromic, so we do not need to take       */
/* account of the data storage order.                                       */
                     else
                     {
                        for ( i = 0; i < nval; i++ )
                        {
                           if ( in[ i ] == dat_gl_ndr[ DAT__L ].bad.L )
                           {
                              out[ i ] = in[ i ];
                           }
                           else
                           {
                              out[ i ] = in[ i ] ? true.value : 0 ;
                           }
                        }
                     }
                     break;
                  }

/* Report an error if the destination format is unknown in this context.    */
                  default:
                  {
                     *status = DAT__FATAL;
                     emsSeti( "DTYPE", exp->format );
                     emsRep( "DAT1_CVT_FORMAT_9",
                                "Attempt to convert NZ format logical values \
to unknown format; DTYPE code=^DTYPE (internal programming error).",
                                status );
                     break;
                  }
               }
               break;
            }

/* Report an error if the input format is unknown.                          */
            default:
            {
               *status = DAT__FATAL;
               emsSeti( "DTYPE", imp->format );
               emsRep( "DAT1_CVT_FORMAT_10",
                          "Attempt to convert from unknown number format; \
DTYPE code=^DTYPE (internal programming error).",
                          status );
               break;
            }
         }
      }

/* Exit the routine.                                                        */
      return;
   }
