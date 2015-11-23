#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

   void dat1_cvt_order( UINT_BIG nval, const struct PDD *imp, struct PDD *exp,
                        int *status )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    dat1_cvt_order                                                        */

/* Purpose:                                                                 */
/*    Convert the (byte) storage order of primitive data.                   */

/* Invocation:                                                              */
/*    dat1_cvt_order( nval, imp, exp, status )                              */

/* Description:                                                             */
/*    This function reverses the order of characters (bytes) in each        */
/*    element of an array of primitive input data as part of converting     */
/*    between number representations on different machines.                 */

/* Parameters:                                                              */
/*    UINT_BIG nval                                                         */
/*       Number of data elements to be processed.                           */
/*    const struct PDD *imp                                                 */
/*       Pointer to a PDD descriptor for the array of input data.           */
/*    struct PDD *exp                                                       */
/*       Pointer to a PDD descriptor for the array to receive the output    */
/*       data.                                                              */
/*    int *status                                                           */
/*       The inherited global status.                                       */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    -  This function will execute if *status is OK on input or if it is   */
/*    set to DAT__CONER (indicating a previous conversion error).           */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK)                                    */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    1-JUL-1991 (RFWS):                                                    */
/*       Original version.                                                  */
/*    22-JUL-1991 (RFWS):                                                   */
/*       Changed to use struct PDD descriptors as arguments.                */
/*    26-JUL-1991 (RFWS);                                                   */
/*       Changed to allow execution if status is set to DAT__CONER on       */
/*       entry.                                                             */
/*    20-NOV-2015 (DSB):                                                    */
/*       Change nval from int to UNIT_BIG.                                  */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      UINT_BIG i;                /* Loop counter for groups of chars        */
      int j;                     /* Loop counter for chars within groups    */
      int nswap;                 /* Number of characters to swap            */
      unsigned char *in;         /* Pointer to input array                  */
      unsigned char *out;        /* Pointer to output array                 */

/*.                                                                         */

/* Check the inherited global status. Allow the routine to execute if       */
/* status is set to DAT__CONER indicating a previous conversion error.      */
      if ( !( _ok( *status ) || ( *status == DAT__CONER ) ) ) return;

/* Obtain pointers to the input and output data arrays and determine the    */
/* number of characters to be swapped.                                      */
      in = imp->body;
      out = exp->body;
      nswap = imp->length;

/* Test for common special values of nswap.                                 */
      switch ( nswap )
      {

/* If nswap is 1, simply copy the characters.                               */
         case 1:
            memcpy( (void *) out, (void *) in, nval * nswap );
            break;

/* If nswap is two, loop to swap characters in pairs.                       */
         case 2:
            for ( i = 1; i < nval * nswap; i += 2 )
            {
               out[ i - 1 ] = in[ i ];
               out[ i ] = in[ i - 1 ];
            }
            break;

/* If nswap is four, loop to swap characters in groups of 4.                */
         case 4:
            for ( i = 3; i < nval * nswap; i += 4 )
            {
               out[ i - 3 ] = in[ i ];
               out[ i - 2 ] = in[ i - 1 ];
               out[ i - 1 ] = in[ i - 2 ];
               out[ i ] = in[ i - 3 ];
            }
            break;

/* If nswap is eight, loop to swap characters in groups of 8.               */
         case 8:
            for ( i = 7; i < nval * nswap; i += 8 )
            {
               out[ i - 7 ] = in[ i ];
               out[ i - 6 ] = in[ i - 1 ];
               out[ i - 5 ] = in[ i - 2 ];
               out[ i - 4 ] = in[ i - 3 ];
               out[ i - 3 ] = in[ i - 4 ];
               out[ i - 2 ] = in[ i - 5 ];
               out[ i - 1 ] = in[ i - 6 ];
               out[ i ] = in[ i - 7 ];
            }
            break;

/* For other values of nswap, cater for the general case. This involves an  */
/* extra loop.                                                              */
         default:
            for ( i = nswap - 1; i < nval * nswap; i += nswap )
            {
               for ( j = 0; j < nswap; j++ )
               {
                  out[ i - j ] = in[ i + j - ( nswap - 1 ) ];
               }
            }
            break;
      }

/* Exit the routine.                                                        */
      return;
   }
