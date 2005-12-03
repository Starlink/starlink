#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds.h"

/* F77_INTEGER_FUNCTION(dat_drep) ( struct STR *locator_str,
 *                                  struct STR *format_str,
 *                                  struct STR *order_str,
 *                                  F77_INTEGER_TYPE *status
 *                                  TRAIL(locator_str)
 *                                  TRAIL(format_str)
 *                                  TRAIL(order_str ) )
 */
int
datDrep( const HDSLoc *locator,
         char **format_str,
         char **order_str,
         int *status)
 
{
/*
*+
*  Name:
*     DAT_DREP

*  Purpose:
*     Obtain primitive data representation information.

*  Language:
*     ANSI C

*  Invocation:
*     CALL DAT_DREP( LOC, FORMAT, ORDER, STATUS )

*  Description:
*     The routine returns information describing how the data stored in
*     a primitive object are actually represented. An object's data
*     representation will match that used by the computer system on
*     which it was created, and this forms a permanent attribute of the
*     object. If necessary, HDS will automatically perform conversion
*     to the representation used by the host computer when the data are
*     accessed (except when using DAT_BASIC, which provides direct
*     access to the data without conversion).

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Primitive object locator.
*     FORMAT = CHARACTER * ( * ) (Returned)
*        Description of the format used to encode each data element
*        (see the "Data Format" section).
*     ORDER = CHARACTER * ( * ) (Returned)
*        Description of the (byte) storage order used for each data
*        element (see the "Storage Order" section).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Data Format:
*     HDS currently supports the following encodings of primitive data
*     elements. Each description is preceded by the character string
*     returned by DAT_DREP to describe it:
*     
*     -  'BIT0': Used to encode logical values, in which the least
*     significant bit (bit zero) holds the logical value such that 1
*     implies .TRUE. and 0 implies .FALSE.. All other bits are
*     disregarded (except in "bad" data values when they are all
*     significant).
*
*     -  'NZ': Used to encode logical values, in which all bits set to
*     zero implies .FALSE. and any bit set to 1 (i.e. a non-zero data
*     value) implies .TRUE..
*
*     -  'BINARY': Used for unsigned integers; this is a straight
*     binary encoding.
*
*     -  '2COMP': Used for signed integers in which a "2's complement"
*     binary encoding of the sign information is employed.
*
*     -  'VAXF': Used for single precision floating point values; this
*     is the VAX/VMS "F-floating" number representation.
*
*     -  'IEEE_S': Used for single precision floating point values;
*     this is the standard IEEE single precision floating point format.
*
*     -  'VAXD': Used for double precision floating point values; this
*     is the VAX/VMS "D-floating" number representation.
*
*     -  'IEEE_D': Used for double precision floating point values;
*     this is the standard IEEE double precision floating point format.
*
*     -  'ASCII': Used for character strings; each character employs
*     the standard ASCII encoding.

*  Storage Order:
*     HDS currently supports the following storage orders for the bytes
*     of primitive data elements. Each description is preceded by the
*     character string returned by DAT_DREP to describe it:
*
*     -  'MSB': Most significant byte stored first.
*
*     -  'LSB': Least significant byte stored first.
*
*     In the case of floating point formats, the byte in question is the
*     most/least significant byte of the fraction.

*  Notes:
*     Not all combinations of data format and storage order are
*     supported.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-AUG-1991 (RFWS):
*        Original version.
*     26-MAR-2002 (BKM):
*        Revised to pure C version.
*     15-NOV-2005 (TIMJ):
*        Use dat1_import_loc
*        Use HDSLoc in API
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables:                                                         */
   char *fmt;                 /* Pointer to format text                     */
   char *ord;                 /* Pointer to order text                      */
   struct LCP *lcp;           /* Pointer to Locator Control Packet          */
   struct LCP_DATA *data=NULL;/* Pointer to LCP data fields                 */
   struct PDD *obj;           /* Pointer to object PDD                      */

/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( *status ) ) return *status;
      hds_gl_status = *status;

/* Import the locator and obtain a pointer to the LCP data fields.          */
      dat1_import_loc(locator, &lcp );
   if ( _ok( hds_gl_status ) )
   {
      data = &lcp->data;

/* Report an error if the object is a structure.                            */
      if ( data->struc )
      {
         hds_gl_status = DAT__OBJIN;
         ems_rep_c( "DAT_DREP_1",
                    "Object is not primitive; its data representation \
is not defined (possible programming error).",
                    &hds_gl_status );
      }
   }

/* Otherwise, obtain a pointer to the object primitive data descriptor.     */
   if ( _ok( hds_gl_status ) )
   {
      obj = &data->obj;

/* Compare the object's data format with each permitted value and obtain a  */
/* pointer to the matching character string description of the format.      */
      switch ( obj->format )
      {
         case DAT__BIT0:      /* Bit zero encodes the value              */
            fmt = "BIT0";
            break;

         case DAT__NZ:        /* Logical: non-zero ==> TRUE              */
            fmt = "NZ";
            break;

         case DAT__BINARY:    /* Binary encoded unsigned integer         */
            fmt = "BINARY";
            break;

         case DAT__2COMP:     /* 2's complement encoded signed integer   */
            fmt = "2COMP";
            break;

         case DAT__VAXF:      /* VAX single precision (F format)         */
            fmt = "VAXF";
            break;

         case DAT__IEEE_S:    /* IEEE single precision floating point    */
            fmt = "IEEE_S";
            break;

         case DAT__VAXD:      /* VAX double precision (D format)         */
            fmt = "VAXD";
            break;

         case DAT__IEEE_D:    /* IEEE double precision floating point    */
            fmt = "IEEE_D";
            break;

         case DAT__ASCII:     /* ASCII characters                        */
            fmt = "ASCII";
            break;

         default:
            fmt = "?";
            break;
      }

/* Similarly compare the object's data storage order with each permitted    */
/* value and obtain a pointer to the matching character string description  */
/* of the storage order.                                                    */
      switch ( obj->order )
      {
         case DAT__MSB:
            ord = "MSB";
            break;

         case DAT__LSB:
            ord = "LSB";
            break;

         default:
            ord = "?";
            break;
      }

/* Return the required character strings.                                   */
      *format_str = fmt;
      *order_str  = ord;
   }

/* If an error occurred, then report contextual information.                */
   if ( !_ok( hds_gl_status ) )
   {
      ems_rep_c( "DAT_DREP_ERR",
                 "DAT_DREP: Error obtaining data representation information \
for an HDS primitive.",
                 &hds_gl_status );
   }

/* Return the current global status value.                                  */
   *status = hds_gl_status;
   return *status;
}
