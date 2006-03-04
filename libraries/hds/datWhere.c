#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "hds2.h"                /* Private public prototypes               */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

#include "hds2.h"

int
datWhere(HDSLoc *locator,
         INT_BIG *block,
         int *offset,
         int *status)
{
/*
*+
*  Name:
*     DAT_WHERE

*  Purpose:
*     Find position of primitive in HDS file.

*  Language:
*     ANSI C

*  Invocation:
*     CALL DAT_WHERE( LOC, BLOCK, OFFSET, STATUS )

*  Description:
*     The routine returns information describing the position in an HDS
*     container file at which the data associated with a primitive
*     object are stored.

*  Arguments:
*     LOC = CHARACTER * ( * ) (Given)
*        Primitive object locator.
*     BLOCK = INTEGER (Returned)
*        Number of the file block in which the object's data start. HDS
*        file blocks are 512 bytes long and are numbered from the
*        beginning of the file, starting at block 1.
*     OFFSET = INTEGER (Returned)
*        Byte offset (zero based) of the start of data within the file
*        block.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The use of this routine is not recommended. It is provided
*     solely for use by experienced programmers who are familiar with
*     the internal structure of HDS container files and who wish to
*     access the file contents directly. Note, however, that changes to
*     the format of an HDS file may occur in future.
*     -  The start of the data associated with a cell or a slice of a
*     primitive object may be located with this routine, but the data
*     associated with a slice will not, in general, be stored at
*     contiguous locations within the file.
*     -  Care must be taken that no changes are made to adjacent bytes
*     within the file which are not part of the requested object's
*     data.
*     -  Note that the data associated with primitive objects may not
*     necessarily be stored contiguously in future versions of HDS.

*  Authors:
*     WFL: William Lupton (AAO)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     ??? (WFL):
*        Original version.
*     13-AUG-1991 (RFWS):
*        Added prologue and tidied. Made portable.
*     15-NOV-2005 (TIMJ):
*        Use dat1_import_loc
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables:                                                         */
   INT_BIG objlen;               /* Object length in chars                  */
   INT_BIG objoff;               /* Object offset from start of data        */
   struct LCP *lcp;              /* Pointer to Locator Control Packet       */
   struct LCP_DATA *data=NULL;   /* Pointer to LCP data fields              */
   struct PDD *obj;              /* Pointer to object PDD                   */

/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( *status ) ) return *status;
      hds_gl_status = *status;

/* Import the locator and obtain a pointer to the LCP data fields.          */
      dat1_import_loc(locator, &lcp );
   if ( _ok( hds_gl_status ) )
   {
      data = &lcp->data;

/* Report an error if the object is not primitive.                          */
      if ( data->struc )
      {
         hds_gl_status = DAT__OBJIN;
         emsRep( "DAT_WHERE_1",
                    "Object is not primitive; position in container file \
is not defined (possible programming error).",
                    &hds_gl_status );
      }
   }

/* Calculate the length (in chars) of the object data and determine the     */
/* offset into the object record's dynamic domain.                          */
   if ( _ok( hds_gl_status ) )
   {
      obj = &data->obj;
      objlen = obj->length * data->size;
      objoff = obj->length * data->offset;

/* Determine where in the container file the object data begins.            */
      rec_where( &data->han, objlen, objoff, block, offset );
   }

/* If an error occurred, then report contextual information.                */
   if ( !_ok( hds_gl_status ) )
   {
      emsRep( "DAT_WHERE_ERR",
                 "DAT_WHERE: Error finding the position of primitive data \
in an HDS container file.",
                 &hds_gl_status );
   }

/* Return the current global status value.                                  */
   *status = hds_gl_status;
   return *status;
}
