#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Private rec_ definitions                */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */

int
hdsCopy(char locator_str[DAT__SZLOC],
        char *file_str,
        char name_str[DAT__SZNAM],
        int *status )
{
/*
*+
*  Name:
*     HDS_COPY

*  Purpose:
*     Copy an object to a new container file.

*  Language:
*     Starlink C

*  Invocation:
*     CALL hdsCopy( locator, file, name, status )

*  Description:
*     The routine makes a copy of an HDS object, placing the copy in a
*     new container file (which is created), as the top-level object.
*     The copying operation is recursive; i.e. all sub-components of a
*     structure will also be copied.

*  Arguments:
*     locator = char[DAT__SZLOC] (Given)
*        Locator for the object to be copied.
*     file = char * (Given)
*        Name of the new container file to be created.
*     name= char[DAT__SZNAM] (Given)
*        Name which the new top-level object is to have.
*     status = int * (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to eliminate unused space during the
*     copying operation and is therefore a useful method of compressing
*     a container file from which components have been deleted.
*     -  The routine may be used to copy both primitive and structured
*     objects, but cannot be used to make a copy of a cell or a slice.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     WFL: William Lupton (AAO)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM: Brian McIlwrath (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     18-JUL-1991 (RFWS):
*        Complete re-write to avoid file corruption problems associated
*        with previous attempts to copy more quickly. The routine now
*        always performs a recursive copy and therefore recovers unused
*        space. Tidied and added prologue.
*     21-AUG-1991 (RFWS):
*        Turn off file mapping for efficiency.
*     8-SEP-1992 (RFWS):
*        Allow copy operation to use file mapping or I/O according to
*        which performs better on the host machine.
*     8-FEB-1993 (RFWS):
*        Fixed bug which caused a crash if the output file was not
*        opened successfully (extra ststus check needed).
*     16-JAN-2002 (BKM):
*        Convert from C function with a FORTRAN interface to a C-callable
*        function with FORTRAN wrapper.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables:                                                         */
   char *name2;                      /* Pointer to name in output CRV       */
   char nambuf[ DAT__SZNAM ];        /* Name buffer                         */
   int save_map;              /* Saved mapping flag value                   */
   struct DSC file;           /* Descriptor for file name                   */
   struct DSC locator;        /* Descriptor for locator                     */
   struct DSC name;           /* Descriptor for component name              */
   struct HAN han1;           /* Handle to input record                     */
   struct HAN han2;           /* Handle to output record                    */
   struct LCP *lcp;           /* Pointer to Locator Control Packet          */
   struct LCP_DATA *data=NULL;/* Pointer to LCP data fields                 */
   struct LCP_STATE *state;   /* Pointer to LCP state fields                */
   struct RCL rcl;            /* Record Control Label                       */
   struct RID rid1;           /* Input Record ID                            */
   unsigned char *crv2;       /* Pointer to output CRV                      */
   unsigned char crv1[DAT__SZCRV];   /* Input Component Record Vector       */

/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( *status ) ) return *status;
      hds_gl_status = *status;

/* Import the source locator string and the file and name strings.          */
   _strflcsimp( &locator, locator_str, DAT__SZLOC );
   _strcsimp( &file, file_str );
   _strcsimp( &name, name_str );

/* Import the source locator.                                               */
   dau_import_loc( &locator, &lcp );
   if ( _ok( hds_gl_status ) )
   {

/* Find the Locator Control Packet data and state fields.                   */
      data = &lcp->data;
      state = &data->state;

/* Report an error if the locator is associated with an array slice or      */
/* a cell.                                                                  */
      if ( state->slice )
      {
         hds_gl_status = DAT__OBJIN;
         ems_rep_c( "HDS_COPY_1",
                    "Input object is an array slice and cannot be copied \
(possible programming error).",
                    &hds_gl_status );
      }
      else if ( state->cell )
      {
         hds_gl_status = DAT__OBJIN;
         ems_rep_c( "HDS_COPY_2",
                    "Input object is an array cell and cannot be copied \
(possible programming error).",
                    &hds_gl_status );
      }
   }

/* Validate the output object's name.                                       */
   dau_check_name( &name, nambuf );

/* Save the current value of the global mapping flag and set a new value to */
/* give the best performance for sequential access.                         */
   save_map = hds_gl_map;
   hds_gl_map = ( HDS__MAPSEQ && HDS__CANMAP );

/* Obtain a handle for the input record and manufacture a Component Record  */
/* Vector for it (only the RID component is needed).                        */
   if ( _ok( hds_gl_status ) )
   {
      han1 = data->han;
      rec_get_rid( &han1, &rid1 );
      dat1_pack_crv( &rid1, 0, crv1 );

/* Attach a handle to the new (output) container file, thus creating the    */
/* file.                                                                    */
      hds_gl_64bit = hds_gl_c64bit;
      rcl.zero = 1;
      rcl.class = DAT__CONTAINER;
      rcl.slen = 0;
      rcl.dlen = SZCRV;
      rec_attach_file( 1, (const char *) file.body, file.length, 'N', 'W',
                       &rcl, &han2 );
      if ( _ok( hds_gl_status ) )
      {

/* Locate the output Container Record Vector and copy the new top-level     */
/* component name into it.                                                  */
         rec_locate_data( &han2, SZCRV, 0, 'W', &crv2 );
         dat1_locate_name( crv2, 0, &name2 );
         if ( _ok( hds_gl_status ) )
         {
            (void) memcpy( (void *) name2, (void *) nambuf,
                           (size_t) DAT__SZNAM );
         }

/* Perform the copy and release the CRV.                                    */
         dau_copy_object( 1, &han1, crv1, &han2, crv2 );
         rec_release_data( &han2, SZCRV, 0, 'W', &crv2 );

/* Close the new file.                                                      */
         rec_close_file( &han2 );
      }
   }

/* Restore the value of the global mapping flag.                            */
   hds_gl_map = save_map;

/* If an error occurred, then report contextual information.                */
   if ( !_ok( hds_gl_status ) )
   {
      ems_rep_c( "HDS_COPY_ERR",
                 "HDS_COPY: Error copying an HDS object to a new container \
file.",
                 &hds_gl_status );
   }

/* Return the current global status value.                                  */
   *status = hds_gl_status;
   return *status;
}
