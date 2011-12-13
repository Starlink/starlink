#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include "cnf.h"                 /* F77 <-> C string handling functions     */
#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "hds.h"
#include "dat_err.h"             /* DAT__ error code definitions            */

/* F77_INTEGER_FUNCTION(dat_paren)( struct STR *locator1_str,
 *                                  struct STR *locator2_str,
 *                                  F77_INTEGER_TYPE *status
 *                                  TRAIL(locator1_str)
 *                                  TRAIL(locator2_str) )
 */

int
datParen(const HDSLoc *locator1,
         HDSLoc **locator2,
         int *status)
{
/*
*+
*  Name:
*     DAT_PAREN

*  Purpose:
*     Locate parent structure.

*  Language:
*     ANSI C

*  Invocation:
*     CALL DAT_PAREN( LOC1, LOC2, STATUS )

*  Description:
*     The routine returns a locator for the parent structure of an HDS
*     object; i.e. the structure which contains the object.

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        Object locator.
*     LOC2 = CHARACTER * ( * ) (Returned)
*        Parent structure locator.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  On successful exit, the parent structure locator will identify
*     a scalar structure (number of dimensions zero). If appropriate,
*     this may be a cell of a structure array.
*     -  An error will result, and the STATUS value DAT__OBJIN will be
*     returned if the object supplied does not have a parent; i.e. if
*     it is the top-level object in a container file. The DAT__OBJIN
*     error code is defined in the include file DAT_ERR.
*     -  If this routine is called with STATUS set, then a value of
*     DAT__NOLOC will be returned for the LOC2 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason. The
*     DAT__NOLOC constant is defined in the include file DAT_PAR.

*  Example:
*     The parent of the object A.B.C.D is A.B.C, the parent of
*     X.DATA.ARRAY(1:256) is X.DATA, and the parent of Z.STRUC(17).FLAG
*     is Z.STRUC(17).

*  Copyright:
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     Copyright (C) 2005 Particle Physics and Engineering Research Council.
*     Copyright (C) 2000, 2002 Central Laborartory of the Research Councils.
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     WFL: William Lupton (AAO)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     TIMJ: Tim Jenness       (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-JUN-1991 (RFWS):
*        Prologue added and re-written to take account of parent
*        structures which are cells in structure arrays. Also added
*        error reporting.
*     1-OCT-1991 (RFWS):
*        Fixed bug causing access violation if called with STATUS set
*        (output locator string not addressed correctly).
*     18-AUG-2000 (BKM):
*        Added tests for "extended" (HDS V4) record.
*     27-OCT-2000 (BKM):
*        Change argument list to full CNF functionality.
*     23-APR-2002 (BKM):
*        Change to C interface
*     19-APR-2004 (BKM):
*        Revised 64-bit HDS logic.
*     15-NOV-2005 (TIMJ):
*        Use dat1_import_loc
*        Use HDSLoc in API
*     14-NOV-2007 (TIMJ):
*        Only free locator2 if it was successfully allocated by this routine.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables:                                                         */
   char *name;                   /* Pointer to parent object name           */
   char nambuf[ DAT__SZNAM ];    /* Buffer for parent object name           */
   int i;                        /* Loop counter for CRV                    */
   INT_BIG off=0;                /* Parent structure offset in SRV          */
   struct HAN hancmp;            /* Handle for Component Record             */
   struct HAN hanpar;            /* Handle for parent object                */
   struct HAN hantop;            /* Handle for parent's parent record       */
   struct LCP *lcp1;             /* Pointer to input LCP                    */
   struct LCP *lcp2;             /* Pointer to output LCP                   */
   struct LCP_DATA *data1=NULL;  /* Pointer to input LCP data fields        */
   struct LCP_DATA *data2=NULL;  /* Pointer to output LCP data fields       */
   struct LCP_STATE *state2;     /* Pointer to output LCP state fields      */
   struct ODL odl;               /* Parent object Object Descriptor Label   */
   struct RCL rclcmp;            /* RCL for Component Record                */
   struct RCL rclpar;            /* RCL for parent object                   */
   struct RCL rcltop;            /* RCL for parent's parent record          */
   struct RID rid;               /* Record ID to be tested                  */
   struct RID ridcmp;            /* RID of Component Record                 */
   struct RID ridpar;            /* RID of parent object                    */
   unsigned char *crv;           /* Pointer to Component Record Vector      */
   unsigned char *srv;           /* Pointer to Structure Record Vector      */
   int loc2ok = 0;               /* Boolean to indicate that loc2 is ok     */

/*.                                                                         */

/* Check the inherited global status.                                       */
   hds_gl_status = *status;
   if ( _ok( hds_gl_status ) )
   {

/* Import the input locator and find the associated Locator Control         */
/* Packet's data fields.                                                    */
      dat1_import_loc( locator1, &lcp1 );
      if ( _ok( hds_gl_status ) )
      {
         data1 = &lcp1->data;

/* If the input object does not have a parent, then report an error.        */
         if ( data1->level == 0 )
         {
            hds_gl_status = DAT__OBJIN;
            emsRep( "DAT_PAREN_1",
                       "Object is a top-level object and has no parent \
structure (possible programming error).",
                       &hds_gl_status );
         }
      }

/* Obtain a handle for the object's parent record (this is the parent       */
/* structure's Component Record).                                           */
      if ( _ok( hds_gl_status ) )
      {
         rec_get_handle( &data1->parent, &data1->han, &hancmp );

/* Obtain the Record Control Label of this Component Record and use this to */
/* obtain a handle for its parent record (this is the Structure Record for  */
/* the parent object - possibly a structure array).                         */
         rec_get_rcl( &hancmp, &rclcmp );
         rec_get_handle( &rclcmp.parent, &hancmp, &hanpar );

/* Obtain the Record Control Label of the parent object and use this to     */
/* obtain a handle for its parent record (this is the Component/Container   */
/* Record which contains the parent object and will contain the parent      */
/* object's name).                                                          */
         rec_get_rcl( &hanpar, &rclpar );
         rec_get_handle( &rclpar.parent, &hanpar, &hantop );

/* Obtain the Record ID of the parent object and locate the Component       */
/* Record Vector of its parent record.                                      */
         rec_get_rid( &hanpar, &ridpar );
         rec_get_rcl( &hantop, &rcltop );
         rec_locate_data( &hantop, rcltop.dlen, (INT_BIG)0, 'R', &crv );

/* Search this Component Record Vector for the component entry that has a   */
/* Record ID matching that of the parent object.                            */
         for ( i = 0; _ok( hds_gl_status ); i++ )
         {
            dat1_unpack_crv( crv, i, &rid );
            if ( ( rid.bloc == ridpar.bloc ) &&
                 ( rid.chip == ridpar.chip ) )
            {
               break;
            }
         }

/* Locate the component name which goes with this Record ID and save it.    */
         dat1_locate_name( crv, i, &name );
         if ( _ok ( hds_gl_status ) )
         {
            _chmove( DAT__SZNAM, name, nambuf );
         }

/* Release the Component Record Vector.                                     */
         rec_release_data( &hantop, rcltop.dlen, (INT_BIG)0, 'R', &crv );
      }

/* Obtain the Object Descriptor Label from the parent object and test if    */
/* this object is scalar. If not, then we must identify the cell in this    */
/* structure array which is the true parent structure.                      */
      dat1_get_odl( &hanpar, &odl );
      if ( _ok( hds_gl_status ) )
      {
         off = 0;
         if ( odl.naxes != 0 )
         {

/* Obtain the Record ID of the parent structure's Component Record and      */
/* locate the Structure Record Vector for the parent object.                */
            rec_get_rid( &hancmp, &ridcmp );
            rec_locate_data( &hanpar, rclpar.dlen, (INT_BIG)0, 'R', &srv );

/* Search for the Structure Record Vector element which has a Record ID     */
/* matching that of the parent structure's Component Record.                */
            for ( ; _ok( hds_gl_status ); off++ )
            {
               dat1_unpack_srv( srv + off * SZSRV, &rid );
               if ( ( rid.chip == ridcmp.chip ) &&
                    ( rid.bloc == ridcmp.bloc ) )
               {
                  break;
               }
            }

/* Release the Structure Record Vector.                                     */
            rec_release_data( &hanpar, rclpar.dlen, (INT_BIG)0, 'R', &srv );
         }
      }

/* Export the output locator and find the data fields in the associated     */
/* Locator Control Packet.                                                  */
      dat1_alloc_lcp( locator2, &lcp2 );
      if ( _ok( hds_gl_status ) )
      {
         loc2ok = 1;
         data2 = &lcp2->data;

/* Fill in the LCP handle and parent fields for the new object and record   */
/* its hierarchical level.                                                  */
         data2->han = hanpar;
         rec_get_rid( &hantop, &data2->parent );
         data2->level = data1->level - 1;

/* Fill in the object name and type fields and propagate the group and      */
/* access mode of the input object.                                         */
         _chmove( DAT__SZNAM, nambuf, data2->name );
         _chmove( DAT__SZTYP, odl.type, data2->type );
         _chmove( DAT__SZGRP, data1->group, data2->group );
         data2->read = data1->read;

/* Initiallise the new LCP data object descriptor.                          */
         dat1_unpack_type( data2->type, &data2->obj );

/* Note the new object is a scalar structure and set its offset.            */
         data2->naxes  = 0;
         data2->size   = 1;
         data2->struc  = 1;
         data2->offset = off;

/* Initialise the LCP state fields, noting whether the new LCP identifies a */
/* cell.                                                                    */
         state2 = &data2->state;
         state2->mapped = 0;
         state2->vmcopy = 0;
         state2->unlike = 0;
         state2->slice  = 0;
         state2->cell = ( odl.naxes != 0 );
         state2->vector = 0;
         state2->broken = 0;
      }

/* If successful, then mark the new LCP as valid.                           */
      if ( _ok( hds_gl_status ) )
      {
         data2->valid = 1;
      }

/* Otherwise, defuse the new LCP.                                           */
      else
      {
         dau_defuse_lcp( &lcp2 );
      }

/* If an error occurred, then report contextual information.                */
      if ( !_ok( hds_gl_status ) )
      {
         emsRep( "DAT_PAREN_ERR",
                    "DAT_PAREN: Error locating the parent structure of an \
HDS object.",
                    &hds_gl_status );
      }
   }

/* If the routine will exit with status set, then nullify the output        */
/* locator (if it was allocated by this routine)                            */
   if ( !_ok( hds_gl_status ) && loc2ok )
   {
     dat1_free_hdsloc( locator2 );
   }

/* Return the current global status value.                                  */
   *status = hds_gl_status;
   return *status;
}
