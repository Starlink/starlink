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

#include "hds.h"

/* F77_INTEGER_FUNCTION(dat_copy)( struct STR *locator1_str,
 *                                 struct STR *locator2_str,
 *                                 struct STR *name_str,
 *                                 F77_INTEGER_TYPE *status
 *                                 TRAIL(locator1_str)
 *                                 TRAIL(locator2_str)
 *                                 TRAIL(name_str) )
 */

int
datCopy(const HDSLoc * locator1,
        const HDSLoc * locator2,
        const char *name_c,
        int  *status )
{
/*
*+
*  Name:
*     DAT_COPY

*  Purpose:
*     Copy an object.

*  Language:
*     ANSI C

*  Invocation:
*     CALL DAT_COPY( LOC1, LOC2, NAME, STATUS )

*  Description:
*     The routine makes a copy of an HDS object, placing the copy into
*     an existing structure as a new component. The copying operation
*     is recursive; i.e. all sub-components of a structure will also be
*     copied.

*  Arguments:
*     LOC1 = CHARACTER * ( * ) (Given)
*        Locator for the object to be copied.
*     LOC2 = CHARACTER * ( * ) (Given)
*        Locator for an existing scalar structure which is to receive
*        the new component.
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the new structure component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to eliminate any unused space during the
*     copying operation.
*     -  The routine may be used to copy both primitive and structured
*     objects, but cannot be used to make a copy of a cell or a slice.
*     -  The output structure component should not already exist.

*  Authors:
*     WFL: William Lupton (AAO)
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-AUG-1991 (RFWS):
*        Made portable, tidied and added prologue. Installed error
*        reporting and local setting of the global mapping flag.
*     8-SEP-1992 (RFWS):
*        Allow copy operation to use file mapping or I/O according to
*        which performs better on the host machine.
*     30-JUN-2000 (BKM):
*        Add argument to call to dat1_pack_crv
*     24-NOV-2000 (BKM):
*        Under Linux changes to mapped files are written directly to disk but
*        NOT reflected in the (buffered) sequential access - flush the FCB
*        if we change modes.    
*     17-MAY-2002 (BKM):
*        Make int a pure C routine.
*     24-NOV-2000 (BKM):
*        Under Linux changes to mapped files are written directly to disk but
*        NOT reflected in the (buffered) sequential access - flush the FCB
*        if we change modes.    
*     14-JUN-2005 (BKM):
*        Fix copying from 32<>64bit locators
*     15-NOV-2005 (TIMJ):
*        Use dat1_import_loc
*        Use HDSLoc in API
*     {enter_further_changes_here}
*
*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables:                                                         */
   char *name1;                  /* Pointer to component name               */
   char nambuf[ DAT__SZNAM ];    /* Buffer for new component name           */
   int i;                        /* Loop counter for components             */
   int ncomp;                    /* Number of output components             */
   int szcrv;                    /* Size of a CRV element.                  */
   INT_BIG off;                  /* Offset into Structure Record Vector     */
   int save_map;                 /* Saved global mapping flag value         */
   struct DSC name;              /* Output name string descriptor           */
   struct HAN han;               /* Handle for output component record      */
   struct LCP *lcp1;             /* Pointer to locator1 LCP                 */
   struct LCP *lcp2;             /* Pointer to locator2 LCP                 */
   struct LCP_DATA *data1=NULL;  /* Pointer to locator1 data fields         */
   struct LCP_DATA *data2=NULL;  /* Pointer to locator2 data fields         */
   struct LCP_STATE *state1;     /* Pointer to locator1 state fields        */
   struct RCL rcl;               /* RCL for component record                */
   struct RID rid1;              /* Record ID for input object              */
   struct RID rid;               /* ID of component record                  */
   unsigned char *crv2;          /* Pointer to output CRV                   */
   unsigned char *srv;           /* Pointer to Structure Record Vector      */
   unsigned char crv1[ DAT__SZCRV ]; /* Input CRV - note sized for 64bit    */
   int lcp1_64bit;               /* Locator 1 64-bit state                  */
   int lcp2_64bit;               /* Locator 2 64-bit state                  */
/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( *status ) ) return *status;
      hds_gl_status = *status;

/* Import name strings. */
   _strcsimp(   &name, name_c );

/* Import the first locator.                                                */
   dat1_import_loc(locator1, &lcp1 );
   if ( _ok( hds_gl_status ) )
   {

/* Obtain pointers to the LCP data and state fields.                        */
      data1 = &lcp1->data;
      state1 = &data1->state;
      lcp1_64bit = hds_gl_64bit;

/* Report an error if the locator is associated with an array slice or      */
/* a cell.                                                                  */
      if ( state1->slice )
      {
         hds_gl_status = DAT__OBJIN;
         ems_rep_c( "DAT_COPY_1",
                    "Input object is an array slice and cannot be copied \
(possible programming error).",
                    &hds_gl_status );
      }
      else if ( state1->cell )
      {
         hds_gl_status = DAT__OBJIN;
         ems_rep_c( "DAT_COPY_2",
                    "Input object is an array cell and cannot be copied \
(possible programming error).",
                    &hds_gl_status );
      }
   }

/* Manufacture a dummy Component Record Vector for the input structure.     */
/* Only the RID component is needed.                                        */
   if ( _ok( hds_gl_status ) )
   {
      rec_get_rid( &data1->han, &rid1 );
      dat1_pack_crv( &rid1, 0, crv1 );

/* Import the second locator and obtain a pointer to the LCP data fields.   */
      dat1_import_loc(locator2, &lcp2 );
      if ( _ok( hds_gl_status ) )
      {
         data2 = &lcp2->data;
         lcp2_64bit = hds_gl_64bit;

/* Report an error if the locator is associated with anything other than a  */
/* scalar structure.                                                        */
         if ( !data2->struc )
         {
            hds_gl_status = DAT__OBJIN;
            ems_rep_c( "DAT_COPY_3",
                       "Output object is not a structure (possible \
programming error).",
                       &hds_gl_status );
         }
         else if ( data2->naxes != 0 )
         {
            hds_gl_status = DAT__OBJIN;
            ems_rep_c( "DAT_COPY_4",
                       "Output object is not scalar (possible programming \
error).",
                       &hds_gl_status );
         }
      }
   }

/* Validate the new object's name.                                          */
   dau_check_name( &name, nambuf );

/* Save the current value of the global mapping flag and set a new value to */
/* give the best performance for sequential access.                         */
   save_map = hds_gl_map;
   hds_gl_map = ( HDS__MAPSEQ && HDS__CANMAP );

/* Flush the output FCB                                                    */
      if( save_map != hds_gl_map )
         fflush( rec_ga_fcv[data2->han.slot].write );

/* Locate the Structure Record Vector entry which contains the ID of the    */
/* output component record and unpack this Record ID.                       */
   if ( _ok( hds_gl_status ) )
   {
      off = data2->offset * SZSRV;
      rec_locate_data( &data2->han, SZSRV, off, 'U', &srv );
      dat1_unpack_srv( srv, &rid );

/* If the component Record ID is null, then create a new record.            */
      if ( _ok( hds_gl_status ) )
      {
         if ( ( rid.bloc == 0 ) && ( rid.chip == 0 ) )
         {
            rcl.class = DAT__COMPONENT;
            rcl.zero = 0;
            rcl.slen = DAT__SZNCOMP;
            rcl.dlen = SZCRV * hds_gl_ncomp;
            hds_gl_ncomp = hds_gl_ncomp0;
            rec_create_record( &data2->han, &rcl, &han );

/* Obtain the new Record ID and pack it into the Structure Record Vector.   */
/* Note there are not yet any components.                                   */
            rec_get_rid( &han, &rid );
            dat1_pack_srv( &rid, srv );
            ncomp = 0;
         }

/* Otherwise, obtain a handle for the component record, get the Record      */
/* Control Label and read the component count.                              */
         else
         {
            rec_get_handle( &rid, &data2->han, &han );
            rec_get_rcl( &han, &rcl );
            dat1_get_ncomp( &han, &ncomp );
         }
      }

/* Release the output Structure Record Vector entry.                        */
      rec_release_data( &data2->han, SZSRV, off, 'U', &srv );

/* If necessary, expand the Component Record Vector to accommodate the new  */
/* component (in fact, allow for a total of hds_gl_ncomp0 extra components  */
/* to avoid frequent re-extension of the record).                           */
      if ( _ok( hds_gl_status ) )
      {
         szcrv = SZCRV;
         if ( ncomp * szcrv == rcl.dlen )
         {
            rec_extend_record( &han, szcrv * hds_gl_ncomp0 );
         }
      }
   }

/* If the structure currently has components, then locate the region of the */
/* Component Record Vector which contains existing component entries.       */
   if ( _ok( hds_gl_status ) )
   {
      if ( ncomp > 0 )
      {
         szcrv = SZCRV;
         rec_locate_data( &han, ncomp * szcrv, 0, 'R', &crv2 );

/* Locate the name of each existing component in turn and check to see if   */
/* it matches the new component name.                                       */
         rid = rec_gl_ridzero;
         for ( i = 0; i < ncomp; i++ )
         {
            dat1_locate_name( crv2, i, &name1 );
            if ( _ok( hds_gl_status ) )
            {
               if ( !memcmp( (void *) nambuf, (void *) name1, DAT__SZNAM ) )
               {
                  dat1_unpack_crv( crv2, i, &rid );
                  break;
               }
            }
         }

/* Release the Component Record Vector.                                     */
         rec_release_data(&han, ncomp * szcrv, 0, 'R', &crv2 );

/* Report an error if a component of the same name already exists.          */
         if ( _ok( hds_gl_status ) )
         {
            if ( ( rid.bloc != 0 ) || ( rid.chip != 0 ) )
            {
               hds_gl_status = DAT__COMEX;
               ems_setc_c( "NAME", (char *) name.body, name.length );
               ems_rep_c( "DAT_COPY_5",
                          "A component called \'^NAME\' already exists in \
the output structure (possible programming error).",
                          &hds_gl_status );
            }
         }
      }
   }

/* Re-locate the Component Record Vector and store the name of the new      */
/* component in it.                                                         */
   if ( _ok( hds_gl_status ) )
   {
      szcrv = SZCRV;
      rec_locate_data( &han, szcrv, ncomp * szcrv, 'W', &crv2 );
      dat1_locate_name( crv2, 0, &name1 );
      if ( _ok( hds_gl_status ) )
      {
         memcpy( (void *) name1, (void *) nambuf, DAT__SZNAM );
      }

/* Copy the object and release the Component Record Vector.                 */
      dau_copy_object( 1, &data1->han, crv1, &han, crv2 );
      rec_release_data( &han, szcrv, ncomp * szcrv, 'W', &crv2 );

/* Increment the component count for the output structure.                  */
      ++ncomp;
      dat1_put_ncomp( &han, ncomp );
   }

/* Restore the value of the global mapping flag - after ensuring that the   */
/* output FCB is flushed if mapping has ben altered.                        */
   if( save_map != hds_gl_map )
      fflush( rec_ga_fcv[data2->han.slot].write );
   hds_gl_map = save_map;

/* If an error occurred, then report contextual information.                */
   if ( !_ok( hds_gl_status ) )
   {
      ems_rep_c( "DAT_COPY_ERR",
                 "DAT_COPY: Error copying an HDS object to a new structure \
component.",
                 &hds_gl_status );
   }

/* Return the current global status value.                                  */
   *status = hds_gl_status;
   return *status;
}
