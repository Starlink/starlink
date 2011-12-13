#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <ctype.h>

#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS public constants                    */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */
#include "hds.h"

int
hdsWild(const char *fspec,
        const char *mode,
            int *iwld,
            HDSLoc **locator,
            int *status)
{
/*
*+
*  Name:
*     HDS_WILD

*  Purpose:
*     Perform a wild-card search for HDS container files.

*  Language:
*     ANSI C

*  Invocation:
*     hdsWild( fspec, mode, iwild, loc, status )

*  Description:
*     The routine searches for HDS container files whose names match a
*     given wild-card file specification, and which are accessible
*     using a specified mode of access. It is normally called
*     repeatedly, returning a locator for the top-level object in a new
*     container file on each occasion, and a null locator value
*     (DAT__NOLOC) when no more container files remain to be located.
*
*     In normal use, the IWLD argument should be set to the value
*     DAT__NOWLD before the first call to HDS_WILD. The value returned
*     through this argument subsequently identifies the search context,
*     which is retained between calls. In this way, several wild-card
*     searches may be performed concurrently if required.
*
*     A call to HDS_EWILD should be made to annul the search context
*     identifier when the search is complete. This will release any
*     resources used.

*  Arguments:
*     fspec = char * (Given)
*        The wild-card file specification identifying the container
*        files required (a default file type extension of ".sdf" is
*        assumed, if not specified). The syntax of this specification
*        depends on the host operating system.
*     mode = char * (Given)
*        The mode of access required to the container files: 'READ',
*        'UPDATE' or 'WRITE' (case insensitive).
*     iwld = int * (Given and Returned)
*        If a value of DAT__NOWLD is supplied on input, then a new
*        wild-card search context will be started, a new value for IWLD
*        will be returned, and the first HDS container file matching
*        the file specification given in FSPEC will be located. If an
*        IWLD value saved from a previous invocation of HDS_WILD is
*        supplied, then the previous search context will be used and
*        the next container file appropriate to that context will be
*        located. In this case, the value of FSPEC is not used.
*     locator = HDSLoc ** (Returned)
*        A primary locator to the top-level object in the next
*        container file to satisfy the file specification given in
*        FSPEC.  From C a NULL pointer will be returned (without error)
*        if no further container files remain to be located.
*     status = int * (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine does not return locators for files which are not
*     valid HDS container files or which are not accessible using the
*     specified access mode.
*     -  The routine takes a "snapshot" of the file system when it is
*     first called (with IWLD set to DAT__NOWLD) and subsequently
*     returns locators for each of the HDS container files found, one
*     at a time. This strategy avoids possible run-away conditions if
*     (say) output files created by an application were later to be
*     selected by a wild-card search used to identify further input
*     files.
*     -  An error may result if any file matched by an initial call to
*     HDS_WILD (with IWLD set to DAT__NOWLD) cannot be accessed when a
*     subsequent call requires that a locator be returned for it. This
*     might happen, for instance, if the file has been deleted in the
*     intervening time. If the resulting error condition is annulled,
*     the offending file may be skipped and subsequent calls to
*     HDS_WILD will continue to locate any remaining files.
*     -  An error will result and a STATUS value of DAT__FILNF will be
*     returned if no HDS container files can be found which match the
*     wild-card specification supplied on an initial call to HDS_WILD.
*     -  A value of DAT__NOLOC will be returned for the LOC argument if
*     this routine is called with STATUS set, or if it should fail for
*     any reason. In addition, the value of IWLD will be returned
*     unchanged if the routine is called with STATUS set or if failure
*     should occur during an initial call (i.e. when IWLD is set to
*     DAT__NOWLD on entry).
*     -  The DAT__NOLOC and DAT__NOWLD constants are defined in the
*     include file DAT_PAR. The DAT__FILNF error code is defined in the
*     include file DAT_ERR.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1995, 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council.
*     Copyright (C) 2007 Science and Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     TIMJ: Tim Jenness       (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1992 (RFWS):
*        Original version.
*     24-NOV-1992 (RFWS):
*        Improved error message.
*     22-JUN-1995 (RFWS):
*        Ensure that HDS is initialised before attempting to open any
*        container files.
*     19-MAR-2001 (BKM);
*        Convert to C interface and remove tabs.
*     16-NOV-2005 (TIMJ):
*        Use HDSLoc**
*     14-NOV-2007 (TIMJ):
*        Only free the output locator on error if the locator was
*        allocated by this routine.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


/* Local Variables:                                                         */
      INT fname_len;             /* Number of characters in file name       */
      INT i;                     /* Loop counter for dimensions             */
      INT refcnt;                /* Container file reference count (junk)   */
      INT start;                 /* Array index of first non-blank char     */
      char *fname;               /* Pointer to name of file to open         */
      char *name;                /* Pointer to object name                  */
      char mode_c;               /* Validated access mode character         */
      int again;                 /* Look for another file name?             */
      int alldone;               /* All filenames processed?                */
      int first;                 /* Initial call to HDS_WILD?               */
      int outlocok = 0;          /* Was the output locator allocated ok?    */
      struct HAN han;            /* Handle for top-level object             */
      struct LCP *lcp;           /* Pointer to Locator Control Packet       */
      struct ODL odl;            /* Object Descriptor Label                 */
      struct RCL rcl;            /* Record Control Label for top record     */
      struct RID rid;            /* Record ID                               */
      struct WLD *context;       /* Pointer to wild-card context structure  */
      unsigned char *crv;        /* Pointer to Component Record Vector      */

/*.                                                                         */

/* Check the inherited global status.                                       */
      if ( !_ok( *status) ) return *status;
      hds_gl_status = (INT) *status;

/* Validate the access mode.                                                */
      dat1_check_mode( mode, strlen(mode), &mode_c, &hds_gl_status );

/* Note if this is an initial call to HDS_WILD (i.e. whether IWLD has the   */
/* null value) and make a copy of the wild-card context identifier.         */
      first = ( *iwld == DAT__NOWLD );
      context = (struct WLD *) *iwld;

/* Ensure that HDS has been initialised.                                    */
      if ( !hds_gl_active )
      {
         dat1_init();
      }

/* Loop until a valid container file is obtained or there are no files left */
/* to process or an error occurs.                                           */
      again = 1;
      while( again && _ok( hds_gl_status ) )
      {
         again = 0;

/* Obtain the next file name matching the wild-card specification.          */
         rec_wild_file( fspec, strlen(fspec), &context, &alldone, &fname,
                        &fname_len );

/* Update the returned context pointer.                                     */
         if ( _ok( hds_gl_status ) )
         {
            *iwld = (F77_INTEGER_TYPE) context;

/* If there were no more file names to process, then nullify the output     */
/* locator.                                                                 */
            if ( alldone )
            {
 	       dat1_free_hdsloc( locator );

/* If this is an initial call of HDS_WILD, then report an error, since      */
/* there were no accessible files which matched the specification (omit any */
/* white space surrounding the file specification).                         */
               if ( first )
               {
                  hds_gl_status = DAT__FILNF;
                  for ( start = 0; start < ( strlen(fspec) - 1 ); start++ )
                  {
                     if ( !isspace( fspec[ start ] ) ) break;
                  }
                  emsSetnc( "FSPEC", fspec + start, strlen(fspec) - start );
                  emsSetnc( "ACCESS", ( mode_c == 'R' ? "read" :
                                        ( mode_c == 'U' ? "update" :
                                                        "write" ) ),
                              EMS__SZTOK );
                  emsRep( "HDS_WILD_1",
                             "No ^ACCESS-accessible HDS files found matching \
the specification \'^FSPEC\'.",
                             &hds_gl_status );

/* Annul the allocated context structure and reset the IWLD argument.       */
                  rec_end_wild( &context );
                  *iwld = DAT__NOWLD;
               }
            }

/* Otherwise, mark the error stack and attempt to open the file.            */
            else
            {
               emsMark( );
               rec_attach_file( 0, fname, fname_len, 'O', mode_c, &rcl, &han );

/* If the file could not be opened for acceptable reasons, then annul the   */
/* error, release the error stack and note that we must try again.          */
               if ( ( hds_gl_status == DAT__FILIN ) || /* Not an HDS file   */
                    ( hds_gl_status == DAT__FILPR ) )  /* File protected    */
               {
                  emsAnnul( &hds_gl_status );
                  emsRlse( );
                  again = 1;
               }

/* Otherwise, release the error stack, allocate a Locator Control Packet    */
/* and initialise the returned locator.                                     */
               else
               {
                  emsRlse( );
                  dat1_alloc_lcp(locator, &lcp );
                  if ( _ok( hds_gl_status ) )
                  {
                     outlocok = 1;

/* Locate the container record's dynamic domain (the Component Record       */
/* Vector), locate the top-level object name, and extract it for storage in */
/* the LCP.                                                                 */
                     rec_locate_data( &han, SZCRV, 0, 'R', &crv );
                     dat1_locate_name( crv, 0, &name );
                     if ( _ok( hds_gl_status ) )
                     {
                        (void) memcpy( (void *) lcp->data.name,
                                       (const void *) name,
                                       (size_t) DAT__SZNAM );
                     }

/* Obtain a handle for the top-level object and release the dynamic domain. */
                     dat1_unpack_crv( crv, 0, &rid );
                     rec_get_handle( &rid, &han, &lcp->data.han );
                     rec_release_data( &han, SZCRV, 0, 'R', &crv );

/* Save the parent Record ID.                                               */
                     rec_get_rid( &han, &lcp->data.parent );

/* Read the Object Descriptor Label from the object record's static domain  */
/* and determine the object attributes.                                     */
                     dat1_get_odl( &lcp->data.han, &odl );
                     if ( _ok( hds_gl_status ) )
                     {
                        (void) memcpy( (void *) lcp->data.type,
                                       (const void *) odl.type,
                                       (size_t) DAT__SZTYP );
                     }
                     dat1_unpack_type( lcp->data.type, &lcp->data.obj );

/* Save the shape information in the LCP and calculate the total size of    */
/* the object. (The Dimension Bounds Table is used to hold the 1st three    */
/* axis sizes).                                                             */
                     if ( _ok( hds_gl_status ) )
                     {
                        lcp->data.naxes = odl.naxes;
                        lcp->data.size = 1;
                        for ( i = 0; i < odl.naxes; i++ )
                        {
                           lcp->data.size *= odl.axis[ i ];
                           if ( i < DAT__MXSLICE )
                           {
                              lcp->data.bounds[ i ][ LOWER ] = 1;
                              lcp->data.bounds[ i ][ UPPER ] = odl.axis[ i ];
                           }
                        }

/* Fill the remaining Locator Control Packet fields, making this a primary  */
/* locator and incrementing the container file reference count.             */
                        lcp->data.mode = mode_c;
                        lcp->data.read = ( mode_c == 'R' );
                        lcp->data.struc =
                           ( lcp->data.obj.class == DAT__STRUCTURE );
                        lcp->primary = 1;
                        rec_refcnt( &han, 1, &refcnt, &hds_gl_status );
                     }

/* If there has been no error, then mark the LCP as valid.                  */
                     if ( _ok( hds_gl_status ) )
                     {
                        lcp->data.valid = 1;
                     }

/* Otherwise, defuse the allocated LCP.                                     */
                     else
                     {
                        dau_defuse_lcp( &lcp );
                     }
                  }
               }
            }
         }
      }

/* If an error has occurred, then nullify the output locator and make a     */
/* contextual error report.                                                 */
      if ( !_ok( hds_gl_status ) )
      {
         if (outlocok) dat1_free_hdsloc( locator );
         emsRep( "HDS_WILD_ERR",
                    "HDS_WILD: Error performing a wild-card search for HDS \
container files.", &hds_gl_status );
      }

/* Return the current global status value.                                  */
      *status = hds_gl_status;
      return *status;
   }
