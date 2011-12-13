#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <string.h>
#include "ems.h"                 /* EMS error reporting routines            */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Private rec_ definitions                */
#include "str.h"                 /* Character string import/export macros   */
#include "dat1.h"                /* Internal dat_ definitions               */
#include "dat_err.h"             /* DAT__ error code definitions            */
#include "hds.h"

int
hdsTune(const char *param_str,
        int  value,
        int  *status)
{
/*
*+
*  Name:
*     HDS_TUNE

*  Purpose:
*     Set tuning parameter value.

*  Language:
*     ANSI C

*  Description:
*     The routine sets a new value for an HDS tuning parameter.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the tuning parameter whose value is to be set (case
*        insensitive).
*     VALUE = INTEGER (Given)
*        New value for the parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Tuning parameter names may be abbreviated, to no less than four
*     characters.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26-FEB-1992 (RFWS):
*        Re-written.
*     10-SEP-1992 (RFWS):
*        Added special values -1, -2 and -3 for the MAP tuning
*        parameter.
*     30-NOV-1992 (RFWS):
*        Added support for the SHELL tuning parameter.
*     27-NOV-2000 (BKM):
*        Changing file mapping mode on Linux found to be causing data
*        corruption due to file system caching. Make this routine flush
*        all buffers on a change.
*     06-DEC-2001 (BKM)
*        Convert to a C function with FORTRAN wrapper.
*     06-MAY-2004 (BKM)
*        Add 64BIT tuning parameter.
*     04-DEC-2005 (TIMJ):
*        No reason for input arg to be a pointer.
*     {enter_further_changes_here}

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

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local variables:                                                         */
   char name[ DAT__SZNAM + 1 ];    /* Upper case version of parameter name  */
   struct DSC param;               /* Parameter name descriptor             */
   int i;                          /* Loop counter                          */

/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( *status ) ) return *status;
   hds_gl_status = *status;

/* Import the tuning parameter name.                                        */
   _strcsimp( &param, param_str );

/* Convert the name to upper case, and perform initial validation.          */
   dau_check_name( &param, name );

/* Ensure that the default tuning parameter values have been initialised.   */
   dat1_intune( &hds_gl_status );

/* Test for each valid tuning parameter name in turn and assign the new     */
/* value, applying appropriate constraints (note that these constraints     */
/* should match those imposed by dat1_intune). Where a parameter has a      */
/* "one-shot" value, then assign the new value to it (i.e. modify           */
/* hds_gl_inalq, rather than hds_gl_inalq0).                                */
   if ( _ok( hds_gl_status ) )
   {

/* Initial file allocation quantity: constrain this to be at least 2        */
/* blocks.                                                                  */
      if ( strncmp( name, "INAL", 4 ) == 0 )
      {
         hds_gl_inalq = value;
         if ( hds_gl_inalq < 2 ) hds_gl_inalq = 2;
      }

/* Use file mapping if available?                                           */
      else if ( strncmp( name, "MAP", 3 ) == 0 )
      {
/* Flush all open files if mapping is changed                               */
          if ( value != hds_gl_map )
              for( i=0; i<rec_gl_endslot; i++ )
                  if( rec_ga_fcv[i].open )
                      if ( rec_ga_fcv[i].write != REC__NOIOCHAN )
                          fflush(rec_ga_fcv[i].write);

         switch ( value )
         {

/* If -1 was specified, set file mapping if it is the best mode for         */
/* sequential file access.                                                  */
            case -1:
               hds_gl_map = HDS__MAPSEQ;
               break;

/* If -2 was specified, set file mapping if it is the best mode for sparse  */
/* file access.                                                             */
            case -2:
               hds_gl_map = HDS__MAPSPARSE;
                  break;

/* If -3 was specified, set file mapping if it is the best mode for         */
/* minimising memory usage.                                                 */
            case -3:
               hds_gl_map = HDS__MAPMEM;
               break;

/* Convert all other non-zero values to 1.                                  */
            default:
               hds_gl_map = ( value != 0 );
               break;
         }

/* Turn mapping off if it is not implemented.                               */
         hds_gl_map = ( hds_gl_map && HDS__CANMAP );
      }

/* Maximum size of the "working page list": constrain this to be at least   */
/* the default value (HDS__MAXWPL).                                         */
      else if ( strncmp( name, "MAXW", 4 ) == 0 )
      {
         hds_gl_maxwpl = value;
         if ( hds_gl_maxwpl < HDS__MAXWPL ) hds_gl_maxwpl = HDS__MAXWPL;
      }

/* Size of the internal "transfer buffer": constrain this to be at least    */
/* the default value (HDS__NBLOCKS).                                        */
      else if ( strncmp( name, "NBLO", 4 ) == 0 )
      {
         hds_gl_nblocks = value;
         if ( hds_gl_nblocks < HDS__NBLOCKS ) hds_gl_nblocks = HDS__NBLOCKS;
      }

/* Optimum number of structure components: constrain this to be at least    */
/* one.                                                                     */
      else if ( strncmp( name, "NCOM", 4 ) == 0 )
      {
         hds_gl_ncomp = value;
         if ( hds_gl_ncomp < 1 ) hds_gl_ncomp = 1;
      }

/* Shell used for file name expansion (UNIX & POSIX systems only): if the   */
/* supplied value lies outside the supported range, then use the default    */
/* shell.                                                                   */
      else if ( strncmp( name, "SHEL", 4 ) == 0 )
      {
         hds_gl_shell = value;
         if ( ( hds_gl_shell < HDS__NOSHELL ) ||
              ( hds_gl_shell > HDS__MXSHELL ) )
         {
            hds_gl_shell = HDS__SHELL;
         }
      }

/* System wide lock flag: zero implies no system wide locking, everything   */
/* else implies system wide locking.                                        */
      else if ( strncmp( name, "SYSL", 4 ) == 0 )
      {
         hds_gl_syslck = value;
         hds_gl_syslck = ( hds_gl_syslck != 0 );
      }

/* Wait for locked files? Zero implies don't wait, everything else implies  */
/* wait.                                                                    */
      else if ( strncmp( name, "WAIT", 4 ) == 0 )
      {
         hds_gl_wait = value;
         hds_gl_wait = ( hds_gl_wait != 0 );
      }

/* Create 64-bit (HDS V4) files? Zero implies no (create HDS3 compatible    */
/* files. anything else implies yes.                                        */
      else if ( strncmp( name, "64BIT", 5 ) == 0 )
      {
         hds_gl_c64bit = value;
         hds_gl_c64bit = ( hds_gl_c64bit != 0 );
      }

/* If the tuning parameter name was not recognised, then report an error.   */
      else
      {
         hds_gl_status = DAT__NAMIN;
         emsSetnc( "PARAM", (char *) param.body, param.length );
         emsRep( "HDS_TUNE_1",
                    "Unknown tuning parameter name \'^PARAM\' specified \
(possible programming error).",
                    &hds_gl_status );
      }
   }

/* If an error occurred, then report contextual information.                */
   if ( !_ok( hds_gl_status ) )
   {
      emsRep( "HDS_TUNE_ERR",
                 "HDS_TUNE: Error setting a new value for an HDS tuning \
parameter.",
                 &hds_gl_status );
   }

/* Return the current global status value.                                  */
   *status = hds_gl_status;
   return *status;
}
