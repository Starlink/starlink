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

int
hdsGtune(const char *param_str,
         int *value,
         int *status)
{
/*
*+
*  Name:
*     HDS_GTUNE

*  Purpose:
*     Obtain tuning parameter value.

*  Language:
*     ANSI C

*  Invocation:
*     CALL HDS_GTUNE( PARAM, VALUE, STATUS )

*  Description:
*     The routine returns the current value of an HDS tuning parameter
*     (normally this will be its default value, or the value last
*     specified using the HDS_TUNE routine).

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Name of the tuning parameter whose value is required (case
*        insensitive).
*     VALUE = INTEGER (Returned)
*        Current value of the parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Tuning parameter names may be abbreviated to 4 characters.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council
*     Copyright (C) 2005 Particle Physics and Astronomy Research Council

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     BKM:  B.K. McIlwrath    (STARLINK, RAL)
*     PWD:  P.W. Draper       (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1991 (RFWS):
*        Original version.
*     25-FEB-1992 (RFWS):
*        Added auto-initialisation of HDS.
*     30-NOV-1992 (RFWS):
*        Added support for the "SHELL" tuning parameter.
*     13-DEC-2001 (BKM):
*        Convert to a C function with FORTRAN wrapper.
*     08-NOV-2005 (PWD):
*        Incorporate 64 bit changes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local variables:                                                         */
   char name[ DAT__SZNAM + 1 ];      /* Validated tuning parameter name     */
   struct DSC param;                 /* Parameter string descriptor         */

/*.                                                                         */

/* Check the inherited global status.                                       */
   if ( !_ok( *status ) ) return *status;
   hds_gl_status = *status;

/* Import the tuning parameter name and perform preliminary validation.     */
   _strcsimp( &param, param_str );
   dau_check_name( &param, name );

/* Ensure that HDS has been initialised.                                    */
   if ( !hds_gl_active ) dat1_init( );

/* Return the appropriate value. Note that where "one-shot" values are      */
/* provided, we return these, rather than the default tuning setting (e.g.  */
/* hds_gl_inalq, rather than hds_gl_inalq0).                                */
   if( _ok( hds_gl_status ) )
   {
      if ( strncmp( name, "INAL", 4 ) == 0 )
      {
         *value = hds_gl_inalq;
      }
      else if ( strncmp( name, "MAP", 3 ) == 0 )
      {
         *value = hds_gl_map;
      }
      else if ( strncmp( name, "MAXW", 4 ) == 0 )
      {
         *value = hds_gl_maxwpl;
      }
      else if ( strncmp( name, "NBLO", 4 ) == 0 )
      {
         *value = hds_gl_nblocks;
      }
      else if ( strncmp( name, "NCOM", 4 ) == 0 )
      {
         *value = hds_gl_ncomp;
      }
      else if ( strncmp( name, "SHEL", 4 ) == 0 )
      {
         *value = hds_gl_shell;
      }
      else if ( strncmp( name, "SYSL", 4 ) == 0 )
      {
         *value = hds_gl_syslck;
      }
      else if ( strncmp( name, "WAIT", 4 ) == 0 )
      {
         *value = hds_gl_wait;
      }
      else if ( strncmp( name, "64BIT", 5 ) == 0 )
      {
          *value = hds_gl_c64bit;
      }

/* If the tuning parameter name was not recognised, then report an error.   */
      else
      {
         hds_gl_status = DAT__NAMIN;
         emsSetnc( "PARAM", (char *) param.body, param.length );
         emsRep( "HDS_GTUNE_1",
                    "Unknown tuning parameter name \'^PARAM\' specified \
(possible programming error).",
                    &hds_gl_status );
      }
   }

/* If an error occurred, then report contextual information.                */
   if ( !_ok( hds_gl_status ) )
   {
      emsRep( "HDS_GTUNE_ERR",
                 "HDS_GTUNE: Error obtaining the value of an HDS tuning \
parameter.",
                 &hds_gl_status );
   }

/* Return the current global status value.                                  */
   *status = hds_gl_status;
   return *status;
}
