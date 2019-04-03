#include "star/util.h"
#include "ndf1_types.h"
#include "ndf1.h"
#include "mers.h"
#include "sae_par.h"

#if HAVE_CONFIG_H
#include "config.h"
#endif

#if HAVE_UNAME
#include <stdlib.h>       /* Standard library */
#include <string.h>       /* String handling library */
#include <sys/utsname.h>
#endif

void ndf1Uname( NdfUNAME *info, int *status ){
/*
*+
*  Name:
*     ndf1Uname

*  Purpose:
*     Gets information about the host computer system

*  Synopsis:
*     ndf1Uname( NdfUNAME *info, int *status )

*  Description:
*     This function inquires about the operating system, the name
*     of the computer and the type of the hardware. If an error is
*     detected then "*status" is set to SAI__ERROR and an error is
*     reported, although this should not happen. This function
*     mimics routine PSX_UNAME (see SUN/121).

*  Parameters:
*     info
*        Address of a structure to receive the returned information.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on psx_uname.f by PMA et al.
*        Remove VMS support.

*-
*/

/*--------------  POSIX (with uname) version  ---------------------*/
#if HAVE_UNAME

/* Local Variables: */
   int pstat;                 /* Status returned by uname */
   struct utsname temp_space; /* Temporary space to store results */
   struct utsname *name = &temp_space;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Get the information that uname provides. */
   pstat = uname( name );
   if( pstat != -1 ){

/* Export the information to the arguments of the subroutine. */
      star_strlcpy( info->sysname, name->sysname, sizeof( info->sysname ) );
      star_strlcpy( info->nodename, name->nodename, sizeof( info->nodename ) );
      star_strlcpy( info->release, name->release, sizeof( info->release ) );
      star_strlcpy( info->version, name->version, sizeof( info->version ) );
      star_strlcpy( info->machine, name->sysname, sizeof( info->sysname ) );

/* Set the status to a general error condition as POSIX does not specify
   that uname should set errno and report an error. */
   } else {
      info->sysname[ 0 ] = 0;
      info->nodename[ 0 ] = 0;
      info->release[ 0 ] = 0;
      info->version[ 0 ] = 0;
      info->machine[ 0 ] = 0;
      *status = SAI__ERROR;
      errRep( " ", "Error in call to C run time library function uname",
              status );
   }

/*--------------  POSIX (no uname) version  ---------------------*/
#else

/* System doesn't have a uname function. Not much we can do. */
/* If this is Windows (probably MinGW) we could try harder and look at */
/* GetSystemInfo, GetVersion etc. if the need arises. */
   if( *status != SAI__OK ) return;

   info->sysname[ 0 ] = 0;
   info->nodename[ 0 ] = 0;
   info->release[ 0 ] = 0;
   info->version[ 0 ] = 0;
   info->machine[ 0 ] = 0;
   *status = SAI__ERROR;
   errRep( " ", "C run time library function uname not available on "
           "this platform.", status );
#endif
}
