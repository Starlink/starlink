/* Function:  psx_uname( sysname, nodename, release, version, machine, status )
*+
*  Name:
*     PSX_UNAME

*  Purpose:
*     Gets information about the host computer system

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE, STATUS )

*  Description:
*     The routine inquires about the operating system, the name of the computer
*     and the type of the hardware.
*     If an error is detected then STATUS is set to SAI__ERROR and an error is
*     reported, although this should not happen.

*  Arguments:
*     SYSNAME = CHARACTER * ( * ) (Returned)
*        Name of the operating system.
*     NODENAME = CHARACTER * ( * ) (Returned)
*        Node name of the computer.
*     RELEASE = CHARACTER * ( * ) (Returned)
*        Version of the operating system.
*     VERSION = CHARACTER * ( * ) (Returned)
*        Sub-version of the operating system.
*     MACHINE = CHARACTER * ( * ) (Returned)
*        Name of the hardware of the computer.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE, STATUS )
*        When run on a SUN workstation, this will return values
*        something like
*           SYSNAME  = SunOS
*           NODENAME = rlssp1
*           RELEASE  = 4.1.1
*           VERSION  = 1
*           MACHINE  = sun4c
*
*     CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE,STATUS )
*        When run on a DECstation, this will return values something like
*           SYSNAME  = ULTRIX
*           NODENAME = rlsux1
*           RELEASE  = 4.0
*           VERSION  = 0
*           MACHINE  = RISC

*  External Routines Used:
*     cnf: cnfCopyf, cnfExprt

*  References:
*     -  POSIX standard (1988), section 4.4.1

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

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
*     PMA: Peter Allan (Starlink, RAL)
*     AJC: Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     21-JAN-1991 (PMA):
*        Original version.
*     15-APR-1991 (PMA):
*        Changed calls to ems to calls to psx1.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*      2-JUL-1992 (PMA):
*        Changed if defined(VMS) to if defined(vms)
*     23-JUN-2000 (AJC):
*        Tidy refs to CNF routines
*        Remove refs to VMS in prologue
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
----------------------------------------------------------------------------
*/

#include <config.h>

/* Global Constants:							    */

#if defined(vms)
#include <ssdef.h>		 /* VMS system services			    */
#include <syidef.h>		 /* VMS codes for system services	    */
#elif HAVE_UNAME
#include <stdlib.h>		 /* Standard library			    */
#include <string.h>		 /* String handling library		    */
#include <sys/utsname.h>
#endif

#include "f77.h"		 /* C - Fortran interface		    */
#include "psx1.h"		 /* Internal PSX routines		    */
#include "sae_par.h"		 /* ADAM constants			    */


F77_SUBROUTINE(psx_uname)( CHARACTER(sysname),
                           CHARACTER(nodename),
                           CHARACTER(release),
                           CHARACTER(version),
                           CHARACTER(machine),
                           INTEGER(status)
                           TRAIL(sysname)
                           TRAIL(nodename)
                           TRAIL(release)
                           TRAIL(version)
                           TRAIL(machine)
                         )
{

/* Pointers to Arguments:						    */

   GENPTR_CHARACTER(sysname)
   GENPTR_CHARACTER(nodename)
   GENPTR_CHARACTER(release)
   GENPTR_CHARACTER(version)
   GENPTR_CHARACTER(machine)
   GENPTR_INTEGER(status)


#if !defined(vms) && HAVE_UNAME
/*-------------------   Normal executable code   ---------------------------*/

/* Structure Declarations:						    */


/* Local Variables:							    */

   int pstat;			 /* Status returned by uname		    */
   struct utsname temp_space;	 /* Temporary space to store results	    */
   struct utsname *name = &temp_space;
				 /* Pointer to temporary space		    */

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Get the information that uname provides.				    */

   pstat = uname( name );

   if( pstat != -1 )
   {

/* Export the information to the arguments of the subroutine.		    */

      cnfExprt( name->sysname, sysname, sysname_length );
      cnfExprt( name->nodename, nodename, nodename_length );
      cnfExprt( name->release, release, release_length );
      cnfExprt( name->version, version, version_length );
      cnfExprt( name->machine, machine, machine_length );
   }
   else

/* Set the status to a general error condition as POSIX does not specify    */
/* that uname should set errno and report an error.			    */

   {
      cnfExprt( " ", sysname, sysname_length );
      cnfExprt( " ", nodename, nodename_length );
      cnfExprt( " ", release, release_length );
      cnfExprt( " ", version, version_length );
      cnfExprt( " ", machine, machine_length );
      *status = SAI__ERROR;
      psx1_rep_c( "PSX_UNAME_ERR",
         "Error in call to C run time library function uname", status );
   }

#elif defined(vms)
/*-------------------   Executable code for VMS  ---------------------------*/

/* VMS does not yet have the uname function, so use a call to sys$getsyiw   */
/* instead.								    */

/* Local Variables:							    */

   int vstat;			 /* Return value of a function		    */
   char  temp_sysname[4];
   short temp_sysname_len;
   char  temp_nodename[15];
   short temp_nodename_len;
   char  temp_release[4];
   short temp_release_len;
   char  temp_version[8];
   short temp_version_len;
   char  temp_machine[31];
   short temp_machine_len;

/* Structures:								    */

   typedef struct {		       /* Define an item list		    */
      short  buffer_length;
      short  item_code;
      int    buffer_address;
      int    return_length_address;
      } item;

   item itmlst[6];		       /* Item list for passing to getsyiw  */

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Set up the item list.						    */

   itmlst[0].buffer_length         = 4;
   itmlst[0].item_code             = SYI$_NODE_SWTYPE;
   itmlst[0].buffer_address        = &temp_sysname;
   itmlst[0].return_length_address = &temp_sysname_len;
   itmlst[1].buffer_length         = 15;
   itmlst[1].item_code             = SYI$_NODENAME;
   itmlst[1].buffer_address        = &temp_nodename;
   itmlst[1].return_length_address = &temp_nodename_len;
   itmlst[2].buffer_length         = 4;
   itmlst[2].item_code             = SYI$_NODE_SWVERS;
   itmlst[2].buffer_address        = &temp_release;
   itmlst[2].return_length_address = &temp_release_len;
   itmlst[3].buffer_length         = 8;
   itmlst[3].item_code             = SYI$_VERSION;
   itmlst[3].buffer_address        = &temp_version;
   itmlst[3].return_length_address = &temp_version_len;
   itmlst[4].buffer_length         = 31;
   itmlst[4].item_code             = SYI$_HW_NAME;
   itmlst[4].buffer_address        = &temp_machine;
   itmlst[4].return_length_address = &temp_machine_len;
   itmlst[5].buffer_length         = 0;
   itmlst[5].item_code             = 0;

/* Get the information using GETSYIW.					    */

   vstat = sys$getsyiw( 0, 0, 0, itmlst, 0, 0, 0 );

   if( vstat == SS$_NORMAL )

/* Copy the information to the arguments of the subroutine.		    */

   {
      cnfCopyf( temp_sysname, (int)temp_sysname_len, sysname, sysname_length );
      cnfCopyf( temp_nodename, (int)temp_nodename_len, nodename, nodename_length );
      cnfCopyf( temp_release, (int)temp_release_len, release, release_length );
      cnfCopyf( temp_version, (int)temp_version_len, version, version_length );
      cnfCopyf( temp_machine, (int)temp_machine_len, machine, machine_length );
   }
   else

/* Set the status to a general error condition and report an error.	    */

   {
      cnfExprt( " ", sysname, sysname_length );
      cnfExprt( " ", nodename, nodename_length );
      cnfExprt( " ", release, release_length );
      cnfExprt( " ", version, version_length );
      cnfExprt( " ", machine, machine_length );
      *status = SAI__ERROR;
      psx1_rep_c( "PSX_UNAME_ERR",
         "Error in call to C run time library function uname", status );
   }

#else

/* Not VMS and system doesn't have a uname function. Not much we can do. */
/* If this is Windows (probably MinGW) we could try harder and look at  */
/* GetSystemInfo, GetVersion etc. if the need arises. */

  if( *status != SAI__OK ) return;

   cnfExprt( " ", sysname, sysname_length );
   cnfExprt( " ", nodename, nodename_length );
   cnfExprt( " ", release, release_length );
   cnfExprt( " ", version, version_length );
   cnfExprt( " ", machine, machine_length );
   *status = SAI__ERROR;
   psx1_rep_c( "PSX_UNAME_ERR",
       "PSX_UNAME not supported on this platform", status );

#endif
}
