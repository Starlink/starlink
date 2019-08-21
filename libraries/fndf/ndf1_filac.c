#define _POSIX_SOURCE 1		 /* Declare POSIX source		    */

#include <stdlib.h>		 /* Define malloc, free, etc.		    */
#include <errno.h>		 /* Define errno			    */
#include <ctype.h>		 /* Character functions			    */
#include <string.h>		 /* String functions			    */

#if defined( vms )		 /* VMS version include files:		    */
#include <stdio.h>		 /* Define access function		    */

#else				 /* POSIX version include files:	    */
#include <unistd.h>		 /* Define access function		    */
#endif

#include "sae_par.h"		 /* Standard SAE constants		    */
#include "ems.h"		 /* ems_ error reporting routines	    */
#include "f77.h"		 /* Fortran 77 <=> C interface macros	    */
#include "ndf1.h"		 /* Internal NDF definitions		    */

   F77_SUBROUTINE(ndf1_filac)( CHARACTER(FNAME),
                               CHARACTER(MODE),
			       LOGICAL(REPORT),
			       LOGICAL(OK),
			       INTEGER(STATUS)
			       TRAIL(FNAME)
			       TRAIL(MODE) )
   {
/*
*+
*  Name:
*     NDF1_FILAC

*  Purpose:
*     Check if a specified mode of access is available for a file.

*  Language:
*     ANSI C

*  Invocation:
*     CALL NDF1_FILAC( FNAME, MODE, REPORT, OK, STATUS )

*  Description:
*     The routine checks whether a specified mode of access is
*     available for a named file and returns a logical result. If
*     access is not available, the routine will also (optionally) set
*     STATUS and make an appropriate error report.

*  Arguments:
*     FNAME = CHARACTER * ( * ) (Given)
*        Name of the file.
*     MODE = CHARACTER * ( * ) (Given)
*        The required mode of access: 'READ', 'UPDATE' or 'WRITE' (case
*        insensitive.
*     REPORT = LOGICAL (Given)
*        Whether to set STATUS and make an error report if the requested
*        mode of access is not available.
*     OK = LOGICAL (Returned)
*        Whether the requested mode of access was available.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine is intended to be called from Fortran.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 2005 Particle Physics and Engineering Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-OCT-1993 (RFWS):
*        Original version.
*     5-NOV-1993 (RFWS):
*        Changed FILE argument to FNAME to prevent clash with C data
*        type used for accessing files.
*     27-DEC-2005 (TIMJ):
*        Fix compiler warning with uninitialized flags.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Arguments Given:							    */
      GENPTR_CHARACTER(FNAME)
      GENPTR_CHARACTER(MODE)
      GENPTR_LOGICAL(REPORT)

/* Arguments Returned:							    */
      GENPTR_LOGICAL(OK)

/* Status:								    */
      GENPTR_INTEGER(STATUS )

/* Local Constants:							    */
#if defined( vms )		 /* Define VMS file access mode flags:	    */
#define X_OK 1			 /* File is executable			    */
#define W_OK 2			 /* File is writable (implies deletable)    */
#define R_OK 4			 /* File is readable			    */
#endif

/* Local Variables:							    */
      char *fname;		 /* Pointer to file name string		    */
      char *mode;		 /* Pointer to access mode string	    */
      int acc;			 /* File accessible?			    */
      int flags = 0;		 /* Access mode flags			    */
      int i;			 /* Loop counter for characters		    */
      size_t size;		 /* Amount of space to allocate		    */

/*.									    */

/* Check inherited global status.					    */
      if ( *STATUS != SAI__OK ) return;

/* Allocate space to hold the file name and check for errors.		    */
      size = (size_t) FNAME_length + (size_t) 1;
      fname = (char *) malloc( size );
      if ( fname == NULL )
      {
         *STATUS = NDF__NOMEM;
	 emsSeti( "NBYTES", (int) size );
	 emsErrno( "MESSAGE", errno );
	 emsRep( "NDF1_FILAC_1",
	            "Unable to allocate a block of ^NBYTES bytes of memory - \
^MESSAGE",
		    STATUS );
      }

/* Copy the file name into the allocated space and append a null.	    */
      else
      {
         for ( i = 0; i < FNAME_length; i++ )
	 {
	    fname[ i ] = (char) FNAME[ i ];
	 }
	 fname[ i ] = '\0';

/* Allocate space to hold the access mode and check for errors.		    */
         size = (size_t) MODE_length + (size_t) 1;
         mode  = (char *) malloc( size );
         if ( mode == NULL )
         {
            *STATUS = NDF__NOMEM;
	    emsSeti( "NBYTES", (int) size );
	    emsErrno( "MESSAGE", errno );
	    emsRep( "NDF1_FILAC_2",
	               "Unable to allocate a block of ^NBYTES bytes of memory \
- ^MESSAGE", STATUS );
         }

/* Copy the access mode string into the allocated space, converting to	    */
/* upper case and omitting trailing blanks. Append a null.		    */
         else
         {
            for ( i = 0; i < MODE_length; i++ )
	    {
	       if ( (char) MODE[ i ] == ' ' ) break;
	       mode[ i ] = (char) toupper( (int) (char) MODE[ i ] );
	    }
	    mode[ i ] = '\0';

/* Compare the requested access mode with each recognised value in turn and */
/* set the access mode flags accordingly.				    */
	    if ( !strcmp( mode, "READ" ) )
	    {
	       flags = R_OK;
	    }
	    else if ( !strcmp( mode, "WRITE" ) )
	    {
	       flags = W_OK;
	    }
	    else if ( !strcmp( mode, "UPDATE" ) )
	    {
	       flags = R_OK | W_OK;
	    }

/* Report an error if the access mode is not recognised.		    */
	    else
	    {
	       *STATUS = NDF__FATIN;
	       emsSetnc( "MODE", mode, MODE_length );
	       emsRep( "NDF1_FILAC_3",
		          "Routine NDF1_FILAC called with an invalid file \
access mode of \'^MODE\' (internal programming error).", STATUS );
	    }

/* Enquire if the requested file access is available and set *OK	    */
/* accordingly.								    */
	    if ( *STATUS == SAI__OK )
	    {
	       acc = !access( fname, flags );
	       *OK = acc ? F77_TRUE : F77_FALSE;

/* The access function does not set errno on VMS, so do it explicitly.	    */
#if defined( vms )
               if ( !acc ) errno = EACCES;
#endif

/* If access is not available, set *STATUS and report an error if required. */
	       if ( !acc && F77_ISTRUE( *REPORT ) )
	       {
	          *STATUS = NDF__FILPR;
	          emsSetnc( "FNAME", fname, FNAME_length );
	          emsSetnc( "MODE", mode, MODE_length );
	          emsErrno( "MESSAGE", errno );
	          emsRep( "NDF1_FILAC_4",
			     "Unable to open the file \'^FNAME\' for ^MODE \
access - ^MESSAGE",
			     STATUS );
	       }
	    }

/* Free the space allocated for the access mode string.			    */
	    free( (void *) mode );
	 }

/* Free the space allocated for the file name.				    */
	 free( (void *) fname );
      }

/* If necessary, call the error tracing function. */
      if ( *STATUS != SAI__OK ) ndf1Trace( "ndf1_filac", STATUS );

/* Exit the routine.							    */
      return;
   }
