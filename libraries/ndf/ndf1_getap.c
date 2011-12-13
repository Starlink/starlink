#define _POSIX_SOURCE 1		 /* POSIX source			    */

#if defined( vms )		 /* VMS version include files:		    */
#include <descrip.h>		 /* Descriptor definitions		    */
#include <jpidef.h>		 /* lib$getjpi definitions		    */
#include <libdef.h>		 /* Run time library definitions	    */
#include <stddef.h>		 /* Define NULL				    */
#include <stsdef.h>		 /* System status codes			    */

#else				 /* UNIX version include files:		    */
#include <limits.h>		 /* System limits (for PATH_MAX)	    */
#include <stdio.h>		 /* For FILENAME_MAX			    */
#endif

#if !defined( FILENAME_MAX )	 /* Overcome gcc compiler problems on SUNs  */
#if defined( PATH_MAX )		 /* Use POSIX definition instead of ANSI C  */
#define FILENAME_MAX PATH_MAX
#else
#define FILENAME_MAX _POSIX_PATH_MAX
#endif
#endif

#include "sae_par.h"		 /* Standard SAE constants		    */
#include "ems.h"		 /* EMS_ error reporting routines	    */
#include "f77.h"		 /* Fortran 77 <=> C interface macros	    */
#include "ndf1.h"		 /* Internal NDF definitions		    */

   F77_SUBROUTINE(ndf1_getap)( CHARACTER(APPN),
			       INTEGER(STATUS)
			       TRAIL(APPN) )
   {
/*
*+
*  Name:
*     NDF1_GETAP

*  Purpose:
*     Get the name of the currently-executing application.

*  Language:
*     ANSI C

*  Invocation:
*     CALL NDF1_GETAP( APPN, STATUS )

*  Description:
*     The routine returns the name of the currently-running
*     application, left justified. The returned value will be truncated
*     without error if the variable supplied is too short.

*  Arguments:
*     APPN = CHARACTER * ( * ) (Returned)
*        Application name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This is the "standalone" version of this routine. It returns
*     the name of the currently executing file or command.
*     -  This routine is intended to be callable from Fortran.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     9-AUG-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Arguments Returned:							    */
      GENPTR_CHARACTER(APPN)

/* Status:								    */
      GENPTR_INTEGER(STATUS)

/* External References:							    */
#if defined( vms )		 /* VMS system calls:			    */
      extern unsigned int lib$getjpi /*	Get job/process information	    */
         ( int *item_code,
	   unsigned int *process_id,
	   struct dsc$descriptor *process_name,
	   void *resultant_value,
	   struct dsc$descriptor *resultant_string,
	   unsigned short int *resultant_length );
#endif

/* Local Variables:							    */
#if defined( vms )		 /* VMS version local variables:	    */
      int code;			 /* getjpi item code			    */
      static char *buf;		 /* Pointer to image name		    */
      static int first = 1;	 /* First invocation?			    */
      static int i1;		 /* Index of start of name field	    */
      static int i2;		 /* Index of end of name field		    */
      struct dsc$descriptor dsc; /* VMS descriptor			    */
      unsigned int systat;	 /* System status code			    */
      unsigned short int len;	 /* Length of executing image name	    */

#else				 /* UNIX version local variables:	    */
      DECLARE_CHARACTER(arg0,FILENAME_MAX); /* Buffer for command string    */
      DECLARE_INTEGER(iarg);	 /* Number of the argument required	    */
      DECLARE_INTEGER(larg);	 /* Length of argument			    */
      F77_CHARACTER_TYPE *buf;	 /* Pointer to result buffer		    */
      int i1;			 /* Index of start of name field	    */
      int i2;			 /* Index of end of name field		    */
#endif

      int i;			 /* Loop counter			    */

/*.									    */

/* Check the inherited global status.					    */
      if ( *STATUS != SAI__OK ) return;

/* VMS version:								    */
/* ===========								    */
#if defined( vms )

/* We only need to perform this on the first invocation.		    */
      if ( first )
      {

/* Initialise a dynamic character string descriptor to describe the	    */
/* returned value.							    */
         dsc.dsc$a_pointer = NULL;
         dsc.dsc$b_class = DSC$K_CLASS_D;
         dsc.dsc$b_dtype = DSC$K_DTYPE_T;
         dsc.dsc$w_length = (unsigned short int) 0;

/* Obtain the file name of the currently executing image.		    */
         code = JPI$_IMAGNAME;
         systat = lib$getjpi( &code, (unsigned int) 0,
                              (struct dsc$descriptor *) 0, (void *) 0,
			      &dsc, &len );

/* Check for and report any errors - also trap string truncation (normally  */
/* regarded as a success status).					    */
         if ( !( systat & STS$M_SUCCESS ) || ( systat == LIB$_STRTRU ) )
         {
            *STATUS = NDF__FATIN;
	    emsSyser( "MESSAGE", systat );
	    emsRep( "NDF1_GETAP_VMS",
	               "Error determining the file name of the currently \
executing VMS image - ^MESSAGE.", STATUS );
         }

/* If OK, extract a pointer to the returned string from the descriptor.	    */
         else
         {
            buf = (char *) dsc.dsc$a_pointer;

/* Search for the start of the file name field following the end of a	    */
/* directory specification.						    */
            for ( i1 = ( (int) len ) - 1; i1 >= 0; i1-- )
            {
	       if ( buf[ i1 ] == ']' ) break;
            }
            i1 = i1 + 1;

/* Search for the '.' which delimits the file type extension.		    */
            for ( i2 = i1; i2 < (int) len; i2++ )
            {
	       if ( buf[ i2 ] == '.' ) break;
            }
            i2 = i2 - 1;

/* Note that the first invocation completed successfully.		    */
	    first = 0;
         }
      }

/* UNIX version:							    */
/* ============								    */
#else

/* Get the value of the zero'th argument (the name of the command being	    */
/* executed).								    */
      iarg = (F77_INTEGER_TYPE) 0;
      F77_LOCK( F77_CALL(ndf1_gtarg)( INTEGER_ARG(&iarg), CHARACTER_ARG(arg0),
                            INTEGER_ARG(&larg), INTEGER_ARG(STATUS)
			    TRAIL_ARG(arg0) ); )

/* If the argument value is blank, then return "<unknown>" as the	    */
/* application name.							    */
      if ( (int) larg == 0 )
      {
         buf = "<unknown>";
	 i1 = 0;
	 i2 = 8;
      }

/* Otherwise, search the resulting (Fortran) string to find the last	    */
/* non-blank character.							    */
      else
      {
	 buf = arg0;
         for ( i2 = ( (int) larg ) - 1; i2 >= 0; i2-- )
         {
            if ( buf[ i2 ] != (F77_CHARACTER_TYPE) ' ' ) break;
         }

/* Search backwards from the end of the command to find the last '/' which  */
/* marks the end of the directory path, so as to select just the name	    */
/* field.								    */
         for ( i1 = i2; i1 >= 0; i1-- )
         {
            if( buf[ i1 ] == (F77_CHARACTER_TYPE) '/' ) break;
         }
         i1 = i1 + 1;
      }
#endif

/* If OK, copy the application name back to the caller, truncating if	    */
/* necessary.								    */
      if ( *STATUS == SAI__OK )
      {
         for ( i = 0; ( i <= ( i2 - i1 ) ) && ( i < APPN_length ); i++ )
         {
            APPN[ i ] = buf[ i + i1 ];
         }

/* Pad any remaining space in the caller's buffer with blanks.		    */
         for ( ; i < APPN_length; i++ )
         {
            APPN[ i ] = (F77_CHARACTER_TYPE) ' ';
         }
      }

/* If necessary, call the error tracing function. */
      if ( *STATUS != SAI__OK ) ndf1Trace( "ndf1_getap", STATUS );

/* Exit the routine.							    */
      return;
   }
