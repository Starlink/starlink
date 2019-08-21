#define _POSIX_SOURCE 1		 /* Declare POSIX source		    */

#if defined( vms )		 /* VMS version include files:		    */
#include <descrip.h>		 /* Descriptor definitions		    */
#include <jpidef.h>		 /* lib$getjpi definitions		    */
#include <libdef.h>		 /* Run time library definitions	    */
#include <stddef.h>		 /* Define NULL				    */
#include <stsdef.h>		 /* System status codes			    */
#endif

#include "sae_par.h"		 /* Standard SAE constants		    */
#include "ems.h"		 /* EMS_ error reporting routines	    */
#include "f77.h"		 /* Fortran 77 <=> C interface macros	    */
#include "ndf1.h"		 /* Internal NDF definitions		    */


F77_SUBROUTINE(ndf1_farg)( INTEGER(IARG), CHARACTER(ARG) TRAIL(ARG) );


   F77_SUBROUTINE(ndf1_gtarg)( INTEGER(IARG),
                               CHARACTER(ARG),
			       INTEGER(LARG),
			       INTEGER(STATUS)
			       TRAIL(ARG) )
   {
/*
*+
*  Name:
*     NDF1_GTARG

*  Purpose:
*     Get command line arguments.

*  Language:
*     ANSI C

*  Invocation:
*     NDF1_GTARG( IARG, ARG, LARG, STATUS )

*  Description:
*     The routine returns any of the command line arguments used when
*     invoking the current application. The returned value will be
*     truncated without error if the variable supplied is too short.

*  Arguments:
*     IARG = INTEGER (Given)
*        The number of the argument required. Argument zero gives the
*        name of the command used to invoke the application. Subsequent
*        arguments return successive tokens from whatever followed this
*        command.
*     ARG = CHARACTER * ( * ) (Returned)
*        The argument value. A blank value is returned if the requested
*        argument does not exist.
*     LARG = INTEGER (Returned)
*        Number of significant characters in the returned argument
*        value. Zero is returned if the requested argument does not
*        exist,
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine is intended for calling from Fortran.

*  Machine-specific features:
*     -  On UNIX systems, this routine uses the GETARG Fortran system
*     call which may not exist on all systems.
*     -  If it should not prove possible to determine the required
*     argument values on any particular system, then this routine
*     should be implemented so as to return blank values for all
*     arguments (code to perform this is included). Routines which call
*     it should anticipate this possible behaviour.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

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
*     PWD: Peter Draper (Durham)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     9-SEP-1993 (RFWS):
*        Original version.
*     5-OCT-1998 (RFWS):
*        Use global variables to obtain argument information if available.
*     29-JUL-2005 (PWD):
*        Fix for g95 when calling G95 intrinsic.
*     21-SEP-2006 (TIMJ):
*        Check length before checking content.
*     7-NOV-2007 (DSB):
*        Add prototype for ndf1_farg to avoid compiler warnings.
*     <{enter_further_changes_here}>

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Arguments Given:							    */
      GENPTR_INTEGER(IARG)

/* Arguments Returned:							    */
      GENPTR_CHARACTER(ARG)
      GENPTR_INTEGER(LARG)

/* Status:								    */
      GENPTR_INTEGER(STATUS)

/* External References:							    */
#if defined( vms )		 /* VMS version system calls:		    */
      extern unsigned int lib$getjpi /*	Get job/process information	    */
         ( int *item_code,
	   unsigned int *process_id,
	   struct dsc$descriptor *process_name,
	   void *resultant_value,
	   struct dsc$descriptor *resultant_string,
	   unsigned short int *resultant_length );

      extern unsigned int lib$get_foreign /* Get foreign command line	    */
	 ( struct dsc$descriptor *resultant_string,
	   struct dsc$descriptor *prompt_string,
	   unsigned short int *resultant_length,
	   unsigned int *flags );

#else				 /* UNIX version system calls:		    */
      extern void F77_EXTERNAL_NAME(getarg) /* Get command line arguments   */
         ( INTEGER(IARG),
	   CHARACTER(ARG)
	   TRAIL(ARG) );
#endif

/* Local Variables:							    */
#if defined( vms )		 /* VMS version local variables:	    */
      int code;			 /* getjpi item code			    */
      int found;		 /* Required argument found?		    */
      int gap;			 /* In an inter-argument gap?		    */
      int i1;			 /* Index of start of field		    */
      int i2;			 /* Index of end of field		    */
      int narg;			 /* Number of arguments found		    */
      int quoted;		 /* Character is quoted?		    */
      static char *buf0;	 /* Pointer to file name		    */
      static char *bufn;	 /* Pointer to command line		    */
      static char *result0;	 /* Pointer to start of argument 0	    */
      static int firstarg0 = 1;  /* First request for argument 0?	    */
      static int firstargn = 1;	 /* First request for argument 1..n?	    */
      static int len0;		 /* Length of argument 0		    */
      static unsigned short int lencmd;	/* Length of foreign command string */
      struct dsc$descriptor dsc; /* String descriptor			    */
      unsigned int systat;	 /* System status code			    */
      unsigned short int lenfil; /* Length of executing image file name	    */
#endif
      const char *result;	 /* Pointer to start of result string	    */
      int i;			 /* Loop counter for characters		    */
      int iarg;                  /* Argument index */
      int len;			 /* Length of result string		    */
      int ndf1_argc = -1;       /* Number of command-line arguments from ndfInit */
      char *const *ndf1_argv = NULL; /* Command-line arguments from ndfInit */

/*.									    */

/* Check inherited global status.					    */
      if ( *STATUS != SAI__OK ) return;

/* Obtain the index of the required argument. */
      iarg = (int) *IARG;

/* See if ndfInit has been used to initialise an argument list. If so,
   we use this in preference to any other source of argument
   information. */
      ndf1_argv = ndf1_getargvc( &ndf1_argc, STATUS );
      if ( ndf1_argc > -1 ) {

/* Obtain a pointer to the required argument string. */
         if ( iarg < 0 ) {
            result = "";
         } else {
            result = ( iarg < ndf1_argc ) ? ndf1_argv[ iarg ] : "";
         }

/* Copy the string to the caller's buffer. */
         for ( len = 0; (len < ARG_length) && result[ len ]; len++ ) {
            ARG[ len ] = (F77_CHARACTER_TYPE) result[ len ];
         }

/* Pad the buffer with blanks. */
         for ( i = len; i < ARG_length; i++ ) {
            ARG[ i ] = (F77_CHARACTER_TYPE) ' ';
         }

/* Return the string length. */
         *LARG = (F77_INTEGER_TYPE) len;

/* If no argument information has been initialised, then look
   elsewhere for it. */
      } else {

/* VMS Version:								    */
/* ===========								    */
#if defined( vms )

/* IARG is negative.							    */
/* ----------------							    */
/* Return a blank argument value.					    */
         if ( iarg < 0 ) {
            result = NULL;
            len = 0;

/* IARG is zero.							    */
/* ------------								    */
/* We must obtain the name of the command being executed. On VMS this is    */
/* not available via the command line, so we obtain it from the file name   */
/* of the currently executing image instead.				    */
         } else if ( !iarg ) {

/* We only need to perform this once, on the first invocation of this	    */
/* routine.								    */
            if ( firstarg0 ) {

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
                                    &dsc, &lenfil );

/* Check for and report any errors - also trap string truncation (normally  */
/* regarded as a success status).					    */
               if ( !( systat & STS$M_SUCCESS ) ||
                     ( systat == LIB$_STRTRU ) ) {
                  *STATUS = NDF__FATIN;
                  emsSyser( "MESSAGE", systat );
                  emsRep( "NDF1_GTARG_VMS0",
                             "Error determining the file name of the \
currently executing VMS image - ^MESSAGE.", STATUS );

/* If OK, extract a pointer to the returned string from the descriptor.	    */
               } else {
                  buf0 = (char *) dsc.dsc$a_pointer;

/* Search for the start of the file name field following the end of a	    */
/* directory specification.						    */
                  for ( i1 = ( (int) lenfil ) - 1; i1 >= 0; i1-- ) {
                     if ( buf0[ i1 ] == ']' ) break;
                  }
                  i1 = i1 + 1;

/* Search for the '.' which delimits the file type extension.		    */
                  for ( i2 = i1; i2 < (int) lenfil; i2++ ) {
                     if ( buf0[ i2 ] == '.' ) break;
                  }
                  i2 = i2 - 1;

/* For use on subsequent invocations, set up a pointer to the start of the  */
/* file name field and record its length.				    */
                  result0 = buf0 + i1;
                  len0 = i2 - i1 + 1;

/* Note that the first invocation of this routine requiring argument zero   */
/* has completed successfully.						    */
                  firstarg0 = 0;
               }
            }

/* On all invocations, obtain the result pointer and length.		    */
            if ( *STATUS == SAI__OK ) {
               result = result0;
               len = len0;
            }
         }

/* IARG is greater than zero.						    */
/* -------------------------						    */
/* We must obtain the VMS "foreign command line" which contains the command */
/* arguments. As before, this need only be done on the first invocation	    */
/* that requires it.							    */
         else {
            if ( firstargn ) {

/* Initialise a dynamic character string descriptor to describe the	    */
/* returned value.							    */
               dsc.dsc$a_pointer = NULL;
               dsc.dsc$b_class = DSC$K_CLASS_D;
               dsc.dsc$b_dtype = DSC$K_DTYPE_T;
               dsc.dsc$w_length = (unsigned short int) 0;

/* Obtain the foreign command line for the currently executing image.	    */
               systat = lib$get_foreign( &dsc, (struct dsc$descriptor *) 0,
                                         &lencmd, (unsigned int *) 0 );

/* Check for and report any errors.					    */
               if ( !( systat & STS$M_SUCCESS ) ) {
                  *STATUS = NDF__FATIN;
                  emsSyser( "MESSAGE", systat );
                  emsRep( "NDF1_GTCMD_VMSN",
                             "Error obtaining the VMS \"foreign\" command \
line used to invoke the current application - ^MESSAGE.", STATUS );

/* If OK, extract a pointer to the returned string from the descriptor.	    */
               } else {
                  bufn = (char *) dsc.dsc$a_pointer;

/* Note that the first invocation of this routine requiring the foreign	    */
/* command line has completed successfully.				    */
                  firstargn = 0;
               }
            }

/* On all invocations, initialise and loop to inspect each character in the */
/* command line.							    */
            if ( *STATUS == SAI__OK ) {
               found = 0;
               gap = 1;
               len = 0;
               narg = 0;
               quoted = 0;
               result = NULL;
               for ( i = 0; i < (int) lencmd; i++ ) {

/* Keep track of whether the current character is inside quotes.	    */
                  if ( bufn[ i ] == '\"' ) quoted = !quoted;

/* If the current character is an unquoted space and we are not already in  */
/* an inter-argument gap, then we have found the end of an argument.	    */
/* Increment the argument count and quit searching if it is the one we	    */
/* want. Otherwise, note we are now in an inter-argument gap.		    */
                  if ( ( bufn[ i ] == ' ' ) && !quoted ) {
                     if ( !gap ) {
                        if ( found = ( (int) *IARG == ++narg ) ) break;
                        gap = 1;
                     }
                  }

/* Otherwise, the character lies within an argument string. If we were	    */
/* previously in an inter-argument gap, it is the first argument character, */
/* so initialise the result pointer and length.				    */
                  else {
                     if ( gap ) {
                        result = bufn + i;
                        len = 0;
                     }

/* Increment the result length and note we are not in an inter-argument	    */
/* gap.									    */
                     len++;
                     gap = 0;
                  }
               }

/* If the loop above completed without identifying the end of the required  */
/* argument, then increment the argument count to include the final	    */
/* undetected argument (whose end is not followed by a blank). Return a	    */
/* blank result if the required argument was not present.		    */
               if ( (int) *IARG != ( found ? narg : ++narg ) ) {
                  result = NULL;
                  len = 0;
               }
            }
         }

/* If OK, copy the result into the caller's buffer, truncating if	    */
/* necessary.								    */
         if ( *STATUS == SAI__OK ) {
            for ( i = 0; ( i < len ) && ( i < ARG_length ); i++ ) {
               ARG[ i ] = (F77_CHARACTER_TYPE) result[ i ];
            }

/* Pad any remaining space in the caller's buffer with blanks.		    */
            for ( ; i < ARG_length; i++ ) {
               ARG[ i ] = (F77_CHARACTER_TYPE) ' ';
            }

/* Return the argument length.						    */
            *LARG = (F77_INTEGER_TYPE) ( len < ARG_length ) ? len : ARG_length;
         }

/* UNIX Version:							    */
/* ============								    */
/* This version should be disabled on UNIX systems which do not support the */
/* GETARG system call.							    */
#elif 1

/* Call the Fortran GETARG system routine to get the argument required.	    */
         F77_LOCK( F77_CALL(ndf1_farg)( INTEGER_ARG(IARG), CHARACTER_ARG(ARG)
                              TRAIL_ARG(ARG) ); )

/* Determine the argument length by searching for the last non-blank	    */
/* character.								    */
         for ( i = ARG_length; i > 0; i-- ) {
            if ( ARG[ i - 1 ] != (F77_CHARACTER_TYPE) ' ' ) break;
         }
         *LARG = (F77_INTEGER_TYPE) i;

/* Default Version:							    */
/* ===============							    */
/* For use if argument values cannot be found.				    */
#else

/* Simply fill the caller's buffer with blanks and return a length of zero. */
         for ( i = 0; i < ARG_length; i++ ) {
            ARG[ i ] = (F77_CHARACTER_TYPE) ' ';
         }
         *LARG = (F77_INTEGER_TYPE) 0;
#endif
      }

/* If necessary, call the error tracing function. */
      if ( *STATUS != SAI__OK ) ndf1Trace( "ndf1_gtarg", STATUS );

/* Exit the routine.							    */
      return;
   }
