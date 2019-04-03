#define _POSIX_SOURCE 1		 /* Declare POSIX source		    */

#if defined( vms )		 /* VMS version include files:		    */
#include <stddef.h>		 /* Define NULL				    */

#include <rms.h>		 /* RMS definitions			    */
#include <stsdef.h>		 /* System status codes			    */

#else				 /* POSIX version include files:	    */
#include <errno.h>		 /* For errno				    */
#include <limits.h>		 /* System limits (for PATH_MAX)	    */
#include <stdio.h>		 /* For FILENAME_MAX			    */
#include <stdlib.h>		 /* For malloc, free, etc.		    */
#include <string.h>		 /* String functions			    */

#include <sys/types.h>		 /* For struct stat			    */
#include <sys/stat.h>		 /* For stat function			    */
#include <unistd.h>		 /* For getcwd, etc.			    */

#if !defined( FILENAME_MAX )	 /* Overcome gcc compiler problems on SUNs  */
#if defined( PATH_MAX )		 /* Use POSIX definition instead of ANSI C  */
#define FILENAME_MAX PATH_MAX
#else
#define FILENAME_MAX _POSIX_PATH_MAX
#endif
#endif
#endif

#include "sae_par.h"		 /* Standard SAE constants		    */
#include "ems.h"		 /* EMS_ error reporting routines	    */
#include "f77.h"		 /* Fortran 77 <=> C interface macros	    */
#include "ndf1.h"		 /* Internal NDF definitions		    */

   F77_SUBROUTINE(ndf1_expfn)( CHARACTER(IN),
                               LOGICAL(GETFID),
                               CHARACTER(OUT),
                               INTEGER(LOUT),
                               CHARACTER(FID),
                               INTEGER(STATUS)
                               TRAIL(IN)
                               TRAIL(OUT)
                               TRAIL(FID) )
   {
/*
*+
*  Name:
*     NDF1_EXPFN

*  Purpose:
*     Expand a file name.

*  Language:
*     ANSI C

*  Invocation:
*     CALL NDF1_EXPFN( IN, GETFID, OUT, LOUT, FID, STATUS )

*  Description:
*     The routine expands a foreign file name into its full (absolute)
*     form and optionally returns a file identification code which
*     uniquely identifies the file.

*  Arguments:
*     IN = CHARACTER * ( * ) (Given)
*        The initial file name, which is to be expanded. Leading and
*        trailing blanks are ignored.
*     GETFID = LOGICAL (Read)
*        Whether a file identification code is to be returned (this also
*        affects whether a file version number is returned on VMS).
*     OUT = CHARACTER * ( * ) (Returned)
*        The fully expanded file name.
*     LOUT = INTEGER (Returned)
*        The number of significant characters in the expanded file name.
*     FID = CHARACTER * ( * ) (Returned)
*        The file identification code (not used unless GETFID is .TRUE.).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine is designed to be called from Fortran.
*     -  On VMS systems, the expanded file name returned will contain a
*     file type field and/or a version number field only if the input
*     file name also contains the corresponding field, or if GETFID is
*     .TRUE. (in which case a search for the file will be made and a
*     fully-expanded, unique, file name will be returned).
*     -  On POSIX (UNIX) systems, the file name and type fields are not
*     interpreted by this routine and are returned as supplied. Only
*     directory information is added.
*     -  A blank value for OUT will be returned without error if the
*     input file name is blank (no FID value will be returned in this
*     case).
*     -  An error will be reported if the OUT buffer is too short to
*     accommodate the returned file name.
*     -  The file itself need not exist unless a file identification
*     code is requested.
*     -  The length of the file identification code returned will be
*     system-dependent (a suitably long character string should be
*     provided). An error will result if the string supplied is too
*     short to accommodate it.
*     -  If it is not possible to obtain file identification
*     information (for instance, this may occur when accessing some
*     remote files) then FID will be returned set to all blanks. No
*     error will result unless the file also appears not to exist.

*  Implementation Deficiencies:
*     On POSIX (UNIX) systems, full shell expansion of the file name is
*     not performed. Hence, environment variable substitution will not
*     occur, although use of file names such as ~user/file is supported.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     RFWS: R.F. Warren-Smith (STARLINK. RAL)
*     {@enter_new_authors_here@}

*  History:
*     10-MAR-1994 (RFWS):
*        Original version.
*     16-MAR-1994 (RFWS):
*        Added the GETFID and FID arguments.
*     12-MAY-1994 (RFWS):
*        Report an error if the returned file name is truncated.
*     12-MAY-1994 (RFWS):
*        Allow version numbers to be returned (if originally specified) on
*        VMS.
*     20-MAY-1994 (RFWS):
*        Also return version numbers (to uniquely identify the file) on
*        VMS if GETFID is .TRUE..
*     {@enter_further_changes_here@}

*  Bugs:
*     {@note_any_bugs_here@}

*-
*/

/* Arguments Given:							    */
     GENPTR_CHARACTER(IN)
     GENPTR_LOGICAL(GETFID)

/* Arguments Returned:							    */
      GENPTR_CHARACTER(OUT)
      GENPTR_INTEGER(LOUT)
      GENPTR_CHARACTER(FID)

/* Status:								    */
      GENPTR_INTEGER(STATUS)

/* External References:							    */
#if defined( vms )		 /* VMS version system calls:		    */
      unsigned int SYS$PARSE( struct FAB *fab ); /* Parse file name	    */
      unsigned int SYS$SEARCH( struct FAB *fab ); /* Search for file	    */
#endif

/* Local Variables:							    */
      char *result;		 /* Pointer to result string		    */
      char *tmp;		 /* Temporary variable			    */
      int i;			 /* Loop counter for characters		    */
      int len;			 /* Length of file name			    */
      int start;		 /* Offset to start of file name	    */
      size_t size;		 /* Amount of memory to allocate	    */

#if defined( vms )		 /* VMS version local variables:	    */
      char *name;                /* Pointer to file name string             */
      char esabuf[ NAM$C_MAXRSS ]; /* Expanded file name string buffer	    */
      char rsabuf[ NAM$C_MAXRSS ]; /* Resultant file name string buffer	    */
      struct FAB fab;		 /* RMS file access block		    */
      struct NAM nam;		 /* RMS NAM block			    */
      unsigned int systat;	 /* System status code			    */

#else				 /* POSIX version local variables:	    */
      char cwd[ FILENAME_MAX + 1 ]; /* Current working directory buffer	    */
      size_t szname;		 /* Size of name string			    */
      struct stat statbuf;	 /* File status information buffer	    */
#endif

/*.                                                                         */

/* Check the inherited global status.					    */
      if ( *STATUS != SAI__OK ) return;

/* Determine the offet to the start of the file name (the first non-blank   */
/* character).								    */
      for ( start = 0; start < IN_length; start++ )
      {
         if ( IN[ start ] != (F77_CHARACTER_TYPE) ' ' ) break;
      }

/* Determine the length of the input file name, omitting trailing blanks.   */
      for ( len = IN_length; len > start; len-- )
      {
         if ( IN[ len - 1 ] != (F77_CHARACTER_TYPE) ' ' ) break;
      }
      len -= start;

/* If the name is blank, then return a blank result.			    */
      if ( len == 0 )
      {
         for ( i = 0; i < OUT_length; i++ )
         {
            OUT[ i ] = (F77_CHARACTER_TYPE) ' ';
         }
         *LOUT = (F77_INTEGER_TYPE) 0;
      }

/* Allocate space to hold the input name as a C string. Check for errors.   */
      else
      {
         size = (size_t) len + (size_t) 1;
         result = (char *) malloc( size );
         if ( result == NULL )
         {
            *STATUS = NDF__NOMEM;
            emsSeti( "NBYTES", (int) size );
            emsErrno( "MESSAGE", errno );
            emsRep( "NDF1_EXPFN_1",
                       "Unable to allocate a block of ^NBYTES bytes of \
memory - ^MESSAGE",
                       STATUS );
         }

/* If OK, copy the name into the allocated space.			    */
         else
         {
            for ( i = 0; i < len; i++ )
            {
               result[ i ] = (char) IN[ i + start ];
            }
            result[ i ] = '\0';

/* VMS version:								    */
/* ===========								    */
#if defined( vms )

/* Initialise the FAB and NAM blocks.					    */
            fab = cc$rms_fab;
            fab.fab$l_fna = result;
            fab.fab$b_fns = len;
            fab.fab$l_nam = &nam;

            nam = cc$rms_nam;
            nam.nam$l_esa = esabuf;
            nam.nam$b_ess = NAM$C_MAXRSS;
            nam.nam$l_rsa = rsabuf;
            nam.nam$b_rss = NAM$C_MAXRSS;

/* Parse the file name and check for errors.				    */
            systat = SYS$PARSE( &fab );
            if ( !( systat & STS$M_SUCCESS ) )
            {
               *STATUS = ( systat == RMS$_PRV ) ? NDF__FILPR : NDF__FILIN;
               emsSetnc( "FILE", result, len );
               emsSyser( "MESSAGE", systat );
               emsRep( "NDF1_EXPFN_2",
                          "Error in file name \'^FILE\' - ^MESSAGE.",
                          STATUS );
            }

/* If OK, obtain a pointer to the resulting file name together with its     */
/* length.                                                                  */
            else
            {
               name = esabuf;
               len = (int) nam.nam$b_esl;
            }

/* If OK, and a file identification code is required, then search for the   */
/* file to obtain this. Check for errors.				    */
            if ( ( *STATUS == SAI__OK ) && ( F77_ISTRUE( *GETFID ) ) )
	    {
               systat = SYS$SEARCH( &fab );
               if ( !( systat & STS$M_SUCCESS ) )
   	       {
                  *STATUS = ( systat == RMS$_PRV ) ? NDF__FILPR : NDF__FILNF;
                  emsSetnc( "FILE", (const char *) nam.nam$l_esa,
                              (int) nam.nam$b_esl );
                  emsSyser( "MESSAGE", systat );
                  emsRep( "NDF1_EXPFN_3",
                             "Error searching for file ^FILE - ^MESSAGE.",
                             STATUS );
	       }

/* If OK, obtain a pointer to the resulting file name together with its     */
/* length.                                                                  */
               else
               {
                  name = rsabuf;
                  len = (int) nam.nam$b_rsl;
               }

/* Check that the FID string is long enough to accommodate the result.	    */
/* Report an error if it is not (demand one extra character so that an      */
/* all-blank result can be distinguished from all possible valid results,   */
/* where trailing characters will be set to zero).                          */
               if ( *STATUS == SAI__OK )
               {
                  if ( FID_length < 7 )
	          {
                     *STATUS = NDF__TRUNC;
                     emsSetnc( "ROUTINE", "NDF1_EXPFN", EMS__SZTOK );
                     emsSeti( "LEN", FID_length );
                     emsRep( "NDF1_EXPFN_4",
                                "Routine ^ROUTINE called with a FID argument \
which is too short (^LEN characters) to accommodate the returned file \
identification (7 characters) - internal programming error.",
                                STATUS );
                  }

/* If the file identification is all zero (which will be the case for	    */
/* DECnet operations, for example) then signify this by setting FID to all  */
/* blanks.								    */
                  else if ( ( nam.nam$w_fid[ 0 ] == 0 ) &&
                            ( nam.nam$w_fid[ 1 ] == 0 ) &&
                            ( nam.nam$w_fid[ 2 ] == 0 ) )
                  {
	             for ( i = 0; i < FID_length; i++ )
		     {
		        FID[ i ] = (F77_CHARACTER_TYPE) ' ';
		     }
	          }

/* If OK, zero the returned FID string and copy the file identification (6  */
/* bytes) into the start of it.						    */
                  else
                  {
                     (void) memset( (void *) FID, 0, (size_t) FID_length );
                     (void) memcpy( (void *) FID,
                                    (const void *) nam.nam$w_fid,
                                    (size_t) 6 );
	          }
               }
            }

/* If OK, allocate memory to hold the file name string and check for        */
/* errors.                                                                  */
            if ( *STATUS == SAI__OK )
            {
               size = (size_t) len + (size_t) 1;
               tmp = (char *) malloc( size );
               if ( tmp == NULL )
               {
                  *STATUS = NDF__NOMEM;
                  emsSeti( "NBYTES", (int) size );
                  emsErrno( "MESSAGE", errno );
                  emsRep( "NDF1_EXPFN_5",
                             "Unable to allocate a block of ^NBYTES bytes of \
memory - ^MESSAGE",
                             STATUS );
               }

/* Copy the expanded file name and terminate it with a null. Free the	    */
/* previously-allocated memory.						    */
               else
               {
                  (void) memcpy( (void *) tmp, (const void *) name,
                                 (size_t) len );
                  tmp[ len ] = '\0';
                  free( (void *) result );
                  result = tmp;

/* Search backwards through the expanded file name to locate the start of   */
/* the version field.							    */
                  for ( i = len - 1; i >= 0; i-- )
                  {

/* If the version delimiter ';' immediately follows a '.' the file type     */
/* is absent (e.g. was not specified). Shift the remainder of the file name */
/* to the left to eliminate the '.' and adjust the length.                  */
                     if ( ( result[ i ] == ';' ) && ( i > 0 ) &&
			  ( result[ i - 1 ] == '.' ) )
                     {
		        (void) memmove( (void *) ( result + i - 1 ),
				        (const void *) ( result + i ),
				        (size_t) ( len - i + 1 ) );
                        len--;
                        break;
                     }
                  }

/* If the version delimiter ';' occurs at the end of the file name, then    */
/* no version number was specified, so eliminate the delimiter.             */
                  if ( result[ len - 1 ] == ';' ) result[ len - 1 ] = '\0';
               }
            }

/* POSIX version:							    */
/* =============							    */
#else

/* If the input file name begins with ~, then expand this into the	    */
/* appropriate user's initial working directory name and free the original  */
/* string.								    */
            if ( result[ 0 ] == '~' )
            {
               tmp = ndf1_tilde( result, STATUS );
               free( (void *) result );
               result = tmp;
            }

/* If the input name is not an absolute file name, then attempt to obtain   */
/* the name of the current working directory to prepend to it.  Check for   */
/* errors.								    */
            if ( *STATUS == SAI__OK )
            {
               if ( result[ 0 ] != '/' )
               {
                  if ( getcwd( cwd, (size_t) FILENAME_MAX ) == NULL )
                  {
                     *STATUS = NDF__FATIN;
                     emsErrno( "MESSAGE", errno );
                     emsRep( "NDF1_EXPFN_6",
                                "Unable to determine the path name of the \
current working directory - ^MESSAGE",
                                STATUS );
                  }

/* Loop to repeatedly remove any redundant occurrences of ./ at the start   */
/* of the relative path name.						    */
                  else
                  {
                     for ( szname = strlen( result );
                           ( szname >= (size_t) 2 ) &&
                           ( result[ 0 ] == '.' ) && ( result[ 1 ] == '/' );
                           szname -= (size_t) 2 )
                     {
                        (void) memmove( (void *) result,
                                        (void *) ( result + 2 ),
                                        szname - (size_t) 1 );
                     }

/* Allocate space to hold the full file name, checking for errors.	    */
                     size = strlen( cwd ) + (size_t) 1 + szname + (size_t) 1;
                     tmp = (char *) malloc( size );
                     if ( tmp == NULL )
                     {
                        *STATUS = NDF__NOMEM;
                        emsSeti( "NBYTES", (int) size );
                        emsErrno( "MESSAGE", errno );
                        emsRep( "NDF1_EXPFN_7",
                                   "Unable to allocate a block of ^NBYTES \
bytes of memory - ^MESSAGE",
                                   STATUS );
                     }

/* If OK, construct the full file name and free the space holding the	    */
/* original version.							    */
                     else
                     {
                        (void) strcpy( tmp, cwd );
                        (void) strcat( tmp, "/" );
                        (void) strcat( tmp, result );
                        free( (void *) result );
                        result = tmp;
                     }
                  }
               }
            }

/* If OK and a file identification code is required, then obtain file	    */
/* status information and check for errors. Zero the file status buffer     */
/* first to avoid any possible problems with junk which may exist within    */
/* structure padding bytes.                                                 */
            if ( ( *STATUS == SAI__OK ) && ( F77_ISTRUE( *GETFID ) ) )
	    {
               (void) memset( (void *) &statbuf, 0, sizeof( statbuf ) );
               if ( stat( result, &statbuf ) )
      	       {
                  *STATUS = NDF__FILNF;
                  emsSetnc( "FILE", result, EMS__SZTOK );
                  emsErrno( "MESSAGE", errno );
                  emsRep( "NDF1_EXPFN_8",
                             "Unable to obtain file status information for \
file \'^FILE\' - ^MESSAGE",
                             STATUS );
	       }

/* Check that the FID string is long enough to accommodate the result.	    */
/* Report an error if it is not (demand one extra character so that an      */
/* all-blank result can be distinguished from all possible valid results,   */
/* where trailing characters will be set to zero).                          */
               else if ( FID_length < (int) ( sizeof( statbuf.st_ino ) +
                                              sizeof( statbuf.st_dev ) +
                                              1 ) )
	       {
                  *STATUS = NDF__TRUNC;
                  emsSetnc( "ROUTINE", "NDF1_EXPFN", EMS__SZTOK );
                  emsSeti( "LEN", FID_length );
                  emsSeti( "RET", (int) ( sizeof( statbuf.st_ino ) +
                                             sizeof( statbuf.st_dev ) +
                                             1 ) );
                  emsRep( "NDF1_EXPFN_9",
                             "Routine ^ROUTINE called with a FID argument \
which is too short (^LEN characters) to accommodate the returned file \
identification (^RET characters) - internal programming error.",
                             STATUS );
               }

/* If OK, zero the FID string and copy the file status fields that identify */
/* the file into the start of it.					    */
	       else
	       {
                  (void) memset( (void *) FID, 0, (size_t) FID_length );
                  (void) memcpy( (void *) FID,
                                 (const void *) &statbuf.st_ino,
                                 sizeof( statbuf.st_ino ) );
                  (void) memcpy( (void *) ( FID + sizeof( statbuf.st_ino ) ),
                                 (const void *) &statbuf.st_dev,
                                 sizeof( statbuf.st_dev ) );
 	       }
	    }
#endif

/* If there has been no error, then copy the expanded file name into the    */
/* output buffer, truncating if necessary.				    */
            if ( *STATUS == SAI__OK )
            {
               for ( i = 0; ( result[ i ] != '\0' ) &&
                            ( i < OUT_length ); i++ )
               {
                  OUT[ i ] = (F77_CHARACTER_TYPE) result[ i ];
               }

/* Return the length of the expanded file name.				    */
               *LOUT = (F77_INTEGER_TYPE) i;

/* Report an error if the file name has been truncated.                     */
               if ( result[ i ] != '\0' )
	       {
                  *STATUS = NDF__TRUNC;
                  emsSetnc( "ROUTINE", "NDF1_EXPFN", EMS__SZTOK );
                  emsSeti( "LEN", OUT_length );
                  emsSeti( "FLEN", (int) strlen( result ) );
                  emsRep( "NDF1_EXPFN_10",
                             "Routine ^ROUTINE called with an OUT argument \
which is too short (^LEN characters) to accommodate the returned file \
name (^FLEN characters).",
                             STATUS );
	          emsRep( "NDF1_EXPFN_11",
                             "The NDF_ library is unable to handle a file \
name of this length.", STATUS );
	       }

/* Pad the caller's buffer with blanks if necessary.			    */
	       else
	       {
                  for ( ; i < OUT_length; i++ )
                  {
                     OUT[ i ] = (F77_CHARACTER_TYPE) ' ';
		  }
               }
            }

/* Free temporary workspace.						    */
            free( (void *) result );
         }
      }

/* If necessary, call the error tracing function. */
      if ( *STATUS != SAI__OK ) ndf1Trace( "ndf1_expfn", STATUS );

/* Exit the routine.							    */
      return;
   }
