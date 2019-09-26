/* Subroutine:  psx_calloc( nmemb, type, pntr, status )
*+
*  Name:
*     PSX_CALLOC

*  Purpose:
*     Allocate space for several objects of specified type.

*  Language:
*     ANSI C

*  Invocation:
*     CALL PSX_CALLOC( NMEMB, TYPE, PNTR, STATUS )

*  Description:
*     The routine allocates an amount of virtual memory specified by
*     NMEMB and TYPE and initializes the space to all bits zero. The
*     number of bytes allocated is equal to the number of bytes
*     required to store a single variable of type TYPE, multiplied by
*     NMEMB. A pointer to the allocated storage is returned in PNTR.
*     This pointer can be passed on to other subroutines using the %VAL
*     construct. If the storage cannot be allocated, then PNTR is set
*     to zero, STATUS is set to PSX__NOALL and an error is reported.

*  Arguments:
*     NMEMB = INTEGER (Given)
*        The number of locations of TYPE required. If the number required
*        exceeds the maximum that can be stored in an INTEGER (about
*        2.1E9), them routine PSX_CALLOC8 should be used in place of
*        PSX_CALLOC. PSX_CALLOC8 has an identical interface except that
*        the NMEMB argument is an INTEGER*8.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of each location
*     PNTR = POINTER (Returned)
*        A pointer to the allocated storage.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*        CALL PSX_CALLOC( 20, '_INTEGER', PNTR, STATUS )
*        CALL SUB1( %VAL(PNTR), 20, STATUS )
*            ...
*        SUBROUTINE SUB1( ARRAY, N, STATUS )
*        INTEGER N
*        INTEGER ARRAY( N )
*           ...
*
*        Generates storage for an array.
*
*        The call to PSX_CALLOC allocates storage for a 20 element array
*        of type INTEGER. The pointer to this storage is then passed to
*        subroutine SUB1, where it is accessed as an array of integers.
*        We assume SUB1 returns without action if STATUS is bad.

*  Notes:
*     -  Storage allocated by PSX_CALLOC should be returned by a call to
*        PSX_FREE when it is no longer needed.
*
*     -  PNTR is declared to be of type POINTER. This is usually
*        represented in FORTRAN as an INTEGER, although any type that
*        uses the same amount of storage would be just as good.
*        The pointer will have been registered for C and FORTRAN use,
*        according to the scheme described in SUN/209, allowing its use
*        where pointers are longer than INTEGERs. For portability, the
*        construct %VAL(CNF_PVAL(PNTR)), rather than simply %VAL(PNTR),
*        should be used to pass the pointer to the subroutine. Function
*        CNF_PVAL is described in SUN/209 Section `Pointers'.
*
*     -  If several calls to PSX_CALLOC are made, the space returned by
*        each call is completely separate from that made by any other
*        call. In particular, the program should not assume that the
*        space returned by successive calls is contiguous.
*
*     -  PSX_CALLOC differs from the POSIX function calloc in that the
*        size of each member to be allocated is specified by a character
*        string (TYPE) rather than as a numerical value. This has been
*        done to increase the portability of the routine.
*
*     -  The allowed values of TYPE are _INTEGER, _REAL, _DOUBLE,
*        _INT64, _LOGICAL, _CHAR, _BYTE, _UBYTE, _WORD and _UWORD.
*        The number of bytes allocated for each is as defined in the
*        f77.h header file.

*  External Routines Used:
*     cnf: cnfCalloc, cnfFptr, cnfImpn

*  References:
*     -  POSIX standard (1988), section 8.1
*     -  ANSI C standard (1989), section 4.10.3.1

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     PMA: Peter Allan (Starlink, RAL)
*     RFWS: R.F. Warren-Smith (Starlink, RAL)
*     AJC: A.J. Chipperfield (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     8-APR-1991 (PMA):
*        Original version.
*     15-APR-1991 (PMA):
*        Changed calls to ems to calls to psx1.
*     19-JUN-1991 (PMA):
*        Removed dummy variables used by sizeof.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     14-APR-1993 (PMA):
*        Cast the temporary pointer to an F77 pointer.
*     14-APR-1999 (RFWS):
*        Use CNF for memory allocation.
*     23-JUN-2000 (AJC):
*        Improve documentation re pointers
*        Tidy refs to CNF routines
*      8-JAN-2002 (AJC):
*        Add type _BYTE, _UBYTE, _WORD and _UWORD
*      3-SEP-2004 (TIMJ):
*        Fix -Wall compiler warnings
*      23-SEP-2005 (TIMJ):
*        Fix compiler warning when sizeof(size_t) != sizeof(int)
*      19-DEC-2006 (TIMJ):
*        Error message should be unsigned.
*     25-MAY-2011 (TIMJ):
*        Simplify error reporting.
*     2012-05-11 (TIMJ):
*        Add _INT64
*     26-SEP-2019 (DSB):
*        Add 8-byte interface (PSX_CALLOC8).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/

#include <config.h>

/* Global Constants:		.					    */

#if STDC_HEADERS
#  include <ctype.h>		 /* Character handling			    */
#  include <stdlib.h>		 /* Standard C library			    */
#  include <stdio.h>		 /* Standard C I/O library		    */
#  include <string.h>
#  include <stdint.h>
#  include <inttypes.h>
#endif
#include "f77.h"		 /* C - Fortran interface		    */
#include "psx_err.h"		 /* PSX error codes			    */
#include "psx1.h"		 /* Internal PSX routines		    */
#include "sae_par.h"		 /* ADAM constants			    */

/* Prototypes */
F77_SUBROUTINE(psx_calloc8)( INTEGER8(nmemb), CHARACTER(type),
                             POINTER(pntr), INTEGER(status) TRAIL(type) );



/* 4-byte interface - just calls the 8-byte interface */
F77_SUBROUTINE(psx_calloc)( INTEGER(nmemb), CHARACTER(type),
                            POINTER(pntr), INTEGER(status) TRAIL(type) ){
   GENPTR_INTEGER(nmemb)
   GENPTR_CHARACTER(type)
   GENPTR_POINTER(pntr)
   GENPTR_INTEGER(status)
   DECLARE_INTEGER8(nmemb8);
   nmemb8 = *nmemb;
   F77_CALL(psx_calloc8)( INTEGER8_ARG(&nmemb8), CHARACTER_ARG(type),
                          POINTER_ARG(pntr), INTEGER_ARG(status) TRAIL_ARG(type) );
}



/* 8-byte interface */
F77_SUBROUTINE(psx_calloc8)( INTEGER8(nmemb), CHARACTER(type),
                             POINTER(pntr), INTEGER(status) TRAIL(type) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER8(nmemb)
   GENPTR_CHARACTER(type)
   GENPTR_POINTER(pntr)
   GENPTR_INTEGER(status)

/* Local Constants:							    */

#define maxlen 10			 /*  Maximum length of TYPE.	    */

/* Local Variables:							    */

   int i;			 /* Loop counter			    */
   size_t size;			 /* The size in bytes of the data type	    */
   void *temp;			 /* Temporary return value from malloc	    */
   char type_c[maxlen+1];	 /* A C string copy of the argument TYPE    */
   char *p;			 /* Temporary pointer			    */

/* Variable initialisation */
   size = 0;

/* Check inherited global status.					    */

   if( *status != SAI__OK ) return;

/* Import the argument "type" into a local C string.			    */

   cnfImpn( type, type_length, maxlen, type_c );

/* Convert type_c to upper case.					    */

   for( i = 0, p = type_c ; ( i < maxlen ) && *p ; i++, p++ )
      if( islower( *p ) )
         *p = toupper( *p );

/* Find the size of the requested FORTRAN type.				    */

   if( strcmp( type_c, "_INTEGER" ) == 0 )
      size = sizeof(F77_INTEGER_TYPE);
   else if( strcmp( type_c, "_REAL" ) == 0 )
      size = sizeof(F77_REAL_TYPE);
   else if( strcmp( type_c, "_DOUBLE" ) == 0 )
      size = sizeof(F77_DOUBLE_TYPE);
   else if( strcmp( type_c, "_INT64" ) == 0 )
      size = sizeof(F77_INTEGER8_TYPE);
   else if( strcmp( type_c, "_LOGICAL" ) == 0 )
      size = sizeof(F77_LOGICAL_TYPE);
   else if( strcmp( type_c, "_CHAR" ) == 0 )
      size = 1;
   else if( strcmp( type_c, "_BYTE" ) == 0 )
      size = sizeof(F77_BYTE_TYPE);
   else if( strcmp( type_c, "_UBYTE" ) == 0 )
      size = sizeof(F77_UBYTE_TYPE);
   else if( strcmp( type_c, "_WORD" ) == 0 )
      size = sizeof(F77_WORD_TYPE);
   else if( strcmp( type_c, "_UWORD" ) == 0 )
      size = sizeof(F77_UWORD_TYPE);
   else
      {
         *status = PSX__INTYP;
         psx1_rep_c( "PSX_CALLOC_INTYP",
           "Invalid type in call to PSX_CALLOC", status );
      }

/* Check the status and allocate the space if all is well.		    */

   if( *status == SAI__OK )
   {

/* Allocate the space (use CNF so that the resulting pointer can be used    */
/* from both C and Fortran).						    */

      temp = cnfCalloc( *nmemb, size );

/* Check that the space was allocated.					    */

      if( temp != 0 )

/* Copy the pointer to the allocated storage space to the subroutine	    */
/* argument, converting to a Fortran pointer.				    */

      {
         *pntr = cnfFptr( temp );
      }
      else

/* Set the pointer to zero, set STATUS to an error code and report the	    */
/* error.								    */

      {
         size_t total = 0;
         *pntr = (F77_POINTER_TYPE)0;
         *status = PSX__NOALL;
	 total = *nmemb * size;
         psx1_rep_c( "PSX_CALLOC_NOALL",
                     "Failed to allocate space with calloc. % " PRId64 " elements of size %d bytes (%zu bytes total) requested",
                     status,
                     *nmemb, (int)size, total );
      }
   }

}
