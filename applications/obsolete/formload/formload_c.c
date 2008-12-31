/* *********************************************************************
*  FORMLOAD_C.C -- C subroutines called by FORMLOAD.FOR 
*    
*  Contains:-
*    WHATSY      Gets if VMS or UNIX
*    GENV        Gets value of environment variable
*
*   Other s/rs called by those above

*/

/* Subroutine:  whatsy ( kn )
*+
*  Name:
*     WHATSY

*  Purpose:
*     VMS or UNIX system?

*  Language:
*     ANSI C

*  Invocation:
*     CALL WHATSY ( KN )

*  Description:

*  Arguments:
*     KN = INTEGER (Returned)
*        0 = UNIX 1 = VMS   

*  Examples:

*  Notes:

*  External Routines Used:

*  References:
*     -  POSIX standard (1988), section 4.6.1
*     -  ANSI C standard (1989), section 4.10.4.4
      
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     AJP: Alan Penny (Astrophysics, RAL )
*     {enter_new_authors_here}

*  History:
*     5-NOV-1992 (AJP)
*        made
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/


/* Global Constants:		.					    */

#include <stdlib.h>		 /* Standard library			    */
#include <string.h>		 /* String handling library		    */

/*#include "f77.h"	C - Fortran interface		    */
/* define C/FORTRAN interface for our needs
     PMA: Peter Allan (Starlink, RAL)
*/

#if !defined(F77_MACROS)

#define F77_MACROS

#define F77_EXTERNAL_NAME(X) X ## _
#define F77_INTEGER_TYPE   int 
#define F77_CHARACTER_TYPE char
#define F77_WORD_TYPE      short int
#define F77_SUBROUTINE(X)  void F77_EXTERNAL_NAME(X)
#define CHARACTER_RETURN_VALUE (X) CHARACTER(X) TRAIL(X)
#define INTEGER(X)     F77_INTEGER_TYPE *X
#define POINTER(X)     void **X
#define CHARACTER(X)             F77_CHARACTER_TYPE X[]
#define TRAIL(X)                 ,int X ## _length
#define GENPTR_INTEGER(X)
#define GENPTR_CHARACTER(X)

#if defined(vms)	/* vms				*/
#undef  F77_EXTERNAL_NAME
#define F77_EXTERNAL_NAME(X) X
#include <descrip.h>
#undef  CHARACTER
#define CHARACTER(X) struct dsc$descriptor_s *X/**/_arg
#undef  TRAIL
#define TRAIL(X)
#undef  GENPTR_CHARACTER
#define GENPTR_CHARACTER(X) \
   F77_CHARACTER_TYPE *X = X/**/_arg->dsc$a_pointer; \
   int X/**/_length = X/**/_arg->dsc$w_length;
#endif  

#if defined(sun)		/* sun4				*/
#endif  

#if defined(mips)		/* mips			    */
#if !defined(__STDC__)
#undef  F77_EXTERNAL_NAME
#define F77_EXTERNAL_NAME(X) X/**/_
#undef  TRAIL
#define TRAIL(X) ,int X/**/_length
#endif  /* of non ANSI redefinitions			    */
#endif  

#endif


F77_SUBROUTINE(whatsy)( INTEGER(kn) )
{

/* Pointers to Arguments:						    */

   GENPTR_INTEGER(kn)

   *kn = 0;

#if defined(vms)
    *kn = 1;
#endif
}


/* Subroutine:  genv ( name, trans, status )
*+
*  Name:
*     GENV

*  Purpose:
*     Translate an environment variable

*  Language:
*     ANSI C

*  Invocation:
*     CALL GENV ( NAME, TRANS, STATUS )

*  Description:
*     The routine tries to get the translation of the environment
*     variable NAME. If it succeeds, it returns the translation in
*     TRANS. If it fails, it sets STATUS to 1

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the environment variable to be translated.
*     TRANS = CHARACTER * ( * ) (Returned)
*        The translation of the environment variable.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     CALL GENV( 'USER', TRANS, STATUS )
*        This will return the value of the environment variable USER,
*        i.e. the username of the current process.

*  Notes:
*     -  On VMS, there are four special environment variables.
*        HOME = The home directory of the user,
*               e.g. DISK$USER1:[PMA]
*        PATH = The default place(s) where files are looked for, 
*               i.e. the current default directory,
*               e.g. DISK$SCRATCH:[PMA.CCD]
*        TERM = The type of terminal attached to the process,
*               e.g. vt300-80.
*        USER = The username,
*               e.g. PMA
*        If the environment variable is not one of these special values,
*        then the system will attempt to translate NAME as a logical
*        name (using all of the normal logical name tables) and finally
*        it will attempt to translate NAME as a DCL symbol.
*     -  On VMS, PATH will translate to a single place, whereas on Unix,
*        it will commonly translate to a series of places.

*  External Routines Used:
*     cnf:
*        cnf_creim, cnf_exprt, cnf_free

*  References:
*     -  POSIX standard (1988), section 4.6.1
*     -  ANSI C standard (1989), section 4.10.4.4
      
*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     AJP: Alan Penny (Astrophysics, RAL )
*     {enter_new_authors_here}

*  History:
*     25-JAN-1991 (PMA):
*        Original version.
*     15-APR-1991 (PMA):
*        Changed calls to ems to calls to psx1.
*      3-MAY-1991 (PMA):
*        Ensure that the output argument is set, even on an error.
*     27-JUN-1991 (PMA):
*        Changed IMPORT and EXPORT macros to GENPTR.
*     5-NOV-1992 (AJP)
*        modified to be stand alone without PSX, or SAE
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/


/* Global Constants:		.					    */

#include <stdlib.h>		 /* Standard library			    */
#include <string.h>		 /* String handling library		    */

char* cnf_creim( char* source_f, int source_len );
void cnf_exprt( char* source_c, char* dest_f, int dest_len );
void cnf_free( char* temp );

void psx1_init_rtl ( void );

/*#include "f77.h"		 C - Fortran interface		    */
/* define C/FORTRAN interface for our needs
     PMA: Peter Allan (Starlink, RAL)
*/

#if !defined(F77_MACROS)

#define F77_MACROS

#define F77_EXTERNAL_NAME(X) X ## _
#define F77_INTEGER_TYPE   int 
#define F77_CHARACTER_TYPE char
#define F77_WORD_TYPE      short int
#define F77_SUBROUTINE(X)  void F77_EXTERNAL_NAME(X)
#define CHARACTER_RETURN_VALUE (X) CHARACTER(X) TRAIL(X)
#define INTEGER(X)     F77_INTEGER_TYPE *X
#define POINTER(X)     void **X
#define CHARACTER(X)             F77_CHARACTER_TYPE X[]
#define TRAIL(X)                 ,int X ## _length
#define GENPTR_INTEGER(X)
#define GENPTR_CHARACTER(X)

#if defined(vms)	/* vms				*/
#undef  F77_EXTERNAL_NAME
#define F77_EXTERNAL_NAME(X) X
#include <descrip.h>
#undef  CHARACTER
#define CHARACTER(X) struct dsc$descriptor_s *X/**/_arg
#undef  TRAIL
#define TRAIL(X)
#undef  GENPTR_CHARACTER
#define GENPTR_CHARACTER(X) \
   F77_CHARACTER_TYPE *X = X/**/_arg->dsc$a_pointer; \
   int X/**/_length = X/**/_arg->dsc$w_length;
#endif  

#if defined(sun)		/* sun4				*/
#endif  

#if defined(mips)		/* mips			    */
#if !defined(__STDC__)
#undef  F77_EXTERNAL_NAME
#define F77_EXTERNAL_NAME(X) X/**/_
#undef  TRAIL
#define TRAIL(X) ,int X/**/_length
#endif  /* of non ANSI redefinitions			    */
#endif  

#endif


F77_SUBROUTINE(genv)( CHARACTER(name),
                      CHARACTER(trans),
                      INTEGER(status)
                      TRAIL(name)
                      TRAIL(trans)
                          )
{

/* Pointers to Arguments:						    */

   GENPTR_CHARACTER(name)
   GENPTR_CHARACTER(trans)
   GENPTR_INTEGER(status)
   
/* Local Variables:							    */

   char *temp_name;		 /* Pointer to local copy of name 	    */
   char *ptr;			 /* Pointer to environment variable	    */

   *status = 0;

#if defined(vms)					/* If this is a VMS system, initialize */
   psx1_init_rtl();					/* the VAX C run time library. */
#endif

   temp_name = cnf_creim ( name, name_length );		/* Convert Fortran string to C string */
   ptr = getenv ( temp_name );				/* Get environment variable pointer */	

   if ( ptr != 0 )
      { cnf_exprt( ptr, trans, trans_length ) ;  	/* variable to the Fortran string trans. */
        cnf_free( temp_name );}				/* Free the temporary space.*/
   else
       *status = 1 ;					/* No translation found. */
}

#include <stdlib.h>		 /* Standard C run-time library		    */

char *cnf_creim( char *source_f, int source_len )

/*
*+
*  Name:
*     cnf_creim

*  Purpose:
*     Create a temporary C string and import a FORTRAN string into it

*  Language:
*     ANSI C

*  Invocation:
*     pointer = cnf_creim( source_f, source_len )

*  Description:
*     Create a temporary C string, import a FORTRAN string into it and
*     return a pointer to this C string.
*     Any trailing blanks in the FORTRAN string are discarded.
*     The length of the C string that is created is just long enough to
*     hold the FORTRAN string (less trailing blanks), plus the null
*     terminator.

*  Arguments:
*     char *source_f (Given)
*        A pointer to the input FORTRAN string
*     int source_len (Given)
*        The length of the input FORTRAN string

*  Returned Value:
*     char *cnf_creim
*        A pointer to the storage space allocated by this function.

*  Notes:
*     If the routine could not create the space, then it returns a null
*     pointer.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     26-MAR-1991 (PMA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
/* Local Variables:							    */

   int i;			 /* Loop counter			    */
   char *ptr;			 /* Pointer to storage allocated	    */


/* Locate the last non blank character in the input FORTRAN string.	    */

   for( i = source_len - 1 ; ( i >= 0 ) && ( source_f[i] == ' ' ) ; i-- )
      ;

/* Allocate enough space for a copy of the input string.		    */

   ptr = (char *)malloc( (size_t)( i + 2 ) );

/* If the space was allocated successfully, copy the input FORTRAN string   */
/* to it.								    */

   if( ptr != 0 )
   {
      ptr[i+1] = '\0';

      for(  ; i >= 0 ; i-- )
         ptr[i] = source_f[i];
   }

   return( ptr );
}

void cnf_exprt( char *source_c, char *dest_f, int dest_len )

/*
*+
*  Name:
*     cnf_exprt

*  Purpose:
*     Export a C string to a FORTRAN string

*  Language:
*     ANSI C

*  Invocation:
*     cnf_exprt( source_c, dest_f, dest_len )

*  Description:
*     Export a C string to a FORTRAN string. If the C string is
*     shorter than the space allocated to the FORTRAN string, then pad
*     it with blanks. If the C string is longer than the space
*     allocated to the FORTRAN string, then truncate the string.

*  Arguments:
*     char *source_c (Given)
*        A pointer to the input C string
*     char *dest_f (Returned via pointer)
*        A pointer to the output FORTRAN string
*     int dest_len (Given)
*        The length of the output FORTRAN string

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     27-MAR-1991 (PMA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
/* Local Variables:							    */

   int i;			 /* Loop counter			    */


/* Copy the characters of the input C string to the output FORTRAN string,  */
/* taking care not to go beyond the end of the FORTRAN string.		    */

   for( i = 0 ; (i < dest_len ) && ( source_c[i] != '\0' ) ; i++ )
      dest_f[i] = source_c[i];

/* Fill the rest of the output FORTRAN string with blanks.		    */

   for(  ; i < dest_len ; i++ )
      dest_f[i] = ' ';
}


#include <stdlib.h>		 /* Standard C run-time library		    */

void cnf_free( char *temp )

/*
*+
*  Name:
*     cnf_free

*  Purpose:
*     Return temporary space

*  Language:
*     ANSI C

*  Invocation:
*     cnf_free( temp )

*  Description:
*     Return temporary storage space that was allocated by a previous
*     call to cnf_creat, cnf_creib or cnf_creim.

*  Arguments:
*     char *temp (Given)
*        A pointer to the storage to be deallocated.

*  Notes:
*     -  The source code for this function is trivial, being merely a
*        call to the C run-time library routine, free(). However, it is
*        included for completeness.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     27-MAR-1991 (PMA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*...........................................................................*/

{
   free( (void *)temp );
}

void psx1_init_rtl( void )

/*
*+
*  Name:
*     psx1_init_rtl

*  Purpose:
*     Initialize the VAX C run time library.

*  Language:
*     ANSI C

*  Invocation:
*     psx1_init_rtl()

*  Description:
*     Initialize the VAX C run time library. This is needed on VMS for
*     PSX routines that get environment variables. On Unix (Sun and
*     DECstation), this routine has no effect.

*  Algorithm:
*     The status of the static variable init is checked to see if this
*     routine has already initialized the VAX C run time library. If it
*     has not, the routine tries to translate the environment variable
*     "USER".  If this is not possible, then the VAX C run time library
*     is initialized.

*  External Routines Used:
*     VAX C run time library:
*        vaxc$crtl_init

*  VMS-specific features used:
*     Although a VAX specific routine is called, that piece of code is
*     only included by the preprocessor on VMS systems, so the code is
*     portable.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     16-APR-1991 (PMA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
-----------------------------------------------------------------------------
*/

{

#if defined(vms)		 /* VMS specific code.			    */

/* Local Variables:							    */

   static int init = 0;          /* Is the run time library initialized?    */

/* External routines:							    */

   void vaxc$crtl_init( void );	 /* Function to initialize C run time
      				    library				    */

/* Has this routine initialized the library already?			    */

   if( init == 0 )

/* Test to see if something else has initialized the library.		    */

   {
      if( getenv( "USER" ) == 0 )

/* Initialize the VAX C run time library.				    */

      {
         vaxc$crtl_init();
         init = 1;
      }
   }

#endif				 /* End of VMS specific code.		    */
}
