      SUBROUTINE TPU$CALLUSER( I, STR1, STR2 )
*+
*  Name:
*     TPU$CALLUSER

*  Purpose:
*     Return process context information to LSE.

*  Language:
*     VAX Fortran

*  Type of module:
*     SUBROUTINE

*  Description:
*     This is a CALL_USER service routine for VAXTPU.  It returns
*     information about the process in which it runs, thereby making
*     this available to LSE.

*  Arguments:
*     I = INTEGER (Given)
*        The integer argument passed from VAXTPU.  It is not used by
*        this routine.
*     STR1 = CHARACTER * ( * ) (Given)
*        The string argument passed by TPU.  This may be either of the
*        following:
*           'DEFAULT_DIR' => The routine returns the name of the
*              current default directory for the parent process by
*              translating the job logical name
*              STARLSE$DEFAULT_DIR_xxx, where xxx is the current
*              process name.  If this logical name does not exist, then
*              a single blank character is returned instead.
*           'DETACH_KEY' => The routine returns the name of the "detach
*              key" which is used to detach the terminal from LSE by
*              translating the job logical name STARLSE$ATTACH_KEY_xxx,
*              where xxx is the current process name. If this logical
*              name does not exist, then a single blank character is
*              returned instead.
*           'USER_AFFILIATION' => The routine returns the user's
*              insitutional affiliation by translating the logical name
*              STARLSE$PERSONAL_AFFILIATION. If this logical name does
*              not exist, then a single blank character is returned
*              instead.
*           'USER_ID' => The routine returns the user ID of the current
*              process (or a user-defined replacement for it, obtained
*              by translating the logical name
*              STARLSE$PERSONAL_USERID).
*           'USER_NAME' => The routine returns the user's personal name
*              by translating the logical name STARLSE$PERSONAL_NAME.
*              If this logical name does not exist, then a single blank
*              character is returned instead.
*     STR2 = INTEGER( 2 ) (Returned)
*        The routine fills in this character string descriptor argument
*        so that it describes a dynamically allocated string containing
*        the character sequence to be returned to VAXTPU.

*  Notes:
*    -  This routine should be linked to produce a shareable image, to
*    which the logical name TPU$CALLUSER must be assigned.

*  VAX-specific features used:
*     -  This routine is designed for VAX use only, in conjunction with
*     VAXTPU, and is not intended to be portable.

*  Authors:
*     RFWS: R.F.Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-FEB-1989 (RFWS):
*        Original version.
*     27-FEB-1989 (RFWS):
*        Added personal name facility.
*     6-APR-1989 (RFWS):
*        Added STARLSE$PERSONAL_USERID logical name facility and
*        changed ADAMLSE$... to STARLSE$... throughout.  Also updated
*        the prologue layout.
*     14-AUG-1989 (RFWS):
*        Changed to allocate the returned string dynamically and fill in
*        its descriptor as required by LSE version 2.3-96.
*     13-SEP-1990 (RFWS):
*        Added translation of the STARLSE$PERSONAL_AFFILIATION logical
*        name to give the user's institutional affiliation.
*     27-NOV-1990 (RFWS):
*        Changed to use the LNM$FILE_DEV logical name table for
*        translating logical names, rather than just the process
*        logical name.
*     22-AUG-1991 (RFWS):
*        Added translation of the STARLSE$ATTACH_KEY_xxx job logical
*        name to determine the name of the "detach key" to be used for
*        detaching the terminal from LSE.
*     22-AUG-1991 (RFWS):
*        Added translation of the STARLSE$DEFAULT_DIR__xxx job logical
*        name to determine the name of the parent process' default
*        directory.
*     {enter_further_changes_here}

*  Bugs:
*     {enter_new_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global constants:
      INCLUDE '($JPIDEF)'        ! Item codes for LIB$GETJPI
      INCLUDE '($LNMDEF)'        ! Item codes for SYS$TRNLNM

*  Given:
      INTEGER I
      CHARACTER * ( * ) STR1

*  Returned:
      INTEGER STR2( 2 )

*  Local variables:
      CHARACTER * ( 15 ) PRCNAM  ! Process name
      CHARACTER * ( 255 ) CHARS  ! Returned character buffer
      INTEGER * 2 BUFLEN         ! Return buffer length
      INTEGER * 2 ITEM           ! Item code
      INTEGER * 2 ITMLST( 8 )    ! GETJPI item list
      INTEGER * 2 LENGTH         ! String length
      INTEGER EOL                ! End of list
      INTEGER NC                 ! Number of characters
      INTEGER RETAD              ! Return buffer address
      INTEGER RETLAD             ! Returned length address

      EQUIVALENCE ( BUFLEN, ITMLST( 1 ) ) ! Set up item list fields...
      EQUIVALENCE ( ITEM, ITMLST( 2 ) )
      EQUIVALENCE ( RETAD, ITMLST( 3 ) )
      EQUIVALENCE ( RETLAD, ITMLST( 5 ) )
      EQUIVALENCE ( EOL, ITMLST( 7 ) )

*.

*   Initialise the returned character string.
      CHARS = ' '
      NC = 0

*   If the name of the parent process default directory has been
*   requested, set up the item list for SYS$TRNLNM.
      IF ( STR1 .EQ. 'DEFAULT_DIR' ) THEN
         BUFLEN = LEN( CHARS )
         ITEM = LNM$_STRING
         RETAD = %LOC( CHARS )
         RETLAD = %LOC( NC )
         EOL = 0

*  Obtain the current process name and translate the logical name
*  STARLSE$DEFAULT_DIR_xxx. Note that this must be a job logical name.
         CALL LIB$GETJPI( JPI$_PRCNAM, , , , PRCNAM, NC )
         CALL SYS$TRNLNM( , 'LNM$JOB',
     :                    'STARLSE$DEFAULT_DIR_' // PRCNAM( : NC ), ,
     :                    ITMLST ) 

*   If the name of the "detach key" has been requested, set up the item
*   list for SYS$TRNLNM.
      ELSE IF ( STR1 .EQ. 'DETACH_KEY' ) THEN
         BUFLEN = LEN( CHARS )
         ITEM = LNM$_STRING
         RETAD = %LOC( CHARS )
         RETLAD = %LOC( NC )
         EOL = 0

*  Obtain the current process name and translate the logical name
*  STARLSE$ATTACH_KEY_xxx. Note that this must be a job logical name.
         CALL LIB$GETJPI( JPI$_PRCNAM, , , , PRCNAM, NC )
         CALL SYS$TRNLNM( , 'LNM$JOB',
     :                    'STARLSE$ATTACH_KEY_' // PRCNAM( : NC ), ,
     :                    ITMLST ) 

*   If the user's institutional affiliation has been requested, set up
*   the item list for SYS$TRNLNM.
      ELSE IF ( STR1 .EQ. 'USER_AFFILIATION' ) THEN
         BUFLEN = LEN( CHARS )
         ITEM = LNM$_STRING
         RETAD = %LOC( CHARS )
         RETLAD = %LOC( NC )
         EOL = 0

*  Translate the logical name STARLSE$PERSONAL_AFFILIATION.
         CALL SYS$TRNLNM( , 'LNM$FILE_DEV',
     :                      'STARLSE$PERSONAL_AFFILIATION', , ITMLST ) 

*   If the user ID has been requested, set up an item list for
*   SYS$TRNLNM in order to translate the logical name which may be used
*   to specify a personal replacement for it.
      ELSE IF ( STR1 .EQ. 'USER_ID' ) THEN
         BUFLEN = LEN( CHARS )
         ITEM = LNM$_STRING
         RETAD = %LOC( CHARS )
         RETLAD = %LOC( NC )
         EOL = 0

*  Translate the logical name STARLSE$PERSONAL_USERID.
         CALL SYS$TRNLNM( , 'LNM$FILE_DEV',
     :                      'STARLSE$PERSONAL_USERID', , ITMLST ) 

*  If the logical name was not defined, call LIB$GETJPI to obtain the
*  process user ID instead.
         IF ( CHARS .EQ. ' ' ) CALL LIB$GETJPI( JPI$_USERNAME, , , ,
     :                                          CHARS, NC )

*   If the user's personal name has been requested, set up the item
*   list for SYS$TRNLNM.
      ELSE IF ( STR1 .EQ. 'USER_NAME' ) THEN
         BUFLEN = LEN( CHARS )
         ITEM = LNM$_STRING
         RETAD = %LOC( CHARS )
         RETLAD = %LOC( NC )
         EOL = 0

*      Translate the logical name STARLSE$PERSONAL_NAME.
         CALL SYS$TRNLNM( , 'LNM$FILE_DEV',
     :                      'STARLSE$PERSONAL_NAME', , ITMLST ) 
      ENDIF

*   Remove trailing blanks from the string to be returned.
      DO LENGTH = NC, 1, -1
         IF( CHARS( LENGTH : LENGTH ) .NE. ' ' ) GO TO 1
      ENDDO
    1 CONTINUE
      LENGTH = MAX( LENGTH, 1 )

*   Copy the result to a dynamic string.
      CALL LIB$SCOPY_DXDX( CHARS( : LENGTH ), STR2 )

      END
