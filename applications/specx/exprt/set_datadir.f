      SUBROUTINE SET_DATADIR( IFAIL )
*+
*  Name:
*     SET_DATADIR

*  Purpose:
*     Set the DATADIR environment variable

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SET_DATADIR( IFAIL )

*  Description:
*     This routine prompts the user for a new data directory and
*     stores that value in the DATADIR environment variable.

*  Arguments:
*     IFAIL = INTEGER (Returned)
*        The global status. The status is reset on entry.

*  Authors:
*     timj: Tim Jenness (JAC, Hilo)
*     rpt:  Remo Tilanus (JAC, Hilo)

*  History:
*     03 Mar 2003 (timj)
*        First version
*     18 Mar 2003 (rpt)
*        Modified to use UTRNLOG and UPUTLOG

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      CHARACTER * ( 10 ) ENVVAR ! Name of DATA DIR envionment variable
      PARAMETER ( ENVVAR = 'DATADIR' )

*  External Functions:
      INTEGER GEN_ILEN

*  Global Variables:
      INCLUDE 'PSX_ERR'
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER IFAIL              ! Global status
      CHARACTER *( 132 ) CURDIR  ! Current data dir
      CHARACTER *( 132 ) DATADIR ! New Data dir
      INTEGER STATUS             ! Starlink status
      INTEGER CURLEN             ! Length of CURDIR
      CHARACTER *3 CURDIR_FMT    ! Format used for default string prompt
      INTEGER JDEF               ! Returned by GEN_GETSTR

*  Local Variables:


*.

*     We Seem to reset status on entry
      IFAIL = 0
      STATUS = SAI__OK

      CALL ERR_BEGIN( STATUS )

*     Get the current DATADIR to show the default
      CALL UTRNLOG( ENVVAR, CURDIR, STATUS )

*     Only proceed if we got the value okay
      IF ( STATUS .EQ. SAI__OK .OR. STATUS .EQ. PSX__NOENV) THEN

*         print *,'PSX STATUS ', STATUS

*     If we have a current value, use it as default

         IF (STATUS .EQ. SAI__OK ) THEN

*     Length of string
            CURLEN = GEN_ILEN( CURDIR )

*     Format for displayed default
            WRITE( CURDIR_FMT, '(''A'',I2.2)') CURLEN

         ELSE

*     Disable display of default value
            CURLEN = 0
            CURDIR_FMT = ' '

*            print *,'Annull status'
            CALL ERR_ANNUL ( STATUS )

         END IF

*     Prompt for string
         CALL GEN_GETSTR( 'New data directory? (use single quotes)',
     :        CURDIR(:CURLEN), CURDIR_FMT,
     :        DATADIR, JDEF)

*     Set the new value (if JDEF is zero)
         IF (JDEF .EQ. 0) THEN
            CALL UPUTLOG( ENVVAR, DATADIR, STATUS )
         ELSE
            PRINT *,'Value not modified'
         END IF

      END IF

*     Set IFAIL to bad if status is bad
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_FLUSH( STATUS )
         IFAIL = 18
      END IF

      CALL ERR_END( STATUS )

      END
