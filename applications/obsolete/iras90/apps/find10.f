      SUBROUTINE FIND10( SOPOS, STNAME, STATUS )
*+
*  Name:
*     FIND10

*  Purpose:
*     Generates a string containing the source name and coordinate
*     system

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND10( SOPOS, STNAME, STATUS )

*  Description:
*     Generates a string containing the source name and coordinate
*     system

*  Arguments:
*     SOPOS = INTEGER (Given)
*        Pointer to the source currently being processed
*     STNAME = CHARACTER * ( * ) (Returned)
*        String to contain source name and coordinate system
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     None

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1992 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      INTEGER SOPOS

*  Arguments Returned:
      CHARACTER * ( * ) STNAME

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the source name into the begining of the variable STNAME
      STNAME(1:8) = SONAME(SOPOS)
      STNAME(9:11) = '   '

*  Set up an appropriate coordinate system string
*
*  Equatorial
      IF      ( ( SOCOSY(SOPOS) .EQ. 'EQUATORIAL(B1950)        ')
     :   .OR.   ( SOCOSY(SOPOS) .EQ. 'EQUATORIAL(J1950)        ') ) THEN
         STNAME(12:16) = 'Eq''50'
      ELSE IF ( (SOCOSY(SOPOS) .EQ. 'EQUATORIAL(B2000)         ')
     :   .OR.   (SOCOSY(SOPOS) .EQ. 'EQUATORIAL(J2000)         ') ) THEN
         STNAME(12:16) = 'Eq20'''
      ELSE IF   (SOCOSY(SOPOS)(1:10) .EQ. 'EQUATORIAL')             THEN
         STNAME(12:16) = 'Eq???'
*  Ecliptic
      ELSE IF ( ( SOCOSY(SOPOS) .EQ. 'ECLIPTIC(B1950)          ')
     :   .OR.   ( SOCOSY(SOPOS) .EQ. 'ECLIPTIC(J1950)          ') ) THEN
         STNAME(12:16) = 'Ec''50'
      ELSE IF ( (SOCOSY(SOPOS) .EQ. 'ECLIPTIC(B2000)           ')
     :   .OR.   (SOCOSY(SOPOS) .EQ. 'ECLIPTIC(J2000)           ') ) THEN
         STNAME(12:16) = 'Ec20'''
      ELSE IF   (SOCOSY(SOPOS)(1:8) .EQ. 'ECLIPTIC')               THEN
         STNAME(12:16) = 'Ec???'
*  Galactic
      ELSE IF ( SOCOSY(SOPOS)(1:8) .EQ. 'GALACTIC' ) THEN
         STNAME(12:16) = 'Galac'
      ELSE
*  Unknown
         STNAME(12:16) = '?????'
      END IF

      END
