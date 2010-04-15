      SUBROUTINE IFLDZ1 (PLATE, DEC, BETA, HCON, DISK, REJECT,
     *                   STATUS)

*+

*  Name:
*     IFLDZ1

*  Purpose:
*     Determines the best ISSA plate for a given coordinate

*  Language:
*     Starlink Fortran 77

*  Type of module:
*     Subroutine to an ADAM A-task

*  Invocation:
*     CALL IFLDZ1 (PLATE, DEC, BETA, HCON, DISK, REJECT, STATUS)

*  Description:
*       This routine takes a plate number and return the disk(s) on
*       which that plate may be found. The latitude in both Ecliptic
*       and Equatorial, and the Hcon are also needed.

*  Notes:
*       This routine was written from the information contained in the
*       supplementary booklet for the ISSA-II release.

*  Parameters:
*     PLATE = INTEGER (Read)
*       The plate number of the desired plate
*
*     DEC = DOUBLE (Read)
*       The latitude in equatorial degrees of the co-ordinate
*       used to determine the plate
*
*     BETA = DOUBLE (Read)
*       The latitude in ecliptic degrees
*
*     HCON = INTEGER (Read)
*       The Hcon of the desired plate
*
*     DISK = INTEGER(2) (Write)
*       The disk(s) which the plate may be found on
*
*     REJECT = LOGICAL (Write)
*       Whether the plate comes from the reject directory
*
*     STATUS = INTEGER (Read and Write)
*       The global status

*  Authors:
*     HM: Huw Morris (IPMAF)

*  History:
*     07-MAR-1994:
*       Original version.
*     30-NOV-1994:
*       Added release II information

*  Bugs:
*     {note_any_bugs_here}

*-

*  No implicit typing
      IMPLICIT NONE

*  Include various constants used by ISFIELD
      INCLUDE 'IRA_PAR'
      INCLUDE 'IRA_ERR'
      INCLUDE 'SAE_PAR'


c  Parameters
      DOUBLEPRECISION DEC, BETA


      INTEGER HCON           ! Hcon of the desired plate
      INTEGER PLATE          ! The desired plate number
      INTEGER DISK(2)        ! The disk on which the plate may be found
      LOGICAL REJECT         ! Whether the plate is a reject or not

      INTEGER STATUS         ! Global status


c  Check global status
      IF (STATUS .NE. SAI__OK) GOTO 999


      DISK(1) = -1
      DISK(2) = -1

c  Work out whether the plate is a reject or not
      IF (  (PLATE .GE.  85 .AND. PLATE .LE.  90)  .OR.
     *      (PLATE .GE. 112 .AND. PLATE .LE. 124)  .OR.
     *      (PLATE .EQ. 127)                       .OR.
     *      (PLATE .GE. 144 .AND. PLATE .LE. 165)  .OR.
     *      (PLATE .GE. 177 .AND. PLATE .LE. 204)  .OR.
     *      (PLATE .GE. 210 .AND. PLATE .LE. 222)  .OR.
     *      (PLATE .GE. 228 .AND. PLATE .LE. 255)  .OR.
     *      (PLATE .GE. 268 .AND. PLATE .LE. 288)  .OR.
     *      (PLATE .EQ. 304)                       .OR.
     *      (PLATE .GE. 307 .AND. PLATE .LE. 319)  .OR.
     *      (PLATE .GE. 342 .AND .PLATE .LE. 346) ) THEN

        REJECT = .TRUE.
c  Calculate the disk number
        IF (DEC .LT. -20.0) THEN
          DISK(1) = 7
        ELSE IF (DEC .LT. 20.0) THEN
          DISK(1) = 8
        ELSE
          DISK(1) = 9
        ENDIF

      ELSE

        REJECT = .FALSE.

        IF (BETA .LT. -20.0 .AND. BETA .GT. -50.0) THEN
          DISK(1) = 5
        ELSE IF (BETA .GT. 20.0 .AND. BETA .LT. 50.0) THEN
          DISK(1) = 6
        ELSE
          IF (BETA .LE. 50.0 .AND. BETA .GE. -50.0) THEN
c  If we reach here, an error has occured, as the plate should have been
c  flagged as a reject
            DISK(1) = - 1
            STATUS = SAI__ERROR
            CALL ERR_REP(' ','Error calculating disk', STATUS)
            GOTO 999
          ELSE
            IF (PLATE .LE. 71) THEN
              DISK(1) = 2
            ELSE IF (PLATE .LE. 363) THEN
              DISK(1) = 3
            ELSE
              DISK(1) = 4
            ENDIF
          ENDIF
        ENDIF
      ENDIF

c  Calculate the disk number
      IF (HCON .EQ. 0) THEN

        IF (PLATE .LE. 197) THEN
c  Southern sky
          DISK(2) = 0
        ELSE
c  Northern sky
          DISK(2) = 1
        ENDIF
      ENDIF

 999  CONTINUE

      END
