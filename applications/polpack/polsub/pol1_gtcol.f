      SUBROUTINE POL1_GTCOL( CI, GNAME, REPORT, GI, STATUS )
*+
*  Name:
*     POL1_GTCOL

*  Purpose:
*     Get an identifier for a named pre-existing catalogue column.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL POL1_GTCOL( CI, GNAME, REPORT, GI, STATUS )

*  Description:
*     This a wrapper for CAT_TIDNT which gets an identifier for a named
*     pre-existing catalogue column. This wrapper translates the supplied
*     standard polpack column name (GNAME) into the actual name which the
*     uses wants to use for the column, before calling CAT_TIDNT.

*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue to which the column belongs.
*     GNAME  =  CHARACTER*(*) (Given)
*        The standard polpack name of the column.
*     REPORT = LOGICAL (Given)
*        If .TRUE., an error is reported if the column is not available.
*        Otherwise no error is reported.
*     GI  =  INTEGER (Returned)
*        Identifier for the column. The null identifier is returned
*        if the specified column is not available.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     13-DEC-2000 (DSB):
*        Original version.

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT constants.

*  Arguments Given:
      INTEGER CI
      CHARACTER GNAME*(*)
      LOGICAL REPORT

*  Arguments Returned:
      INTEGER GI

*  Status:
      INTEGER STATUS              ! Global status

*  Local Variables:
      CHARACTER ENAME*30          ! External column name

*.

*  Initialize.
      GI = CAT__NOID

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the extenal name to use for the column.
      CALL POL1_COLNM( GNAME, .FALSE., ENAME, STATUS )

*  If the user has not supplied a name for this column, report an error
*  if required.
      IF( ENAME .EQ. ' ' ) THEN
         IF( REPORT .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'N', GNAME )
            CALL ERR_REP( 'POL1_GTCOL_ERR1', 'A column name for '//
     :                    '^N values is required but is not '//
     :                    'specified in your polpack setup file '//
     :                    '($HOME/.polpackrc or $POLPACKRC).', STATUS )
            CALL ERR_REP( 'POL1_GTCOL_ERR1B', 'You could try deleting'//
     :                    ' or moving your polpack setup file and '//
     :                    'then re-running this application.', STATUS )

         END IF

*  Otherwise...
      ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Get the identifer for the column.
         CALL CAT_TIDNT( CI, ENAME, GI, STATUS )

*  If an error occurred, annul the error, and then re-report the error
*  if required.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            GI = CAT__NOID

            IF( REPORT ) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'N', ENAME )
               CALL ERR_REP( 'POL1_GTCOL_ERR2', 'A column named '//
     :                       '''^N'' is required but is not '//
     :                       'available in the catalogue.', STATUS )
            END IF

         END IF

      END IF

      END
