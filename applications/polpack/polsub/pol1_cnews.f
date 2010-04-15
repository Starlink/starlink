      SUBROUTINE POL1_CNEWS( CI, FNAME, REPORT, DTYPE, CSIZE, UNITS,
     :                       EXTFMT, COMM, FI, STATUS)
*+
*  Name:
*     POL1_CNEWS

*  Purpose:
*     Create a catalogue column, simultaneously setting some of its
*     attributes.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL POL1_CNEWS( CI, FNAME, REPORT, DTYPE, CSIZE, UNITS, EXTFMT,
*                      COMM, FI, STATUS )

*  Description:
*     This is a wrapper for CAT_CNEWS which creates a column, simultaneously
*     setting some of its attributes. This wrapper translates the supplied
*     standard polpack column name (FNAME) into the actual name which the
*     uses wants to use for the column, before calling CAT_CNEWS.

*  Arguments:
*     CI = INTEGER (Given)
*        Catalogue identifier.
*     FNAME = CHARACTER*(*) (Given)
*        Name of the column (or field).
*     REPORT = LOGICAL (Given)
*        If .TRUE., an error is reported if the user does not want to use
*        the specified column. Otherwise no error is reported.
*     DTYPE = INTEGER (Given)
*        Type of the column.
*     CSIZE = INTEGER (Given)
*        Size of a CHARACTER column.  If the column is not of type
*        CHARACTER CSIZE is irrelevant; it is conventional to set it
*        to zero.
*     UNITS = CHARACTER*(*) (Given)
*        The units of the column.
*     EXTFMT = CHARACTER*(*) (Given)
*        The external format for the column.
*     COMM = CHARACTER*(*) (Given)
*        Comments about the column.
*     FI = INTEGER (Returned)
*        Identifier for the column.
*     STATUS = INTEGER (Given and Returned)
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
      INCLUDE 'SAE_PAR'           ! Standard SAE constants.
      INCLUDE 'CAT_PAR'           ! CAT constants.

*  Arguments Given:
      INTEGER CI
      INTEGER DTYPE
      LOGICAL REPORT
      INTEGER CSIZE
      CHARACTER FNAME*(*)
      CHARACTER UNITS*(*)
      CHARACTER EXTFMT*(*)
      CHARACTER COMM*(*)

*  Arguments Returned:
      INTEGER FI

*  Status:
      INTEGER STATUS             ! Global status.

*  Local Variables:
      CHARACTER ENAME*30         ! External column name

*.

*  Initialise
      FI = CAT__NOID

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the extenal name to use for the column.
      CALL POL1_COLNM( FNAME, .FALSE., ENAME, STATUS )

*  If the user has not supplied a name for this column, report an error
*  if required.
      IF( ENAME .EQ. ' ' ) THEN
         IF( REPORT .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'N', FNAME )
            CALL ERR_REP( 'POL1_CNEWS_ERR1', 'A column name for '//
     :                    '^N values is required but is not '//
     :                    'specified in your polpack setup file '//
     :                    '($HOME/.polpackrc or $POLPACKRC).', STATUS )
            CALL ERR_REP( 'POL1_CNEWS_ERR1B', 'You could try deleting'//
     :                    ' or moving your polpack setup file and '//
     :                    'then re-running this application.', STATUS )
         END IF

*  Otherwise, create the column.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         CALL CAT_CNEWS( CI, ENAME, DTYPE, CSIZE, UNITS, EXTFMT, COMM,
     :                   FI, STATUS)
      END IF

      END
