      SUBROUTINE CAT1_GTSRW (NUMSEL, SELIST, ROWNO, CATROW, STATUS)
*+
*  Name:
*     CAT1_GTSRW
*  Purpose:
*     Get the catalogue row number from a selection.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT1_GTSRW (NUMSEL, SELIST, ROWNO; CATROW; STATUS)
*  Description:
*     Given a row number in a selection, extract the corresponding
*     catalogue row number from the selection list.
*  Arguments:
*     NUMSEL  =  INTEGER (Given)
*        Number of rows in the selection.
*     SELIST(NUMSEL)  =  INTEGER (Given)
*        The selection list; the list of catalogue rows which comprise
*        the selection.
*     ROWNO  =  INTEGER (Given)
*        Selection row number.
*     CATROW  =  INTEGER (Returned)
*        Corresponding catalogue row number.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Copy the catalogue row number from the appropriate element of the
*     selection list.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     14/4/94 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      INTEGER
     :  NUMSEL,
     :  SELIST(NUMSEL),
     :  ROWNO
*  Arguments Returned:
      INTEGER
     :  CATROW
*  Status:
      INTEGER STATUS             ! Global status.
*.

      IF (STATUS .EQ. CAT__OK) THEN

         CATROW = SELIST(ROWNO)

      END IF

      END
