      SUBROUTINE SCULIB_INSERT_BOL(NBOL, N_BOLS, N_POS, BOLDATA,
     :     BOLQUAL, SCUDATA, SCUQUAL, STATUS)
*+
*  Name:
*     SCULIB_INSERT_BOL

*  Purpose:
*     To insert a single bolometer into a data array

*  Invocation:
*     CALL SCULIB_INSERT_BOL(NBOL, N_BOLS, N_POS, BOLDATA,
*    :     BOLQUAL, SCUDATA, SCUQUAL, STATUS)

*  Description:
*     This routine inserts a single bolometer into a SCUBA data array.

*  Arguments:
*     NBOL = INTEGER (Given)
*        The bolometer number
*     N_BOLS = _INTEGER (Given)
*        Total number of bolometers in data array
*     N_POS = INTEGER (Given)
*        Number of jiggle positions in data set
*     BOLDATA(N_POS) = REAL (Given)
*        The specified bolometer data
*     BOLQUAL(N_POS) = _BYTE (Given)
*        The bolometer quality
*     SCUDATA(N_BOLS, N_POS) = REAL (Given & Returned)
*        The data
*     SCUQUAL(N_BOLS, N_POS) = BYTE (Given &  Returned)
*        The data quality
*     STATUS = INTEGER (Given and Returned)
*        Global Status value

*  Implementation Status:
*     Propogates quality

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1996 November 18 (TIMJ):
*       Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! VAL__ constants
      INCLUDE 'SAE_PAR'               ! SSE global definitions

*  Arguments Given:
      INTEGER NBOL
      INTEGER N_BOLS
      INTEGER N_POS
      REAL    BOLDATA(N_POS)
      BYTE    BOLQUAL(N_POS)

*  Arguments Given and Returned:
      REAL    SCUDATA(N_BOLS, N_POS)
      BYTE    SCUQUAL(N_BOLS, N_POS)

*  Status:
      INTEGER STATUS                 ! Global status

*  Local Variables:
      INTEGER I                    ! Loop counter
*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, N_POS
         SCUDATA(NBOL, I) = BOLDATA(I)
         SCUQUAL(NBOL, I) = BOLQUAL(I)
      END DO

      END
