      SUBROUTINE SCULIB_EXTRACT_BOL(NBOL, N_BOLS, N_POS, SCUDATA,
     :     SCUQUAL, BOLDATA, BOLQUAL, STATUS)
*+
*  Name:
*     SCULIB_EXTRACT_BOL

*  Purpose:
*     To extract a single bolometer from a data array

*  Invocation:
*     CALL SCULIB_EXTRACT_BOL(NBOL, N_BOLS, N_POS, SCUDATA,
*    :     SCUQUAL, BOLDATA, BOLQUAL, STATUS)

*  Description:
*     This routine extracts a single bolometer from a SCUBA data array.

*  Arguments:
*     NBOL = INTEGER (Given)
*        The bolometer number
*     N_BOLS = _INTEGER (Given)
*        Total number of bolometers in data array
*     N_POS = INTEGER (Given)
*        Number of jiggle positions in data set
*     SCUDATA(N_BOLS, N_POS) = REAL (Given)
*        The data
*     SCUQUAL(N_BOLS, N_POS) = BYTE (Given)
*        The data quality
*     BOLDATA(N_POS) = REAL (Returned)
*        The specified bolometer data
*     BOLQUAL(N_POS) = _BYTE (Returned)
*        The bolometer quality
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
      REAL    SCUDATA(N_BOLS, N_POS)
      BYTE    SCUQUAL(N_BOLS, N_POS)

*  Arguments Returned:
      REAL    BOLDATA(N_POS)
      BYTE    BOLQUAL(N_POS)

*  Status:
      INTEGER STATUS                 ! Global status

*  Local Variables:
      INTEGER I                    ! Loop counter
*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, N_POS
         BOLDATA(I) = SCUDATA(NBOL, I)
         BOLQUAL(I) = SCUQUAL(NBOL, I)
      END DO

      END
