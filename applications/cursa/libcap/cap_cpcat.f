      SUBROUTINE CAP_CPCAT (CIIN, CIOUT, TEXT, STATUS)
*+
*  Name:
*     CAP_CPCAT
*  Purpose:
*     Copy a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CPCAT (CIIN, CIOUT, TEXT; STATUS)
*  Description:
*     Copy a catalogue.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue or selection.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     TEXT  =  CHARACTER*(*) (Given)
*        Flag indicating what textual information is to be copied,
*        coded as follows;
*        A - all (that is, generate a complete copy of the original
*            header as comments),
*        C - just copy the genuine comments (that is, COMMENTS and
*            HISTORY keywords in the case of FITS tables).
*        N - none.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Get the parent catalogue identifier for the input catalogue or
*     selection.
*     Create columns in the output catalogue.
*     Create parameters in the output catalogue.
*     Copy the table to the output catalogue.
*     Create text in the output catalogue.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     24/9/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
*  Arguments Given:
      INTEGER
     :  CIIN,
     :  CIOUT
      CHARACTER
     :  TEXT*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE,     ! Type of input catalogue identifier.
     :  CIP,        ! Identifier for parent input catalogue.
     :  NUMCOL,     ! Number of columns in the input catalogue.
     :  FIIN(CAT__MXCOL),   ! Column identifiers for input  catalogue.
     :  FIOUT(CAT__MXCOL)   !   "         "       "  output     "    .
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       If the given input catalogue is not a genuine catalogue (for
*       example, it is a selection) then get the identifier for the
*       corresponding parent catalogue.

         CALL CAT_TIDTP (CIIN, IDTYPE, STATUS)

         IF (IDTYPE .NE. CAT__CITYP) THEN
            CALL CAT_TIDPR (CIIN, CIP, STATUS)
         ELSE
            CIP = CIIN
         END IF

*
*       Create the output catalogue.  First create columns corresponding
*       to the input catalogue, then copy the parameters, followed by
*       the table of values.  Finally copy any header text which is
*       required.

         CALL CAP_CPCOL (CIP, CIOUT, CAT__MXCOL, NUMCOL, FIIN, FIOUT,
     :     STATUS)

         CALL CAP_CPPAR (CIP, CIOUT, STATUS)

         CALL CAP_CPTAB (CIIN, CIOUT, NUMCOL, FIIN, FIOUT, STATUS)

         IF (TEXT(1 : 1) .NE. 'N') THEN
            CALL CAP_CPTXT (CIP, CIOUT, TEXT(1 : 1), STATUS)
         END IF

      END IF

      END
