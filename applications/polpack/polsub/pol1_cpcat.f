      SUBROUTINE POL1_CPCAT( CIIN, CIOUT, TWCS, STATUS )
*+
*  Name:
*     POL1_CPCAT

*  Purpose:
*     Copy a catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPCAT( CIIN, CIOUT, TWCS, STATUS )

*  Description:
*     This routine copies an entire catalogue, excluding the textual
*     information, appending the rows to the end of any rows already
*     present in the supplied output catalogue.
*
*     If, on entry, the output catalogue contains no columns, then
*     columns are created corresponding to each column in the input
*     catalogue, and parameter values are copied from the input catalogue.
*     If the catalogue already contains one or more columns on entry, an
*     error is reported if there is any difference between the columns or
*     parameters in the input and output catalogues.

*  Arguments:
*     CIIN = INTEGER (Given)
*        The CAT identifier for the input catalogue.
*     CIOUT = INTEGER (Given)
*        The CAT identifier for the output catalogue.
*     TWCS = INTEGER (Given)
*        An identifier for an AST FrameSet, or AST__NULL. If not AST__NULL,
*        and if the input catalogue contains columns named "X" and "Y",
*        and if the input catalogue contains a WCS FrameSet, the input
*        X and Y values will be transformed so that they refer to
*        equivalent sky positions within the PIXEL frame in the supplied
*        TWCS FrameSet.
*
*        In addition, if TWCS is not AST__NULL, and if the input catalogue
*        contains columns named "Q", "U" and "ANG", and if the input catalogue
*        contains a WCS FrameSet, the input Q, U and ANG values (together
*        with their errors) will be transformed so that they refer to the
*        reference direction specified by the POLANAL Frame in the TWCS
*        FrameSet.
*
*        If AST__NULL is supplied for TWCS, all values are copied from
*        input to output without any change.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.

*  Authors:
*     DSB: David S. Berry (EAO)
*     ACD: A C Davenhall (Edinburgh)

*  History:
*     29-JUN-2017 (DSB):
*        Original version, based on cap_cpcat.f by ACD.
*     28-SEP-2017 (DSB):
*        Added argument TWCS, and allow input catalogue to be appended to
*        the end of the excisting output catalogue.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'CAT_PAR'         ! CAT constants

*  Arguments Given:
      INTEGER CIIN
      INTEGER CIOUT
      INTEGER TWCS

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      DOUBLE PRECISION DATE     ! Creation date of catalogue
      INTEGER CIP               ! Identifier for parent input catalogue
      INTEGER FIIN(CAT__MXCOL)  ! Column identifiers for input  catalogue.
      INTEGER FIOUT(CAT__MXCOL) !   "         "       "  output     "    .
      INTEGER IDTYPE            ! Type of input catalogue identifier.
      INTEGER NCOL              ! Number of columns in output catalogue
      INTEGER NIND              ! Number of indicies in output catalogue
      INTEGER NPAR              ! Number of parameters in output catalogue
      INTEGER NROW              ! Number of rows in output catalogue
      INTEGER NUMCOL            ! Number of columns in the input catalogue.
      LOGICAL REPORT            ! Report differences?
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the given input catalogue is not a genuine catalogue (for
*  example, it is a selection) then get the identifier for the
*  corresponding parent catalogue.
      CALL CAT_TIDTP( CIIN, IDTYPE, STATUS )
      IF (IDTYPE .NE. CAT__CITYP) THEN
         CALL CAT_TIDPR( CIIN, CIP, STATUS )
      ELSE
         CIP = CIIN
      END IF

*  See how many columns and rows there are in the supplied output
*  catalogue.
      CALL CAT_TDETL( CIOUT, CAT__GPHYS, NROW, NCOL, NIND, NPAR, DATE,
     :                STATUS)

*  If there are any columns already in the output catalogue, indicate that
*  any differences between input and output columns/parameters should be
*  cause an error.
      REPORT = ( NROW .GT. 0 )

*  Copy information to the output catalogue.  First ensure the output has
*  columns corresponding to the input catalogue, then ensure the output
*  catalogue contains the same parameters as the input, then append the
*  table data to the end of the output catalogue.
      CALL POL1_CPCOL( CIP, CIOUT, CAT__MXCOL, REPORT, NUMCOL, FIIN,
     :                 FIOUT, STATUS)
      CALL POL1_CPPAR( CIP, CIOUT, REPORT, STATUS )
      CALL POL1_CPTAB( CIIN, CIOUT, NUMCOL, FIIN, FIOUT, TWCS, STATUS )

      END
