      SUBROUTINE CAP_CPTRG (CIIN, CIOUT, RAC, DECC, PMRAC, PMDEC,
     :  PLXC, RVC, LABELC, EQUINX, EPOCH, TEXT, STATUS)
*+
*  Name:
*     CAP_CPTRG
*  Purpose:
*     Copy a catalogue as a target list.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_CPTRG (CIIN, CIOUT, RAC, DECC, PMRAC, PMDEC, PLXC, RVC,
*       LABELC, EQUINX, EPOCH, TEXT; STATUS)
*  Description:
*     Copy a catalogue as a target list.
*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier for the input catalogue or selection.
*     CIOUT  =  INTEGER (Given)
*        Identifier for the output catalogue.
*     RAC  =  CHARACTER*(*) (Given)
*        Name of Right Ascension column.
*     DECC  =  CHARACTER*(*) (Given)
*        Name of Declination column.
*     PMRAC  =  CHARACTER*(*) (Given)
*        Name of proper motion in Right Ascension column.
*     PMDEC  =  CHARACTER*(*) (Given)
*        Name of proper motion in Declination column.
*     PLXC  =  CHARACTER*(*) (Given)
*        Name of parallax column.
*     RVC  =  CHARACTER*(*) (Given)
*        Name of radial velocity column.
*     LABELC  =  CHARACTER*(*) (Given)
*        Name of label column.
*     EQUINX  =  CHARACTER*(*) (Given)
*        Equinox of the coordinates.
*     EPOCH  =  CHARACTER*(*) (Given)
*        Epoch of the coordinates.
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
*     As required, create parameters for
*       the equinox and epoch,
*       the special columns of the target list.
*     end as
*     Copy the reminaing parameters to the output catalogue.
*     Copy the table to the output catalogue.
*     As required create text in the output catalogue.
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
     :  RAC*(*),
     :  DECC*(*),
     :  PMRAC*(*),
     :  PMDEC*(*),
     :  PLXC*(*),
     :  RVC*(*),
     :  LABELC*(*),
     :  EQUINX*(*),
     :  EPOCH*(*),
     :  TEXT*(*)
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  IDTYPE,     ! Type of input catalogue identifier.
     :  CIP,        ! Identifier for parent input catalogue.
     :  NUMCOL,     ! Number of columns in the input catalogue.
     :  FIIN(CAT__MXCOL),   ! Column identifiers for input  catalogue.
     :  FIOUT(CAT__MXCOL),  !   "         "       "  output     "    .
     :  QI          ! Parameter identifier.
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
*       Create the columns in the output target list.

         CALL CAP_CPTGC (CIP, CIOUT, RAC, DECC, CAT__MXCOL, NUMCOL,
     :     FIIN, FIOUT, STATUS)

*
*       As and if required create parameters for the equinox and
*       epoch and parameters to hold the names of the special columns.

         IF (EQUINX .NE. ' ') THEN
            CALL CAT_PPTSC (CIP, 'EQUINOX', EQUINX, 'Equinox of '/
     :        /'the coordinates.', QI, STATUS)
         END IF

         IF (EPOCH .NE. ' ') THEN
            CALL CAT_PPTSC (CIP, 'EPOCH', EPOCH, 'Epoch of '/
     :        /'the coordinates.', QI, STATUS)
         END IF

         IF (PMRAC .NE. 'none') THEN
            CALL CAT_PPTSC (CIP, 'PMRA_C', PMRAC, ' ', QI, STATUS)
         END IF

         IF (PMDEC .NE. 'none') THEN
            CALL CAT_PPTSC (CIP, 'PMDE_C', PMDEC, ' ', QI, STATUS)
         END IF

         IF (PLXC .NE. 'none') THEN
            CALL CAT_PPTSC (CIP, 'PLX_C', PLXC, ' ', QI, STATUS)
         END IF

         IF (RVC .NE. 'none') THEN
            CALL CAT_PPTSC (CIP, 'RV_C', RVC, ' ', QI, STATUS)
         END IF

         IF (LABELC .NE. 'none') THEN
            CALL CAT_PPTSC (CIP, 'LABEL_C', LABELC, ' ', QI, STATUS)
         END IF

*
*       Copy the remaining parameters from the input catalogue.

         CALL CAP_CPPAR (CIP, CIOUT, STATUS)

*
*       Copy the table of values.

         CALL CAP_CPTAB (CIIN, CIOUT, NUMCOL, FIIN, FIOUT, STATUS)

*
*       Copy the textual information as required.

         IF (TEXT(1 : 1) .NE. 'N') THEN
            CALL CAP_CPTXT (CIP, CIOUT, TEXT(1 : 1), STATUS)
         END IF

      END IF

      END
