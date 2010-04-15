      SUBROUTINE CAP_NSAMP (CI, FREQ, REJCAT, SI, NUMSEL, SIR, NUMREJ,
     :  STATUS)
*+
*  Name:
*     CAP_NSAMP
*  Purpose:
*     Select every FREQth object in a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_NSAMP (CI, FREQ, REJCAT; SI, NUMSEL, SIR, NUMREJ;
*       STATUS)
*  Description:
*     Select every FREQth object in a catalogue.
*  Arguments:
*     CI  =  INTEGER (Given)
*        Identifier to the catalogue from which the selection is to be
*        created.  This identifier may correspond to either a genuine
*        catalogue, a selection or an index.
*     FREQ  =  INTEGER (Given)
*        Every FREQth entry in the input catalogue will be selected.
*     REJCAT  =  LOGICAL (Given)
*        Flag indicating whether a second optional selection of
*        rejected entries is also to be created.  It is coded as
*        follows:
*        .TRUE.  -  create selection of rejected objects,
*        .FALSE. -  do not create selection of rejected objects,
*     SI  =  INTEGER (Returned)
*        Identifier to list of selected objects.
*     NUMSEL  =  INTEGER (Returned)
*        Number of selected objects.
*     SIR  =  INTEGER (Returned)
*        Identifier to list of rejected objects.  If no list of rejected
*        objects has been specified it is returned set to null
*        (CAT__NOID).
*     NUMREJ  =  INTEGER (Returned)
*        Number of rejected objects.  If no list of rejected objects
*        has been specified it is returned set to zero.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the requested frequency is greater than 1 then
*       Determine the number of rows in the catalogue.
*       Determine the number of rows to be selected.
*       Create an array to hold the list of selected rows.
*       Create the list of selected rows.
*       Assemble the selection criterion.
*       Create the selection.
*       Release the workspace array.
*     else
*       Set the status.
*       set the return identifiers to null and the number of objects to
*       zero.
*     end if
*     Report any error.
*  Implementation Deficiencies:
*     <...>
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     13/6/96 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'CNF_PAR'           ! CNF functions
C      INCLUDE 'CAT_ERR'           ! CAT error codes.
C      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
C      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      INTEGER
     :  CI,
     :  FREQ
      LOGICAL
     :  REJCAT
*  Arguments Returned:
      INTEGER
     :  SI,
     :  NUMSEL,
     :  SIR,
     :  NUMREJ
*  Status:
      INTEGER STATUS             ! Global status
*  Local Variables:
      INTEGER
     :  ROWS,      ! Number of rows in the catalogue.
     :  LSTPTR,    ! Pointer to list of rows to be selected.
     :  LCRIT      ! Length of CRIT (excl. trail. blanks).
      CHARACTER
     :  CRIT*80    ! Description of selection criterion.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check that the requested frequency is greater than one.

         IF (FREQ .GT. 1) THEN

*
*          Determine the number of rows in the catalogue.

            CALL CAT_TROWS (CI, ROWS, STATUS)

*
*          Determine the number of rows to be selected.

            NUMSEL = ROWS / FREQ
            NUMSEL = MAX(NUMSEL, 1)

*
*          Create an array to hold the list of selected rows.

            CALL CAP_CRTAR (NUMSEL, '_INTEGER', LSTPTR, STATUS)

*
*          Create the list of row numbers to be selected.

            CALL CAP_CRNSP (FREQ, NUMSEL, %VAL(CNF_PVAL(LSTPTR)),
     :                      STATUS)

*
*          Assemble the selection criterion.

            CRIT = ' '
            LCRIT = 0

            CALL CHR_PUTC ('Select every ', CRIT, LCRIT)
            CALL CHR_PUTI (FREQ, CRIT, LCRIT)

            IF (FREQ .EQ. 2) THEN
               CALL CHR_PUTC ('nd', CRIT, LCRIT)
            ELSE IF (FREQ .EQ. 3) THEN
               CALL CHR_PUTC ('rd', CRIT, LCRIT)
            ELSE
               CALL CHR_PUTC ('th', CRIT, LCRIT)
            END IF

            CALL CHR_PUTC (' entry.', CRIT, LCRIT)

*
*          Create the selection.

            CALL CAT_SLIST (NUMSEL, %VAL(CNF_PVAL(LSTPTR)), CRIT,
     :        REJCAT, CI, SI, SIR, NUMREJ, STATUS)

*
*          Release the workspace array.

            CALL CAP_FREAR (LSTPTR, STATUS)

         ELSE

*
*          The given frequency is invalid: set the status, set the return
*          identifiers to null and the number of objects to zero.

            STATUS = SAI__ERROR

            SI = CAT__NOID
            NUMSEL = 0

            SIR = CAT__NOID
            NUMREJ = 0
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETI ('FREQ', FREQ)

            CALL ERR_REP ('CAP_SELECT_NSMP',
     :        'Failure selecting every Nth entry for N = ^FREQ.',
     :        STATUS)
         END IF

      END IF

      END
