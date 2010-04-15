      SUBROUTINE CAP_GSCAT (CATOUT, CFLAG, TFLAG, COMM, STATUS)
*+
*  Name:
*     CAP_GSCAT
*  Purpose:
*     Save the current selection as a catalogue.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSCAT (CATOUT, CFLAG, TFLAG, COMM; STATUS)
*  Description:
*     Save the current selection as a catalogue.
*  Arguments:
*     CATOUT  =  CHARACTER*(*) (Given)
*        The name of the output catalogue.
*     CFLAG  =  LOGICAL (Given)
*        Flag indicating the columns which are to be copied.  It is
*        coded as follows:
*        .TRUE.  -  copy all columns in the catalogue,
*        .FALSE. -  copy the currently chosen catalogues.
*     TFLAG  =  LOGICAL (Given)
*        Flag indicating whether header text in the original catalogue
*        is to be copied to the new catalogue.  It is coded as follows:
*        .TRUE.  -  copy the header text,
*        .FALSE. -  do not copy the header text.
*     COMM  =  CHARACTER*(*) (Given)
*        An optional line of comments to be added to the catalogue.
*        The line is only added if it is non-blank.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the catalogue is open then
*       Attempt to get an identifier for the output catalogue.
*       If ok then
*         If all the columns are to be copied then
*           Create copies of all the columns.
*         else
*           Create copies of the selected columns.
*         end if
*         Create copies of the parameters.
*         Copy the table for all the specified columns.
*         Add any comments to the header.
*         If the header text of the original catalogue is to be copied
*         then
*           Copy the header text.
*         end if
*         Attempt to close the catalogue.
*         If ok then
*           Report a message.
*         else
*           Report an error.
*         end if
*       else
*         Report error: failed to open catalogue.
*       end if
*     else
*       Report warning: no open catalogue.
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     30/10/94 (ACD): Original version.
*     6/3/95   (ACD): Modified to reflect the changed names for the
*        constants defining the array sizes.
*     10/12/96 (ACD): Removed unused argument in calling list to
*        CAP_CPCCL and fixed bug in calling list for CAP_CPTXT.
*     29/7/97  (ACD): Changed the name of CAP_CPCCL to CAP_CPSCL to
*        avoid a name clash.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      CHARACTER
     :  CATOUT*(*),
     :  COMM*(*)
      LOGICAL
     :  CFLAG,
     :  TFLAG
*  Status:
      INTEGER STATUS              ! Global status.
*  Local Variables:
      INTEGER
     :  SI,         ! Identifier for the current selection.
     :  CIOUT,      ! Identifier for the output  catalogue.
     :  ROWS,       ! No. of rows in the current selection.
     :  NUMCOL,     ! Number of columns in the current selection.
     :  FIIN(CAT__MXCOL),  ! Column identifiers for current selection.
     :  FIOUT(CAT__MXCOL)  !   "         "       "  output catalogue.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Check if there is a catalogue open.

         IF (COPEN__SGZ) THEN
            CALL CAP_INFO (GUI__SGZ, ' ', 'Writing catalogue now...',
     :        STATUS)

*
*          Attempt to get an identifier for the output catalogue and
*          proceed if ok.

            CALL CAT_TOPEN (CATOUT, 'NEW', 'WRITE', CIOUT, STATUS)

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Get the number of rows in the current selection.

               SI = SELID__SGZ(CSEL__SGZ)

               CALL CAT_TROWS (SI, ROWS, STATUS)

*
*             Create copies of either all the columns or the currently
*             chosen columns, as appropriate.

               IF (CFLAG) THEN
                  CALL CAP_CPCOL (CI__SGZ, CIOUT, CAT__MXCOL, NUMCOL,
     :              FIIN, FIOUT, STATUS)
               ELSE
                  CALL CAP_CPSCL (CIOUT, CAT__MXCOL, NUMCOL, FIIN,
     :              FIOUT, STATUS)
               END IF

*
*             Create copies of the parameters.

               CALL CAP_CPPAR (CI__SGZ, CIOUT, STATUS)

*
*             Copy the table for all the specified columns.

               CALL CAP_CPTAB (SI, CIOUT, NUMCOL, FIIN, FIOUT, STATUS)

*
*             Add the mandatory comments and any additional comments to the
*             header.

               CALL CAP_GANCT (CIOUT, COMM, STATUS)

*
*             Copy the header text from the original catalogue, if this
*             is required.

               IF (TFLAG) THEN
                  CALL CAT_PUTXT (CIOUT, 'COMMENT', 'A copy of the '/
     :              /'header for the base catalogue follows.', STATUS)

                  CALL CAP_CPTXT (CI__SGZ, CIOUT, 'C', STATUS)
               END IF

*
*             Attempt to close the catalogue.

               CALL CAT_TRLSE (CIOUT, STATUS)

*
*             Report either a success message or an error, as
*             appropriate.

               IF (STATUS .EQ. SAI__OK) THEN
                  CALL MSG_SETI ('ROWS', ROWS)
                  CALL MSG_SETI ('CSEL', CSEL__SGZ)

                  CALL CAP_INFO (GUI__SGZ, ' ', 'Catalogue of ^ROWS '/
     :              /'rows created succesfully from selection ^CSEL.',
     :              STATUS)
               ELSE
                  CALL ERR_REP ('CAP_GSCAT_ERR', 'Error creating '/
     :              /'catalogue.', STATUS)
               END IF

            ELSE

*
*             The new catalogue could not be opened; report an error.

               CALL ERR_REP ('CAP_GSCAT_OPN', 'Failed to open the '/
     :           /'output catalogue.', STATUS)
            END IF

         ELSE
            CALL CAP_WARN (GUI__SGZ, ' ', 'There is no open catalogue.',
     :        STATUS)

         END IF

      END IF

      END
