      SUBROUTINE CATSORT (STATUS)
*+
*  Name:
*     CATSORT
*  Purpose:
*     Create a copy of a catalogue sorted on a specified column.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATSORT (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     Create a copy of a catalogue sorted on a specified column.  Note
*     that catsort generates a new sorted catalogue; it does not overwrite
*     the original catalogue.  The new catalogue can be sorted into either
*     ascending or descending order.  All the columns and parameters in the
*     input catalogue are copied.  Optionally any textual information
*     associated with the input catalogue can also be copied.
*
*     Catalogues can be sorted on columns of any of the numeric data
*     types.  They should not be sorted on columns of data type CHARACTER
*     or LOGICAL.  If a catalogue is sorted on a column which contains null
*     values then the rows for which the column is null will appear after
*     all the rows with a valid value. The order of such rows is
*     unpredictable.
*  Usage:
*     catsort
*  ADAM Parameters:
*     CATIN  =  CHARACTER (read)
*        Name of the input catalogue.
*     CATOUT  =  CHARACTER (read)
*        Name of the output catalogue, sorted on the specified column.
*     FNAME  =  CHARACTER (read)
*        The name of the column the output catalogue is to be sorted on.
*     ORDER  =  CHARACTER (read)
*        Order into which the catalogue is to be sorted: ascending or
*        descending.
*     TEXT  =  CHARACTER (read)
*        Flag indicating the textual header information to be copied.
*        The valid responses are:
*        A - all; the output catalogue will contain a complete copy
*            of the header information for the input catalogue,
*            duplicated as comments,
*        C - (default) copy only the comments from the input catalogue.
*            In the case of a FITS table the COMMENTS and HISTORY
*            keywords will be copied.
*        N - none; no textual header information is copied.
*     QUIET  =  LOGICAL (read)
*        Operate in quiet mode where warnings are suppressed.  The
*        permitted values are:
*        TRUE  - quiet mode,
*        FALSE - verbose mode.
*  Examples:
*     catsort
*        The following parameters will be prompted for: input catalogue,
*        output catalogue, name of the column to be sorted on and the
*        order required (ascending or descending).  The sorted catalogue
*        will then be created.  Any comments in the input catalogue will
*        be copied.
*     catsort  text=all
*        The following parameters will be prompted for: input catalogue,
*        output catalogue, name of the column to be sorted on and the
*        order required (ascending or descending).  The sorted catalogue
*        will then be created.  All the header information in the input
*        catalogue will be duplicated as comments in the output
*        catalogue.
*     catsort  text=none
*        The following parameters will be prompted for: input catalogue,
*        output catalogue, name of the column to be sorted on and the
*        order required (ascending or descending).  The sorted catalogue
*        will then be created.  Any comments in the input catalogue will not
*        be copied.
*     catsort  input-catalogue  output-catalogue  column-name  ascending
*        Here the all the required parameters have been specified on the
*        command line.  Because no value was specified for parameter
*        TEXT the default will be adopted and any comments in the input
*        catalogue will be copied.
*  Pitfalls:
*     Catalogues should not be sorted on columns of data type CHARACTER
*     or LOGICAL.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Attempt to get an identifier for the input catalogue.
*     Attempt to get an identifier for the output catalogue.
*     Get the number of rows in the input catalogue and set the
*     number expected in the output catalogue.
*     Get the name of the column the output catalogue is to be sorted
*     on.
*     Get the sort order required: ascending or descending.
*     Determine what header text is to be copied.
*     Create columns in the output catalogue corresponding to those in
*     the input catalogue.
*     Create parameters in the output catalogue corresponding to those
*     in the input catalogue.
*     Generate a temporary index on the sorted column.
*     Copy the table of values from the input catalogue to the output
*     catalogue via the temporary index.
*     If any header text is to be copied then
*       Copy any header text from the input catalogue to the output
*       catalogue.
*     end if
*     Close the output catalogue.
*     Close the input catalogue.
*     If ok then
*       Report success message.
*     else
*       Report any error.
*     end if
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     24/11/94 (ACD): Original version.
*     20/4/95  (ACD): First stable version.
*     26/8/96  (ACD): Made copying of header text optional and added a
*        proper A-task prologue.
*     3/6/97   (ACD): Added check for bad status when looping for
*        parameter TEXT.
*     5/4/01   (ACD): Added the quiet mode.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'CAT_PAR'          ! CAT symbolic constants.
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      INTEGER
     :  CIIN,       ! Identifier for the input  catalogue.
     :  CIOUT,      !     "       "   "  output     "    .
     :  ROWS,       ! No. of rows in the input catalogue.
     :  NUMCOL,     ! Number of columns in the input catalogue.
     :  FIIN(CAT__MXCOL),    ! Column identifiers for input  catalogue.
     :  FIOUT(CAT__MXCOL),   !   "         "       "  output     "    .
     :  II,         ! Index identifier.
     :  FISOR,      ! Identifier to sort column.
     :  ORDER       ! Required order for the sort column.
      CHARACTER
     :  FNAME*(CAT__SZCMP),  ! Name of the sort column.
     :  CORDER*30,  ! Required order for the sort column.
     :  TEXT*10     ! Flag indicating header text to be copied.
      LOGICAL
     :  QUIET       ! Flag; operate in quiet or verbose (normal) mode?
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Obtain and set the quiet mode.

         CALL PAR_GET0L ('QUIET', QUIET, STATUS)

         IF (QUIET) THEN
            CALL CAT_TUNES ('QUIET', 'YES', STATUS)
         ELSE
            CALL CAT_TUNES ('QUIET', 'NO', STATUS)
         END IF

*
*       Attempt to get identifiers for the input and output catalogues.

         CALL CAT_ASSOC ('CATIN', 'READ', CIIN, STATUS)
         CALL CAT_CREAT ('CATOUT', CIOUT, STATUS)

*
*       Get the number of rows in the input catalogue and set the
*       number expected in the output catalogue.  (This information
*       will be either used or ignored, depending on the format of
*       the output catalogue.)

         CALL CAT_TROWS (CIIN, ROWS, STATUS)
         CALL CAT_RSET (CIOUT, ROWS, STATUS)

*
*       Get the name of the column the output catalogue is to be sorted
*       on, and get an identifier for it.

         CALL PAR_GET0C ('FNAME', FNAME, STATUS)
         CALL PAR_CANCL ('FNAME', STATUS)

         CALL CAT_TIDNT (CIIN, FNAME, FISOR, STATUS)

*
*       Get the order required: ascending or descending.

         CALL PAR_GET0C ('ORDER', CORDER, STATUS)
         CALL PAR_CANCL ('ORDER', STATUS)

         IF (CORDER(1 : ) .EQ. 'D'  .OR.  CORDER(1 : 1) .EQ. 'd') THEN
            ORDER = CAT__DSCND
         ELSE
            ORDER = CAT__ASCND
         END IF

*
*       Determine what header text is to be copied.  The options are:
*       A - all,   C - comments only,   N - none.

         TEXT = ' '

         DO WHILE (TEXT(1 : 1) .NE. 'A'  .AND.
     :             TEXT(1 : 1) .NE. 'C'  .AND.
     :             TEXT(1 : 1) .NE. 'N'  .AND.
     :             STATUS .EQ. SAI__OK)
            CALL PAR_GET0C ('TEXT', TEXT, STATUS)
            CALL PAR_CANCL ('TEXT', STATUS)

            CALL CHR_UCASE (TEXT)
         END DO

*
*       Create columns in the output catalogue corresponding to those in
*       the input catalogue.

         CALL CAP_CPSCO (CIIN, CIOUT, FISOR, ORDER, CAT__MXCOL, NUMCOL,
     :    FIIN, FIOUT, STATUS)

*
*       Create parameters in the output catalogue corresponding to those
*       in the input catalogue.

         CALL CAP_CPPAR (CIIN, CIOUT, STATUS)

*
*       Create a temporary index on the sorted column.

         CALL CAT_INEW (FISOR, 'TEMP', ORDER, II, STATUS)

*
*       Copy the table of values from the input catalogue to the output
*       catalogue via the temporary index.

         CALL CAP_CPTAB (II, CIOUT, NUMCOL, FIIN, FIOUT, STATUS)

*
*       If required, copy any header text from the input catalogue to the
*       output catalogue.

         IF (TEXT(1 : 1) .NE. 'N') THEN
            CALL CAP_CPTXT (CIIN, CIOUT, TEXT(1 : 1), STATUS)
         END IF

*
*       Close the catalogues.

         CALL CAT_TRLSE (CIIN, STATUS)
         CALL CAT_TRLSE (CIOUT, STATUS)

*
*       Report either a success message or an error, as appropriate.

         IF (STATUS .EQ. SAI__OK) THEN
            CALL MSG_SETI ('ROWS', ROWS)
            CALL MSG_SETC ('FNAME', FNAME)

            CALL MSG_OUT (' ', 'Catalogue of ^ROWS rows sorted on '/
     :        /'column ^FNAME created successfully.', STATUS)
         ELSE
            CALL ERR_REP ('CATSORT_ERR', 'Error creating sorted '/
     :        /'catalogue.', STATUS)
         END IF

      END IF

      END
