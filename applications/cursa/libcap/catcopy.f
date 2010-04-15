      SUBROUTINE CATCOPY (STATUS)
*+
*  Name:
*     CATCOPY
*  Purpose:
*     Generate a new copy of a CAT catalogue.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATCOPY (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     Generate a new copy of a CAT catalogue.  By default all the
*     columns, parameters and textual information in the input catalogue
*     are copied.
*
*     Optionally some or all of the parameters in the input catalogue
*     can be omitted from the output catalogue and new parameters can
*     be added to the output catalogue.  Also any textual information
*     associated with the input catalogue can be omitted from the output
*     catalogue.
*
*     It is possible to use catcopy to generate a copy of a catalogue
*     in the same format (FITS table or whatever) as the original, but
*     there is little point in doing so; the same result can be achieved
*     using the Unix command 'cp', which is much quicker. The real
*     usefulness of catcopy is in converting a catalogue to a new format,
*     for example, converting a FITS table to an STL (small text list)
*     format catalogue.
*  Usage:
*     catcopy
*  ADAM Parameters:
*     CATIN  =  CHARACTER (read)
*        Name of the input catalogue.
*     CATOUT  =  CHARACTER (read)
*        Name of the output catalogue.
*     COPYPAR  =  CHARACTER (read)
*        Flag indicating which parameters are to be copied.
*        The valid responses are:
*        A - all; (default) copy all the parameters,
*        F - filter; omit (that is, filter out) selected parameters,
*        N - none; omit all the parameters.
*     PFILTER  =  CHARACTER (read)
*        A comma-separated list of the parameters to filter out (that is,
*        to omit).
*     ADDPAR  =  LOGICAL (read)
*        Flag indicating whether any new parameters are to be added to
*        the output catalogue.  The permitted values are:
*        TRUE  - add new parameters,
*        FALSE - (default) do not add new parameters.
*     PNAME  =  CHARACTER (read)
*        Name of the current new parameter.
*     PARTYP  =  CHARACTER (read)
*        Data type of the current new parameter.  The permitted types are:
*        REAL, DOUBLE, INTEGER, LOGICAL and CHAR.
*     PCSIZE  =  INTEGER (read)
*        Size of the current new parameter if it is of type CHAR.
*     PVALUE  =  CHARACTER (read)
*        Value of the current new parameter.
*     PUNITS  =  CHARACTER (read)
*        Units of the current new parameter.
*     PCOMM  =  CHARACTER (read)
*        Comments describing the current new parameter.
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
*     catcopy
*        The input and output catalogues will be prompted for and then
*        copying proceeds.  Any parameters and comments in the input
*        catalogue will be copied.
*     catcopy  input-catalogue  output-catalogue
*        Here the input and output catalogues have been specified on
*        the command line.  Copying proceeds and any parameters and
*        comments in the input catalogue will be copied.
*     catcopy  copypar=none
*        The input and output catalogues will be prompted for and then
*        copying proceeds.  None of the parameters in the input catalogue
*        will be written to the output catalogue.
*     catcopy  copypar=filter
*        The input catalogue, output catalogue and a list of parameters
*        will be prompted for.  Copying then proceeds.  All the parameters
*        specified in the list will not be written to the output catalogue.
*     catcopy  copypar=filter  pfilter=\'FSTATION,PLATESCA,TELFOCUS\'
*        The input and output catalogues will be prompted for and then
*        copying proceeds.  The parameters given in the list (FSTATION,
*        PLATESCA and TELFOCUS) will not be written to the output catalogue
*        (that is, they will be 'filtered out').  The items in the list
*        must be separated by commas.  When the list is specified on the
*        command line, as here, it must be enclosed in quotes and each
*        quote must be preceded by a backslash character (as shown) to
*        prevent it being interpreted by the Unix shell.
*     catcopy  addpar=true
*        The input and output catalogues will be prompted for.  The
*        details of additional parameters to be added to the output
*        catalogue will then be prompted for.  The details which must
*        be supplied for each parameter are: name, data type (and size
*        if of type CHAR), value, units and comments.  An arbitrary
*        number of comments can be added.  Once all the parameters required
*        have been specified then copying proceeds.
*     catcopy  text=all
*        The input and output catalogues will be prompted for and then
*        copying proceeds.  All the header information in the input
*        catalogue will be duplicated as comments in the output
*        catalogue.
*     catcopy  text=none
*        The input and output catalogues will be prompted for and then
*        copying proceeds.  Any comments in the input catalogue will not
*        be copied.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Attempt to get an identifier for the input catalogue.
*     Attempt to get an identifier for the output catalogue.
*     Get the number of rows in the input catalogue and set the
*     number expected in the output catalogue.
*     Determine what header text is to be copied.
*     Create columns in the output catalogue corresponding to those in
*     the input catalogue.
*     Check which parameters are to be copied.
*     If all the parameters are to be copied then
*       Create parameters in the output catalogue corresponding to those
*       in the input catalogue.
*     else only some of the parameters are to be copied then
*       Obtain the list of parameters which are to be filtered out.
*       Create parameters in the output catalogue corresponding to those
*       in the input catalogue, but excluding the specified ones.
*     end if
*     If extra parameters are to be added then
*       Get details of the additional parameters.
*     end if
*     Copy the table of values from the input catalogue to the output
*     catalogue.
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
*     1/7/94  (ACD): Original version.
*     19/4/95 (ACD): First stable version.
*     14/8/96 (ACD): Added setting the expected number of rows in the
*        output catalogue.
*     26/8/96 (ACD): Made copying of header text optional and added a
*        proper A-task prologue.
*     3/6/97  (ACD): Added check for bad status when looping for
*        parameter TEXT.
*     23/3/01 (ACD): Added functions to exclude selected parameters and
*        add new ones.
*     5/4/01  (ACD): Added the quiet mode.
*     24/4/01 (ACD): Fixed bugs in functions to exclude selected
*        parameters and add new ones.
*     27/4/01 (ACD): Modified the prologue to document the options added
*        recently.
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
     :  FIIN(CAT__MXCOL),  ! Column identifiers for input  catalogue.
     :  FIOUT(CAT__MXCOL)  !   "         "       "  output     "    .
      CHARACTER
     :  TEXT*10,    ! Flag indicating header text to be copied.
     :  CPYPAR*6,   ! Flag indicating which parameters are to be copied.
     :  PFILT*75    ! List of parameters to be excluded (comma-separated).
      LOGICAL
     :  QUIET,      ! Flag; operate in quiet or verbose (normal) mode?
     :  ADDPAR      ! Flag; add new parameters?

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

         CALL CAP_CPCOL (CIIN, CIOUT, CAT__MXCOL, NUMCOL, FIIN, FIOUT,
     :    STATUS)

*
*       Check which parameters are to be copied.

         CALL PAR_CHOIC ('COPYPAR', 'ALL', 'ALL,FILTER,NONE', .TRUE.,
     :     CPYPAR, STATUS)

*
*       Create parameters in the output catalogue corresponding to either
*       some or all of those in the input catalogue, as required.
*       Note that if CPYPAR .EQ. 'NONE' then neither clause is executed
*       an no parameters are copied.

         IF (CPYPAR .EQ. 'ALL') THEN
            CALL CAP_CPPAR (CIIN, CIOUT, STATUS)

         ELSE IF (CPYPAR .EQ. 'FILTER') THEN

*
*          Get the list of parameters to be filtered out.

            CALL PAR_GET0C ('PFILTER', PFILT, STATUS)
            CALL PAR_CANCL ('PFILTER', STATUS)

            CALL CAP_CFPAR (PFILT, CIIN, CIOUT, STATUS)

         END IF

*
*       Check whether any new parameters are to be added to the output
*       catalogue, and if so then get their details.

         CALL PAR_GET0L ('ADDPAR', ADDPAR, STATUS)
         CALL PAR_CANCL ('ADDPAR', STATUS)

         IF (ADDPAR) THEN
            CALL CAP_ADPAR (CIOUT, STATUS)
         END IF

*
*       Copy the table of values from the input catalogue to the output
*       catalogue.

         CALL CAP_CPTAB (CIIN, CIOUT, NUMCOL, FIIN, FIOUT, STATUS)

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
            CALL MSG_OUT (' ', 'Catalogue of ^ROWS rows copied '/
     :        /'successfully.', STATUS)
         ELSE
            CALL ERR_REP ('CATCOPY_ERR', 'Error copying catalogue.',
     :        STATUS)
         END IF

      END IF

      END
