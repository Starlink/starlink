      SUBROUTINE CATCHARTRN (STATUS)
*+
*  Name:
*     CATCHARTRN
*  Purpose:
*     Translate a target list into a graphics attribute list.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATCHARTRN (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     Translate a target list into a graphics attribute list.  The
*     program computes some extra columns and parameters which define
*     how the objects in a target list are to be plotted (that is, the
*     symbol, column and size used to draw each object).  The details
*     required are read from a pre-prepared file, the so-called
*     'graphics translation file'.
*  Usage:
*     catchartrn
*  ADAM Parameters:
*     GTFILE  =  CHARACTER (read)
*        Name of the graphics translation file.
*     CATIN  =  CHARACTER (read)
*        Name of the input target list.
*     CATOUT  =  CHARACTER (read)
*        Name of the output graphics attribute list.
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
*     catchartrn
*        The graphics translation file, input and output catalogues will
*        be prompted for.  Then the output catalogue will be written
*        containing a copy of the original catalogue and new columns
*        defining how the objects are to be plotted.  Any comments in the
*        input catalogue will be copied.
*     catchartrn  text=all
*        The graphics translation file, input and output catalogues will
*        be prompted for.  Then the output catalogue will be written
*        containing a copy of the original catalogue and new columns
*        defining how the objects are to be plotted.  All the header
*        information in the input catalogue will be duplicated as comments
*        in the output catalogue.
*     catchartrn  text=none
*        The graphics translation file, input and output catalogues will
*        be prompted for.  Then the output catalogue will be written
*        containing a copy of the original catalogue and new columns
*        defining how the objects are to be plotted.  Any comments in the
*        input catalogue will not be copied.
*     catchartrn  graphics-translation-file  input-catalogue  output-catalogue
*        Here the graphics translation file, input and output catalogues
*        have been specified on the command line.  Because no value was
*        specified for parameter TEXT the default will be adopted and any
*        comments in the input catalogue will be copied.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Get the name of the graphics translation file.
*     Attempt to read and parse the graphics translation file.
*     If ok then
*       Attempt to open the input catalogue.
*       Attempt to open the output catalogue.
*       Determine whether or not the text information is to be copied.
*       If ok then
*         Set the number of rows in the output catalogue.
*         Check for any 'ascale' expressions and replace them with the
*         corresponding 'scale' expressions.
*         Create the columns and parameters in the output catalogue.
*         Copy the table of values, creating the new graphics attribute
*         fields as required.
*         If textual information is to be copied then
*           Copy the textual information.
*         end if
*         Close the output catalogue.
*         Close the input catalogue.
*       end if
*     end if
*     Report any error.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     12/8/96 (ACD): Original version.
*     6/6/97  (ACD): Converted for CURSA.
*     26/7/00 (ACD): Removed the `Bugs' item from the prologue.
*     5/4/01  (ACD): Added the quiet mode.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'CAT_PAR'          ! CAT symbolic constants.
*  Status:
      INTEGER STATUS             ! Global status.
*  Local Variables:
      CHARACTER
     :  GTFILE*75,  ! Graphics translation file.
     :  TEXT*10     ! Flag indicating header text to be copied.
      INTEGER
     :  CIIN,       ! Identifier for the input  catalogue.
     :  CIOUT,      !     "       "   "  output     "    .
     :  ROWS,       ! No. of rows in the input catalogue.
     :  NGRAT,      ! No. of columns of graphics attributes.
     :  FIGRAT(8),  ! Column identifiers for graphics attribute.
     :  TYGRAT(8),  ! Type of each column of graphic attributes.
     :  NUMCOL,     ! Number of columns in the input catalogue.
     :  FIIN(CAT__MXCOL),  ! Column identifiers for input  catalogue.
     :  FIOUT(CAT__MXCOL)  !   "         "       "  output     "    .
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
*       Get the name of the graphics translation file, attempt to
*       read and parse it and proceed if ok.

         CALL PAR_GET0C ('GTFILE', GTFILE, STATUS)
         CALL PAR_CANCL ('GTFILE', STATUS)

         CALL CAP_PRGRT (GTFILE, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Attempt to get identifiers for the input and output
*          catalogues.

            CALL CAT_ASSOC ('CATIN', 'READ', CIIN, STATUS)
            CALL CAT_CREAT ('CATOUT', CIOUT, STATUS)

*
*          Determine what header text is to be copied.  The options are:
*          A - all,   C - comments only,   N - none.

            TEXT = ' '

            DO WHILE (TEXT(1 : 1) .NE. 'A'  .AND.
     :                TEXT(1 : 1) .NE. 'C'  .AND.
     :                TEXT(1 : 1) .NE. 'N'  .AND.
     :                STATUS .EQ. SAI__OK)
               CALL PAR_GET0C ('TEXT', TEXT, STATUS)
               CALL PAR_CANCL ('TEXT', STATUS)

               CALL CHR_UCASE (TEXT)
            END DO

*
*          Proceed if all is ok.

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Set the number of rows expected in the output catalogue.

               CALL CAT_TROWS (CIIN, ROWS, STATUS)
               CALL CAT_RSET (CIOUT, ROWS, STATUS)

*
*             Check for any 'ASCALE' expressions entered in the graphics
*             translation file and replace them with the corresponding
*             'SCALE' expressions.

               CALL CAP_RASCL (CIIN, STATUS)

*
*             Create the columns and parameters in the output catalogue.
*             First the columns and parameters corresponding to the
*             graphics attributes and then the columns and parameters
*             copied from the input catalogue.

               CALL CAP_CRTCP (CIIN, CIOUT, 8, NGRAT, FIGRAT, TYGRAT,
     :           STATUS)
               CALL CAP_CPCOL (CIIN, CIOUT, CAT__MXCOL, NUMCOL, FIIN,
     :           FIOUT, STATUS)
               CALL CAP_CPPAR (CIIN, CIOUT, STATUS)

*
*             Copy the table of values, creating new fields for the
*             graphics attributes as required.

               CALL CAP_CRTAB (CIIN, CIOUT, NGRAT, FIGRAT, TYGRAT,
     :           NUMCOL, FIIN, FIOUT, STATUS)

*
*             If required, copy any header text from the input catalogue
*             to the output catalogue.

               IF (TEXT(1 : 1) .NE. 'N') THEN
                  CALL CAP_CPTXT (CIIN, CIOUT, TEXT(1 : 1), STATUS)
               END IF

*
*             Close the catalogues.

               CALL CAT_TRLSE (CIIN, STATUS)
               CALL CAT_TRLSE (CIOUT, STATUS)

            END IF
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CATCHARTRN_ERR', 'CATCHARTRN: Error '/
     :        /'generating the graphics attributes list.', STATUS)
         END IF

      END IF

      END
