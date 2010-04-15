      SUBROUTINE CATGSCIN (STATUS)
*+
*  Name:
*     CATGSCIN
*  Purpose:
*     Convert a GSC region to the preferred CURSA format.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATGSCIN (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     Convert a FITS table containing a region of the HST Guide Star
*     Catalogue (GSC) into a FITS table with the preferred CURSA format.
*     This format has the Right Ascension and Declination formatted as
*     CURSA angular columns and is sorted on Declination.  Though CURSA
*     can access the GSC regions directly it is more convenient to
*     process them with catgscin first.
*
*     The name of the output FITS table is generated automatically from
*     the name of the input GSC region.  GSC regions have names of the
*     form region-number.gsc (where region-number is an integer number).
*     The name of the output catalogue is 'gsc' followed by the region
*     number.  Thus, for example, if region 5828.gsc was imported the
*     converted catalogue would be written to FITS table gsc5828.FIT.
*  Usage:
*     catgscin
*  ADAM Parameters:
*     CATIN  =  CHARACTER (read)
*        Name of the input GSC region.
*     TEXT  =  CHARACTER (read)
*        Flag indicating the textual header information to be copied.
*        The valid responses are:
*        A - all; the output catalogue will contain a complete copy
*            of the header information for the input GSC region,
*            duplicated as comments,
*        C - (default) copy only the comments from the input GSC region.
*        N - none; no textual header information is copied.
*     QUIET  =  LOGICAL (read)
*        Operate in quiet mode where warnings are suppressed.  The
*        permitted values are:
*        TRUE  - quiet mode,
*        FALSE - verbose mode.
*  Examples:
*     catgscin
*        The input GSC region will be prompted for and then conversion
*        proceeds.  Comments in the input region will be copied.
*     catgscin  text=all
*        The input GSC region will be prompted for and then conversion
*        proceeds.  All the header information in the input region
*        will be duplicated as comments in the output catalogue.
*     catgscin  text=none
*        The input GSC region will be prompted for and then conversion
*        proceeds.  Comments in the input catalogue will not be copied.
*     catgscin  input-region
*        Here the input region has been specified on the command line.
*        Because no value was specified for parameter TEXT the default
*        will be adopted and comments in the region catalogue will be
*        copied.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Attempt to get an identifier for the input catalogue.
*     Attempt to get an identifier for the output catalogue.
*     Get the number of rows in the input catalogue and set the
*     number expected in the output catalogue.
*     Determine what header text is to be copied.
*     Generate an index on the Declination.
*     Create the columns in the output catalogue.
*     Create parameters in the output catalogue corresponding to those
*     in the input catalogue.
C     Create parameters in the output catalogue for the equinox and
C     epoch of the coordinates.
*     Copy the chosen columns from the input catalogue to the output
*     catalogue.
*     If any header text is to be copied then
*       Copy any header text from the input catalogue to the output
*       catalogue.
*     end if
*     Close the output catalogue.
*     Close the input catalogue.
*     Report any error.
*  Implementation Deficiencies:
*     The output catalogue should contain parameters specifying the
*     equinox and epoch of the coordinates.  Suitable code is included
*     but commented out because I am unsure about the equinox and
*     epoch.
*  Authors:
*     ACD: A C Davenhall (Leicester)
*  History:
*     10/5/95 (ACD): Original version.
*     26/8/96 (ACD): Made copying of header text optional and added a
*        proper A-task prologue.
*     3/6/97   (ACD): Added check for bad status when looping for
*        parameter TEXT.
*     5/4/01  (ACD): Added the quiet mode.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'CAT_PAR'          ! CAT symbolic constants.
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CIIN,       ! Identifier for the input  catalogue.
     :  CIOUT,      !     "       "   "  output     "    .
     :  ROWS,       ! No. of rows in the input catalogue.
     :  FIDEC,      ! Identifier for Declination column.
     :  II,         ! Identifier to index on Declination.
     :  NUMCOL,     ! Number of columns in the output catalogue.
     :  FIOUT(CAT__MXCOL), ! Column identifiers for output  catalogue.
     :  LCATIN,     ! Length of CATIN  (excl. trail. blanks).
     :  LCATOU      !   "    "  CATOUT ( "  .   "  .   "   ).
      CHARACTER
     :  CATIN*(CAT__SZCNM),  ! Name of the input  catalogue.
     :  CATOUT*(CAT__SZCNM), !  "   "   "  output     "    .
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
*       Note that the name of the output catalogue is constructed
*       automatically from the name of the input catalogue.

         CALL CAT_ASSOC ('CATIN', 'READ', CIIN, STATUS)

         CALL CAT_TIQAC (CIIN, 'NAME', CATIN, STATUS)
         LCATIN = CHR_LEN(CATIN)

         CATOUT = ' '
         LCATOU = 0

         CALL CHR_PUTC ('gsc', CATOUT, LCATOU)
         CALL CHR_PUTC (CATIN(1 : LCATIN), CATOUT, LCATOU)
         CALL CHR_PUTC ('.FIT', CATOUT, LCATOU)

         CALL CAT_TOPEN (CATOUT(1 : LCATOU), 'NEW', 'WRITE', CIOUT,
     :     STATUS)

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
*       Generate an index on the Declination column.  Note that the
*       values in the column will be in degrees when the index is
*       generated, but the values will be written in radians.  However,
*       the order is the same in both cases.

         CALL CAT_TIDNT (CIIN, 'DEC_DEG', FIDEC, STATUS)
         CALL CAT_INEW (FIDEC, 'TEMP', CAT__ASCND, II, STATUS)

*
*       Create columns in the output catalogue corresponding to those in
*       the input catalogue.

         CALL CAP_GSCCL (CIOUT, CAT__MXCOL, NUMCOL, FIOUT, STATUS)

*
*       Create parameters in the output catalogue corresponding to those
*       in the input catalogue.

         CALL CAP_CPPAR (CIIN, CIOUT, STATUS)

*
*       Create parameters in the output catalogue for the equinox and
*       epoch of the coordinates.

C        CALL CAT_PNEW0 (CIOUT, CAT__QITYP, 'EQUINOX', CAT__TYPEC,
C    :     QIOUTC, STATUS)
C        CALL CAT_TATTI (QIOUTC, 'CSIZE', 10, STATUS)
C        CALL CAT_TATTC (QIOUTC, 'VALUE', 'B1950.0', STATUS)
C        CALL CAT_TATTC (QIOUTC, 'COMM', 'Equinox of the Right '/
C    :     /'Ascension and Declination.', STATUS)

C        CALL CAT_PNEW0 (CIOUT, CAT__QITYP, 'EPOCH', CAT__TYPEC,
C    :     QIOUTC, STATUS)
C        CALL CAT_TATTI (QIOUTC, 'CSIZE', 10, STATUS)
C        CALL CAT_TATTC (QIOUTC, 'VALUE', 'B1950.0', STATUS)
C        CALL CAT_TATTC (QIOUTC, 'COMM', 'Epoch of the Right '/
C    :     /'Ascension and Declination.', STATUS)

*
*       Copy the table of values from the input catalogue to the output
*       catalogue.

         CALL CAP_GSCTB (CIIN, II, CIOUT, NUMCOL, FIOUT, STATUS)

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
            CALL MSG_SETC ('CATOUT', CATOUT)

            CALL MSG_OUT (' ', 'GSC region of ^ROWS rows converted '/
     :        /'successfully to catalogue ^CATOUT.', STATUS)
         ELSE
            CALL ERR_REP ('CATGSCIN_ERR', 'Error converting GSC '/
     :        /'region.', STATUS)
         END IF

      END IF

      END
