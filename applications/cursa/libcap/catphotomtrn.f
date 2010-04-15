      SUBROUTINE CATPHOTOMTRN (STATUS)
*+
*  Name:
*     CATPHOTOMTRN
*  Purpose:
*     Transform instrumental to calibrated mags. for programme stars.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATPHOTOMTRN (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     This application transforms instrumental magnitudes into
*     calibrated magnitudes in some photometric system for a catalogue
*     of programme objects.  A new catalogue is written which contains
*     the calibrated magnitudes as well as all the columns in the
*     original catalogue.
*
*     The transformation coefficients used to convert the instrumental
*     magnitudes are read from a file.  Application catphotomfit
*     can be used to prepare a suitable file.  See the documentation
*     for this application for details of the transformation used.
*
*     The transformation includes a term for the air mass through
*     which the object was observed.  By default a column containing
*     the air mass is read from the input catalogue.  However,
*     optionally, a column containing the observed zenith distance
*     of the object may be read instead and used to automatically
*     calculate the air mass.
*  Usage:
*     catphotomtrn
*  ADAM Parameters:
*     ZENITHDIST  =  LOGICAL (read)
*        Flag; is the air mass read directly from a column or is it
*        computed from the observed zenith distance?  It is coded as
*        follows:
*        TRUE  - computed from the observed zenith distance,
*        FALSE - read directly from a column.
*     FILNME  =  CHARACTER (read)
*        The name of the file which contains the transformation
*        coefficients.
*     INSCON  =  DOUBLE PRECISION (read)
*        Arbitrary constant previously added to the instrumental
*        magnitudes.
*     CATIN  =  CHARACTER (read)
*        Name of the input catalogue.
*     CATOUT  =  CHARACTER (read)
*        Name of the output catalogue.
*     INSMAG  =  CHARACTER (read)
*        Name of the column or expression in the input catalogue
*        holding the instrumental magnitudes.
*     AIRMASS  =  CHARACTER (read)
*        Name of the column or expression in the input catalogue
*        holding the air mass.
*     ZENDST  =  CHARACTER (read)
*        Name of the column or expression in the input catalogue
*        holding the observed zenith distance.
*     CALMAG  =  CHARACTER (read)
*        Name of the column in the output catalogue to hold the
*        calibrated magnitudes.
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
*     catphotomtrn
*        The input and output catalogues and various other details will
*        be prompted for.  A new catalogue containing the calibrated
*        magnitudes will be written.  All the header information in the
*        input catalogue will be duplicated as comments in the output
*        catalogue.
*     catphotomtrn  zenithdist=true
*        The input and output catalogues and various other details will
*        be prompted for.  You should supply a column containing the
*        observed zenith distance rather than one containing the air
*        mass.  This column will be used to calculate the air mass
*        automatically.
*     catphotomtrn  text=all
*        The input and output catalogues and various other details will
*        be prompted for.  A new catalogue containing the calibrated
*        magnitudes will be written.  All the header information in the
*        input catalogue will be duplicated as comments in the output
*        catalogue.
*     catphotomtrn  text=none
*        The input and output catalogues and various other details will
*        be prompted for.  A new catalogue containing the calibrated
*        magnitudes will be written.  Any comments in the input catalogue
*        will not be copied.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Check whether the air mass is given or computed from the zenith
*     distance.
*     Get the name of the transformation coefficients file.
*     Read the transformation coefficients file.
*     Allow the user to replace the value for the instrumental constant.
*     Attempt to get an identifier for the input catalogue.
*     Attempt to get an identifier for the output catalogue.
*     If ok then
*       Determine the number of rows in the input catalogue.
*       Get identifiers for the instrumental magnitude and air mass
*       (or observed zenith distance) columns in the input catalogue.
*       Get the name of the calibrated magnitude column in the output
*       catalogue.
*       Determine what textual information is to be copied.
*       Create the columns in the output catalogue.
*       Create parameters in the output catalogue corresponding to those
*       in the input catalogue.
*       Copy the table of values from the input catalogue to the output
*       catalogue, calculating the calibratred magnitudes for each row.
*       If any textual information is to be copied then
*         Copy any header text from the input catalogue to the output
*         catalogue.
*       end if
*       Close the output catalogue.
*       Close the input catalogue.
*     end if
*     If ok then
*       Report success message.
*     else
*       Report any error.
*     end if
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     6/10/97 (ACD): Original version.
*     6/11/97 (ACD): First stable version.
*     5/4/01  (ACD): Added the quiet mode.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'       ! Standard SAE constants.
      INCLUDE 'CAT_PAR'       ! CAT symbolic constants.
*  Status:
      INTEGER STATUS          ! Global status.
*  Local Variables:
      LOGICAL
     :  QUIET,      ! Flag; operate in quiet or verbose (normal) mode?
     :  ZENDST      ! Flag; compute the air mass from the zenith distance?
      DOUBLE PRECISION
     :  ZEROP,      ! Fixed zero point.
     :  ATMOS,      !   "   atmospheric extinction.
     :  INSCON      ! Arbitrary constant applied to instrumental magnitudes.
      INTEGER
     :  CIIN,       ! Identifier for the input  catalogue.
     :  CIOUT,      !     "       "   "  output     "    .
     :  ROWS,       ! No. of rows in the input catalogue.
     :  EINSMG,     ! Expression identifier for instrumental magnitudes.
     :  EAIRMS,     !     "           "      "  air mass.
     :  FCALMG,     ! Column          "      "  calibrated       "     .
     :  NUMCOL,     ! Number of columns in the input catalogue.
     :  FIIN(CAT__MXCOL),      ! Column identifiers for input  catalogue.
     :  FIOUT(CAT__MXCOL)      !   "         "       "  output     "    .
      CHARACTER
     :  INSMAG*(CAT__SZCMP),   ! Name of column of instrumental mags.
     :  AIRMAS*(CAT__SZCMP),   ! "   "    "    "   air mass.
     :  CALMAG*(CAT__SZCMP),   !  "   "    "    "  calibrated    "  .
     :  FILNME*75,  ! Transformation coefficients file name.
     :  TEXT*10     ! Flag indicating header text to be copied.
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
*       Check whether the air mass is given or computed from the
*       observed zenith distance.

         CALL PAR_GET0L ('ZENITHDIST', ZENDST, STATUS)
         CALL PAR_CANCL ('ZENITHDIST', STATUS)

*
*       Get the name of the transformation coefficients file and
*       attempt to read it.

         CALL PAR_GET0C ('FILNME', FILNME, STATUS)
         CALL PAR_CANCL ('FILNME', STATUS)

         CALL CAP_RFLPH (FILNME, INSCON, ZEROP, ATMOS, STATUS)

*
*       Allow the user to over-ride the default instrumental constant.

         CALL PAR_DEF0D ('INSCON', INSCON, STATUS)
         CALL PAR_GET0D ('INSCON', INSCON, STATUS)
         CALL PAR_CANCL ('INSCON', STATUS)

*
*       Attempt to get identifiers for the input and output catalogues
*       and proceed if ok.

         CALL CAT_ASSOC ('CATIN', 'READ', CIIN, STATUS)
         CALL CAT_CREAT ('CATOUT', CIOUT, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Determine the number of rows in the input catalogue and set the
*          number expected in the output catalogue.  (This information
*          will be either used or ignored, depending on the format of
*          the output catalogue.)

            CALL CAT_TROWS (CIIN, ROWS, STATUS)
            CALL CAT_RSET (CIOUT, ROWS, STATUS)

*
*          Get identifiers for the instrumental magnitude and air mass
*          columns in the input catalogue.

            CALL PAR_GET0C ('INSMAG', INSMAG, STATUS)
            CALL PAR_CANCL ('INSMAG', STATUS)
            CALL CAT_TIDNT (CIIN, INSMAG, EINSMG, STATUS)

            IF (.NOT. ZENDST) THEN
               CALL PAR_GET0C ('AIRMASS', AIRMAS, STATUS)
               CALL PAR_CANCL ('AIRMASS', STATUS)
            ELSE
               CALL PAR_GET0C ('ZENDST', AIRMAS, STATUS)
               CALL PAR_CANCL ('ZENDST', STATUS)
            END IF
            CALL CAT_TIDNT (CIIN, AIRMAS, EAIRMS, STATUS)

*
*          Get the name of calibrated magnitude column in the output
*          catalogue.

            CALL PAR_GET0C ('CALMAG', CALMAG, STATUS)
            CALL PAR_CANCL ('CALMAG', STATUS)

*
*          Determine what textual information is to be copied.  The
*          options are:
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
*          Create the columns in the output catalogue.  These columns
*          correspond to those in the input catalogue except that
*          there is an additional column to contain the calibrated
*          magnitude.

            CALL CAP_CPCPH (CALMAG, CIIN, CIOUT, CAT__MXCOL,
     :        FCALMG, NUMCOL, FIIN, FIOUT, STATUS)

*
*          Create parameters in the output catalogue corresponding to those
*          in the input catalogue.

            CALL CAP_CPPAR (CIIN, CIOUT, STATUS)

*
*          Copy the table of values from the input catalogue to the output
*          catalogue, calculating the calibrated magnitude for each row.

            CALL CAP_TBLPH (CIIN, CIOUT, ZENDST, INSCON, ZEROP, ATMOS,
     :        EINSMG, EAIRMS, FCALMG, NUMCOL, FIIN, FIOUT, STATUS)

*
*          If required, copy any textual information from the input
*          catalogue to the output catalogue.

            IF (TEXT(1 : 1) .NE. 'N') THEN
               CALL CAP_CPTXT (CIIN, CIOUT, TEXT(1 : 1), STATUS)

*
*             Append textual information describing the transformation
*             coefficients.

               CALL CAP_TXTPH (CIOUT, CALMAG, INSCON, ZEROP, ATMOS,
     :           STATUS)
            END IF

*
*          Close the catalogues.

            CALL CAT_TRLSE (CIIN, STATUS)
            CALL CAT_TRLSE (CIOUT, STATUS)
         END IF

*
*       Report either a success message or an error, as appropriate.

         IF (STATUS .EQ. SAI__OK) THEN
            CALL MSG_SETI ('ROWS', ROWS)
            CALL MSG_OUT (' ', 'Calibrated magnitudes calculated '/
     :        /'successfully for catalogue of ^ROWS rows.', STATUS)
         ELSE
            CALL ERR_REP ('CATPHOTOMTRN_ERR', 'Error calculating '/
     :        /'the calibrated magnitudes.', STATUS)
         END IF

      END IF

      END
