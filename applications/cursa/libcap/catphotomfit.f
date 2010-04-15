      SUBROUTINE CATPHOTOMFIT (STATUS)
*+
*  Name:
*     CATPHOTOMFIT
*  Purpose:
*     Fit instrumental to standard magnitudes.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATPHOTOMFIT (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     This application fits instrumental magnitudes (typically
*     measured from images in a CCD frame) to standard magnitudes
*     in some photometric system for a set of photometric standard
*     stars.  The instrumental and standard magnitudes, and other
*     quantities, are read from a catalogue.
*
*     The transformation coefficients determined by the fit are
*     written to a file and can subsequently be used to calibrate
*     the instrumental magnitudes of a set of programme objects.
*     The equation used to relate the instrumental and standard
*     magnitudes is:
*
*       Mstd = Minst - arb + zero + (atmos * airmass)
*
*     where:
*       Mstd    - standard or calibrated magnitude,
*       Minst   - instrumental magnitude,
*       arb     - arbitrary constant added to the instrumental magnitudes,
*       zero    - zero point,
*       atmos   - atmospheric extinction,
*       airmass - air mass through which the standard star was observed.
*
*     Note that this relation is a particularly simple way of relating
*     standard and instrumental magnitudes.  In particular no correction
*     is made for any colour correction between the standard and
*     instrumental systems.
*
*     The application has a number of options, including the following.
*
*     * Either or both of the zero point and atmospheric extinction
*       can be supplied rather than fitted.  If both are supplied then
*       no fit is necessary and the file of transformation coefficients
*       is simply written.
*
*     * A table showing the residuals between the standard magnitudes
*       and the calibrated magnitudes computed from the instrumental
*       magnitudes can be listed.
*
*     * The table of residuals may optionally include a column showing
*       a name for each of the standard stars.
*
*     * Optionally a column containing the observed zenith distance
*       may be supplied instead of a column containing the air mass.
*       In this case the air mass is automatically calculated from the
*       observed zenith distance.
*
*     * By default all the stars in the input catalogue are included in
*       the fit.  However, optionally a column of `include in fit'
*       flags may be supplied and a star will only be included if it
*       has the flag set to TRUE.  This mechanism provides an easy way
*       to exclude stars which give a poor fit.
*  Usage:
*     catphotomfit
*  ADAM Parameters:
*     FIXED  =  LOGICAL (read)
*        Flag; are any of the coefficients fixed or are they all
*        determined by the fit.  It is coded as follows:
*        TRUE  - some or all of the coefficients are fixed,
*        FALSE - all the coefficients are determined from the fit.
*     ZENITHDIST  =  LOGICAL (read)
*        Flag; is the air mass read directly from a column or is it
*        computed from the observed zenith distance?  It is coded as
*        follows:
*        TRUE  - computed from the observed zenith distance,
*        FALSE - read directly from a column.
*     FZEROP  =  LOGICAL (read)
*        Flag; is the zero point fixed.  It is coded as follows:
*        TRUE  - the zero point is fixed,
*        FALSE - the zero point is determined from the fit.
*     ZEROP  =  DOUBLE PRECISION (read)
*        Value of the fixed zero point.
*     FATMOS  =  LOGICAL (read)
*        Flag; is the atmospheric extinction fixed.  It is coded as
*        follows:
*        TRUE  - the atmospheric extinction is fixed,
*        FALSE - the atmospheric extinction is determined from the fit.
*     ATMOS  =  DOUBLE PRECISION (read)
*        Value of the fixed atmospheric extinction.
*     RESID  =  LOGICAL (read)
*        Flag; are the residuals to be listed?  It is coded as follows:
*        TRUE  - list the residuals,
*        FALSE - do not list the residuals.
*     CATALOGUE  =  CHARACTER (read)
*        Name of the catalogue containing the standard and instrumental
*        magnitudes.
*     NAME  =  CHARACTER (read)
*        Name of a column containing names of the standard stars.
*        The special value NONE indicates that a column of star names
*        is not required.
*     INCLUDE  =  CHARACTER (read)
*        Name of a column of `include in fit' flags for the standard
*        stars.  The special value ALL indicates that all the stars are
*        to be included in the fit.
*     CATMAG  =  CHARACTER (read)
*        Name of the column or expression holding the standard or
*        catalogue magnitudes.
*     INSMAG  =  CHARACTER (read)
*        Name of the column or expression holding the instrumental
*        magnitudes.
*     AIRMASS  =  CHARACTER (read)
*        Name of the column or expression holding the air mass.
*     ZENDST  =  CHARACTER (read)
*        Name of the column or expression holding the observed zenith
*        distance.
*     INSCON  =  DOUBLE PRECISION (read)
*        Arbitrary constant previously added to the instrumental
*        magnitudes.
*     FILNME  =  CHARACTER (read)
*        The name of the file which is to contain the transformation
*        coefficients.
*     QUIET  =  LOGICAL (read)
*        Operate in quiet mode where warnings are suppressed.  The
*        permitted values are:
*        TRUE  - quiet mode,
*        FALSE - verbose mode.
*  Examples:
*     catphotomfit
*        The input catalogue and various other details will be prompted
*        for.  The transformation coefficients and a table of residuals
*        will be displayed.  The transformation coefficients will be
*        written to a file.
*     catphotomfit  zenithdist=true
*        You should supply a column containing the observed zenith
*        distance rather than one containing the air mass.  This
*        column will be used to calculate the air mass automatically.
*     catphotomfit  fixed=true
*        You will supply either the zero point, the atmospheric
*        extinction or both, rather than allowing them to be fitted.
*        You will be prompted for the appropriate details.
*     catphotomfit  resid=false
*        A table of residuals will not be listed.  However, the
*        transformation coefficients will still be displayed and
*        written to a file.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Check whether any of the coefficients are fixed.
*     Check whether the air mass is given or computed from the zenith
*     distance.
*     If any of the coefficients are fixed then
*       Check if the zero point is fixed.
*       If so then
*         Get the zero point.
*       end if
*       Check if the atmospheric extinction is fixed.
*       If so then
*         Get the atmospheric extinction.
*       end if
*     end if
*     Get the arbitrary constant applied to the instrumental
*     magnitudes.
*     Check whether the residuals are to be listed.
*     If a fit is required or the residuals are to be listed then
*       Attempt to open the catalogue.
*       Determine the number of rows in the catalogue.
*       Attempt to map the workspace.
*       Get identifiers for the columns to be read: star name,
*       'include in fit' flag, catalogue magnitude, instrumental magnitude
*       and airmass or observed zenith distance.
*       Read in the required columns.
*       If required then
*         Compute the air mass from the zenith distance.
*       end if
*       If a fit is required then
*         Perform the fit.
*       end if
*       If all ok then
*         Display the transformation coefficients.
*         If the residuals are required then
*           Compute and display the residuals.
*         end if
*         Get the name of the coefficients file.
*         Write the transformation coefficients to this file.
*       end if
*       Free the workspace.
*       Close the catalogue.
*     else
*       Get the name of the coefficients file.
*       Write the transformation coefficients to this file.
*     end if
*     Report any error.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*     DSB: David S Berry (JAC)
*  History:
*     8/5/97   (ACD): Original version.
*     16/11/97 (ACD): First stable version.
*     5/4/01   (ACD): Added the quiet mode.
*     22/6/06  (DSB): Initialised individual fixed value flags if FIXED
*                     is false.
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'CAT_PAR'          ! Standard CAT constants.
      INCLUDE 'CNF_PAR'          ! CNF functions
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  NUMFIX,   ! Number of fixed transformation coefficients.
     :  CI,       ! Catalogue identifier.
     :  ROWS,     ! Number of rows in the catalogue.
     :  NUMFIT,   ! Number of stars fitted.
     :  CATLEN,   ! Length of CATNAM (excl. trail. blanks).
     :  FILLEN,   !   "    "  FILNME ( "  .   "  .   "   ).
     :  WRKSZE,   ! Size of work array.
     :  LSTAT     ! Local Fortran I/O status.
      INTEGER
     :  INCPTR,   !    "    "    "    "  'include in fit' flags.
     :  CMGPTR,   !    "    "    "    "  catalogue magnitudes.
     :  IMGPTR,   !    "    "    "    "  instrumental magnitudes.
     :  AIRPTR,   !    "    "    "    "  air masses.
     :  WRKPTR,   !    "    "    "    "  work space.
     :  CLCPTR,   !    "    "    "    "  calculated magnitudes.
     :  RSDPTR    !    "    "    "    "  residuals.
      INTEGER
     :  ENAME,    ! Expression identifier for the star name.
     :  EINCL,    !     "          "       "   "  'include in fit' flag.
     :  ECATMG,   !     "          "       "   "  catalogue magnitude.
     :  EINSMG,   !     "          "       "   "  instrumental magnitude.
     :  EAIRMS    !     "          "       "   "  airmass.
      DOUBLE PRECISION
     :  ZEROP,    ! Fixed zero point.
     :  ATMOS,    !   "   atmospheric extinction.
     :  INSCON,   ! Arbitrary constant applied to instrumental magnitudes.
     :  RNORM     ! Minimum residual vector length.
      LOGICAL
     :  QUIET,    ! Flag; operate in quiet or verbose (normal) mode?
     :  FIXED,    ! Flag; any of zero pt, atmos. ext'n, colour corr'n fixed?
     :  ZENDST,   ! Flag; compute the air mass from the zenith distance?
     :  FZEROP,   ! Flag; is the zero point             fixed?
     :  FATMOS,   ! Flag; "   "  atmospheric extinction   "  ?
     :  RESID     ! Flag; are the residuals to be listed?
      CHARACTER
     :  NAME*(CAT__SZEXP),   ! Expression for the star name.
     :  INCLUD*(CAT__SZEXP), !     "       "   "  'include in fit' flag.
     :  CATMG*(CAT__SZEXP),  !     "       "   "  catalogue magnitude.
     :  INSMG*(CAT__SZEXP),  !     "       "   "  instrumental magnitude.
     :  AIRMS*(CAT__SZEXP),  !     "       "   "  air mass.
     :  CATNAM*(CAT__SZCNM), ! Catalogue name.
     :  FILNME*75,           ! Transformation coefficients file name.
     :  FMTBUF*13            ! Format buffer.
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
*       Check whether any of the coefficients are fixed.

         CALL PAR_GET0L ('FIXED', FIXED, STATUS)
         CALL PAR_CANCL ('FIXED', STATUS)

         NUMFIX = 0

*
*       Check whether the air mass is given or computed from the
*       observed zenith distance.

         CALL PAR_GET0L ('ZENITHDIST', ZENDST, STATUS)
         CALL PAR_CANCL ('ZENITHDIST', STATUS)

*
*       If any of the coefficients are fixed then check for each one
*       separately.  If necessary get the value and set the control
*       arrays for the fitting routine appropriately.

         IF (FIXED) THEN

*
*          Zero point.

            CALL PAR_GET0L ('FZEROP', FZEROP, STATUS)
            CALL PAR_CANCL ('FZEROP', STATUS)

            IF (FZEROP) THEN
               CALL PAR_GET0D ('ZEROP', ZEROP, STATUS)
               CALL PAR_CANCL ('ZEROP', STATUS)

               NUMFIX = NUMFIX + 1
            END IF

*
*          Atmospheric extinction.

            CALL PAR_GET0L ('FATMOS', FATMOS, STATUS)
            CALL PAR_CANCL ('FATMOS', STATUS)

            IF (FATMOS) THEN
               CALL PAR_GET0D ('ATMOS', ATMOS, STATUS)
               CALL PAR_CANCL ('ATMOS', STATUS)

               NUMFIX = NUMFIX + 1
            END IF
*
*       If nothing is fixed set the flags to indicate this and initialise
*       the fixed values to avoid compiler warnings.

         ELSE
            FZEROP = .FALSE.
            FATMOS = .FALSE.
            ZEROP = 0.0
            ATMOS = 0.0
         END IF

*
*       Get the arbitrary constant applied to the instrumental
*       magnitudes.

         CALL PAR_GET0D ('INSCON', INSCON, STATUS)
         CALL PAR_CANCL ('INSCON', STATUS)

*
*       Check whether the residuals are to be listed.

         CALL PAR_GET0L ('RESID', RESID, STATUS)
         CALL PAR_CANCL ('RESID',  STATUS)

*
*       Check whether a fit is required or the residuals are to be
*       listed.  (A fit is required if any of the coefficients remain
*       to be determined.)

         IF (NUMFIX .LT. 2) THEN

*
*          First attempt to open the catalogue.

            CALL CAT_ASSOC ('CATALOGUE', 'READ', CI, STATUS)

*
*          Determine the number of rows in the catalogue.

            CALL CAT_TROWS (CI, ROWS, STATUS)

*
*          Attempt to map the workspace.

            CALL CAP_CRTAR (ROWS, '_LOGICAL', INCPTR, STATUS)
            CALL CAP_CRTAR (ROWS, '_DOUBLE', CMGPTR, STATUS)
            CALL CAP_CRTAR (ROWS, '_DOUBLE', IMGPTR, STATUS)
            CALL CAP_CRTAR (ROWS, '_DOUBLE', AIRPTR, STATUS)

            WRKSZE = ROWS * 3
            CALL CAP_CRTAR (WRKSZE, '_DOUBLE', WRKPTR, STATUS)

            CALL CAP_CRTAR (ROWS, '_DOUBLE', CLCPTR, STATUS)
            CALL CAP_CRTAR (ROWS, '_DOUBLE', RSDPTR, STATUS)

*
*          Get identifiers for the required columns: star name,
*          'include in fit flag', catalogue magnitude, instrumental
*          magnitude and air mass.
*
*          Note:
*
*          (a) the star name and 'include in fit flag' are optional.
*          If they are not required the appropriate parameter adopts
*          the value 'NONE' or 'ALL' respectively.  Their absence is
*          signalled to the rest of the program by setting the
*          identifier to null,
*
*          (b) the identifiers obtained are deliberately for expressions,
*          not columns.

            CALL PAR_GET0C ('NAME', NAME, STATUS)
            CALL PAR_CANCL ('NAME', STATUS)
            CALL CHR_UCASE (NAME)
            IF (NAME .NE. 'NONE') THEN
               CALL CAT_EIDNT (CI, NAME, ENAME, STATUS)
            ELSE
               ENAME = CAT__NOID
            END IF

            CALL PAR_GET0C ('INCLUDE', INCLUD, STATUS)
            CALL PAR_CANCL ('INCLUDE', STATUS)
            CALL CHR_UCASE (INCLUD)
            IF (INCLUD .NE. 'ALL') THEN
               CALL CAT_EIDNT (CI, INCLUD, EINCL, STATUS)
            ELSE
               EINCL = CAT__NOID
            END IF

            CALL PAR_GET0C ('CATMAG', CATMG, STATUS)
            CALL PAR_CANCL ('CATMAG', STATUS)
            CALL CAT_EIDNT (CI, CATMG, ECATMG, STATUS)

            CALL PAR_GET0C ('INSMAG', INSMG, STATUS)
            CALL PAR_CANCL ('INSMAG', STATUS)
            CALL CAT_EIDNT (CI, INSMG, EINSMG, STATUS)

            IF (.NOT. ZENDST) THEN
               CALL PAR_GET0C ('AIRMASS', AIRMS, STATUS)
               CALL PAR_CANCL ('AIRMASS', STATUS)
            ELSE
               CALL PAR_GET0C ('ZENDST', AIRMS, STATUS)
               CALL PAR_CANCL ('ZENDST', STATUS)
            END IF
            CALL CAT_EIDNT (CI, AIRMS, EAIRMS, STATUS)

*
*          Read in the required columns.

C           print2001, rows
C2001       format(1x, 'rows: ', i5 )

            CALL CAP_RDTPH (CI, ROWS, EINCL, ECATMG, EINSMG, EAIRMS,
     :        %VAL(CNF_PVAL(INCPTR)), %VAL(CNF_PVAL(CMGPTR)),
     :        %VAL(CNF_PVAL(IMGPTR)), %VAL(CNF_PVAL(AIRPTR)),
     :        STATUS)
C           print2000, 'after reading in catalogue', status
C2000       format(1x, a, 3X, 'status: ', I20)

*
*          If required then compute the air mass from the observed
*          zenith distance.

            IF (ZENDST) THEN
               CALL CAP_AIRMS (ROWS, %VAL(CNF_PVAL(AIRPTR)), STATUS)
            END IF

*
*          If required the perform the fit.

            IF (NUMFIX .LT. 2) THEN
               CALL CAP_FITPH (ROWS, FZEROP, FATMOS,
     :           %VAL(CNF_PVAL(INCPTR)), %VAL(CNF_PVAL(CMGPTR)),
     :           %VAL(CNF_PVAL(IMGPTR)), %VAL(CNF_PVAL(AIRPTR)),
     :           INSCON, ZEROP, ATMOS, %VAL(CNF_PVAL(WRKPTR)),
     :           RNORM, NUMFIT, STATUS)
C              print2000, 'after fit', status
            END IF

*
*          If all ok then display the transformation coefficients and
*          write them to a file.

            IF (STATUS .EQ. SAI__OK) THEN

*
*             Display the transformation coefficients.

               CALL MSG_OUT (' ', ' ', STATUS)

               CALL MSG_SETI ('NUMFIT', NUMFIT)
               CALL MSG_OUT (' ', 'Coefficients determined '/
     :           /'successfully from fitting ^NUMFIT stars:', STATUS)

               CALL MSG_OUT (' ', ' ', STATUS)

               WRITE(FMTBUF, '(0PF13.6)', IOSTAT=LSTAT) ZEROP
               CALL CHR_LDBLK (FMTBUF)
               CALL MSG_SETC ('FMTBUF', FMTBUF)
               CALL MSG_OUT (' ', ' zero point = ^FMTBUF', STATUS)

               WRITE(FMTBUF, '(0PF13.6)', IOSTAT=LSTAT) ATMOS
               CALL CHR_LDBLK (FMTBUF)
               CALL MSG_SETC ('FMTBUF', FMTBUF)
               CALL MSG_OUT (' ', ' atmospheric extinction = ^FMTBUF',
     :           STATUS)

               CALL MSG_OUT (' ', ' ', STATUS)

               WRITE(FMTBUF, '(0PF13.6)', IOSTAT=LSTAT) RNORM
               CALL CHR_LDBLK (FMTBUF)
               CALL MSG_SETC ('FMTBUF', FMTBUF)
               CALL MSG_OUT (' ', ' (minimum residual vector length '/
     :           /'= ^FMTBUF)', STATUS)

               CALL MSG_OUT (' ', ' ', STATUS)

*
*             If required then list the residuals.

               IF (RESID) THEN
                  CALL CAP_RESPH (ROWS, CI, ENAME,
     :              %VAL(CNF_PVAL(INCPTR)),
     :              %VAL(CNF_PVAL(CMGPTR)), %VAL(CNF_PVAL(IMGPTR)),
     :              %VAL(CNF_PVAL(AIRPTR)), INSCON, ZEROP, ATMOS,
     :              %VAL(CNF_PVAL(CLCPTR)), %VAL(CNF_PVAL(RSDPTR)),
     :              STATUS)
C                 print2000, 'after residuals', status
               END IF

*
*             Get the name of the coefficients file.

               CALL CAT_TIQAC (CI, 'NAME', CATNAM, STATUS)

               IF (CATNAM .NE. ' ') THEN
                  CATLEN = CHR_LEN(CATNAM)
               ELSE
                  CATLEN = 1
               END IF

               FILNME = ' '
               FILLEN = 0

               CALL CHR_PUTC (CATNAM(1 : CATLEN), FILNME, FILLEN)
               CALL CHR_PUTC ('.CFT', FILNME, FILLEN)

               CALL PAR_DEF0C ('FILNME', FILNME(1 : FILLEN), STATUS)
               CALL PAR_GET0C ('FILNME', FILNME, STATUS)
               CALL PAR_CANCL ('FILNME', STATUS)

*
*             Write the transformation coefficients to this file.

               CALL CAP_CFLPH (FILNME, INSCON, ZEROP, ATMOS, STATUS)
C              print2000, 'after writing file', status
            END IF

*
*          Free the workspace.

            CALL CAP_FREAR (INCPTR, STATUS)
            CALL CAP_FREAR (CMGPTR, STATUS)
            CALL CAP_FREAR (IMGPTR, STATUS)
            CALL CAP_FREAR (AIRPTR, STATUS)

*
*          Close the catalogue.

            CALL CAT_TRLSE (CI, STATUS)
         ELSE

*
*          All the transformation coefficients were fixed; simply write
*          the file.

            CALL PAR_GET0C ('FILNME', FILNME, STATUS)
            CALL PAR_CANCL ('FILNME', STATUS)

            CALL CAP_CFLPH (FILNME, INSCON, ZEROP, ATMOS, STATUS)
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP ('CATPHOTOMFIT_ERR', 'Failure fitting '/
     :        /'standard stars.', STATUS)
         END IF

      END IF

      END
