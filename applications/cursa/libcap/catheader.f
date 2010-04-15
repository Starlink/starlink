      SUBROUTINE CATHEADER (STATUS)
*+
*  Name:
*     CATHEADER
*  Purpose:
*     List various header information for a catalogue.
*  Language:
*     Fortran 77.
*  Type of Module:
*     ADAM A-task
*  Invocation:
*     CALL CATHEADER (STATUS)
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*  Description:
*     List various header information for a catalogue.  By default the
*     information listed is: the number of rows, the number of columns,
*     the number of catalogue parameters and a list of the names of all
*     the columns.  Parameter DETAILS can be used to specify that various
*     alternative details are to be listed.
*
*     The output is directed to the standard output and optionally may
*     also be copied to a text file.  If the name of the catalogue is
*     CNAME, then this output file will be called CNAME.lis.
*
*     Application parameters ROWS, COLS, PARS and NAMES are written only
*     if DETAILS=SUMMARY or FULL.
*  Usage:
*     catheader
*  ADAM Parameters:
*     CATALOGUE  =  CHARACTER (read)
*        Name of the catalogue.
*     FILE  =  LOGICAL (read)
*        Flag indicating whether or not an output file is to be written.
*        It is coded as follows:
*        .TRUE.  - write the output file,
*        .FALSE. - do not write the output file.
*     DETAIL  =  CHARACTER (read)
*        Flag specifying the details which catheader is to display.
*        The options are:
*        SUMMARY    - summary (default),
*        COLUMNS    - full details of all the columns,
*        PARAMETERS - full details of all the parameters,
*        TEXT       - textual information,
*        AST        - details of any AST information,
*        FULL       - full information (all the above).
*     QUIET  =  LOGICAL (read)
*        Operate in quiet mode where warnings are suppressed.  The
*        permitted values are:
*        TRUE  - quiet mode,
*        FALSE - verbose mode.
*     ROWS  =  INTEGER (write)
*        The number of rows in the catalogue.
*     COLS  =  INTEGER (write)
*        The number of columns in the catalogue.
*     PARS  =  INTEGER (write)
*        The number of parameters in the catalogue.
*     NAMES  =  CHARACTER (write)
*        A list of the names of all the columns in the catalogue.
*  Examples:
*     catheader
*        The catalogue name will be prompted for, then the default
*        details will be displayed.
*     catheader  input-catalogue
*        Here the input catalogue has been specified on the command
*        line.  The default details will be displayed.
*     catheader  details=columns
*        The catalogue name will be prompted for, then details of all
*        the columns in the catalogue will be displayed.
*     catheader  file=true
*        The catalogue name will be prompted for, then the default
*        details will be both displayed and written to a text file
*        called input-catalogue.lis.
*  Algorithm:
*     Obtain and set the quiet mode.
*     Attempt to open the catalogue.
*     If ok then
*       Obtain the flag indicating whether or not an output file is to
*       be written.
*       Convert this flag to the form required by the output
*       subroutines.
*       Obtain the name of the catalogue.
*       If an output file is required then
*         Attempt to get a Fortran unit number for the file.
*         Assemble the name of the file.
*         Attempt to open the file.
*       end if
*       Determine the mode required and set the flags appropriately.
*       If required then
*         List the summary details.
*       end if
*       If required then
*         List the details of the columns.
*       end if
*       If required then
*         List the details of the parameters.
*       end if
*       If required then
*         List the textual information.
*       end if
*       If required then
*         List the AST details.
*       end if
*       If an output file is required then
*         Close the file.
*       end if
*       Release the catalogue identifier.
*     end if
*     Report any error.
*  Authors:
*     ACD: A C Davenhall (Leicester)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*  History:
*     16/9/94 (ACD): Original version.
*     19/4/95 (ACD): First stable version.
*     27/8/96 (ACD): Added a proper A-task prologue.
*     5/4/01  (ACD): Added the quiet mode.
*     2/11/01 (ACD): Revised to allow options about which information is
*       listed.
*     15/8/05 (TIMJ): OPEN should use FILE= not NAME=
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'CAT_PAR'          ! Standard CAT constants.
*  Status:
      INTEGER STATUS             ! Global status.
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  CI,       ! Catalogue identifier.
     :  LCNAME,   ! Length of CNAME (excl. trail. blanks).
     :  LOFILE,   !   "    "  OFILE ( "  .   "  .   "   ).
     :  FLUNIT,   ! Fortran unit number for writing to file.
     :  LSTAT,    ! Local Fortran I/O status.
     :  OFLAG     ! Flag indicating the output to be produced,
*                   coded as follows:
*                    1 - screen ('standard output') only,
*                    2 - file only,
*                    3 - screen and file.
      LOGICAL
     :  QUIET,    ! Flag; operate in quiet or verbose (normal) mode?
     :  FILE,     ! Flag; output to be written to text file?
     :  SUMFLG,   ! Flag; list summary?
     :  COLFLG,   ! Flag; list column details?
     :  PARFLG,   ! Flag; list parameter details?
     :  TXTFLG,   ! Flag; list header text?
     :  ASTFLG,   ! Flag; list AST details?
     :  FULFLG    ! Flag; list fuil details (all the above)?
      CHARACTER
     :  CNAME*(CAT__SZCNM), ! Catalogue name.
     :  OFILE*70, ! Name of the output file.
     :  DETAIL*10 ! Mode in which the application is to operate.
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
*       Attempt to open the catalogue (get an identifier for it),
*       and proceed if ok.

         CALL CAT_ASSOC ('CATALOGUE', 'READ', CI, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

*
*          Obtain the flag indicating whether or not an output file is
*          to be written.

            CALL PAR_GET0L ('FILE', FILE, STATUS)
            CALL PAR_CANCL ('FILE', STATUS)

*
*          Convert this flag to the form required by the output
*          subroutines.

            IF (FILE) THEN
               OFLAG = 3
            ELSE
               OFLAG = 1
            END IF

*
*          Inquire the name of the catalogue.

            CALL CAT_TIQAC (CI, 'NAME', CNAME, STATUS)

*
*          If an output file is required then: attempt to get a freed
*          Fortran unit number, assemble the file name and attempt to
*          open the file.

            IF (FILE) THEN
               CALL FIO_GUNIT (FLUNIT, STATUS)

               OFILE = ' '
               LOFILE = 0

               IF (CNAME .NE. ' ') THEN
                  LCNAME = CHR_LEN(CNAME)
               ELSE
                  LCNAME = 1
               END IF

               CALL CHR_PUTC (CNAME(1 : LCNAME), OFILE, LOFILE)
               CALL CHR_PUTC ('.lis', OFILE, LOFILE)

               OPEN(UNIT=FLUNIT, FILE=OFILE(1 : LOFILE), STATUS='NEW',
     :           IOSTAT=LSTAT)
               CALL FIO_SERR (LSTAT, STATUS)
            END IF

*
*          Determine the mode to be used and set the flags appropriately.

            CALL PAR_CHOIC ('DETAIL', 'SUMMARY',
     :        'SUMMARY,COLUMNS,PARAMETERS,TEXT,AST,FULL', .FALSE.,
     :        DETAIL, STATUS)

            SUMFLG = .FALSE.
            COLFLG = .FALSE.
            PARFLG = .FALSE.
            TXTFLG = .FALSE.
            ASTFLG = .FALSE.
            FULFLG = .FALSE.

            IF (DETAIL .EQ. 'SUMMARY') THEN
               SUMFLG = .TRUE.
            ELSE IF (DETAIL .EQ. 'COLUMNS') THEN
               COLFLG = .TRUE.
            ELSE IF (DETAIL .EQ. 'PARAMETERS') THEN
               PARFLG = .TRUE.
            ELSE IF (DETAIL .EQ. 'TEXT') THEN
               TXTFLG = .TRUE.
            ELSE IF (DETAIL .EQ. 'AST') THEN
               ASTFLG = .TRUE.
            ELSE IF (DETAIL .EQ. 'FULL') THEN
               SUMFLG = .TRUE.
               COLFLG = .TRUE.
               PARFLG = .TRUE.
               TXTFLG = .TRUE.
               ASTFLG = .TRUE.
               FULFLG = .TRUE.
            END IF

*
*          If required then list the summary details.

            IF (SUMFLG) THEN
               CALL CAP_LSTSM (CI, OFLAG, FLUNIT, .FALSE., CNAME,
     :           STATUS)

*
*             Add the list of column names if only a summary is being
*             produced.

               IF (.NOT. FULFLG) THEN
                  CALL CAP_LSTNM (CI, OFLAG, FLUNIT, .FALSE., STATUS)
               END IF

*
*             Also write the summary details to environment variables.

               CALL CAP_ENVSM (CI, STATUS)
            END IF

*
*          If required then list the details of the columns.

            IF (COLFLG) THEN
               CALL CAP_LSTCL (CI, OFLAG, FLUNIT, .FALSE., STATUS)
            END IF

*
*          If required then list the details of the parameters.

            IF (PARFLG) THEN
               CALL CAP_LSTPR (CI, OFLAG, FLUNIT, .FALSE., STATUS)
            END IF

*
*          If required then list the textual information.

            IF (TXTFLG) THEN
               CALL CAP_LSTTX (CI, OFLAG, FLUNIT, .FALSE., STATUS)
            END IF

*
*          If required then list the AST details.

            IF (ASTFLG) THEN
               CALL CAP_LSTAS (CI, OFLAG, FLUNIT, .FALSE., STATUS)
            END IF

*
*          If an output file is required then close it.

            IF (FILE) THEN
               CLOSE(UNIT=FLUNIT, IOSTAT=LSTAT)
               CALL FIO_SERR (LSTAT, STATUS)
            END IF

*
*          Release the catalogue identifier.

            CALL CAT_TRLSE (CI, STATUS)
         END IF

*
*       Report any error.

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETC ('CNAME', CNAME)
            CALL ERR_REP ('CATHEADER_ERR', 'Error listing the header '/
     :        /'for catalogue ^CNAME.', STATUS)
         END IF

      END IF

      END
