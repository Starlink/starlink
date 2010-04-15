      SUBROUTINE SURF_RECURSE_READ( RLEV, NAME, MAX_FILE,
     :     OUT_COORDS, N_FILE, N_BOL, N_POS, N_INTS, N_MEAS,
     :     IN_UT1, IN_RA_CEN, IN_DEC_CEN, FITS, N_FITS, WAVELENGTH,
     :     SUB_INSTRUMENT, OBJECT, UTDATE, UTSTART, FILENAME,
     :     BOL_ADC, BOL_CHAN,
     :     BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR,
     :     BOL_DEC_END, DATA_PTR, DATA_END, VARIANCE_PTR, VARIANCE_END,
     :     QMF, QUALITY_PTR, QUALITY_END, QBITS, ANG_INT, ANG_MEAS,
     :     INT_LIST, MEAS_LIST, BOLWT, WEIGHT, SHIFT_DX, SHIFT_DY,
     :     NPARS, PARS, STATUS)
*+
*  Name:
*     SURF_RECURSE_READ

*  Purpose:
*     Allow the recursive read of REBIN text files and NDFs

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SURF_RECURSE_READ( RLEV, NAME, MAX_FILE,
*     :     OUT_COORDS, N_FILE, N_BOL, N_POS, N_INTS,
*     :     IN_UT1, IN_RA_CEN, IN_DEC_CEN, FITS, N_FITS, WAVELENGTH,
*     :     SUB_INSTRUMENT, OBJECT, UTDATE, UTSTART, FILENAME,
*     :     BOL_ADC, BOL_CHAN,
*     :     BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR,
*     :     BOL_DEC_END, DATA_PTR, DATA_END, VARIANCE_PTR, VARIANCE_END,
*     :     QMF, QUALITY_PTR, QUALITY_END, QBITS, ANG_INT, ANG_MEAS,
*     :     INT_LIST, MEAS_LIST, BOLWT, WEIGHT, SHIFT_DX, SHIFT_DY,
*     :     NPARS, PARS, STATUS)

*  Description:
*     This routine takes an input name and reads in all NDFs resulting
*     from this name. SCUBA sections are parsed. Text files are expanded
*     into NDF+parameters. Nested text files are allowed.

*  Arguments:
*     RLEV = INTEGER (Given)
*        Recursion level. This is the number of times that the routine
*        has called itself. A value of 1 should be passed in by the
*        external routine.
*     NAME = CHAR (Given)
*        Name of input file. This may also contain scuba section
*        specifications. The input file can be a NDF or a text file
*        containing a list of NDFs.
*     MAX_FILE = INTEGER (Given)
*        Maximum number of NDFs that can be read by the system.
*     OUT_COORDS = CHAR (Given)
*        Output coordinates system. (Passed into SURF_READ_REBIN_NDFS)
*     N_FILE = INTEGER (Given & Returned)
*        Current file number (less than MAX_FILE and greater than 0).
*        This counter is incremented when an NDF has been read successfully.
*     N_BOL( MAX_FILE ) = INTEGER (Returned)
*        Number of bolometers associated with each file
*     N_POS( MAX_FILE ) = INTEGER (Returned)
*        Number of samples associated with each file
*     N_INTS( MAX_FILE ) = INTEGER (Returned)
*        Total Number of integrations associated with each file (INT*MEAS)
*     N_MEAS( MAX_FILE ) = INTEGER (Returned)
*        Number of measurements associated with each file
*     IN_UT1( MAX_FILE ) = DOUBLE (Returned)
*        Modified Julian data of observation for each file
*     IN_RA_CEN( MAX_FILE ) = DOUBLE (Returned)
*        RA of centre for each file
*     IN_DEC_CEN( MAX_FILE ) = DOUBLE (Returned)
*        Dec of centre for each file
*     FITS ( N_FITS, MAX_FILE) = CHARACTER*(80) (Returned)
*        FITS entries for each file
*     N_FITS = INTEGER (Given)
*        Size of FITS array for each file
*     WAVELENGTH = REAL (Given & Returned)
*        Wavelength of map
*     SUB_INSTRUMENT = CHAR (Given & Returned)
*        Sub instrument of map
*     OBJECT( MAX_FILE ) = CHAR (Returned)
*        Name of object in each file
*     UTDATE( MAX_FILE ) = CHAR (Returned)
*        UT date of each observation
*     UTSTART( MAX_FILE ) = CHAR (Returned)
*        UT time of each observation
*     FILENAME( MAX_FILE ) = CHAR (Returned)
*        Actual filename of each file read.
*     BOL_ADC ( ) = INTEGER (Returned)
*        ADC information for bolometers - only used by BOLREBIN
*     BOL_CHAN ( ) = INTEGER (Returned)
*        Channel information for bolometers - only used by BOLREBIN
*     BOL_RA_PTR( MAX_FILE ) = INTEGER (Returned)
*        Array of pointers to RA bolometer positions read from each file
*     BOL_RA_END( MAX_FILE ) = INTEGER (Returned)
*        Array of pointers to end of RA bolometer positions read from each file
*     BOL_DEC_PTR( MAX_FILE ) = INTEGER (Returned)
*        Array of pointers to DEC bolometer positions read from each file
*     BOL_DEC_END( MAX_FILE ) = INTEGER (Returned)
*        Array of pointers to end of DEC bol positions read from each file
*     DATA_PTR( MAX_FILE ) = INTEGER (Returned)
*        Array of pointers to data values read from each file
*     DATA_END( MAX_FILE ) = INTEGER (Returned)
*        Array of pointers to end of data values
*     VARIANCE_PTR( MAX_FILE ) = INTEGER (Returned)
*        Array of pointers to variance values read from each file
*     VARIANCE_END( MAX_FILE ) = INTEGER (Returned)
*        Array of pointers to end of variance values
*     QMF = LOGICAL (Given)
*        Flag to decide whether quality is being stored (.FALSE.) or
*        being folded into the data array (.true.). See NDF_SQMF
*     QUALITY_PTR(MAX_FILE) = INTEGER (Returned)
*        Pointer to quality array
*     QUALITY_END(MAX_FILE) = INTEGER (Returned)
*        Pointer to end of quality array
*     QBITS(MAX_FILE) = BYTE (Returned)
*        Bad bits mask for each file
*     ANG_INT( MAX_FILE, SCUBA__MAX_INT,2)  = REAL (Returned)
*        Array containing the polarimetry angles for each integration
*        The 2 dimensions are for WPLATE and ANGROT
*     ANG_MEAS( MAX_FILE, SCUBA__MAX_MEAS,2) = REAL (Returned)
*        Array containing the pol angles for each measurement
*        The 2 dimensions are for WPLATE and ANGROT
*     INT_LIST( MAX_FILE, SCUBA__MAX_INT+1) = INTEGER (Returned)
*        Position of integrations in each data file
*     MEAS_LIST(MAX_FILE, SCUBA__MAX_MEAS+1) = INTEGER (Returned)
*        Position of measurements in each data file
*     BOLWT (max num of bols, MAX_FILE) = REAL (Returned)
*        Relative Weights of each bolometer for each file
*     WEIGHT( MAX_FILE ) = REAL (Returned)
*        Weight of each input file
*     SHIFT_DX( MAX_FILE ) = REAL (Returned)
*        X Shift of each input file
*     SHIFT_DY( MAX_FILE ) = REAL (Returned)
*        Y Shift of each input file
*     NPARS = INTEGER (Given)
*        Number of parameters in PARS array.
*     PARS( NPARS ) = REAL (Given)
*        Values of input parameters. 1: WEIGHT, 2: SHIFT_DX, 3: SHIFT_DY
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Algorithm:
*     This routine first separates the filename from any SCUBA sections.
*     Then the file is opened as an NDF. If the read is successful the
*     input data and parameters are read and the subroutine is exited.
*     If the NDF open fails, the subroutine attempts to open the file
*     as a text file. On failure the routine returns with bad status.
*     If the text file is opened successfully each line is read from the
*     file in turn - Non comment lines are passed to this routine
*     so that the data can be read in as an NDF.
*     Parameters are read from the file if they are found and then passed
*     to this routine as arguments.
*     A limit of 5 recursion calls is imposed.

*  Notes:
*     - This subroutine can be called recursively. A limit of 5 recursion
*       levels is imposed. Note that recursion is not approved of in
*       Fortran 77 (mainly because local variables remember their state
*       on entry!).
*     - Text files with extension .txt are converted to NDFs by the
*       NDF_OPEN command. This is bad - do not use .txt files as batch
*       files.


*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL: John Lightfoot (RoE)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 May 12 (TIMJ)
*       Initial version removed from reds_wtfn_rebin.f

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SURF_PAR'         ! SURF
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants

*  Local Constants:
      CHARACTER*25     TSKNAME   ! Name of task
      PARAMETER (TSKNAME = 'SURF_RECURSE_READ')

*  Arguements Given:
      INTEGER          MAX_FILE
      CHARACTER*(*)    NAME
      CHARACTER*(*)    OUT_COORDS
      LOGICAL          QMF

*  Arguments Given & Returned:
      INTEGER          N_FILE
      CHARACTER*(*)    SUB_INSTRUMENT
      REAL             WAVELENGTH

*  Arguments Returned:
      REAL             ANG_INT(MAX_FILE,SCUBA__MAX_INT, 2)
      REAL             ANG_MEAS(MAX_FILE,SCUBA__MAX_MEAS, 2)
      REAL             BOLWT (SCUBA__NUM_CHAN * SCUBA__NUM_ADC,
     :     MAX_FILE)
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
      INTEGER          BOL_DEC_END(MAX_FILE)
      INTEGER          BOL_DEC_PTR(MAX_FILE)
      INTEGER          BOL_RA_END(MAX_FILE)
      INTEGER          BOL_RA_PTR(MAX_FILE)
      INTEGER          DATA_END(MAX_FILE)
      INTEGER          DATA_PTR(MAX_FILE)
      CHARACTER*(*)    FILENAME(MAX_FILE)
      CHARACTER*(*)    FITS(SCUBA__MAX_FITS, MAX_FILE)
      INTEGER          INT_LIST(MAX_FILE, SCUBA__MAX_INT + 1)
      DOUBLE PRECISION IN_DEC_CEN(MAX_FILE)
      DOUBLE PRECISION IN_RA_CEN(MAX_FILE)
      DOUBLE PRECISION IN_UT1(MAX_FILE)
      INTEGER          MEAS_LIST(MAX_FILE, SCUBA__MAX_MEAS + 1)
      INTEGER          N_BOL(MAX_FILE)
      INTEGER          N_FITS(MAX_FILE)
      INTEGER          N_INTS(MAX_FILE)
      INTEGER          N_MEAS(MAX_FILE)
      INTEGER          N_POS(MAX_FILE)
      INTEGER          NPARS
      REAL             PARS(3) ! I know there are 3 parameters
      CHARACTER*(*)    OBJECT(MAX_FILE)
      INTEGER          QUALITY_END(MAX_FILE)
      INTEGER          QUALITY_PTR(MAX_FILE)
      BYTE             QBITS(MAX_FILE)
      INTEGER          RLEV    ! Recursion level
      REAL             SHIFT_DX(MAX_FILE)
      REAL             SHIFT_DY(MAX_FILE)
      CHARACTER*(*)    UTDATE(MAX_FILE)
      CHARACTER*(*)    UTSTART(MAX_FILE)
      INTEGER          VARIANCE_END(MAX_FILE)
      INTEGER          VARIANCE_PTR(MAX_FILE)
      REAL             WEIGHT(MAX_FILE)

*  Status
      INTEGER          STATUS

*  Local constants:
      INTEGER          MAX_RECURS      ! Maximum recursion depth
      PARAMETER (MAX_RECURS = 5)

*  Local Variables:
      CHARACTER * (40) DATA_SPEC(SCUBA__MAX_SECT) ! Array of data specs
      CHARACTER * (132)FNAME           ! Temporary storage of file name
      INTEGER          FD(MAX_RECURS)  ! File IO identifier
      INTEGER          FIOSTATUS(MAX_RECURS) ! Status for file IO
      INTEGER          IN_NDF          ! NDF index of input file
      INTEGER          ITEMP           ! scratch integer
      CHARACTER * (132)LINE            ! Line read from file
      INTEGER          NSPEC           ! Number of sections in specifier
      INTEGER          N_FOUND         ! Number of parameters in line
      CHARACTER * (132)SNAME           ! File name read from file
      REAL             T_SHIFT_DX      ! X Shift read from file
      REAL             T_SHIFT_DY      ! Y Shift read from file
      REAL             T_WEIGHT        ! Weight read from file
      LOGICAL          USE_SECTION     ! Are we using the section or invers

*.

*     Check status on entry
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_SETI('LEV',RLEV)
         CALL ERR_REP(' ','Error entering RECURSE, Lev = ^LEV', STATUS)
         RETURN
      END IF

*     Check that the recursion level is okay
      IF (RLEV .GT. MAX_RECURS) THEN
         CALL MSG_SETI ('MAX', MAX_RECURS)
         STATUS = SAI__ERROR
         CALL MSG_SETC('TASK', TSKNAME)
         CALL ERR_REP (' ', '^TASK: recursion depth exceeds '//
     :        'maximum depth of ^MAX levels', STATUS)
         RETURN
      END IF

*     Check that recursion level is not too low
      IF (RLEV .LT. 0) THEN
         CALL MSG_SETI('MIN', RLEV)
         STATUS = SAI__ERROR
         CALL MSG_SETC('TASK', TSKNAME)
         CALL ERR_REP(' ','^TASKL recursion depth is too low (^MIN)',
     :        STATUS)
      END IF

*     Check that we have not exceeded the allowed number of files

      IF (N_FILE .GT. MAX_FILE) THEN

         CALL MSG_SETI ('MAX', MAX_FILE)
         STATUS = SAI__ERROR
         CALL MSG_SETC('TASK', TSKNAME)
         CALL ERR_REP (' ', '^TASK: number of '//
     :        'files read exceeds maximum allowed - ^MAX', STATUS)

      END IF


*     First task is to split the name up into a filename
*     and a SCUBA section

      USE_SECTION = .TRUE.

      CALL SCULIB_SPLIT_FILE_SPEC(NAME, SCUBA__MAX_SECT, FNAME, NSPEC,
     :     DATA_SPEC, USE_SECTION, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

*     open the data NDF
         ITEMP = 0

         IN_NDF = NDF__NOID

         CALL NDF_FIND (DAT__ROOT, FNAME, IN_NDF, STATUS)

*     If the status from this action is good then proceed to read
*     the file. If it is bad then clear status and assume that we
*     are dealing with an ASCII text file.

         IF (STATUS .EQ. SAI__OK) THEN

*     If we are below level 1 of recursion print a message telling
*     the user that we are opening a file

            IF (RLEV .GT. 1) THEN
               CALL MSG_SETC('NAME',FNAME)
               CALL MSG_SETC('PKG',PACKAGE)
               CALL MSG_OUTIF(MSG__NORM,' ','^PKG: Reading file '//
     :              '^NAME', STATUS)
            END IF

*     Read in the NDF
            CALL SURF_READ_REBIN_NDF( IN_NDF, MAX_FILE,
     :           NSPEC, DATA_SPEC, OUT_COORDS, N_FILE, USE_SECTION,
     :           N_BOL(N_FILE), N_POS(N_FILE), N_INTS(N_FILE),
     :           N_MEAS(N_FILE),
     :           1, IN_UT1(1), IN_UT1(N_FILE), IN_RA_CEN(N_FILE),
     :           IN_DEC_CEN(N_FILE), FITS(1, N_FILE), N_FITS(N_FILE),
     :           WAVELENGTH, SUB_INSTRUMENT,
     :           OBJECT(N_FILE), UTDATE(N_FILE), UTSTART(N_FILE),
     :           BOL_ADC, BOL_CHAN, BOL_RA_PTR(N_FILE),
     :           BOL_RA_END(N_FILE), BOL_DEC_PTR(N_FILE),
     :           BOL_DEC_END(N_FILE), DATA_PTR(N_FILE),
     :           DATA_END(N_FILE), VARIANCE_PTR(N_FILE),
     :           VARIANCE_END(N_FILE), QMF, QUALITY_PTR(N_FILE),
     :           QUALITY_END(N_FILE), QBITS(N_FILE),
     :           .FALSE., 0, ANG_INT, ANG_MEAS, INT_LIST, MEAS_LIST,
     :           BOLWT(1,N_FILE), STATUS)

            CALL MSG_BLANK(STATUS)

*     If this was all successful then I can ask for the other parameters
*     and increment the FILE counter
            IF (STATUS .EQ. SAI__OK) THEN

               IF (NPARS .GT. 0) THEN
                  WEIGHT(N_FILE) = PARS(1)
               ELSE
                  CALL PAR_GET0R ('WEIGHT', WEIGHT(N_FILE), STATUS)
               END IF

               IF (NPARS .GT. 1) THEN
                  SHIFT_DX(N_FILE) = PARS(2)
               ELSE
                  CALL PAR_GET0R ('SHIFT_DX', SHIFT_DX(N_FILE), STATUS)
               END IF

               IF (NPARS .GT. 2) THEN
                  SHIFT_DY(N_FILE) = PARS(3)
               ELSE
                  CALL PAR_GET0R ('SHIFT_DY', SHIFT_DY(N_FILE), STATUS)
               END IF

               IF (STATUS .EQ. SAI__OK) THEN
                  SHIFT_DX(N_FILE) = SHIFT_DX(N_FILE) / REAL (R2AS)
                  SHIFT_DY(N_FILE) = SHIFT_DY(N_FILE) / REAL (R2AS)
               END IF

               CALL PAR_CANCL ('WEIGHT', STATUS)
               CALL PAR_CANCL ('SHIFT_DX', STATUS)
               CALL PAR_CANCL ('SHIFT_DY', STATUS)

               FILENAME(N_FILE) = FNAME

*     Dont increment file if status was bad through this
*     eg someone has returned PAR__NULL for a parameter
               IF (STATUS .EQ. SAI__OK) THEN
                  N_FILE = N_FILE + 1
               END IF

            ELSE

               CALL MSG_SETC('FILE', FNAME)
               CALL ERR_REP(' ', 'RECURSE_READ: Error whilst reading'//
     :              ' the NDF ^FILE', STATUS)

            END IF

*     At this point we have finished with the input NDF. We need to
*     annul the identifier regardless of STATUS otherwise it will never
*     get freed and the filename will not even be usable for output
*     later on.

            CALL NDF_ANNUL(IN_NDF, STATUS)

         ELSE
*     Annul global status
            CALL ERR_ANNUL(STATUS)

            FD(RLEV) = 0

*     Open as a text file and read that

            FIOSTATUS(RLEV) = SAI__OK
            CALL FIO_OPEN(FNAME,'READ','LIST',0,FD(RLEV),
     :           FIOSTATUS(RLEV))

            IF (FIOSTATUS(RLEV) .EQ. SAI__OK) THEN

*     Now read in each line at a time

               DO WHILE ((FIOSTATUS(RLEV) .EQ. SAI__OK) .AND.
     :              (STATUS .EQ. SAI__OK))

                  LINE = '#'
                  CALL FIO_READ(FD(RLEV), LINE, ITEMP, FIOSTATUS(RLEV))

*     Left justify line
                  CALL CHR_LDBLK(LINE)

*     Parse the line into the relevant name and parameters
*     I assume that the number of values found in the line is the
*     number of parameters specified (ie assume the order in the line
*     is fixed).

                  N_FOUND = 0
                  CALL SCULIB_DECODE_REBIN_LINE(LINE, N_FOUND,
     :                 SNAME, T_WEIGHT, T_SHIFT_DX,
     :                 T_SHIFT_DY, STATUS)

*     Only read an NDF if at least one parameter was found on the line
*     (And assume the first is a filename)

                  IF (N_FOUND .GT. 0) THEN

*     Set the parameters
                     IF (N_FOUND .GT. 1) PARS(1) = T_WEIGHT
                     IF (N_FOUND .GT. 2) PARS(2) = T_SHIFT_DX
                     IF (N_FOUND .GT. 3) PARS(3) = T_SHIFT_DY

                     NPARS = N_FOUND - 1

*     Call myself to read the NDF (or further text file)

                     RLEV = RLEV + 1

                     CALL SURF_PSEUDO_RECURSE( RLEV, SNAME,
     :                    MAX_FILE, OUT_COORDS, N_FILE, N_BOL, N_POS,
     :                    N_INTS, N_MEAS, IN_UT1, IN_RA_CEN,
     :                    IN_DEC_CEN, FITS, N_FITS, WAVELENGTH,
     :                    SUB_INSTRUMENT, OBJECT, UTDATE,
     :                    UTSTART, FILENAME, BOL_ADC, BOL_CHAN,
     :                    BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR,
     :                    BOL_DEC_END, DATA_PTR, DATA_END,
     :                    VARIANCE_PTR, VARIANCE_END,
     :                    QMF, QUALITY_PTR, QUALITY_END, QBITS,
     :                    ANG_INT, ANG_MEAS,
     :                    INT_LIST, MEAS_LIST, BOLWT, WEIGHT, SHIFT_DX,
     :                    SHIFT_DY,  NPARS, PARS,
     :                    STATUS)

                     RLEV = RLEV - 1

                  END IF

               END DO

*     Annul file descriptor and status
               CALL FIO_CLOSE(FD(RLEV), FIOSTATUS(RLEV))
               IF (FIOSTATUS(RLEV) .NE. SAI__OK)
     :              CALL ERR_ANNUL(FIOSTATUS(RLEV))

            ELSE
               CALL MSG_SETC('TASK',TSKNAME)
               CALL MSG_SETC('FILE',FNAME)
               CALL ERR_REP(' ','^TASK: Error opening file ^FILE',
     :              FIOSTATUS(RLEV))
               CALL ERR_FLUSH(FIOSTATUS(RLEV))

            END IF

         END IF
      END IF

      END


      SUBROUTINE SURF_PSEUDO_RECURSE( RLEV, SNAME, MAX_FILE,
     :     OUT_COORDS, N_FILE, N_BOL, N_POS, N_INTS, N_MEAS,
     :     IN_UT1, IN_RA_CEN, IN_DEC_CEN, FITS, N_FITS, WAVELENGTH,
     :     SUB_INSTRUMENT, OBJECT, UTDATE, UTSTART, FILENAME,
     :     BOL_ADC, BOL_CHAN,
     :     BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR,
     :     BOL_DEC_END, DATA_PTR, DATA_END, VARIANCE_PTR, VARIANCE_END,
     :     QMF, QUALITY_PTR, QUALITY_END, QBITS, ANG_INT, ANG_MEAS,
     :     INT_LIST, MEAS_LIST, BOLWT, WEIGHT, SHIFT_DX, SHIFT_DY,
     :     NPARS, PARS,
     :     STATUS)
*+
*  Name:
*     SURF_PSEUDO_RECURSE

*  Purpose:
*     Fool the alpha compiler into thinking that I am not recursing

*  Language:
*     Starlink Fortran 77 (+ recursion)

*  Invocation:
*     CALL SURF_PSEUDO_RECURSE( RLEV, SNAME, MAX_FILE,
*    :     OUT_COORDS, N_FILE, N_BOL, N_POS, N_INTS, N_MEAS,
*    :     IN_UT1, IN_RA_CEN, IN_DEC_CEN, FITS, N_FITS, WAVELENGTH,
*    :     SUB_INSTRUMENT, OBJECT, UTDATE, UTSTART, FILENAME,
*    :     BOL_ADC, BOL_CHAN, BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR,
*    :     BOL_DEC_END, DATA_PTR, DATA_END, VARIANCE_PTR, VARIANCE_END,
*    :     QMF, QUALITY_PTR, QUALITY_END, QBITS, ANG_INT, ANG_MEAS,
*    :     INT_LIST, MEAS_LIST, BOLWT, WEIGHT, SHIFT_DX, SHIFT_DY,
*    :     NPARS, PARS, STATUS)

*  Description:
*     This routine is called from SURF_RECURSE_READ and immediately
*     calls SURF_RECURESE_READ. Recursion via an intermediary.

*  Author:
*     TIMJ: Tim Jenness (timj@jach.hawaii.edu)

*  History:
*     May 1997 (TimJ)
*        Original Version

*-
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INCLUDE 'SURF_PAR'


*  Arguements Given:
      INTEGER          MAX_FILE
      CHARACTER*(*)    SNAME
      CHARACTER*(*)    OUT_COORDS
      LOGICAL          QMF

*  Arguments Given & Returned:
      INTEGER          N_FILE
      CHARACTER*(*)    SUB_INSTRUMENT
      REAL             WAVELENGTH

*  Arguments Returned:
      REAL             ANG_INT(MAX_FILE,SCUBA__MAX_INT, 2)
      REAL             ANG_MEAS(MAX_FILE,SCUBA__MAX_MEAS, 2)
      REAL             BOLWT (SCUBA__NUM_CHAN * SCUBA__NUM_ADC,
     :     MAX_FILE)
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
      INTEGER          BOL_DEC_END(MAX_FILE)
      INTEGER          BOL_DEC_PTR(MAX_FILE)
      INTEGER          BOL_RA_END(MAX_FILE)
      INTEGER          BOL_RA_PTR(MAX_FILE)
      INTEGER          DATA_END(MAX_FILE)
      INTEGER          DATA_PTR(MAX_FILE)
      CHARACTER*(*)    FILENAME(MAX_FILE)
      CHARACTER*(*)    FITS(SCUBA__MAX_FITS, MAX_FILE)
      INTEGER          INT_LIST(MAX_FILE, SCUBA__MAX_INT + 1)
      DOUBLE PRECISION IN_DEC_CEN(MAX_FILE)
      DOUBLE PRECISION IN_RA_CEN(MAX_FILE)
      DOUBLE PRECISION IN_UT1(MAX_FILE)
      INTEGER          MEAS_LIST(MAX_FILE, SCUBA__MAX_MEAS + 1)
      INTEGER          N_BOL(MAX_FILE)
      INTEGER          N_FITS(MAX_FILE)
      INTEGER          N_INTS(MAX_FILE)
      INTEGER          N_MEAS(MAX_FILE)
      INTEGER          N_POS(MAX_FILE)
      INTEGER          NPARS
      REAL             PARS(3) ! I know there are 3 parameters
      CHARACTER*(*)    OBJECT(MAX_FILE)
      INTEGER          QUALITY_END(MAX_FILE)
      INTEGER          QUALITY_PTR(MAX_FILE)
      BYTE             QBITS (MAX_FILE)
      INTEGER          RLEV    ! Recursion level
      REAL             SHIFT_DX(MAX_FILE)
      REAL             SHIFT_DY(MAX_FILE)
      CHARACTER*(*)    UTDATE(MAX_FILE)
      CHARACTER*(*)    UTSTART(MAX_FILE)
      INTEGER          VARIANCE_END(MAX_FILE)
      INTEGER          VARIANCE_PTR(MAX_FILE)
      REAL             WEIGHT(MAX_FILE)

*     Status
      INTEGER STATUS
*.

      IF (STATUS .NE. SAI__OK) RETURN

      CALL SURF_RECURSE_READ( RLEV, SNAME, MAX_FILE,
     :     OUT_COORDS, N_FILE, N_BOL, N_POS,
     :     N_INTS, N_MEAS, IN_UT1, IN_RA_CEN,
     :     IN_DEC_CEN, FITS, N_FITS, WAVELENGTH,
     :     SUB_INSTRUMENT, OBJECT, UTDATE,
     :     UTSTART, FILENAME, BOL_ADC, BOL_CHAN,
     :     BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR,
     :     BOL_DEC_END, DATA_PTR, DATA_END,
     :     VARIANCE_PTR, VARIANCE_END,
     :     QMF, QUALITY_PTR, QUALITY_END, QBITS, ANG_INT,
     :     ANG_MEAS, INT_LIST, MEAS_LIST,BOLWT, WEIGHT, SHIFT_DX,
     :     SHIFT_DY,  NPARS, PARS,
     :     STATUS)

      END


