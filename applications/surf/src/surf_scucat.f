      SUBROUTINE REDS_SCUCAT (STATUS)
*+
*  Name:
*     SCUCAT

*  Purpose:
*     Routine to concatenate photometry datasets for further processing

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL REDS_SCUCAT( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This routine reads in a list of user specified files and concatenates
*     their data, variance and quality arrays so that KAPPA routines like
*     stats and kstest can analyse the complete set of photometry observations.
*     Data for each individual bolometer is written to a different file.
*     If a file contained data for H7 and H9 then two output files would
*     be created (eg test_h7 and test_h9 - if the OUT parameter was set to
*     'test'). For each new bolometer a new
*     file is created. Data for existing bolometers are appended. New files
*     are created each time SCUCAT is run (ie existing files are over-written).
*

*  Usage:
*     scucat out in

*  ADAM Parameters:
*     IN = NDF (Read)
*        The input dataset(s). This parameter is requested repeatedly
*        until a NULL (!) value is given.
*     OUT = _CHAR (Write)
*        The root name of the output NDF. 

*  Examples:
*     scucat in=phot out=test
*        This routine will copy the data from phot to test, reducing multiple
*        bolometers to indiviual files. If the input set contained data
*        for bolometer H7 the output file will be test_h7.sdf.
*        The program will then ask for another data set.

*  Notes:
*     This routine is necessary since the output file from SCUPHOT contains
*     an NDF for each bolometer used during the photometry observation.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}

*  History:
*     $Id$
*     $Log$
*     Revision 1.13  1997/03/31 21:18:29  timj
*     Use PACKAGE and TSKNAME.
*
*     Revision 1.12  1997/01/17 00:32:36  timj
*     Spell fixes to header.
*
c Revision 1.11  1997/01/09  18:35:02  timj
c Write a new NDF for each bolometer found in input files.
c
c Revision 1.10  1996/12/06  18:31:59  timj
c Now recognises multiple bolometers in file and writes out all bolometer
c data to concatenated data.
c
c Revision 1.9  1996/11/02  01:42:34  timj
c Fix bug in Author/History header
c
c Revision 1.8  1996/11/02  01:24:00  timj
c Change Name to SCUCAT (from REDS_SCUCAT)
c
c Revision 1.7  1996/10/30  20:23:05  timj
c Add modern STARLINK header.
c Replace SCULIB_COPY? with VEC_
c Annul LOC after use.
c
c Revision 1.6  1996/10/24  21:29:09  timj
c Fixed GLOBAL default problem (use DUMMY PARAMETER)
c
c Revision 1.5  1996/10/19  00:07:04  timj
c Use GLOBAL.sdf and DAT_ASSOC, remove FILENAME(FILE)
c
c     Revision 1.4  1996/10/17  18:13:49  timj
c     Open OUT after reporting information on first input file
c     
c     Revision 1.3  1996/09/18  19:14:18  timj
c     Change from CONCAT to SCUCAT
c     
c     Revision 1.2  1996/09/18  02:16:51  timj
c     Add bad bit mask
c     
c     Revision 1.1  1996/09/18  02:02:07  timj
c     Initial revision
c     
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
 
*-

*     Type definitions:

      IMPLICIT NONE              ! No implicit typing allowed


*     Global constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'REDS_SYS'         ! SCUBA constants

*     Status:
      INTEGER STATUS

*     External references:
      INTEGER  CHR_LEN           ! Length of string
      EXTERNAL CHR_LEN

*     Local constants:
      INTEGER MAXCMP             ! Max number of bolometers in an HDS
      PARAMETER (MAXCMP = 15)
      INTEGER MAXBOLS            ! Max number of bolometers used
      PARAMETER (MAXBOLS = 132)
      CHARACTER * 10 TSKNAME     ! Name of task
      PARAMETER (TSKNAME = 'SCUCAT')

*     Local variables:
      BYTE          BADBIT       ! Bad bit mask
      CHARACTER*3  BOL(MAXCMP)   ! Name of bolometers present in NDF
      INTEGER      BOL_INDEX(MAXCMP)! Which entry in BOLUSED is the current bol
      CHARACTER*3  BOLUSED(MAXBOLS) ! Name of bolometers used so far
      CHARACTER*(MAXCMP*4) BOL_LIST ! List of all bolometers in NDF
      INTEGER       EL           ! Number of input data points
      INTEGER       FILE         ! File count
      CHARACTER *132 FILENAME    ! Filename
      LOGICAL       FILEMINONE   ! Have I decremented the file counter yet?
      INTEGER       I            ! Loop counter
      INTEGER       IERR         ! Location of error during VEC_ copy
      INTEGER       INDEX        ! Bolometer reference in big bol list
      INTEGER       IN_NDF(MAXCMP)! Input NDF identifiers
      INTEGER       IN_DATA_PTR  ! Pointer to D array
      INTEGER       IN_VAR_PTR   ! Pointer to V array
      INTEGER       IN_QUAL_PTR  ! Pointer to Q array
      INTEGER       IPAR         ! Parameter ID
      INTEGER       IPOSN        ! Position in string
      INTEGER       ITEMP        ! Temporary integer
      INTEGER       LBND(MAXBOLS)! Lower bound of output array
      CHARACTER*(DAT__SZLOC) LOC ! Locator to root HDS file
      LOGICAL       LOOPING      ! Controls read loop
      INTEGER       LOWER        ! Lower bound of section
      CHARACTER*10  MODE         ! Access mode for output
      INTEGER       N            ! Bolometer counter
      CHARACTER*15  NAME(MAXCMP) ! Names of NDFs
      INTEGER       NBOLUSED     ! Number of bolometers catted
      INTEGER       NCOMP        ! Number of components in HDS
      INTEGER       NERR         ! Number of errors during VEC_ copy
      CHARACTER*(DAT__SZLOC) NLOC! Loc to  Bol NDF inside HDS file
      INTEGER       NUM_NDF      ! Counter of each sub NDF
      INTEGER       N_PHOT       ! Number of bolometers in an HDS
      CHARACTER*(132) OUTFILE    ! Name of output file
      CHARACTER*(132) OUTROOT    ! Rootname of output file
      INTEGER       OUTPLACE     ! Output place holder
      INTEGER       OUT_NDF(MAXBOLS)! Output NDF identifiers
      INTEGER       OUT_APTR     ! Pointer to Axis array
      INTEGER       OUT_DATA_PTR ! Pointer to D array
      INTEGER       OUT_VAR_PTR  ! Pointer to V array
      INTEGER       OUT_QUAL_PTR ! Pointer to Q array
      CHARACTER*(DAT__SZLOC) PLOC(MAXCMP) ! Locs to PEAK NDFs
      LOGICAL       READING      ! Logical to control file reading
      INTEGER       SEC_NDF      ! NDF identifier to NDF section
      CHARACTER*40  TITLE        ! Title of observation
      LOGICAL       THERE        ! A bolometer has already been catted
      INTEGER       UBND(MAXBOLS)! Upper bound of output array
      LOGICAL       USE_OUT      ! Have I opened an output file
*.

      IF (STATUS .NE. SAI__OK) RETURN

*     start up the NDF system and read in the input photometry data
      
      CALL NDF_BEGIN
      
      USE_OUT = .FALSE.
      READING = .TRUE.
      LOOPING = .TRUE.
      FILE = 0
      NBOLUSED = 0

* Initialise the output NDFs
      DO I = 1, MAXBOLS
         OUT_NDF(I) = NDF__NOID
      END DO

* Ask for the output root name
      CALL PAR_GET0C('OUT', OUTROOT, STATUS)

* Start loop
      DO WHILE (LOOPING)

         FILE = FILE + 1
         
*     read the name of the file to be read
         
*     Read in the GLOBAL value first
         IF (FILE .EQ. 1) THEN
            CALL SUBPAR_FINDPAR( 'DUMMY', IPAR, STATUS)
            CALL SUBPAR_GETNAME(IPAR, FILENAME, STATUS)
            CALL PAR_DEF0C('IN', FILENAME, STATUS)
         ELSE
*           Make sure the parameter is cancelled and DEFAULT (NULL) used
            CALL PAR_CANCL('IN', STATUS)
            CALL PAR_UNSET('IN', 'DEFAULT', STATUS)
         END IF

*     Read in the latest file
*         CALL DAT_ASSOC('IN', 'READ', LOC, STATUS)
         CALL PAR_GET0C('IN', FILENAME, STATUS)
         CALL HDS_OPEN(FILENAME, 'READ', LOC, STATUS)

         IF (STATUS .EQ. PAR__NULL .OR. FILENAME.EQ.'!') THEN
            CALL ERR_ANNUL(STATUS)
            LOOPING = .FALSE.
            READING = .FALSE.
            FILE = FILE - 1
         ELSE
*     Try to open the HDS file and find out how many components are there

            N_PHOT = 0
            IF (STATUS .EQ. SAI__OK) THEN
               CALL DAT_NCOMP(LOC, NCOMP, STATUS)

               DO I = 1, MIN(NCOMP, MAXCMP)
                  CALL DAT_INDEX(LOC, I, NLOC, STATUS)

                  CALL DAT_NAME(NLOC, NAME(I), STATUS)

                  ITEMP = CHR_LEN(NAME(I))
                  CALL CHR_FIND(NAME(I), '_PEAK', .FALSE., ITEMP)

*     A photometry observation
                  IF (ITEMP .GT. 0 .AND. ITEMP.LT.CHR_LEN(NAME(I))) THEN
                     N_PHOT = N_PHOT + 1
                     BOL(N_PHOT) = NAME(I)(:ITEMP-1)
                     CALL CHR_LCASE(BOL(N_PHOT))
                     CALL DAT_CLONE(NLOC, PLOC(N_PHOT), STATUS)
                     CALL DAT_PRMRY(.TRUE., PLOC(N_PHOT), .TRUE.,STATUS)
                  END IF

                  CALL DAT_ANNUL(NLOC, STATUS)
               END DO

*     Finish with LOC
               CALL DAT_ANNUL(LOC, STATUS)
            ELSE
               CALL MSG_SETC('TASK',TSKNAME)
               CALL ERR_REP(' ', '^TASK: Failed to open file', STATUS)
               CALL ERR_ANNUL(STATUS)
            END IF

*     Have I found a photometry observation
            IF (N_PHOT .GT. 0) THEN
               
*     Sort the bolometer list

               BOL_LIST = ''
               IPOSN = 0
               DO I =  1, N_PHOT
                  IF(IPOSN.GT.0) CALL CHR_APPND(', ',BOL_LIST,IPOSN)
                  CALL CHR_APPND(BOL(I),BOL_LIST, IPOSN)
               END DO

               CALL MSG_SETC('BOL',BOL_LIST)
               CALL MSG_SETC('PKG', PACKAGE)
               CALL MSG_OUT(' ','^PKG: Found data for the following '//
     :              'bolometers: ^BOL', STATUS)

*     Find out if we already used this one
               DO I = 1, N_PHOT
                  THERE = .FALSE.
                  DO ITEMP = 1, NBOLUSED
                     IF (BOLUSED(ITEMP) .EQ. BOL(I)) THEN
                        THERE = .TRUE.
                        BOL_INDEX(I) = ITEMP
                     END IF
                  END DO
                  IF (.NOT.THERE) THEN
                     NBOLUSED = NBOLUSED + 1
                     BOLUSED(NBOLUSED) = BOL(I)
                     BOL_INDEX(I) = NBOLUSED
                  END IF
               END DO


            ELSE

               READING = .FALSE.
            END IF

*     Now open the file

            FILEMINONE = .FALSE.
            IF (N_PHOT .GT. 0) THEN
               DO N = 1, N_PHOT
                  IF (STATUS .EQ. SAI__OK) THEN 
                     IF (PLOC(N) .NE. DAT__NOLOC) THEN
                        CALL NDF_FIND(PLOC(N), ' ', IN_NDF(N), STATUS)
                        CALL DAT_ANNUL(PLOC(N), STATUS)
                     ELSE
                        IN_NDF(N) = NDF__NOID
                     END IF
                  
                     IF (IN_NDF(N) .EQ. NDF__NOID) THEN
                        READING = .FALSE.
*                       Need to make sure dont decrement file more than once
                        IF (.NOT.FILEMINONE) THEN
                           FILE = FILE - 1
                           FILEMINONE = .TRUE.
                        END IF
                     END IF
                  END IF
               END DO
            END IF

*     END of If (file = 'null')
         END IF
         
         IF (READING.AND. STATUS .EQ. SAI__OK) THEN

*     Loop through all bolometers
            DO NUM_NDF = 1, N_PHOT

*     Map the input file
               CALL NDF_SQMF(.FALSE., IN_NDF(NUM_NDF), STATUS)
               CALL NDF_MAP(IN_NDF(NUM_NDF), 'QUALITY', '_UBYTE',
     :              'READ', IN_QUAL_PTR, EL, STATUS)
               CALL NDF_MAP(IN_NDF(NUM_NDF), 'Data,','_REAL', 'READ',
     :              IN_DATA_PTR, EL, STATUS)
               CALL NDF_MAP(IN_NDF(NUM_NDF), 'Variance','_REAL', 'READ',
     :              IN_VAR_PTR, EL, STATUS)

*     Give some information about this data

               IF (NUM_NDF .EQ. 1) THEN

*     Name of source
                  CALL NDF_CGET(IN_NDF(NUM_NDF), 'Title', TITLE, STATUS)
                  
*     Number of integrations
                  CALL MSG_SETI('NINT', EL)
                  CALL MSG_SETC('TITLE', TITLE)
                  CALL MSG_SETC('PKG', PACKAGE)
                  
                  CALL MSG_OUT(' ','^PKG: This is a PHOTOM '//
     :                 'observation of ^TITLE. There are ^NINT '//
     :                 'integrations', STATUS)
               END IF

*     Current index of bolometer in full bolometer list

               INDEX = BOL_INDEX(NUM_NDF)

*     Open the output file

               IF (OUT_NDF(INDEX) .EQ. NDF__NOID) THEN
*     Make filename
                  IPOSN = CHR_LEN(OUTROOT)
                  OUTFILE = OUTROOT
                  CALL CHR_APPND('_', OUTFILE, IPOSN)
                  CALL CHR_APPND(BOLUSED(INDEX) ,OUTFILE, IPOSN)

*     Dimensions
                  LBND(INDEX) = 1
                  UBND(INDEX) = 2

*     Make NDF
                  CALL NDF_PLACE(DAT__ROOT, OUTFILE, OUTPLACE, STATUS)
                  CALL NDF_NEW('_REAL', 1, LBND(INDEX), UBND(INDEX),
     :                 OUTPLACE, OUT_NDF(INDEX), STATUS)
                  UBND(INDEX) = 0
               END IF


*     Change the bounds
               UBND(INDEX) = UBND(INDEX) + EL
               CALL NDF_SBND(1, LBND(INDEX), UBND(INDEX), 
     :              OUT_NDF(INDEX), STATUS)

*     Get the new NDF section
               LOWER = UBND(INDEX) - EL + 1
               CALL NDF_SECT(OUT_NDF(INDEX), 1, LOWER,
     :              UBND(INDEX), 
     :              SEC_NDF, STATUS)

*     Map the output data arrays
               MODE = 'WRITE'
               CALL NDF_MAP(SEC_NDF, 'QUALITY', '_UBYTE', MODE,
     :              OUT_QUAL_PTR, ITEMP, STATUS)
               CALL NDF_MAP(SEC_NDF, 'Data', '_REAL', MODE,
     :              OUT_DATA_PTR, ITEMP, STATUS)
               CALL NDF_MAP(SEC_NDF, 'Variance', '_REAL', MODE,
     :              OUT_VAR_PTR, ITEMP, STATUS)

*     Copy the input data into the output data
               CALL VEC_RTOR(.FALSE., EL, %VAL(IN_DATA_PTR),
     :              %VAL(OUT_DATA_PTR), IERR, NERR, STATUS)
               CALL VEC_RTOR(.FALSE., EL, %VAL(IN_VAR_PTR),
     :              %VAL(OUT_VAR_PTR), IERR, NERR, STATUS)
               CALL VEC_UBTOUB(.FALSE., EL, %VAL(IN_QUAL_PTR),
     :              %VAL(OUT_QUAL_PTR), IERR, NERR, STATUS)

*     Tidy up

               CALL NDF_UNMAP(IN_NDF(NUM_NDF), '*', STATUS)
               CALL NDF_ANNUL(IN_NDF(NUM_NDF), STATUS)

*     Unmap the output array
               CALL NDF_UNMAP(SEC_NDF, '*', STATUS)
               CALL NDF_ANNUL(SEC_NDF, STATUS)

            END DO

         END IF
         CALL PAR_CANCL('IN', STATUS)

*     break out of loop if status has gone bad
         
         IF (STATUS .NE. SAI__OK) THEN
            LOOPING = .FALSE.
            READING = .FALSE.
         END IF
         
      END DO

*     Setup the axis

      IF (NBOLUSED .GT. 0) THEN
         DO I = 1, NBOLUSED

            CALL NDF_AMAP(OUT_NDF(I), 'CENTRE', 1, 
     :           '_REAL', 'WRITE', OUT_APTR, ITEMP, STATUS)

            IF (STATUS.EQ.SAI__OK) CALL 
     :           SCULIB_NFILLR(ITEMP,%VAL(OUT_APTR))
            CALL NDF_ACPUT('Integration', OUT_NDF(I), 'LABEL', 1,STATUS)
            CALL NDF_CPUT('Volts', OUT_NDF(I), 'UNITS', STATUS)
            CALL NDF_CPUT(TITLE, OUT_NDF(I), 'Title', STATUS)
            CALL NDF_CPUT('Signal', OUT_NDF(I), 'LAB', STATUS)
            
            BADBIT = 1
            CALL NDF_SBB(BADBIT, OUT_NDF(I), STATUS)
*     Just in case
            CALL NDF_SBAD(.TRUE., OUT_NDF(I),
     :           'Data,Variance' ,STATUS)

*     Tidy up
            CALL NDF_ANNUL(OUT_NDF(I), STATUS)

         END DO
      ELSE 
         CALL MSG_SETC('TASK', TSKNAME)
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', '^TASK: there was no '//
     :           'input data', STATUS)
         ELSE
            CALL ERR_REP(' ','^TASK: No data found', STATUS)
         END IF

      END IF

      CALL NDF_END(STATUS)

      END
