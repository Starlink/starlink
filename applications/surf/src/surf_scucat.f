*+  REDS_SCUCAT - routine to pack photometry datasets for further processing
      SUBROUTINE REDS_SCUCAT (STATUS)
*    Description:
*     This routine reads in a list of user specified files and concatenates
*     their data, variance and quality arrays so that routines like errclip
*     and kstest can analyse the complete set of photometry observations.
*
*     PARAMETERS:
*
*        OUT:        Output datafile
*        IN :        Loops through NDF files appending to OUT ('end' to end)
*
*    Parameters:
*     STATUS        = INTEGER (Given and returned)
*    Authors:
*     T. Jenness (JACH)
*    History:
*     $Id$
*     $Log$
*     Revision 1.5  1996/10/19 00:07:04  timj
*     Use GLOBAL.sdf and DAT_ASSOC, remove FILENAME(FILE)
*
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
*     endhistory
*     Type definitions:
      IMPLICIT NONE
*     Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'NDF_PAR'
      INCLUDE 'PRM_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'DAT_PAR'
*     Import:
*     Import-Export:
*     Export:
*     Status:
      INTEGER STATUS
*     External references:
      INTEGER CHR_LEN           ! Length of string
      LOGICAL CHR_SCOMP         ! Sort algorithm
*     Global variables:
*     Local constants:
      INTEGER MAXCMP            ! Max number of bolometers in an HDS
      PARAMETER (MAXCMP = 20)
      INTEGER MAX_FILE          ! Max number of files
      PARAMETER (MAX_FILE = 20)
*     Local variables:
      BYTE          BADBIT      ! Bad bit mask
      CHARACTER*15  BOL(MAXCMP) ! Name of bolometers present in NDF
      CHARACTER*15  BOLOMETER   ! Selected bolometer
      CHARACTER*(MAXCMP*15) BOL_LIST ! List of all bolometers in NDF
      INTEGER       EL          ! Number of input data points
      INTEGER       FILE        ! File count
      CHARACTER *20 FILENAME    ! Filename
      INTEGER       I           ! Loop counter
      INTEGER       IN_NDF      ! Input NDF identifier
      INTEGER       IN_DATA_PTR ! Pointer to D array
      INTEGER       IN_VAR_PTR  ! Pointer to V array
      INTEGER       IN_QUAL_PTR ! Pointer to Q array
      INTEGER       IPAR        ! Parameter ID
      INTEGER       IPOSN       ! Position in string
      INTEGER       ITEMP       ! Temporary integer
      INTEGER       LBND(1)     ! Lower bound of output array
      CHARACTER*(DAT__SZLOC) LOC ! Locator to root HDS file
      LOGICAL       LOOPING     ! Controls read loop
      CHARACTER*10  MODE        ! Access mode for output
      CHARACTER*15  NAME(MAXCMP) ! Names of NDFs
      INTEGER       NCOMP       ! Number of components in HDS
      INTEGER       NDATA       ! Number of data points in input
      CHARACTER*(DAT__SZLOC) NLOC ! Loc to  Bol NDF inside HDS file
      INTEGER       N_PHOT      ! Number of bolometers in an HDS
      INTEGER       OUT_NDF     ! Output NDF identifier
      INTEGER       OUT_APTR    ! Pointer to Axis array
      INTEGER       OUT_DATA_PTR ! Pointer to D array
      INTEGER       OUT_VAR_PTR ! Pointer to V array
      INTEGER       OUT_QUAL_PTR ! Pointer to Q array
      CHARACTER*(DAT__SZLOC) PLOC(MAXCMP) ! Locs to PEAK NDFs
      LOGICAL       READING     ! Logical to control file reading
      CHARACTER*40  TITLE       ! Title of observation
      INTEGER       UBND(1)     ! Upper bound of output array
      
      CHARACTER*(DAT__SZLOC) USE_LOC ! Locator to selected file
*     Internal references:
*     Local data:
*     -

      IF (STATUS .NE. SAI__OK) RETURN

*     start up the NDF system and read in the input photometry data
      
      CALL NDF_BEGIN
      
      READING = .TRUE.
      LOOPING = .TRUE.
      FILE = 0

      DO WHILE (LOOPING)

         FILE = FILE + 1
         
*     read the name of the file to be read
         
         IF (FILE .GT. 1) CALL PAR_DEF0C ('IN', '!', STATUS) 

C     Read in the latest file
         CALL DAT_ASSOC('IN', 'READ', LOC, STATUS)

         CALL SUBPAR_FINDPAR( 'IN', IPAR, STATUS)
         CALL SUBPAR_GETNAME(IPAR, FILENAME, STATUS)
C         CALL DAT_CANCL('IN', STATUS)

         IF (STATUS .EQ. PAR__NULL .OR. FILENAME.EQ.'!') THEN
            CALL ERR_ANNUL(STATUS)
            LOOPING = .FALSE.
            READING = .FALSE.
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
                     CALL DAT_CLONE(NLOC, PLOC(N_PHOT), STATUS)
                     CALL DAT_PRMRY(.TRUE., PLOC(N_PHOT), .TRUE.,STATUS)
                  END IF

                  CALL DAT_ANNUL(NLOC, STATUS)
               END DO
            ELSE
               CALL ERR_REP(' ', 'Failed to open file', STATUS)
               CALL ERR_ANNUL(STATUS)
            END IF

*     Have I found a photometry observation
            IF (N_PHOT .GT. 1) THEN
               
               CALL MSG_OUT(' ','Found data for the following '//
     :              'bolometers:', STATUS)

*     Sort the bolometer list
               CALL CHR_SORT(CHR_SCOMP, N_PHOT, BOL, ITEMP)

               BOL_LIST = ''
               IPOSN = 0
               CALL CHR_APPND(BOL(1),BOL_LIST, IPOSN)
               DO I =  2, N_PHOT
                  CALL CHR_APPND(',',BOL_LIST,IPOSN)
                  CALL CHR_APPND(BOL(I),BOL_LIST, IPOSN)

                  CALL MSG_SETC('BOL',BOL(I))
                  CALL MSG_OUT(' ','    ^BOL', STATUS)
               END DO

               CALL PAR_CHOIC('BOLOMETER', BOL(1), BOL_LIST, .TRUE.,
     :              BOLOMETER, STATUS)

*     Which one has been selected
               DO I = 1, N_PHOT
                  IF (BOLOMETER .EQ. BOL(I)) THEN
                     ITEMP = I
                     I = N_PHOT
                  END IF
               END DO

               CALL DAT_CLONE(PLOC(ITEMP), USE_LOC, STATUS)
               CALL DAT_PRMRY(.TRUE., USE_LOC, .TRUE., STATUS)

*     Only one, so I know what to do
            ELSE IF (N_PHOT.EQ.1) THEN
               CALL MSG_SETC('BOL', BOL(1))
               CALL MSG_OUT(' ','Contains data for bolometer ^BOL', 
     :              STATUS)

               CALL DAT_CLONE(PLOC(1), USE_LOC, STATUS)
               CALL DAT_PRMRY(.TRUE., USE_LOC, .TRUE., STATUS)
            ELSE

               READING = .FALSE.
               USE_LOC = DAT__NOLOC
            END IF


*     Annul all the temporary locators
            IF (N_PHOT .GT. 0) THEN
               DO I = 1, N_PHOT
                  CALL DAT_ANNUL(PLOC(I), STATUS)
               END DO
            END IF

*     Now open the file

            IF (STATUS .EQ. SAI__OK) THEN 
               IF (USE_LOC .NE. DAT__NOLOC) THEN
                  CALL NDF_FIND(USE_LOC, ' ', IN_NDF, STATUS)
                  CALL DAT_ANNUL(USE_LOC, STATUS)
               ELSE
                  IN_NDF = NDF__NOID
               END IF

               IF (IN_NDF .EQ. NDF__NOID) THEN
                  FILE = FILE - 1
                  READING = .FALSE.
               END IF
            END IF

*     END of If (file = 'null')
         END IF
         
         IF (READING.AND. STATUS .EQ. SAI__OK) THEN

*     Map the input file

            CALL NDF_MAP(IN_NDF, 'QUALITY', '_UBYTE', 'READ',
     :           IN_QUAL_PTR, EL, STATUS)
            CALL NDF_MAP(IN_NDF, 'Data,','_REAL', 'READ',
     :           IN_DATA_PTR, EL, STATUS)
            CALL NDF_MAP(IN_NDF, 'Variance','_REAL', 'READ',
     :           IN_VAR_PTR, EL, STATUS)

*     Give some information about this data

*     Name of source
            CALL NDF_CGET(IN_NDF, 'Title', TITLE, STATUS)

*     Number of integrations
            CALL MSG_SETI('NINT', EL)
            CALL MSG_SETC('TITLE', TITLE)
            
            CALL MSG_OUT(' ','This is a PHOTOM observation of '//
     :           '^TITLE. There are ^NINT integrations', STATUS)


*     Open the output file

            IF (FILE .EQ. 1) THEN
               LBND(1) = 1
               UBND(1) = 2
               CALL NDF_CREAT('OUT', '_REAL', 1, LBND, UBND, OUT_NDF, 
     :              STATUS)
               UBND(1) = 0
            END IF

*     Change the bounds

            NDATA = UBND(1) - LBND(1) + 1
            UBND(1) = UBND(1) + EL

            CALL NDF_SBND(1, LBND, UBND, OUT_NDF, STATUS)


*     Map the output data arrays

            IF (FILE .EQ. 1) THEN
               MODE = 'WRITE'
            ELSE
               MODE = 'UPDATE'
            END IF

            CALL NDF_MAP(OUT_NDF, 'QUALITY', '_UBYTE', MODE,
     :           OUT_QUAL_PTR, ITEMP, STATUS)
            CALL NDF_MAP(OUT_NDF, 'Data', '_REAL', MODE,
     :           OUT_DATA_PTR, ITEMP, STATUS)
            CALL NDF_MAP(OUT_NDF, 'Variance', '_REAL', MODE,
     :           OUT_VAR_PTR, ITEMP, STATUS)

*     Copy all the data
            CALL SCULIB_COPYR(EL, %VAL(IN_DATA_PTR),
     :           %VAL(OUT_DATA_PTR + NDATA * VAL__NBR))
            CALL SCULIB_COPYR(EL, %VAL(IN_VAR_PTR),
     :           %VAL(OUT_VAR_PTR + NDATA * VAL__NBR))
            CALL SCULIB_COPYB(EL, %VAL(IN_QUAL_PTR),
     :           %VAL(OUT_QUAL_PTR + NDATA * VAL__NBUB))

*     Unmap the output array
            CALL NDF_UNMAP(OUT_NDF, '*', STATUS)

*     Tidy up

            CALL NDF_UNMAP(IN_NDF, '*', STATUS)
            CALL NDF_ANNUL(IN_NDF, STATUS)

         END IF
         CALL PAR_CANCL('IN', STATUS)

*     break out of loop if status has gone bad
         
         IF (STATUS .NE. SAI__OK) THEN
            LOOPING = .FALSE.
            READING = .FALSE.
         END IF
         
      END DO

*     Setup the axis

      IF (FILE .GT. 0) THEN
         CALL NDF_AMAP(OUT_NDF, 'CENTRE', 1, '_REAL', 'WRITE', OUT_APTR,
     :        ITEMP, STATUS)

         CALL SCULIB_NFILLR(ITEMP, %VAL(OUT_APTR))
         CALL NDF_ACPUT('Integration',OUT_NDF,'LABEL',1,STATUS)
         CALL NDF_CPUT('Volts', OUT_NDF, 'UNITS', STATUS)
         CALL NDF_CPUT(TITLE, OUT_NDF, 'Title', STATUS)
         CALL NDF_CPUT('Signal', OUT_NDF, 'LAB', STATUS)
         
         BADBIT = 1
         CALL NDF_SBB(BADBIT, OUT_NDF, STATUS)
      ELSE 
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_CONCAT: there was no '//
     :           'input data', STATUS)
         ELSE
            CALL ERR_REP(' ','REDS_CONCAT: No data found', STATUS)
         END IF

      END IF

      CALL NDF_END(STATUS)

      END
