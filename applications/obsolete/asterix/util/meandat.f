*+  MEANDAT - Averages a series of files of any dimensionality
      SUBROUTINE MEANDAT( STATUS )
*
*    Description :
*
*      Averages upto 20 datafiles using a weighted or non-weighted mean.
*      Elements with bad quality are ignored.
*
*    Environment parameters :
*
*      NFILES       INTEGER        Number of files to average
*      FILE1        UNIV           Name of first file
*      FILE2        UNIV           Name of second file
*      FILEn        UNIV           Name of nth file
*      WEIGHT1      REAL           Weight of 1st file
*      WEIGHTn      REAL           Weight of nth file
*      AVERAGE      LOGICAL        Average or sum the files ?
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*      Richard Saxton  (LTVAD::RDS)
*
*    History :
*
*      12 May 90 : V1.2-1 Original (RDS)
*      28 Feb 94 : V1.7-0 Use BIT_ routines for quality manipulation (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      INTEGER MAXFIL
        PARAMETER (MAXFIL=20)                ! Maximum number of files to avrge.
*
*    Local variables :
*
      LOGICAL LVAR                           ! Are variances present in all file
      LOGICAL LQUAL                          ! Is quality present in all files ?
      INTEGER LP,AXLP
      INTEGER NDIM                           ! Number of dimensions in array
      INTEGER DIMS(DAT__MXDIM)               ! Dimensions of data array
      INTEGER TNDIM                          ! Test number of dimensions
      INTEGER TDIMS(DAT__MXDIM)              ! Test dimensions of data array
      INTEGER NFILES                         ! Number of files to be averaged
      CHARACTER*7 PARAM                      ! Filename parameter
      CHARACTER*(DAT__SZLOC) LOC(MAXFIL)     ! Locators to input files
      CHARACTER*(DAT__SZLOC) LOCO            ! Locator to output file
      LOGICAL INPRIM                         ! Is input array primitive ?
      LOGICAL OK                             ! Is data array present
      INTEGER DP(MAXFIL)                     ! Pointer to data arrays
      INTEGER VP(MAXFIL)                     ! Pointer to variance arrays
      INTEGER QP(MAXFIL)                     ! Pointer to quality arrays
      INTEGER ODP                            ! Pointer to output arrays
      INTEGER OVP                            ! Pointer to output variance array
      INTEGER OQP                            ! Pointer to output quality array
      BYTE MASK(MAXFIL)                      ! Badbits mask for each file
      INTEGER NELS                           ! Total number of elements in array
      LOGICAL AVERAGE                        ! Average the files or sum ?
      INTEGER CPTR                           ! Pointer to dynamic workspace
      CHARACTER*3 CNUM                       ! File number character string
      INTEGER IDUM,NDUM                      ! Dummy integer
      INTEGER IDIM(DAT__MXDIM)               ! Dummy dimensions
      CHARACTER*(60) TYPE                    ! Type of input datafile
      CHARACTER*80 PATH(42)                  ! Historty records
      INTEGER NLINES                         ! Number of lines of history text
*    Local data :
*     <any DATA initialisations for local variables>
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'MEANDAT version 1.7-0')
*-

*    Version id
      CALL MSG_PRNT( VERSION )

*    ASTERIX initialisation
      CALL AST_INIT()

*    Initialise variables
      LVAR=.TRUE.
      LQUAL=.TRUE.
*
      DO LP=1,DAT__MXDIM
         DIMS(LP)=1
         TDIMS(LP)=1
      ENDDO
*
* Find how many files are wanted
      CALL PAR_GET0I('NFILES', NFILES, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
      IF (NFILES .LT. 2 .OR. NFILES .GT. 20) THEN
         CALL MSG_PRNT('Number of files must be between 2 and 20')
         GOTO 999
      ENDIF
*
* Open files
      DO LP=1,NFILES
*
         CALL CHR_ITOC(LP, CNUM, IDUM)
         PARAM='FILE'//CNUM
*
         CALL USI_ASSOCI(PARAM, 'READ', LOC(LP), INPRIM, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
      ENDDO
*
* Get dimensions of data array in first file
      CALL BDA_CHKDATA(LOC(1), OK, NDIM, DIMS, STATUS)
*
* Map data array, variance and quality from all files
      DO LP=1,NFILES
*
         CALL MSG_SETI('FNO', LP)
*
*  Get data array dimensions
         CALL BDA_CHKDATA(LOC(LP), OK, TNDIM, TDIMS, STATUS)
*
         IF (.NOT. OK .OR. STATUS .NE. SAI__OK) THEN
            CALL MSG_PRNT('Error looking for data array ^FNO')
            GOTO 999
         ELSE
*
*  Test if dimensions are the same as for file 1
            DO AXLP=1,7
               IF (DIMS(AXLP) .NE. TDIMS(AXLP)) THEN
                  CALL MSG_PRNT('File ^FNO has different dimensions'/
     &                         /' to file 1')
                  GOTO 999
               ENDIF
            ENDDO
*
*  Map data array
            CALL BDA_MAPDATA(LOC(LP), 'READ', DP(LP), STATUS)
*
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('Error mapping data array ^FNO')
               GOTO 999
            ENDIF
         ENDIF
*
* Attempt to map variance from file if not already missing
         IF (LVAR) THEN
*
            CALL BDA_CHKVAR(LOC(LP), LVAR, NDUM, IDIM, STATUS)
*
            IF (STATUS .NE. SAI__OK .OR. .NOT. LVAR) THEN
               CALL MSG_PRNT('Variance missing - will not be used')
               LVAR=.FALSE.
               CALL ERR_ANNUL(STATUS)
            ELSE
               CALL BDA_MAPVAR(LOC(LP), 'READ', VP(LP), STATUS)
            ENDIF
         ENDIF
*
* Attempt to map quality from file if not already missing
         IF (LQUAL) THEN
*
            CALL BDA_CHKQUAL(LOC(LP), LQUAL, NDUM, IDIM, STATUS)
*
            IF (STATUS .NE. SAI__OK .OR. .NOT. LQUAL) THEN
               CALL MSG_PRNT('Quality missing - will not be used')
               LQUAL=.FALSE.
               CALL ERR_ANNUL(STATUS)
            ELSE
               CALL BDA_MAPQUAL(LOC(LP), 'READ', QP(LP), STATUS)
*
* Attempt to get badbits value from file
               CALL BDA_GETMASK(LOC(LP), MASK(LP), STATUS)
*
               IF (STATUS .NE. SAI__OK) THEN
                  CALL ERR_ANNUL(STATUS)
                  MASK(LP) = QUAL__MASK
               ENDIF
*
            ENDIF
         ENDIF
*
      ENDDO

*    Calculate total number of elements in output array
      CALL ARR_SUMDIM( NDIM, DIMS, NELS )

*    Create an output file
      CALL DAT_TYPE(LOC(1), TYPE, STATUS)
*
      CALL USI_ASSOCO('OUTPUT', TYPE, LOCO, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Copy everything from the first file into the output file
      CALL HDX_COPY(LOC(1), LOCO, STATUS)
*
* Map the data array in the output file and zero it.
      CALL BDA_MAPDATA(LOCO, 'UPDATE', ODP, STATUS)
      CALL ARR_INIT1R(0.0, NELS, %val(ODP), STATUS )
*
* Map the variance array if variances being used, otherwise delete any variance
* array
      IF (LVAR) THEN
         CALL BDA_MAPVAR(LOCO, 'UPDATE', OVP, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
         CALL ARR_INIT1R(0.0, NELS, %val(OVP), STATUS)
*
      ELSE
*
         CALL DAT_ERASE(LOCO, 'VARIANCE', STATUS)
*
         IF (STATUS .NE. SAI__OK)  CALL ERR_ANNUL(STATUS)
*
      ENDIF
*
* Map the quality array if quality being used, otherwise delete any quality
* structure
      IF (LQUAL) THEN
         CALL BDA_MAPQUAL(LOCO, 'UPDATE', OQP, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
* Zero the quality array
         CALL ARR_INIT1B( QUAL__GOOD, NELS, %val(OQP), STATUS )
*
      ELSE
*
         CALL DAT_ERASE(LOCO, 'QUALITY', STATUS)
*
         IF (STATUS .NE. SAI__OK)  CALL ERR_ANNUL(STATUS)
*
      ENDIF
*
* By default the files are averaged, if the user sets average to NO on
* the command line then the output will be the sum of these files.
      CALL PAR_GET0L('AVERAGE', AVERAGE, STATUS)
*
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Create a workspace array and zero it
      CALL DYN_MAPR(7, DIMS, CPTR, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error creating dynamic space')
         GOTO 999
      ENDIF
*
      CALL ARR_INIT1R(0.0, NELS, %val(CPTR), STATUS)

*    Average these files
      CALL MEANDAT_AVERAGE(MAXFIL, NFILES, LVAR, LQUAL, MASK, AVERAGE,
     :         NELS, DP, VP, QP, %val(CPTR), %val(ODP), %val(OVP),
     :         %val(OQP), STATUS)

*    Add standard record to new history structure
      CALL HIST_ADD(LOCO, VERSION, STATUS)

*    Put names of input datafiles used into history
      CALL USI_NAMEI(NLINES, PATH, STATUS)
      CALL HIST_PTXT(LOCO, NLINES, PATH, STATUS)
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL MSG_PRNT('Error adding history record in output file')
      ENDIF
*
999   CONTINUE
*
      CALL AST_CLOSE(STATUS)
*
      END


*+  MEANDAT_AVERAGE - Averages several datafiles
      SUBROUTINE MEANDAT_AVERAGE(MAXFIL, NFILES, LVAR, LQUAL, MASK,
     :                     AVERAGE, NELS, DP, VP, QP, WTSUM, ODATA,
     :                     OVAR, OQUAL, STATUS)
*    Description :
*      Averages several datafiles weighting them either by variance,
*      or a user defined value. Each array is used as a 1-d vector regardless
*      of how many intrinsic dimensions it has.
*    Environment parameters :
*      WEIGHT        REAL        How to weight the datafiles
*    Method :
*      Get weighting mechanism from user
*      Loop around each file
*        Add the contents of each element into the output element
*            if the quality is good using the apropriate weight
*      End loop
*      Average the output arrays using a counter for each element
*
*    Deficiencies :
*    Bugs :
*    Authors :
*       Richard Saxton  (LTVAD::RDS)
*    History :
*       15-MAY-1990     Original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Import :
      INTEGER MAXFIL                     ! Maximum number of files to average
      INTEGER NFILES                     ! Number of files being averaged
      LOGICAL LVAR                       ! Use variances ?
      LOGICAL LQUAL                      ! Use quality ?
      BYTE MASK(MAXFIL)                  ! Badbit mask for each file
      LOGICAL AVERAGE                    ! Average files or leave as sum ?
      INTEGER NELS                       ! Array length
      INTEGER DP(MAXFIL)                 ! Pointers to data arrays
      INTEGER VP(MAXFIL)                 ! Pointers to variance arrays
      INTEGER QP(MAXFIL)                 ! Pointers to quality arrays
*    Import-Export :
      REAL WTSUM(NELS)                   ! Number to normalise output bins
*    Export :
      REAL ODATA(NELS)                   ! Output data array
      REAL OVAR(NELS)                    ! Output variance array
      BYTE OQUAL(NELS)                   ! Output quality array
*    Status :
      INTEGER STATUS
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      CHARACTER*20 WTMETH                ! Subtraction method. 'V'=inverse
*                                        ! variance, 'N'=no weight, 'U'=
*                                        ! user defined weights
      CHARACTER*2 CNUM                   ! Character string of observation
      CHARACTER*8 PARAM                  ! Character string for weight parameter
      REAL WEIGHT                        ! Weight for each file
      INTEGER FLP,LP,IDUM
      LOGICAL JUMPOUT
*    Local data :
*     <any DATA initialisations for local variables>
*-
* Ask which weighting mechanism is wanted
      JUMPOUT=.FALSE.
      DO WHILE (.NOT. JUMPOUT)
*
         CALL PAR_GET0C('WTMETH', WTMETH, STATUS)
*
         IF (STATUS .NE. SAI__OK) GOTO 999
*
         CALL CHR_UCASE(WTMETH)
*
         IF ( INDEX('VNU', WTMETH(1:1)) .EQ. 0 ) THEN
            CALL MSG_PRNT('Weighting operation not recognised')
            CALL PAR_CANCL('WTMETH', STATUS)
         ELSE
            JUMPOUT=.TRUE.
         ENDIF
      ENDDO
*
* If no weighting mechanism wanted - set weight to 1.0
      IF (WTMETH(1:1) .EQ. 'N') THEN
         WEIGHT=1.0
*
* Check variances available if variance weighting requested
      ELSEIF (WTMETH(1:1) .EQ. 'V' .AND. .NOT. LVAR) THEN
*
         CALL MSG_PRNT('Variances not available for all files - '/
     &                /'producing an unweighted mean')
*
         WEIGHT=1.0
         WTMETH='N    '
      ENDIF
*
      DO FLP=1,NFILES
*
*   Weights supplied by user ?
         IF (WTMETH(1:1) .EQ. 'U') THEN
*
*     Create prompt for the weight parameter
            CALL CHR_ITOC(FLP, CNUM, IDUM)
            PARAM='WEIGHT'//CNUM
*
            CALL PAR_PROMT(PARAM, 'Enter weighting for file '/
     &                                             /CNUM, STATUS)
*
*     Ask for the weighting for this file
            CALL PAR_GET0R(PARAM, WEIGHT, STATUS)
*
            IF (STATUS .NE. SAI__OK) GOTO 999
*
         ENDIF
*
* Add the elements from this data array into the output arrays
         CALL MEANDAT_SUM(WTMETH, WEIGHT, LVAR, LQUAL, MASK(FLP), NELS,
     :                    %VAL(DP(FLP)),  %VAL(VP(FLP)), %VAL(QP(FLP)),
     :                                              ODATA, OVAR, WTSUM)

      END DO

*    By default the files should be averaged but give the user a chance to
*    have them summed by not dividing by the number of elements used in each
*    output bin.
      DO LP=1,NELS
*
*   Test if this element had any contribution from the input file
         IF (WTSUM(LP) .GT. 0.0) THEN
*
*     Average the arrays if producing a mean rather than a sum
            IF (AVERAGE) THEN

               ODATA(LP) = ODATA(LP) / WTSUM(LP)
*
               IF (LVAR) THEN
                  OVAR(LP) = OVAR(LP) / ( WTSUM(LP) **2 )
               ENDIF
*
            ENDIF
*
*     Set quality good
            IF (LQUAL) OQUAL(LP) = QUAL__GOOD
*
*   Element is bad
         ELSE
*
            IF (LQUAL) OQUAL(LP) = QUAL__BAD
*
         ENDIF
*
      ENDDO

*    Tidy up
 999  IF (STATUS .NE. SAI__OK) THEN
        CALL AST_REXIT( 'MEANDAT_AVERAGE', STATUS )
      END IF

      END



*+  MEANDAT_SUM - Adds an input array to an output array
      SUBROUTINE MEANDAT_SUM(WTMETH, WEIGHT, LVAR, LQUAL, BADBITS,
     &                 NELS, IDATA, IVAR, IQUAL, ODATA, OVAR, WTSUM)
*    Description :
*    Environment parameters :
*    Method :
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*       Richard Saxton  (LTVAD::RDS)
*    History :
*       15-MAY-1990     Original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'QUAL_PAR'
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Import :
      CHARACTER*(*) WTMETH               ! Method of calculating weighting
*                                        ! factor 'V'=inverse variance,
*                                        ! 'N'=No weighting
      REAL WEIGHT                        ! Weight if WTMETH .ne. 'V'
      LOGICAL LVAR                       ! Use variances ?
      LOGICAL LQUAL                      ! Use quality ?
      BYTE BADBITS                       ! Badbits mask from datafile
      INTEGER NELS                       ! Number of elements in arrays
      REAL IDATA(NELS)                   ! Input data array
      REAL IVAR(NELS)                    ! Input variance array
      BYTE IQUAL(NELS)                   ! Input quality array
*    Import-Export :
      REAL ODATA(NELS)
      REAL OVAR(NELS)
      REAL WTSUM(NELS)                   ! Sum of weights used in pixel
*    Export :
*     <declarations and descriptions for exported arguments>
*
*    Function declarations :
*
      BYTE BIT_ANDUB
*
*    Local variables :
*
      INTEGER LP
      LOGICAL LBAD                       ! Is this pixel bad ?
*-
*
      DO LP=1,NELS
*
*  Calculate weight from variance if wanted
         IF (WTMETH(1:1) .EQ. 'V') THEN
*
            IF (IVAR(LP) .GT. 0.0) THEN
               WEIGHT = 1.0 / IVAR(LP)
            ELSE
               WEIGHT = 0.0
            ENDIF
*
         ENDIF
*
*  Is quality of this element ok ?
         IF (LQUAL) THEN
            LBAD = BIT_ANDUB(IQUAL(LP),BADBITS)
         ELSE
*
*  If quality is not present - assume all elements are good
            LBAD = .FALSE.
*
         ENDIF
*
         IF (LBAD .EQ. QUAL_GOOD) THEN
*
*    Add weighted data array element
            ODATA(LP) = ODATA(LP) + IDATA(LP) * WEIGHT
*
*    Add variance if possible
            IF (LVAR)  OVAR(LP) = OVAR(LP) + IVAR(LP) * ( WEIGHT**2 )
*
*    Tot up the normalising factor
            WTSUM(LP) = WTSUM(LP) + WEIGHT
*
         ENDIF
*
      ENDDO
*
      END
