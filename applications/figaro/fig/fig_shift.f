C+
C                       F I G _ S H I F T
C  Routine:
C     FIG_SHIFT
C
C  Function:
C     Determines the shift between two spectra.
C
C  Description:
C     This is an easy-to-use routine that wraps up the more
C     comprehensive routine FIG_CROSS in order to determine the relative
C     shift between two spectra.  None of the options of FIG_CROSS are
C     available, and reasonable default values are used for all its
C     various parameters. The spectra passed are returned unchanged.
C     The shift is that of the first spectrum relative to the second,
C     and is positive if the first is shifted to the right (similar
C     data appears at higher pixel numbers) relative to the second.
C
C  Language: FORTRAN
C
C  Call:
C     CALL FIG_SHIFT (SPECT0,SPECT1,NX,SHIFT,STATUS)
C
C  Parameters:    (">" input, "<" output, "!" modified, "W" workspace)
C     (>) SPECT0  (Float array,ref) The first spectrum.
C     (>) SPECT1  (Float array,ref) The second spectrum.
C     (>) NX      (Integer, ref) The number of elements in each of the
C                 spectra.
C     (<) SHIFT   (Float,ref) The shift of spectrum 1 relative to
C                 spectrum 2 in elements (pixels).
C     (<) STATUS  (Integer,ref) A status code. 0=> OK, non-zero implies
C                 some error occurred. If an error occurs, DSA_WRUSER
C                 will have been used to output an error message.
C
C  External variables used: None.
C
C  External subroutines / functions used:
C     DSA_GET_WORKSPACE, DSA_FREE_WORKSPACE, DSA_TYPESIZE, DSA_WRUSER
C     CNF_PVAL, GEN_MOVE, GEN_POWER2, FIG_CROSS
C
C  Support: K. Shortridge, AAO
C
C  Version date: 20th March 1991.
C-
C  History:
C     20th Mar 1991  KS/AAO.  Original version.
C     2nd  Sep 1992  HME/UoE, Starlink.  INCLUDE changed.
C                    Changed name from FIGX_SHIFT to FIG_SHIFT.
C     2005 June 15   MJC / Starlink  Use CNF_PVAL for pointers to
C                    mapped data.
C+
      SUBROUTINE FIG_SHIFT (SPECT0,SPECT1,NX,SHIFT,STATUS)
C
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      INTEGER NX,STATUS
      REAL SPECT0(NX),SPECT1(NX),SHIFT
C
C     Functions
C
      INTEGER DSA_TYPESIZE
C
C     Local variables
C
      INTEGER   ARRAY0           ! Pointer to workspace holding SPECT
                                 ! data
      INTEGER   ARRAY1           ! Pointer to workspace holding TEMPLATE
                                 ! data
      INTEGER   BDOUB            ! Number of bytes per double-prec.
                                 ! number
      INTEGER   BFLOAT           ! Number of bytes per floating-point
                                 ! number
      INTEGER   BYTES            ! Number of bytes
      LOGICAL   CFIT             ! If false, disables the usual
                                 ! continuum fit
      INTEGER   CFN              ! Pointer to the correlation function
                                 ! array
      INTEGER   FT0              ! Pointer to the fourier transform of
                                 ! SPECT data
      INTEGER   FT1              ! Pointer to the fourier transform of
                                 ! TEMPLATE data
      INTEGER   FTFCN            ! Pointer to the fourier transform of
                                 ! CORRL data
      LOGICAL   GOTWORK          ! Indicates workspace was obtained
      INTEGER   IGNORE           ! Used for status values we don't care
                                 ! about
      INTEGER   KZ(4)            ! Defines the cosine bell used to
                                 ! filter the FTs
      LOGICAL   NORM             ! True if cross-correlation fn. to be
                                 ! normalised
      INTEGER   NX1              ! Either equal to NX or next highest
                                 ! power of 2
      INTEGER   SLOT             ! Slot number referencing ! workspace
      INTEGER   SLOT1            ! Slot number referencing ! workspace
      INTEGER   SLOT2            ! Slot number referencing ! workspace
      INTEGER   SLOT3            ! Slot number referencing ! workspace
      INTEGER   SLOT4            ! Slot number referencing ! workspace
      INTEGER   SLOT5            ! Slot number referencing ! workspace
      INTEGER   SLOT6            ! Slot number referencing ! workspace
      REAL      WIDTH            ! The width of the correlation function
      INTEGER   XV               ! Pointer to array holding spectra
                                 ! pixel values
      REAL      ZPC              ! Percentage of spectrum covered at
                                 ! each end by a cosine bell prior to
                                 ! fourier transformation
C
C     Initial values
C
      STATUS=0
      GOTWORK=.FALSE.
C
C     Get byte size for float and double precision numbers
C
      BFLOAT=DSA_TYPESIZE('FLOAT',STATUS)
      BDOUB=DSA_TYPESIZE('DOUBLE',STATUS)
C
C     FIG_CROSS can't handle spectra with fewer than four pixels, so
C     trap that case.
C
      IF (NX.LE.3) THEN
         CALL DSA_WRUSER('Cannot determine a relative shift for spectra'
     :                   //' with fewer than four pixels.')
         STATUS=1
         GOTO 500
      END IF
C
C     The cross-correlation needs a lot of workspace, so grab that now.
C
      CALL GEN_POWER2(NX,NX1)
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',ARRAY0,SLOT,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX,'FLOAT',ARRAY1,SLOT1,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX1,'DOUBLE',FT0,SLOT2,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX1,'DOUBLE',FT1,SLOT3,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX1,'DOUBLE',FTFCN,SLOT4,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX1,'FLOAT',XV,SLOT5,STATUS)
      CALL DSA_GET_WORK_ARRAY(NX1,'FLOAT',CFN,SLOT6,STATUS)
      IF(STATUS.NE.0) GOTO 500
      GOTWORK=.TRUE.
C
C     Set default value for the cosine bell percentage, and indicate
C     that it is to be applied.
C
      CFIT=.TRUE.
      ZPC=10.0
C
C     Copy the spectrum and template data into the work arrays
C
      BYTES=BFLOAT*NX
      CALL GEN_MOVE(BYTES,SPECT0,%VAL(CNF_PVAL(ARRAY0)))
      CALL GEN_MOVE(BYTES,SPECT1,%VAL(CNF_PVAL(ARRAY1)))
C
C     Pick reasonable values for the fourier domain filter - this
C     section could be refined, but these will do..
C
      KZ(1)=5
      KZ(2)=MIN(20,NX1-2)
      KZ(3)=MAX(NX1/6,KZ(2)+1)
      KZ(4)=MIN(2*KZ(3),NX1)
C
C     Perform the cross-correlation
C
      NORM=.TRUE.
      CALL FIG_CROSS(%VAL(CNF_PVAL(ARRAY0)),%VAL(CNF_PVAL(ARRAY1)),
     :               NX,NX1,CFIT,ZPC,KZ,NORM,%VAL(CNF_PVAL(FT0)),
     :               %VAL(CNF_PVAL(FT1)),%VAL(CNF_PVAL(FTFCN)),
     :               %VAL(CNF_PVAL(XV)),%VAL(CNF_PVAL(CFN)),SHIFT,WIDTH)
C
  500 CONTINUE
C
C     On exit, release the workspace, if any was allocated.
C
      IF (GOTWORK) THEN
         IGNORE=0
         CALL DSA_FREE_WORKSPACE(SLOT6,IGNORE)
         CALL DSA_FREE_WORKSPACE(SLOT5,IGNORE)
         CALL DSA_FREE_WORKSPACE(SLOT4,IGNORE)
         CALL DSA_FREE_WORKSPACE(SLOT3,IGNORE)
         CALL DSA_FREE_WORKSPACE(SLOT2,IGNORE)
         CALL DSA_FREE_WORKSPACE(SLOT1,IGNORE)
         CALL DSA_FREE_WORKSPACE(SLOT,IGNORE)
      END IF

      END
