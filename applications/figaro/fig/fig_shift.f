C+
C                       F I G _ S H I F T
C  Routine:
C     FIG_SHIFT
C
C  Function:
C     Determines the shift between two spectra.
C
C  Description:
C     This is an easy-to-use routine that wraps up the more comprehensive
C     routine FIG_CROSS in order to determine the relative shift between
C     two spectra.  None of the options of FIG_CROSS are available, and
C     reasonable default values are used for all its various parameters.
C     The spectra passed are returned unchanged.  The shift is that of
C     the first spectrum relative to the second, and is positive if
C     the first is shifted to the right (similar data appears at higher
C     pixel numbers) relative to the second.
C
C  Language: FORTRAN
C     
C  Call:
C     CALL FIG_SHIFT (SPECT0,SPECT1,NX,SHIFT,STATUS)
C
C  Parameters:    (">" input, "<" output, "!" modified, "W" workspace)
C     (>) SPECT0  (Float array,ref) The first spectrum.
C     (>) SPECT1  (Float array,ref) The second spectrum.
C     (>) NX      (Integer, ref) The number of elements in each of the spectra.
C     (<) SHIFT   (Float,ref) The shift of spectrum 1 relative to spectrum 2
C                 in elements (pixels).
C     (<) STATUS  (Integer,ref) A status code. 0=> OK, non-zero implies
C                 some error occurred. If an error occurs, DSA_WRUSER will have 
C                 been used to output an error message.
C
C  External variables used: None.
C
C  External subroutines / functions used: 
C     DSA_GET_WORKSPACE, DSA_FREE_WORKSPACE, DSA_TYPESIZE, DSA_WRUSER
C     DYN_ELEMENT, DYN_INCREMENT, GEN_MOVE, GEN_POWER2, FIG_CROSS
C
C  Support: K. Shortridge, AAO
C
C  Version date: 20th March 1991.
C-
C  History:
C     20th Mar 1991.  KS/AAO.  Original version.
C     2nd  Sep 1992.  HME/UoE, Starlink.  INCLUDE changed.
C                     Changed name from FIGX_SHIFT to FIG_SHIFT.
C+
      SUBROUTINE FIG_SHIFT (SPECT0,SPECT1,NX,SHIFT,STATUS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,STATUS
      REAL SPECT0(NX),SPECT1(NX),SHIFT
C
C     Functions
C 
      INTEGER DSA_TYPESIZE, DYN_ELEMENT, DYN_INCREMENT
C
C     Local variables
C
      INTEGER   ADDRESS   ! Address of dynamic memory element
      INTEGER   ARRAY0    ! Pointer to workspace holding SPECT data
      INTEGER   ARRAY1    ! Pointer to workspace holding TEMPLATE data
      INTEGER   BDOUB     ! Number of bytes per double prec. number
      INTEGER   BFLOAT    ! Number of bytes per floating point number
      INTEGER   BYTES     ! Number of bytes
      LOGICAL   CFIT      ! If false, disables the usual continuum fit
      INTEGER   CFN       ! Pointer to the correlation function array
      INTEGER   CPTR      ! Dynamic memory pointer to CORRL data
      INTEGER   FT0       ! Pointer to the fourier transform of SPECT data
      INTEGER   FT1       ! Pointer to the fourier transform of TEMPLATE data
      INTEGER   FTFCN     ! Pointer to the fourier transform of CORRL data
      LOGICAL   GOTWORK   ! Indicates workspace was obtained
      INTEGER   IGNORE    ! Used for status values we don't care about
      INTEGER   KZ(4)     ! Defines the cosine bell used to filter the FTs
      LOGICAL   NORM      ! True if cross-correlation fn. to be normalised
      INTEGER   NX1       ! Either equal to NX or next highest power of 2
      INTEGER   SLOT      ! Slot number used to refer to workspace
      REAL      WIDTH     ! The width of the correlation function
      INTEGER   XV        ! Pointer to array holding spectra pixel values
      REAL      ZPC       ! Percentage of spectrum covered at each end 
C                           by a cosine bell prior to fourier transformation
C
C     Dynamic memory support - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
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
     :                              //' with fewer than four pixels.\N')
         STATUS=1
         GOTO 500
      END IF
C
C     The cross-correlation needs a lot of workspace, so grab that now.
C
      CALL GEN_POWER2(NX,NX1)
      BYTES = 2*NX*BFLOAT + 3*NX1*BDOUB + 2*NX1*BFLOAT
      CALL DSA_GET_WORKSPACE(BYTES,ADDRESS,SLOT,STATUS)
      ARRAY0=DYN_ELEMENT(ADDRESS)
      IF(STATUS.NE.0)GOTO 500
      GOTWORK=.TRUE.
C
C     Calculate values for pointers into work area.
C
      ARRAY1=DYN_INCREMENT(ARRAY0,'FLOAT',NX)
      FT0=DYN_INCREMENT(ARRAY1,'FLOAT',NX)
      FT1=DYN_INCREMENT(FT0,'DOUBLE',NX1)
      FTFCN=DYN_INCREMENT(FT1,'DOUBLE',NX1)
      XV=DYN_INCREMENT(FTFCN,'DOUBLE',NX1)
      CFN=DYN_INCREMENT(XV,'FLOAT',NX1)
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
      CALL GEN_MOVE(BYTES,SPECT0,DYNAMIC_MEM(ARRAY0))
      CALL GEN_MOVE(BYTES,SPECT1,DYNAMIC_MEM(ARRAY1))
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
      CALL FIG_CROSS(DYNAMIC_MEM(ARRAY0),DYNAMIC_MEM(ARRAY1),
     :        NX,NX1,CFIT,ZPC,KZ,NORM,
     :        DYNAMIC_MEM(FT0),DYNAMIC_MEM(FT1),DYNAMIC_MEM(FTFCN),
     :        DYNAMIC_MEM(XV),DYNAMIC_MEM(CFN),
     :        SHIFT,WIDTH)
C
  500 CONTINUE
C
C     On exit, release the workspace, if any was allocated.
C
      IF (GOTWORK) THEN
         IGNORE=0
         CALL DSA_FREE_WORKSPACE(SLOT,IGNORE)
      END IF

      END
