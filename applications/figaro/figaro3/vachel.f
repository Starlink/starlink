C+
      SUBROUTINE VACHEL
C
C     V A C H E L
C
C     VACHEL alters the wavelength (X) structure, to convert it from
C     air to vacuum wavelengths, and to correct for a recession velocity
C     (user supplied value).  Usually (but not necessarily) the velocity
C     will be the recession velocity required to correct from observed
C     wavelengths to heliocentric. The velocity correction uses the
C     relativistic formula.
C         Either or both of the vacuum and recession corrections may be
C     applied in the one run.
C         The resulting X structure will be (slightly) non-linear, due
C     to the wavelength dependence of the refractive index of air. It
C     will therefore generally be advisable to rescrunch after use of
C     this routine.  An exception is if only a recession correction is
C     made: this leaves a linear scale (if the input is linear).
C         The input wavelength (X) data must be in Angstroms for the
C     air to vacuum conversion to work correctly.
C
C
C     Command parameters -
C
C     IMAGE  (Character) The name of the structure containing the image.
C     VEL    (Float) Recession velocity for which wavelength is to be
C            corrected
C     OUTPUT (Character) The name of the result of the operation.  This
C            can be the same as for IMAGE.  If not, a new structure
C            is created, with everything but the x-data a direct
C            copy of the input.
C
C     Command keywords -
C
C     VAC    Correction from air to vacuum is to be made
C     NOVAC  No correction from air to vacuum is to be made
C     DPLR   Correction for (+ or -) recession velocity is to
C            be made
C     NODPLR No velocity correction
C
C                                      JGR  July 1985
C     Modified:
C
C     22nd April 1986  KS / AAO.  EXTRACT routine name changed to
C                      avoid conflict with Figaro routine of same name.
C     7 th Aug   1987  DJA/ AAO.  Revised DSA_ routines - some specs
C                      changed. Now uses DYN_ routines for dynamic-
C                      memory handling.
C     3rd  Feb   1988  KS / AAO.  Modified to work properly with data
C                      of any dimensions.
C     20th April 1989  KS / AAO.  Now maps axis data array for update
C                      and not for write - was mapping arrays of zeros
C                      if the array was not of the type mapped. Also
C                      changed to work internally in double precision.
C     23rd Sep 1992    HME / UoE, Starlink.  TABs removed. INCLUDE
C                      changed.
C     2005 June 15     MJC / Starlink  Use CNF_PVAL for pointers to
C                      mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Functions used
C
      INTEGER ICH_ENCODE
      REAL EXTRCT2
C
C     Local variables
C
      INTEGER      DIMS(10)      ! Sizes of dimensions of data
      LOGICAL      FAULT         ! Indicates fault in processing
      INTEGER      IGNORE        ! Used to pass ignorable status
      INTEGER      INVOKE        ! Dummy function value
      INTEGER      NDIM          ! Number of dimensions in data
      INTEGER      NELM          ! Total number of elements in data
      INTEGER      NEXT          ! Next character in STRING
      INTEGER      NX            ! Size of 1st dimension
      INTEGER      STATUS        ! Running status for DSA_ routines
      CHARACTER    STRING*64     ! Used to format result details
      REAL         XEND          ! Last resulting wavelength
      LOGICAL      XEXIST        ! There is a valid x-axis structure?
      INTEGER      XPTR          ! Dynamic-memory pointer to x-axis
                                 ! data array
      INTEGER      XSLOT         ! Map slot number x-axis data array
      REAL         XST           ! First resulting wavelength
      LOGICAL      VACLG         ! Vacuum correction is to be made
      REAL         VEL           ! Value of VEL parameter
      LOGICAL      VELLG         ! Velocity correction is to be made
C
C     Initial values
C
      FAULT=.FALSE.
C
C     Initialisation of DSA_ routines
C
      STATUS=0
      CALL DSA_OPEN(STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Get input name
C
      CALL DSA_INPUT('IMAGE','IMAGE',STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Check that there is a wavelength array, and see what dimensions
C     it has.  Note that we can treat it as a 1D array of length NX,
C     no matter what the actual dimensionality.
C
      CALL DSA_SEEK_AXIS('IMAGE',1,XEXIST,STATUS)
      IF (STATUS.NE.0) GO TO 500
      IF (.NOT.XEXIST) THEN
         CALL PAR_WRUSER('Input data has no wavelength information',
     :                                                        STATUS)
         FAULT=.TRUE.
         GO TO 500
      END IF
      CALL DSA_AXIS_SIZE('IMAGE',1,10,NDIM,DIMS,NELM,STATUS)
      IF (STATUS.NE.0) GO TO 500
      NX=NELM
C
C     Get recession velocity and options for which conversions are to
C     be made
C
      CALL PAR_RDKEY('VAC',.TRUE.,VACLG)
      IF(VACLG)THEN
        CALL PAR_WRUSER('Wavelengths must be in Angstroms '
     :                  //'or results will be wrong',IGNORE)
        CALL PAR_WRUSER('Resulting wavelength scale will be slightly'
     :                  //' non-linear',IGNORE)
      END IF
      CALL PAR_RDKEY('DPLR',.TRUE.,VELLG)
      IF(.NOT.(VACLG.OR.VELLG))THEN
        CALL PAR_WRUSER('With these options routine does nothing! '
     :                  //' Exiting without action',IGNORE)
        FAULT=.TRUE.
        GO TO 500
      END IF
      IF(VELLG) CALL PAR_RDVAL('VEL',-2.9E+5,2.9E+5,0.,'Km/s',VEL)
C
C     Get output structure name
C
      CALL DSA_OUTPUT('OUTPUT','OUTPUT','IMAGE',0,0,STATUS)
      IF (STATUS.NE.0) GO TO 500
C
C     Map data
C
      CALL DSA_MAP_AXIS_DATA('OUTPUT',1,'UPDATE','DOUBLE',XPTR,
     :                       XSLOT,STATUS)
C
C     Call VCHLCON to do calculation of wavelength values.
C     Report new start and end wavelengths to user.
C
      CALL VCHLCON(%VAL(CNF_PVAL(XPTR)),NX,VEL,VELLG,VACLG)
      XST = EXTRCT2(%VAL(CNF_PVAL(XPTR)),NX,1)
      XEND = EXTRCT2(%VAL(CNF_PVAL(XPTR)),NX,NX)
      STRING='Output X-array has '
      INVOKE=ICH_ENCODE(STRING,FLOAT(NX),20,0,NEXT)
      STRING(NEXT:)=' elements, start wavelength = '
      INVOKE=ICH_ENCODE(STRING,XST,NEXT+31,3,NEXT)
      CALL PAR_WRUSER(STRING(:NEXT),IGNORE)
      STRING='End wavelength = '
      INVOKE=ICH_ENCODE(STRING,XEND,18,3,NEXT)
      CALL PAR_WRUSER(STRING(:NEXT),IGNORE)
C
C     Tidy up
C
  500 CONTINUE
C
C     Closedown everything
C
      CALL DSA_CLOSE(STATUS)
      IF (FAULT) CALL FIG_SETERR
C
      END
      SUBROUTINE VCHLCON(ARRAY,NX,VEL,VELLG,VACLG)
C
C     This routine does the calculation of the new wavelength scale,
C     with conversion for one or both of air to vacuum and correction
C     for recession velocity VEL.
C     ARRAY is the (data mapped) output array, which also holds the
C     input data when entering this routine.
C     NX is the size of the X structure (wavelength array).
C     VEL is the recession velocity in Km/s which is to be corrected
C     for (negative if approach velocity).
C     VELLG = .TRUE. if velocity correction is to be done
C     VACLG = .TRUE. if air to vacuum correction is to be done
C
      DOUBLE PRECISION ARRAY(NX)
      LOGICAL VELLG,VACLG
      DOUBLE PRECISION C,WAVM,SUM,REFIND
C
      C = 2.99792E+5
C
      IF(VELLG)VFACT=SQRT((1.-VEL/C)/(1.+VEL/C))
C
      DO I =1,NX
        IF(.NOT.VACLG)GO TO 1             ! Do vacuum conversion here
        WAVM=1./(ARRAY(I)*1.E-4)**2       ! 1/(wavelength in microns)**2
        SUM = 64.328 + 29498.1/(146.-WAVM) + 255.4/(41.-WAVM)
        REFIND = SUM*1.E-6 + 1.            ! Refractive index
        ARRAY(I) = ARRAY(I)*REFIND
C
   1    IF(.NOT.VELLG)GO TO 2             ! Do velocity conversion here
        ARRAY(I) = ARRAY(I)*VFACT
C
   2    CONTINUE
      END DO
      RETURN
      END
      FUNCTION EXTRCT2(ARRAY,NELM,I)
C
C     Extract single element froma real (data mapped) array.
C     I is element number; I = 1 gives first element.
C
      DOUBLE PRECISION ARRAY(NELM)
      EXTRCT2=ARRAY(I)
      RETURN
      END
