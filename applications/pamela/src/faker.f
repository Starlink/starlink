*FAKER
*
* FAKER
*
* Fakes up a data frame with multiple objects, sky lines and noise for
* testing purposes.
*
* Parameters:
*
* TEMPLATE  -- ASCII file specifying frame format.
*
* SEED      -- Seed integer for random number generator.
*
* OUTPUT    -- The image file to create
*
* Here is an example template file (the comments should be removed
* however)
*
* Fake object frame ! Object name
* 100 500           ! Dimensions NX,NY
* 3. 2.             ! Readout noise, photons/ADU
* 3                 ! Number of objects along slit
* 3. 4              ! Their FWHM and number of poly coeffs for track
* 100. 500. 300.    ! Number of counts/row
* 20. 1. 1. 1.      ! Poly coeffs of track 1
* 50. 1. 1. 1.      ! Poly coeffs of track 2
* 70. 1. 1. 1.      ! Poly coeffs of track 3
* 20. 10            ! Continuum of sky, number of lines
* 2.5 3             ! FWHM of sky lines, number of coeffs
* 20. 100. 400. 400. 300. 50. 60. 350. 600. 100. ! Counts/col of lines
* 10. 1. 1.         ! Poly coeffs of line 1
* 30. 1. 1.         ! Poly coeffs of line 2
* 60. 1. 1.         ! Poly coeffs of line 3
* 90. 1. 1.         ! Poly coeffs of line 4
* 150. 1. 1.        ! Poly coeffs of line 5
* 180. 1. 1.        ! Poly coeffs of line 6
* 220. 1. 1.        ! Poly coeffs of line 7
* 340. 1. 1.        ! Poly coeffs of line 8
* 390. 1. 1.        ! Poly coeffs of line 9
* 410. 1. 1.        ! Poly coeffs of line 10
* 200 5000.         ! Number of cosmic rays, number of counts/ray
*
* History: adapted to NDF 13/01/1998 by TRM
*
*FAKER
      SUBROUTINE FAKER(STATUS)
C
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
      INTEGER IFAIL, STATUS, MAXOBJ, NX, NY, MAX
     :POLY
      INTEGER NOBJECTS, NOBJPOLY, NSKYLINES, MAXSKY
      INTEGER NCOSRAYS, SEED, LBND(2), UBND(2)
      INTEGER NSKYPOLY
      PARAMETER (MAXOBJ=50)
      PARAMETER (MAXPOLY=10)
      PARAMETER (MAXSKY=100)
      REAL OBJCOUNTS(MAXOBJ), SKYINT(MAXSKY), COSINT, SKY
      DOUBLE PRECISION OBJPOLY(MAXPOLY, MAXOBJ)
      DOUBLE PRECISION SKYPOLY(MAXPOLY, MAXSKY)
      REAL READOUT, PHOTON, OFWHM, SFWHM
      CHARACTER*32 OBJECT
      CHARACTER*128 TEMPLATE
      CHARACTER*(DAT__SZLOC) LOC
C
      INTEGER OUTPUT, OPTR, EL, I, J, LUNIT
      DATA LUNIT/31/
C
      IF(STATUS.NE.SAI__OK) RETURN
C
C Load ascii template
C
      CALL PAR_GET0C('TEMPLATE',TEMPLATE,STATUS)
      OPEN(UNIT=LUNIT,FILE=TEMPLATE,STATUS='OLD',IOSTAT=IFAIL)
      IF(IFAIL.NE.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Failed to open template',STATUS)
         RETURN
      END IF
C
C Read in parameters specifying frame
C
      READ(LUNIT,'(A)',IOSTAT=IFAIL) OBJECT
      IF(IFAIL.NE.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Failed to read object name',STATUS)
         CLOSE(LUNIT)
         RETURN
      END IF
      CALL MSG_SETC('OBJECT',OBJECT)
      CALL MSG_OUT(' ','Object = ^OBJECT',STATUS)
C
      READ(LUNIT,*,IOSTAT=IFAIL) NX, NY
      IF(IFAIL.NE.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Failed to read frame size',STATUS)
         CLOSE(LUNIT)
         RETURN
      END IF
      CALL MSG_SETI('NX',NX)
      CALL MSG_SETI('NY',NY)
      CALL MSG_OUT(' ',' NX = ^NX, NY = ^NY',STATUS)
C
      READ(LUNIT,*,IOSTAT=IFAIL) READOUT, PHOTON
      IF(IFAIL.NE.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Failed to read READOUT, PHOTON',STATUS)
         CLOSE(LUNIT)
         RETURN
      END IF
      CALL MSG_SETR('READOUT',READOUT)
      CALL MSG_SETR('PHOTON',PHOTON)
      CALL MSG_OUT(' ',' READOUT = ^READOUT, Phot = ^PHOTON',STATUS)
*
      READ(LUNIT,*,IOSTAT=IFAIL) NOBJECTS
      IF(IFAIL.NE.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Failed to read NOBJECTS',STATUS)
         CLOSE(LUNIT)
         RETURN
      ELSE IF(NOBJECTS.GT.MAXOBJ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','NOBJECTS too large',STATUS)
         CALL MSG_SETI('MAXOBJ',MAXOBJ)
         CALL ERR_REP(' ','Maximum = ^MAXOBJ',STATUS)
         CLOSE(LUNIT)
         RETURN
      END IF
      CALL MSG_SETI('NOBJECTS',NOBJECTS)
      CALL MSG_OUT(' ','NOBJECTS = ^NOBJECTS',STATUS)
      IF(NOBJECTS.GE.1) THEN
        READ(LUNIT,*,IOSTAT=IFAIL) OFWHM, NOBJPOLY
        IF(IFAIL.NE.0) THEN
           STATUS = SAI__ERROR
           CALL ERR_REP(' ','Failed to read OFWHM, NOBJPOLY',
     &          STATUS)
           CLOSE(LUNIT)
           RETURN
        ELSE IF(NOBJPOLY.LT.1 .OR. NOBJPOLY.GT.MAXPOLY) THEN
           STATUS = SAI__ERROR
           CALL ERR_REP(' ','NOBJPOLY out of range',STATUS)
           CLOSE(LUNIT)
           RETURN
        END IF
        CALL MSG_SETR('OFWHM',OFWHM)
        CALL MSG_SETI('NOBJPOLY',NOBJPOLY)
        CALL MSG_OUT(' ',
     &       'OFWHM = ^OFWHM, NOBJPOLY = ^NOBJPOLY', STATUS)
C
        READ(LUNIT,*,IOSTAT=IFAIL) (OBJCOUNTS(J),J=1,NOBJECTS)
        IF(IFAIL.NE.0) THEN
           STATUS = SAI__ERROR
           CALL ERR_REP(' ','Failed to read object counts',
     &          STATUS)
           CLOSE(LUNIT)
           RETURN
        END IF
        DO J=1, NOBJECTS
           IF(OBJCOUNTS(J).LT.0.) THEN
              STATUS = SAI__ERROR
              CALL ERR_REP('','Object counts must not be < 0',
     &             STATUS)
              CLOSE(LUNIT)
              RETURN
           END IF
        END DO
C
        DO J=1, NOBJECTS
          READ(LUNIT,*,IOSTAT=IFAIL) (OBJPOLY(I,J),I=1,NOBJPOLY)
          IF(IFAIL.NE.0) THEN
             STATUS = SAI__ERROR
             CALL ERR_REP(' ','Failed to read object polys',
     &            STATUS)
             CLOSE(LUNIT)
             RETURN
          END IF
        END DO
      END IF
C
      READ(LUNIT,*,IOSTAT=IFAIL) SKY, NSKYLINES
      IF(IFAIL.NE.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Failed to read SKY, NSKYLINES',
     &        STATUS)
         CLOSE(LUNIT)
         RETURN
      ELSE IF(NSKYLINES.GT.MAXSKY) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','NSKYLINES out of range',STATUS)
         CLOSE(LUNIT)
         RETURN
      ELSE IF(SKY.LT.0.) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','Sky counts must not be < 0',
     &        STATUS)
         CLOSE(LUNIT)
         RETURN
      END IF
      CALL MSG_SETR('SKY',SKY)
      CALL MSG_SETI('NSKYLINES',NSKYLINES)
      CALL MSG_OUT(' ',
     &       'SKY = ^SKY, NSKYLINES = ^NSKYLINES', STATUS)
      IF(NSKYLINES.GT.0) THEN
        READ(LUNIT,*,IOSTAT=IFAIL) SFWHM, NSKYPOLY
        IF(IFAIL.NE.0) THEN
           STATUS = SAI__ERROR
           CALL ERR_REP(' ','Failed to read SFWHM, NSKYPOLY',
     &          STATUS)
           CLOSE(LUNIT)
           RETURN
        ELSE IF(NSKYPOLY.LT.1 .OR. NSKYPOLY.GT.MAXPOLY) THEN
           STATUS = SAI__ERROR
           CALL ERR_REP(' ','NSKYPOLY out of range',STATUS)
           CLOSE(LUNIT)
           RETURN
        END IF
        CALL MSG_SETR('SFWHM',SFWHM)
        CALL MSG_SETI('NSKYPOLY',NSKYPOLY)
        CALL MSG_OUT(' ',
     &       'SFWHM = ^SFWHM, NSKYPOLY = ^NSKYPOLY', STATUS)
        READ(LUNIT,*,IOSTAT=IFAIL) (SKYINT(I),I=1,NSKYLINES)
        IF(IFAIL.NE.0) THEN
           STATUS = SAI__ERROR
           CALL ERR_REP(' ',
     &          'Failed to read sky intensities', STATUS)
           CLOSE(LUNIT)
           RETURN
        END IF
        DO J=1, NSKYLINES
           IF(SKYINT(J).LT.0.) THEN
              STATUS = SAI__ERROR
              CALL ERR_REP(' ','Sky line counts must not be < 0',
     &             STATUS)
              CLOSE(LUNIT)
              RETURN
           END IF
        END DO
C
        DO J = 1, NSKYLINES
          READ(LUNIT,*,IOSTAT=IFAIL) (SKYPOLY(I,J),I=1,NSKYPOLY)
          IF(IFAIL.NE.0) THEN
             STATUS = SAI__ERROR
             CALL ERR_REP(' ',
     &            'Failed to read sky polys', STATUS)
             CLOSE(LUNIT)
             RETURN
          END IF
        END DO
      END IF
C
      READ(LUNIT,*,IOSTAT=IFAIL) NCOSRAYS, COSINT
      IF(IFAIL.NE.0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ',
     &        'Failed to read NCOSRAYS, COSINT', STATUS)
         CLOSE(LUNIT)
         RETURN
      END IF
      CALL MSG_SETI('NCOSRAYS',NCOSRAYS)
      CALL MSG_SETR('COSINT',COSINT)
      CALL MSG_OUT(' ',
     &       'NCOSRAYS = ^NCOSRAYS, COSINT = ^COSINT', STATUS)
      CLOSE(LUNIT)
C
C Get seed integer for random number generator
C
      CALL PAR_GET0I('SEED',SEED,STATUS)
      SEED = - ABS(SEED)
C
C Open output file
C
      CALL NDF_BEGIN
      LBND(1) = 1
      LBND(2) = 1
      UBND(1) = NX
      UBND(2) = NY
      CALL NDF_CREAT('OUTPUT','_REAL',2,LBND,UBND,OUTPUT,STATUS)
      CALL NDF_MAP(OUTPUT,'Data','_REAL','WRITE',OPTR,EL,STATUS)
C
C Create 'pamela' extension
C
      CALL NDF_XNEW(OUTPUT,'PAMELA', 'Ext', 0, 0, LOC, STATUS)
      CALL NDF_XPT0C(OBJECT,OUTPUT,'Pamela','Object',STATUS)
      CALL DAT_ANNUL(LOC, STATUS)
C
C Set data
C
      CALL FAKE(%VAL(CNF_PVAL(OPTR)), NX, NY, READOUT, PHOTON,
     &SKY, NOBJECTS, NOBJPOLY, OBJPOLY, OFWHM, OBJCOUNTS,
     &NSKYLINES, SKYPOLY, NSKYPOLY, SKYINT, SFWHM, NCOSRAYS,
     &COSINT, SEED, MAXOBJ, MAXSKY, MAXPOLY, STATUS)
C
C     Tidy up
C
      CALL NDF_END(STATUS)
      RETURN
      END

      SUBROUTINE FAKE(DATA,NX,NY,READOUT,PHOTON,SKY,NOBJECTS,
     &NOBJPOLY,OBJPOLY,OBJFWHM,OBJCOUNTS,NSKYLINES,SKYPOLY,
     &NSKYPOLY,SKYINT,SKYFWHM,NCOSRAYS,COSINT,NSEED,MAXOBJ,
     &MAXSKY,MAXPOLY,STATUS)
*
* Routine to fake data
*
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER NX,NY,NOBJECTS,NOBJPOLY,NSKYLINES,NSKYPOLY
      INTEGER NCOSRAYS,NSEED,MAXOBJ,MAXSKY,MAXPOLY
      INTEGER I,J,N,IX,IY,STATUS
      REAL DATA(NX,NY),READOUT,PHOTON,SKY,COSINT
      REAL OBJFWHM, OBJCOUNTS(MAXOBJ)
      REAL SKYINT(MAXSKY), SKYFWHM
      REAL DEVIATION, SIGMA, PEAKXPOS, PEAKYPOS
      REAL RAN2, GAUSS2
      DOUBLE PRECISION OBJPOLY(MAXPOLY,MAXOBJ)
      DOUBLE PRECISION SKYPOLY(MAXPOLY,MAXSKY)
      DOUBLE PRECISION AREA, OBJVAL, SKYVAL
      DOUBLE PRECISION POLY, Z
C
      IF(STATUS.NE.SAI__OK) RETURN
C
C Set background level to 'sky'
C
      DO J = 1, NY
         DO I = 1, NX
            DATA(I,J) = SKY
         END DO
      END DO
C
C Add object in
C Calls function AREA : AREA(x,sigma) is a maximum at x=0
C
      SIGMA = OBJFWHM / 2.3548
      DO N = 1,NOBJECTS
         DO IY = 1, NY
            Z = 2.D0*(DBLE(IY)-DBLE(1+NY)/2.)/DBLE(NY)
            PEAKXPOS = REAL(POLY(OBJPOLY(1,N),NOBJPOLY,Z))
            DO IX = 1, NX
               OBJVAL = AREA(REAL(IX)-PEAKXPOS, SIGMA)
               DATA(IX,IY) = REAL(DATA(IX,IY) + OBJVAL*OBJCOUNTS(N))
            END DO
         END DO
      END DO
C
C Add skylines in with correct Gaussian shapes etc..
C Take each skyline in turn, calculate y position at each x
C Scan top to bottom of image frame, adding on intensities
C
      SIGMA = SKYFWHM / 2.3548
      DO N = 1, NSKYLINES
         DO IX = 1, NX
            Z = 2.D0*(DBLE(IX)-DBLE(1+NX)/2.)/DBLE(NX)
            PEAKYPOS = REAL(POLY(SKYPOLY(1,N),NSKYPOLY,Z))
            DO IY = 1, NY
               SKYVAL = AREA(REAL(IY)-PEAKYPOS, SIGMA)
               DATA(IX,IY) = DATA(IX,IY) + REAL(SKYVAL*SKYINT(N))
            END DO
         END DO
      END DO
C
C Add cosmic rays in
C
      DO N = 1, NCOSRAYS
         IX = NINT(1.+REAL(NX-1)*RAN2(NSEED))
         IY = NINT(1.+REAL(NY-1)*RAN2(NSEED))
         DATA(IX,IY) = DATA(IX,IY) + COSINT
      END DO
C
C Add noise
C
      DO IY = 1, NY
         DO IX = 1, NX
            DEVIATION = SQRT(READOUT*READOUT+
     &           MAX(0.,DATA(IX,IY))/PHOTON)
            DATA(IX,IY) = GAUSS2(DATA(IX,IY),DEVIATION,NSEED)
         END DO
      END DO
      RETURN
      END

      DOUBLE PRECISION FUNCTION AREA (X, SIGMA)
*
* Calculates area under 'bin' at central position x, width unity
*
      IMPLICIT NONE
      REAL X, SIGMA, TEMP
      DOUBLE PRECISION PDA_DERF, VALUE1, VALUE2
*
      TEMP   = 1./SIGMA/SQRT(2.)
      VALUE1 = TEMP*(X-0.5)
      VALUE2 = TEMP*(X+0.5)
      AREA   = 5.D-1*(PDA_DERF(VALUE2)-PDA_DERF(VALUE1))
      RETURN
      END

