*+  GFX_GRID - puts grid over image in specified coords
      SUBROUTINE GFX_GRID(XYTOSKY,SKYTOXY,STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Import :
      EXTERNAL XYTOSKY,SKYTOXY	! routines for coord conversion
*    Global variables :
*    Status :
      INTEGER STATUS
*    Local constants :
        DOUBLE PRECISION D90,D180,D360,DTOAS,ASTOD
        PARAMETER (D90=90.0D0,D180=180.0D0,D360=360.0D0,
     &             DTOAS=3600.0D0,ASTOD=1.0D0/3600.0D0)
*
        INTEGER MAXPTS,MAXGAP
	   PARAMETER (MAXPTS = 500, MAXGAP = 15)
*    Local variables :
        INTEGER NFRAME		! coord frame 1=cel 2=ecl 3=gal
        INTEGER POS		! position of labels 1=edge 2=cent 3=outside
        INTEGER NOMGAP(MAXGAP)
        INTEGER NP
        INTEGER I
        REAL XP(MAXPTS), YP(MAXPTS)
        REAL AMIN(2),AMAX(2),AMID(2)   ! Axis extremes
        REAL XX(8),YY(8)
        REAL SAVE
*
        DOUBLE PRECISION ELMIN, ELBEGIN, ELMAX, ELRANGE, ELSTEP,
     &   ELGAP, AZMIN, AZBEGIN, AZMAX, AZRANGE, AZSTEP, AZGAP,
     &   AZCEN, ELCEN, AZ, EL
        LOGICAL FOK,POK
*
*    Statement function :
        LOGICAL INBOX
        REAL X,Y
	INBOX(X,Y) = X.GE.AMIN(1).AND.X.LE.AMAX(1).AND.
     &               Y.GE.AMIN(2).AND.Y.LE.AMAX(2)
*
*    Local data :
        DATA NOMGAP / 1, 3, 15, 30, 60, 150, 300, 900, 1800,
     &    3600, 9000, 18000, 54000, 108000, 10000000 /

*-
      IF (STATUS.EQ.SAI__OK) THEN

*  see if grid needed
        CALL GCB_GETI('GRID_FRAME',FOK,NFRAME,STATUS)
        IF (FOK) THEN

          CALL GCB_GETI('GRID_POS',POK,POS,STATUS)
          IF (.NOT.POK) THEN
            POS=1
          ENDIF

	  CALL PGSCH(0.7)
	  CALL PGSLS(4)

* get axis extremes and mid-points
          CALL PGQWIN(AMIN(1),AMAX(1),AMIN(2),AMAX(2))
          IF (AMIN(1) .GT. AMAX(1)) THEN
            SAVE=AMAX(1)
            AMAX(1)=AMIN(1)
            AMIN(1)=SAVE
          ENDIF
          AMID(1)=(AMIN(1)+AMAX(2))*0.5
          AMID(2)=(AMIN(2)+AMAX(2))*0.5

          XX(1)=AMIN(1)
          YY(1)=AMIN(2)
          XX(2)=AMIN(1)
          YY(2)=AMID(2)
          XX(3)=AMIN(1)
          YY(3)=AMAX(2)
          XX(4)=AMID(1)
          YY(4)=AMAX(2)
          XX(5)=AMAX(1)
          YY(5)=AMAX(2)
          XX(6)=AMAX(1)
          YY(6)=AMID(2)
          XX(7)=AMAX(1)
          YY(7)=AMIN(2)
          XX(8)=AMID(1)
          YY(8)=AMIN(2)


*
* Find AZ and EL of the limiting positions, this includes the four corners
* and also the four mid-points (which might be just outside range if
* shape is distorted at high elevations).
* Compute AZ values relative to the centre of the plot (AZCEN) and adjust later
* to avoid wrap-around problem at 360.
	  AZMIN = +D360
	  AZMAX = -D360
	  ELMIN = +D360
	  ELMAX = -D360
	  CALL XYTOSKY(AMID(1),AMID(2),NFRAME,AZCEN,ELCEN)

	  DO I=1,8
            CALL XYTOSKY(XX(I), YY(I), NFRAME, AZ, EL)
	    AZ = MOD(AZ-AZCEN+D360+D180, D360) - D180
	    AZMIN = MIN(AZMIN, AZ)
	    AZMAX = MAX(AZMAX, AZ)
	    ELMIN = MIN(ELMIN, EL)
	    ELMAX = MAX(ELMAX, EL)
          ENDDO

	  AZMIN = AZMIN + AZCEN
	  AZMAX = AZMAX + AZCEN
*
* Is North pole in field?
	  CALL SKYTOXY(0.0D0, D90,  NFRAME, X, Y)
*
	  IF ( INBOX(X,Y) ) THEN
	    ELMAX = D90 - ASTOD
	    AZMIN = 0.0D0
	    AZMAX = D360
	    CALL PGPTEXT(X, Y, 0.0, 0.5, 'N')
	  ENDIF
*
* Is South pole in field?
	  CALL SKYTOXY(0.0D0, -D90, NFRAME, X, Y)
*
	  IF ( INBOX(X,Y) ) THEN
	    ELMIN = -(D90 - ASTOD)
	    AZMIN = 0.0D0
	    AZMAX = D360
	    CALL PGPTEXT(X, Y, 0.0, 0.5, 'S')
	  ENDIF
*
* Choose Azimuth gap as whole numbers of arcseconds
	  AZRANGE = (AZMAX - AZMIN) * DTOAS
          I=2
	  DO WHILE (I.LT.MAXGAP.AND.NOMGAP(I)/AZRANGE.LT.0.3)
            I=I+1
          ENDDO
	  AZGAP = REAL(NOMGAP(I-1)) * ASTOD
*
* Ditto elevation gap.
	  ELRANGE = ABS(ELMAX - ELMIN) * DTOAS
          I=2
	  DO WHILE (I.LT.MAXGAP.AND.NOMGAP(I)/ELRANGE.LT.0.3)
            I=I+1
          ENDDO
	  ELGAP = REAL(NOMGAP(I-1)) * ASTOD

*
* Plot lines of constant elevation
*
	  ELBEGIN = ELMIN - MOD(ELMIN, ELGAP)
*
	  IF (ELBEGIN .GT. 0D0) THEN
            ELBEGIN = ELBEGIN + ELGAP
          ENDIF
          IF (ELBEGIN.EQ.ELMIN) THEN
            ELBEGIN=ELBEGIN+ELGAP
          ENDIF
*
	  AZSTEP  = -AZGAP / 40.D0
*
*
*
*
          EL=ELBEGIN
          DO WHILE (EL.LT.ELMAX)
	    NP = 0
	    DO AZ = AZMAX, AZMIN, AZSTEP

	      CALL SKYTOXY(AZ, EL, NFRAME, X, Y)

	      IF (INBOX(X,Y) .AND. NP .LT. MAXPTS) THEN
	        NP = NP + 1
                XP(NP) = X
	        YP(NP) = Y
	      ENDIF

            ENDDO
*
	    CALL GFX_GRID_LINE(NP, XP, YP)
            CALL GFX_GRID_LABEL(NP,XP,YP,EL,NFRAME,POS,.FALSE.)
*
            EL=EL+ELGAP

	  ENDDO
*
* Plot lines of constant azimuth.
	  AZBEGIN = AZMIN - MOD(AZMIN, AZGAP)
          IF (AZBEGIN.GT.0.0D0) THEN
            AZBEGIN=AZBEGIN+AZGAP
          ENDIF
          IF (AZBEGIN.EQ.AZMIN) THEN
            AZBEGIN=AZBEGIN+AZGAP
          ENDIF
	  ELSTEP = ELGAP / 40.D0
*
          AZ=AZBEGIN
          DO WHILE (AZ.LT.AZMAX)
            NP = 0
            DO EL = ELMIN, ELMAX, ELSTEP
              CALL SKYTOXY(AZ, EL, NFRAME, X, Y)
              IF(INBOX(X,Y) .AND. NP .LT. MAXPTS) THEN
	        NP = NP + 1
	        XP(NP) = X
	        YP(NP) = Y
	      ENDIF
	    ENDDO

            CALL GFX_GRID_LINE(NP, XP, YP)
            CALL GFX_GRID_LABEL(NP,XP,YP,AZ,NFRAME,POS,.TRUE.)
*
            AZ=AZ+AZGAP

	  ENDDO
*
* Restore original settings

          CALL GCB_SETDEF(STATUS)

        ENDIF

      ENDIF

      END


	SUBROUTINE GFX_GRID_LINE(NP,XP,YP)
*    Description :
*    Type Definitions :
      IMPLICIT NONE
*    Import :
	INTEGER NP		! Number of points.
	REAL XP(*)		! X coordinates (pixels)
	REAL YP(*)		! Y coordinates (pixels)
*    Import-Export :
*    Export :
*    Local constants :
*    Global variables :
*    Local variables :
*
* Check if number of points is less than two.
	IF (NP .GT. 2) THEN

           CALL PGLINE(NP,XP,YP)

        ENDIF

	END

*+
	SUBROUTINE GFX_GRID_LABEL(NP,XP,YP,THETA,NFRAME,POS,AZI)

        IMPLICIT NONE

*  Import :
        INTEGER NP
        REAL XP(*),YP(*)
	DOUBLE PRECISION THETA	!input	Angle to encode, radians
	INTEGER NFRAME		!input	Coord frame, 1=cel, 2=ecl, 3=gal.
        INTEGER POS		!text position 1=edge 2=centre 3=outside
        LOGICAL AZI             ! plotting azimuth lines

*  Constants :
        REAL PI,RTOD
	PARAMETER (PI=3.14159265, RTOD = 180.0/PI)
*  Local variables :
	CHARACTER LABEL*40
        INTEGER NC
        INTEGER N
        DOUBLE PRECISION PHI
        REAL X,Y,ANGLE
        REAL X1,X2,Y1,Y2
        REAL JUST
*-

*Annotation as follows:
*NFRAME=1 	RA     hh mm ss		DEC  +dd mm ss
*NFRAME=2	ELONG  dd mm ss		ELAT +dd mm ss
*NFRAME=3	GLONG  ddd.dd		GLAT +dd.dd

      IF (NP.GT.10) THEN

        IF (AZI) THEN
          IF (THETA.LT.0.0D0) THEN
            PHI=360.0D0+THETA
          ELSE
            PHI=THETA
          ENDIF
          IF (NFRAME.EQ.1) THEN
            NC=8
            CALL CONV_DEGHMS(REAL(PHI),LABEL)
          ELSEIF (NFRAME.EQ.2) THEN
            NC=9
            CALL CONV_DEGDMS(REAL(PHI),LABEL)
          ELSEIF (NFRAME.EQ.3) THEN
            NC=7
            WRITE(LABEL(:NC),'(F7.2)') REAL(PHI)
          ENDIF
        ELSE
          IF (NFRAME.EQ.1) THEN
            NC=9
            CALL CONV_DEGDMS(REAL(THETA),LABEL)
          ELSEIF (NFRAME.EQ.2) THEN
            NC=9
            CALL CONV_DEGDMS(REAL(THETA),LABEL)
          ELSEIF (NFRAME.EQ.3) THEN
            NC=6
            WRITE(LABEL(:NC),'(F6.2)') REAL(THETA)
          ENDIF
        ENDIF

        IF (POS.EQ.1) THEN
          X=XP(1)
          X1=X
          X2=XP(2)
          Y=YP(1)
          Y1=Y
          Y2=YP(2)
          ANGLE = ATAN2(Y2-Y1, X1-X2) * RTOD
          IF(ABS(ANGLE) .GT. 90.0) THEN
            ANGLE = ANGLE + 180.0
            JUST = +1.1
          ELSE
            JUST = -0.1
          ENDIF
          CALL PGPTEXT(X, Y, ANGLE, JUST, LABEL(1:NC))

        ELSEIF (POS.EQ.2) THEN
          N=NP/2
          X=XP(N)
          X1=X
          X2=XP(N+1)
          Y=YP(N)
          Y1=Y
          Y2=YP(N+1)
          ANGLE = ATAN2(Y2-Y1, X1-X2) * RTOD
          IF(ABS(ANGLE) .GT. 90.0) THEN
            ANGLE = ANGLE + 180.0
          ENDIF
          JUST=0.5
          CALL PGPTEXT(X, Y, ANGLE, JUST, LABEL(1:NC))

        ELSEIF (POS.EQ.3) THEN
          X=XP(1)
          X1=X
          X2=XP(2)
          Y=YP(1)
          Y1=Y
          Y2=YP(2)
          ANGLE = ATAN2(Y2-Y1, X1-X2) * RTOD
          IF(ABS(ANGLE) .GT. 90.0) THEN
            ANGLE = ANGLE + 180.0
            JUST = -0.1
          ELSE
            JUST = +1.1
          ENDIF
          CALL PGPTEXT(X, Y, ANGLE, JUST, LABEL(1:NC))
          X=XP(NP)
          X1=XP(NP-1)
          X2=XP(NP)
          Y=YP(NP)
          Y1=YP(NP-1)
          Y2=YP(NP)
          ANGLE = ATAN2(Y2-Y1, X1-X2) * RTOD
          IF(ABS(ANGLE) .GT. 90.0) THEN
            ANGLE = ANGLE + 180.0
            JUST = +1.1
          ELSE
            JUST = -0.1
          ENDIF
          CALL PGPTEXT(X, Y, ANGLE, JUST, LABEL(1:NC))
        ENDIF

      ENDIF

      END
