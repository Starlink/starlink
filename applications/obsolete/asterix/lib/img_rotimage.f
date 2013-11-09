*+  IMG_ROTIMAGE - Rotates and shifts image about selected centre.
      SUBROUTINE IMG_ROTIMAGE(NCTS, XINOFF, YINOFF, PHI, COSPHI,
     :               SINPHI, NREBINX, NREBINY, NXPIX, NYPIX, IX, IY)
*
*-Author	Richard Saxton	1988 Jan
*
* Description:
*     Rotates data pixels about an angle PHI
*       and offsets from the source minimum position.
* Global:
        IMPLICIT NONE
* Local:
	INTEGER NCTS		!input	No of events
	REAL XINOFF		!input	X-offset of image centre, output pixels
	REAL YINOFF		!input	Y-offset of image centre, output pixels
	REAL PHI		!input	Angle of new axes wrt to old.
	REAL COSPHI		!input	Cos of PHI
	REAL SINPHI		!input	Sin of PHI
	INTEGER NREBINX		!input	Input pixels per output pixel Y dim.
	INTEGER NREBINY		!input	Input pixels per output pixel X dim.
	INTEGER NXPIX		!input	Size of X edge of output image, pixels.
	INTEGER NYPIX		!input	Size of Y edge of output image, pixels.
	INTEGER IX(NCTS)	!in/out	X-coordinates, input range 0-2047.
	INTEGER IY(NCTS)	!in/out	Y-coordinates, input range 0-2047.
*
* Functions:
        REAL SLA_RANDOM
*
* Internal:
        REAL DX,DY,XROT,YROT
        REAL X,Y,XOFF,YOFF
        REAL DITHX(64), DITHY(64)
        INTEGER IXOFF,IYOFF
        INTEGER I,ISEED,LP
        LOGICAL FIRST
*
* Initialise
        DATA ISEED/314159265/, FIRST /.TRUE./
*
* Create dither components if rotation required
        IF (FIRST .AND. PHI .NE. 0.0) THEN
           DO LP=1,64
              DITHX(LP)=SLA_RANDOM(ISEED)-0.5
              DITHY(LP)=SLA_RANDOM(ISEED)-0.5
           ENDDO
           FIRST=.FALSE.
        ENDIF
*
	IF(PHI .EQ. 0.0) THEN
	    IXOFF = NINT(0.5 * NXPIX*NREBINX - 1024 - XINOFF)
	    IYOFF = NINT(0.5 * NYPIX*NREBINY  - 1024 - YINOFF)
	    DO I = 1,NCTS
		IX(I) = (IX(I) + IXOFF)
		IY(I) = (IY(I) + IYOFF)
	    END DO
	ELSE
*
	    DX = 0.5*NXPIX*NREBINX
	    DY = 0.5*NYPIX*NREBINY
	    XOFF = XINOFF + 1024.0
	    YOFF = YINOFF + 1024.0
*
	    DO I = 1,NCTS
		X = IX(I) - XOFF
		Y = IY(I) - YOFF
		XROT = X * COSPHI + Y * SINPHI
		YROT = Y * COSPHI - X * SINPHI
		IX(I) = NINT(XROT + DX + DITHX(I))
		IY(I) = NINT(YROT + DY + DITHY(I))
	    END DO
	END IF
	END
