*+ GFX_XYTOSKY	Converts axis coords to sky coords
	SUBROUTINE GFX_XYTOSKY(X,Y,FRAME,AZ,EL)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'DAT_PAR'
*  Import :
	REAL X				!input	X value
	REAL Y				!input	Y value
        INTEGER FRAME
*  Export :
	DOUBLE PRECISION AZ	!output	azimuth (degrees)
	DOUBLE PRECISION EL	!output	elevation (degrees)
*  Status :
        INTEGER STATUS
*  Global variables :
        INCLUDE 'GFX_SKY_CMN'
*  Local constants :
        DOUBLE PRECISION PI,RTOD
	PARAMETER (PI = 3.14159265358979D0,RTOD=180.0D0/PI)
*  Local variables :
*-
        STATUS=SAI__OK

        CALL CONV_XYTOPOL(X*G_XYTORAD,Y*G_XYTORAD,
     :                       G_TMAT(1,1,FRAME),AZ,EL,STATUS)
        AZ=AZ*RTOD
        EL=EL*RTOD


      END
