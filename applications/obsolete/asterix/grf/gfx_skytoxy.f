*+ GFX_SKYTOXY	Converts sky coords to axis coords
	SUBROUTINE GFX_SKYTOXY(AZ,EL,FRAME,X,Y)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'DAT_PAR'
*  Import :
	DOUBLE PRECISION AZ		!input	azimuth (degrees)
	DOUBLE PRECISION EL		!input	elevation (degrees)
        INTEGER FRAME
*  Export :
	REAL X				!output	X value
	REAL Y				!output	Y value
*  Status :
        INTEGER STATUS
*  Global variables :
        INCLUDE 'GFX_SKY_CMN'
*  Local constants :
        DOUBLE PRECISION PI,DTOR
	PARAMETER (PI = 3.14159265358979D0,DTOR=PI/180.0D0)
*  Local variables :
*-
        STATUS=SAI__OK
        CALL CONV_POLTOXY(AZ*DTOR,EL*DTOR,G_TMAT(1,1,FRAME),X,Y,STATUS)
        X=X/G_XYTORAD
        Y=Y/G_XYTORAD


      END
