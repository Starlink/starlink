*+  IMG_SUB - subroutines for image processing system
      SUBROUTINE IMG_WORLDTOPIX(XW,YW,XP,YP,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      REAL XW,YW
*    Export :
      REAL XP,YP
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*    Version :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        XP= (XW-I_XBASE+I_XSCALE)/I_XSCALE
        YP= (YW-I_YBASE+I_YSCALE)/I_YSCALE

      ENDIF

      END


      SUBROUTINE IMG_PIXTOWORLD(XP,YP,XW,YW,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      REAL XP,YP
*    Export :
      REAL XW,YW
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*    Version :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        XW=I_XBASE+(XP-1.0)*I_XSCALE
        YW=I_YBASE+(YP-1.0)*I_YSCALE

      ENDIF

      END


      SUBROUTINE IMG_GETVAL(I,J,VAL,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER I,J
*    Export :
      REAL VAL
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*    Version :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETVAL_INT(%VAL(I_DPTR),I_NX,I_NY,I,J,VAL)

      ENDIF

      END

      SUBROUTINE IMG_GETVAL_INT(D,NX,NY,I,J,VAL)

      INTEGER NX,NY
      REAL D(NX,NY)
      INTEGER I,J
      REAL VAL

      VAL=D(I,J)

      END

      SUBROUTINE IMG_GETVAR(I,J,VAL,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER I,J
*    Export :
      REAL VAL
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*    Version :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETVAL_INT(%VAL(I_VPTR),I_NX,I_NY,I,J,VAL)

      ENDIF

      END


      SUBROUTINE IMG_GETQUAL(I,J,QVAL,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER I,J
*    Export :
      BYTE QVAL
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*    Version :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETQUAL_INT(%VAL(I_QPTR),I_NX,I_NY,I,J,QVAL)

      ENDIF

      END

      SUBROUTINE IMG_GETQUAL_INT(Q,NX,NY,I,J,QVAL)

      INTEGER NX,NY
      BYTE Q(NX,NY)
      INTEGER I,J
      BYTE QVAL

      QVAL=Q(I,J)

      END


      SUBROUTINE IMG_GETREG(I,J,RVAL,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER I,J
*    Export :
      BYTE RVAL
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*    Version :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_GETREG_INT(%VAL(I_REG_PTR),I_NX,I_NY,I,J,RVAL)

      ENDIF

      END

      SUBROUTINE IMG_GETREG_INT(REG,NX,NY,I,J,RVAL)

      INTEGER NX,NY
      BYTE REG(NX,NY)
      INTEGER I,J
      BYTE RVAL

      RVAL=REG(I,J)

      END


      SUBROUTINE IMG_SWAP(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
*    Export :
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*80 CDUM
      INTEGER IDUM
      REAL RDUM
      BYTE BDUM
      LOGICAL LDUM
*-
      IF (STATUS.EQ.SAI__OK) THEN

        RDUM=I_XBASE
        I_XBASE=I_XBASE_W
        I_XBASE_W=RDUM

        RDUM=I_YBASE
        I_YBASE=I_YBASE_W
        I_YBASE_W=RDUM

        RDUM=I_XSCALE
        I_XSCALE=I_XSCALE_W
        I_XSCALE_W=RDUM

        RDUM=I_YSCALE
        I_YSCALE=I_YSCALE_W
        I_YSCALE_W=RDUM

        RDUM=I_XWID
        I_XWID=I_XWID_W
        I_XWID_W=RDUM

        RDUM=I_YWID
        I_YWID=I_YWID_W
        I_YWID_W=RDUM

        RDUM=I_DMIN
        I_DMIN=I_DMIN_W
        I_DMIN_W=RDUM

        RDUM=I_DMAX
        I_DMAX=I_DMAX_W
        I_DMAX_W=RDUM

        IDUM=I_DPTR
        I_DPTR=I_DPTR_W
        I_DPTR_W=IDUM

        IDUM=I_XPTR
        I_XPTR=I_XPTR_W
        I_XPTR_W=IDUM

        IDUM=I_YPTR
        I_YPTR=I_YPTR_W
        I_YPTR_W=IDUM

        IDUM=I_VPTR
        I_VPTR=I_VPTR_W
        I_VPTR_W=IDUM

        IDUM=I_QPTR
        I_QPTR=I_QPTR_W
        I_QPTR_W=IDUM

        BDUM=I_MASK
        I_MASK=I_MASK_W
        I_MASK_W=BDUM

        LDUM=I_BAD
        I_BAD=I_BAD_W
        I_BAD_W=LDUM

        CDUM=I_TITLE
        I_TITLE=I_TITLE_W
        I_TITLE_W=CDUM

      ENDIF

      END



*+ IMG_TMAT - Get coordinate transformation matrices
	SUBROUTINE IMG_TMAT(STATUS)
*
*    Description:
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local variables :
      DOUBLE PRECISION		EPOCH			! Epoch of obs'n

      REAL			EQNX			! Equinox of input sys
*-

*  Status ok
      IF (STATUS .EQ. SAI__OK) THEN

*    Extract equinox and epoch from input WCS
        IF ( I_SYSID .NE. ADI__NULLID ) THEN
          CALL ADI_CGET0R( I_SYSID, 'EQUINOX', EQNX, STATUS )
          CALL ADI_CGET0D( I_SYSID, 'EPOCH', EPOCH, STATUS )
        ELSE
          EQNX = 2000.0
          EPOCH = 2000.0
        END IF

*    Create descriptions of eclipic and galactic coordinates
        CALL WCI_NEWSYS( 'ECL', EQNX, EPOCH, I_ECLSYS, STATUS )
        CALL WCI_NEWSYS( 'GAL', EQNX, EPOCH, I_GALSYS, STATUS )

      END IF

      END



*+ IMG_WORLDTOCEL - world (axis) coords to celestial coords
	SUBROUTINE IMG_WORLDTOCEL(X,Y,RA,DEC,STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*  Import :
	REAL X				!input	X value
	REAL Y				!input	Y value
*  Export :
	DOUBLE PRECISION RA	!output	RA (degrees)
	DOUBLE PRECISION DEC	!output	DEC (degrees)
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local variables :
        DOUBLE PRECISION LCEL(2)
        REAL	LWORLD(2)
*-
      IF (STATUS.EQ.SAI__OK) THEN

        LWORLD(1) = X
        LWORLD(2) = Y

        CALL WCI_CNA2S( LWORLD, I_PIXID, I_PRJID, LCEL, STATUS )

        RA = LCEL(1) * MATH__DRTOD
        DEC = LCEL(2) * MATH__DRTOD

      ENDIF

      END


*+ IMG_CELTOWORLD - Converts celestial coords to world coords
	SUBROUTINE IMG_CELTOWORLD(RA,DEC,X,Y,STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*  Import :
	DOUBLE PRECISION RA	!input	RA (degrees)
	DOUBLE PRECISION DEC	!input	DEC (degrees)
*  Export :
	REAL X				!output	X value
	REAL Y				!output	Y value
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local variables :
        DOUBLE PRECISION LCEL(2)
        REAL	LWORLD(2)
*-
      IF (STATUS.EQ.SAI__OK) THEN

        LCEL(1) = RA * MATH__DDTOR
        LCEL(2) = DEC * MATH__DDTOR

        CALL WCI_CNS2A( LCEL, I_PIXID, I_PRJID, LWORLD, STATUS )

        X = LWORLD(1)
        Y = LWORLD(2)

      ENDIF

      END



*+ IMG_WORLDTOECL - world (axis) coords to ecliptic coords
	SUBROUTINE IMG_WORLDTOECL(X,Y,LON,LAT,STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*  Import :
	REAL X				!input	X value
	REAL Y				!input	Y value
*  Export :
	DOUBLE PRECISION LON	!output	longitude (degrees)
	DOUBLE PRECISION LAT	!output	latitude (degrees)
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local variables :
      DOUBLE PRECISION		FILESYS(2),LECL(2)

      REAL			LWORLD(2)
*-
      IF (STATUS.EQ.SAI__OK) THEN

*    Local copy of world coords
        LWORLD(1) = X
        LWORLD(2) = Y

*    Perform conversion to file system
        CALL WCI_CNA2S( LWORLD, I_PIXID, I_PRJID, FILESYS, STATUS )

*    Convert to ecliptic
        CALL WCI_CNS2S( FILESYS, I_SYSID, I_ECLSYS, LECL, STATUS )

*    Export in degrees
        LON = LECL(1) * MATH__DRTOD
        LAT = LECL(2) * MATH__DRTOD

      ENDIF

      END


*+ IMG_ECLTOWORLD - Converts ecliptic coords to world coords
	SUBROUTINE IMG_ECLTOWORLD(LON,LAT,X,Y,STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*  Import :
	DOUBLE PRECISION LON	!input longitude (degrees)
	DOUBLE PRECISION LAT	!input latitude (degrees)
*  Export :
	REAL X				!output	X value
	REAL Y				!output	Y value
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local variables :
        DOUBLE PRECISION FILESYS(2),LECL(2)
      REAL LWORLD(2)
*-
      IF (STATUS.EQ.SAI__OK) THEN

*    Export in degrees
        LECL(1) = LON * MATH__DDTOR
        LECL(2) = LAT * MATH__DDTOR

*    Convert to file system
        CALL WCI_CNS2S( LECL, I_SYSID, I_ECLSYS, FILESYS, STATUS )

*    Perform conversion to world
        CALL WCI_CNS2A( FILESYS, I_PIXID, I_PRJID, LWORLD, STATUS )

*    Local copy of world coords
        X = LWORLD(1)
        Y = LWORLD(2)

      ENDIF

      END




*+ IMG_WORLDTOGAL - world (axis) coords to galactic coords
	SUBROUTINE IMG_WORLDTOGAL(X,Y,L,B,STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*  Import :
	REAL X				!input	X value
	REAL Y				!input	Y value
*  Export :
	DOUBLE PRECISION L	!output	longitude (degrees)
	DOUBLE PRECISION B	!output	latitude (degrees)
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'

*  Local variables :
      DOUBLE PRECISION 		FILESYS(2), LCEL(2)
      REAL			LWORLD(2)
*-
      IF (STATUS.EQ.SAI__OK) THEN

*    Local copy of world coords
        LWORLD(1) = X
        LWORLD(2) = Y

*    Perform conversion to file system
        CALL WCI_CNA2S( LWORLD, I_PIXID, I_PRJID, FILESYS, STATUS )

*    Convert to ecliptic
        CALL WCI_CNS2S( FILESYS, I_SYSID, I_GALSYS, LCEL, STATUS )

*    Export in degrees
        L = LCEL(1) * MATH__DRTOD
        B = LCEL(2) * MATH__DRTOD

      END IF

      END


*+ IMG_GALTOWORLD - Converts celestial coords to world coords
	SUBROUTINE IMG_GALTOWORLD(L,B,X,Y,STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*  Import :
	DOUBLE PRECISION L	!input	longitude (degrees)
	DOUBLE PRECISION B	!input	latitude (degrees)
*  Export :
	REAL X				!output	X value
	REAL Y				!output	Y value
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local variables :
        DOUBLE PRECISION FILESYS(2),LECL(2)
      REAL LWORLD(2)
*-
      IF (STATUS.EQ.SAI__OK) THEN

*    Export in degrees
        LECL(1) = L * MATH__DDTOR
        LECL(2) = B * MATH__DDTOR

*    Convert to file system
        CALL WCI_CNS2S( LECL, I_SYSID, I_GALSYS, FILESYS, STATUS )

*    Perform conversion to world
        CALL WCI_CNS2A( FILESYS, I_PIXID, I_PRJID, LWORLD, STATUS )

*    Local copy of world coords
        X = LWORLD(1)
        Y = LWORLD(2)

      END IF

      END



*+ IMG_CLEAR - clear current image
	SUBROUTINE IMG_CLEAR(HALF,STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*  Import :
      INTEGER HALF
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
      INTEGER N
      PARAMETER (N=5)
*  Local variables :
      REAL X(N)/0.0,1.0,1.0,0.0,0.0/
      REAL Y(N)/0.0,0.0,1.0,1.0,0.0/
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  select appropriate half of display
        IF (HALF.EQ.1) THEN
          CALL PGVPORT(0.0,0.5,0.0,1.0)
        ELSE
          CALL PGVPORT(0.5,1.0,0.0,1.0)
        ENDIF
        CALL PGWINDOW(0.0,1.0,0.0,1.0)

*  set plotting colour to background
        CALL PGSCI(0)

*  plot blank filled rectangle
        CALL PGPOLY(N,X,Y)

*  restore plotting defaults
        CALL GCB_SETDEF(STATUS)

      ENDIF

      END




*+ IMG_DISP - display image
	SUBROUTINE IMG_DISP(STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
      LOGICAL OK,YES
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_GETL('PIX_FLAG',OK,YES,STATUS)
        IF (OK.AND.YES) THEN
          CALL IMG_PIXEL(STATUS)
        ENDIF
        CALL GCB_GETL('CONT_FLAG',OK,YES,STATUS)
        IF (OK.AND.YES) THEN
          CALL IMG_CONTOUR(STATUS)
        ENDIF
        CALL IMG_GRID(STATUS)
        CALL GFX_MARKS(STATUS)
        CALL GFX_SHAPES(STATUS)
        CALL GFX_NOTES(STATUS)
        I_PMIN=I_DMIN
        I_PMAX=I_DMAX
        CALL GFX_KEY(I_PMIN,I_PMAX,STATUS)

      ENDIF

      END



*+ IMG_PIXEL - display pixels only
	SUBROUTINE IMG_PIXEL(STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  set colours
        CALL GFX_SETCOLS(STATUS)

        I_PMIN=I_DMIN
        I_PMAX=I_DMAX

        IF (I_QOK.AND.I_BAD) THEN

*  plot pixels
          CALL GFX_PIXELQ(I_WKPTR,I_NX,I_NY,I_IX1,I_IX2,I_IY1,I_IY2,
     :                .TRUE.,%VAL(I_XPTR),%VAL(I_YPTR),0,0,%VAL(I_DPTR),
     :                         I_PMIN,I_PMAX,%VAL(I_QPTR),I_MASK,STATUS)
        ELSE
          CALL GFX_PIXEL(I_WKPTR,I_NX,I_NY,I_IX1,I_IX2,I_IY1,I_IY2,
     :                .TRUE.,%VAL(I_XPTR),%VAL(I_YPTR),0,0,%VAL(I_DPTR),
     :                                             I_PMIN,I_PMAX,STATUS)
        ENDIF

      ENDIF

      END



*+ IMG_QPIXEL - display QUALITY only
	SUBROUTINE IMG_QPIXEL(STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  set colours
        CALL GFX_SETCOLS(STATUS)


        IF (I_QOK) THEN

*  plot pixels
          CALL GFX_QPIXEL(I_WKPTR,I_NX,I_NY,I_IX1,I_IX2,I_IY1,I_IY2,
     :                           .TRUE.,%VAL(I_XPTR),%VAL(I_YPTR),0,0,
     :                                    %VAL(I_QPTR),I_MASK,STATUS)
        ENDIF

      ENDIF

      END



*+ IMG_RPIXEL - display region mask
	SUBROUTINE IMG_RPIXEL(STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*  Import :
*  Export :
*  Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  set colours
        CALL GFX_SETCOLS(STATUS)


        IF (I_QOK) THEN

*  plot pixels
          CALL GFX_QPIXEL(I_WKPTR,I_NX,I_NY,I_IX1,I_IX2,I_IY1,I_IY2,
     :                           .TRUE.,%VAL(I_XPTR),%VAL(I_YPTR),0,0,
     :                              %VAL(I_REG_PTR),QUAL__MASK,STATUS)
        ENDIF

      ENDIF

      END


*+ IMG_SUBPIXEL - display pixels of subset of image
	SUBROUTINE IMG_SUBPIXEL(I1,I2,J1,J2,STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*  Import :
      INTEGER I1,I2,J1,J2
*  Export :
*  Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        I_PMIN=I_DMIN
        I_PMAX=I_DMAX

        IF (I_QOK.AND.I_BAD) THEN

*  plot pixels
          CALL GFX_PIXELQ(I_WKPTR,I_NX,I_NY,I1,I2,J1,J2,
     :                .TRUE.,%VAL(I_XPTR),%VAL(I_YPTR),0,0,%VAL(I_DPTR),
     :                         I_PMIN,I_PMAX,%VAL(I_QPTR),I_MASK,STATUS)
        ELSE
          CALL GFX_PIXEL(I_WKPTR,I_NX,I_NY,I1,I2,J1,J2,
     :                .TRUE.,%VAL(I_XPTR),%VAL(I_YPTR),0,0,%VAL(I_DPTR),
     :                                             I_PMIN,I_PMAX,STATUS)
        ENDIF

      ENDIF

      END


*+ IMG_CONTOUR - display pixels only
	SUBROUTINE IMG_CONTOUR(STATUS)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN



*  plot contours
        I_PMIN=I_DMIN
        I_PMAX=I_DMAX
        IF (I_QOK.AND.I_BAD) THEN
          CALL GFX_CONTOURQ(I_NX,I_NY,I_IX1,I_IX2,I_IY1,I_IY2,
     :                      %VAL(I_XPTR),%VAL(I_YPTR),%VAL(I_DPTR),
     :                        I_PMIN,I_PMAX,%VAL(I_QPTR),I_MASK,STATUS)
        ELSE
          CALL GFX_CONTOUR(I_NX,I_NY,I_IX1,I_IX2,I_IY1,I_IY2,
     :                        %VAL(I_XPTR),%VAL(I_YPTR),%VAL(I_DPTR),
     :                                           I_PMIN,I_PMAX,STATUS)
        ENDIF



      ENDIF

      END

*+
      SUBROUTINE IMG_GRID(STATUS)
*    Description :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    External functions :
      EXTERNAL IMG_SKYTOXY,IMG_XYTOSKY
*    Local constants :
*    Local variables :
*
*-


      IF (STATUS.EQ.SAI__OK) THEN

        CALL GFX_GRID(IMG_XYTOSKY,IMG_SKYTOXY,STATUS)


      ENDIF

      END



      SUBROUTINE IMG_SKYTOXY(AZ,EL,NFRAME,X,Y)

      IMPLICIT NONE

      DOUBLE PRECISION AZ,EL
      INTEGER NFRAME
      REAL X,Y

      INTEGER STATUS
      INCLUDE 'SAE_PAR'
      STATUS=SAI__OK

      IF (NFRAME.EQ.1) THEN
        CALL IMG_CELTOWORLD(AZ,EL,X,Y,STATUS)
      ELSEIF (NFRAME.EQ.2) THEN
        CALL IMG_ECLTOWORLD(AZ,EL,X,Y,STATUS)
      ELSEIF (NFRAME.EQ.3) THEN
        CALL IMG_GALTOWORLD(AZ,EL,X,Y,STATUS)
      ENDIF

      END



      SUBROUTINE IMG_XYTOSKY(X,Y,NFRAME,AZ,EL)

      IMPLICIT NONE

      REAL X,Y
      INTEGER NFRAME
      DOUBLE PRECISION AZ,EL

      INTEGER STATUS
      INCLUDE 'SAE_PAR'
      STATUS=SAI__OK

      IF (NFRAME.EQ.1) THEN
        CALL IMG_WORLDTOCEL(X,Y,AZ,EL,STATUS)
      ELSEIF (NFRAME.EQ.2) THEN
        CALL IMG_WORLDTOECL(X,Y,AZ,EL,STATUS)
      ELSEIF (NFRAME.EQ.3) THEN
        CALL IMG_WORLDTOGAL(X,Y,AZ,EL,STATUS)
      ENDIF

      END




*+
	SUBROUTINE IMG_WINDOW(STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
      REAL X1,X2,Y1,Y2
      REAL XW1,XW2,YW1,YW2
      REAL XP1,XP2,YP1,YP2
      LOGICAL ABS,SCALED
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get world coords of edges of image
        XP1=REAL(I_IX1)-0.5
        XP2=REAL(I_IX2)+0.5
        YP1=REAL(I_IY1)-0.5
        YP2=REAL(I_IY2)+0.5
        CALL IMG_PIXTOWORLD(XP1,YP1,XW1,YW1,STATUS)
        CALL IMG_PIXTOWORLD(XP2,YP2,XW2,YW2,STATUS)

        ABS=.FALSE.
        SCALED=.TRUE.

*  set viewport to half display surface after clearing it
        IF (I_SPLIT_DISP) THEN
          X1=0.0
          X2=0.5
          Y1=0.0
          Y2=1.0
*  or whole surface
        ELSE
          X1=0.0
          X2=1.0
          Y1=0.0
          Y2=1.0
        ENDIF
        CALL GFX_VPORT(X1,X2,Y1,Y2,ABS,STATUS)

        CALL GFX_WINDOW(XW1,XW2,YW1,YW2,SCALED,STATUS)

*  store transformation
        CALL GTR_SAVE(.FALSE.,X1,X2,Y1,Y2,ABS,XW1,XW2,YW1,YW2,
     :                                              SCALED,STATUS)

      ENDIF

      END


	SUBROUTINE IMG_AXES(STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
      CHARACTER*40 XLABEL,YLABEL
      LOGICAL RADEC
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  RADEC axes?
        CALL GFX_QRADEC(RADEC,STATUS)

*  draw box and labels
        IF (RADEC) THEN
          CALL GFX_RADEC(I_XYUNITS,I_RA,I_DEC,I_ROLL,STATUS)
          CALL GFX_LABELS('Right Ascension','Declination',STATUS)
        ELSE
          CALL GFX_AXES(STATUS)
          CALL GFX_DEFLBL(I_XLABEL,I_XYUNITS,XLABEL,STATUS)
          CALL GFX_DEFLBL(I_YLABEL,I_XYUNITS,YLABEL,STATUS)
          CALL GFX_LABELS(XLABEL,YLABEL,STATUS)
        ENDIF

*  title
        CALL GFX_TITLE(I_TITLE,STATUS)

      ENDIF

      END


*+ IMG_1DGCB
	SUBROUTINE IMG_1DGCB(STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN


*  if 2D data currently displayed
        IF (I_DISP) THEN
*  cache GCB for 2D data
          CALL GCB_CACHE(I_CACHE,STATUS)
          IF (I_CACHE_1D.EQ.0) THEN
*  create one for 1D if first time
            CALL GCB_CRECACHE(I_CACHE_1D,STATUS)
            CALL GCB_CLEAR(STATUS)
          ELSE
*  or uncache previous one
            CALL GCB_UNCACHE(I_CACHE_1D,STATUS)
          ENDIF
        ENDIF


      ENDIF
      END

*+ IMG_2DGCB
	SUBROUTINE IMG_2DGCB(STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN


*  if 1D data currently displayed
        IF (I_DISP_1D) THEN
*  cache GCB for 1D data
          CALL GCB_CACHE(I_CACHE_1D,STATUS)
          IF (I_CACHE.EQ.0) THEN
*  create one for 2D if first time
            CALL GCB_CRECACHE(I_CACHE,STATUS)
            CALL GCB_CLEAR(STATUS)
          ELSE
*  or uncache previous one
            CALL GCB_UNCACHE(I_CACHE,STATUS)
          ENDIF
        ENDIF


      ENDIF

      END


*+ IMG_PLOT - plot 1D data
	SUBROUTINE IMG_PLOT(STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
      CHARACTER*80 XLABEL,YLABEL
      REAL X1,X2,Y1,Y2
      LOGICAL ABS,SCALED
      LOGICAL POLY,STEP,POINTS,ERRS
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL GCB_SETDEF(STATUS)

*  set viewport to half display surface
        IF (I_SPLIT_DISP) THEN
          X1=0.5
          X2=1.0
          Y1=0.0
          Y2=1.0
*  or full display
        ELSE
          X1=0.0
          X2=1.0
          Y1=0.0
          Y2=1.0
        ENDIF
        ABS=.FALSE.
        SCALED=.FALSE.
        CALL GFX_VPORT(X1,X2,Y1,Y2,ABS,STATUS)
        CALL GFX_WINDOW(I_X1_1D,I_X2_1D,I_Y1_1D,I_Y2_1D,SCALED,STATUS)

*  store transformation
c        CALL GTR_SAVE(.FALSE.,X1,X2,Y1,Y2,ABS,
c     :           I_X1_1D,I_X2_1D,I_Y1_1D,I_Y2_1D,SCALED,STATUS)

*  draw axes
        CALL GFX_AXES(STATUS)

*  put on labels
        CALL GFX_DEFLBL(I_XLABEL_1D,I_XUNITS_1D,XLABEL,STATUS)
        CALL GFX_DEFLBL(I_LABEL_1D,I_UNITS_1D,YLABEL,STATUS)
        CALL GFX_LABELS(XLABEL,YLABEL,STATUS)

        CALL GFX_TITLE(I_TITLE_1D,STATUS)

*  get plotting style
        CALL GFX_1DSTYLE(POLY,STEP,ERRS,POINTS,STATUS)
        IF (.NOT.(POLY.OR.STEP.OR.POINTS.OR.ERRS)) THEN
          ERRS=.TRUE.
        ENDIF

*  plot points or line according to style
        IF (POLY) THEN
          CALL GFX_POLY(I_N_1D,1,I_N_1D,%VAL(I_APTR_1D),
     :                                  %VAL(I_DPTR_1D),STATUS)
        ENDIF
        IF (STEP) THEN
          CALL GFX_STEP(I_N_1D,1,I_N_1D,%VAL(I_APTR_1D),
     :                                  %VAL(I_WPTR_1D),
     :                                  %VAL(I_DPTR_1D),STATUS)
        ENDIF
        IF (POINTS) THEN
          CALL GFX_POINT(I_N_1D,1,I_N_1D,%VAL(I_APTR_1D),
     :                                   %VAL(I_DPTR_1D),STATUS)
        ENDIF
        IF (ERRS) THEN
          CALL GFX_ERR(I_N_1D,1,I_N_1D,%VAL(I_APTR_1D),
     :                                 %VAL(I_WPTR_1D),
     :                                 %VAL(I_DPTR_1D),
     :                                 %VAL(I_VPTR_1D),STATUS)
        ENDIF

        CALL GCB_SETDEF(STATUS)

*  plot auxiliary line
        IF (I_N_AUX.GT.0) THEN
          CALL IMG_AUX(STATUS)
        ENDIF


      ENDIF

      END



*+ IMG_AUX - plot 1D auxiliary data as polyline
	SUBROUTINE IMG_AUX(STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL PGSLS(2)
        CALL PGLINE(I_N_AUX,%VAL(I_APTR_1D),%VAL(I_DPTR_AUX))
        CALL GCB_SETDEF(STATUS)

      ENDIF

      END


*+ IMG_COPY
	SUBROUTINE IMG_COPY(VAR,QUAL,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'QUAL_PAR'
*  Import :
        LOGICAL VAR,QUAL
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
      INTEGER N
*-
      IF (STATUS.EQ.SAI__OK) THEN


*  copy existing data to work area
        N=I_NX*I_NY
        CALL ARR_COP1R(N,%VAL(I_DPTR),%VAL(I_DPTR_W),STATUS)

*  axis values
        CALL ARR_COP1R(I_NX,%VAL(I_XPTR),%VAL(I_XPTR_W),STATUS)
        CALL ARR_COP1R(I_NY,%VAL(I_YPTR),%VAL(I_YPTR_W),STATUS)

        IF (I_VOK) THEN
          CALL ARR_COP1R(N,%VAL(I_VPTR),%VAL(I_VPTR_W),STATUS)
*  create variance if required
        ELSEIF (VAR) THEN
          CALL DYN_MAPR(1,N,I_VPTR,STATUS)
          CALL DYN_MAPR(1,N,I_VPTR_W,STATUS)
          CALL ARR_COP1R(N,%VAL(I_DPTR),%VAL(I_VPTR),STATUS)
          CALL ARR_COP1R(N,%VAL(I_DPTR),%VAL(I_VPTR_W),STATUS)
          I_VOK=(STATUS.EQ.SAI__OK)
        ENDIF
        IF (I_QOK) THEN
          CALL ARR_COP1B(N,%VAL(I_QPTR),%VAL(I_QPTR_W),STATUS)
          I_MASK_W=I_MASK
          I_BAD_W=I_BAD
*  create quality if required
        ELSEIF (QUAL) THEN
          CALL DYN_MAPB(1,N,I_QPTR,STATUS)
          CALL DYN_MAPB(1,N,I_QPTR_W,STATUS)
          CALL ARR_INIT1B(QUAL__GOOD,N,%VAL(I_QPTR),STATUS)
          CALL ARR_INIT1B(QUAL__GOOD,N,%VAL(I_QPTR_W),STATUS)
          I_QOK=(STATUS.EQ.SAI__OK)
          I_BAD_W=.FALSE.
          I_BAD=I_BAD_W
          I_MASK_W=QUAL__MASK
          I_MASK=I_MASK_W
        ENDIF

        I_XBASE_W=I_XBASE
        I_XSCALE_W=I_XSCALE
        I_YBASE_W=I_YBASE
        I_YSCALE_W=I_YSCALE
        I_XWID_W=I_XWID
        I_YWID_W=I_YWID
        I_DMIN_W=I_DMIN
        I_DMAX_W=I_DMAX
        I_TITLE_W=I_TITLE


      ENDIF

      END

*+ IMG_COPYBITS
	SUBROUTINE IMG_COPYBITS(STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN


        IF (I_QOK) THEN
          I_MASK_W=I_MASK
          I_BAD_W=I_BAD
        ENDIF

        I_XBASE_W=I_XBASE
        I_XSCALE_W=I_XSCALE
        I_YBASE_W=I_YBASE
        I_YSCALE_W=I_YSCALE
        I_XWID_W=I_XWID
        I_YWID_W=I_YWID
        I_DMIN_W=I_DMIN
        I_DMAX_W=I_DMAX
        I_TITLE_W=I_TITLE

      ENDIF

      END


*+ IMG_SAVE - save current image to file
	SUBROUTINE IMG_SAVE(ID,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*  Import :
      INTEGER			ID
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
      REAL			SPARR(2)

      INTEGER DPTR,VPTR,QPTR
      INTEGER DIMS(2),LBND(2),UBND(2)
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  top level text
        CALL BDI_PUT0C( ID, 'Title', I_TITLE, STATUS )
        CALL BDI_PUT0C( ID, 'Label', I_LABEL, STATUS )
        CALL BDI_PUT0C( ID, 'Units', I_UNITS, STATUS )

*    Set up slice bounds
        LBND(1) = I_IX1
        LBND(2) = I_IY1
        UBND(1) = I_IX2
        UBND(2) = I_IY2
        DIMS(1) = I_NX
        DIMS(2) = I_NY

*  copy data
        CALL BDI_MAPR( ID, 'Data', 'WRITE', DPTR, STATUS )
        CALL ARR_SLCOPR( 2, DIMS, %VAL(I_DPTR), LBND, UBND, %VAL(DPTR),
     :                   STATUS )
        CALL BDI_UNMAP(ID,'Data',DPTR,STATUS)

*  variance
        IF (I_VOK) THEN
          CALL BDI_MAPR( ID, 'Variance', 'WRITE', VPTR, STATUS )
          CALL ARR_SLCOPR( 2, DIMS, %VAL(I_VPTR), LBND, UBND,
     :                     %VAL(VPTR), STATUS )
          CALL BDI_UNMAP(ID,'Variance',VPTR,STATUS)
        ENDIF

*  quality
        IF (I_QOK) THEN
          CALL BDI_MAP( ID, 'Quality', 'UBYTE', 'WRITE', QPTR, STATUS )
          CALL ARR_SLCOPB( 2, DIMS, %VAL(I_QPTR), LBND, UBND,
     :                     %VAL(QPTR), STATUS )
          CALL BDI_UNMAP(ID,'Quality',QPTR,STATUS)
          CALL BDI_PUT0B( ID, 'QualityMask', I_MASK, STATUS )
        ENDIF

*  axis values
        SPARR(1) = I_XBASE + REAL(I_IX1-1)*I_XSCALE
        SPARR(2) = I_XSCALE
        CALL BDI_AXPUT1R( ID, 1, 'SpacedData', 2, SPARR, STATUS )
        SPARR(1) = I_YBASE + REAL(I_IY1-1)*I_YSCALE
        SPARR(2) = I_YSCALE
        CALL BDI_AXPUT1R( ID, 2, 'SpacedData', 2, SPARR, STATUS )

        CALL BDI_AXPUT0R( ID, 1, 'ScalarWidth', I_XWID, STATUS )
        CALL BDI_AXPUT0R( ID, 2, 'ScalarWidth', I_YWID, STATUS )

        CALL BDI_AXPUT0L( ID, 1, 'Normalised', I_NORM, STATUS )
        CALL BDI_AXPUT0L( ID, 2, 'Normalised', I_NORM, STATUS )
        CALL BDI_AXPUT0C( ID, 1, 'Label', I_XLABEL, STATUS )
        CALL BDI_AXPUT0C( ID, 2, 'Label', I_YLABEL, STATUS )
        CALL BDI_AXPUT0C( ID, 1, 'Units', I_XYUNITS, STATUS )
        CALL BDI_AXPUT0C( ID, 2, 'Units', I_XYUNITS, STATUS )

*  Copy ancilliary stuff from input
        CALL UDI_COPANC( I_FID, ' ', ID, STATUS )

*  GRAFIX control
        IF ( I_DISP_1D ) THEN
          CALL GCB_CSAVE( I_CACHE, ID, STATUS )
        ELSE
          CALL GCB_FSAVE( ID, STATUS )
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('IMG_SAVE',STATUS)
        ENDIF

      ENDIF

      END


*+ IMG_SAVE1D - save current 1D data to file
	SUBROUTINE IMG_SAVE1D(FID,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*  Import :
        INTEGER FID
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
      REAL	SPARR(2)
      INTEGER ID
      INTEGER NVAL,BID
      LOGICAL AUX
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  if auxilliary data then create multiple dataset
        AUX=(I_N_AUX.NE.0)
        IF (AUX) THEN
          CALL GMI_CREMULT(FID,2,STATUS)
          CALL BDI_NEW( 'BinDS', 1, I_N_AUX, 'REAL', BID, STATUS )
          CALL GMI_LOCNDF(FID,1,'*',ID,STATUS)
          CALL ADI_SETLNK( BID, ID, STATUS )
          ID = BID
        ELSE
          CALL ADI_CLONE(FID,ID,STATUS)
        ENDIF

*  text
        CALL BDI_PUT0C( ID, 'Title', I_TITLE_1D, STATUS )
        CALL BDI_PUT0C( ID, 'Label', I_LABEL_1D, STATUS )
        CALL BDI_PUT0C( ID, 'Units', I_UNITS_1D, STATUS )

        NVAL=I_N_1D

*  copy data
        CALL BDI_PUT1R( ID, 'Data', NVAL, %VAL(I_DPTR_1D), STATUS)

*  variance
        CALL BDI_PUT1R( ID, 'Variance', NVAL, %VAL(I_VPTR_1D), STATUS)

*  quality
        CALL BDI_PUT( ID, 'Quality', 'UBYTE', 1, NVAL,
     :                         %VAL(I_QPTR_1D), STATUS)
        CALL BDI_PUT0B( ID, 'QualityMask', I_MASK, STATUS )

*  axis values
        SPARR(1) = I_XBASE_1D
        SPARR(2) = I_XSCALE_1D
        CALL BDI_AXPUT1R( ID, 1, 'SpacedData', 2, SPARR, STATUS )
        CALL BDI_AXPUT0C(ID,1,'Normalised', .TRUE.,STATUS)
        CALL BDI_AXPUT0C(ID,1,'Label', I_XLABEL_1D,STATUS)
        CALL BDI_AXPUT0C(ID,1,'Units', I_XUNITS_1D,STATUS)

*  copy ancilliary stuff from input
        CALL UDI_COPANC( I_FID, ' ', ID, STATUS )

*  GRAFIX control
        IF (I_DISP) THEN
          CALL GCB_CSAVE(I_CACHE_1D,ID,STATUS)
        ELSE
          CALL GCB_FSAVE( ID, STATUS )
        ENDIF

*    Release primary output object
        CALL ADI_ERASE( ID, STATUS )

*  copy ancilliary data if present
        IF (AUX) THEN

          CALL BDI_NEW( 'BinDS', 1, NVAL, 'REAL', BID, STATUS )
          CALL GMI_LOCNDF(FID,2,'*',ID,STATUS)
          CALL ADI_SETLNK( BID, ID, STATUS )
          ID = BID

          CALL BDI_AXPUT1R( ID, 1, 'SpacedData', 2, SPARR, STATUS )
          CALL BDI_PUT1R( ID, 'Data', NVAL, %VAL(I_DPTR_AUX) ,STATUS)
          CALL GMI_SETPLOT( FID,1,1,'2',STATUS)

*      Release secondary output object
          CALL ADI_ERASE( ID, STATUS )

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('IMG_SAVE1D',STATUS)
        ENDIF

      ENDIF

      END


*+ IMG_CHECK - check image
	SUBROUTINE IMG_CHECK(IFID, STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'ADI_PAR'
*  Import :
      INTEGER			IFID
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
      CHARACTER*80 XUNITS,YUNITS
      INTEGER NDIM,DIMS(ADI__MXDIM)
      INTEGER XPTR,YPTR
      LOGICAL DOK,AXOK,AYOK,AXREG,AYREG
      LOGICAL XNORM,YNORM
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  check validity of data array
        CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
        CALL BDI_CHK( IFID, 'Data', DOK, STATUS )
        IF (STATUS.EQ.SAI__OK) THEN
          IF (.NOT.DOK) THEN
            CALL MSG_PRNT('AST_ERR: invalid data array')
            STATUS=SAI__ERROR
          ELSEIF (NDIM.EQ.1) THEN
            CALL MSG_PRNT('AST_ERR: this is a one dimensional dataset')
            STATUS=SAI__ERROR
          ELSEIF (NDIM.EQ.2) THEN
            I_NX=DIMS(1)
            I_NY=DIMS(2)
            I_XAX=1
            I_YAX=2
            I_CUBE=.FALSE.
          ELSEIF (NDIM.EQ.3) THEN
            CALL IMG_CHECK_CUBE(IFID,STATUS)
            I_NX=DIMS(I_XAX)
            I_NY=DIMS(I_YAX)
            I_NZ=DIMS(I_ZAX)
            I_CUBE=.TRUE.
          ELSE
            CALL MSG_PRNT('AST_ERR: dataset has invalid dimensions')
            STATUS=SAI__ERROR
          ENDIF
        ENDIF

*  X axis present
        CALL BDI_AXCHK( IFID, I_XAX, 'Data', AXOK, STATUS )
        IF ( AXOK ) THEN
          CALL BDI_AXMAPR( IFID, I_XAX, 'Data', 'READ', XPTR, STATUS )
          CALL ARR_CHKREG(%val(XPTR),I_NX,AXREG,I_XBASE,I_XSCALE,
     :                                                    STATUS)
          IF (.NOT.AXREG) THEN
            CALL MSG_PRNT('AST_ERR: x-axis has non-equal bin sizes')
            STATUS=SAI__ERROR
          END IF

        ELSE
          I_XBASE = 1.0
          I_XSCALE = 1.0
          I_NX = DIMS(I_XAX)
        END IF

*  Y axis present
        CALL BDI_AXCHK( IFID, I_YAX, 'Data', AYOK, STATUS )
        IF ( AYOK ) THEN
          CALL BDI_AXMAPR( IFID, I_YAX, 'Data', 'READ', YPTR, STATUS )
          CALL ARR_CHKREG(%val(YPTR),I_NY,AYREG,I_YBASE,I_YSCALE,
     :                                                    STATUS)
          IF (.NOT.AYREG) THEN
            CALL MSG_PRNT('AST_ERR: y-axis has non-equal bin sizes')
            STATUS=SAI__ERROR
          END IF

        ELSE
          I_YBASE = 1.0
          I_YSCALE = 1.0
          I_NY = DIMS(I_YAX)
        END IF

*  check axis units
        IF (STATUS.EQ.SAI__OK) THEN

          CALL BDI_AXGET0C( IFID, I_XAX, 'Units', XUNITS, STATUS )
          CALL BDI_AXGET0C( IFID, I_YAX, 'Units', YUNITS, STATUS )
          IF (XUNITS.NE.YUNITS) THEN
            CALL MSG_PRNT('** different x & y axis units - '
     :                        //'assuming image non-spatial')
            I_SPATIALIMAGE=.FALSE.
            I_XYUNITS=' '
            CALL CONV_UNIT2R('arcmin',I_WTORAD,STATUS)
          ELSE
            IF (XUNITS.EQ.' ') THEN
              I_XYUNITS='arcmin'
              CALL MSG_PRNT('** no axis units - will assume ''arcmin''')
            ELSE
              I_XYUNITS=XUNITS
            ENDIF
            CALL CONV_UNIT2R(I_XYUNITS,I_WTORAD,STATUS)
            I_SPATIALIMAGE=.TRUE.
          ENDIF

          CALL BDI_AXGET0C( IFID, I_XAX, 'Label', I_XLABEL, STATUS )
          CALL BDI_AXGET0C( IFID, I_YAX, 'Label', I_YLABEL, STATUS )

        ENDIF

*  check axis normalisations
        IF (STATUS.EQ.SAI__OK) THEN

          CALL BDI_AXGET0L( IFID, I_XAX, 'Normalised', XNORM, STATUS )
          CALL BDI_AXGET0L( IFID, I_YAX, 'Normalised', YNORM, STATUS )
          IF (.NOT.((XNORM.AND.YNORM).OR..NOT.(XNORM.OR.YNORM))) THEN
            CALL MSG_PRNT('AST_ERR: axis normalisations are different')
            STATUS=SAI__ERROR
          ELSE
            I_NORM=XNORM
          ENDIF

        ENDIF

*  check attitude information present
        IF (STATUS.EQ.SAI__OK) THEN

          IF (.NOT.I_SPATIALIMAGE) THEN
            I_PIXID = ADI__NULLID
            I_PRJID = ADI__NULLID
            I_SYSID = ADI__NULLID
            I_RA=0.0D0
            I_DEC=0.0D0
            I_ROLL=0.0D0
          ELSE

            CALL WCI_GETIDS( IFID, I_PIXID, I_PRJID, I_SYSID, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
              I_PIXID = ADI__NULLID
              I_PRJID = ADI__NULLID
              I_SYSID = ADI__NULLID
              CALL ERR_ANNUL( STATUS )
              CALL MSG_PRNT('** insufficient attitude information')
              CALL MSG_PRNT('** using 0.0 for RA, DEC and ROLL')

              I_RA=0.0D0
              I_DEC=0.0D0
              I_ROLL=0.0D0
            ENDIF

          ENDIF

*  set up transformations
          CALL IMG_TMAT(STATUS)
        ENDIF

      ENDIF

      END


      SUBROUTINE IMG_CHECK_CUBE(IFID,STATUS)

      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Import :
      INTEGER			IFID
*  Export :
*  Status :
      INTEGER STATUS
*  Local constants :
*  Local variables :
      CHARACTER*80 LBL
      INTEGER IX,IY,IZ
      INTEGER I
*  Functions :
      LOGICAL STR_ABBREV,STR_SUB
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get axis labels and try and identify
        IX=0
        IY=0
        IZ=0
        DO I=1,3
          CALL BDI_AXGET0C(IFID,I,'Label',LBL,STATUS)
          IF (STR_ABBREV('X',LBL).OR.STR_SUB('X_',LBL)) THEN
            IX=I
          ELSEIF (STR_ABBREV('Y',LBL).OR.STR_SUB('Y_',LBL)) THEN
            IY=I
          ENDIF
        ENDDO

*  check for acceptable axis ordering
        IF (IX.EQ.1.AND.IY.EQ.2) THEN
          I_XAX=1
          I_YAX=2
          I_ZAX=3
        ELSEIF (IX.EQ.2.AND.IY.EQ.3) THEN
          I_XAX=2
          I_YAX=3
          I_ZAX=1
        ELSE
          IF(IX.EQ.0.OR.IY.EQ.0) THEN
            CALL MSG_PRNT('AST_ERR: cannot identify spacial axes')
          ELSE
            CALL MSG_PRNT('AST_ERR: axes are in weird order')
            CALL MSG_PRNT('         must be: x,y,*  or *,x,y')
          ENDIF
          STATUS=SAI__ERROR
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('IMG_CHECK_CUBE',STATUS)
        ENDIF

      ENDIF

      END



*+ IMG_LOAD - load image into system
	SUBROUTINE IMG_LOAD(IFID,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'ADI_PAR'
*  Import :
        INTEGER			IFID
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
      INTEGER NDIM,DIMS(ADI__MXDIM),NVAL,NWID
      INTEGER DPTR,IDUM
      INTEGER VPTR,QPTR
c      LOGICAL VOK,QOK
      LOGICAL UNIF,WOK
*-


      IF (STATUS.EQ.SAI__OK) THEN


*  store identifier
        I_FID = IFID

        NVAL=I_NX*I_NY
        NDIM=2
        DIMS(1)=I_NX
        DIMS(2)=I_NY

*  map data and get axis values
	print *,'map data'
        call flush(6)
        CALL BDI_MAPR( IFID, 'Data', 'READ', DPTR, STATUS )
        CALL DYN_MAPR(1,NVAL,I_DPTR,STATUS)
        CALL ARR_COP1R(NVAL,%VAL(DPTR),%VAL(I_DPTR),STATUS)
        CALL BDI_UNMAP(IFID,'Data',DPTR,STATUS)
        CALL DYN_MAPR(1,NVAL,I_DPTR_W,STATUS)

        CALL DYN_MAPR(1,I_NX,I_XPTR,STATUS)
        CALL DYN_MAPR(1,I_NX,I_XPTR_W,STATUS)
	print *,'create x-axis'
        call flush(6)
        CALL ARR_REG1R(I_XBASE,I_XSCALE,I_NX,%VAL(I_XPTR),STATUS)
        CALL DYN_MAPR(1,I_NY,I_YPTR,STATUS)
        CALL DYN_MAPR(1,I_NY,I_YPTR_W,STATUS)
	print *,'create y-axis'
        call flush(6)
        CALL ARR_REG1R(I_YBASE,I_YSCALE,I_NY,%VAL(I_YPTR),STATUS)
C        CALL BDI_AXCHK( IFID, 1, 'Width', WOK, STATUS )
C        IF (WOK) THEN
C          CALL BDI_AXMAPR( IFID, 1, 'Width', 'READ', WPTR, STATUS )
C          CALL ARR_CHKREG( DIMS(1), %VAL(WPTR), UNIF,
C          CALL BDA_GETAXWID(ILOC,1,I_XWID,STATUS)
C        ELSE
          I_XWID=ABS(I_XSCALE)
C        ENDIF
C        CALL BDA_CHKAXWID(ILOC,2,WOK,UNIF,NWID,STATUS)
C        IF (WOK.AND.UNIF) THEN
C          CALL BDA_GETAXWID(ILOC,2,I_YWID,STATUS)
C        ELSE
          I_YWID=ABS(I_YSCALE)
C        ENDIF

*  get variance and quality if there
        CALL BDI_CHK( IFID, 'Variance', I_VOK, STATUS )
        IF (I_VOK) THEN
	print *,'map variance'
        call flush(6)
          CALL BDI_MAPR(IFID,'Variance','READ',VPTR,STATUS)
          CALL DYN_MAPR(1,NVAL,I_VPTR,STATUS)
          CALL ARR_COP1R(NVAL,%VAL(VPTR),%VAL(I_VPTR),STATUS)
          CALL BDI_UNMAP(IFID,'Variance',VPTR,STATUS)
          CALL DYN_MAPR(1,NVAL,I_VPTR_W,STATUS)
        ENDIF

        CALL BDI_CHK( IFID, 'Quality', I_QOK, STATUS )
        IF (I_QOK) THEN
	print *,'map quality'
        call flush(6)
          CALL BDI_MAP(IFID,'Quality','UBYTE','READ',QPTR,STATUS)
          CALL DYN_MAPB(1,NVAL,I_QPTR,STATUS)
          CALL ARR_COP1B(NVAL,%VAL(QPTR),%VAL(I_QPTR),STATUS)
          CALL BDI_GET(IFID,'QualityMask','UBYTE',0,0,I_MASK,IDUM,
     :                  STATUS)
          CALL IMG_BAD(%VAL(I_QPTR),STATUS)
          CALL BDI_UNMAP(IFID,'Quality',QPTR,STATUS)
          CALL DYN_MAPB(1,NVAL,I_QPTR_W,STATUS)
        ELSE
          I_BAD=.FALSE.
        ENDIF

*  get min and max
	print *,'get min/max'
        call flush(6)
        CALL IMG_MINMAX(STATUS)

*  get top level text
	print *,'get titles'
        call flush(6)
        CALL BDI_GET0C( IFID, 'Title', I_TITLE, STATUS )
        CALL BDI_GET0C( IFID, 'Label', I_LABEL, STATUS )
        CALL BDI_GET0C( IFID, 'Units', I_UNITS, STATUS )

*  get work area
        CALL DYN_MAPI(1,NVAL,I_WKPTR,STATUS)

*  get region mask
        CALL DYN_MAPB(1,NVAL,I_REG_PTR,STATUS)

      ENDIF

      END

*+ IMG_LOADCUBE - load image from cube
	SUBROUTINE IMG_LOADCUBE(IFID,PAR,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'ADI_PAR'
*  Import :
        INTEGER		IFID
        CHARACTER*(*) PAR
*  Export :
*  Status :
        INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
*  Local variables :
      CHARACTER*20 C1,C2
      REAL ZMIN,ZMAX,RANGE(2)
      INTEGER NR
      INTEGER NDIM,DIMS(ADI__MXDIM),NVAL,NWID
      INTEGER DPTR,VPTR,QPTR,ZPTR
      INTEGER NC1,NC2,IDUM
c      LOGICAL VOK,QOK
      LOGICAL UNIF,WOK
*-


      IF (STATUS.EQ.SAI__OK) THEN


*  store identifier
        I_FID = IFID

        NVAL=I_NX*I_NY

*  map z-axis
        CALL BDI_AXMAPR( IFID, I_ZAX, 'Data', 'READ', ZPTR, STATUS )
        CALL ARR_RANG1R(I_NZ,%VAL(ZPTR),ZMIN,ZMAX,STATUS)
        CALL CHR_RTOC(ZMIN,C1,NC1)
        CALL CHR_RTOC(ZMAX,C2,NC2)
        CALL USI_DEF0C(PAR,C1(:NC1)//':'//C2(:NC2),STATUS)

*  get range of axis values for slice
        CALL PRS_GETRANGES(PAR,2,1,ZMIN,ZMAX,RANGE,NR,STATUS)

*  convert to range (inclusive) of indices
        CALL IMG_ZRANGE(RANGE(1),RANGE(2),%VAL(ZPTR),STATUS)

*  map data and get axis values
        CALL BDI_MAPR(IFID,'Data','READ',DPTR,STATUS)
        CALL DYN_MAPR(1,NVAL,I_DPTR,STATUS)
        CALL DYN_MAPR(1,NVAL,I_DPTR_W,STATUS)

*  get variance and quality if there
        CALL BDI_CHK( IFID, 'Variance', I_VOK, STATUS )
        IF (I_VOK) THEN
          CALL BDI_MAPR(IFID,'Variance','READ',VPTR,STATUS)
          CALL DYN_MAPR(1,NVAL,I_VPTR,STATUS)
          CALL DYN_MAPR(1,NVAL,I_VPTR_W,STATUS)
        ENDIF

        CALL BDI_CHK( IFID, 'Quality', I_QOK, STATUS )
        IF (I_QOK) THEN
          CALL BDI_MAP(IFID,'Quality','UBYTE','READ',QPTR,STATUS)
          CALL DYN_MAPB(1,NVAL,I_QPTR,STATUS)
          CALL DYN_MAPB(1,NVAL,I_QPTR_W,STATUS)
          CALL BDI_GET(IFID,'QualityMask','UBYTE',0,0,I_MASK,IDUM,
     :          STATUS)
        ENDIF

        CALL IMG_BINCUBE(%VAL(DPTR),%VAL(VPTR),%VAL(QPTR),STATUS)

        CALL BDI_UNMAP( IFID, 'Data', DPTR, STATUS )
        IF (I_QOK) THEN
          CALL IMG_BAD(%VAL(I_QPTR),STATUS)
          CALL BDI_UNMAP( IFID, 'Quality', QPTR, STATUS )
        ENDIF
        IF (I_VOK) THEN
          CALL BDI_UNMAP( IFID, 'Variance', VPTR, STATUS )
        ENDIF

        CALL DYN_MAPR(1,I_NX,I_XPTR,STATUS)
        CALL DYN_MAPR(1,I_NX,I_XPTR_W,STATUS)
        CALL ARR_REG1R(I_XBASE,I_XSCALE,I_NX,%VAL(I_XPTR),STATUS)
        CALL DYN_MAPR(1,I_NY,I_YPTR,STATUS)
        CALL DYN_MAPR(1,I_NY,I_YPTR_W,STATUS)
        CALL ARR_REG1R(I_YBASE,I_YSCALE,I_NY,%VAL(I_YPTR),STATUS)

c        CALL BDA_CHKAXWID(ILOC,I_XAX,WOK,UNIF,NWID,STATUS)
c        IF (WOK.AND.UNIF) THEN
c          CALL BDA_GETAXWID(ILOC,I_XAX,I_XWID,STATUS)
c        ELSE
          I_XWID=ABS(I_XSCALE)
c        ENDIF
c        CALL BDA_CHKAXWID(ILOC,I_YAX,WOK,UNIF,NWID,STATUS)
c        IF (WOK.AND.UNIF) THEN
c          CALL BDA_GETAXWID(ILOC,I_YAX,I_YWID,STATUS)
c        ELSE
          I_YWID=ABS(I_YSCALE)
c        ENDIF

*  get min and max
        CALL IMG_MINMAX(STATUS)

*  set data slice
        I_IX1=1
        I_IX2=I_NX
        I_IY1=1
        I_IY2=I_NY

*  set current position to centre of image
        I_X=I_XBASE+(REAL(I_NX)/2.0-1.0)*I_XSCALE
        I_Y=I_YBASE+(REAL(I_NY)/2.0-1.0)*I_YSCALE
        I_DX=ABS(I_X-I_XBASE)
        I_DY=ABS(I_Y-I_YBASE)
        I_R=0.0

*  get top level text
        CALL BDI_GET0C( IFID, 'Title', I_TITLE, STATUS )
        CALL BDI_GET0C( IFID, 'Label', I_LABEL, STATUS )
        CALL BDI_GET0C( IFID, 'Units', I_UNITS, STATUS )

*  get work area
        CALL DYN_MAPI(1,NVAL,I_WKPTR,STATUS)

*  get region mask
        CALL DYN_MAPB(1,NVAL,I_REG_PTR,STATUS)

      ENDIF

      END



*+ IMG_MATCH - match image to one loaded
	SUBROUTINE IMG_MATCH(IFID,MATCH,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'ADI_PAR'
        INCLUDE 'PRM_PAR'
*  Import :
      INTEGER           IFID
*  Export :
      LOGICAL MATCH
*  Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Local constants :
      REAL SLOP
      PARAMETER (SLOP=3.0*VAL__SMLR)
*  Local variables :
      REAL BASE,SCALE
      INTEGER NDIM,DIMS(ADI__MXDIM)
      INTEGER PIXID, PRJID, SYSID, AXPTR
      LOGICAL DOK,AXOK,AYOK,AXREG,AYREG
      LOGICAL HOK,SAME
*-
      IF (STATUS.EQ.SAI__OK) THEN

        MATCH=.TRUE.

*  check validity of data array
        CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
        CALL BDI_CHK( IFID, 'Data', DOK, STATUS )
        IF (STATUS.EQ.SAI__OK) THEN
          IF (.NOT.DOK) THEN
            CALL MSG_PRNT('AST_ERR: invalid data array')
            MATCH=.FALSE.
          ELSEIF (NDIM.NE.2) THEN
            CALL MSG_PRNT('AST_ERR: this is not an image dataset')
            MATCH=.FALSE.
          ELSEIF (DOK.AND.NDIM.EQ.2) THEN
            IF (DIMS(1).NE.I_NX.OR.DIMS(2).NE.I_NY) THEN
              CALL MSG_PRNT(
     :             'AST_ERR: dimensions don''t match loaded image')
              MATCH=.FALSE.
            ENDIF
          ENDIF
        ENDIF

*  check axes are regular
        IF (MATCH) THEN
          CALL BDI_AXMAPR( IFID, 1, 'Data', 'READ', AXPTR, STATUS )
          CALL ARR_CHKREG( %VAL(AXPTR), DIMS(1), AXREG, BASE, SCALE,
     :                     STATUS )
          CALL BDI_AXUNMAP( IFID, 1, 'Data', AXPTR, STATUS )
          IF (.NOT.AXREG.OR.
     :        .NOT.(ABS(BASE-I_XBASE).LE.SLOP.AND.
     :              ABS(SCALE-I_XSCALE).LE.SLOP)) THEN
            CALL MSG_PRNT(
     :             'AST_ERR: x-axis values do not match loaded image')
            MATCH=.FALSE.
          ENDIF

          CALL BDI_AXMAPR( IFID, 2, 'Data', 'READ', AXPTR, STATUS )
          CALL ARR_CHKREG( %VAL(AXPTR), DIMS(2), AYREG, BASE, SCALE,
     :                     STATUS )
          CALL BDI_AXUNMAP( IFID, 2, 'Data', AXPTR, STATUS )
          IF (.NOT.AYREG.OR.
     :        .NOT.(ABS(BASE-I_YBASE).LE.SLOP.AND.
     :              ABS(SCALE-I_YSCALE).LE.SLOP)) THEN
            CALL MSG_PRNT(
     :             'AST_ERR: y-axis values do not match loaded image')
            MATCH=.FALSE.
          ENDIF
        ENDIF


*  match attitude information (for spatial images)
        IF (MATCH.AND.I_SPATIALIMAGE) THEN

          CALL WCI_GETIDS( IFID, PIXID, PRJID, SYSID, STATUS )
          CALL WCI_SAME( PIXID, PRJID, SYSID,
     :                   I_PIXID, I_PRJID, I_SYSID, SAME, STATUS )

          IF ( .NOT. SAME ) THEN
            CALL MSG_PRNT(
     :              'AST_ERR: pointing doesn''t match loaded image')
            MATCH=.FALSE.
          ENDIF

        ENDIF

      ENDIF

      END



*+
      SUBROUTINE IMG_ZRANGE(LO,HI,Z,STATUS)

      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Import :
      REAL LO,HI
      REAL Z(*)
*  Export :
*  Status :
      INTEGER STATUS
*  Local constants :
*  Local variables :
      INTEGER I
*-
      IF (STATUS.EQ.SAI__OK) THEN

        I=1
        DO WHILE (LO.GT.Z(I).AND.I.LT.I_NZ)
          I=I+1
        ENDDO
        I_IZ1=I
        I=I_NZ
        DO WHILE (HI.LT.Z(I).AND.I.GT.1)
          I=I-1
        ENDDO
        I_IZ2=I

      ENDIF

      END


*+ IMG_BINCUBE
	SUBROUTINE IMG_BINCUBE(D,V,Q,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL D(*)
        REAL V(*)
        BYTE Q(*)
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

      IF (I_ZAX.EQ.3) THEN
        CALL IMG_BINCUBE_XYZ(D,V,Q,%VAL(I_DPTR),%VAL(I_VPTR),
     :                                     %VAL(I_QPTR),STATUS)
      ELSEIF (I_ZAX.EQ.1) THEN
        CALL IMG_BINCUBE_ZXY(D,V,Q,%VAL(I_DPTR),%VAL(I_VPTR),
     :                                     %VAL(I_QPTR),STATUS)
      ENDIF

      ENDIF

      END


*+ IMG_BINCUBE_XYZ
	SUBROUTINE IMG_BINCUBE_XYZ(D,V,Q,DD,VV,QQ,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'QUAL_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL D(I_NX,I_NY,I_NZ)
        REAL V(I_NX,I_NY,I_NZ)
        BYTE Q(I_NX,I_NY,I_NZ)
*  Export :
        REAL DD(I_NX,I_NY)
        REAL VV(I_NX,I_NY)
        BYTE QQ(I_NX,I_NY)
*  Status :
        INTEGER STATUS
*  Function declarations :
      BYTE BIT_ANDUB,BIT_ORUB
*  Local constants :
*  Local variables :
      INTEGER IX,IY,IZ
      INTEGER N
      LOGICAL GOOD
      BYTE QQQ
*-
      IF (STATUS.EQ.SAI__OK) THEN

        DO IY=1,I_NY
          DO IX=1,I_NX

            DD(IX,IY)=0.0
            QQQ=QUAL__GOOD
            IF (I_VOK) THEN
              VV(IX,IY)=0.0
            ENDIF

            N=0
            DO IZ=I_IZ1,I_IZ2

              IF (I_QOK) THEN
                GOOD=(BIT_ANDUB(Q(IX,IY,IZ),I_MASK).EQ.QUAL__GOOD)
              ELSE
                GOOD=.TRUE.
              ENDIF

              IF (GOOD) THEN
                N=N+1
                DD(IX,IY)=DD(IX,IY)+D(IX,IY,IZ)
                IF (I_VOK) THEN
                  VV(IX,IY)=VV(IX,IY)+V(IX,IY,IZ)
                ENDIF
              ENDIF

              IF (I_QOK) THEN
                QQQ=BIT_ORUB(QQQ,Q(IX,IY,IZ))
              ENDIF

            ENDDO

            IF (I_QOK) THEN
              IF (N.EQ.0) THEN
                QQ(IX,IY)=QQQ
              ELSE
                QQ(IX,IY)=QUAL__GOOD
              ENDIF
            ENDIF

          ENDDO
        ENDDO

      ENDIF

      END


*+ IMG_BINCUBE_ZXY
	SUBROUTINE IMG_BINCUBE_ZXY(D,V,Q,DD,VV,QQ,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'QUAL_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL D(I_NZ,I_NX,I_NY)
        REAL V(I_NZ,I_NX,I_NY)
        BYTE Q(I_NZ,I_NX,I_NY)
*  Export :
        REAL DD(I_NX,I_NY)
        REAL VV(I_NX,I_NY)
        BYTE QQ(I_NX,I_NY)
*  Status :
        INTEGER STATUS
*  Function declarations :
      BYTE BIT_ANDUB,BIT_ORUB
*  Local constants :
*  Local variables :
      INTEGER IX,IY,IZ
      INTEGER N
      LOGICAL GOOD
      BYTE QQQ
*-
      IF (STATUS.EQ.SAI__OK) THEN

        DO IY=1,I_NY
          DO IX=1,I_NX

            DD(IX,IY)=0.0
            QQQ=QUAL__GOOD
            IF (I_VOK) THEN
              VV(IX,IY)=0.0
            ENDIF

            N=0
            DO IZ=I_IZ1,I_IZ2

              IF (I_QOK) THEN
                GOOD=(BIT_ANDUB(Q(IZ,IX,IY),I_MASK).EQ.QUAL__GOOD)
              ELSE
                GOOD=.TRUE.
              ENDIF

              IF (GOOD) THEN
                N=N+1
                DD(IX,IY)=DD(IX,IY)+D(IZ,IX,IY)
                IF (I_VOK) THEN
                  VV(IX,IY)=VV(IX,IY)+V(IZ,IX,IY)
                ENDIF
              ENDIF

              IF (I_QOK) THEN
                QQQ=BIT_ORUB(QQQ,Q(IZ,IX,IY))
              ENDIF

            ENDDO

            IF (I_QOK) THEN
              IF (N.EQ.0) THEN
                QQ(IX,IY)=QQQ
              ELSE
                QQ(IX,IY)=QUAL__GOOD
              ENDIF
            ENDIF

          ENDDO
        ENDDO

      ENDIF

      END




*+ IMG_BAD - checks for bad quality
      SUBROUTINE IMG_BAD(QUAL,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Import :
        BYTE QUAL(I_NX,I_NY)
*  Export :
*  Status :
        INTEGER STATUS
*  Function declarations :
      BYTE BIT_ANDUB
*  Local constants :
*  Local variables :
      INTEGER I,J
*-
      IF (STATUS.EQ.SAI__OK) THEN

        I_BAD=.FALSE.
        DO J=1,I_NY
          DO I=1,I_NX
            IF (BIT_ANDUB(QUAL(I,J),I_MASK).NE.QUAL__GOOD) THEN
              I_BAD=.TRUE.
            ENDIF
          ENDDO
        ENDDO


      ENDIF

      END



*+ IMG_GET1D - get dynamic arrays for 1D data
	SUBROUTINE IMG_GET1D(NVAL,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        INTEGER NVAL
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
        INTEGER N
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  check size of any existing array
        CALL DYN_SIZE(I_DPTR_1D,N,STATUS)
*  if not big enough then get new ones
        IF (N.LT.NVAL) THEN
          CALL DYN_UNMAP(I_DPTR_1D,STATUS)
          CALL DYN_UNMAP(I_VPTR_1D,STATUS)
          CALL DYN_UNMAP(I_QPTR_1D,STATUS)
          CALL DYN_UNMAP(I_APTR_1D,STATUS)
          CALL DYN_UNMAP(I_WPTR_1D,STATUS)
          CALL DYN_MAPR(1,NVAL,I_DPTR_1D,STATUS)
          CALL DYN_MAPR(1,NVAL,I_VPTR_1D,STATUS)
          CALL DYN_MAPB(1,NVAL,I_QPTR_1D,STATUS)
          CALL DYN_MAPR(1,NVAL,I_APTR_1D,STATUS)
          CALL DYN_MAPR(1,NVAL,I_WPTR_1D,STATUS)
        ENDIF


      ENDIF

      END

*+ IMG_GETAUX - get dynamic arrays for auxialiary data
	SUBROUTINE IMG_GETAUX(NVAL,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        INTEGER NVAL
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
        INTEGER N
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  check size of any existing array
        CALL DYN_SIZE(I_DPTR_AUX,N,STATUS)
*  if not big enough then get new ones
        IF (N.LT.NVAL) THEN
          CALL DYN_UNMAP(I_DPTR_AUX,STATUS)
          CALL DYN_MAPR(1,NVAL,I_DPTR_AUX,STATUS)
        ENDIF


      ENDIF

      END



*+ IMG_CIRCLE
	SUBROUTINE IMG_CIRCLE(XC,YC,RAD,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL XC,YC,RAD
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
	REAL PI, DTOR
	PARAMETER (PI = 3.141592, DTOR = PI/180.0)
*  Local variables :
      REAL X1,X2,X3,X4,X
      REAL Y1,Y2,Y3,Y4,Y
      REAL A
      INTEGER IA
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL PGUPDT(0)

        X1=XC+RAD
        Y1=YC
        X2=X1
        Y2=Y1
        X3=XC-RAD
        Y3=YC
        X4=X3
        Y4=Y3

        DO IA=5,90,5

          A=REAL(IA)*DTOR
          X=RAD*COS(A)
          Y=RAD*SIN(A)

          CALL PGMOVE(X1,Y1)
          X1=XC+X
          Y1=YC+Y
          CALL PGDRAW(X1,Y1)
          CALL PGMOVE(X2,Y2)
          X2=XC+X
          Y2=YC-Y
          CALL PGDRAW(X2,Y2)
          CALL PGMOVE(X3,Y3)
          X3=XC-X
          Y3=YC+Y
          CALL PGDRAW(X3,Y3)
          CALL PGMOVE(X4,Y4)
          X4=XC-X
          Y4=YC-Y
          CALL PGDRAW(X4,Y4)

        ENDDO

        CALL PGUPDT(2)
        CALL PGUPDT(1)

      ENDIF

      END



*+ IMG_ELLIPSE
	SUBROUTINE IMG_ELLIPSE(XC,YC,A,B,ANGLE,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL XC,YC,A,B,ANGLE
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
	REAL PI, DTOR
	PARAMETER (PI = 3.141592, DTOR = PI/180.0)
*  Local variables :
      REAL ANG
      REAL SA,CA
      REAL SANG,CANG
      INTEGER IA
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL PGUPDT(0)

        SANG=SIN(ANGLE)
        CANG=COS(ANGLE)
        CALL PGMOVE(A*CANG,A*SANG)

        DO IA=1,360


          ANG=REAL(IA)*DTOR
          CA=COS(ANG)
          SA=SIN(ANG)

          CALL PGMOVE(XC+A*CA*CANG-B*SA*SANG,YC+A*CA*SANG+B*SA*CANG)

        ENDDO

        CALL PGUPDT(2)
        CALL PGUPDT(1)

      ENDIF

      END



*+ IMG_INIT - initialise common block
	SUBROUTINE IMG_INIT()

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
*  Export :
*  Status :
c        INTEGER STATUS
*  Local constants :
*  Local variables :
*-
      I_NX=0
      I_NY=0
      I_N_1D=0
      I_N_AUX=0

      END



*+ IMG_SETPOS
	SUBROUTINE IMG_SETPOS(X,Y,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'PRM_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL X,Y
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
      CHARACTER*12 RAS,DECS
      DOUBLE PRECISION RA,DEC
      INTEGER ID
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL IMG_WORLDTOPIX(X,Y,I_XPIX,I_YPIX,STATUS)
        I_X=X
        I_Y=Y

*  being run from GUI so put on noticeboard
        IF (I_GUI) THEN
          CALL NBS_FIND_ITEM(I_NBID,'X',ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,X,STATUS)
          CALL NBS_FIND_ITEM(I_NBID,'Y',ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,Y,STATUS)
	print *,x,y
	call flush(6)
          CALL IMG_WORLDTOCEL(X,Y,RA,DEC,STATUS)
	print *,ra,dec
	call flush(6)
          CALL CONV_DEGHMS(REAL(RA),RAS)
          CALL CONV_DEGDMS(REAL(DEC),DECS)
	print *,ras,decs
	call flush(6)
          CALL NBS_FIND_ITEM(I_NBID,'RA',ID,STATUS)
          CALL NBS_PUT_CVALUE(ID,0,RAS,STATUS)
          CALL NBS_FIND_ITEM(I_NBID,'DEC',ID,STATUS)
          CALL NBS_PUT_CVALUE(ID,0,DECS,STATUS)
        ENDIF

      ENDIF

      END



*+ IMG_SETCIRC
	SUBROUTINE IMG_SETCIRC(X,Y,R,EXCLUDE,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL X,Y,R
        LOGICAL EXCLUDE
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
        INTEGER I1,I2,J1,J2
c        REAL XX,XP,YP
        BYTE FLAG
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get range of pixels containing circle
        CALL IMG_CIRCTOBOX(X,Y,R,I1,I2,J1,J2,STATUS)

        IF (EXCLUDE) THEN
          FLAG='00'X
        ELSE
          FLAG='01'X
        ENDIF

*  set region mask
        CALL IMG_SETCIRC_SUB(X,Y,R,I1,I2,J1,J2,FLAG,%val(I_REG_PTR))
        IF (I_REG_TYPE.EQ.'NONE') THEN
          I_REG_TYPE='CIRCLE'
        ELSE
          I_REG_TYPE='COMPLEX'
        ENDIF

      ENDIF

      END

      SUBROUTINE IMG_SETCIRC_SUB(X,Y,R,I1,I2,J1,J2,FLAG,REG)

      INCLUDE 'SAE_PAR'
      INCLUDE 'IMG_CMN'

      REAL X,Y,R
      INTEGER I1,I2,J1,J2
      BYTE FLAG
      BYTE REG(I_NX,I_NY)

      LOGICAL IMG_INCIRC

      INTEGER I,J

      DO J=J1,J2
        DO I=I1,I2
          IF (IMG_INCIRC(I,J,X,Y,R)) THEN
            REG(I,J)=FLAG
          ENDIF
        ENDDO
      ENDDO

      END



*+ IMG_SETINV
	SUBROUTINE IMG_SETINV(STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN


*  set region mask
        IF (I_REG_TYPE.NE.'NONE') THEN
          CALL IMG_SETINV_SUB(%val(I_REG_PTR))
          I_REG_TYPE='COMPLEX'
        ENDIF

      ENDIF

      END

      SUBROUTINE IMG_SETINV_SUB(REG)

      INCLUDE 'SAE_PAR'
      INCLUDE 'IMG_CMN'

      BYTE REG(I_NX,I_NY)


      INTEGER I,J

      DO J=I_IY1,I_IY2
        DO I=I_IX1,I_IX2
          IF (REG(I,J).EQ.'00'X) THEN
            REG(I,J)='01'X
          ELSEIF (REG(I,J).EQ.'01'X) THEN
            REG(I,J)='00'X
          ENDIF
        ENDDO
      ENDDO

      END



*+ IMG_SETARD
	SUBROUTINE IMG_SETARD(MASK,EXCLUDE,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        INTEGER MASK(I_NX,I_NY)
        LOGICAL EXCLUDE
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
      INTEGER I1,I2,J1,J2
      BYTE FLAG
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (EXCLUDE) THEN
          FLAG='00'X
        ELSE
          FLAG='01'X
        ENDIF

*  set region mask
        I1=1
        I2=I_NX
        J1=1
        J2=I_NY
        CALL IMG_SETARD_SUB(MASK,I1,I2,J1,J2,FLAG,%val(I_REG_PTR))

        I_REG_TYPE='COMPLEX'

      ENDIF

      END

      SUBROUTINE IMG_SETARD_SUB(MASK,I1,I2,J1,J2,FLAG,REG)

      INCLUDE 'SAE_PAR'
      INCLUDE 'IMG_CMN'

      INTEGER MASK(I_NX,I_NY)
      INTEGER I1,I2,J1,J2
      BYTE FLAG
      BYTE REG(I_NX,I_NY)


      INTEGER I,J

      DO J=J1,J2
        DO I=I1,I2
          IF (MASK(I,J).GT.0) THEN
            REG(I,J)=FLAG
          ENDIF
        ENDDO
      ENDDO

      END






*+ IMG_SETANNULUS
	SUBROUTINE IMG_SETANNULUS(X,Y,IRAD,ORAD,EXCLUDE,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL X,Y,IRAD,ORAD
        LOGICAL EXCLUDE
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
c        REAL XX,XP,YP
        INTEGER I1,I2,J1,J2
        BYTE FLAG
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (EXCLUDE) THEN
          FLAG='00'X
        ELSE
          FLAG='01'X
        ENDIF

*  get range of pixels containing outer circle
        CALL IMG_CIRCTOBOX(X,Y,ORAD,I1,I2,J1,J2,STATUS)

*  set region mask
        CALL IMG_SETANNULUS_SUB(X,Y,IRAD,ORAD,I1,I2,J1,J2,
     :                                FLAG,%val(I_REG_PTR))
        IF (I_REG_TYPE.EQ.'NONE') THEN
          I_REG_TYPE='ANNULUS'
        ELSE
          I_REG_TYPE='COMPLEX'
        ENDIF

      ENDIF

      END

      SUBROUTINE IMG_SETANNULUS_SUB(X,Y,IRAD,ORAD,I1,I2,J1,J2,
     :                                               FLAG,REG)

      INCLUDE 'SAE_PAR'
      INCLUDE 'IMG_CMN'

      REAL X,Y,IRAD,ORAD
      INTEGER I1,I2,J1,J2
      BYTE FLAG
      BYTE REG(I_NX,I_NY)

      LOGICAL IMG_INANNULUS

      INTEGER I,J

      DO J=J1,J2
        DO I=I1,I2
          IF (IMG_INANNULUS(I,J,X,Y,IRAD,ORAD)) THEN
            REG(I,J)=FLAG
          ENDIF
        ENDDO
      ENDDO

      END




*+ IMG_SETPOLY
	SUBROUTINE IMG_SETPOLY(NV,XV,YV,EXCLUDE,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        INTEGER NV
        REAL XV(*),YV(*)
        LOGICAL EXCLUDE
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
      BYTE FLAG
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (EXCLUDE) THEN
          FLAG='00'X
        ELSE
          FLAG='01'X
        ENDIF

*  set region mask
        CALL IMG_SETPOLY_SUB(NV,XV,YV,FLAG,%val(I_REG_PTR))
        IF (I_REG_TYPE.EQ.'NONE') THEN
          I_REG_TYPE='POLYGON'
        ELSE
          I_REG_TYPE='COMPLEX'
        ENDIF

      ENDIF

      END

      SUBROUTINE IMG_SETPOLY_SUB(NV,XV,YV,FLAG,REG)

      INCLUDE 'SAE_PAR'
      INCLUDE 'IMG_CMN'

      INTEGER NV
      REAL XV,YV
      BYTE FLAG
      BYTE REG(I_NX,I_NY)

      LOGICAL IMG_INPOLY

      INTEGER I1,I2,J1,J2
      INTEGER I,J

*  get range of pixels containing circle
      CALL IMG_POLYTOBOX(NV,XV,YV,I1,I2,J1,J2,STATUS)

      DO J=I_IY1,I_IY2
        DO I=I_IX1,I_IX2
          IF (IMG_INPOLY(I,J,NV,XV,YV)) THEN
            REG(I,J)=FLAG
          ENDIF
        ENDDO
      ENDDO

      END



*+ IMG_STORECIRC
	SUBROUTINE IMG_STORECIRC(X,Y,R,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL X,Y,R
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
        REAL XP,YP
        REAL XX
*-
      IF (STATUS.EQ.SAI__OK) THEN

        I_X=X
        I_Y=Y
        I_R=R
        I_DX=0.0
        I_DY=0.0

*  set box containing circle
        CALL IMG_CIRCTOBOX(X,Y,R,I_IX1,I_IX2,I_IY1,I_IY2,STATUS)


*  convert world coords to pixel coords
        CALL IMG_WORLDTOPIX(X,Y,I_XPIX,I_YPIX,STATUS)
        XX=X+R
        CALL IMG_WORLDTOPIX(XX,Y,XP,YP,STATUS)
        I_RPIX=ABS(XP-I_XPIX)


      ENDIF

      END



*+ IMG_STOREBOX
	SUBROUTINE IMG_STOREBOX(X,Y,DX,DY,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL X,Y,DX,DY
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        I_X=X
        I_Y=Y
        I_R=0.0
        I_DX=DX
        I_DY=DY

*  set pixel range
        CALL IMG_BOXTOBOX(X,Y,DX,DY,I_IX1,I_IX2,I_IY1,I_IY2,STATUS)


*  convert world coords of centre to pixel coords
        CALL IMG_WORLDTOPIX(X,Y,I_XPIX,I_YPIX,STATUS)


      ENDIF

      END




*+ IMG_CIRCTOBOX - get box containing circle
	SUBROUTINE IMG_CIRCTOBOX(X,Y,R,I1,I2,J1,J2,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL X,Y,R
*  Export :
      INTEGER I1,I2,J1,J2
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
      REAL XPIX,YPIX,RPIX
      REAL XX,XP,YP
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  convert world coords to pixel coords
        CALL IMG_WORLDTOPIX(X,Y,XPIX,YPIX,STATUS)
        XX=X+R
        CALL IMG_WORLDTOPIX(XX,Y,XP,YP,STATUS)
        RPIX=ABS(XP-XPIX)
*  get range of pixels containing circle
        I1=MAX(1,MIN(I_NX,INT(XPIX-RPIX+0.5)))
        I2=MAX(1,MIN(I_NX,INT(XPIX+RPIX-0.5)))
        J1=MAX(1,MIN(I_NY,INT(YPIX-RPIX+0.5)))
        J2=MAX(1,MIN(I_NY,INT(YPIX+RPIX-0.5)))

      ENDIF

      END



*+ IMG_SETBOX
	SUBROUTINE IMG_SETBOX(X,Y,DX,DY,EXCLUDE,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL X,Y,DX,DY
        LOGICAL EXCLUDE
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
      INTEGER I1,I2,J1,J2
      BYTE FLAG
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (EXCLUDE) THEN
          FLAG='00'X
        ELSE
          FLAG='01'X
        ENDIF

*  get range of pixels containing box
        CALL IMG_BOXTOBOX(X,Y,DX,DY,I1,I2,J1,J2,STATUS)

*  set region mask
        CALL IMG_SETBOX_SUB(I1,I2,J1,J2,FLAG,%val(I_REG_PTR))
        IF (I_REG_TYPE.EQ.'NONE') THEN
          I_REG_TYPE='BOX'
        ELSE
          I_REG_TYPE='COMPLEX'
        ENDIF
      ENDIF

      END


      SUBROUTINE IMG_SETBOX_SUB(I1,I2,J1,J2,FLAG,REG)

      INCLUDE 'SAE_PAR'
      INCLUDE 'IMG_CMN'

      INTEGER I1,I2,J1,J2
      BYTE FLAG
      BYTE REG(I_NX,I_NY)


      INTEGER I,J

      DO J=J1,J2
        DO I=I1,I2
          REG(I,J)=FLAG
        ENDDO
      ENDDO

      END



*+ IMG_SETSLICE
	SUBROUTINE IMG_SETSLICE(XC,YC,ANGLE,LENGTH,WIDTH,EXCLUDE,
     :                                                     STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL XC,YC,ANGLE,LENGTH,WIDTH
        LOGICAL EXCLUDE
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
      INTEGER I1,I2,J1,J2
      BYTE FLAG
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (EXCLUDE) THEN
          FLAG='00'X
        ELSE
          FLAG='01'X
        ENDIF

*  get range of pixels containing slice
        CALL IMG_SLICETOBOX(XC,YC,ANGLE,LENGTH,WIDTH,I1,I2,J1,J2,
     :                                                     STATUS)

*  set region mask
        CALL IMG_SETSLICE_SUB(XC,YC,ANGLE,LENGTH,WIDTH,I1,I2,J1,J2,
     :                                         FLAG,%val(I_REG_PTR))
        IF (I_REG_TYPE.EQ.'NONE') THEN
          I_REG_TYPE='SLICE'
        ELSE
          I_REG_TYPE='COMPLEX'
        ENDIF
      ENDIF

      END


      SUBROUTINE IMG_SETSLICE_SUB(XC,YC,ANGLE,LENGTH,WIDTH,
     :                                   I1,I2,J1,J2,FLAG,REG)

      INCLUDE 'SAE_PAR'
      INCLUDE 'IMG_CMN'

      REAL XC,YC,ANGLE,LENGTH,WIDTH
      INTEGER I1,I2,J1,J2
      BYTE FLAG
      BYTE REG(I_NX,I_NY)

      LOGICAL IMG_INSLICE

      INTEGER I,J

      DO J=J1,J2
        DO I=I1,I2
          IF (IMG_INSLICE(I,J,XC,YC,ANGLE,LENGTH,WIDTH)) THEN
            REG(I,J)=FLAG
          ENDIF
        ENDDO
      ENDDO

      END




*+ IMG_SETELLIPSE
	SUBROUTINE IMG_SETELLIPSE(XC,YC,MAJOR,MINOR,EXCLUDE,ANGLE,
     :                                                      STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL XC,YC,MAJOR,MINOR,ANGLE
        LOGICAL EXCLUDE
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
      INTEGER I1,I2,J1,J2
      BYTE FLAG
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (EXCLUDE) THEN
          FLAG='00'X
        ELSE
          FLAG='01'X
        ENDIF

*  get range of pixels containing slice
        CALL IMG_ELLIPSETOBOX(XC,YC,MAJOR,MINOR,ANGLE,I1,I2,J1,J2,
     :                                                     STATUS)

*  set region mask
        CALL IMG_SETELLIPSE_SUB(XC,YC,MAJOR,MINOR,ANGLE,I1,I2,J1,J2,
     :                                           FLAG,%val(I_REG_PTR))
        IF (I_REG_TYPE.EQ.'NONE') THEN
          I_REG_TYPE='ELLIPSE'
        ELSE
          I_REG_TYPE='COMPLEX'
        ENDIF
      ENDIF

      END


      SUBROUTINE IMG_SETELLIPSE_SUB(XC,YC,MAJOR,MINOR,ANGLE,
     :                                   I1,I2,J1,J2,FLAG,REG)

      INCLUDE 'SAE_PAR'
      INCLUDE 'IMG_CMN'

      REAL XC,YC,ANGLE
      INTEGER I1,I2,J1,J2
      BYTE FLAG
      BYTE REG(I_NX,I_NY)

      LOGICAL IMG_INELLIPSE

      INTEGER I,J

      DO J=J1,J2
        DO I=I1,I2
          IF (IMG_INELLIPSE(I,J,XC,YC,MAJOR,MINOR,ANGLE)) THEN
            REG(I,J)=FLAG
          ENDIF
        ENDDO
      ENDDO

      END




*+ IMG_REGTOBOX - get range of pixels inside current region
	SUBROUTINE IMG_REGTOBOX(I1,I2,J1,J2,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
*  Export :
        INTEGER I1,I2,J1,J2
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (I_REG_TYPE.EQ.'NONE') THEN
          I1=1
          I2=I_NX
          J1=1
          J2=I_NY

        ELSE

          CALL IMG_REGTOBOX_SUB(%val(I_REG_PTR),I1,I1,J1,J2)

        ENDIF


      ENDIF

      END



*+
	SUBROUTINE IMG_REGTOBOX_SUB(REG,I1,I2,J1,J2)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        BYTE REG(I_NX,I_NY)
*  Export :
        INTEGER I1,I2,J1,J2
*  Status :
*  Local constants :
*  Local variables :
      INTEGER I,J
*-

      I1=I_NX
      I2=1
      J1=I_NY
      J2=1

      J=1
      DO WHILE (J.LE.I_NY.AND.J1.NE.1.AND.J2.NE.I_NY)
        I=1
        DO WHILE (I.LE.I_NX.AND.I1.NE.1.AND.I2.NE.I_NX)
          IF (REG(I,J).NE.'00'X) THEN
            I1=MIN(I1,I)
            I2=MAX(I2,I)
            J1=MIN(J1,J)
            J2=MAX(J2,J)
          ENDIF
          I=I+1
        ENDDO
        J=J+1
      ENDDO

      END



*+ IMG_BOXTOBOX - convert box spec. to range of pixels
	SUBROUTINE IMG_BOXTOBOX(X,Y,DX,DY,I1,I2,J1,J2,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
        REAL X,Y,DX,DY
*  Export :
        INTEGER I1,I2,J1,J2
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
        REAL XX,YY,XP,YP,DXP,DYP,PX,PY
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  convert world coords to pixel coords
        CALL IMG_WORLDTOPIX(X,Y,PX,PY,STATUS)

        XX=X+DX
        CALL IMG_WORLDTOPIX(XX,Y,XP,YP,STATUS)
        DXP=ABS(XP-PX)
        YY=Y+DY
        CALL IMG_WORLDTOPIX(X,YY,XP,YP,STATUS)
        DYP=ABS(YP-PY)

*  get range of pixels containing box
        I1=MAX(1,MIN(I_NX,INT(PX-DXP+0.5)))
        I2=MAX(1,MIN(I_NX,INT(PX+DXP-0.5)))
        J1=MAX(1,MIN(I_NY,INT(PY-DYP+0.5)))
        J2=MAX(1,MIN(I_NY,INT(PY+DYP-0.5)))

      ENDIF

      END



*+ IMG_SLICETOBOX - get box enclosing slice
	SUBROUTINE IMG_SLICETOBOX(XC,YC,ANGLE,LENGTH,WIDTH,
     :                                    I1,I2,J1,J2,STATUS)
*    Description :
*    Type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*    Import :
        REAL XC,YC,ANGLE,LENGTH,WIDTH
*    Import-Export :
*    Export :
        INTEGER I1,I2,J1,J2
*    Status :
        INTEGER STATUS
*    Function declarations :
*    Local variables :
        REAL PHI
        REAL HDIAG
        REAL DX1,DX2,DY1,DY2
        REAL X,Y
        REAL PX1,PX2,PX3,PX4
        REAL PY1,PY2,PY3,PY4
*-
        IF (STATUS.NE.SAI__OK) RETURN

*  get half diagonal
        HDIAG=SQRT(LENGTH**2 + WIDTH**2)/2.0

*  angle of diagonal from x-axis
        PHI=ATAN2(WIDTH,LENGTH)

*  offset of corners from centre
        DX1=HDIAG*COS(PHI+ANGLE)
        DY1=HDIAG*SIN(PHI+ANGLE)
        DX2=HDIAG*COS(ANGLE-PHI)
        DY2=HDIAG*SIN(ANGLE-PHI)

*  get coords of corners
        X=XC+DX1
        Y=YC+DY1
        CALL IMG_WORLDTOPIX(X,Y,PX1,PY1,STATUS)
        X=XC+DX2
        Y=YC+DY2
        CALL IMG_WORLDTOPIX(X,Y,PX2,PY2,STATUS)


        X=XC-DX1
        Y=YC-DY1
        CALL IMG_WORLDTOPIX(X,Y,PX3,PY3,STATUS)
        X=XC-DX2
        Y=YC-DY2
        CALL IMG_WORLDTOPIX(X,Y,PX4,PY4,STATUS)

        I1=INT(MIN(PX1,MIN(PX2,MIN(PX3,PX4))))
        I2=INT(MAX(PX1,MAX(PX2,MAX(PX3,PX4))))
        J1=INT(MIN(PY1,MIN(PY2,MIN(PY3,PY4))))
        J2=INT(MAX(PY1,MAX(PY2,MAX(PY3,PY4))))


	END





*+ IMG_ELLIPSETOBOX - get box enclosing ellipse
	SUBROUTINE IMG_ELLIPSETOBOX(XC,YC,MAJOR,MINOR,ANGLE,
     :                                    I1,I2,J1,J2,STATUS)
*    Description :
*    Type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*    Import :
        REAL XC,YC,MAJOR,MINOR,ANGLE
*    Import-Export :
*    Export :
        INTEGER I1,I2,J1,J2
*    Status :
        INTEGER STATUS
*    Function declarations :
*    Local variables :
*-
        IF (STATUS.NE.SAI__OK) RETURN

          call img_slicetobox(xc,yc,angle,major,minor,i1,i2,j1,j2,
     :                                                       status)

	END





*+ IMG_POLYTOBOX - get box containing polygon
      SUBROUTINE IMG_POLYTOBOX(NVERTEX,XVERT,YVERT,I1,I2,J1,J2,STATUS)
*
*    Description :
*
*    Method :
*
*
*
*    Deficiencies :
*
*    Bugs :
*    Authors :
*
*    History :
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
      INTEGER NVERTEX
      REAL XVERT(*),YVERT(*)
*    Export :
      INTEGER I1,I2,J1,J2
*    Status :
*
      INTEGER STATUS
*
*    Global Variables :
*
      INCLUDE 'IMG_CMN'
*
*    Functions :
*
*
*    Local Constants :
*
*    Local variables :
*
      REAL                XMIN, XMAX,                ! Bounding rectangle
     :                    YMIN, YMAX                 !   of polygon
      REAL                XP, YP                     ! Position in pixels
      INTEGER             IV                         ! Loop over vertices
*
*-
      IF (STATUS.EQ.SAI__OK) THEN

* Convert vertices to fractional pixels, finding minima & maxima
        XMIN = REAL(I_NX)
        XMAX = 0.0
        YMIN = REAL(I_NY)
        YMAX = 0.0
        DO IV = 1, NVERTEX
          CALL IMG_WORLDTOPIX( XVERT(IV), YVERT(IV), XP, YP, STATUS )
          XMIN=MIN(XP,XMIN)
          XMAX=MAX(XP,XMAX)
          YMIN=MIN(YP,YMIN)
          YMAX=MAX(YP,YMAX)
        ENDDO

        I1=MAX(1,INT(XMIN+0.5))
        I2=MIN(I_NX,INT(XMAX+0.5))
        J1=MAX(1,INT(YMIN+0.5))
        J2=MIN(I_NY,INT(YMAX+0.5))

      ENDIF

      END



*+ IMG_SETWHOLE
	SUBROUTINE IMG_SETWHOLE(STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
        BYTE ZERO
        PARAMETER (ZERO='00'X)
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        I_DX=0.0
        I_DY=0.0
        I_R=0.0
        I_RPIX=0.0
        I_IX1=1
        I_IX2=I_NX
        I_IY1=1
        I_IY2=I_NY

        CALL ARR_INIT1B(ZERO,I_NX*I_NY,%val(I_REG_PTR),STATUS)
        I_REG_TYPE='NONE'

      ENDIF

      END



*+ IMG_INBOUNDS
	LOGICAL FUNCTION IMG_INBOUNDS(I,J)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Import :
      INTEGER I,J
*  Export :
*  Status :
*  Local constants :
*  Local variables :
*-

      IMG_INBOUNDS=(I.GE.1.AND.I.LE.I_NX.AND.J.GE.1.AND.J.LE.I_NY)


      END



*+ IMG_INCIRC
	LOGICAL FUNCTION IMG_INCIRC(I,J,XC,YC,R)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Import :
      INTEGER I,J
      REAL XC,YC,R
*  Export :
*  Status :
      INTEGER STATUS
*  Functions :
      LOGICAL IMG_INBOUNDS
*  Local constants :
*  Local variables :
      REAL XPIX,YPIX
      REAL X,Y
      REAL DISP
*-
      STATUS=SAI__OK
      XPIX=REAL(I)
      YPIX=REAL(J)
      CALL IMG_PIXTOWORLD(XPIX,YPIX,X,Y,STATUS)
      DISP=SQRT((X-XC)**2 + (Y-YC)**2)

      IMG_INCIRC=(DISP.LE.R.AND.IMG_INBOUNDS(I,J))


      END



*+ IMG_INANNULUS
	LOGICAL FUNCTION IMG_INANNULUS(I,J,XC,YC,IRAD,ORAD)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Import :
      INTEGER I,J
      REAL XC,YC,IRAD,ORAD
*  Export :
*  Status :
      INTEGER STATUS
*  Functions :
      LOGICAL IMG_INBOUNDS
*  Local constants :
*  Local variables :
      REAL XPIX,YPIX
      REAL X,Y
      REAL DISP
*-
      STATUS=SAI__OK
      XPIX=REAL(I)
      YPIX=REAL(J)
      CALL IMG_PIXTOWORLD(XPIX,YPIX,X,Y,STATUS)
      DISP=SQRT((X-XC)**2 + (Y-YC)**2)

      IMG_INANNULUS=(DISP.GE.IRAD.AND.DISP.LE.ORAD.AND.
     :                                      IMG_INBOUNDS(I,J))


      END



*+ IMG_INSLICE - determine whether pixel is inside slice
	LOGICAL FUNCTION IMG_INSLICE(I,J,XC,YC,ANGLE,LENGTH,WIDTH)

*    Description :
*    Type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*    Import :
        INTEGER I,J
        REAL XC,YC,ANGLE,LENGTH,WIDTH
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*  Functions :
      LOGICAL IMG_INBOUNDS
*    Local variables :
      REAL X,Y
      REAL THETA
      REAL DISP
      REAL LANGLE
*-

*  get world coord of pixel centre
        STATUS=SAI__OK
        CALL IMG_PIXTOWORLD(REAL(I),REAL(J),X,Y,STATUS)

*  adjust angle to direction of axes
        LANGLE=ANGLE*(I_XSCALE/ABS(I_XSCALE))
     :                         *(I_YSCALE/ABS(I_YSCALE))


*  transform to frame centred on slice
        X=X-XC
        Y=Y-YC

*  rotate to frame parallel with slice axes
        THETA=ATAN2(Y,X)
        DISP=SQRT(X**2 + Y**2)
        X=DISP*COS(LANGLE-THETA)
        Y=DISP*SIN(LANGLE-THETA)

*  check transformed coord falls within slice
        IMG_INSLICE=(ABS(X).LE.LENGTH/2.0.AND.ABS(Y).LE.WIDTH/2.0.AND.
     :                                   IMG_INBOUNDS(I,J))


	END





*+ IMG_INELLIPSE - determine whether pixel is inside ellipse
	LOGICAL FUNCTION IMG_INELLIPSE(I,J,XC,YC,MAJOR,MINOR,ANGLE)

*    Description :
*    Type Definitions :
        IMPLICIT NONE
*    Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*    Import :
        INTEGER I,J
        REAL XC,YC,MAJOR,MINOR,ANGLE
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*  Functions :
      LOGICAL IMG_INBOUNDS
*    Local variables :
      REAL X,Y
      REAL THETA
      REAL DISP
      REAL LANGLE
      REAL ALPHA
      REAL RAD
*-

*  get world coord of pixel centre
        STATUS=SAI__OK
        CALL IMG_PIXTOWORLD(REAL(I),REAL(J),X,Y,STATUS)

*  adjust angle to direction of axes
        LANGLE=ANGLE*(I_XSCALE/ABS(I_XSCALE))
     :                         *(I_YSCALE/ABS(I_YSCALE))


*  transform to frame centred on ellipse
        X=X-XC
        Y=Y-YC

*  rotate to frame parallel with major axis
        THETA=ATAN2(Y,X)
        DISP=SQRT(X**2 + Y**2)
        X=DISP*COS(LANGLE-THETA)
        Y=DISP*SIN(LANGLE-THETA)

*  get angular displacement of point
        ALPHA=ATAN2(Y,X)

*  get radius of ellipse at this point
        RAD=SQRT((MAJOR*COS(ALPHA))**2 + (MINOR*SIN(ALPHA))**2)

*  check transformed coord falls within ellipse
        IMG_INELLIPSE=(DISP.LE.RAD.AND.IMG_INBOUNDS(I,J))


	END




*+ IMG_INPOLY - is pixel inside given polygon
      LOGICAL FUNCTION IMG_INPOLY(I,J,NVERTEX,XVERT,YVERT)
*
*    Description :
*    Method :
*    Deficiencies :
*
*     The GEO_POLYIN routine is not robust to highly convoluted polygons.
*
*    Bugs :
*    Authors :
*
*    History :
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS

*    Import :
      INTEGER             I,J
      INTEGER             NVERTEX              ! # of vertices
      REAL                XVERT(*)             ! World X coord of vertices
      REAL                YVERT(*)             ! World Y coord of vertices
*
*    Global Variables :
*
      INCLUDE 'IMG_CMN'
*
*    Functions :
      LOGICAL IMG_INBOUNDS
*
*
*    Local Constants :
*
*    Local variables :
*
      REAL                XW, YW
      INTEGER             ISTAT                  ! Local poly include status
      INTEGER             NIN, NOUT              ! # points inside/outside
      LOGICAL FLAG
*-
*  Perform test

      STATUS=SAI__OK
      CALL IMG_PIXTOWORLD(REAL(I),REAL(J),XW,YW,STATUS)
      CALL GEO_POLYIN(NVERTEX,XVERT,YVERT,'INSIDE',1,XW,YW,NIN,NOUT,
     :                                            FLAG,ISTAT,STATUS)

*  Polygon too complex?
      IF (ISTAT.NE.0) THEN
        CALL MSG_PRNT('AST_ERR: Polygon too complex')
        FLAG=.FALSE.
      ENDIF

      IMG_INPOLY=(FLAG.AND.IMG_INBOUNDS(I,J))

      END




*+ IMG_INREG
	LOGICAL FUNCTION IMG_INREG(I,J)

        IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*  Import :
      INTEGER I,J
*  Export :
*  Status :
*  Local constants :
*  Local variables :
      INTEGER ISTAT
      BYTE REG
*-
      IF (I_REG_TYPE.EQ.'NONE') THEN

        IMG_INREG=.TRUE.

      ELSE

        ISTAT=SAI__OK
        CALL IMG_GETREG(I,J,REG,ISTAT)
        IMG_INREG=(REG.EQ.'01'X)

      ENDIF


      END



*+ IMG_REMCURR
	SUBROUTINE IMG_REMCURR(MODE,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
      CHARACTER*(*) MODE
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
      REAL X,Y,R,XP,YP,RP,DX,DY
      INTEGER IX1,IX2,IY1,IY2
      SAVE X,Y,R,XP,YP,RP,DX,DY,IX1,IX2,IY1,IY2
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (MODE.EQ.'SAVE') THEN
          X=I_X
          Y=I_Y
          R=I_R
          XP=I_XPIX
          YP=I_YPIX
          RP=I_RPIX
          IX1=I_IX1
          IX2=I_IX2
          IY1=I_IY1
          IY2=I_IY2
          DX=I_DX
          DY=I_DY
        ELSEIF (MODE.EQ.'RESTORE') THEN
          I_X=X
          I_Y=Y
          I_R=R
          I_XPIX=XP
          I_YPIX=YP
          I_RPIX=RP
          I_IX1=IX1
          I_IX2=IX2
          I_IY1=IY1
          I_IY2=IY2
          I_DX=DX
          I_DY=DY
        ENDIF


      ENDIF

      END




*+  IMG_GETBOX - define rectangular section of image
      SUBROUTINE IMG_GETBOX(PAR1,PAR2,PAR3,PAR4,XC,YC,DX,DY,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) PAR1,PAR2,PAR3,PAR4
*    Import-Export :
*    Export :
      REAL XC,YC
      REAL DX,DY
*    Status :
      INTEGER STATUS
*    External references :
*    Local Constants :
*    Local variables :
      REAL XCORN,YCORN,XWID,YWID
      CHARACTER*1 CH
      LOGICAL LEFT,RIGHT
*    Global Variables :
      INCLUDE 'IMG_CMN'
*-
      IF (STATUS.EQ.SAI__OK) THEN

        IF (I_MODE.EQ.1) THEN
*  get centre
          CALL MSG_PRNT(' ')
          XC=I_X
          YC=I_Y
          CALL MSG_SETR('XC',XC)
          CALL MSG_SETR('YC',YC)
          CALL MSG_PRNT('Select centre/^XC,^YC/...')
          CALL GFX_CURS(XC,YC,LEFT,RIGHT,CH,STATUS)
          IF (CH.EQ.CHAR(13)) THEN
            XC=I_X
            YC=I_Y
          ENDIF
          CALL PGPOINT(1,XC,YC,2)

          CALL MSG_PRNT('Select any corner...')
          XCORN=XC
          YCORN=YC
          CALL GFX_CURS(XCORN,YCORN,LEFT,RIGHT,CH,STATUS)

*  calculate box half-widths
          DX=ABS(XCORN-XC)
          DY=ABS(YCORN-YC)

*  keyboard mode
        ELSE

          CALL USI_DEF0R(PAR1,I_X,STATUS)
          CALL USI_GET0R(PAR1,XC,STATUS)
          CALL USI_DEF0R(PAR2,I_Y,STATUS)
          CALL USI_GET0R(PAR2,YC,STATUS)
          CALL USI_GET0R(PAR3,XWID,STATUS)
          CALL USI_GET0R(PAR4,YWID,STATUS)
          DX=XWID/2.0
          DY=YWID/2.0

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('IMG_GETBOX',STATUS)
        ENDIF

      ENDIF

      END



*+ IMG_GETSLICE - get rectangular slice of image
      SUBROUTINE IMG_GETSLICE(PAR1,PAR2,PAR3,PAR4,PAR5,XCENT,YCENT,
     :                                   ANGLE,LENGTH,WIDTH,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) PAR1,PAR2,PAR3,PAR4,PAR5
*    Export :
      REAL XCENT,YCENT,ANGLE,LENGTH,WIDTH
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      REAL PI,DTOR
      PARAMETER (PI=3.14159265,DTOR=PI/180.0)
*    Local variables :
      CHARACTER*1 CH
      REAL PXCENT,PYCENT
      REAL XEND,YEND
      REAL PXEND,PYEND
      REAL XOEND,YOEND
      REAL PXOEND,PYOEND
      REAL XWID,YWID
      REAL PXWID,PYWID
c      REAL HLEN
c      REAL HWID
      REAL PLENGTH,PHLEN,PWIDTH,PHWID
      REAL ALPHA,BETA
      REAL D
      REAL XTR,YTR,XTL,YTL,XBR,YBR,XBL,YBL
      REAL PXTR,PYTR,PXTL,PYTL,PXBR,PYBR,PXBL,PYBL
      REAL A,ASQ,B,BSQ,CSQ
      INTEGER LS
      LOGICAL LEFT,RIGHT
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get centre of cut
        IF (I_MODE.EQ.1) THEN
          CALL MSG_PRNT(' ')
          XCENT=I_X
          YCENT=I_Y
          CALL MSG_SETR('X',XCENT)
          CALL MSG_SETR('Y',YCENT)
          CALL MSG_PRNT('Select centre/^X,^Y/...')
          CALL GFX_CURS(XCENT,YCENT,LEFT,RIGHT,CH,STATUS)
          IF (CH.EQ.CHAR(13).OR.RIGHT) THEN
            XCENT=I_X
            YCENT=I_Y
          ENDIF
          CALL PGPOINT(1,XCENT,YCENT,2)
          CALL IMG_WORLDTOPIX(XCENT,YCENT,PXCENT,PYCENT,STATUS)

*  get mid-point of end
          CALL MSG_PRNT('Select end...')
          XEND=XCENT
          YEND=YCENT
          CALL GFX_CURS(XEND,YEND,LEFT,RIGHT,CH,STATUS)
          CALL PGPOINT(1,XEND,YEND,2)
          CALL IMG_WORLDTOPIX(XEND,YEND,PXEND,PYEND,STATUS)

*  calculate other end and draw centre line
          XOEND=2.0*XCENT-XEND
          YOEND=2.0*YCENT-YEND
          CALL IMG_WORLDTOPIX(XOEND,YOEND,PXOEND,PYOEND,STATUS)
          CALL PGPOINT(1,XOEND,YOEND,2)
          CALL PGQLS(LS)
          CALL PGSLS(2)
          CALL PGDRAW(XEND,YEND)


*  get width
          CALL MSG_PRNT('Select width...')
          XWID=XCENT
          YWID=YCENT
          CALL GFX_CURS(XWID,YWID,LEFT,RIGHT,CH,STATUS)
          CALL IMG_WORLDTOPIX(XWID,YWID,PXWID,PYWID,STATUS)

*  calc length
          LENGTH=2.0*SQRT((XEND-XCENT)**2 + (YEND-YCENT)**2)
          PHLEN=SQRT((PXEND-PXCENT)**2 + (PYEND-PYCENT)**2)
          PHLEN=MAX(1.0,PHLEN)
          PLENGTH=PHLEN*2.0

*  calc angle
          ANGLE=ATAN2((PYEND-PYCENT),(PXEND-PXCENT))

*  calc width (pixels)
          D=SQRT((PXWID-PXCENT)**2 + (PYWID-PYCENT)**2)
          ALPHA=ATAN2((PYWID-PYCENT),(PXWID-PXCENT))
          BETA=ALPHA-ANGLE
          PHWID=ABS(D*SIN(BETA))
          PHWID=MAX(0.5,PHWID)
          PWIDTH=PHWID*2.0
*  calc width (world coords)
          ASQ=(XWID-XEND)**2 + (YWID-YEND)**2
          A=SQRT(ASQ)
          BSQ=(XEND-XOEND)**2 + (YEND-YOEND)**2
          B=SQRT(BSQ)
          CSQ=(XWID-XOEND)**2 + (YWID-YOEND)**2
          ALPHA=ACOS((ASQ+BSQ-CSQ)/(2.0*A*B))
          WIDTH=2.0*A*SIN(ALPHA)

*  keyboard mode
        ELSE
          CALL USI_DEF0R(PAR1,I_X,STATUS)
          CALL USI_GET0R(PAR1,XCENT,STATUS)
          CALL USI_DEF0R(PAR2,I_Y,STATUS)
          CALL USI_GET0R(PAR2,YCENT,STATUS)
          CALL USI_GET0R(PAR3,ANGLE,STATUS)
          CALL USI_GET0R(PAR4,LENGTH,STATUS)
          CALL USI_GET0R(PAR5,WIDTH,STATUS)
          ANGLE=ANGLE*DTOR
*  convert to pixel coords
          CALL IMG_WORLDTOPIX(XCENT,YCENT,PXCENT,PYCENT,STATUS)
          PLENGTH = LENGTH/(ABS(I_XSCALE*COS(ANGLE)) +
     :                        ABS(I_YSCALE*SIN(ANGLE)))
          PLENGTH=MAX(2.0,PLENGTH)
          PWIDTH = WIDTH/(ABS(I_XSCALE*SIN(ANGLE))   +
     :                        ABS(I_YSCALE*COS(ANGLE)))
          PWIDTH=MAX(1.0,PWIDTH)
          PHWID=PWIDTH/2.0
          PHLEN=PLENGTH/2.0
          PXEND=PXCENT+PHLEN*COS(ANGLE)
          PXOEND=PXCENT-PHLEN*COS(ANGLE)
          PYEND=PYCENT+PHLEN*SIN(ANGLE)
          PYOEND=PYCENT-PHLEN*SIN(ANGLE)
        ENDIF

*  plot extent of cut
        PXTR=PXEND+PHWID*SIN(ANGLE)
        PYTR=PYEND-PHWID*COS(ANGLE)
        PXTL=PXEND-PHWID*SIN(ANGLE)
        PYTL=PYEND+PHWID*COS(ANGLE)
        PXBR=PXOEND+PHWID*SIN(ANGLE)
        PYBR=PYOEND-PHWID*COS(ANGLE)
        PXBL=PXOEND-PHWID*SIN(ANGLE)
        PYBL=PYOEND+PHWID*COS(ANGLE)
        CALL IMG_PIXTOWORLD(PXBR,PYBR,XBR,YBR,STATUS)
        CALL IMG_PIXTOWORLD(PXBL,PYBL,XBL,YBL,STATUS)
        CALL IMG_PIXTOWORLD(PXTL,PYTL,XTL,YTL,STATUS)
        CALL IMG_PIXTOWORLD(PXTR,PYTR,XTR,YTR,STATUS)
        CALL PGSLS(1)
        CALL PGMOVE(XBR,YBR)
        CALL PGDRAW(XBL,YBL)
        CALL PGDRAW(XTL,YTL)
        CALL PGDRAW(XTR,YTR)
        CALL PGDRAW(XBR,YBR)
        CALL PGSLS(LS)


      ENDIF


      END



*+ IMG_GETELLIPSE - get ellipse
      SUBROUTINE IMG_GETELLIPSE(PAR1,PAR2,PAR3,PAR4,PAR5,XCENT,YCENT,
     :                                       MAJOR,MINOR,ANGLE,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      CHARACTER*(*) PAR1,PAR2,PAR3,PAR4,PAR5
*    Export :
      REAL XCENT,YCENT,MAJOR,MINOR,ANGLE
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      REAL PI,DTOR
      PARAMETER (PI=3.14159265,DTOR=PI/180.0)
*    Local variables :
      CHARACTER*1 CH
      REAL PXCENT,PYCENT
      REAL XEND,YEND
      REAL PXEND,PYEND
      REAL XOEND,YOEND
      REAL XWID,YWID
      REAL A,ASQ,B,BSQ,CSQ
      REAL ALPHA
      LOGICAL LEFT,RIGHT
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get centre of cut
        IF (I_MODE.EQ.1) THEN
          CALL MSG_PRNT(' ')
          XCENT=I_X
          YCENT=I_Y
          CALL MSG_SETR('X',XCENT)
          CALL MSG_SETR('Y',YCENT)
          CALL MSG_PRNT('Select centre/^X,^Y/...')
          CALL GFX_CURS(XCENT,YCENT,LEFT,RIGHT,CH,STATUS)
          IF (CH.EQ.CHAR(13).OR.RIGHT) THEN
            XCENT=I_X
            YCENT=I_Y
          ENDIF
          CALL PGPOINT(1,XCENT,YCENT,2)

*  get mid-point of end
          CALL MSG_PRNT('Select major radius...')
          XEND=XCENT
          YEND=YCENT
          CALL GFX_CURS(XEND,YEND,LEFT,RIGHT,CH,STATUS)
          CALL PGPOINT(1,XEND,YEND,2)

*  calculate other end and draw centre line
          XOEND=2.0*XCENT-XEND
          YOEND=2.0*YCENT-YEND
          CALL PGPOINT(1,XOEND,YOEND,2)
          CALL PGSLS(2)
          CALL PGDRAW(XEND,YEND)

          CALL GCB_SETDEF(STATUS)

*  get width
          CALL MSG_PRNT('Select minor radius...')
          XWID=XCENT
          YWID=YCENT
          CALL GFX_CURS(XWID,YWID,LEFT,RIGHT,CH,STATUS)

*  calc major radius
          MAJOR=SQRT((XEND-XCENT)**2 + (YEND-YCENT)**2)

*  calc angle
          CALL IMG_WORLDTOPIX(XCENT,YCENT,PXCENT,PYCENT,STATUS)
          CALL IMG_WORLDTOPIX(XEND,YEND,PXEND,PYEND,STATUS)
          ANGLE=ATAN2((PYEND-PYCENT),(PXEND-PXCENT))

*  calc width (world coords)
          ASQ=(XWID-XEND)**2 + (YWID-YEND)**2
          A=SQRT(ASQ)
          BSQ=(XEND-XOEND)**2 + (YEND-YOEND)**2
          B=SQRT(BSQ)
          CSQ=(XWID-XOEND)**2 + (YWID-YOEND)**2
          ALPHA=ACOS((ASQ+BSQ-CSQ)/(2.0*A*B))
          MINOR=A*SIN(ALPHA)

*  keyboard mode
        ELSE
          CALL USI_DEF0R(PAR1,I_X,STATUS)
          CALL USI_GET0R(PAR1,XCENT,STATUS)
          CALL USI_DEF0R(PAR2,I_Y,STATUS)
          CALL USI_GET0R(PAR2,YCENT,STATUS)
          CALL USI_GET0R(PAR3,MAJOR,STATUS)
          CALL USI_GET0R(PAR4,MINOR,STATUS)
          CALL USI_GET0R(PAR5,ANGLE,STATUS)
          ANGLE=ANGLE*DTOR
        ENDIF

*  plot ellipse
        CALL IMG_ELLIPSE(XCENT,YCENT,MAJOR,MINOR,ANGLE,STATUS)

      ENDIF


      END





*+  IMG_GETARD - define region described by ARD
      SUBROUTINE IMG_GETARD(PAR1,RMASK,STATUS)
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global Variables :
      INCLUDE 'IMG_CMN'
*    Import :
      CHARACTER*(*) PAR1
*    Import-Export :
*    Export :
      INTEGER RMASK(I_NX,I_NY)
*    Status :
      INTEGER STATUS
*    External references :
*    Local Constants :
*    Local variables :
      INTEGER GRPID
      INTEGER DIMS(2)
      REAL BASE(2),SCALE(2)
      CHARACTER*(20) UNITS(2)
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get ARD input
        CALL ARX_OPEN('READ',GRPID,STATUS)
        CALL ARX_READ(PAR1,GRPID,STATUS)


*  convert to mask
        DIMS(1)=I_NX
        DIMS(2)=I_NY
        BASE(1)=I_XBASE
        BASE(2)=I_YBASE
        SCALE(1)=I_XSCALE
        SCALE(2)=I_YSCALE
        UNITS(1)=I_XYUNITS
        UNITS(2)=I_XYUNITS
        CALL ARX_MASK(GRPID,DIMS,BASE,SCALE,UNITS,RMASK,STATUS)


        CALL ARX_CLOSE(GRPID,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'IMG_GETARD',STATUS)
        ENDIF

      ENDIF

      END




*+  IMG_GETCIRC - select a circular region of the plot
      SUBROUTINE IMG_GETCIRC(PAR1,PAR2,PAR3,XC,YC,RAD,STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Import :
      CHARACTER*(*) PAR1,PAR2,PAR3
*    Export :
      REAL XC,YC,RAD
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*1 CH
      REAL XR,YR
      LOGICAL LEFT,RIGHT
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  cursor mode
        IF (I_MODE.EQ.1) THEN
*  get centre
          XC=I_X
          YC=I_Y
          CALL MSG_PRNT(' ')
          CALL MSG_SETR('XC',XC)
          CALL MSG_SETR('YC',YC)
          CALL MSG_PRNT('Select centre/^XC,^YC/...')
          CALL GFX_CURS(XC,YC,LEFT,RIGHT,CH,STATUS)
          IF (CH.EQ.CHAR(13)) THEN
            XC=I_X
            YC=I_Y
          ENDIF
          CALL PGPOINT(1,XC,YC,2)

*  get radius
          CALL MSG_SETR('RAD',I_R)
          CALL MSG_PRNT('Select radius/^RAD/...')
          XR=XC
          YR=YC
          CALL GFX_CURS(XR,YR,LEFT,RIGHT,CH,STATUS)
          IF (CH.EQ.CHAR(13)) THEN
            RAD=I_R
          ELSE
            RAD=SQRT((XR-XC)**2 + (YR-YC)**2)
          ENDIF

*  keyboard mode
        ELSE
          CALL USI_DEF0R(PAR1,I_X,STATUS)
          CALL USI_GET0R(PAR1,XC,STATUS)
          CALL USI_DEF0R(PAR2,I_Y,STATUS)
          CALL USI_GET0R(PAR2,YC,STATUS)
          CALL USI_DEF0R(PAR3,I_R,STATUS)
          CALL USI_GET0R(PAR3,RAD,STATUS)
        ENDIF

        CALL IMG_CIRCLE(XC,YC,RAD,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('IMG_GETCIRC',STATUS)
        ENDIF

      ENDIF

      END




*+  IMG_GETANNULUS - select an annular region of the plot
      SUBROUTINE IMG_GETANNULUS(PAR1,PAR2,PAR3,PAR4,XC,YC,IRAD,ORAD,
     :                                                        STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Import :
      CHARACTER*(*) PAR1,PAR2,PAR3,PAR4
*    Export :
      REAL XC,YC,IRAD,ORAD
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*1 CH
      REAL XR,YR
      LOGICAL LEFT,RIGHT
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  cursor mode
        IF (I_MODE.EQ.1) THEN
*  get centre
          XC=I_X
          YC=I_Y
          CALL MSG_PRNT(' ')
          CALL MSG_SETR('XC',XC)
          CALL MSG_SETR('YC',YC)
          CALL MSG_PRNT('Select centre/^XC,^YC/...')
          CALL GFX_CURS(XC,YC,LEFT,RIGHT,CH,STATUS)
          IF (CH.EQ.CHAR(13)) THEN
            XC=I_X
            YC=I_Y
          ENDIF
          CALL PGPOINT(1,XC,YC,2)

*  get radii
          CALL MSG_SETR('RAD',I_R)
          CALL MSG_PRNT('Select inner radius/^RAD/...')
          XR=XC
          YR=YC
          CALL GFX_CURS(XR,YR,LEFT,RIGHT,CH,STATUS)
          IF (CH.EQ.CHAR(13)) THEN
            IRAD=I_R
          ELSE
            IRAD=SQRT((XR-XC)**2 + (YR-YC)**2)
          ENDIF
          CALL IMG_CIRCLE(XC,YC,IRAD,STATUS)

          CALL MSG_SETR('RAD',I_R)
          CALL MSG_PRNT('Select outer radius/^RAD/...')
          XR=XC
          YR=YC
          CALL GFX_CURS(XR,YR,LEFT,RIGHT,CH,STATUS)
          IF (CH.EQ.CHAR(13)) THEN
            ORAD=I_R
          ELSE
            ORAD=SQRT((XR-XC)**2 + (YR-YC)**2)
          ENDIF
          CALL IMG_CIRCLE(XC,YC,ORAD,STATUS)

*  keyboard mode
        ELSE
          CALL USI_DEF0R(PAR1,I_X,STATUS)
          CALL USI_GET0R(PAR1,XC,STATUS)
          CALL USI_DEF0R(PAR2,I_Y,STATUS)
          CALL USI_GET0R(PAR2,YC,STATUS)
          CALL PGPOINT(1,XC,YC,2)
          CALL USI_DEF0R(PAR3,I_R,STATUS)
          CALL USI_GET0R(PAR3,IRAD,STATUS)
          CALL IMG_CIRCLE(XC,YC,IRAD,STATUS)
          CALL USI_DEF0R(PAR4,I_R,STATUS)
          CALL USI_GET0R(PAR4,ORAD,STATUS)
          CALL IMG_CIRCLE(XC,YC,ORAD,STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('IMG_GETANNULUS',STATUS)
        ENDIF

      ENDIF

      END



*+ IMG_GETPOLY - get vertices of polygon
      SUBROUTINE IMG_GETPOLY(MAXVERT,XVERT,YVERT,NVERTEX,STATUS)
*
*    Description :
*
*    Method :
*
*
*
*    Deficiencies :
*
*
*    Bugs :
*    Authors :
*
*    History :
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'

*    Import :
      INTEGER        MAXVERT                        ! Max no. of vertices

*    Export :
      REAL                XVERT(MAXVERT)             ! World X coord of vertices
      REAL                YVERT(MAXVERT)             ! World Y coord of vertices
      INTEGER             NVERTEX                    ! # of vertices

*    Status :
*
      INTEGER STATUS
*
*    Global Variables :
*
      INCLUDE 'IMG_CMN'
*
*    Functions :
*
*    Local Constants :
*
      INTEGER        OPEN_BOX                       ! Vertex symbol
        PARAMETER    ( OPEN_BOX = 6 )
*
*    Local variables :
*
      CHARACTER           CH                         ! Cursor character
      REAL                XW, YW                     ! Cursor position
      LOGICAL             LEFT,RIGHT		     ! left or right button
*-
      IF (STATUS.EQ.SAI__OK) THEN

        NVERTEX=0

*      Instructions
        CALL MSG_PRNT(' ')
        CALL MSG_PRNT(
     :            ' Select vertices - X to close polygon and eXit...')
        CALL MSG_PRNT(' ')

*      Select vertices
        XW=I_X
        YW=I_Y
        CH=' '
        RIGHT=.FALSE.
        NVERTEX = 0
        DO WHILE (CH.NE.'X'.AND..NOT.RIGHT.AND.NVERTEX.NE.MAXVERT)

*        Get cursor position in world coords
          CALL GFX_CURS(XW,YW,LEFT,RIGHT,CH,STATUS)

          IF (CH.NE.'X'.AND..NOT.RIGHT) THEN

            NVERTEX = NVERTEX + 1
            XVERT(NVERTEX) = XW
            YVERT(NVERTEX) = YW

            IF ( NVERTEX .EQ. MAXVERT ) THEN
              CALL MSG_PRNT( 'Maximum number of polygon vertices '/
     :                                  /'used - closing polygon' )
            ENDIF

*          Draw a box on the first point
            IF ( NVERTEX .EQ. 1 ) THEN
              CALL PGPOINT( 1, XVERT(NVERTEX), YVERT(NVERTEX),
     :                                              OPEN_BOX )

*          otherwise draw line from previous point
            ELSE
              CALL PGMOVE( XVERT(NVERTEX-1), YVERT(NVERTEX-1) )
              CALL PGDRAW( XVERT(NVERTEX), YVERT(NVERTEX) )

            ENDIF

          ENDIF

        ENDDO

*      Abort if too few vertices
        IF (NVERTEX.GT.0.AND.NVERTEX.LT.3) THEN
          CALL MSG_PRNT( 'AST_ERR: Too few vertices' )
          STATUS=SAI__ERROR
          NVERTEX=0
        ELSEIF (NVERTEX.GE.3) THEN

*      Draw line to first point to complete polygon
          CALL PGMOVE( XVERT(NVERTEX), YVERT(NVERTEX) )
          CALL PGDRAW( XVERT(1), YVERT(1) )

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('IMG_GETPOLY',STATUS)
        ENDIF

      ENDIF


      END



*+ IMG_BOX - draw box
	SUBROUTINE IMG_BOX(XC,YC,DX,DY,STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
*  Import :
      REAL XC,YC,DX,DY
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
      REAL X1,X2,Y1,Y2
*-
      IF (STATUS.EQ.SAI__OK) THEN

        X1=XC-DX
        X2=XC+DX
        Y1=YC-DY
        Y2=YC+DY

        CALL PGMOVE(X1,Y1)
        CALL PGDRAW(X2,Y1)
        CALL PGDRAW(X2,Y2)
        CALL PGDRAW(X1,Y2)
        CALL PGDRAW(X1,Y1)


      ENDIF

      END




*+ IMG_MINMAX - update data min & max
	SUBROUTINE IMG_MINMAX( STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'PRM_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
      INTEGER NVAL
      INTEGER ID

*-
      IF (STATUS.EQ.SAI__OK) THEN

        NVAL=I_NX*I_NY

        IF (I_QOK) THEN

	print *,'arr_rang1rq'
	call flush(6)
          CALL ARR_RANG1RQ(NVAL,%val(I_DPTR),%val(I_QPTR),I_MASK,
     :                                       I_DMIN,I_DMAX,STATUS)
	print *,'done'
	call flush(6)

        ELSE
	print *,'arr_rang1rm'
	call flush(6)

          CALL ARR_RANG1RM(NVAL,%val(I_DPTR),I_DMIN,I_DMAX,STATUS)
	print *,'done'
	call flush(6)

        ENDIF


*  being run from a GUI so update noticeboard
        IF (I_GUI) THEN
          CALL NBS_FIND_ITEM(I_NBID,'MIN',ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,I_DMIN,STATUS)
          CALL NBS_FIND_ITEM(I_NBID,'MAX',ID,STATUS)
          CALL NBS_PUT_VALUE(ID,0,VAL__NBR,I_DMAX,STATUS)
        ENDIF

      ENDIF

      END




*+ IMG_
	SUBROUTINE IMG_( STATUS)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
*    Global variables :
        INCLUDE 'IMG_CMN'
*  Import :
*  Export :
*  Status :
        INTEGER STATUS
*  Local constants :
*  Local variables :
*-
      IF (STATUS.EQ.SAI__OK) THEN



      ENDIF

      END
