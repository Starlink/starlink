*+  STR_ROOTEXT - Creates new file-name by adding new extension to root of file
      SUBROUTINE STR_ROOTEXT(INFILE,EXTN,OUTFIL)
* Description :
*	Creates new file-name by adding new extension to root of file
* Restrictions:
*         intended for VAX/VMS filename formats only.
* History :
*       Author	MGW 1988 April 5
*       Asterix88 version   May 1988   (LTVAD::RDS)
* Type Definitions :
        IMPLICIT NONE
* Import :
	CHARACTER*(*) INFILE      ! Name of file (which may include structured
*                                 !     directory, extension, version number)
	CHARACTER*(*) EXTN        ! The default extension complete with a
*                                 !                leading dot.
* Import-Export :
* Export :
	CHARACTER*(*) OUTFIL      ! The bare file complete with extension
*                                 !                 inserted/appended.
* Functions :
        INTEGER CHR_LEN
* Local constants :
* Local variables :
        INTEGER L
*-
	CALL STR_ROOT(INFILE,OUTFIL)
*
	L = INDEX(OUTFIL,'.')
	IF(L.GT.0) THEN
          OUTFIL = OUTFIL(1:L-1)//EXTN
	ELSE
          L = CHR_LEN(OUTFIL)
          OUTFIL = OUTFIL(1:L)//EXTN
	ENDIF
*
	END
