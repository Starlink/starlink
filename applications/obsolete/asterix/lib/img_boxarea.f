*+  IMG_BOXAREA - Computes areas of circular/rectangular image boxes.
	SUBROUTINE IMG_BOXAREA(SHAPE, ICX, ICY, IDX, IDY, AREA)
* Description :
*     Computes areas of circular/rectangular image boxes.
*    It calculates the area for two boxes, usually these will
*    correspond to source and background.
* History :
*       Author	Clive Page(original)	1987 MAY 1
*       May 10 1988  Asterix88 version   (LTVAD::RDS)
* Type Definitions :
      IMPLICIT NONE
* Import :
	CHARACTER*1 SHAPE	!input: 'C' for circle, 'R' for rectangle.
*    Arrays have element (1)=source, (2)=background, all units are pixels.
	INTEGER ICX(2)		!input: centre of boxes, x-coordinate.
	INTEGER ICY(2)		!input: centre of boxes, y-coordinate.
	INTEGER IDX(2)		!input: radiii (circle) or half-width (rect)
	INTEGER IDY(2)		!input: half-heights (rectangle)
* Import-Export :
*     <declarations and descriptions for imported/exported arguments>
* Export :
	REAL AREA(2)		!output: areas in square pixels.
* Local constants :
        REAL PI
	PARAMETER (PI = 3.1415926535)
* Local variables :
	LOGICAL CONCENT, OVERLAP
	INTEGER IAREA(9)
        INTEGER K
        INTEGER KDX,KDY
*
        REAL DELTA
* Local data :
*-
	DATA IAREA/ 5, 13, 29, 49, 81, 113, 149, 197, 253 /
*
	CONCENT = .FALSE.
	OVERLAP = .FALSE.
*
	DO K = 1,2
	    IF(SHAPE .EQ. 'C') THEN
		IF(ABS(IDX(K)) .LE. 9) THEN
		    AREA(K) = IAREA(ABS(IDX(K)))
		ELSE
		    AREA(K) = INT(PI * IDX(K)**2)
		END IF
	    ELSE
		AREA(K) = (2*IDX(K) + 1) * (2*IDY(K) + 1)
	    END IF
	END DO
*Now allow for possibility of concentric boxes
*Overlapping boxes is an error.
	IF(SHAPE .EQ. 'C') THEN
	    DELTA = SQRT(REAL((ICX(1)-ICX(2))**2 + (ICY(1)-ICY(2))**2))
	    IF(DELTA + MIN(IDX(1),IDX(2)) .LE. MAX(IDX(1),IDX(2))) THEN
		CONCENT = .TRUE.
	    ELSE IF(DELTA .LT. IDX(1) + IDX(2)) THEN
		OVERLAP = .TRUE.
	    END IF
	ELSE IF(SHAPE .EQ. 'R') THEN
	    KDX = ABS(ICX(1) - ICX(2))
	    KDY = ABS(ICY(1) - ICY(2))
	    IF(KDX + MIN(IDX(1),IDX(2)) .LE. MAX(IDX(1),IDX(2)) .AND.
     &         KDY + MIN(IDY(1),IDY(2)) .LE. MAX(IDY(1),IDY(2))) THEN
		CONCENT = .TRUE.
	    ELSE IF(KDX .LT. IDX(1) + IDX(2) .AND.
     &		    KDY .LT. IDY(1) + IDY(2)) THEN
		OVERLAP = .TRUE.
	    END IF
	ELSE
	    WRITE(*,*)'BOXAREA: unknown shape ',SHAPE
	END IF
	IF(AREA(1) .GT. AREA(2)) THEN
	    WRITE(*,*)'BOXAREA warning: background box should be bigger',
     &        AREA
	ELSE IF(CONCENT) THEN
	    AREA(2) = AREA(2) - AREA(1)
	ELSE IF(OVERLAP) THEN
	    WRITE(*,*)'BOXAREA warning: overlapping boxes'
	END IF
	END
