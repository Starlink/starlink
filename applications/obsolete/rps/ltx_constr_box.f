*+LTX_CONSTR_BOX   Finds where to put the Constraint X
      SUBROUTINE LTX_CONSTR_BOX(IFIELD,IFILE,FIELD,
     &           STRING,XS,YS,SKIPNEXT)
      IMPLICIT NONE
 
*  Calling Arguments
      INTEGER IFIELD			! In	Database Field no.
      INTEGER IFILE			!	   :: section
      CHARACTER*19 FIELD		!	 Fieldname
      CHARACTER*(*) STRING		! In	Holds Value
      CHARACTER*6 XS,YS			! Out	Strings with X,Y values
      INTEGER SKIPNEXT			! 	No fields to skip if not constraint
 
*  Global Variables
      INCLUDE 'com_form_latex.inc'
 
*  Function
*-
 
*  Local Variables
      INTEGER NPAGE
      CHARACTER*19 FIELDNAME
 
*  Ececutable Code
 
      IF (STRING(1:1) .EQ. 'T' .OR. STRING(1:1) .EQ. 'Y' ) THEN
         NPAGE = 4
         SKIPNEXT = 0
      ELSE
         NPAGE = 3
         IF (FIELD(1:1) .EQ. '$' ) THEN
            FIELDNAME = FIELD (2:)
         ELSE
            FIELDNAME = FIELD
         END IF
         IF (FIELD .EQ. 'COORD.OBSERVATION' ) THEN
            SKIPNEXT = 10
         ELSE IF (FIELD .EQ. 'MONITOR' ) THEN
            SKIPNEXT = 1
         ELSE IF (FIELD .EQ. 'CONTIGUOUS.OBS' ) THEN
            SKIPNEXT = 1
         ELSE 
            SKIPNEXT = 2
         END IF
      END IF
 
      XS = XSTART(IFIELD, NPAGE, IFILE)
      YS = YSTART(IFIELD, NPAGE, IFILE)
      STRING(:1) = 'X'
 
      END
