*+LTX_DECODE_DSCF  Decodes dscf Print info
      SUBROUTINE LTX_DECODE_DSCF(STRING,SIZE,IFIELD,IFILE)
      IMPLICIT NONE

*  Calling Arguments
      CHARACTER*(*) STRING		! Print instructions to be decoded
      INTEGER IFIELD			! Start field, this variable
      INTEGER SIZE			! Array size (if .gt. 1)
      INTEGER IFILE			! 1: Cover, 2: Target

*  Global Variables
      INCLUDE 'com_form_latex.inc'

*******************************************************************************
*  History
*     1988 October	M Ricketts	1st Version
*     1989 Jan		    ::		Mods for split form
*     1991 Oct				Add 5th page
*     1993 Sep		P. Brisco	The 'ALL' category was overwriting
*					specific choices.  This was corrected.
*******************************************************************************
*-
*  Functions
      INTEGER INDEX
      REAL MDH_CTOR

* Parameter
      INTEGER PAGE_MAX
      PARAMETER(PAGE_MAX=6)

*  Local Variables
      INTEGER P1, P2, PCOMMA, I, J, IPAGE, PMINUS, NF
      REAL RYSTART, RYDEC
      LOGICAL ARRAY_FILL, MORE_FIELDS
      CHARACTER*3 PAGES				! 'ONE' or 'ALL'
      CHARACTER*6 YST
      CHARACTER*22 FIELD

*  Executable Code
      DO J = 1,SIZE				! Clear print flags
         DO I=1,PAGE_MAX
            PR_ON(IFIELD+J-1,I,IFILE) = .FALSE.
         END DO
      END DO

      P1 = 1
      P2 = INDEX(STRING(P1:),':') - 1		! Get next field end
      IF (P2 - P1.GT.0) THEN
         MORE_FIELDS = .TRUE.
      ELSE
         MORE_FIELDS = .FALSE.
      END IF
c
c	Check to see if a default value is desired.
c
	IF (more_fields) THEN
		p1 = INDEX(string,'A')

		IF (p1 .GT. 0) THEN
			p2 = INDEX(string(p1:),':') + (p1 - 2)
			pages = 'ALL'
		 ELSE
			p1 = 1
			pages = 'ONE'
		ENDIF

	ENDIF

      DO WHILE (MORE_FIELDS)
         FIELD = STRING(P1:P2)

	IF (pages .EQ. 'ALL') THEN
		ipage = 1
		IF (P1 .GT. 1) THEN
  		  p2 = p1 - 1
		  p1 = 1
		  string = string(p1:p2)//'                '
		  p2 = INDEX(string(p1:),':') + (p1 - 2)
		ELSE
		  MORE_FIELDS = .FALSE.
		END IF
	 ELSE
		READ(field(:INDEX(field,'/') - 1),'(I)') ipage
	END IF

         PCOMMA = INDEX(FIELD,',')		! Separate x,y fields
         XSTART(IFIELD,IPAGE,IFILE) = FIELD(3:PCOMMA-1)
	PR_ON(IFIELD,ipage,IFILE)  = .TRUE.

         IF (SIZE.EQ.1) THEN
            YSTART(IFIELD,IPAGE,IFILE) = FIELD(PCOMMA+1:)

         ELSE
            PMINUS = INDEX(FIELD,'-')		! If present fill whole array
            IF (PMINUS .EQ. 0) THEN
               ARRAY_FILL = .FALSE.
               YSTART(IFIELD,IPAGE,IFILE) = FIELD(PCOMMA+1:)
            ELSE
               ARRAY_FILL = .TRUE.
               YST = FIELD(PCOMMA+1:PMINUS-1)
               YSTART(IFIELD,IPAGE,IFILE) = YST
               RYDEC   = MDH_CTOR(FIELD(PMINUS+1:))
               RYSTART = MDH_CTOR(YST)
            END IF

            IF (ARRAY_FILL) THEN		! Put in rest of start locations
               NF = IFIELD
               DO I=2,SIZE
                  NF = NF+1
                  XSTART(NF,IPAGE,IFILE) = XSTART(IFIELD,IPAGE,IFILE)
                  RYSTART = RYSTART - RYDEC
                  WRITE(YSTART(NF,IPAGE,IFILE), '(F6.2)') RYSTART
                  PR_ON(NF,ipage,IFILE) = .TRUE.
               END DO
            END IF
         END IF					! Filling array

         IF (PAGES.EQ. 'ALL') THEN		! Fill other pages
            DO I=1,PAGE_MAX
               PR_ON(IFIELD,I,IFILE)  = .TRUE.
               XSTART(IFIELD,I,IFILE) = XSTART(IFIELD,IPAGE,IFILE)
               YSTART(IFIELD,I,IFILE) = YSTART(IFIELD,IPAGE,IFILE)
            IF (ARRAY_FILL) THEN		! Put in rest of start locations
               NF = IFIELD
               DO J=2,SIZE
                  NF = NF+1
                  XSTART(NF,I,IFILE) = XSTART(IFIELD,IPAGE,IFILE)
                  WRITE(YSTART(NF,I,IFILE), '(F6.2)') RYSTART
                  PR_ON(NF,I,IFILE) = .TRUE.
               END DO
            END IF
            END DO
c            MORE_FIELDS = .FALSE.
         END IF

*     See if any more fields to decode
         IF (MORE_FIELDS .AND. pages .NE. 'ALL') THEN
            STRING = STRING(P2+2:)
            P1 = 1
            P2 = INDEX(STRING(P1:),':') - 1
            IF (P2-P1 .LE. 0 ) MORE_FIELDS = .FALSE.
         END IF

	pages = 'ONE'
      END DO					! While more fields

      IFIELD = IFIELD + SIZE

      END
