*+DBS_FIELDNO    Looks for match to whole or partial field name in file
*-  Author M.D.C.Harris ( R.A.L )                   27th March 1987.
*	9 Apr 1992	M. Duesterhaus (GSFC)	PORT TO UNIX
**********************************************************************
      INTEGER FUNCTION DBS_FIELDNO( REF_NO , FLD )

*INPUT:

      CHARACTER*(*) FLD       	! Whole or partial field name to match.
      INTEGER       REF_NO	! Reference number of files.

*LOCAL:

      CHARACTER*17 TEST		! Field to test against.
      INTEGER      N		! Number of fields.
      LOGICAL      FOUND	! Indicates if field found.

*  -------------
*  COMMON BLOCKS
*  -------------

      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_field.inc'

*  ------------------------------------
*  FUNCTIONS AND SUBROUTINES REFERENCED
*  ------------------------------------

      LOGICAL       MDH_WILD


      DBS_FIELDNO = 0								! Set return value at 1.
      N = NFIELDS( REF_NO )
      FOUND = .FALSE.

      DO WHILE ( DBS_FIELDNO .LT. N .AND. .NOT. FOUND )

        DBS_FIELDNO = DBS_FIELDNO + 1						!  Increase return value by 1.
        TEST = FIELD( DBS_FIELDNO , REF_NO )
        IF ( INDEX( TEST , '$' ) .EQ. 1 ) TEST = TEST( 2: )
        FOUND = MDH_WILD( TEST , FLD )

      END DO									! End of loop.

      IF ( .NOT. FOUND ) THEN
         DBS_FIELDNO = -4							! If no match found then return error.
c         WRITE(9,*) ' DBS_FIELDNO can''t find ', FLD, ' in file ',REF_NO
      END IF

      END									! Return field number.
