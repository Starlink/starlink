*+  HDX_PUTC - Puts character variable/array on HDS file.
      SUBROUTINE HDX_PUTC(LOC, NAME, NELS, ARRAY, STATUS)
*
* Description :
*     <description of what the subroutine does - for user info>
* Method :
*     <description of how the subroutine works - for programmer info>
* Deficiencies :
*     <description of any deficiencies>
* Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     Clive Page (LTVAD::CGP)
*     Dick Willingale (LTVAD::RW)
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      9 Jul 86 : Original (CGP)
*     18 Sep 86 : Modified. Structured names (RW)
*      6 May 87 : Added error message on test for STATUS (CGP)
*     10 May 88 : ASTERIX version (LTVAD::RDS)
*     11 Jun 91 : Use CHR_ITOC (DJA)
*      1 Dec 93 : Correct mis-typed LOCNAM variable (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
	CHARACTER*(DAT__SZLOC) LOC   !locator to structure.
	CHARACTER*(*) NAME	     !name of variable or array.
	INTEGER NELS		     !no of elements, =1 for scalar.
	CHARACTER*(*) ARRAY(NELS)    !values to be written to HDS file.
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)	LOCNAM		!
      CHARACTER*(DAT__SZLOC)	LOCV		!
      CHARACTER*(DAT__SZTYP) 	TYPE

      INTEGER NDIMS,LENGTH,NTOT,NCC

      LOGICAL 			THERE
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Object already exists?
      NDIMS = MIN(1, NELS-1)
      LENGTH = LEN(ARRAY(1))
      CALL HDX_WHAT( LOC, NAME, THERE, TYPE, NTOT, STATUS )

*    Create if not already there
      IF ( (.NOT.THERE) .OR. (NTOT.NE.NELS) ) THEN
        TYPE = '_CHAR*'
        CALL CHR_ITOC( LENGTH, TYPE(7:), NCC )
        CALL HDX_CREATE( LOC, NAME, NDIMS, NELS, TYPE, LOCNAM, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
      ELSE
        CALL HDX_FIND( LOC, NAME, LOCNAM, STATUS )
      END IF

*    Vectorise
      IF ( NDIMS .EQ. 0 ) THEN
        CALL DAT_PUTC( LOCNAM, 0, 0, ARRAY, STATUS )
      ELSE
        CALL DAT_VEC( LOCNAM, LOCV, STATUS )
        CALL DAT_PUTC( LOCV, NDIMS, NELS, ARRAY, STATUS )
        CALL DAT_ANNUL( LOCV, STATUS )
      END IF
      CALL DAT_ANNUL( LOCNAM, STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', 'from HDX_PUTC', STATUS )
      END IF

      END
