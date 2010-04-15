*+DBS_PUTC       Puts a character value into an unformatted file
*-  Author M.D.C.Harris ( R.A.L )                    20th March 1987.
*	9 Apr 1992	M. Duesterhaus (GSFC)	port to unix
************************************************************************

      SUBROUTINE DBS_PUTC( REF_NO , FIELDNO , VALUE , IERR )

*INPUT:

      CHARACTER*(*) VALUE	! Value to be put into record.
      INTEGER       FIELDNO   	! Number of field.
     & ,            REF_NO	! Reference number of database.

*OUTPUT:

      INTEGER       IERR	! Error message.

*LOCAL:

      CHARACTER*60  COLD	! Temporary storage variable.
      CHARACTER*60  BLANK
      CHARACTER*6   FMT		! Format of returned data.
      CHARACTER*1   TYPE        ! Type of storage.
      INTEGER       I		! Loop variable.
      LOGICAL       OK		! Error indicator.

*  ------------------------------------
*  FUNCTIONS AND SUBROUTINES REFERENCED
*  ------------------------------------

*     [MDH.DBS] DBS_PUTI	! Puts integer data into a record.
*    & ,        DBS_PUTL	! Puts logical data into a record.
*    & ,        DBS_PUTD	! Puts double precision data into a record.
*    & ,        DBS_PUTR	! Puts real data into a record.
*     [MDH]     MDH_CHECK	! Checks formatting of variable.



      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_dbs_field.inc'
      INCLUDE 'com_dbs_rec.inc'

      DO I=1,60
        BLANK(I:I) = ' '
      END DO

      FMT = NULFORMAT( FIELDNO , REF_NO )					! Get the format of the data.
      CALL MDH_CHECK( VALUE , FMT , COLD , OK )					! Convert it to the correct format if possible.

      IF (COLD.EQ.' ') THEN
         COLD = BLANK(1:LENTH(FIELDNO, REF_NO))
      END IF
      IF ( OK ) THEN								! If converting is possible then.

        IERR = 0
        TYPE = FORMAT( FIELDNO , REF_NO )( :1 )					!  Get type of variable.


          I = START(FIELDNO,REF_NO) +  LENTH( FIELDNO , REF_NO )-1 					!   For each byte of field.

            RECORD( REF_NO) (START( FIELDNO , REF_NO ) : I) = COLD 	!    Transfer each byte to equivalence variable.

      END IF
      END									! End.
