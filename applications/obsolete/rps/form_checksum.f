*+FORM_CHECKSUM    Gets checksum , puts in record
*	DATE		AUTHOR			DESCRIPTION
*	???		RAL			ORIGINAL
*	9 APR 1992	M. DUESTERHAUS (GSFC)	MODIFIED FOR PORTING
*     1993 June         P. Brisco       Recompile with new com_form_files.inc
*	10 Oct 1993	P. Brisco	Commented out BSUM output statement
*     1994 Feb		M Ricketts	RAL version
*****************************************************************
      SUBROUTINE FORM_CHECKSUM(REFPF,KTARGET)
      IMPLICIT NONE

*  Calling Arguments
      INTEGER REFPF		! In	Reference no. of file
      INTEGER KTARGET		!	Target no. or 0 for cover

*  Global Variables
      INCLUDE 'com_form_files.inc'
      INCLUDE 'com_form_qual.inc'
      INCLUDE 'aaa_dbs_params.inc'	! Needs DBSLIB in same directory
      INCLUDE 'com_dbs_rec.inc'
      INCLUDE 'com_dbs_iof.inc'

*  Functions
      CHARACTER*6  MDH_ITOC
      INTEGER DBS_FIELDNO, DBS_INFOI


*  Local Variables
      INTEGER ISUM
      CHARACTER*6 BSUM
      INTEGER LASTBYTE, I, FLD_NTARG, IADD

* ______________________ Executable Code ________________________

      IF (REFPF .EQ. REF_FORM) THEN
          FLD_NTARG = DBS_FIELDNO(REF_FORM,'NUMBER.OF.TARGETS')
           LASTBYTE = DBS_INFOI( REF_FORM, FLD_NTARG, 'START') +
     &                DBS_INFOI( REF_FORM, FLD_NTARG, 'LENGTH') - 1
      ELSE
         LASTBYTE = RECSIZE(REFPF) - 5
      ENDIF

      IF (REFPF .EQ. REF_FORM) THEN				! Cover
         IF (.NOT.QUAL_COVER) GOTO 10
         IF (.NOT.QUAL_GEN) GOTO 10
      ELSE
         IF (.NOT.QUAL_TARGET(KTARGET) ) GOTO 10
      END IF

      ISUM = 0
      DO I=1, LASTBYTE
         IADD = ICHAR( RECORD(REFPF)(I:I) )
         ISUM = ISUM + IADD
      END DO
      GOTO 20

 10   CONTINUE					! Failed con-check so put '-1' in
      ISUM = -1000
      GOTO 30

 20   CONTINUE
      LASTBYTE = RECSIZE(REFPF) - 5

 30   BSUM = MDH_ITOC (ISUM)
      RECORD(REFPF) (LASTBYTE:LASTBYTE+5)= BSUM

      END
