*+DBS_INFOI      Returns the integer information available on the database.
*-	DATE		AUTHOR			DESCRIPTION
*-	6 Apr 1987	MDC Harris (RAL)	original
*-  	8 Apr 1992	M. Duesterhaus (GSFC)	remove VAX RTL stuff
***************************************************************************
      INTEGER FUNCTION DBS_INFOI( REF_NO , NUMBER , NAME )

*INPUT:

      CHARACTER*(*) NAME    	! Name of value to be returned.
      INTEGER       NUMBER  	! Actual field number.
     & ,            REF_NO	! Reference number of data set.

*LOCAL:

      CHARACTER*11  UPPER	! Upper case version of NAME.
      INTEGER       PL1 , PL2 	! Markers for character manipulation.
      INTEGER	    SUBINDEX	! Needed for call to find_first
*  ------------------------------------
*  FUNCTIONS AND SUBROUTINES REFERENCED
*  ------------------------------------

      INTEGER  FIND_FIRST	 !  Finds first example of one character 
*                                    !  from one string in another.
 
*  Global Variables
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_field.inc'
      INCLUDE 'com_dbs_bytes.inc'
      INCLUDE 'com_dbs_iof.inc'

                                                             
      IF ( REF_NO .LE. 0 .OR. ARRAY( REF_NO ) .LE. 0 ) THEN			! If reference number not OK then.

        DBS_INFOI = -11								!  Return error number.

      ELSE IF ( NUMBER .LE. NFIELDS( REF_NO ) ) THEN				! Else if number of field not too large then.

	 UPPER=NAME
	 CALL UPC (UPPER)

        IF ( UPPER .EQ. 'START' ) THEN						!  Position of first byte of field -

          DBS_INFOI = START( NUMBER , REF_NO )					!   to be returned.

        ELSE IF ( UPPER .EQ. 'UNIT NUMBER' ) THEN				!  Logical unit number of the main file -

          DBS_INFOI = LNDB( REF_NO )						!   to be returned.

        ELSE IF ( UPPER .EQ. 'LENGTH' ) THEN					!  Number of bytes in the field -

          DBS_INFOI = LENTH( NUMBER , REF_NO )					!   to be returned.

        ELSE IF ( UPPER .EQ. 'NULLENGTH' ) THEN					!  Length of the field variable in digits needed.

        PL1=FIND_FIRST(NULFORMAT(NUMBER,REF_NO),'1234567890',SUBINDEX)		!   Get position of first integer character.
          PL2 = FIND_FIRST						!   Get position of last integer character in -
     &           (NULFORMAT(NUMBER,REF_NO),' .',SUBINDEX )			!   substring.
          PL2 = PL2  -1							!   Get actual position of last integer character.
          READ ( NULFORMAT( NUMBER , REF_NO )( PL1:PL2 ) , 
     &  	'( I )' ) DBS_INFOI

        ELSE IF ( UPPER .EQ. 'CONDITIONAL') THEN				!  Whether read dependant on a previous entry

          DBS_INFOI = COND_OFFSET( NUMBER, REF_NO )				!  if non-zero indicates offset

        ELSE IF ( UPPER .EQ. 'NFIELDS' ) THEN					!  Number of fields in each record -

          DBS_INFOI = NFIELDS( REF_NO )						!   to be returned.

        ELSE IF ( UPPER .EQ. 'NRECORDS' ) THEN					!  Number of records in the database -

          DBS_INFOI = NRECORDS( REF_NO )					!   to be returned.

        ELSE IF ( UPPER .EQ. 'RECORDSIZE' .OR. 
     &            UPPER .EQ. 'RECSIZE' ) THEN					!  Number of bytes in each record -

          DBS_INFOI = RECSIZE( REF_NO )						!   to be returned.

        ELSE IF ( UPPER .EQ. 'NKEYS' ) THEN					!  Number of keyfields -

          DBS_INFOI = NKEYS( REF_NO )						!   to be returned.

        ELSE									!  Let programmer know he needs a better grasp of -

          DBS_INFOI = -9							!   spelling or how this subroutine works.

        END IF									!  End if.

      ELSE									! Else if error.

        DBS_INFOI = -10								!  Tell user field number too great.

      END IF									!  End if.

      END									! End.
                                              
