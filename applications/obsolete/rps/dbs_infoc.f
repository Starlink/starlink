*+DBS_INFOC      Returns the data provided by DBS_OPEN
*-	DATE		AUTHOR			DESCRIPTION
*-	6th April 1987	MDC Harris (RAL)	original
*- 	8th April 1992	M. Duesterhaus (GSFC)	removed VAX RTL calls
***********************************************************************
      CHARACTER*(*) FUNCTION DBS_INFOC( REF_NO , NUMBER , NAME )

*INPUT:

      CHARACTER*(*) NAME	! Name of variable to be returned.
      INTEGER       NUMBER	! Field number.
     & ,            REF_NO	! Reference number of data set.

*LOCAL:

      CHARACTER*20  ASCENDING	! Ascending or descending.
      CHARACTER*9   UPPER	! Upper case version of name.
      INTEGER       I		! Loop variable.
     & ,            P1, P2, P3	! String position indicators.

*  Common
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_chars.inc'
      INCLUDE 'com_dbs_field.inc'


      UPPER = NAME
      CALL UPC (UPPER)
      DBS_INFOC = ' '								! Set return value to blank.

      IF ( UPPER .EQ. 'FIELDNAME' ) THEN						! If fieldname required then.

        DBS_INFOC = FIELD( NUMBER , REF_NO )		     			!  Return it.

      ELSE IF ( UPPER .EQ. 'FORMAT' ) THEN					! Else if format in record required then.

        DBS_INFOC = FORMAT( NUMBER , REF_NO )					!  Return it.

      ELSE IF ( UPPER .EQ. 'NULFORMAT' ) THEN					! Else return the null format.

        DBS_INFOC = NULFORMAT( NUMBER , REF_NO )

      ELSE IF ( UPPER .EQ. 'TYPE' ) THEN						! SAME FOR THE REST OF THE PARAMETERS.

        IF ( FORMAT( NUMBER , REF_NO )(1:3) .EQ. 'R*8' ) THEN

          DBS_INFOC = 'D'

        ELSE

          DBS_INFOC = FORMAT( NUMBER , REF_NO )(:1)

        END IF

      ELSE IF ( UPPER .EQ. 'UNITS' ) THEN

        DBS_INFOC = UNIT( NUMBER , REF_NO )

      ELSE IF ( UPPER .EQ. 'ORDER' ) THEN

        DBS_INFOC = ORDER( REF_NO )

      ELSE IF ( UPPER .EQ. 'NULVALUE' ) THEN

        DBS_INFOC = NULVALUE( NUMBER , REF_NO )

      ELSE IF ( UPPER .EQ. 'KEYFIELD' ) THEN

        DBS_INFOC = KEYFIELD( NUMBER , REF_NO )

      ELSE IF ( UPPER .EQ. 'KEYFIELDS' ) THEN					! If list of keyfields required.

        P1 = 1									!  Set position pointer.

        DO I = 1 , NKEYS( REF_NO )						!  Do for each keyfield.

          P2 = INDEX( KEYFIELD ( I , REF_NO ) , ' ' )				!   FInd end of keyfield.
          P3 = P1 + P2								!   Get present end of list.
          DBS_INFOC(P1:P3) = KEYFIELD( I , REF_NO )(1:P2) // ','			!   Add keyfield to list.
          P1 = P3 + 1								!   Get new start of list.

        END DO									!  End do.

        DBS_INFOC( P3: ) = ' '						!  Return list of keyfield.

      ELSE IF ( UPPER .EQ. 'ASCEND' ) THEN

        DBS_INFOC = ASCEND( NUMBER , REF_NO )

      ELSE IF ( UPPER .EQ. 'ASCENDING' ) THEN					!  As for keyfields in method.

        P1 = 1

        DO I = 1 , NKEYS( REF_NO )

          P3 = P1 + 1
          ASCENDING(P1:P3) = ASCEND( I , REF_NO )
          P3 = P3 + 1
          ASCENDING(P3:P3) = ','
          P1 = P3 + 1

        END DO

        DBS_INFOC = ASCENDING(:P3-1)

      ELSE IF ( UPPER .EQ. 'CRITERION' ) THEN

        DBS_INFOC = CRITERION( REF_NO )

      ELSE IF (UPPER .EQ. 'FILENAME' ) THEN

        DBS_INFOC = FILENAME( REF_NO )

      ELSE

         WRITE(9,'(A)') ' DBS_INFOC Unknown name:'//UPPER

      END IF

      END

