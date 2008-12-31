*+DBS_RECORDS    Does ???
*	DATE		AUTHOR			DESCRIPTION
*	???		RAL			ORIGINAL
*	9 APR 1992	M. DUESTERHAUS (GSFC)	PORT TO UNIX
****************************************************************
      INTEGER FUNCTION DBS_RECORDS( REF_NO )
 
*  Calling Argument
      INTEGER REF_NO
 
*  Global Variables
      INCLUDE 'aaa_dbs_params.inc'
      INCLUDE 'com_dbs_iof.inc'

*  Local Variables
      INTEGER TOP,      MIDDLE,      BOTTOM,      IERR
 
*-
      TOP = NRECORDS( REF_NO )
      READ( LNDB( REF_NO ) , REC = TOP , IOSTAT = IERR )
                   
      IF ( IERR .EQ. 0 ) THEN

        BOTTOM = TOP
        READ( LNDB( REF_NO ) , REC = TOP + 1 , IOSTAT = IERR )

        DO WHILE ( IERR .EQ. 0 )

          BOTTOM = TOP
          TOP = TOP + TOP
          READ( LNDB( REF_NO ) , REC = TOP , IOSTAT = IERR )

        END DO

      ELSE

        BOTTOM = 1                                            
        READ( LNDB( REF_NO ) , REC = 1 , IOSTAT = IERR )
        IF ( IERR .NE. 0 ) TOP = 0

      END IF

      DO WHILE ( TOP - BOTTOM .GT. 1 )

        MIDDLE = ( TOP + BOTTOM ) * 0.5
        READ( LNDB( REF_NO ) , REC = MIDDLE , IOSTAT = IERR )

        IF ( IERR .EQ. 0 ) THEN

          BOTTOM = MIDDLE

        ELSE

          TOP = MIDDLE

        END IF

      END DO

      DBS_RECORDS = BOTTOM
      END
