*  Aug 1992	M. Duesterhaus	Remove VAX specific calls
******************************************************************
      CHARACTER*(*) FUNCTION MDH_RTOC( RNUM )

      CHARACTER*15 DUMMY
      INTEGER      I
      REAL         RNUM

      INTEGER FIND_NOT_BLANK
     & ,      MDH_ENDWORD

      WRITE( DUMMY , '( F )' ) RNUM
      I = MDH_ENDWORD( DUMMY )
      
      DO WHILE ( DUMMY( I:I ) .EQ. '0' )

        I = I - 1

      END DO

      MDH_RTOC = DUMMY( FIND_NOT_BLANK( DUMMY ):I+1 )

      END
