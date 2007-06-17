      SUBROUTINE STRRD(STRING)
C
C Subroutine to read a string
C
      CHARACTER STRING*(*)
      INTEGER IOS
!     CALL TALPHA
      READ (*,'(A)',IOSTAT=IOS) STRING
10000 IF ( IOS.NE.0 ) THEN
        PRINT *,'Error during read.  Please repeat.'
         WRITE (*,'(A)') 7
        READ (*,'(A)',IOSTAT=IOS) STRING
        GOTO 10000
      END IF
      END
