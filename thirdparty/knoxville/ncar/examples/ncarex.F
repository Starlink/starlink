      PROGRAM NCAREX
*+
*  Name:
*     NCAREX

*  Purpose:
*     Test program for NCAR demonstration subroutines.

*  Language:
*     Starlink Fortran 77

*  Description:
*     Edit 'CALL GOPWK' for GKS connection id (5) and workstation (201).
*     Edit 'CALL TAUTOG' for demonstration subroutine T*.
*     IUNIT is for TISOHR which requires a scratch file.
*     Link with NCAR and GKS.

*  Authors:
*     PWH: P.W. Hill (St Andrews)
*     PCTR: P.C.T. Rees (STARLINK)

*  History:
*     3-SEP-1991 (PXWH):
*        Original version.
*     11-DEC-1991 (PCTR):
*        Added workstation prompt.
*        Use UNIT 0 for errors.

*  Bugs:

*-

*  Type Declarations:
      IMPLICIT NONE

*  Global Variables:
      INTEGER IUNIT
      COMMON / UNITS / IUNIT

*  Local Constants:
      INTEGER ICONID
      PARAMETER ( ICONID = 5 )

      INTEGER IWKID
      PARAMETER ( IWKID = 1 )

      INTEGER KERRFL
      PARAMETER ( KERRFL = 0 )

*  Local Variables:
      INTEGER IERR
      INTEGER IOSTAT
      INTEGER IWKSTN

*  Local Data:
      DATA IUNIT / 23 /

*  Open GKS.
      CALL GOPKS( KERRFL, -1 )

*  Prompt for and read the GKS worstation number.
 10   CONTINUE
      WRITE( *, '( ''Enter workstation number: '' )', IOSTAT=IOSTAT )

      IF ( IOSTAT .EQ. 0 ) THEN
         READ( *, *, IOSTAT=IOSTAT ) IWKSTN

         IF ( IOSTAT .EQ. -1 ) THEN
            WRITE( *, '( ''Exit test.'' )', IOSTAT=IOSTAT )
            GO TO 999
         ELSE IF ( IOSTAT .NE. 0 ) THEN
            WRITE( *, '( ''Error: read error.'' )', IOSTAT=IOSTAT )
            GO TO 10
         END IF
      ELSE
         WRITE( *, '( ''Error: unable to write to standard output.'' )', 
     :   IOSTAT=IOSTAT )
         GO TO 999
      END IF

      CALL GOPWK( IWKID, ICONID, IWKSTN )
      CALL GACWK( IWKID )

*  NCAR example to be run.
#if USE_TAUTOG
      CALL TAUTOG( IERR )
#else
#error "Must define correct example CFLAG switch"
#endif

*  Close the GKS workstation.
      CALL GDAWK( IWKID )
      CALL GCLWK( IWKID )
      CALL GCLKS

      CLOSE ( IUNIT, STATUS='DELETE' )

 999  CONTINUE

      END
