*+
      SUBROUTINE RED4_SHOWLOC( LOC )
*
*   Debug routine for displaying the contents of an HDS locator
*
*    26-Apr-1990: Original version.     (SMB)
*
      INTEGER SZLOC
      PARAMETER ( SZLOC = 15 )    ! Size of locator
      CHARACTER*( SZLOC ) LOC     ! Given locator
      INTEGER ILOC( SZLOC )       ! ASCII codes obtained from locator
      INTEGER I                   ! Loop index
*-

*   Extract the ASCII codes contained in the locator

      DO I = 1, SZLOC

         ILOC( I ) = ICHAR( LOC(I:I) )
      END DO

*   Display the ASCII codes

      WRITE(*,1) (ILOC(I),I=1,SZLOC)
 1    FORMAT( 1X, <SZLOC>I4 )

      END
