*+
      SUBROUTINE RED4_WRITEB( DIM, ARRAY )
*
*   Debug routine for writing the contents of a BYTE array.
*   This is useful for examining the contents of an array
*   when only an address is known at the top level. Use
*
*   CALL RED4_WRITEB( DIM, %val(ADDRESS) )
*
*   2-D arrays may be examined by giving DIM = DIM1 * DIM2.
*
*   1989:        Original version.                          (JFL)
*   26-Apr-1990: Brief description and comments added.
*                Modified to examine an array of any size.
*                Modified into generic routine.             (SMB)
*
      INTEGER DIM                 ! Size of array
      BYTE ARRAY( DIM )         ! Array to be examined
      INTEGER I                   ! Loop variable
*-
      DO I = 1, DIM

         WRITE(*,*) I, ARRAY( I )
      END DO

      END
*+
      SUBROUTINE RED4_WRITED( DIM, ARRAY )
*
*   Debug routine for writing the contents of a DOUBLE PRECISION array.
*   This is useful for examining the contents of an array
*   when only an address is known at the top level. Use
*
*   CALL RED4_WRITED( DIM, %val(ADDRESS) )
*
*   2-D arrays may be examined by giving DIM = DIM1 * DIM2.
*
*   1989:        Original version.                          (JFL)
*   26-Apr-1990: Brief description and comments added.
*                Modified to examine an array of any size.
*                Modified into generic routine.             (SMB)
*
      INTEGER DIM                 ! Size of array
      DOUBLE PRECISION ARRAY( DIM )         ! Array to be examined
      INTEGER I                   ! Loop variable
*-
      DO I = 1, DIM

         WRITE(*,*) I, ARRAY( I )
      END DO

      END
*+
      SUBROUTINE RED4_WRITEI( DIM, ARRAY )
*
*   Debug routine for writing the contents of a INTEGER array.
*   This is useful for examining the contents of an array
*   when only an address is known at the top level. Use
*
*   CALL RED4_WRITEI( DIM, %val(ADDRESS) )
*
*   2-D arrays may be examined by giving DIM = DIM1 * DIM2.
*
*   1989:        Original version.                          (JFL)
*   26-Apr-1990: Brief description and comments added.
*                Modified to examine an array of any size.
*                Modified into generic routine.             (SMB)
*
      INTEGER DIM                 ! Size of array
      INTEGER ARRAY( DIM )         ! Array to be examined
      INTEGER I                   ! Loop variable
*-
      DO I = 1, DIM

         WRITE(*,*) I, ARRAY( I )
      END DO

      END
*+
      SUBROUTINE RED4_WRITER( DIM, ARRAY )
*
*   Debug routine for writing the contents of a REAL array.
*   This is useful for examining the contents of an array
*   when only an address is known at the top level. Use
*
*   CALL RED4_WRITER( DIM, %val(ADDRESS) )
*
*   2-D arrays may be examined by giving DIM = DIM1 * DIM2.
*
*   1989:        Original version.                          (JFL)
*   26-Apr-1990: Brief description and comments added.
*                Modified to examine an array of any size.
*                Modified into generic routine.             (SMB)
*
      INTEGER DIM                 ! Size of array
      REAL ARRAY( DIM )         ! Array to be examined
      INTEGER I                   ! Loop variable
*-
      DO I = 1, DIM

         WRITE(*,*) I, ARRAY( I )
      END DO

      END
*+
      SUBROUTINE RED4_WRITEW( DIM, ARRAY )
*
*   Debug routine for writing the contents of a WORD array.
*   This is useful for examining the contents of an array
*   when only an address is known at the top level. Use
*
*   CALL RED4_WRITEW( DIM, %val(ADDRESS) )
*
*   2-D arrays may be examined by giving DIM = DIM1 * DIM2.
*
*   1989:        Original version.                          (JFL)
*   26-Apr-1990: Brief description and comments added.
*                Modified to examine an array of any size.
*                Modified into generic routine.             (SMB)
*
      INTEGER DIM                 ! Size of array
      INTEGER*2 ARRAY( DIM )         ! Array to be examined
      INTEGER I                   ! Loop variable
*-
      DO I = 1, DIM

         WRITE(*,*) I, ARRAY( I )
      END DO

      END
