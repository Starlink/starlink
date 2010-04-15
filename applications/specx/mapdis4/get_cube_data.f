*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*-----------------------------------------------------------------------

      SUBROUTINE GET_CUBE_DATA (J, K, LSPEC, BUF2, INTERP,
     &                          I1, I2, BUF1,
     &                          INDEX, M, N, SUCCESS)

      IMPLICIT   NONE

*     Formal parameters:

      INTEGER    J, K           ! (X,Y) location of spectrum
      INTEGER    LSPEC          ! Length of a spectrum
      REAL       BUF2(*)        ! Workspace
      LOGICAL    INTERP         ! Interpolate on demand
      INTEGER    I1, I2         ! First and last spectral elements to get
      REAL       BUF1(*)        ! Returned data
      INTEGER    M, N           ! Size of index array
      INTEGER    INDEX(M,N)     ! Index array
      LOGICAL    SUCCESS        ! OK, good data returned

*     Local variables

      LOGICAL    HIT_DATA       ! Found some data in beam, not used here.

*     Ok, go...

CD    PRINT *, ' -- get_cube_data --'
CD    PRINT *, '    j, k = ', j, k

      SUCCESS = .FALSE.

      IF (J.LE.0 .OR. J.GT.M .OR. K.LE.0 .OR. K.GT.N) RETURN

      IF (INTERP) THEN
        CALL READ_INTERP_DATA (LSPEC, J, K, BUF2, BUF1, I1, I2,
     &                         HIT_DATA, SUCCESS)
      ELSE
        CALL READ_NEW_CUBE2  (J, K, LSPEC, I1, I2, BUF1)
        SUCCESS = (INDEX(J,K) .GT. -1)
      END IF

      RETURN
      END

*-----------------------------------------------------------------------
