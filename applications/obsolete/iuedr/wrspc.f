      SUBROUTINE WRSPC(NAME, STATUS)

*+
*
*   Name:
*      SUBROUTINE WRSPC
*
*   Description:
*      This writes the spectrum part of the dataset to the file NAME.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          22-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     15-JUL-94     IUEDR Vn. 3.1-1
*         Fixed write_ndf call, added status check
*
*   Method:
*      Use Fortran I/O.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'
      INCLUDE 'CHR_ERR'

*   Import:
      CHARACTER*(*) NAME ! file to write?

*   Export:
      INTEGER STATUS     ! status return

*   Global variables:
      INCLUDE 'CMSAVE'

*   Local variables:
      REAL*8  RDUM(100)  ! dummy for write_ndf call

      CHARACTER*80 TITLE ! title for write_ndf call

      INTEGER I          ! loop index
      INTEGER NLEN       ! length of NAME
      INTEGER NMAX

      INTEGER CHR_LEN

*   Check inherited global status
      IF (STATUS .NE. SAI__OK) RETURN

      IF (.NOT.NOSPEC) THEN
         IF (NORDER.GT.0) THEN
            NMAX = 1
            DO I = 1, NORDER
              IF (NWAVS(I) .GT. NMAX) NMAX = NWAVS(I)
            END DO
            NLEN = CHR_LEN(NAME)
            IF (NLEN .GT. 80) NLEN = 80
            TITLE = NAME(1:NLEN)
            CALL WRITE_NDF('SPECTRUM', NAME, NMAX,
     :                       RDUM, RDUM,
     :                       'Wavelength', 'Flux', TITLE,
     :                       NORDER, STATUS)
         END IF
      END IF
      END
