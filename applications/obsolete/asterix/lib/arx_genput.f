*+ ARX_GENPUT - Convert shape and parameters to ARD text
      SUBROUTINE ARX_GENPUT(ARDID,INDEX,EXCLUDE,SHAPE,NPAR,PAR,STATUS)
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER ARDID
      INTEGER INDEX
      LOGICAL EXCLUDE
      CHARACTER*(*) SHAPE
      INTEGER NPAR
      REAL PAR(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Functions:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER*80 STRING        ! String to write to ARD file
      INTEGER I
      INTEGER L
      INTEGER LINDEX
*-

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      LINDEX=INDEX

*  include or exclude
      IF (EXCLUDE) THEN
         STRING = ' .NOT.('
         L=8
      ELSE
         STRING = ' '
         L=1
      ENDIF

*  add shape to string
      STRING(L:)=SHAPE
      L=CHR_LEN(STRING)
      L=L+1
      STRING(L:L)='('
      L=L+1

*  now add parameters
      DO I=1,NPAR

        CALL MSG_SETR('X',PAR(I))
        CALL MSG_MAKE(STRING(:L)//' ^X ,',STRING,L)

        IF (L.GE.65.AND.I.LT.NPAR) THEN
          CALL ARX_PUT(ARDID,LINDEX,STRING(:L),STATUS)
          STRING=' '
          L=1
          IF (LINDEX.GT.0) THEN
            LINDEX=LINDEX+1
          ENDIF
        ENDIF

      ENDDO

      IF (EXCLUDE) THEN
        STRING(L:)='))'
        L=L+1
      ELSE
        STRING(L:L)=')'
      ENDIF

      CALL ARX_PUT(ARDID,LINDEX,STRING(:L),STATUS)

      IF (STATUS.NE.SAI__OK) THEN
        CALL ERR_REP(' ','from ARX_GENPUT',STATUS)
      ENDIF

      END
