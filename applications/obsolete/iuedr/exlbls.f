      SUBROUTINE EXLBLS( NSUB, DSUB, QSUB, RSUB, WSUB )

*+
*
*   Name:
*      SUBROUTINE EXLBLS
*
*   Description:
*      Extract LBLS from image subset list.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      The object LBLS is extracted from the image subset list provided.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NSUB             ! number of subset pixels

*   Import-Export:
      INTEGER*2 DSUB(NSUB)     ! DATA values

      BYTE QSUB(NSUB)          ! QUAL values

      REAL*8 RSUB(NSUB)          ! R-coordinates
      REAL*8 WSUB(NSUB)          ! W-coordinates

*   CMPAN:
      INCLUDE 'CMPAN'

*   Zero out centroid array or use template
      CALL SECEN

*   Extract LBLS array
      CALL EXPAN(NSUB, DSUB, QSUB, RSUB, WSUB, 120, 1200, FPAN, WPAN,
     :           QPAN)

      END
