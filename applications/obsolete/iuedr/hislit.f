      SUBROUTINE HISLIT( STATUS )

*+
*
*   Name:
*      SUBROUTINE HISLIT
*
*   Description:
*      Define object and background channels.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*      Martin Clayton     05-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*      The object and background channel widths and positions are based
*      on the GSLIT, BDIST and BSLIT parameter values.
*      The EXTENDED and CONTINUUM parameters are used to determine
*      how (if at all) these parameters must be constrained to produce
*      the channels to be used.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Export:
      INTEGER STATUS        ! status return

*   External references:
      LOGICAL STR_SIMLR     ! caseless string equality

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDISH'
      INCLUDE 'CMEXTP'

*   Local variables:
      REAL*8 RGAP           ! gap from order to inter-order

      INTEGER IBKG          ! background channel index

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Set EXTP undefined
      NOEXTP = .TRUE.

*   Fix number of backgrounds for now
      NBKG = 2

*   Distance to inter-order
      RGAP = DRDM / 2.0

*   Automatic determination of GSLIT
      IF ( AUSLIT ) THEN
         IF ( EXTND .AND. STR_SIMLR( 'LAP\\', APERS(1, 1) ) ) THEN
            GSLIT(2) = 6.0

         ELSE
            IF (CORD.GE.68) THEN
               GSLIT(2) = (CORD - 68) * (2.5 - 3.5) /
     :                    DBLE(125 - 68) + 3.5

            ELSE
               GSLIT(2) = (CORD - 66) * (3.5 - 5.0) /
     :                    DBLE(68 - 66) + 5.0
            END IF
         END IF

         GSLIT(1) = -GSLIT(2)
         BSLIT(1) = 0.5
         BSLIT(2) = 0.5
         BDIST(2) = RGAP
         BDIST(1) = -BDIST(2)

      END IF

*   Extended with continuum
      IF ( EXTND .AND. CONTN ) THEN
         ROBJ(1) = GSLIT(1)
         ROBJ(2) = GSLIT(2)
         BDIST(1) = -RGAP
         BDIST(2) = RGAP

         DO IBKG = 1, NBKG
            RBKG(1, IBKG) = BDIST(IBKG) - BSLIT(IBKG)
            RBKG(2, IBKG) = BDIST(IBKG) + BSLIT(IBKG)
         END DO

*   Extended without continuum
      ELSE IF ( EXTND ) THEN
         ROBJ(1) = GSLIT(1)
         ROBJ(2) = GSLIT(2)

         DO IBKG = 1, NBKG
            RBKG(1, IBKG) = BDIST(IBKG) - BSLIT(IBKG)
            RBKG(2, IBKG) = BDIST(IBKG) + BSLIT(IBKG)
         END DO

*   Point source with Continuum
      ELSE
         BDIST(1) = -RGAP
         BDIST(2) = +RGAP

         DO IBKG = 1, NBKG
            BSLIT(IBKG) = 0.5
            RBKG(1, IBKG) = BDIST(IBKG) - BSLIT(IBKG)
            RBKG(2, IBKG) = BDIST(IBKG) + BSLIT(IBKG)
         END DO

         ROBJ(1) = MAX(RBKG(2, 1), GSLIT(1))
         ROBJ(2) = MIN(RBKG(1, 2), GSLIT(2))
      END IF

*   Set EXTP defined
      NOEXTP = .FALSE.

      END
