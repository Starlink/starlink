      SUBROUTINE TCONV(K,ARG,N,ISIGN,ID,IM,IS,FS, STATUS )
*+
*   To Convert Radians of Arc or Time to Sexagesimal
*
*   Gets
*   ----
*     K    = 1 if Arc, 2 if Time
*     ARG  = Input Measure in Radians
*     N    = Number of Decimals to which Seconds to be Rounded
*
*   Returns
*   -------
*     ISIGN= Sign of Argument (CHARACTER*1)
*     ID   = Integer Degrees( or Hours )
*     IM   = Integer Minutes
*     IS   = Integer Seconds (Unit Corresponding to N)
*     FS   = Real Seconds

*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   Based on Routine in Program A06E by W. Nicholson
*   ammended by MJV 16-2-83

*   History:
*     4-MAR-1993 (AJJB):
*       STATUS argument added.
*

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      DOUBLE PRECISION ARG,ASECS,TEM,TWOPI,HALFPI,RDSA,RDST,RDDG
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG
      CHARACTER*1 ISIGN

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (K.EQ.1) ASECS = ARG/RDSA
      IF (K.EQ.2) ASECS = ARG/RDST
      FAC = 10**N
      MAX = INT( 60*FAC )
      TEM = ABS(ASECS)
      FS  = SNGL(DMOD(TEM,6.00D1))
      IRG = NINT((TEM-FS)/60.0)
      IS  = NINT(FS*FAC)
      IF(IS.LT.MAX) GO TO 102
      FS  = 0.0
      IS  = 0
      IRG = IRG + 1
 102  CONTINUE
      IM  = MOD(IRG,60)
      ID  = IRG/60
      IF (ARG.GE.0.0) THEN
         ISIGN = '+'
      ELSE
         ISIGN = '-'
      ENDIF
      END
