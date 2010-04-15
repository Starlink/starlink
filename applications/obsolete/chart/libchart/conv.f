      SUBROUTINE CONV(K,ARG,N,ISIGN,ID,IM,IS,FS, STATUS )
*+
*     To convert radians of arc or time to sexagesimal
*
*   Gets
*   ----
*     K    = 1 if arc, 2 if time
*     ARG  = Input measure in radians
*     N    = Number of decimals to which seconds to be rounded
*
*   Returns
*   -------
*     ISIGN= Sign (Character) of argument
*     ID   = Integer Degrees( or hours )
*     IM   = Integer Minutes
*     IS   = Integer Seconds (Unit Corresponding to N)
*     FS   = Real Seconds
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*     5-MAR-1993 (AJJB):
*       STATUS argument added.
*     10-MAR-1993 (AJJB):
*       ISIGN (4th argument) changed to be of type Character rather
*       than a one element array of type integer ! All calling routines
*       changed accordingly.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

      CHARACTER IPLUS, MINUS, ISIGN
      DOUBLE PRECISION ARG,ASECS,TEM,TWOPI,HALFPI,RDSA,RDST,RDDG
      COMMON/CONVF/TWOPI,HALFPI,RDSA,RDST,RDDG
      DATA IPLUS,MINUS/'+','-'/

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
      IF (IS.LT.MAX) GO TO 102
      FS  = 0.0
      IS  = 0
      IRG = IRG + 1
 102  CONTINUE
      IM  = MOD(IRG,60)
      ID  = IRG/60
      ISIGN = IPLUS
      IF (ARG.LT.0.0) ISIGN = MINUS
      END
