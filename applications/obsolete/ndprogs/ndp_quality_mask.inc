c
c Quality mask value as outlined in SGP38.
c
      BYTE       BADBITS
      PARAMETER  (BADBITS='FF'X)
************* 
* Bit masks *
*************
      BYTE       Q_GOOD
      PARAMETER  (Q_GOOD=0)
c
      BYTE       Q_BAD
      PARAMETER  (Q_BAD=1)
c
      BYTE       Q_MISSING
      PARAMETER  (Q_MISSING=2)
c   
      BYTE       Q_PATCHED
      PARAMETER  (Q_PATCHED=64)
c
      BYTE       Q_ARITH
      PARAMETER  (Q_ARITH=4)
c
      BYTE       Q_IGNORE
      PARAMETER  (Q_IGNORE=128)
      
