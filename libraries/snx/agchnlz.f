      SUBROUTINE AGCHNL (IAXS, VILS, CHRM, MCIM, NCIM,
     :                   IPXM, CHRE, MCIE, NCIE)

*+
*
*  - - - - - - -
*   A G C H N L
*  - - - - - - -
*
*  Inserts leading zeros into NCAR numeric axis labels
*
*  Given:
*     IAXS    i      axis number
*     VILS    r      value to be represented by the label
*     CHRM    c*(*)  mantissa string
*     MCIM    i      maximum length of mantissa string
*     NCIM    i      actual length of mantissa string
*     IPXM    i      pointer to "times" symbol if any
*     CHRE    c*(*)  exponent string
*     MCIE    i      maximum length of exponent string
*     NCIE    i      actual length of exponent string
*
*  Returned:
*     CHRM    c*(*)  mantissa string
*     NCIM    i      actual length of mantissa string
*     IPXM    i      pointer to "times" symbol if any
*     CHRE    c*(*)  exponent string
*     NCIE    i      actual length of exponent string
*
*  For detailed explanations see AUTOGRAPH write-up, section 3.26.
*
*  P T Wallace   Starlink   October 1986
*
*+

      IMPLICIT NONE

      INTEGER IAXS
      REAL VILS
      CHARACTER*(*) CHRM
      INTEGER MCIM,NCIM,IPXM
      CHARACTER*(*) CHRE
      INTEGER MCIE,NCIE

      INTEGER IDP,I
      CHARACTER K



*  Check mantissa string not full
      IF (NCIM.LT.MCIM) THEN

*     Look for decimal point in mantissa
         IDP = INDEX(CHRM(:NCIM),'.')

         IF (IDP.NE.0) THEN

*        Pick up preceding character if any
            IF (IDP.GT.1) THEN
               K = CHRM(IDP-1:IDP-1)
            ELSE
               K = ' '
            END IF

*        Make sure point not preceded by numerics
            IF (K.LT.'0'.OR.K.GT.'9') THEN

*           Move the mantissa along one space
               DO I=NCIM,IDP,-1
                  CHRM(I+1:I+1) = CHRM(I:I)
               END DO

*           Insert the zero
               CHRM(IDP:IDP) = '0'

*           Increment the mantissa length
               NCIM = NCIM+1

*           Increment the "times" index if present
               IF (IPXM.NE.0) IPXM = IPXM+1

            END IF

         END IF

      END IF

      END
