      SUBROUTINE LOCATE(ARRAY,NX,NY,SMARGE,RO,SR,JR,LR,
     :                  TO,ST,JT,LT,ICROWDT,JFLAG)
*+
*   LOCATE
*
*     LOCATES AN IMAGE CENTRE VIA A MARGINAL DISTRIBUTION
*
*   Given      (arguments)
*   ARRAY   RA  image frame
*   NX      I   X-dimension of image frame
*   NY      I   Y-dimension of image frame
*   RO      R   coordinate of image centre in axis
*               orthogonal to required marginal
*   SR      R   the range for the marginal sums is
*               RO + or - 2*SR
*   JR      I   lowest acceptible value of start of marginal range
*   LR      I   highest acceptible value of start of marginal range
*   JFLAG   I   = 0 for X-marginal
*               = 1 for Y-marginal
*
*   Returned   (arguments)
*   SMARGE  RA  marginal vector
*   TO      R   estimate of image centre within marginal
*   ST      R   estimate of image width
*   JT      I   low-side local minimum of SMARGE + 1
*   LT      I   high-side local minimum of SMARGE - 1
*   ICROWDT I   = 0 if no other star detected
*               = -1 if brighter star found on low side
*               = +1 if brighter star found on high side
*
*   Subroutines called :
*   MARGIN,SEARCH      : E2DLIB
*
*	B.D KELLY/ROE/1981
*-
      REAL ARRAY(NX,NY),SMARGE( NX + NY )

      I=IFIX(RO-SR-SR+0.5)
      M=IFIX(RO+SR+SR+0.5)
      IF(I.LT.JR) I=JR
      IF(M.GT.LR) M=LR
      NS=NX
      IF(JFLAG.EQ.1) NS=NY

      CALL MARGIN(I,M,ARRAY,NX,NY,SMARGE,NS,JFLAG)
      CALL SEARCH(SMARGE,NS,5,TO,ST,JT,LT,ICROWDT)

      END
