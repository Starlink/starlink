      SUBROUTINE RINGS(JX,LX,JY,LY,XO,YO,SIG,IMIN,
     :                 NX,NY,ARRAY,SUM,SUMRS,NSUM)
*+
*   RINGS
*
*   calculates average image profile in annuli for GAUFIT,
*   using the known position of the image centre and the limits
*   for a subarray determined by the previous routines.
*
*   Given      (arguments)
*   JX      I   minimum X-coordinate of subarray
*   LX      I   maximum X-coordinate of subarray
*   JY      I   minimum Y-coordinate of subarray
*   LY      I   maximum Y-coordinate of subarray
*   XO      R   X-coordinate of image centre
*   YO      R   Y-coordinate of image centre
*   SIG     R   average image sigma from marginal distribution
*   NX      I   X-dimension of image array
*   NY      I   Y-dimension of image array
*   ARRAY   RA  image array
*
*   Returned   (arguments)
*   IMIN    I   no. of annulus with innermost local minimum
*               beyond one-sigma point
*   SUM     RA  mean densities of annular rings
*   SUMRS   RA  sum of squares of radii to pixels in annuar rings
*   NSUM    IA  no. of pixels in annular rings
*
*   B.D.Kelly/ROE/6.5.1982
*-
* DIV,ALP: THE ANNULUS THAT CONTAINS A GIVEN PIXEL IS GIVEN BY
*      I=IFIX(R**2/(DIV+ALP*R**2))+1
*
      INTEGER IRMAX
      REAL DIV,ALP

      PARAMETER (DIV = 3.1831)
      PARAMETER (ALP = 0.04)

      INTEGER JX,LX,JY,LY,NX,NY
      REAL XO,YO,SIG
      REAL SUM(NX),SUMRS(NX)
      REAL ARRAY(NX,NY)
      INTEGER NSUM(NX)

      KVAL(Z,A)=IFIX((Z*Z+A*A)/(DIV+(Z*Z+A*A)*ALP))+1

      IRMAX=NX
      DO I=1,IRMAX
         SUM(I)=0.0
         SUMRS(I)=0.0
         NSUM(I)=0
      ENDDO
*
*   here the pixels are sorted into annuli and the densities and
*   square radii are added up and stored.
*
      DO I=JX,LX
         DX=I-XO
         DO J=JY,LY
            DY=J-YO
            K=KVAL(DX,DY)
            IF(K.GT.IRMAX)K=IRMAX
            SUM(K)=SUM(K)+ARRAY(I,J)
            SUMRS(K)=SUMRS(K)+DX*DX+DY*DY
            NSUM(K)=NSUM(K)+1
         ENDDO
      ENDDO
*
*   compute mean densities and mean square radii for annuli.
*
      MAXRAD=0
      DO I=1,IRMAX
         IF(NSUM(I).LT.1) THEN
            SUM(I)=0.0
         ELSE
            SUM(I)=SUM(I)/NSUM(I)
            SUMRS(I)=SUMRS(I)/NSUM(I)
            MAXRAD=I
         ENDIF
      ENDDO
*
*   find innermost local minimum (beyond the one-sigma point).
*
      I=KVAL(SIG,0.0)
      SMIN=SUM(I)-1.0
      IMIN=I
      DO WHILE((SMIN.NE.SUM(IMIN)).AND.(I.LE.MAXRAD))
         SMIN=SUM(I)
         DO L=1,MIN(MAXRAD-I,4)
            SMIN=MIN(SMIN,SUM(I+L))
         ENDDO
         IMIN=I
         I=I+1
      ENDDO

      END
