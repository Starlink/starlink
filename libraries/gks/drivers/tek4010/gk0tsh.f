      SUBROUTINE GK0TSH(JCHSZD)
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Part of workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To Set Character Height on devices where it is optional
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine.
*
*  ARGUMENTS
*  ---------
*     Inp  JCHSZD  Identifier of character size: 1 (smallest) to n (biggest)
*
      INTEGER JCHSZD
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     ITRIM  Trim the value to range
*     NLEFT  Returned by GKIOBO (and ignored)
*     NSnnn  Number of character sizes for Workstation nnn
*     ICnnn  Buffer for codes to send to Workstation nnn
*     IF801  Cifer T5 font for each character size
*     IF82X  Standard/RAL mods Pericom font for each character size
*     IM82X  Standard/RAL mods Pericom mag. fact. for each character size
*
      INTEGER ITRIM, NLEFT, NS203, IC203(2), NS82X, IC82X(5)
      INTEGER NS801, IC801(2)
      PARAMETER (NS203=4, NS801=12, NS82X=16)
*
      INTEGER IF801(NS801), IF82X(NS82X), IM82X(NS82X)

      DATA IF801/ 12, 11, 1, 10, 9, 2, 3, 4, 5, 6, 7, 8 /
*     To simplify things, Pericom' 4 character sizes and 4 magni-
*     fication factors are taken to be one font with 16 character
*     sizes.
      DATA IF82X/4,3,2,1,4,3,4,2,3,1,4,3,2,1,2,1/
      DATA IM82X/1,1,1,1,2,2,3,2,3,2,4,4,3,3,4,4/

*     1st element contains ESC (constant); 2nd is the code to
*     select the character size and is inserted. For Pericom,
*     the additional 3 elements are to select the magnification
*     factor.
      DATA IC203/27,0/, IC801/27,0/, IC82X/27,0,27,92,0/
*
*-----------------------------------------------------------------------

      ITRIM=JCHSZD
      IF( ITRIM.LT.1 ) ITRIM=1

      IF (KWKTYP.EQ.203) THEN
*  TEK 4014
         IF( ITRIM.GT.NS203 ) ITRIM=NS203
         IC203(2) = 60 - ITRIM
         CALL GKIOBO(KIOPB,2,IC203,NLEFT)

      ELSEIF (KWKTYP.EQ.801) THEN
*  Cifer T5
         IF( ITRIM.GT.NS801 ) ITRIM=NS801
         IC801(2) = IF801(ITRIM) + 47
         CALL GKIOBO(KIOPB,2,IC801,NLEFT)

      ELSEIF (KWKTYP.EQ.820.OR.KWKTYP.EQ.821) THEN
*   Standard/RAL mods Pericom Montereys shared entry
         IF( ITRIM.GT.NS82X ) ITRIM=NS82X
         IC82X(2) = IF82X(ITRIM) + 55
         IC82X(5) = IM82X(ITRIM) + 100
         CALL GKIOBO(KIOPB,5,IC82X,NLEFT)
      END IF

      END
