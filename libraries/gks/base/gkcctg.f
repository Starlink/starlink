      SUBROUTINE GKCCTG
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To copy transformation and clipping data to the W.C.A.
*     The data is obtained assuming that
*     a GKS output function is being obeyed. In other words
*     the data is obtained from the GKS State List.
*
*  MAINTENANCE LOG
*  ---------------
*     16/03/83  JRG   First version
*     30/03/83  JRG   CHECK.INC included
*     11/04/83  JRG   GKS.PAR included
*     10/11/83  JRG   Incorporate segment transformation of open segment
*     19/01/87  DCS   IS conversion. Get clipping rectangle directly
*                     from GKS State List.
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYOPS/ GKS Operating State
*     Read   /GKYSL/  Transformation and clipping parameters.
*     Modify /GKYWCA/ Set parameters in W.C.A.
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     NT    Local for Current Normalisation Transformation Number
*     IDUMMY Integer data from GKDRGE not used in this routine
*     STRAN  Segment transformation data
*
      INTEGER NT,IDUMMY(KSGISZ)
      REAL STRAN(KSGRSZ)
*
*---------------------------------------------------------------------


*   Local copy ... frequently needed
      NT=KCNTN

*   QWR1 ... QWR6 <- TransformationC2:  this is current output
*   transformation. In general C2 includes the insert transformation
*   of a segment being inserted (does not apply if this routine is
*   being called) and the normalization transformation.
      QWR1=(QLVPXR(NT)-QLVPXL(NT))/(QLWXR(NT)-QLWXL(NT))
      QWR2=0.0
      QWR3=QLVPXL(NT) - QLWXL(NT)*QWR1
      QWR4=0.0
      QWR5=(QLVPYT(NT)-QLVPYB(NT))/(QLWYT(NT)-QLWYB(NT))
      QWR6=QLVPYB(NT) - QLWYB(NT)*QWR5

*   QWR11 ... QWR16 <- TransformationC3: this is TransformationC2 combined
*   with the segment transformation if any.
*
*     -  -     -        -    -        -    -  -
*    | xn |   | s1 s2 s3 |  | n1 n2 n3 |  | xw |
*    | yn | = | s4 s5 s6 |  | n4 n5 n6 |  | yw |
*    |  1 |   |  0  0  1 |  |  0  0  1 |  |  1 |
*     -  -     -        -    -        -    -  -
*
      IF( KOPS.EQ.GSGOP ) THEN
*      If segment open, then find transformation of open segment and
*      combine it with C2
        CALL GKDRGE(KSGLST,KOPSG,KSGISZ,KSGRSZ,IDUMMY,STRAN)
        IF( KERROR.NE.0 ) CALL GKBUG(2007,'GKCCTG')
        CALL GKMTML(QWRA(1),STRAN(KSGTRN), QWRA(11) )
      ELSE
        QWR11=QWR1
        QWR12=QWR2
        QWR13=QWR3
        QWR14=QWR4
        QWR15=QWR5
        QWR16=QWR6
      ENDIF

*   Now deal with clipping rectangle
      IF( KCLIN.EQ.GCLIP ) THEN
        QWR7=QCCLXL
        QWR8=QCCLXR
        QWR9=QCCLYB
        QWR10=QCCLYT
      ELSE
        QWR7=0.0
        QWR8=1.0
        QWR9=0.0
        QWR10=1.0
      ENDIF

      END
