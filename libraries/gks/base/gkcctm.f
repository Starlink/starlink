      SUBROUTINE GKCCTM
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONT-END
*  Author:             DSG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Copies transformation and clipping data for Metafile Input
*  Workstation to the Workstation Communication Area
*
*  MAINTENANCE LOG
*  ---------------
*     15/07/83  DSG   Original version stabilied
*     17/10/85  DSG   If segment is open, add segment transformation to
*                     transformation C3 (I225).
*     27/06/86  DCS   (ICL fix MGC 13/11/86!) Fix bug in previous fix to
*                     avoid bound check on QWRA.
*     19/01/87  DCS   IS conversion. Remove metafile index and access to
*                     Metafile Attribute List.  Get clipping data from
*                     GKS State List.
*     22/01/87  JCS   IS conversion. Bug number changed.
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYOPS/    GKS Operating State
*     Read   /GKYSL/     Clipping parameters
*     Modify /GKYWCA/    Set parameters in W.C.A.
*     Modify /GKYERR/    KERROR
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
*     J       DO loop varible
*     IARRAY   Integer data from GKDRGE not used in this routine
*     RARRAY   Real data from GKDRGE (includes segment transformation)
*
      INTEGER J
      INTEGER IARRAY(KSGISZ)
      REAL RARRAY(KSGRSZ)
*
*---------------------------------------------------------------------

* Identity transformation for C2

      QWR1 = 1.0
      QWR2 = 0.0
      QWR3 = 0.0
      QWR4 = 0.0
      QWR5 = 1.0
      QWR6 = 0.0

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
        CALL GKDRGE(KSGLST,KOPSG,KSGISZ,KSGRSZ,IARRAY,RARRAY)
        IF( KERROR.NE.0 ) CALL GKBUG(-2007,'GKCCTM')
        DO 30 J=0,5
         QWRA(11+J)=RARRAY(KSGTRN+J)
   30   CONTINUE
      ELSE
        QWR11 =   QWR1
        QWR12 =   QWR2
        QWR13 =   QWR3
        QWR14 =   QWR4
        QWR15 =   QWR5
        QWR16 =   QWR6
      ENDIF

* Now deal with clipping rectangle (from GKS State List)

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
