C# IL>=a, OL>=1
      SUBROUTINE GQDSGA (IWTYPE,IER,ISGTR,IVONOF,IVOFON,IHIGH,
     :                                         ISGPR,IADD,ISGDEL)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE DYNAMIC MODIFICATION OF SEGMENT ATTRIBUTES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns dynamic modification of segment attributes
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     OUT IER    - error indicator
*     OUT ISGTR  - segment transformation changeable
*     OUT IVONOF - visibility changeable from 'visible' to 'invisible'
*     OUT IVOFON - visibility changeable from 'invisible' to 'visible'
*     OUT IHIGH  - highlighting changeable
*     OUT ISGPR  - segment priority changeable
*     OUT IADD   - adding primitives to the open segment
*     OUT ISGDEL - segment deletion immediately visible
*
      INTEGER IWTYPE, IER, ISGTR, IVONOF, IVOFON, IHIGH, ISGPR
      INTEGER IADD, ISGDEL
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkwke.par'
*
*---------------------------------------------------------------------


      CALL GKQINT(IWTYPE,KQDSGA,IER,ISGTR,IVONOF,IVOFON,IHIGH,
     :                                         ISGPR,IADD,ISGDEL)

      END
