C# IL>=a, OL>=0
      SUBROUTINE GKNEXT(IV,IDIR,ISIZ,LINK)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     returns the vertex adjacent to IV in the current direction,
*     rolling round polygon if barrier encountered.
*     -returns zero if resulting vertex already processed
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  NGB   Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IV    initial Vertex index
*     INP IDIR  Direction in which to search
*     INP ISIZ  size of LINK array
*     INP LINK  array of Links to be searched:
*
      INTEGER IV, IDIR, ISIZ, LINK(0:ISIZ-1)
*
*---------------------------------------------------------------------


      IV = IV+IDIR
      IF (LINK(IV) .EQ. -8888) THEN
* search in opposite direction for barrier
   10    CONTINUE
         IV = IV-IDIR
         IF (.NOT. LINK(IV-IDIR).EQ.-8888)  GOTO 10
      ENDIF

* see if we've been here before
      IF (LINK(IV).EQ.-9999) IV = 0
      END
