C# IL>=a, OL>=0
      SUBROUTINE GKDRGE (IROOT, INDEX, ISIZEI, ISIZER,
     :                                   IARRAY, RARRAY )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Get Directory Entry
*
*  MAINTENANCE LOG
*  ---------------
*     10/08/83  CJW   Original version stabilized
*     15/12/83  CJW   Changed meaning of NITEM
*     16/12/83  CJW   Provide more detailed bug numbers
*     19/01/87  PKY   IS conversion. Error number changes.
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*
*  ARGUMENTS
*  ---------
*     INP IROOT   Heap Index - Points to Directory
*     INP INDEX   Directory item index (>=0)
*     INP ISIZEI  Actual size of IARRAY
*     INP ISIZER  Actual size of RARRAY
*     OUT IARRAY  Returned integer data
*     OUT RARRAY  Returned real data
*
      INTEGER IROOT, INDEX, ISIZEI, ISIZER, IARRAY(*)
      REAL RARRAY(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKHP/    Access directory
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkdir.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IDIR    Addresses in the integer heap - derived index at various levels
*     ISIZ    Number of directory entries
*     INTS    Number of integers per entry
*     IREA    Number of reals per entry
*     IHEAP   Temporary heap pointer - points to block containing item
*     IRPT    Temporary heap pointer - points to reals
*     IOFFI   Offset of required integer data in the integer heap
*     IOFFR   Offset of required real data in the real heap
*     IOFF    Offset of required entry
*     IPRIOR  Pointer to previous block
*
      INTEGER IDIR, ISIZ, INTS, IREA, IHEAP,
     :        IRPT, IOFFI, IOFFR, IOFF, IPRIOR
*
*  HEAP USAGE
*  ----------
*     Access heap directly
*
*  ERRORS
*  ------
*    -1017 Item not found in list
*    -2004 Documented condition to be satisfied by parameter(s) of
*          internal routine is not satisfied.
*    -2016 Invalid Heap pointer
*    -2017 Argument to internal GKS routine specifies an array that is
*          too small
*
*  COMMENTS
*  --------
*
*     Any of the following are detected as bugs -
*
*    (-2004)   INDEX < 0
*    (-2016)   IROOT < 0
*              IROOT > KHPXSI
*              KHPXI(IROOT) = KNIL
*    (-2017)   ISIZEI too small
*              ISIZER too small
*
*     Internal inconsistances in either the heap or the directory may
*     also be detected as bug -2004
*
*     If the entry is not found error -1017 is raised
*
*---------------------------------------------------------------------



*     Check Arguments

      IF (      (IROOT .LT. 0)          .OR.
     :          (IROOT .GT. KHPXSI)     .OR.
     :          (KHPXI(IROOT) .EQ. KNIL)  )  THEN
         CALL GKBUG(-2016, 'GKDRGE')
      ELSE IF (INDEX .LT. 0) THEN
         CALL GKBUG(-2004, 'GKDRGE')
      ELSE
         IDIR = KHPXI(IROOT)
         ISIZ = KHP(IDIR + KDRINC)
         IREA = KHP(IDIR + KDRREA)
         INTS = KHP(IDIR + KDRINT)

         IF ( (INTS .LE. ISIZEI) .AND. (IREA .LE. ISIZER) ) THEN

*           Search Directory for INDEX

            CALL GKDRS( IROOT, INDEX, IHEAP, IOFF, IPRIOR)
            IF (IOFF .NE. KNIL) THEN
               IOFFI = KDRX + ISIZ + IOFF * INTS
               CALL GKHPGI(IHEAP, IOFFI, INTS, IARRAY)
               IDIR = KHPXI(IHEAP)
               IRPT = KHP(IDIR + KDRRPT)
               IF (IRPT .NE. KNIL) THEN
                  IOFFR = IOFF * IREA
                  CALL GKHPGR(IRPT, IOFFR, IREA, RARRAY)
               END IF
            ELSE
              KERROR = -1017
            END IF
         ELSE
           CALL GKBUG(-2017, 'GKDRGE')
         END IF
      END IF
      END
