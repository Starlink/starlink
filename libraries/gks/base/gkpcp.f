C# IL>=a, OL>=0
      SUBROUTINE GKPCP(NROWS, NFIRST, NLAST, XOLD, YOLD,
     :                                          XNEW, YNEW)
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
*     Determine the corner points P, Q and R for a chunk of cell array.
*
*  MAINTENANCE LOG
*  ---------------
*     03/01/84  CJW   Original version stabilized
*     04/05/84  CJW   Incorrect evaluation of Q and R (I200)
*     10/05/84  MGC   Further corrections for P,Q,R (I215)
*     11/12/91  KEVP  Put in a correct algorithm and improved comments
*                     including the writing of a PURPOSE (C92).
*
*  ARGUMENTS
*  ---------
*     NROWS  INP   Number of rows in cell array
*     NFIRST INP   First row in chunk
*     NLAST  INP   Last row in chunk
*     XOLD   INP   X coords of PQR of whole cell array
*     YOLD   INP   Y coords of same
*     XNEW   OUT   X coords of PQR of chunk
*     YNEW   OUT   Y coords of same
*
      INTEGER NROWS, NFIRST, NLAST
      REAL XOLD(3), YOLD(3), XNEW(3), YNEW(3)
*
*  LOCALS
*  ------
*
*     IP, IQ, IR   Array Index of P, Q, R
*     PBEFOR       Proportion of cell array before chunk
*     PAFTER       Proportion of cell array after chunk
*
      INTEGER     IP,     IQ,     IR
      PARAMETER ( IP = 1, IQ = 2, IR = 3 )
      REAL PBEFOR, PAFTER
*
*  COMMENTS
*  --------
*    Although each chunk has a whole number of rows, the X-coords
*    are included in case the cell array has been rotated.
*
*  ALGORITHM
*  ---------
*    Calculate proportion of cell array before chunk and after chunk.
*    Shift PR along QR by the proportion before chunk.
*    Shift  Q along RQ by the proportion after chunk.
*
*---------------------------------------------------------------------

*     Calculate the proportions
      PBEFOR = FLOAT(NFIRST-1)/FLOAT(NROWS)
      PAFTER = FLOAT(NROWS-NLAST)/FLOAT(NROWS)

*     Corner P - Start of First Row
      XNEW(IP) = XOLD(IP) + PBEFOR*(XOLD(IQ)-XOLD(IR))
      YNEW(IP) = YOLD(IP) + PBEFOR*(YOLD(IQ)-YOLD(IR))

*     Corner R - End of First Row
      XNEW(IR) = XOLD(IR) + PBEFOR*(XOLD(IQ)-XOLD(IR))
      YNEW(IR) = YOLD(IR) + PBEFOR*(YOLD(IQ)-YOLD(IR))

*     Corner Q - End of last Row
      XNEW(IQ) = XOLD(IQ) + PAFTER*(XOLD(IR)-XOLD(IQ))
      YNEW(IQ) = YOLD(IQ) + PAFTER*(YOLD(IR)-YOLD(IQ))


      END
