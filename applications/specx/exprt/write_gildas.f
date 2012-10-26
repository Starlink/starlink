*  History:
*     22 Nov 1993 (hme):
*        Remove TABs.
*     20 Sep 2000 (ajc):
*        unused GEN_ILEN
*-----------------------------------------------------------------------

      SUBROUTINE WRITE_GILDAS (IERR)

*  Routine to take currently open map file and to write it out as a
*  GILDAS image, using the subroutines provided in GILDAS.

      IMPLICIT NONE

*     Formal parameters:

      INTEGER IERR

*     Include files:

      INCLUDE 'MAPHD'
      INCLUDE 'MAPS'
      INCLUDE 'CUBE'
      INCLUDE 'PROTOTYPE'

*     Local variables:

      LOGICAL   GDF_ERROR
      INTEGER   ISTAT
      CHARACTER TITLE*80

*  Gildas not supported. Need to write autoconf test
      IERR = 68
      print *,'Gildas support not compiled into specx'
      RETURN

*  Ok? go...

      IF (.NOT. CUBE_IN_MEMORY) THEN
        IERR = 68
        RETURN
      END IF

      CALL GEN_GETSTR ('Title for image? (extension will be .GDF)',
     &                  'SPECX', ' ', TITLE, ISTAT)

*     Note that we are really just moving data from one area of
*     virtual memory to another! Z-array of GILDAS image is identical
*     in all respects to that of our data cube. However...
*     it turns out that the arrays are upside down and GILDAS is incapable
*     therefore of plotting them!
*
*       SUBROUTINE GDF_IMAGE (NAME,NX,NY,NZ,NT,Z,ERROR)
C       INCLUDE 'CNF_PAR'
C----------------------------------------------------------------------
C GDF   Write a GILDAS image from a Fortran array
C
C Arguments :
C       NAME    C*(*)   Name of GILDAS image
C       NX      I       Number of pixels on 1st axis
C       NY      I       Number of pixels on 2nd axis
C       NZ      I       Number of pixels on 3rd axis
C       NT      I       Number of pixels on 4th axis
C       Z       R*4(*)  Data array
C       ERROR   L       Logical error flag
C----------------------------------------------------------------------

C      CALL GDF_IMAGE (TITLE//'.GDF', NPTS(1), MSTEP, NSTEP, 1,
C     &                 %VAL(CNF_PVAL(CURRENT_CUBE_ADDRESS)), GDF_ERROR)

      RETURN
      END

*-----------------------------------------------------------------------
