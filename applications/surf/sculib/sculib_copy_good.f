      SUBROUTINE SCULIB_COPY_GOOD(NPTS, INDATA, INVAR, INARR1, INARR2,
     :     NGOOD, OUTDATA, OUTVAR, OUTARR1, OUTARR2, STATUS)
*+
*  Name:
*     SCULIB_COPY_GOOD

*  Purpose:
*     Go through data set and throw away bad values

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_COPY_GOOD(NPTS, INDATA, INVAR, INARR1, INARR2,
*    :     NGOOD, OUTDATA, OUTVAR, OUTARR1, OUTARR2, STATUS)


*  Description:
*     This routine copies good data and variance from an input array to
*     to an output array, leaving behind BAD values.

*  Arguments:
*     NPTS = INTEGER (Given)
*        The number of points in the input array
*     INDATA ( NPTS ) = REAL (Given)
*        Input data
*     INVAR ( NPTS ) = REAL (Given)
*        Input variance
*     INARR1 ( NPTS ) = DOUBLE (Given)
*        Auxilliary double precision array
*     INARR2 ( NPTS ) = DOUBLE (Given)
*        Auxilliary double precision array
*     NGOOD = INTEGER (Returned)
*        Number of good points in input data
*     OUTDATA ( NPTS ) = REAL (Returned)
*        Output data
*     OUTVAR ( NPTS ) = REAL (Returned)
*        Output variance
*     OUTARR1 ( NPTS ) = REAL (Returned)
*        Good auxiliary data converted to real
*     OUTARR2 ( NPTS ) = REAL (Returned)
*        Good auxiliary data converted to real
*     STATUS = INTEGER (Given & Returned)
*        Global status value

*  Notes:
*     - Data are still good even if VARIANCE or AUX are bad

*  Implementation Status:
*     - Only a routine for REAL numbers exists

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 April 5 (TIMJ):
*       Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
      INCLUDE 'PRM_PAR'               ! VAL__ constants

*  Arguments Given
      INTEGER NPTS
      REAL    INDATA ( NPTS )
      REAL    INVAR ( NPTS )
      DOUBLE PRECISION INARR1 ( NPTS )
      DOUBLE PRECISION INARR2 ( NPTS )

*  Arguments Returned:
      INTEGER NGOOD
      REAL    OUTDATA( NPTS )
      REAL    OUTVAR ( NPTS )
      REAL    OUTARR1 ( NPTS )
      REAL    OUTARR2 ( NPTS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

      IF (STATUS .NE. SAI__OK) RETURN

      NGOOD = 0

      DO I = 1, NPTS

         IF (INDATA(I) .NE. VAL__BADR) THEN
            NGOOD = NGOOD + 1
            OUTDATA(NGOOD) = INDATA(I)
            OUTVAR(NGOOD) = INVAR(I)
            OUTARR1(NGOOD) = REAL(INARR1(I))
            OUTARR2(NGOOD) = REAL(INARR2(I))
         END IF
      END DO

      END

