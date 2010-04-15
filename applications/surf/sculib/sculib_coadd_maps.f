      SUBROUTINE SCULIB_COADD_MAPS(NPTS, DATA_IN, QUALITY_IN, WEIGHT,
     :     NCOADD, DATA_OUT, VARIANCE_OUT, QUALITY_OUT, WEIGHT_OUT,
     :     STATUS)
*+
*  Name:
*     SCULIB_COADD_MAPS

*  Purpose:
*     Average together integration and output map

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCULIB_COADD_MAPS(NPTS, DATA_IN, QUALITY_IN, WEIGHT,
*    :     NCOADD, DATA_OUT, VARIANCE_OUT, QUALITY_OUT, WEIGHT_OUT,
*    :     STATUS)

*  Description:
*     This routine coadds the current integration image to the output image.
*     The coadd variance is calculated from the spread of the integrations
*     about the mean if more than one integration has been added in. The
*     averaging is done over integrations, so that each point in an image
*     is separate. An output point is good if either of the inputs is good.
*     If only one dataset is coadded th variance is set zero.
*     Input pixels with bad quality are ignored.

*  Arguments:
*     NPTS = INTEGER (Given)
*       Number of input data points
*     DATA_IN( NPTS ) = REAL (Given)
*       Input data
*     QUALITY_IN( NPTS ) = BYTE (Given)
*       Input data quality
*     WEIGHT = REAL (Given)
*       Relative weight of input data
*     NCOADD( NPTS ) = INTEGER (Given & Returned)
*       Number of points coadded into each data point
*     DATA_OUT( NPTS ) = REAL (Given & Returned)
*       Coadded output data
*     VARIANCE_OUT( NPTS ) = REAL (Given & Returned)
*       Variance of coadded output data.
*     QUALITY_OUT( NPTS ) = BYTE (Given & Returned)
*       Quality of coadded output data
*     WEIGHT_OUT( NPTS ) = REAL (Given & Returned)
*       Total weight of output data
*     STATUS = INTEGER (Given & Returned)
*       Global status

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL:  John Lightfoot (RoE)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.4  1999/08/06 02:24:40  timj
*     Tweak headers for use with PROLAT.
*
*     1997 April 17 (TIMJ)
*        Rework SCULIB_COADD

*  Bugs:
*     {note_any_bugs_here}

*-

*     Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*    Arguments Given:
      INTEGER NPTS
      REAL    DATA_IN( NPTS )
      INTEGER NCOADD( NPTS )
      BYTE    QUALITY_IN( NPTS )
      REAL    WEIGHT

*     Arguments Given & Returned:
      REAL    DATA_OUT( NPTS )
      BYTE    QUALITY_OUT( NPTS )
      REAL    VARIANCE_OUT( NPTS )
      REAL    WEIGHT_OUT( NPTS )

*    Status:
      INTEGER STATUS

*    Local variables:
      INTEGER I                   ! DO loop
      REAL    SUM                 ! sum of data coadded
      REAL    SUMSQ               ! sum of data squared coadded
*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO I = 1, NPTS

*       Check quality of input data (always assume 0 is good and anything
*       else is bad)
         IF (QUALITY_IN(I) .EQ. 0) THEN

*  good quality input point,
*  ..recover the sum of the data points and the sum of them squared

            IF (NCOADD(I) .EQ. 1) THEN
               SUM = DATA_OUT (I)
               SUMSQ = DATA_OUT(I)**2
            ELSE IF (NCOADD(I) .GT. 1) THEN
               SUM = DATA_OUT (I) * NCOADD (I)
               SUMSQ = NCOADD(I) * DATA_OUT(I)**2 +
     :              NCOADD(I) * (NCOADD(I) - 1) * VARIANCE_OUT(I)
            ELSE
               SUM = 0.0
               SUMSQ = 0.0
               NCOADD (I) = 0
            END IF

*  ..add in the current exposure

            SUM = SUM + DATA_IN (I) * WEIGHT
            SUMSQ = SUMSQ + (DATA_IN(I) * WEIGHT) **2
            NCOADD(I) = NCOADD(I) + 1
            QUALITY_OUT(I) = 0
            WEIGHT_OUT(I) = WEIGHT_OUT(I) + WEIGHT

*  ..calculate the new average and variance, set the variance to the variance
*    of the input data if there is any, otherwise set it to zero


            DATA_OUT(I) = SUM / NCOADD(I)
            IF (NCOADD(I) .GT. 1) THEN
               VARIANCE_OUT (I) =
     :            (SUMSQ - NCOADD(I) * DATA_OUT(I)**2) /
     :            (NCOADD(I) * (NCOADD(I)-1))
            ELSE
               VARIANCE_OUT (I) = 0.0
            END IF
         END IF
      END DO

      END
