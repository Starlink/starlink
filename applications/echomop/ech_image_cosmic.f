      SUBROUTINE ECH_IMAGE_COSMIC( NX, NY, IMAGE, QUALITY, IMAGE2,
     :           STATUS )
*+
*  Name:
*     ECHOMOP - ECH_IMAGE_COSMIC

*  Purpose:
*     Make a copy of object frame with CR's flagged.

*  Description:
*     This routine copies the input frame to the output frame and flags
*     the positions of any pixels identified as cosmic rays.

*  Invocation:
*     CALL ECH_IMAGE_COSMIC( NX, NY,  IMAGE, QUALITY, IMAGE2,
*    :     STATUS )

*  Arguments:
*     IMAGE = REAL (Given)
*        Input frame image of dimensions nx columns and ny rows.
*     QUALITY = BYTE (Given)
*        Quality Input frame image of dimensions nx columns and ny rows.
*     NX = INTEGER (Given)
*        Number of columns in frame.
*     NY = INTEGER (Given)
*        Number of rows in frame.
*     IMAGE2 = REAL (Returned)
*        Output frame image of dimensions nx columns and ny rows.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Copy original image into new one
*     Loop thru image rows
*        Loop thru image columns
*           If pixel flagged as cosmic ray then
*             Set pixel to `bad-pixel-magic-value' in output image
*           Else copy pixel value to output image
*           Endif
*        End loop
*     End loop

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_QUALITIES.INC'
      INCLUDE 'ECH_ENVIR_CONSTANTS.INC'

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL IMAGE( NX, NY )
      BYTE QUALITY( NX, NY )
      REAL IMAGE2( NX, NY )

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER IX
      INTEGER IY
      INTEGER DUMMY

*  Functions Called:
      LOGICAL ECH_FATAL_ERROR
      EXTERNAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Loop through image rows.
      DO IY = 1, NY

*     Loop through image columns.
         DO IX = 1, NX
            DUMMY = QUALITY( IX, IY )

*        If pixel flagged as cosmic ray then set pixel to
*        `bad-pixel-magic-value' in output image.
            IF ( IAND( DUMMY, QTY_COSMIC_RAY ) .EQ.
     :           QTY_COSMIC_RAY .OR.
     :           IAND( DUMMY, QTY_SATURATED ) .EQ.
     :           QTY_SATURATED ) THEN
               IMAGE2( IX, IY ) = ECH__BAD_REAL

*        Else copy pixel value to output image.
            ELSE
               IMAGE2( IX, IY ) = IMAGE( IX, IY )
            END IF
         END DO
      END DO

      END
