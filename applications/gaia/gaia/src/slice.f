      SUBROUTINE SLICE( NAME, IPIN, TYPE, NX, NY, XLOW, YLOW, XHIGH, 
     :                  YHIGH, NELEM, FILE, STATUS )

*+
*  Name:
*     SLICE

*  Purpose:
*     Extracts a slice of an image and saves it to a file.

*  Description:
*     This routine process a given image extract a spectrum along
*     the line defined between the pixel indices defined by the points
*     XLOW,YLOW to XHIGH,YHIGH. The result is written out to the file
*     whose name is given by the string FILE. This is passed to the NDF
*     library, so can be an NDF or a supported foreign file format.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SLICE( NAME, IPIN, TYPE, NX, NY, XLOW, YLOW, XHIGH, YHIGH,
*                  NELEM, FILE, STATUS )

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        Name of the image being processed (used in history record).
*     IPIN = INTEGER (Given)
*        Pointer to the image data.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type of the image. In a form understood by HDS.
*     NX = INTEGER (Given)
*        Size of image in X.
*     NY = INTEGER (Given)
*        Size of image in Y.
*     XLOW = INTEGER (Given)
*        Lower bound in pixel indices of slice.
*     YLOW = INTEGER (Given)
*        Lower bound in pixel indices of slice.
*     XHIGH = INTEGER (Given)
*        Upper bound in pixel indices of slice.
*     YHIGH = INTEGER (Given)
*        Upper bound in pixel indices of slice.
*     NELEM = INTEGER (Given)
*        Number of pixels in output spectrum.
*     FILE = CHARACTER * ( * ) (Given)
*        Name of the output file (either an NDF name or some supported
*        foreign file format, either way the file will be created by
*        NDF).
*     STATUS = INTEGER (Returned)
*        The global status (not SAI__OK when an error occurs).

*  Notes:
*     - This routine is designed to be called from RTD, the real-time
*     display tool and is not intended for use in other ways.
*
*     - Any bad pixels are just ignored.


*  Authors:
*     PWD: P.W. Draper (STARLINK-Durham University)
*     {enter_new_authors_here}

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils

*  History:
*     25-NOV-1997 (PWD):
*        Original version based on FIGARO task SLICE.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'DAT_PAR'         ! HDS parameters
      INCLUDE 'NDF_PAR'         ! NDF parameters

*  Arguments Given:
      CHARACTER * ( * ) NAME
      INTEGER IPIN
      CHARACTER * ( * ) TYPE
      INTEGER NX
      INTEGER NY
      INTEGER XLOW
      INTEGER YLOW
      INTEGER XHIGH
      INTEGER YHIGH
      INTEGER NELEM
      CHARACTER * ( * ) FILE

*  Global status.
      INTEGER STATUS

*  Local variables:
      CHARACTER * ( NDF__SZHIS ) TEXT( 3 ) ! History text
      INTEGER PLACE             ! NDF placeholder
      INTEGER UBND( 1 )         ! Upper bound of NDF
      REAL DELX                 ! Increment in X
      REAL DELY                 ! Increment in Y
      REAL X                    ! Current X position
      REAL Y                    ! Current Y position
      INTEGER INDF              ! NDF identifier
      INTEGER I                 ! Loop variable
      INTEGER EL                ! Number of elements in slice
      INTEGER ISPEC             ! Pointer to mapped slice data

*.

*  Set the global status.
      STATUS = SAI__OK

*  Create the NDF to contain the output slice.
      CALL NDF_BEGIN
      CALL NDF_PLACE( DAT__ROOT, FILE, PLACE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
         UBND( 1 ) = NELEM
         CALL NDF_NEWP( TYPE, 1, UBND, PLACE, INDF, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Map in the spectrum.
            CALL NDF_MAP( INDF, 'DATA', TYPE, 'WRITE', ISPEC, EL,
     :                    STATUS )

*  Initialise the interpolation routines (always use quintic
*  interpolation).
            CALL RTD1_STINT( 3 )

*  Work through the elements of the spectrum...
            DELX = ( XHIGH - XLOW ) / REAL( NELEM - 1 )
            DELY = ( YHIGH - YLOW ) / REAL( NELEM - 1 )
            X = XLOW
            Y = YLOW

*  Each image type is dealt with separately.
            IF ( TYPE .EQ. '_BYTE' ) THEN
               DO I = 1, NELEM
                  CALL RTD1_ETERPB( X, Y, NX, NY, %VAL( IPIN ), I,
     :                              %VAL( ISPEC ) )
                  X = X + DELX
                  Y = Y + DELY
               END DO
            ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
               DO I = 1, NELEM
                  CALL RTD1_ETERPUB( X, Y, NX, NY, %VAL( IPIN ), I,
     :                               %VAL( ISPEC ) )
                  X = X + DELX
                  Y = Y + DELY
               END DO
            ELSE IF ( TYPE .EQ. '_WORD' ) THEN
               DO I = 1, NELEM
                  CALL RTD1_ETERPW( X, Y, NX, NY, %VAL( IPIN ), I,
     ;                              %VAL( ISPEC ) )
                  X = X + DELX
                  Y = Y + DELY
               END DO
            ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
               DO I = 1, NELEM
                  CALL RTD1_ETERPUW( X, Y, NX, NY, %VAL( IPIN ), I,
     :                               %VAL( ISPEC ) )
                  X = X + DELX
                  Y = Y + DELY
               END DO
            ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
               DO I = 1, NELEM
                  CALL RTD1_ETERPI( X, Y, NX, NY, %VAL( IPIN ), I,
     :                        %VAL( ISPEC ) )
                  X = X + DELX
                  Y = Y + DELY
               END DO
            ELSE IF ( TYPE .EQ. '_REAL' ) THEN
               DO I = 1, NELEM
                  CALL RTD1_ETERPR( X, Y, NX, NY, %VAL( IPIN ), I,
     :                              %VAL( ISPEC ) )
                  X = X + DELX
                  Y = Y + DELY
               END DO
            ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
               DO I = 1, NELEM
                  CALL RTD1_ETERPD( X, Y, NX, NY, %VAL( IPIN ), I,
     :                              %VAL( ISPEC ) )
                  X = X + DELX
                  Y = Y + DELY
               END DO
            ELSE
               STATUS = SAI__ERROR
               CALL MSG_SETC( 'TYPE', TYPE )
               CALL ERR_REP( 'UNKNOWN',
     :                     'SLICE: Unknown data type ^TYPE;'//
     :                     'possible programming error', STATUS )
            END IF

*     Add some history to the NDF about how it was created. 
            CALL NDF_HCRE( INDF, STATUS )
            CALL MSG_SETI( 'NX', NX )
            CALL MSG_SETI( 'NY', NY )
            CALL MSG_SETI( 'XLOW', XLOW )
            CALL MSG_SETI( 'YLOW', YLOW )
            CALL MSG_SETI( 'XHIGH', XHIGH )
            CALL MSG_SETI( 'YHIGH', YHIGH )
            CALL MSG_SETC( 'NAME' , NAME )
            TEXT( 1 ) = 'Slice written by GAIA'
            TEXT( 2 ) = 'Original file: ^NAME'
            TEXT( 3 ) = 
     :      'Pixel end points of slice: ^XLOW,^YLOW ^XHIGH,^YHIGH'
            CALL NDF_HPUT( ' ', 'GAIA', .TRUE., 3, TEXT, .TRUE.,
     :                     .FALSE., .FALSE., INDF, STATUS )
         END IF
      END IF

*     Release the NDF and its resources.
      CALL NDF_END( STATUS )

*     Add an informative error message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SLICE_ERR',
     :                 'SLICE: failed to create image slice.', STATUS )
      END IF
      END
