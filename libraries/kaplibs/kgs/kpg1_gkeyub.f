      SUBROUTINE KPG1_GKEYUB( NLEVK, LEVL, LEVU, ZKEY, RMIN, RMAX,
     :                         TITLE, STATUS )
*+
*  Name:
*     KPG1_GKEYx

*  Purpose:
*     Plots a colour-table key and title in the current SGS zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GKEYx( NLEVK, LEVL, LEVU, ZKEY, RMIN, RMAX, TITLE,
*                      STATUS )

*  Description:
*     This subroutine draws a key in the current zone, consisting of a
*     title and an equally spaced selection of the colour-table levels,
*     each of which is annotated with the mean value corresponding to
*     it.  Although it will work with colour it is intended for use in
*     a grey-scale plot.  It assumes a linear mapping between data
*     value and colour index.

*  Arguments:
*     NLEVK = INTEGER (Given)
*        Number of colour indices to be plotted up to 16.
*     LEVL = INTEGER (Given)
*        Upper colour index used in the plot itself.
*     LEVU = INTEGER (Given)
*        Upper colour index used in the plot itself.
*     ZKEY = REAL (Given)
*        The fraction (to the right) of the square zone that will plot
*        the key (about 0.15 to 0.2 produces acceptable results.)
*     RMIN = ? (Given)
*        Value corresponding to LEVL in the plot.  (Black in a
*        grey-scale plot.)
*     RMAX = ? (Given)
*        Value corresponding to LEVU in the plot.  (White in a
*        grey-scale plot.)
*     TITLE = CHARACTER * ( * ) (Given)
*        Title, the maximum length depends on ZKEY; for ZKEY=0.2 only
*        the first 25 characters will be displayed.
*     STATUS = INTEGER (Given and Returned)
*        The global status value.

*  Algorithm:
*     -  Save current parameters defining text attributes, and the zone
*     co-ordinates. Set the world co-ordinates with the aspect ratio of
*     the key zone
*     -  Compute the position of the title and key strings, and the
*     spacing between the key levels.  Adjust the fount size for
*     annotation.
*     -  Find the range of the grey levels and the scaling factor.
*     For each key point
*        o  Determine maximum and minimum values represented and hence
*        the mean.  Scale the mean to a colour index.
*        o  Store grey level in array to be plotted
*        o  Convert the mean value to a text string and annotate its
*        associated colour-index block.  Draw the coloured block.
*     Reset the world co-ordinates and the text attributes to their
*     values upon entering this routine.

*  Notes:
*     -  There is a routine for all numeric data types: replace "x" in
*     the routine name by B, D, I, R, UB, UW or W as appropriate.  The
*     data limits supplied to the routine must have the data type
*     specified.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 April 8 (MJC):
*        Original based on GRKEYR from earlier versions of KAPPA.
*     1991 May 14 (MJC):
*        Made to function for all numeric data types.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER
     :  NLEVK,
     :  LEVL,
     :  LEVU

      BYTE
     :  RMIN,
     :  RMAX

      REAL
     :  ZKEY

      CHARACTER*(*)
     :  TITLE

*  Status:
      INTEGER STATUS

*  Local Constants:
      REAL SCALE                ! Adjusts the size of the pixels in the
                                ! grey-level key
      PARAMETER ( SCALE = 100.0 )

      INTEGER NPIX              ! NPIX is the number of pixels plotted
                                ! in the grey-level key
      PARAMETER ( NPIX=2 )

*  Local Variables:
      INTEGER
     :  FOUNT,                  ! Fount number
     :  I, J, K,                ! Loop counter
     :  IVALUE,                 ! Colour index of a key marker
     :  KEYBLK( NPIX, NPIX ),   ! Key colour-block markers cell array
     :  NCHAR,                  ! Number of characters in annotation
     :  OLDPEN,                 ! Pen number.
     :  TXTPRC                  ! Text precision

      CHARACTER
     :  ANNOTE*11,              ! Colour-index annotation
     :  TXJ*2                   ! Text justification code

      REAL
     :  ASPRAT,                 ! Aspect ratio
     :  DEL,                    ! Small offset
     :  HTCHR,                  ! Character height
     :  SPKEY,                  ! Space in y between the key marks
     :  TXTSPC,                 ! Text spacing
     :  XBL,                    ! X world co-ordinate of the bottom
                                ! left-hand corner of the screen
     :  XM,                     ! X dimension of zone in metres
     :  XOFFA,                  ! X offset for the key annotation
     :  XOFFK,                  ! X offset for the key coloured block
     :  XTR,                    ! X world co-ordinate of the top
                                ! right-hand corner of the screen
     :  XU,                     ! X direction cosine of text orientation
     :  YBL,                    ! Y world co-ordinate of the bottom
                                ! left-hand corner of the screen
     :  YKEY,                   ! Y position of the key
     :  YM,                     ! The y dimension of the zone in metres
     :  YOFFK,                  ! Y offset for the key coloured block
     :  YTITLE,                 ! Y position of the title
     :  YTR,                    ! Y world co-ordinate of the top
                                ! right-hand corner of the screen.
     :  YU                      ! Y direction cosine of text orientation

      DOUBLE PRECISION
     :  DTMAX,                  ! D.P. version of RMAX
     :  DTMIN,                  ! D.P. version of RMIN
     :  GM,                     ! Minimum value of the data
     :  GMIN,                   ! Minimum value of the data for a given
                                ! key mark
     :  GMAX,                   ! Maximum value of the data for a given
                                ! key mark
     :  KSCALE,                 ! Scaling factor for key cell arrays
     :  RANGE,                  ! Range of values
     :  VALUE                   ! Value associated with a key marker

*  Internal References:
      INCLUDE 'NUM_DEC_CVT'    ! NUM declarations for conversions
      INCLUDE 'NUM_DEC_UB'    ! NUM declarations for functions
      INCLUDE 'NUM_DEF_CVT'    ! NUM definitions for conversions
      INCLUDE 'NUM_DEF_UB'    ! NUM definitions for functions

*.

*    Check the inherited status value.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Save parameters defining the text and co-ordinates.

      CALL SGS_IZONE( XBL, XTR, YBL, YTR, XM, YM )
      CALL SGS_ITXA( FOUNT, TXTPRC, HTCHR, ASPRAT, XU, YU, TXTSPC, TXJ )
      CALL SGS_IPEN( OLDPEN )

*    Reset the zone to square in the larger zone. The viewport ranges
*    are the same and large to produce square pixels.

      XOFFK = ( 1.0 - ZKEY ) * SCALE
      CALL SGS_SW( XOFFK, SCALE, 0., SCALE, STATUS )

*    Define the y positions of the title and Key near the top of the
*    zone.  Find the y spacing between key entries (there is none in
*    x).

      YTITLE = 0.95 * SCALE
      YKEY = 0.9 * SCALE
      SPKEY = YKEY / REAL( NLEVK )

*    Write annotations in pen 1.

      CALL SGS_SPEN( 1 )

*    Set fount attributes: height and bold.

      CALL SGS_SHTX( SCALE * 0.02 )
      CALL SGS_SFONT( 104 )
      CALL SGS_BTEXT( XOFFK + SCALE * 0.012, YKEY + SCALE * 0.01 )
      CALL SGS_ATEXT( 'Key:' )
      CALL SGS_BTEXT( XOFFK + SCALE * 0.012, YTITLE + SCALE * 0.02 )
      CALL SGS_ATEXT( 'Title:' )

*    Adjust the fount for the title.

      CALL SGS_SHTX( SCALE * 0.009 )
      CALL SGS_SFONT( 1 )
      CALL SGS_BTEXT( XOFFK + SCALE * 0.015, YTITLE + SCALE * 0.003 )
      CALL SGS_ATEXT( TITLE )
      CALL SGS_OTEXT

*    Adjust the fount for the annotations.

      CALL SGS_SHTX( SPKEY * 0.2 )
      CALL SGS_SARTX( 0.5 )

      XOFFA = SCALE * 0.25 * ZKEY + XOFFK
      DEL = 0.005 * SCALE

*    Find the scale factor and some other useful parameters.  Note use
*    of double precision to minimise the risk of overflows.

      DTMAX = NUM_UBTOD( RMAX )
      DTMIN = NUM_UBTOD( RMIN )
      RANGE = DTMAX - DTMIN
      KSCALE = DBLE( LEVU - LEVL + 1 ) / RANGE

      GM = NUM_UBTOD( NUM_MINUB( RMIN, RMAX ) )
      GMAX = GM

*    Loop for each colour index in the key.

      DO  K = 1, NLEVK
         YOFFK = REAL( K ) * SPKEY

*       Determine range of values it represents.

         GMIN = GMAX
         GMAX = ( DBLE( K ) * ABS( RANGE ) ) / DBLE( NLEVK ) + GM

*       Find the value at the key entry.

         VALUE = 0.5 * ( GMIN + GMAX )

*       Scale the value to find its colour index.

         IVALUE = MIN( MAX( NINT( ( VALUE - DTMIN ) * KSCALE + LEVL ),
     :            LEVL ), LEVU )

*       Place the value into the small cell array.

         DO  J = 1, NPIX
            DO  I = 1, NPIX
               KEYBLK( I, J ) = IVALUE
            END DO
         END DO

*       Convert the data value to characters.

         CALL CHR_DTOC( VALUE, ANNOTE, NCHAR )

*       Annotate colour-index block with the value it represents.

         CALL SGS_BTEXT( XOFFA, YOFFK - 0.45 * SPKEY )
         CALL SGS_ATEXT( ANNOTE( 1:NCHAR ) )
         CALL SGS_OTEXT

*       Draw the colour-index block.

         CALL KPG1_GCA( XOFFK + 2. * DEL, YOFFK - DEL - REAL( NPIX ),
     :                  XOFFK + REAL( NPIX ) + 2. * DEL, YOFFK - DEL,
     :                  NPIX, NPIX, NPIX, NPIX, KEYBLK, STATUS )
      END DO

*    Reset all the attributes to what they were on entry to the
*    subroutine.
*
*    Reset the world co-ordinates.

      CALL SGS_SW( XBL, XTR, YBL, YTR, STATUS )

*    Reset the fount

      CALL SGS_SFONT( FOUNT )

*    Reset the character height.

      CALL SGS_SHTX( HTCHR )

*    Reset the aspect ratio.

      CALL SGS_SARTX( ASPRAT )

*    Reset the text justification

      CALL SGS_STXJ( TXJ )

*    Reset the pen.

      CALL SGS_SPEN( OLDPEN )

      END
