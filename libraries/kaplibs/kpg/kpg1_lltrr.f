      SUBROUTINE KPG1_LLTRR( XLOG, YLOG, SCALE, OFFSET, STATUS )
*+
*  Name:
*     KPG1_LLTRx
 
*  Purpose:
*     Saves a transformation for a data co-ordinate linear or
*     logarithmic plot in the AGI database.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_LLTRx( XLOG, YLOG, SCALE, OFFSET, STATUS )
 
*  Description:
*     This routine defines the transformations between world and a
*     log10-log10 or log10-linear data co-ordinate system, that has
*     either, neither or both axes with reversed polarity (increasing
*     right to left or top to bottom), and saves the transformation in
*     the AGI database with the current picture.
 
*  Arguments:
*     XLOG = LOGICAL (Given)
*        If true the x-axis is logarithmic in data co-ordinates,
*        otherwise it is linear.
*     YLOG = LOGICAL (Given)
*        If true the y-axis is logarithmic in data co-ordinates,
*        otherwise it is linear.
*     SCALE( 2 ) = ? (Given)
*        The scale factors of each linear axis to transform from world
*        co-ordinates to data co-ordinates.
*     OFFSET( 2 ) = ? (Given)
*        The offsets of each linear axis at pixel 0 to transform from
*        world co-ordinates to data co-ordinates.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for real and double-precision data types:
*     replace "x" in the routine name by R or D respectively.  The scale
*     and offset supplied to the routine must have the data type
*     specified.
 
*  Prior Requirements:
*     -  There must be a current AGI picture.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1991 June 21 (MJC):
*        Original version.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
 
*  Arguments Given:
      LOGICAL
     :  XLOG,
     :  YLOG
 
      REAL
     :  SCALE( 2 ),
     :  OFFSET( 2 )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Constants:
      INTEGER NCD                ! Number of data co-ordinates
      PARAMETER ( NCD = 2 )
      INTEGER NCW                ! Number of world co-ordinates
      PARAMETER ( NCW = 2 )
 
*  Local Variables:
      CHARACTER * ( 34 + 2 * VAL__SZR )
     :  DTOW( NCW ),             ! Expressions for converting data to
                                 ! world co-ordinates
     :  WTOD( NCD )              ! Expressions for converting world to
                                 ! data co-ordinates
 
      CHARACTER * ( VAL__SZR )
     :  COFSET( 2 ),             ! Offsets
     :  CSCALE( 2 )              ! Scales
 
      INTEGER
     :  I,                       ! Loop counter
     :  NCO( 2 ),                ! Number of characters in offset
     :  NCS( 2 )                 ! Number of characters in scale
 
*.
 
*    Check the inherited global status.
 
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*    Convert the scales and offsets to character strings.
 
      DO  I = 1, 2
         CALL CHR_RTOC( SCALE( I ), CSCALE( I ), NCS( I ) )
         CALL CHR_RTOC( OFFSET( I ), COFSET( I ), NCO( I ) )
      END DO
 
*    Assign the transformations for the x co-ordinates.
*    ==================================================
 
      IF ( XLOG ) THEN
         WTOD( 1 ) = 'XL = 10.0E0**( X ) * ( '/
     :               /CSCALE( 1 )( :NCS( 1 ) )//' ) + ( '/
     :               /COFSET( 1 )( :NCO( 1 ) )//' )'
         DTOW( 1 ) = 'X = LOG10( ( XL - ( '//COFSET( 1 )( :NCO( 1 ) )/
     :               /' ) ) / ( '//CSCALE( 1 )( :NCS( 1 ) )//' ) )'
      ELSE
         WTOD( 1 ) = 'XL = X * ( '//CSCALE( 1 )( :NCS( 1 ) )//' ) + ( '/
     :               /COFSET( 1 )( :NCO( 1 ) )//' )'
         DTOW( 1 ) = 'X = ( XL - ( '//COFSET( 1 )( :NCO( 1 ) )/
     :               /' ) ) / ( '//CSCALE( 1 )( :NCS( 1 ) )//' )'
      END IF
 
*    Assign the transformations for the y co-ordinates.
*    ==================================================
 
      IF ( YLOG ) THEN
         WTOD( 2 ) = 'YL = 10.0E0**( Y ) * ( '/
     :               /CSCALE( 2 )( :NCS( 2 ) )//' ) + ( '/
     :               /COFSET( 2 )( :NCO( 2 ) )//' )'
         DTOW( 2 ) = 'Y = LOG10( ( YL - ( '//COFSET( 2 )( :NCO( 2 ) )/
     :               /' ) ) / ( '//CSCALE( 2 )( :NCS( 2 ) )//' ) )'
      ELSE
         WTOD( 2 ) = 'YL = Y * ( '//CSCALE( 2 )( :NCS( 2 ) )//' ) + ( '/
     :               /COFSET( 2 )( :NCO( 2 ) )//' )'
         DTOW( 2 ) = 'Y = ( YL - ( '//COFSET( 2 )( :NCO( 2 ) )/
     :               /' ) ) / ( '//CSCALE( 2 )( :NCS( 2 ) )//' )'
      END IF
 
*    Save the transformation in the database associated with the current
*    picture.
 
      CALL AGI_TNEW( NCD, NCW, DTOW, WTOD, -1, STATUS )
 
      END
