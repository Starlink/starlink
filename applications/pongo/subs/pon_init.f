      SUBROUTINE PON_INIT()
*+
* Name:
*    PON_INIT

*  Purpose:
*     Initialise PONGO common blocks.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Fortran - 77 subroutine.

*  Description:
*     This routine initialises the PONGO common block PONGO_CMN.
*     It should be called once from the PONGO monolith.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     23-FEB-1995 (PDRAPER):
*        Original version.
*     17-OCT-1996 (PDRAPER):
*        Changed to a subroutine (was a block data, this section
*        now commented out).
*     6-DEC-1996 (PDRAPER):
*        Added initializations for projection common block.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! The main PONGO common block.
      INCLUDE 'PROJ_CMN'         ! Projection common blocks

*  Global Data:
C      DATA XDATA/ NDATMAX * 0.0D0 / ! X-axis data array
C      DATA YDATA/ NDATMAX * 0.0D0 / ! Y-axis data array
C      DATA ERXCOL / 0 /          ! X-axis error column number
C      DATA ERYCOL / 0 /          ! Y-axis error column number
C      DATA ILABPTR / 0 /         ! Pointer to current label
C      DATA ISYMBS / NDATMAX * 0 / ! Symbol numbers for each data point
C      DATA LABCOL / 0 /          ! Label column number
C      DATA NCOLS / 0 /           ! Number of columns in the data file
C      DATA NDAT / 0 /            ! Number of data read
C      DATA SYMCOL / 0 /          ! Symbol column number
C      DATA XCOL / 0 /            ! X-axis column number
C      DATA YCOL / 0 /            ! Y-axis column number
C      DATA ZCOL / 0 /            ! Z-axis column number
C      DATA LXLOG / .FALSE. /     ! Whether X-axis is logarithmic
C      DATA LYLOG / .FALSE. /     ! Whether Y-axis is logarithmic
C      DATA ERRX / NDATMAX * 0.0 / ! X-axis error array
C      DATA ERRY / NDATMAX * 0.0 / ! Y-axis errors array
C      DATA LABANG / MAXLAB * 0.0 / ! Angles of each of the labels
C      DATA LABJUST / MAXLAB * 0.0 / ! Justification of each of the labels
C      DATA XLABAN / MAXLAB * 0.0 / ! X-axis positions of labels
C      DATA XMAX / 1.0 /          ! Data world co-ordinate limit
C      DATA XMIN / -1.0 /         ! Data world co-ordinate limit
C      DATA YLABAN/ MAXLAB * 0.0 / ! Y-axis positions of labels
C      DATA YMAX / 1.0 /          ! Data world co-ordinate limit
C      DATA YMIN / -1.0 /         ! Data world co-ordinate limit
C      DATA ZDATA / NDATMAX * 0.0 / ! Z-axis data array
C      DATA XINC / 1.0D0 /
C      DATA YINC / 1.0D0 /
C      DATA IMROT / 0.0D0 /

*  Character variables common block.
C      DATA CLABELS / NDATMAX * ' ' / ! Data labels
C      DATA LABLST / MAXLAB * ' ' / ! List of labels
C      DATA COLLAB / MAXCOL * ' ' / ! Data column headers

*  Local variables:
      INTEGER I                    ! Loop variable
*.
      XINC = 1.0D0
      YINC = 1.0D0
      IMROT = 0.0D0
      ERXCOL = 0                   ! X-axis error column number
      ERYCOL = 0                   ! Y-axis error column number
      ILABPTR = 0                  ! Pointer to current label
      LABCOL = 0                   ! Label column number
      NCOLS = 0                    ! Number of columns in the data file
      NDAT = 0                     ! Number of data read
      SYMCOL = 0                   ! Symbol column number
      XCOL = 0                     ! X-axis column number
      YCOL = 0                     ! Y-axis column number
      ZCOL = 0                     ! Z-axis column number
      LXLOG = .FALSE.              ! Whether X-axis is logarithmic
      LYLOG = .FALSE.              ! Whether Y-axis is logarithmic
      XMAX = 1.0                   ! Data world co-ordinate limit
      XMIN = -1.0                  ! Data world co-ordinate limit
      YMAX = 1.0                   ! Data world co-ordinate limit
      YMIN = -1.0                  ! Data world co-ordinate limit

      DO 1 I = 1, NDATMAX
         CLABELS( I ) = ' '        ! Column labels
         ERRX( I ) = 0.0           ! X-axis error array
         ERRY( I ) = 0.0           ! Y-axis errors array
         ISYMBS( I ) = 0           ! Symbol numbers for each data point
         XDATA( I ) = 0.0D0        ! X-axis data array
         YDATA( I ) = 0.0D0        ! Y-axis data array
         ZDATA( I )  = 0.0         ! Z-axis data array
 1    CONTINUE
      DO 2 I = 1, MAXLAB
         LABANG( I ) = 0.0         ! Angles of each of the labels
         LABJUST( I ) = 0.0        ! Justification of each of the labels
         XLABAN( I ) = 0.0         ! X-axis positions of labels
         YLABAN( I ) = 0.0         ! Y-axis positions of labels
         LABLST( I ) = ' '
 2    CONTINUE
      DO 3 I = 1, MAXCOL
         COLLAB( I ) = ' '
 3    CONTINUE
      END
* $Id$
