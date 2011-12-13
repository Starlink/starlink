      SUBROUTINE KPS1_MEMCP( ILEVEL, MODEL, STATUS )
*+
*  Name:
*     KPS1_MEMCP

*  Purpose:
*     Sets up the MEMSYS3 "areas" needed by MEM2D.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MEMCP( ILEVEL, MODEL, STATUS )

*  Description:
*     The common block /MECOMP/ required by MEMSYS3 is initialised.
*
*     If possible, all areas (i.e. space to hold images or data sets)
*     required by the program are stored internally in the ME_ST array
*     in common block /MECOMS/.  If there is not room in ME_ST to do
*     this, all the areas are stored in a single dynamically allocated
*     work array and room for a single buffer for each area is
*     allocated in ME_ST using the system for overlaying buffers
*     described in the MEMSYS3 manual. (NOTE, the manual says you can
*     overlay the buffers for areas 22 and 27, but you can't).  The
*     address as which each area starts is stored in common block
*     C1_COM.  The indices within the whole array, at which each area
*     starts is stored as the MEMSYS3 `allocation pointers' in
*     /MECOMS/.
*
*     If MODEL=CONSTANT, the model is a constant and therefore area
*     <20> is not needed.  Otherwise, area <20> is needed.
*
*     It is assumed that the output and input pixel sizes are the same.
*     It is also assumed that the PSF image will be stored in area <3>.
*
*     The image dimensions stored in C1_COM should include a suitable
*     blank margin to overcome edge effects caused by the convolutions.

*  Arguments:
*     ILEVEL = INTEGER (Given)
*        The amount of user information to display.  If ILEVEL is one
*        or more, then the percentage of internal storage used is
*        displayed, together with a warning if external storage is
*        being used.
*     MODEL = CHARACTER (Given)
*        If as constant model value is to be used in MEMSYS3, then MODEL
*        has the value 'CONSTANT'.  If a variable model is to be used,
*        MODEL has the value 'NDF'.  A variable model is stored in
*        area <20>.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1990 (DSB):
*        Original version.
*     25-FEB-1991 (DSB):
*        Argument MODEL added, to provide facility for using
*        non-constant models.
*      4-MAY-1991 (DSB):
*        Name changed from SETCMP to KPS1_SETCP.
*     1991 July 18 (MJC):
*        Name changed from KPS1_SETCP to KPS1_MEMCP.
*     17-MAR-1995 (DSB):
*        Changed to allow images to be stored in an external work array
*        if the internal work array is too small.  Re-formatted to
*        EDSTAR style.
*     1995 April 7 (MJC):
*        Typo's and minor stylistic changes.  Used modern-style variable
*        declarations.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Global Variables:
      INCLUDE 'ME_COM'           ! Common blocks required by MEMSYS3
*        ME_NJ = INTEGER (Write)
*           Number of buffers per internal "image".
*        ME_NJ = INTEGER (Write)
*           Number of elements per internal "image" buffer.
*        ME_NK = INTEGER (Write)
*           Number of buffers per internal "data set".
*        ME_MK = INTEGER (Write)
*           Number of elements per internal "data set" buffer.
*        ME_KA( 40 ) = INTEGER (Write)
*           Pointers away from internal storage for each area.  If set
*           to zero then the corresponding area is stored internally.
*           Otherwise, it is the index of the first element of the area
*           within the external work array.
*        ME_KB( 40 ) = INTEGER (Write)
*           Base address of the internal buffer for each area.
*        ME_OUT = INTEGER (Write)
*           Fortran I/O unit number for MEMSYS3 diagnostics.

      INCLUDE 'C1_COM'          ! Common block used to communicate with
                                ! OPUS and TROPUS.
*        C1_WEXT = LOGICAL (Write)
*           A flag indicating if external storage is being used.  If
*           not, all areas are stored internally in /MECOMS/.
*        C1_IP0 = INTEGER (Write)
*           Pointer to the mapped external work array which holds all
*           the areas.
*        C1_IP( 40 ) = INTEGER (Write)
*           Pointers to the start of each external area.  These are
*           derived from C1_IP0.
*        C1_NPX = INTEGER (Read)
*           The x dimension of all images and data sets (including
*           margin).
*        C1_NLN = INTEGER (Read)
*           The y dimension of all images and data sets (including
*           margin).

*  Arguments Given:
      INTEGER ILEVEL
      CHARACTER MODEL * ( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER AREA               ! Current area number
      INTEGER EASIZE             ! Size of an external area
      INTEGER IASIZE             ! Size of an internal area
      INTEGER IB1                ! Base pointer for buffer 1
      INTEGER IB2                ! Base pointer for buffer 2
      INTEGER IB3                ! Base pointer for buffer 3
      INTEGER IB4                ! Base pointer for buffer 4
      INTEGER IB5                ! Base pointer for buffer 5
      INTEGER MXB                ! Maximum size of a buffer
      INTEGER NAREA              ! Total no. of areas required
      REAL PERCEN                ! Percentage of internal storage used

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the total number of elements in a area if the area is
*  stored internally.  This is just the size of an image.  Externally
*  stored areas may need to be a little bigger to ensure that they
*  correspond to an integer number of buffers.
      IASIZE = C1_NPX * C1_NLN

*  Store the number of areas needed.  N.B., one area is reserved to
*  hold the PSF.  An extra area is needed if a variable model is to be
*  used.
      IF ( MODEL .EQ. 'CONSTANT' ) THEN
         NAREA = 11
      ELSE
         NAREA = 12
      END IF

*  See if there is enough space in the ME_ST array in common block
*  /MECOMS/ to store all the required areas.  Find the percentage of
*  the internal space which would be used, and if required, tell
*  the user.
      PERCEN = REAL( NAREA * IASIZE ) / REAL( ME_MEM ) * 100.0

      IF ( ILEVEL .GE. 1 ) THEN
         CALL MSG_OUT( 'REPORT', ' ', STATUS )
         CALL MSG_SETI( 'PERCEN', NINT( PERCEN ) )
         CALL MSG_OUT( 'REPORT', '  ^PERCEN % of internal memory used.',
     :                 STATUS )
      END IF

*  If there is sufficient internal memory, all areas will be stored
*  within /MECOMS/.  Set the flag stored in common to indicate this.
      IF ( PERCEN .LT. 100.0 ) THEN
         C1_WEXT = .FALSE.

*  Set the number of buffers per area to 1.
         ME_NJ = 1
         ME_NK = 1

*  Each buffer will hold a complete area.  Store the buffer sizes.
         ME_MJ = IASIZE
         ME_MK = IASIZE

*  Set all the allocation pointers to zero.  This indicates to MEMSYS3
*  that all areas are stored internally.  Also set all base pointers
*  and external pointers to zero.
         DO AREA = 1, 40
            ME_KA( AREA ) = 0
            ME_KB( AREA ) = 0
            C1_IP( AREA ) = 0
         END DO

*  Set up the base pointers to all required areas.  N.B., the FFT of
*  the PSF will be stored in area <3>.
         ME_KB( 1 ) = 1
         ME_KB( 2 ) = ME_KB( 1 ) + ME_MJ
         ME_KB( 3 ) = ME_KB( 2 ) + ME_MJ
         ME_KB( 21 ) = ME_KB( 3 ) + ME_MJ
         ME_KB( 22 ) = ME_KB( 21 ) + ME_MK
         ME_KB( 23 ) = ME_KB( 22 ) + ME_MK
         ME_KB( 24 ) = ME_KB( 23 ) + ME_MK
         ME_KB( 25 ) = ME_KB( 24 ) + ME_MK
         ME_KB( 26 ) = ME_KB( 25 ) + ME_MK
         ME_KB( 27 ) = ME_KB( 26 ) + ME_MK
         ME_KB( 28 ) = ME_KB( 27 ) + ME_MK

*  If necessary, define area <20> to hold a variable model.
         IF ( MODEL. NE. 'CONSTANT' ) ME_KB( 20 ) = ME_KB( 28 ) + ME_MK

*  Image areas <4> and <5> are used as work space at various places in
*  MEM2D.  They are NOT used in MEMSYS3.  In the current version of
*  MEM2D the images and data sets are the same size, so space can be
*  saved by overlaying <4> and <5> on two of the data sets which are
*  only used inside MEMSYS3.
         ME_KB( 4 ) = ME_KB( 27 )
         ME_KB( 5 ) = ME_KB( 28 )

*  If there is insufficient room in /MECOMS/ to store all the required
*  areas, all areas will be stored in a single dynamically allocated
*  array, and /MECOMS/ will be used to store a buffer for each area.
*  Set a flag in common to indicate this, and if required, tell the
*  user.
      ELSE
         C1_WEXT = .TRUE.
         IF ( ILEVEL .GE. 1 ) CALL MSG_OUT( 'REPORT', '  External '/
     :     /'memory will be used instead.', STATUS )

*  Only 5 separate internal buffers are required since some buffers can
*  overlay each other (see MEMSYS3 manual - but note that areas 22 and
*  27 cannot overlay each other, hence 5 rather than 4 buffers are
*  used).  Find the maximum size of a buffer.
         MXB = ME_MEM/5

*  Find the number of buffers (rounded up) required for a complete
*  area.
         ME_NJ = 1 + ( IASIZE - 1 ) / MXB
         ME_NK = ME_NJ

*  Find the smallest buffer size which can be used, given that we have
*  to get a whole image into ME_NK buffers.
         ME_MJ = 1 + ( IASIZE - 1 ) / ME_NJ
         ME_MK = ME_MJ

*  Find the total size of an external area.
         EASIZE = ME_MJ * ME_NJ

*  Allocate dynamic memory to store the required number of areas.
         CALL PSX_CALLOC( NAREA * EASIZE, '_REAL', C1_IP0, STATUS )

*  Store the index within ME_ST at which each of the four buffers start.
         IB1 = 1
         IB2 = IB1 + ME_MJ
         IB3 = IB2 + ME_MJ
         IB4 = IB3 + ME_MJ
         IB5 = IB4 + ME_MJ

*  Initialise all the allocation, base and memory pointers to zero.
         DO AREA = 1, 40
            ME_KA( AREA ) = 0
            ME_KB( AREA ) = 0
            C1_IP( AREA ) = 0
         END DO

*  Set up pointers to all required areas.  Base pointers are only
*  needed for areas which are accessed from MEMSYS3 subroutines (some
*  areas are only used outside MEMSYS3 as convenient work arrays).
         ME_KA( 1 ) = 1
         C1_IP( 1 ) = C1_IP0
         ME_KB( 1 ) = IB2

         ME_KA( 2 ) = ME_KA( 1 ) + EASIZE
         C1_IP( 2 ) = C1_IP( 1 ) + EASIZE * VAL__NBR
         ME_KB( 2 ) = IB1

         ME_KA( 3 ) = ME_KA( 2 ) + EASIZE
         C1_IP( 3 ) = C1_IP( 2 ) + EASIZE * VAL__NBR

         ME_KA( 21 ) = ME_KA( 3 ) + EASIZE
         C1_IP( 21 ) = C1_IP( 3 ) + EASIZE * VAL__NBR
         ME_KB( 21 ) = IB2

         ME_KA( 22 ) = ME_KA( 21 ) + EASIZE
         C1_IP( 22 ) = C1_IP( 21 ) + EASIZE * VAL__NBR
         ME_KB( 22 ) = IB3

         ME_KA( 23 ) = ME_KA( 22 ) + EASIZE
         C1_IP( 23 ) = C1_IP( 22 ) + EASIZE * VAL__NBR
         ME_KB( 23 ) = IB2

         ME_KA( 24 ) = ME_KA( 23 ) + EASIZE
         C1_IP( 24 ) = C1_IP( 23 ) + EASIZE * VAL__NBR
         ME_KB( 24 ) = IB4

         ME_KA( 25 ) = ME_KA( 24 ) + EASIZE
         C1_IP( 25 ) = C1_IP( 24 ) + EASIZE * VAL__NBR
         ME_KB( 25 ) = IB1

         ME_KA( 26 ) = ME_KA( 25 ) + EASIZE
         C1_IP( 26 ) = C1_IP( 25 ) + EASIZE * VAL__NBR
         ME_KB( 26 ) = IB2

         ME_KA( 27 ) = ME_KA( 26 ) + EASIZE
         C1_IP( 27 ) = C1_IP( 26 ) + EASIZE * VAL__NBR
         ME_KB( 27 ) = IB5

         ME_KA( 28 ) = ME_KA( 27 ) + EASIZE
         C1_IP( 28 ) = C1_IP( 27 ) + EASIZE * VAL__NBR
         ME_KB( 28 ) = IB4

*  If necessary, define area <20> to hold a variable model.
         IF ( MODEL. NE. 'CONSTANT' ) THEN
            ME_KA( 20 ) = ME_KA( 28 ) + EASIZE
            C1_IP( 20 ) = C1_IP( 28 ) + EASIZE * VAL__NBR
            ME_KB( 20 ) = IB3
         END IF

*  Areas <4> and <5> are used as work space at various places in MEM2D.
*  They are NOT used in MEMSYS3 so space can be saved by overlaying <4>
*  and <5> on two of the data sets which are only used inside MEMSYS3.
         ME_KA( 4 ) = ME_KA( 27 )
         C1_IP( 4 ) = C1_IP( 27 )

         ME_KA( 5 ) = ME_KA( 28 )
         C1_IP( 5 ) = C1_IP( 28 )

      END IF

*  Give an invalid value to the Fortran IO unit which MEMSYS3 would use
*  for writing diagnostic information.  Such I/O is not allowed within
*  ADAM.
      ME_OUT = -1

      IF ( ILEVEL .GE. 1 ) CALL MSG_BLANK( STATUS )

      END
