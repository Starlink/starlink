      SUBROUTINE SIMCB1( IGRP, DETNO, LBND, UBND, C, PNTR, STATUS )
*+
*  Name:
*     SIMCB1

*  Purpose:
*     Return information about a detector PSF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SIMCB1( IGRP, DETNO, LBND, UBND, C, PNTR, STATUS )

*  Description:
*     The names of the NDFs containing the PSFs are supplied in the
*     group identified by IGRP. Element N of the group should contain
*     the name of the NDF which holds the PSF for detector #N. If this
*     NDF cannot be access an error is reported and no further action
*     takes place. If the NDF can be accessed, then the required
*     information is obtained and returned to the calling routine. It is
*     also stored in some internal arrays, so that if the PSF is
*     required again, the information does not need to be obtained from
*     scratch. The NDF is kept open by storing the NDF identifier in one
*     of the internal arrays. These internal arrays have room for 16
*     detectors to be stored simultaneously. If more than 16 PSFs are
*     needed, then the oldest PSF is closed and the slot in the arrays
*     used to store its information is made available for a new PSF.
*
*     Before accessing any PSF, this routine should be called with DETNO
*     set to zero, which causes all the internal arrays and pointers to
*     be initialised. Once all access to the PSFs is complete, this
*     routine should be called with DETNO set to a negative value, which
*     causes all NDFs currently being accessed to be closed.

*  Arguments:
*     IGRP = INTEGER (Given)
*        A GRP identifier for the group holding the names of the NDFs
*        containing the detector PSFs.
*     DETNO = INTEGER (Given)
*        The detector number for which a PSF is reqired (1-62). If a
*        value of zero is supplied, then the internal arrays are
*        initialised, and all other arguments are ignored. If a
*        negative value is supplied, then the all PSF NDFs are closed,
*        and all other arguments are ignored.
*     LBND( 2 ) = INTEGER (Returned)
*        The lower bounds of the PSF.
*     UBND( 2 ) = INTEGER (Returned)
*        The upper bounds of the PSF.
*     C( 6 ) = REAL (Returned)
*        The coefficients of the linear transformation:
*
*        DZ = C1 + C2*I + C3*J
*        DY = C4 + C5*I + C6*J
*
*        where DZ is the offset parallel to the focal plane Z axis, from
*        the detector centre to the centre of the PSF pixel (I,J) (in
*        radians). DY is the offset parallel to the focal plane Y axis.
*     PNTR = INTEGER (Returned)
*        A pointer to the mapped data array of the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'I90_DAT'          ! IRAS90 data

*  Arguments Given:
      INTEGER IGRP
      INTEGER DETNO

*  Arguments Returned:
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )
      REAL C( 6 )
      INTEGER PNTR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NSLOTS             ! The max. no. of PSFs for which
                                 ! information can be held at any
                                 ! one time.
      PARAMETER ( NSLOTS = I90__MAXDT )

*  Local Variables:
      CHARACTER XLOC*(DAT__SZLOC)! Locator to the IRAS extension of the
                                 ! NDF holding the detectors PSF.


      INTEGER DETNUM             ! A detector number.
      INTEGER EL                 ! No. of elements in an object.
      INTEGER INDF               ! An NDF identifier.
      INTEGER NDIM               ! No. of dimensions in a PSF.
      INTEGER NEXT               ! Next slot number to be used.
      INTEGER PSFDET( NSLOTS )   ! Detector number associated with each
                                 ! slot.
      INTEGER PSFLBN( 2, NSLOTS )! Lower bounds of the stored PSFs.
      INTEGER PSFNDF( NSLOTS )   ! NDF identifiers for the stored PSFs.
      INTEGER PSFPNT( NSLOTS )   ! Pointers to the data arrays of the
                                 ! stored PSFs.
      INTEGER PSFUBN( 2, NSLOTS )! Upper bounds of the stored PSFs.
      INTEGER SLOT               ! A slot number.
      INTEGER SLOTS( I90__DETS ) ! The slot currently used for each
                                 ! detector.


      REAL PSFC( 6, NSLOTS )     ! Coefficients of the pixel indices to
                                 ! focal plane transformation for the
                                 ! stored PSFs.

*  The following local variables are assumed to keep their values
*  between calls to this routine.
      SAVE SLOTS, NEXT, PSFDET, PSFNDF, PSFLBN, PSFUBN, PSFC, PSFPNT
*.

*  Check inherited global status, unless a shutdown was requested.
      IF ( STATUS .NE. SAI__OK .AND. DETNO .GE. 0 ) RETURN

*  If an initialisation has been requested, set the slot number for each
*  detector to zero, set the detector number stored in each slot to
*  zero, and set the next free slot number to 1.
      IF( DETNO .EQ. 0 ) THEN

         DO DETNUM = 1, I90__DETS
            SLOTS( DETNUM ) = 0
         END DO

         DO SLOT = 1, NSLOTS
            PSFDET( SLOT ) = 0
         END DO

         NEXT = 1

*  If a shutdown/reinitialisation has been requested, tidy up by
*  releasing the resources used by each PSF for hich information is
*  currently stored. Do all this in a new error reporting context.
      ELSE IF( DETNO .LT. 0 ) THEN
         CALL ERR_BEGIN( STATUS )

         DO SLOT = 1, NSLOTS
            IF( PSFDET( SLOT ) .NE. 0 ) THEN
               CALL NDF_ANNUL( PSFNDF( SLOT ), STATUS )
               SLOTS( PSFDET( SLOT ) ) = 0
               PSFDET( SLOT ) = 0
            END IF
         END DO

         NEXT = 1

         CALL ERR_END( STATUS )

*  If a positive detector number has been supplied, information
*  relating to the detectors PSF is to be returned to the calling
*  routine.
      ELSE

*  Find the slot number (if any) which holds information about this
*  detector.
         SLOT = SLOTS( DETNO )

*  If information relating to the PSF for this detector is already
*  stored in the PSF arrays, get the information out of the arrays and
*  return it.
         IF( SLOT .NE. 0 ) THEN
            LBND( 1 ) = PSFLBN( 1, SLOT )
            UBND( 1 ) = PSFUBN( 1, SLOT )
            LBND( 2 ) = PSFLBN( 2, SLOT )
            UBND( 2 ) = PSFUBN( 2, SLOT )
            C(1) = PSFC( 1, SLOT )
            C(2) = PSFC( 2, SLOT )
            C(3) = PSFC( 3, SLOT )
            C(4) = PSFC( 4, SLOT )
            C(5) = PSFC( 5, SLOT )
            C(6) = PSFC( 6, SLOT )
            PNTR = PSFPNT( SLOT )

*  If information describing this detectors PSF is not currently stored
*  in the arrays...
         ELSE

*  Get an NDF identifier for the PSF.
            CALL NDG_NDFAS( IGRP, DETNO, 'READ', INDF, STATUS )

*  If the PSF was accessed successfully, get the information describing
*  it.
            IF( STATUS .EQ. SAI__OK ) THEN

*  Get a locator to the IRAS extension of the PSF.
               CALL NDF_XLOC( INDF, 'IRAS', 'READ', XLOC, STATUS )

*  Read the coefficients of the linear transformation from pixel indices
*  to focal plane offsets from the detector centre.
               CALL CMP_GET1R( XLOC, 'TRANS', 6, C, EL, STATUS )

*  Annul the locator to the IRAS extension.
               CALL DAT_ANNUL( XLOC, STATUS )

*  Get the bounds of the PSF image for this detector.
               CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )

*  Map the DATA array.
               CALL NDF_MAP( INDF, 'DATA', '_REAL', 'READ', PNTR, EL,
     :                       STATUS )

*  If the next slot to be used already has information stored in it,
*  release the resources used by the PSF stored in the slot, before
*  storing the new information.
               IF( PSFDET( NEXT ) .NE. 0 ) THEN
                  CALL NDF_ANNUL( PSFNDF( NEXT ), STATUS )
                  SLOTS( PSFDET( NEXT ) ) = 0
                  PSFDET( NEXT ) = 0
               END IF

*  Store the new PSF information.
               PSFNDF( NEXT ) = INDF
               PSFC( 1, NEXT ) = C( 1 )
               PSFC( 2, NEXT ) = C( 2 )
               PSFC( 3, NEXT ) = C( 3 )
               PSFC( 4, NEXT ) = C( 4 )
               PSFC( 5, NEXT ) = C( 5 )
               PSFC( 6, NEXT ) = C( 6 )
               PSFLBN( 1, NEXT ) = LBND( 1 )
               PSFUBN( 1, NEXT ) = UBND( 1 )
               PSFLBN( 2, NEXT ) = LBND( 2 )
               PSFUBN( 2, NEXT ) = UBND( 2 )
               PSFPNT( NEXT ) = PNTR

*  Store the detector number associated with this slot.
               PSFDET( NEXT ) = DETNO

*  Store the slot at which information for this detector is stored.
               SLOTS( DETNO ) = NEXT

*  Increment the next slot to be used.
               NEXT = NEXT + 1

*  If the last slot has been reached, start again at the first.
               IF( NEXT .GT. NSLOTS ) NEXT = 1

            END IF

         END IF

      END IF

      END
