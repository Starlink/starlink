      SUBROUTINE DSA_MAP_COMPLEX( DSAREF, MODE, TYPE,
     :                            RADDR, IADDR, MSLOT, STATUS )
*+
*  Name:
*     DSA_MAP_COMPLEX

*  Purpose:
*     Map the main data array in an NDF as a complex array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA_MAP_COMPLEX( DSAREF, MODE, TYPE,
*        RADDR, IADDR, MSLOT, STATUS )

*  Description:
*     This routine maps the real and imaginary parts of a complex main
*     data array in an NDF. If DSA_USE_QUALITY or DSA_USE_FLAGGED_VALUES
*     have been called for the reference name in question, then a
*     warning will be given that the data will still be cleaned of any
*     bad values. This routine will re-set the flags for using quality
*     or bad values, so that a call to DSA_MAP_QUALITY will fail.
*
*     The returned data array is guaranteed to be free of bad
*     values. This however means that any bad values in the actual data
*     have been filled in with fake values that only appear to be good.

*  Arguments:
*     DSAREF = CHARACTER * ( * ) (Given)
*        The reference name associated with the NDF.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode, 'READ', 'WRITE', or 'UPDATE'.
*     TYPE = CHARACTER * ( * ) (Given)
*        The data type to be used to map the array. E.g. 'FLOAT' or
*        'DOUBLE'.
*     RADDR = INTEGER (Returned)
*        The memory address of the mapped real part.
*     IADDR = INTEGER (Returned)
*        The memory address of the mapped imaginary part.
*     MSLOT = INTEGER (Returned)
*        The map slot, a handle that can be used to unmap this array
*        later. Both parts, real and imaginary, are unmapped with one
*        call.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     mjc: Malcolm Currie (RAL, Starlink)
*     {enter_new_authors_here}

*  History:
*     06 Mar 1996 (hme):
*        Original version.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      CHARACTER * ( * ) DSAREF
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) TYPE

*  Arguments Returned:
      INTEGER RADDR
      INTEGER IADDR
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL MODIFD             ! Whether U or W access
      INTEGER SLOT               ! The reference slot
      CHARACTER * ( NDF__SZTYP ) NDFTYP ! The type in NDF speak

*.

*  Check inherited global status.
      IF ( STATUS .NE. 0 ) RETURN

*  Begin error context and translate status.
      CALL ERR_MARK
      STATUS = SAI__OK

*  Find reference slot and translate data type.
      CALL DSA1_RFND( DSAREF, SLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL DSA1_NDFTYP( TYPE, NDFTYP, STATUS )

*  Is the access mode modifying?
      MODIFD = INDEX( 'UuWw', MODE(:1) ) .NE. 0

*  Check and re-set the flags for using quality and bad values.
      IF ( DSA__REFBAD(SLOT) .OR. DSA__REFQUA(SLOT) ) THEN
         CALL MSG_SETC( 'FDA_T001', DSA__REFNAM(SLOT) )
         CALL MSG_OUT( 'FDA_M015', 'Warning: Use of quality or ' //
     :      'acceptance of bad values have been declared prior to ' //
     :      'mapping the complex data array in ^FDA_T001. These ' //
     :      'declarations are ignored and the flags are now reset.',
     :      STATUS )
         DSA__REFBAD(SLOT) = .FALSE.
         DSA__REFQUA(SLOT) = .FALSE.
      END IF

*  Call the work routine, modelled on DSA1_MDAT.
      CALL DSA1_MCMPLX( SLOT, MODE, NDFTYP,
     :   RADDR, IADDR, MSLOT, STATUS )

*  Set the modification flags for data and quality.
      IF ( MODIFD ) THEN
         DSA__REFMDD(SLOT) = .TRUE.
         DSA__REFMDQ(SLOT) = .TRUE.
      END IF

*  Tidy up.
 500  CONTINUE

*  Translate status and end error context.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = 1
      ELSE
         STATUS = 0
      END IF
      CALL ERR_RLSE

*  Return.
      END



      SUBROUTINE DSA1_MCMPLX( SLOT, MODE, NDFTYP,
     :   RADDR, IADDR, MSLOT, STATUS )
*+
*  Name:
*     DSA1_MCMPLX

*  Purpose:
*     Map a complex data array in an NDF, cleaning bad values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DSA1_MCMPLX( SLOT, MODE, NDFTYP,
*        RADDR, IADDR, MSLOT, STATUS )

*  Description:
*     This routine maps real and imaginary parts of a complex main data
*     array in an NDF, returning the addresses of the dynamic memory
*     arrays that may be used to access the real and imaginary parts.
*     This routine makes sure there are no bad values in
*     the mapped arrays. If necessary, bad-value information will be
*     moved to a separate quality array, which for update access may
*     become permanent.

*  Arguments:
*     SLOT = INTEGER (Given)
*        The reference slot associated with the NDF.
*     MODE = CHARACTER * ( * ) (Given)
*        The access mode, 'READ', 'WRITE', or 'UPDATE'.
*     NDFTYP = CHARACTER * ( * ) (Given)
*        The data type in NDF speak to be used to map the array. E.g.
*        '_REAL' or '_DOUBLE'.
*     RADDR = INTEGER (Returned)
*        The memory address of the mapped real part.
*     IADDR = INTEGER (Returned)
*        The memory address of the mapped imaginary part.
*     MSLOT = INTEGER (Returned)
*        The map slot, a handle that can be used to unmap this array
*        later. Both parts, real and imaginary, are unmapped with one
*        call.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     For read access to data with bad values, the real and imaginary
*     parts are cleaned of bad values independently. Where a pixel is
*     bad due to non-zero quality, this will make both parts bad and
*     both parts are cleaned. But where a pixel is bad due to the
*     flagging value in the data array, it is possible that only one of
*     the two parts of the complex number is bad. Then only the bad part
*     is cleaned.
*
*     For update access to data with bad values the real part is
*     combined with quality first, and the imaginary part is combined
*     with quality second. The quality that is created will naturally
*     flag both parts as bad. But the cleaning in of bad values in the
*     two parts of the data array may be different. The real part will
*     be cleaned only where the real part was bad, the imaginary part
*     will be filled in where either part was bad.
*
*     The data pointer in the reference slot receives the pointer to the
*     real part. In fact the data pointer in the reference slot is never
*     used, but all data mapping routines do set it.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     06 Mar 1996 (hme):
*        Original version.
*     2005 May 31 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     2005 Aug 15 (TIMJ):
*        Use PRM_PAR for BADBIT initialiser
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'PRM_PAR'          ! For NUM__MAXUB

*  Global Variables:
      INCLUDE 'DSA_COMMON'       ! DSA global variables

*  Arguments Given:
      INTEGER SLOT
      CHARACTER * ( * ) MODE
      CHARACTER * ( * ) NDFTYP

*  Arguments Returned:
      INTEGER RADDR
      INTEGER IADDR
      INTEGER MSLOT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      BYTE BADBIT
      PARAMETER ( BADBIT = NUM__MAXUB ) ! Bad bit mask for new quality

*  Local Variables:
      LOGICAL EXIST              ! Whether there are bad values
      LOGICAL QTHERE             ! Whether there is a quality array
      LOGICAL COMPLX             ! Whether data already complex
      INTEGER PLACE              ! NDF placeholder
      INTEGER NELM               ! Mapped array size
      INTEGER QPTR               ! Quality array pointer
      CHARACTER * ( NDF__SZTYP ) TYPE ! Numeric type of data

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Preliminaries.
*  ==============

*  Find map slot, see if quality might be there for use.
*  Also see if data already complex or not.
      CALL DSA1_MSNEW( MSLOT, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL NDF_STATE( DSA__REFID1(SLOT), 'QUALITY', QTHERE, STATUS )
      CALL NDF_CMPLX( DSA__REFID1(SLOT), 'DATA', COMPLX, STATUS )


*  If not write access.
*  ====================

      IF ( MODE(:1) .EQ. 'R' .OR. MODE(:1) .EQ. 'r' .OR.
     :     MODE(:1) .EQ. 'U' .OR. MODE(:1) .EQ. 'u' ) THEN


*     Map data cautiously and see if bad values present.
         CALL NDF_MAPZ( DSA__REFID1(SLOT), 'DATA', NDFTYP, 'READ',
     :      RADDR, IADDR, NELM, STATUS )
         CALL NDF_BAD( DSA__REFID1(SLOT), 'DATA', .TRUE.,
     :      EXIST, STATUS )


*     If read access to data with bad values.
*     =======================================

         IF ( MODE(:1) .EQ. 'R' .OR. MODE(:1) .EQ. 'r' ) THEN
            IF ( EXIST ) THEN

*           Discard original data.
*           Get a temporary NDF to hold the data and quality.
*           Map data (merging quality in).
*           Clean the data.
*           Fill in reference slot and map slot.
               CALL NDF_UNMAP( DSA__REFID1(SLOT), 'DATA', STATUS )
               CALL NDF_TEMP( PLACE, STATUS )
               CALL NDF_SCOPY( DSA__REFID1(SLOT),
     :            'DATA,QUALITY,NOTITLE,NOLABEL,' //
     :            'NOHISTORY,NOEXTENSION()',
     :            PLACE, DSA__REFID2(SLOT), STATUS )
               CALL NDF_MAPZ( DSA__REFID2(SLOT), 'DATA', NDFTYP,
     :            'UPDATE', RADDR, IADDR, NELM, STATUS )
               CALL DSA1_CLEAN( SLOT, NDFTYP, NELM,
     :            %VAL( CNF_PVAL(RADDR) ), STATUS )
               CALL DSA1_CLEAN( SLOT, NDFTYP, NELM,
     :            %VAL( CNF_PVAL(IADDR) ), STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DSA__REFDPT(SLOT) = RADDR
                  DSA__MAPUSD(MSLOT) = .TRUE.
                  DSA__MAPID1(MSLOT) = DSA__REFID2(SLOT)
                  DSA__MAPREF(MSLOT) = SLOT
                  DSA__MAPNAM(MSLOT) = 'DATA'
               END IF


*     Else if read access, but no bad values present.
*     ===============================================

            ELSE

*           Fill in reference slot and map slot.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DSA__REFDPT(SLOT) = RADDR
                  DSA__MAPUSD(MSLOT) = .TRUE.
                  DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
                  DSA__MAPREF(MSLOT) = SLOT
                  DSA__MAPNAM(MSLOT) = 'DATA'
               END IF

            END IF


*     Else if update access to data with bad values.
*     ==============================================

         ELSE IF ( MODE(:1) .EQ. 'U' .OR. MODE(:1) .EQ. 'u' ) THEN
            IF ( EXIST ) THEN

*           Unmap the data again.
*           If not yet complex type, change type.
*           Map quality for update or write/zero.
*           Re-map data for update.
*           Merge quality into bad values.
*           Merge bad values into quality.
*           Clean the data.
*           Set bad pixel flag.
*           Fill in reference slot and map slot.
               CALL NDF_UNMAP( DSA__REFID1(SLOT), 'DATA', STATUS )
               IF ( .NOT. COMPLX ) THEN
                  CALL NDF_TYPE( DSA__REFID1(SLOT), 'DATA',
     :               TYPE, STATUS )
                  CALL NDF_STYPE( 'COMPLEX' // TYPE,
     :               DSA__REFID1(SLOT), 'DATA', STATUS )
               END IF
               IF ( QTHERE ) THEN
                  CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE',
     :               MODE, QPTR, NELM, STATUS )
               ELSE
                  CALL NDF_MAP( DSA__REFID1(SLOT), 'QUALITY', '_UBYTE',
     :               'WRITE/ZERO', QPTR, NELM, STATUS )
                  CALL NDF_SBB( BADBIT, DSA__REFID1(SLOT), STATUS )
               END IF
               CALL NDF_MAPZ( DSA__REFID1(SLOT), 'DATA', NDFTYP, MODE,
     :            RADDR, IADDR, NELM, STATUS )
               CALL DSA1_MRGFQ( NDFTYP, NELM, %VAL( CNF_PVAL(RADDR) ),
     :            %VAL( CNF_PVAL(QPTR) ), STATUS )
               CALL DSA1_CLEAN( SLOT, NDFTYP, NELM,
     :            %VAL( CNF_PVAL(RADDR) ), STATUS )
               CALL DSA1_MRGFQ( NDFTYP, NELM, %VAL( CNF_PVAL(IADDR) ),
     :            %VAL( CNF_PVAL(QPTR) ), STATUS )
               CALL DSA1_CLEAN( SLOT, NDFTYP, NELM,
     :            %VAL( CNF_PVAL(IADDR) ), STATUS )
               CALL NDF_SBAD( .FALSE., DSA__REFID1(SLOT), 'DATA',
     :            STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DSA__REFDPT(SLOT) = RADDR
                  DSA__REFQPT(SLOT) = QPTR
                  DSA__MAPUSD(MSLOT) = .TRUE.
                  DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
                  DSA__MAPREF(MSLOT) = SLOT
                  DSA__MAPNAM(MSLOT) = 'DATA,QUALITY'
               END IF


*     Else if update access, but no bad values present.
*     =================================================

            ELSE

*           Unmap the data and re-map them with the proper access mode.
*           Delete quality in the meantime.
*           If not yet complex type, change type in the meantime as well.
*           Set bad pixel flag.
*           Fill in reference slot and map slot.
               CALL NDF_UNMAP( DSA__REFID1(SLOT), 'DATA', STATUS )
               IF ( QTHERE )
     :            CALL NDF_RESET( DSA__REFID1(SLOT), 'QUALITY', STATUS )
               IF ( .NOT. COMPLX ) THEN
                  CALL NDF_TYPE( DSA__REFID1(SLOT), 'DATA',
     :               TYPE, STATUS )
                  CALL NDF_STYPE( 'COMPLEX' // TYPE,
     :               DSA__REFID1(SLOT), 'DATA', STATUS )
               END IF
               CALL NDF_MAPZ( DSA__REFID1(SLOT), 'DATA', NDFTYP, MODE,
     :            RADDR, IADDR, NELM, STATUS )
               CALL NDF_SBAD( .FALSE., DSA__REFID1(SLOT), 'DATA',
     :            STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DSA__REFDPT(SLOT) = RADDR
                  DSA__MAPUSD(MSLOT) = .TRUE.
                  DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
                  DSA__MAPREF(MSLOT) = SLOT
                  DSA__MAPNAM(MSLOT) = 'DATA'
               END IF

            END IF

         END IF

*  Else (write access)
*  ===================

      ELSE

*     If not yet complex type, change type.
*     Map data with the proper access mode.
*     Delete quality.
*     Set bad pixel flag.
*     Fill in reference slot and map slot.
         IF ( .NOT. COMPLX ) THEN
            CALL NDF_TYPE( DSA__REFID1(SLOT), 'DATA', TYPE, STATUS )
            CALL NDF_STYPE( 'COMPLEX' // TYPE,
     :         DSA__REFID1(SLOT), 'DATA', STATUS )
         END IF
         CALL NDF_MAPZ( DSA__REFID1(SLOT), 'DATA', NDFTYP, MODE,
     :      RADDR, IADDR, NELM, STATUS )
         IF ( QTHERE )
     :      CALL NDF_RESET( DSA__REFID1(SLOT), 'QUALITY', STATUS )
         CALL NDF_SBAD( .FALSE., DSA__REFID1(SLOT), 'DATA', STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            DSA__REFDPT(SLOT) = RADDR
            DSA__MAPUSD(MSLOT) = .TRUE.
            DSA__MAPID1(MSLOT) = DSA__REFID1(SLOT)
            DSA__MAPREF(MSLOT) = SLOT
            DSA__MAPNAM(MSLOT) = 'DATA'
         END IF

      END IF


*  Return.
*  =======

 500  CONTINUE
      END
