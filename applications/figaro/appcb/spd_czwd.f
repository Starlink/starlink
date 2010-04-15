      SUBROUTINE SPD_CZWD( FILE, FU, NDF, XLOC, STATUS )
*+
*  Name:
*     SPD_CZWD

*  Purpose:
*     List Specdre Extension.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZWD( FILE, FU, NDF, XLOC, STATUS )

*  Description:
*     This routine lists the contents of the Specdre Extension either to
*     an ASCII file or to the default output device (user's screen).
*     The list comprises scalars and vectors, but not the contents of
*     NDF arrays.

*  Arguments:
*     FILE = LOGICAL (Given)
*        True if an ASCII file is to be used, has been opened
*        successfully and is identified by the Fortran unit number FU.
*     FU = INTEGER (Given)
*        The Fortran unit number of the output ASCII file. This is
*        unused if FILE is false.
*     NDF = INTEGER (Given)
*        The identifier of the given main NDF.
*        is an extension of.
*     XLOC = CHARACTER * ( * ) (Given)
*        The HDS locator to the Extension. This should be an extension
*        of the main NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine recognises Specdre Extension v. 1.1.

*  Implementation status:
*     This routine does not check the requirement that COORD must either
*     both exist or both be absent.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     15 Mar 1992 (hme):
*        Original version.
*     02 Jul 1992 (hme):
*        Argument list for SPAAU changed.
*        In screen list refractive index would default to "unknown".
*     24 Nov 1994 (hme):
*        Rename from SPAAQ.
*     24 Nov 1995 (hme):
*        Add support for COORD (Extension v. 0.7 -> 1.1)
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
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      LOGICAL FILE
      INTEGER FU
      INTEGER NDF
      CHARACTER * ( * ) XLOC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXIST              ! True if a structure is found
      LOGICAL EXIST2             ! True if a structure is found
      INTEGER IDUMMY             ! Integer argument returned from calls
      INTEGER SPECAX             ! SPECAXIS
      INTEGER FREQUN             ! FREQUNIT
      REAL INDEXR                ! INDEXREFR (REAL)
      REAL FREQRR                ! FREQREF (REAL)
      DOUBLE PRECISION INDEXD    ! INDEXREFR (DOUBLE)
      DOUBLE PRECISION FREQRD    ! FREQREF (DOUBLE)
      CHARACTER * ( 64 ) LABEL   ! SPECVALS label
      CHARACTER * ( 64 ) UNITS   ! SPECVALS unit
      CHARACTER * ( 32 ) RESTFR  ! RESTFRAME
      CHARACTER * ( 78 ) SHORT   ! Short string to write to screen
      CHARACTER * ( NDF__SZTYP ) DTYPE ! Numeric type found
      CHARACTER * ( DAT__SZLOC ) TLOC ! Temporary locator

*.

*
*List of Specdre Extension (v. 1.1)
*
*Name of NDF:         AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA...
*Spectroscopic axis:  I
*Reference frame:     AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
*Refractive index:    F.FFFFFFF
*Refractive index:    F.FFFFFFFFFFFFFFFF
*Reference frequency: G.GGGGGGGEEEE * 10**III Hz
*Reference frequency: G.GGGGGGGGGGGGGGGGEEEE [10**III Hz]
*Spectroscopic values exist and are AAAAAAAAAAAAAAAA [AAAAAAAAAAAAAAAA].
*Spectroscopic values do not exist.
*Spectroscopic widths do exist.
*Spectroscopic widths do not exist.
*First coordinates    exist and are AAAAAAAAAAAAAAAA [AAAAAAAAAAAAAAAA].
*Second cooordinates  exist and are AAAAAAAAAAAAAAAA [AAAAAAAAAAAAAAAA].
*First coordinates    do not exist.
*Second cooordinates  do not exist.
*Covariance row sums  do exist.
*Covariance row sums  do not exist.
*Results do not exist.
*Result structure provides for IIII parameters in III components.
*
*#   line     lab freq.              type    npara mask from    to
*III AAAAAAAA G.GGGGGGGGGGGGGGGGEEEE AAAAAAAA IIII G.GGGGGGEEEE G.GGGGGGEEEE
*III AAAAAAAA G.GGGGGGGGGGGGGGGGEEEE AAAAAAAA IIII G.GGGGGGEEEE G.GGGGGGEEEE
*III AAAAAAAA G.GGGGGGGGGGGGGGGGEEEE AAAAAAAA IIII G.GGGGGGEEEE G.GGGGGGEEEE
*#   line name    lab freq.     comp. type  npara mask from    to
*III AAAAAAAAAAAA G.GGGGGGGEEEE AAAAAAAAAAAA IIII G.GGGGGGEEEE G.GGGGGGEEEE
*III AAAAAAAAAAAA G.GGGGGGGEEEE AAAAAAAAAAAA IIII G.GGGGGGEEEE G.GGGGGGEEEE
*III AAAAAAAAAAAA G.GGGGGGGEEEE AAAAAAAAAAAA IIII G.GGGGGGEEEE G.GGGGGGEEEE
*
*#    parameter type
*IIII AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
*IIII AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
*IIII AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
*
*End of list.
*

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Formats.
 101  FORMAT ( ' Spectroscopic axis:  ', I1 )
 102  FORMAT ( ' Reference frame:     ', A32 )
 103  FORMAT ( ' Refractive index:    ', F18.16 )
 104  FORMAT ( ' Refractive index:    ', F9.7 )
 105  FORMAT ( ' Reference frequency: ', G22.16E2,
     :   ' [10**', I3.3, ' Hz]' )
 106  FORMAT ( ' Reference frequency: ', G13.7E2,
     :   ' [10**', I3.3, ' Hz]' )
 107  FORMAT ( A )
 108  FORMAT ( ' Spectroscopic values exist and are ', A16,
     :         ' [', A16, '].' )
 109  FORMAT ( ' First coordinates    exist and are ', A16,
     :         ' [', A16, '].' )
 110  FORMAT ( ' Second coordinates   exist and are ', A16,
     :         ' [', A16, '].' )


*  File output.
*  ============

*  If list goes to ASCII file.
      IF ( FILE ) THEN

*     Title.
         WRITE ( FU, 107 ) ' '
         WRITE ( FU, 107 ) ' List of Specdre Extension (v. 1.1)'
         WRITE ( FU, 107 ) ' '

*     The NDF specification.
         CALL NDF_MSG( 'CHAR', NDF )
         CALL MSG_LOAD( 'SPD_CZWD_LIST',
     :      ' Name of NDF:         ^CHAR', SHORT, IDUMMY, STATUS )
         WRITE ( FU, 107 ) SHORT

*     Default SPECAXIS, RESTFRAME, FREQUNIT.
         SPECAX = 1
         RESTFR = 'unknown'
         FREQUN = 0
         INDEXD = 1D0
         INDEXR = 1.

*     SPECAXIS.
         CALL DAT_THERE( XLOC, 'SPECAXIS', EXIST, STATUS )
         IF ( EXIST )
     :      CALL CMP_GET0I( XLOC, 'SPECAXIS', SPECAX, STATUS )
         WRITE ( FU, 101 ) SPECAX

*     RESTFRAME.
         CALL DAT_THERE( XLOC, 'RESTFRAME', EXIST, STATUS )
         IF ( EXIST )
     :      CALL CMP_GET0C( XLOC, 'RESTFRAME', RESTFR, STATUS )
         WRITE ( FU, 102 ) RESTFR

*     INDEXREFR: The variable and format used depend on the data type.
         CALL DAT_THERE( XLOC, 'INDEXREFR', EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL CMP_TYPE( XLOC, 'INDEXREFR', DTYPE, STATUS )
            IF ( DTYPE .EQ. '_DOUBLE' ) THEN
               CALL CMP_GET0D( XLOC, 'INDEXREFR', INDEXD, STATUS )
               WRITE ( FU, 103 ) INDEXD
            ELSE
               CALL CMP_GET0R( XLOC, 'INDEXREFR', INDEXR, STATUS )
               WRITE ( FU, 104 ) INDEXR
            END IF
         ELSE
            WRITE ( FU, 104 ) INDEXR
         END IF

*     Reference frequency: This is put together from two scalars. One of
*     them may be _REAL or _DOUBLE.
         CALL DAT_THERE( XLOC, 'FREQREF',  EXIST,  STATUS )
         CALL DAT_THERE( XLOC, 'FREQUNIT', EXIST2, STATUS )

*     If FREQREF exists. (That's enough since FREQUNIT defaults to 0.)
         IF ( EXIST ) THEN

*        Find out type of value.
            CALL CMP_TYPE( XLOC, 'FREQREF', DTYPE, STATUS )

*        Get the unit. (In case it doesn't exist, we already have set
*        the default above.)
            IF ( EXIST2 )
     :         CALL CMP_GET0I( XLOC, 'FREQUNIT', FREQUN, STATUS )

*        If value is _DOUBLE.
            IF ( DTYPE .EQ. '_DOUBLE' ) THEN
               CALL CMP_GET0D( XLOC, 'FREQREF', FREQRD, STATUS )
               IF (  FREQRD .NE. VAL__BADD ) THEN
                  WRITE ( FU, 105 ) FREQRD, FREQUN
               ELSE
                  WRITE ( FU, 107 ) ' Reference frequency: unknown'
               END IF

*        Else (value is _REAL).
            ELSE
               CALL CMP_GET0R( XLOC, 'FREQREF', FREQRR, STATUS )
               IF (  FREQRR .NE. VAL__BADR ) THEN
                  WRITE ( FU, 106 ) FREQRR, FREQUN
               ELSE
                  WRITE ( FU, 107 ) ' Reference frequency: unknown'
               END IF
            END IF

*     Else (FREQREF missing).
         ELSE
            WRITE ( FU, 107 ) ' Reference frequency: unknown'
         END IF

*     If SPECVALS exist.
         CALL DAT_THERE( XLOC, 'SPECVALS', EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL DAT_FIND( XLOC, 'SPECVALS', TLOC, STATUS )

*        Get the label.
            CALL DAT_THERE( TLOC, 'LABEL', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'LABEL', LABEL, STATUS )
            ELSE
               LABEL = 'unknown'
            END IF

*        Get the unit.
            CALL DAT_THERE( TLOC, 'UNITS', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'UNITS', UNITS, STATUS )
            ELSE
               UNITS = 'unknown'
            END IF

*        Annul temporary locator and write the information.
            CALL DAT_ANNUL( TLOC, STATUS )
            WRITE( FU, 108 ) LABEL, UNITS

*     Else (if SPECVALS do not exist).
         ELSE
            WRITE( FU, 107 ) ' Spectroscopic values do not exist.'
         END IF

*     Look for SPECWIDS.
         CALL DAT_THERE( XLOC, 'SPECWIDS', EXIST, STATUS )
         IF ( EXIST ) THEN
            WRITE( FU, 107 ) ' Spectroscopic widths do exist.'
         ELSE
            WRITE( FU, 107 ) ' Spectroscopic widths do not exist.'
         END IF

*     If COORD1 exist.
         CALL DAT_THERE( XLOC, 'COORD1', EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL DAT_FIND( XLOC, 'COORD1', TLOC, STATUS )

*        Get the label.
            CALL DAT_THERE( TLOC, 'LABEL', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'LABEL', LABEL, STATUS )
            ELSE
               LABEL = 'unknown'
            END IF

*        Get the unit.
            CALL DAT_THERE( TLOC, 'UNITS', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'UNITS', UNITS, STATUS )
            ELSE
               UNITS = 'unknown'
            END IF

*        Annul temporary locator and write the information.
            CALL DAT_ANNUL( TLOC, STATUS )
            WRITE( FU, 109 ) LABEL, UNITS

*     Else (if COORD1 do not exist).
         ELSE
            WRITE( FU, 107 ) ' First coordinates    do not exist.'
         END IF

*     If COORD2 exist.
         CALL DAT_THERE( XLOC, 'COORD2', EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL DAT_FIND( XLOC, 'COORD2', TLOC, STATUS )

*        Get the label.
            CALL DAT_THERE( TLOC, 'LABEL', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'LABEL', LABEL, STATUS )
            ELSE
               LABEL = 'unknown'
            END IF

*        Get the unit.
            CALL DAT_THERE( TLOC, 'UNITS', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'UNITS', UNITS, STATUS )
            ELSE
               UNITS = 'unknown'
            END IF

*        Annul temporary locator and write the information.
            CALL DAT_ANNUL( TLOC, STATUS )
            WRITE( FU, 110 ) LABEL, UNITS

*     Else (if COORD2 do not exist).
         ELSE
            WRITE( FU, 107 ) ' Second coordinates   do not exist.'
         END IF

*     Look for COVRS.
         CALL DAT_THERE( XLOC, 'COVRS', EXIST, STATUS )
         IF ( EXIST ) THEN
            WRITE( FU, 107 ) ' Covariance row sums  do exist.'
         ELSE
            WRITE( FU, 107 ) ' Covariance row sums  do not exist.'
         END IF

*     Look for RESULTS.
         CALL DAT_THERE( XLOC, 'RESULTS', EXIST, STATUS )

*     If no results, say so and that's the end of the list.
         IF ( .NOT. EXIST ) THEN
            WRITE( FU, 107 ) ' Results do not exist.'

*     Else (results structure does exist).
         ELSE

*        List result structure.
            CALL SPD_CZWG( FILE, FU, NDF, XLOC, STATUS )
         END IF

*     End of list.
         WRITE( FU, 107 ) ' '
         WRITE( FU, 107 ) ' End of list.'
         WRITE( FU, 107 ) ' '


*  Screen output.
*  ==============

*  Else (list goes to screen etc.)
      ELSE

*     Title.
         SHORT = ' '
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )
         SHORT = ' List of Specdre Extension (v. 1.1)'
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )
         SHORT = ' '
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )

*     The NDF specification.
         CALL NDF_MSG( 'CHAR', NDF )
         CALL MSG_OUT( 'SPD_CZWD_LIST',
     :      ' Name of NDF:         ^CHAR', STATUS )

*     Default SPECAXIS, RESTFRAME, FREQUNIT.
         SPECAX = 1
         RESTFR = 'unknown'
         FREQUN = 0
         INDEXD = 1D0
         INDEXR = 1.

*     SPECAXIS.
         CALL DAT_THERE( XLOC, 'SPECAXIS', EXIST, STATUS )
         IF ( EXIST )
     :      CALL CMP_GET0I( XLOC, 'SPECAXIS', SPECAX, STATUS )
         WRITE ( SHORT, 101 ) SPECAX
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )

*     RESTFRAME.
         CALL DAT_THERE( XLOC, 'RESTFRAME', EXIST, STATUS )
         IF ( EXIST )
     :      CALL CMP_GET0C( XLOC, 'RESTFRAME', RESTFR, STATUS )
         WRITE ( SHORT, 102 ) RESTFR
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )

*     INDEXREFR: The variable and format used depend on the data type.
         CALL DAT_THERE( XLOC, 'INDEXREFR', EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL CMP_TYPE( XLOC, 'INDEXREFR', DTYPE, STATUS )
            IF ( DTYPE .EQ. '_DOUBLE' ) THEN
               CALL CMP_GET0D( XLOC, 'INDEXREFR', INDEXD, STATUS )
               WRITE ( SHORT, 103 ) INDEXD
            ELSE
               CALL CMP_GET0R( XLOC, 'INDEXREFR', INDEXR, STATUS )
               WRITE ( SHORT, 104 ) INDEXR
            END IF
         ELSE
            WRITE ( SHORT, 104 ) INDEXR
         END IF
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )

*     Reference frequency: This is put together from two scalars. One of
*     them may be _REAL or _DOUBLE.
         CALL DAT_THERE( XLOC, 'FREQREF',  EXIST,  STATUS )
         CALL DAT_THERE( XLOC, 'FREQUNIT', EXIST2, STATUS )

*     If FREQREF exists. (That's enough since FREQUNIT defaults to 0.)
         IF ( EXIST ) THEN

*        Find out type of value.
            CALL CMP_TYPE( XLOC, 'FREQREF', DTYPE, STATUS )

*        Get the unit. (In case it doesn't exist, we already have set
*        the default above.)
            IF ( EXIST2 )
     :         CALL CMP_GET0I( XLOC, 'FREQUNIT', FREQUN, STATUS )

*        If value is _DOUBLE.
            IF ( DTYPE .EQ. '_DOUBLE' ) THEN
               CALL CMP_GET0D( XLOC, 'FREQREF', FREQRD, STATUS )
               IF (  FREQRD .NE. VAL__BADD ) THEN
                  WRITE ( SHORT, 105 ) FREQRD, FREQUN
                  CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )
               ELSE
                  SHORT = ' Reference frequency: unknown'
                  CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )
               END IF

*        Else (value is _REAL).
            ELSE
               CALL CMP_GET0R( XLOC, 'FREQREF', FREQRR, STATUS )
               IF (  FREQRR .NE. VAL__BADR ) THEN
                  WRITE ( SHORT, 106 ) FREQRR, FREQUN
                  CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )
               ELSE
                  SHORT = ' Reference frequency: unknown'
                  CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )
               END IF
            END IF

*     Else (FREQREF missing).
         ELSE
            SHORT = ' Reference frequency: unknown'
            CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )
         END IF

*     If SPECVALS exist.
         CALL DAT_THERE( XLOC, 'SPECVALS', EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL DAT_FIND( XLOC, 'SPECVALS', TLOC, STATUS )

*        Get the label.
            CALL DAT_THERE( TLOC, 'LABEL', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'LABEL', LABEL, STATUS )
            ELSE
               LABEL = 'unknown'
            END IF

*        Get the unit.
            CALL DAT_THERE( TLOC, 'UNITS', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'UNITS', UNITS, STATUS )
            ELSE
               UNITS = 'unknown'
            END IF

*        Annul temporary locator and write the information.
            CALL DAT_ANNUL( TLOC, STATUS )
            WRITE( SHORT, 108 ) LABEL, UNITS

*     Else (if SPECVALS do not exist).
         ELSE
            SHORT = ' Spectroscopic values do not exist.'
         END IF
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )

*     Look for SPECWIDS.
         CALL DAT_THERE( XLOC, 'SPECWIDS', EXIST, STATUS )
         IF ( EXIST ) THEN
            SHORT = ' Spectroscopic widths do exist.'
         ELSE
            SHORT = ' Spectroscopic widths do not exist.'
         END IF
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )

*     If COORD1 exist.
         CALL DAT_THERE( XLOC, 'COORD1', EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL DAT_FIND( XLOC, 'COORD1', TLOC, STATUS )

*        Get the label.
            CALL DAT_THERE( TLOC, 'LABEL', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'LABEL', LABEL, STATUS )
            ELSE
               LABEL = 'unknown'
            END IF

*        Get the unit.
            CALL DAT_THERE( TLOC, 'UNITS', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'UNITS', UNITS, STATUS )
            ELSE
               UNITS = 'unknown'
            END IF

*        Annul temporary locator and write the information.
            CALL DAT_ANNUL( TLOC, STATUS )
            WRITE( SHORT, 109 ) LABEL, UNITS

*     Else (if COORD1 do not exist).
         ELSE
            SHORT = ' First coordinates    do not exist.'
         END IF
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )

*     If COORD2 exist.
         CALL DAT_THERE( XLOC, 'COORD2', EXIST, STATUS )
         IF ( EXIST ) THEN
            CALL DAT_FIND( XLOC, 'COORD2', TLOC, STATUS )

*        Get the label.
            CALL DAT_THERE( TLOC, 'LABEL', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'LABEL', LABEL, STATUS )
            ELSE
               LABEL = 'unknown'
            END IF

*        Get the unit.
            CALL DAT_THERE( TLOC, 'UNITS', EXIST2, STATUS )
            IF ( EXIST2 ) THEN
               CALL CMP_GET0C( TLOC, 'UNITS', UNITS, STATUS )
            ELSE
               UNITS = 'unknown'
            END IF

*        Annul temporary locator and write the information.
            CALL DAT_ANNUL( TLOC, STATUS )
            WRITE( SHORT, 110 ) LABEL, UNITS

*     Else (if COORD1 do not exist).
         ELSE
            SHORT = ' Second coordinates   do not exist.'
         END IF
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )

*     Look for COVRS.
         CALL DAT_THERE( XLOC, 'COVRS', EXIST, STATUS )
         IF ( EXIST ) THEN
            SHORT = ' Covariance row sums  do exist.'
         ELSE
            SHORT = ' Covariance row sums  do not exist.'
         END IF
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )

*     Look for RESULTS.
         CALL DAT_THERE( XLOC, 'RESULTS', EXIST, STATUS )

*     If no results, say so and that's the end of the list.
         IF ( .NOT. EXIST ) THEN
            SHORT = ' Results do not exist.'
            CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )

*     Else (result structure does exist).
         ELSE

*        List result structure.
            CALL SPD_CZWG( FILE, FU, NDF, XLOC, STATUS )
         END IF

*     End of list.
         SHORT = ' '
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )
         SHORT = ' End of list.'
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )
         SHORT = ' '
         CALL MSG_OUT( 'SPD_CZWD_LIST', SHORT, STATUS )
      END IF

*  Return
 500  CONTINUE
      END
