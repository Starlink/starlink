      SUBROUTINE SPD_WZKB( INFO, MAX_ARC_LINES, FTR_LIST,
     :   nx, no_of_features, min_dispersion, max_dispersion,
     :   start_wavelength, end_wavelength, max_perm_ftrs,
     :   iden_ftr_position, iden_ftr_wavelength,
     :   start_wavelength_index, end_wavelength_index, status )
*+
*  Name:
*     SPD_WZKB

*  Purpose:
*     FDB: set search limits in wavelength.

*  Language:
*     Fortran

*  Invocation:
*     CALL SPD_WZKB( INFO, MAX_ARC_LINES, FTR_LIST,
*        nx, no_of_features, min_dispersion, max_dispersion,
*        start_wavelength, end_wavelength, max_perm_ftrs,
*        iden_ftr_position, iden_ftr_wavelength,
*        start_wavelength_index, end_wavelength_index, status )

*  Description:
*     This is a modification of Dave Mills' routine GET_WAVE_WINDOW (cf.
*     Mills 1992). "FDB" stands for "feature data base", which is a data
*     base of known features in arc spectra.
*
*     This routine set the search limits for a single order search in
*     terms of indices in the feature database , and min/max
*     dispersion.

*  Arguments:
*     INFO = LOGICAL (Given)
*        If true, some messages are issued.
*     MAX_ARC_LINES = INTEGER (Given)
*        An FDB array dimension.
*     FTR_LIST( MAX_ARC_LINES ) = REAL (Given)
*        The FDB wavelengths (FTR_WAVE).
*     No further information available.

*  References:
*     Mills, D., 1992, Automatic ARC wavelength calibration, in P.J.
*     Grosbol, R.C.E. de Ruijsscher (eds), 4th ESO/ST-ECF Data Analysis
*     Workshop, Garching, 13 - 14 May 1992, ESO Conference and Workshop
*     Proceedings No. 41, Garching bei Muenchen, 1992

*  Authors:
*     djm: Dave Mills (UCL)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     26 Jan 1990 (djm):
*        Original version (GET_WAVE_WINDOW).
*     07 Jun 1993 (hme):
*        Set ABS_MIN/MAX_WAVELENGTH parameters locally: Only restriction
*        is that they must be positive.
*        Add the permanent data base arrays to the argument list and
*        avoid the common block.
*        Make a report where the routine probably intended to do so (by
*        doing an internal write to REPORT_STRING).
*        Widen the window by multiplying with 0.99 and 1.01, no longer
*        by adding -/+ 10.0.
*        Bug in updating dispersion range: it used MAX/MAX when it
*        wanted the more stringent range, now it uses MAX/MIN for the
*        new range min/max.
*     11 Jun 1993 (hme);
*        Remove the global constant that were in the original include
*        file. They are not used in this routine at all.
*        Reduce the artificial intelligence of this routine, i.e. do not
*        update the dispersion range.
*     25 Jan 1995 (hme):
*        Renamed from SPADF.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

      IMPLICIT NONE

*     Input/Output variables used
      LOGICAL INFO
      INTEGER MAX_ARC_LINES
      REAL FTR_LIST( MAX_ARC_LINES )

      INTEGER nx                 !Number of pixels in horizontal dimension
      INTEGER no_of_features     !Number of features in lists
      INTEGER start_wavelength_index !lowest entry in ftr_list to check
      INTEGER end_wavelength_index !Highest entry in ftr_list to check
      INTEGER max_perm_ftrs      !Maximum number of active features
      REAL iden_ftr_position ( max_perm_ftrs ) !Identified feature positions
      REAL iden_ftr_wavelength ( max_perm_ftrs ) !Identified feature wavelengths
      REAL start_wavelength      !Wavelength window start
      REAL end_wavelength        !Wavelength window end
      REAL min_dispersion        !Dispersion window  start
      REAL max_dispersion        !Dispersion window end
      INTEGER status             !Input/Output status condition

*     Local constants
      REAL ABS_MIN_WAVELENGTH
      PARAMETER ( ABS_MIN_WAVELENGTH = 1E-30 )
      REAL ABS_MAX_WAVELENGTH
      PARAMETER ( ABS_MAX_WAVELENGTH = 1E+30 )

*     Local variables
      INTEGER i                  !General loop counter
      REAL scale
      REAL temp
      REAL min_id_pos
      REAL max_id_pos
      REAL min_id_wave
      REAL max_id_wave
      CHARACTER * ( 80 ) REPORT_STRING
      INTEGER IGNORE             ! MSG status

      IGNORE = 0

*%    Look for already identified features
      min_id_pos = 1.0e20
      max_id_pos = -1.0e20
      min_id_wave = 0.0
      max_id_wave = 0.0
      DO i = 1 , max_perm_ftrs
         IF ( iden_ftr_position ( i ) .GT. 0.0 ) THEN
            IF ( min_id_pos .GT. iden_ftr_position ( i ) ) THEN
               min_id_wave = iden_ftr_wavelength ( i )
               min_id_pos = iden_ftr_position ( i )
            ENDIF
            IF ( max_id_pos .LT. iden_ftr_position ( i ) ) THEN
               max_id_wave = iden_ftr_wavelength ( i )
               max_id_pos = iden_ftr_position ( i )
            ENDIF
         ENDIF
      END DO
      IF ( min_id_wave .GT. 0.0 ) THEN
         scale =     ( max_id_wave - min_id_wave ) /
     :               ( max_id_pos - min_id_pos )
         start_wavelength = min_id_wave - scale * min_id_pos
         end_wavelength = min_id_wave + scale * FLOAT ( nx )
         IF ( start_wavelength .GT. end_wavelength ) THEN
            temp = start_wavelength
            start_wavelength = end_wavelength
            end_wavelength = temp
         ENDIF
      ENDIF

*%    If wavelength window start point not set yet then
      IF ( start_wavelength .EQ. 0.0 ) THEN

*%       Set to minimum allowed , and zero start index
         start_wavelength_index = 0
         start_wavelength = abs_min_wavelength

*%    Endif
      ENDIF

*%    If wavelength window end point not set yet then
      IF ( end_wavelength .EQ. 0.0 ) THEN

*%       Set to maximum allowed , and zero end index
         end_wavelength_index = 0
         end_wavelength = abs_max_wavelength

*%    Else
*hme  ELSE

*%       Estimate min/max dispersions using the wavelength window
*%             and use if more stringent than what we have already
*hme     est_min_dispersion = FLOAT ( nx ) * 0.5 /
*hme :                       ( end_wavelength - start_wavelength )
*hme     est_max_dispersion = FLOAT ( nx ) * 4.0 /
*hme :                       ( end_wavelength - start_wavelength )
*hme     min_dispersion = MAX ( est_min_dispersion , min_dispersion )
*hme     max_dispersion = MIN ( est_max_dispersion , max_dispersion )

*%    Endif
      ENDIF

*%    Widen window a bit
      start_wavelength = 0.99 * start_wavelength
      end_wavelength = 1.01 * end_wavelength
      start_wavelength_index = 1

*%    Loop though list of features
      DO i = 1 , no_of_features

*%       If feature is at a lower wavelength than start of window then
*%          Set up feature list index search-start-point
*%       Endif
         IF ( ftr_list ( i ) .LE. start_wavelength )
     :                              start_wavelength_index = i

*%       If end of window is at a higher wavelength than feature then
*%       Endif
         IF ( end_wavelength .GE. ftr_list ( i ) )
     :                              end_wavelength_index = i

*%    End loop
      END DO

*%    Report feature list limits being used
      IF ( INFO ) THEN
         WRITE ( report_string , 1000 )
     :      ftr_list(start_wavelength_index),
     :      ftr_list(end_wavelength_index)
         CALL MSG_OUT( 'FDB_REPORT', REPORT_STRING, IGNORE )
      END IF

*
 1000 FORMAT ( 1X , 'Search ref. feature list between ',F9.2,
     :              ' and ',F9.2 )

      END
