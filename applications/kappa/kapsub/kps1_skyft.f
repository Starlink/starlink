      SUBROUTINE KPS1_SKYFT( LOG, LOGFD, IGRP, NPOS, SCS, EPOCH, PROJ,
     :                       TILT, ORIENT, PSIZE, REFIMG, REFSKY, NP,
     :                       PROJEC, P, RMS, STATUS )
*+
*  Name:
*     KPS1_SKYFT

*  Purpose:
*     Find projection parameter values which are consistent with a given
*     set of sky and image co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SKYFT( LOG, LOGFD, IGRP, NPOS, SCS, EPOCH, PROJ, TILT,
*                      ORIENT, PSIZE, REFIMG, REFSKY, NP, PROJEC, P,
*                      RMS, STATUS )

*  Description:
*     An initial guess is made at the projection parameters, and a PDA
*     routine is called to vary the parameter values until the sum of
*     the squared residuals between the supplied image co-ordinates and
*     the transformed sky co-ordinates is minimised.  This process is
*     repeated for each projection supported by IRA, and the projection
*     giving the smallest residuals is returned.  If explicit values
*     have been supplied for any of the projection parameters, they are
*     omitted from the optimisation.

*  Arguments:
*     LOG = LOGICAL (Given)
*        Should the returned parameter values and residuals be logged to
*        a file?
*     LOGFD = INTEGER (Given)
*        The FIO file descriptor for the log file.  Ignored if LOG is
*        .FALSE.
*     IGRP = INTEGER (Given)
*        GRP identifier for the group holding the formatted versions of
*        the sky and image co-ordinates on which the projection is to
*        be based.  Each position is defined by 4 adjacent elements in
*        this group; the sky longitude, the sky latitude, the image X
*        and the image Y co-ordinates.
*     NPOS = INTEGER (Given)
*        The number of positions in the group identified by IGRP.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky co-ordinate system in which the longitude and latitude
*        values contained in the group identified by IRGP are given.
*     EPOCH = DOUBLE PRECISION (Given)
*        The Julian epoch at which the the sky co-ordinates were
*        determined.
*     PROJ = LOGICAL (Given)
*        Was a specific projection supplied by the user?
*     TILT = LOGICAL (Given)
*        Was a specific value supplied by the user for the tilt of the
*        celestial sphere prior to projection?
*     ORIENT = LOGICAL (Given)
*        Was a specific value supplied by the user for the position
*        angle of the image Y axis?
*     PSIZE = LOGICAL (Given)
*        Was a specific value supplied by the user for the pixel size?
*     REFIMG = LOGICAL (Given)
*        Was a specific value supplied by the user for the image
*        co-ordinates of the reference point.
*     REFSKY = LOGICAL (Given)
*        Was a specific value supplied by the user for the sky
*        co-ordinates of the reference point.
*     NP = INTEGER (Given)
*        The size of array P.
*     PROJEC = CHARACTER * ( * ) (Given and Returned)
*        If PROJ is .TRUE. then this is supplied holding the name of
*        the projection requested by the user.  Otherwise, it's
*        supplied value is ignored and it is returned holding the name
*        of the best projection.
*     P( NP ) = DOUBLE PRECISION (Given and Returned)
*        The projection parameters.  On entry, the array holds any
*        explicit parameter values requested by the user (as indicated
*        by the arguments TILT, ORIENT, PSIZE, REFIMG and REFSKY).
*        Other elements of the array are ignored.  On exit, such values
*        are unchanged, but the other elements are returned holding the
*        best value of the corresponding projection parameter.
*     RMS = REAL (Returned)
*        The RMS residual in pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1996-1998, 2004 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     31-OCT-1994 (DSB):
*        Original version.
*     1996 January 31 (MJC):
*        Replaced the NAG call with PDA.
*     1997 May 22 (MJC):
*        Implemented DSB's 1997 Jan 27 changes made to the NAG version.
*        These improved the security of error checking and reporting.
*     3-DEC-1998 (DSB):
*        Change tolerance used in PDA routine from 1.0D-8 to 1.0D-3 (the
*        smaller value usually prevented the PDA routine from finding a
*        solution).
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRA public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants

*  Global Variables:
      INCLUDE 'SFT_COM'          ! Used for communicating with PDA
                                 ! routine
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
*        EPOCHC = DOUBLE PRECISION (Write)
*           The epoch of the observations.
*        IPWA = INTEGER (Write)
*           Pointer to work space for sky longitude values.
*        IPWB = INTEGER (Write)
*           Pointer to work space for sky latitude values.
*        IPWX = INTEGER (Write)
*           Pointer to work space for supplied image X values.
*        IPWY = INTEGER (Write)
*           Pointer to work space for supplied image Y values.
*        IPWXO = INTEGER (Write)
*           Pointer to work space for temporary image X values.
*        IPWYO = INTEGER (Write)
*           Pointer to work space for temporary image Y values.
*        ISTAT = INTEGER (Write)
*           Local status value.
*        NPOSC = INTEGER (Write)
*           No. of supplied sky positions.
*        ORIENTC = LOGICAL (Write)
*           Was the orientation of the image fixed by the user?
*        PC( 8 ) = DOUBLE PRECISION (Write)
*           The initial guess astrometry parameter values, including any
*           fixed values supplied by the user.
*        PRJECC= CHARACTER * ( IRA__SZPRJ ) (Write)
*           The projection in use.
*        PSIZEC = LOGICAL (Write)
*           Were the pixel dimensions fixed by the user?
*        REFIMC = LOGICAL (Write)
*           Were the pixel co-ordinates of the reference position fixed
*           by the user?
*        REFSKC = LOGICAL (Write)
*           Were the sky co-ordinates of the reference position fixed by
*           the user?
*        SCSC = CHARACTER * ( IRA__SZSCS ) (Write)
*           The sky co-ordinate system to use.
*        TILTC = LOGICAL (Write)
*           Was the tilt of the celestial sphere prior to projection
*           fixed by the user?

*  Arguments Given:
      LOGICAL LOG
      INTEGER LOGFD
      INTEGER IGRP
      INTEGER NPOS
      CHARACTER * ( * ) SCS
      DOUBLE PRECISION EPOCH
      LOGICAL PROJ
      LOGICAL TILT
      LOGICAL ORIENT
      LOGICAL PSIZE
      LOGICAL REFIMG
      LOGICAL REFSKY
      INTEGER NP

*  Arguments Given and Returned:
      CHARACTER * ( * ) PROJEC
      DOUBLE PRECISION P( NP )

*  Arguments Returned:
      REAL RMS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string
      EXTERNAL KPS1_SKYFN        ! Subroutine for evaluating the
                                 ! residuals

*  Local Variables:
      INTEGER END                ! Index of last char in projection name
      DOUBLE PRECISION FS        ! Sum of squared residuals
      DOUBLE PRECISION FSMIN     ! Min. sum of squared residuals so far
      INTEGER I                  ! Loop count
      INTEGER IPWFV              ! Pointer to a work array for the
                                 ! functions evaluated at XC
      INTEGER IPWNA1             ! Pointer to a PDA work array
      INTEGER IPWNA2             ! Pointer to a PDA work array
      INTEGER IFAIL              ! PDA error status
      INTEGER LEND               ! Index of last non-blank char in PRJLST
      INTEGER LW                 ! Size of a PDA work array
      INTEGER N                  ! Number of free parameters
      DOUBLE PRECISION P0( 8 )   ! Best parameters so far
      CHARACTER * ( IRA__SZPRJ ) PRJ ! Best projection so far
      CHARACTER * ( IRA__SZPLS ) PRJLST ! List of recognised projections
      INTEGER START              ! Index of 1st char in projection name
      DOUBLE PRECISION XC( 8 )   ! The free parameters
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get double-precision work space to hold the supplied sky and image
*  co-ordinates.  Store the pointers in the common block used to
*  communicate with the PDA service routine KPS1_SKYFN.
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPWA, STATUS )
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPWB, STATUS )
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPWX, STATUS )
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPWY, STATUS )
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPWXO, STATUS )
      CALL PSX_CALLOC( NPOS, '_DOUBLE', IPWYO, STATUS )

*  Initialise other values in the common block.
      NPOSC = NPOS
      SCSC = SCS
      EPOCHC = EPOCH
      TILTC = TILT
      ORIENTC = ORIENT
      PSIZEC = PSIZE
      REFIMC = REFIMG
      REFSKC = REFSKY

*  See how many parameters are to be included in the optimisation.
      N = 0
      IF ( .NOT. REFIMG ) N = N + 2
      IF ( .NOT. REFSKY ) N = N + 2
      IF ( .NOT. PSIZE ) N = N + 2
      IF ( .NOT. ORIENT ) N = N + 1
      IF ( .NOT. TILT ) N = N + 1

*  Report an error if insufficient positions have been supplied.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      IF ( 2 * NPOS .LT. N ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', ( N + 1 )/2 )
         CALL ERR_REP( 'KPS1_SKYFT_ERR1', 'Too few positions '/
     :                 /'supplied.  Need at least ^N. ', STATUS )
         GO TO 999
      END IF

*  Get work space for the PDA routine.  Since the number of residuals
*  is dynamic, also obtain workspace to store these.
      LW = ( 2 * NPOS + 5 ) * N + 2 * NPOS
      CALL PSX_CALLOC( LW, '_DOUBLE', IPWNA2, STATUS )
      CALL PSX_CALLOC( N, '_INTEGER', IPWNA1, STATUS )
      CALL PSX_CALLOC( 2 * NPOS, '_DOUBLE', IPWFV, STATUS )

*  Read the co-ordinate data from the GRP group into the work arrays
*  and get an initial guess at the projection parameters (returned in
*  P).
      CALL KPS1_SKYF2( IGRP, ORIENT, PSIZE, TILT, REFIMG, REFSKY, SCS,
     :                 NP, NPOS, P, PC, %VAL( CNF_PVAL( IPWA ) ),
     :                 %VAL( CNF_PVAL( IPWB ) ),
     :                 %VAL( CNF_PVAL( IPWX ) ),
     :                 %VAL( CNF_PVAL( IPWY ) ), STATUS )

*  Get a list of the supported projections, and find the indices of the
*  first and last character in the first one in the list.  Each pair of
*  entries in the list are separated by a comma.
      CALL IRA_IPROJ( PRJLST, STATUS )
      START = 1
      END = INDEX( PRJLST, ',' ) - 1

*  Store the index of the last non-blank character in the list.
      LEND = CHR_LEN( PRJLST )

*  A set of optimum parameter values are found for each projection, and
*  the projection which gives the smallest sum of squared residuals is
*  returned as the best projection.  Initialise the smallest sum of
*  squared residuals found so far.
      FSMIN = VAL__MAXD

*  Loop round each projection, extracting the projection name from the
*  list.  The projection name is stored in common so that the PDA
*  service routine knows which projection to use.
      CALL MSG_BLANK( STATUS )

      DO WHILE( END .GE. START .AND. STATUS .EQ. SAI__OK  )
         PRJECC = PRJLST( START : END )

*  Find the best fitting parameters assuming the mapping is described by
*  the current projection.  If the user has supplied an explicit
*  projection, then only do this for the projection supplied.
         IF ( ( .NOT. PROJ ) .OR.
     :        ( PROJ .AND. ( PROJEC .EQ. PRJECC ) ) ) THEN

*  Ignore projection synonyms, unless the user explicitly specified it.
            IF ( PROJ .OR. ( PRJECC .NE. 'TANGENT_PLANE' .AND.
     :                       PRJECC .NE. 'CYLINDRICAL' .AND.
     :                       PRJECC .NE. 'ALL_SKY' ) ) THEN

*  Tell the user which projection is currently being tried.
               CALL MSG_SETC( 'P', PRJECC )
               CALL MSG_OUT( 'KPS1_SKYFT_MSG0', '  Trying ^P ' //
     :                       'projection...', STATUS )

*  Store initial values for the parameters which the PDA routine will
*  search through.
               N = 0

               IF ( .NOT. REFSKY ) THEN
                  N = N + 2
                  XC( N - 1 ) = PC( 1 )
                  XC( N ) = PC( 2 )
               END IF

               IF ( .NOT. REFIMG ) THEN
                  N = N + 2
                  XC( N - 1 ) = PC( 3 )
                  XC( N ) = PC( 4 )
               END IF

               IF ( .NOT. PSIZE ) THEN
                  N = N + 2
                  XC( N - 1 ) = PC( 5 )
                  XC( N ) = PC( 6 )
               END IF

               IF ( .NOT. ORIENT ) THEN
                  N = N + 1
                  XC( N ) = PC( 7 )
               END IF

               IF ( .NOT. TILT ) THEN
                  N = N + 1
                  XC( N ) = PC( 8 )
               END IF

*  Initialise the status flag used by the PDA service routine
*  KPS1_SKYFN.
               ISTAT = SAI__OK

*  Do the search.  The tolerance value is fairly arbitrary.
               CALL PDA_LMDIF1( KPS1_SKYFN, 2 * NPOS, N, XC,
     :                          %VAL( CNF_PVAL( IPWFV ) ),
     :                          1.0D-3, IFAIL,
     :                          %VAL( CNF_PVAL( IPWNA1 ) ),
     :                          %VAL( CNF_PVAL( IPWNA2 ) ), LW )

*  If an error occurred in the PDA service routine (KPS1_SKYFN), add a
*  context message, and then flush the error (so that other projections
*  will be attempted) unless the user has specified a specific
*  projection.
               IF ( STATUS .NE. SAI__OK ) GO TO 999
               IF ( ISTAT .NE. SAI__OK ) THEN

                  STATUS = ISTAT
                  IF ( .NOT. PROJ ) THEN
                     CALL MSG_SETC( 'P', PRJECC )
                     CALL ERR_REP( 'KPS1_SKYFT_ERR2', 'Excluding ^P '/
     :                 /'projection for the optimisation.', STATUS )
                     CALL ERR_FLUSH( STATUS )
                     CALL MSG_BLANK( STATUS )
                  END IF

               ELSE

*  Report an error if the PDA routine could not find an answer.
                  IF ( IFAIL .EQ. 0 .OR. IFAIL .GE. 4 ) THEN
                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'IFAIL', IFAIL )
                     CALL ERR_REP( 'KPS1_SKYFT_ERR3', 'Routine '/
     :                 /'PDA_LMDIF1 returned INFO = ^IFAIL.', STATUS )
                     GO TO 999
                  END IF

*  Compute the sum of the squared residuals.  Since the array of
*  residuals is dynamic it is obtained as workspace therefore a
*  subroutine must be called to evaluate its sum.
                  CALL KPG1_SQSUD( 2 * NPOS, %VAL( CNF_PVAL( IPWFV ) ),
     :                             FS, STATUS )

*  If the sum of the squared residuals for this projection is better
*  than the best so far, record the projection type and parameter
*  values, and the sum of the squared residuals.
                  IF ( FS .LT. FSMIN ) THEN
                     PRJ = PRJECC

                     N = 0

                     IF ( .NOT. REFSKY ) THEN
                        N = N + 2
                        P0( 1 ) = XC( N - 1 )
                        P0( 2 ) = XC( N )
                     ELSE
                        P0( 1 ) = PC( 1 )
                        P0( 2 ) = PC( 2 )
                     END IF

                     IF ( .NOT. REFIMG ) THEN
                        N = N + 2
                        P0( 3 ) = XC( N - 1 )
                        P0( 4 ) = XC( N )
                     ELSE
                        P0( 3 ) = PC( 3 )
                        P0( 4 ) = PC( 4 )
                     END IF

                     IF ( .NOT. PSIZE ) THEN
                        N = N + 2
                        P0( 5 ) = XC( N - 1 )
                        P0( 6 ) = XC( N )
                     ELSE
                        P0( 5 ) = PC( 5 )
                        P0( 6 ) = PC( 6 )
                     END IF

                     IF ( .NOT. ORIENT ) THEN
                        N = N + 1
                        P0( 7 ) = XC( N )
                     ELSE
                        P0( 7 ) = PC( 7 )
                     END IF

                     IF ( .NOT. TILT ) THEN
                        N = N + 1
                        P0( 8 ) = XC( N )
                     ELSE
                        P0( 8 ) = PC( 8 )
                     END IF

                     FSMIN = FS

                  END IF

               END IF

            END IF

         END IF

*  Find the start and end of the next projection name in the list.
         START = END + 2
         IF ( START .LE. LEND ) THEN
            END = INDEX( PRJLST( START: ), ',' )
            IF ( END .GT. 0 ) THEN
               END = END + START - 2
            ELSE
               END = LEND
            END IF
         END IF

      END DO

*  If no solution was found, report an error.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      IF ( FSMIN .EQ. VAL__MAXD ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_SKYFT_ERR4', 'Unable to find any '/
     :                 /'usable parameter values. ', STATUS )
         GO TO 999
      END IF

*  Return the best fitting parameters and projection.
      DO I = 1, NP
         P( I ) = P0( I )
      END DO

      PROJEC = PRJ

*  Return the RMS positional error.
      RMS = REAL( SQRT( MAX( 0.0D0, FSMIN/DBLE( NPOS ) ) ) )

*  Log the results and residuals if required.
      IF ( LOG )
     :  CALL KPS1_SKYF4( PROJEC, NP, P, SCS, EPOCH, NPOS,
     :                   %VAL( CNF_PVAL( IPWA ) ),
     :                   %VAL( CNF_PVAL( IPWB ) ),
     :                   %VAL( CNF_PVAL( IPWX ) ),
     :                   %VAL( CNF_PVAL( IPWY ) ), RMS,
     :                   LOGFD, %VAL( CNF_PVAL( IPWXO ) ),
     :                   %VAL( CNF_PVAL( IPWYO ) ), STATUS )

*  Jump to here if an error occurs.
 999  CONTINUE

*  Free the work space.
      CALL PSX_FREE( IPWA, STATUS )
      CALL PSX_FREE( IPWB, STATUS )
      CALL PSX_FREE( IPWX, STATUS )
      CALL PSX_FREE( IPWY, STATUS )
      CALL PSX_FREE( IPWXO, STATUS )
      CALL PSX_FREE( IPWYO, STATUS )
      CALL PSX_FREE( IPWNA1, STATUS )
      CALL PSX_FREE( IPWNA2, STATUS )
      CALL PSX_FREE( IPWFV, STATUS )

      END
