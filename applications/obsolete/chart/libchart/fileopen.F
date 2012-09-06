#if HAVE_CONFIG_H
# include "config.h"
#endif
      SUBROUTINE FILEOPEN( ARG1, ARG2, ARG3, ARG4, ARG5, ARG6, ARG7,
     : ARG8, STATUS )
*+
*  Name:
*     FILEOPEN

*  Purpose:
*     Opens a file, and is used instead of an OPEN statement in the
*     calling routine. This is because when we ported Chart to Sun
*     machines, we found that Sun Fortran did'nt implement the
*     CARRIAGECONTROL and READONLY specifiers, which were needed on the
*     DEC machines, and so rather than have two different versions of
*     the many routines which use these in their OPEN statements, for
*     the different machines, we can have those routines call this one
*     to do OPENs and just have two versions of this. This is the DEC
*     machine version.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FILEOPEN( UNIT, FILE, STAT, ACCESS, FORM, CARIGE,
*          : RECL, RDONLY, STATUS  )

*  Description:
*     Basically it just does an OPEN statement, with the parameters
*     supplied for the specifiers. Not all OPENs need all the
*     specifiers, so it puts the machine defaults into those left
*     blank. No attempt has been made to cater for every possible
*     specifier / combination of specifiers you can have in the OPEN
*     statement - it is just sufficient to replace the OPENs in the
*     routines with non-standard OPEN specifiers.

*  Arguments:
*     ARG1 = INTEGER (Given)
*        Fortran logical unit #
*     ARG2 = CHARACTER * ( * ) (Given)
*        Filename.
*     ARG3 = CHARACTER * ( * ) (Given)
*        Status specifier.
*     ARG4 = CHARACTER * ( * ) (Given)
*        ACCESS specifier.
*     ARG5 = CHARACTER * ( * ) (Given)
*        FORM specifier.
*     ARG6 = LOGICAL (Given)
*        Flag : .TRUE. if CARRIAGECONTROL = 'LIST' specifier required.
*     ARG7 = INTEGER (Given)
*        RECL specifier - if set to zero, it's not included in the OPEN
*        statement, otherwise it is present.
*     ARG8 = LOGICAL (Given)
*        .TRUE. if READONLY specifier required.
*     [argument_spec]...
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     AJJB: Andrew J. J. Broderick (Starlink - RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-MAR-1993 (AJJB):
*        Original version.
*      3-AUG-2004 (TIMJ):
*        Autoconf version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INCLUDE 'CHT_ERR'          ! Chart error constants

*  Arguments Given:
      INTEGER ARG1
      CHARACTER * ( * ) ARG2
      CHARACTER * ( * ) ARG3
      CHARACTER * ( * ) ARG4
      CHARACTER * ( * ) ARG5
      LOGICAL ARG6
      INTEGER ARG7
      LOGICAL ARG8

*  Arguments Given and Returned:
      INTEGER STATUS

*  Local Constants:
      CHARACTER * ( 7 ) STADEF   ! Default value for STATUS specifier
      PARAMETER ( STADEF = 'UNKNOWN' )
      CHARACTER * ( 10 ) ACCDEF  ! Default value for ACCESS specifier
      PARAMETER ( ACCDEF = 'SEQUENTIAL' )
      CHARACTER * ( 9 ) SEQDEF   ! Default for FORM when ACCESS =
                                 ! SEQUENTIAL.
      PARAMETER ( SEQDEF = 'FORMATTED' )
      CHARACTER * ( 11 ) DIRDEF  ! Default for FORM when ACCESS =
                                 ! DIRECT.
      PARAMETER ( DIRDEF = 'UNFORMATTED' )
      CHARACTER BLANK            ! A space character
      PARAMETER ( BLANK = ' ' )
      INTEGER BYTEPRU            ! Number of bytes per record unit
#if FC_RECL_UNIT == 1
      PARAMETER ( BYTEPRU = 1 )
#elif FC_RECL_UNIT == 2
      PARAMETER ( BYTEPRU = 1 )
#elif FC_RECL_UNIT == 4
      PARAMETER ( BYTEPRU = 1 )
#else
#           error "Unrecognized FC_RECL_UNIT"
#endif


*  Local Variables:
      INTEGER UNIT               ! Fortran logical unit #
      CHARACTER * ( 60 ) FILE    ! Filename.
      CHARACTER * ( 7 ) STAT     ! Status specifier.
      CHARACTER * ( 10 ) ACCESS  ! ACCESS specifier.
      CHARACTER * ( 11 ) FORM    ! FORM specifier.
      LOGICAL CARIGE             ! Flag : .TRUE. if CARRIAGECONTROL =
                                 ! 'LIST' specifier required.
      INTEGER RECL               ! RECL specifier
      LOGICAL RDONLY             ! .TRUE. if READONLY specifier
                                 ! required.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* Assign args to local variables with slightly more meaningful names :

      UNIT = ARG1
      FILE = ARG2
      STAT = ARG3
      ACCESS = ARG4
      FORM = ARG5
      CARIGE = ARG6
      RECL = ARG7
      RDONLY = ARG8

* Set any which are left blank to machine default values :

      IF ( STAT(1:2) .EQ. BLANK )  STAT = STADEF
      IF ( ACCESS(1:2) .EQ. BLANK ) ACCESS = ACCDEF
      IF ( FORM(1:2) .EQ. BLANK .AND. ACCESS(1:10) .EQ. 'SEQUENTIAL' )
     :     FORM = SEQDEF
      IF ( FORM(1:2) .EQ. BLANK .AND. ACCESS(1:6) .EQ. 'DIRECT' )
     :     FORM = DIRDEF

* Convert RECL specifier from longwords (as required on DEC machines) to
* bytes (as required on Sun machines) if need be.
* Note that this assumes we are always being given longwords

      IF (FORM(:11) .EQ. 'UNFORMATTED' ) RECL = RECL * 4 / BYTEPRU


* Pick OPEN statement with appropriate specifiers according to what
* calling routine requires :

      IF ( .NOT. RDONLY .AND. .NOT. CARIGE ) THEN

            OPEN( UNIT = UNIT, FILE = FILE, STATUS = STAT,
     :      ACCESS = ACCESS, RECL = RECL, FORM = FORM, ERR = 500 )

      ELSE IF ( .NOT. RDONLY .AND. CARIGE ) THEN

        IF ( RECL .EQ. 0 ) THEN

            OPEN( UNIT = UNIT, FILE = FILE, STATUS = STAT,
     :      ACCESS = ACCESS, FORM = FORM,
#if HAVE_FC_OPEN_CARRIAGECONTROLLIST
     :      CARRIAGECONTROL = 'LIST',
#endif
     :          ERR = 500 )

        ELSE

            OPEN( UNIT = UNIT, FILE = FILE, STATUS = STAT,
     :      ACCESS = ACCESS, RECL = RECL, FORM = FORM,
#if HAVE_FC_OPEN_CARRIAGECONTROLLIST
     :      CARRIAGECONTROL = 'LIST',
#endif
     :          ERR = 500 )

        ENDIF

      ELSEIF ( RDONLY .AND. .NOT. CARIGE ) THEN

        IF ( RECL .EQ. 0 ) THEN

            OPEN( UNIT = UNIT, FILE = FILE, STATUS = STAT,
     :      ACCESS = ACCESS, FORM = FORM,
#if HAVE_FC_OPEN_ACTIONREAD
     :          ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :          READONLY,
#endif
     :          ERR = 500)

        ELSE

            OPEN( UNIT = UNIT, FILE = FILE, STATUS = STAT,
     :      ACCESS = ACCESS, RECL = RECL, FORM = FORM,
#if HAVE_FC_OPEN_ACTIONREAD
     :          ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :          READONLY,
#endif
     :          ERR = 500)

        ENDIF

      ELSEIF ( RDONLY .AND. CARIGE ) THEN

        IF ( RECL .EQ. 0 ) THEN

            OPEN ( UNIT = UNIT, FILE = FILE, STATUS = STAT,
     :      ACCESS = ACCESS, FORM = FORM,
#if HAVE_FC_OPEN_ACTIONREAD
     :          ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :          READONLY,
#endif
#if HAVE_FC_OPEN_CARRIAGECONTROLLIST
     :      CARRIAGECONTROL = 'LIST',
#endif
     :          ERR = 500 )

        ELSE

            OPEN ( UNIT = UNIT, FILE = FILE, STATUS = STAT,
     :      ACCESS = ACCESS, RECL = RECL, FORM = FORM,
#if HAVE_FC_OPEN_ACTIONREAD
     :          ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     :          READONLY,
#endif
#if HAVE_FC_OPEN_CARRIAGECONTROLLIST
     :      CARRIAGECONTROL = 'LIST',
#endif
     :          ERR = 500 )

        ENDIF

      ENDIF

      RETURN

* If error on opening . . .

 500  CONTINUE

      STATUS = CHT__NOFIL
      CALL ERR_REP( ' ', 'FILEOPEN: Can''t open ' // FILE, STATUS )

      END
