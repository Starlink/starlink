      SUBROUTINE IRA_SETEQ( EQU, BJ, SCS, STATUS )
*+
*  Name:
*     IRA_SETEQ

*  Purpose:
*     Encode the epoch of a reference equinox within an SCS name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_SETEQ( EQU, BJ, SCS, STATUS )

*  Description:
*     On entry, SCS contains the name of a Sky Coordinate System (or an
*     unambiguous abbreviation), with or without an equinox specifier
*     (see routine IRA_ISCS). On exit, SCS contains the full name of
*     the Sky Coordinate System with an equinox specifier appended,
*     determined by arguments EQU and BJ. Any old equinox specifier is
*     first removed. If the Sky Coordinate System is not referred to
*     the equinox (eg GALACTIC) then no equinox specifier is included
*     in SCS on exit.

*  Arguments:
*     EQU = DOUBLE PRECISION (Given)
*        The epoch of the reference equinox. After calling this routine,
*        the sky coordinates described by SCS are referred to the mean
*        equinox of the epoch given by EQU. If EQU has the Starlink
*        "BAD" value (VAL__BADD) then no equinox specifier is included
*        in SCS on exit.
*     BJ = CHARACTER * ( * ) (Given)
*        Determines if the epoch specified by argument EQU is a
*        Besselian or Julian epoch. BJ should have the value "B" or "J".
*        Any other value causes an error report (except that a blank
*        value causes "B" to be used).
*     SCS = CHARACTER * ( * ) (Given and Returned)
*        On entry, SCS should contain an unambiguous abbreviation of a
*        supported Sky Coordinate System (see routine IRA_ISCS), with or
*        without an equinox specifier. On exit, SCS contains the full
*        name of the Sky Coordinate System, appended with an equinox
*        specifier determined by arguments EQU and BJ. If the Sky
*        Coordinate System is not one that is referred to the equinox
*        (eg GALACTIC) then no equinox specifier is included in SCS on
*        exit. SCS should have a declared length equal to the symbolic
*        constant IRA__SZSCS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-APR-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PRM_PAR'          ! BAD data values.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA error values.

*  Arguments Given:
      DOUBLE PRECISION EQU
      CHARACTER BJ*(*)

*  Arguments Given and Returned:
      CHARACTER SCS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL          ALLZER    ! True if all decimal places in the
                                 ! epoch value have so far been zero.
      INTEGER          BJFST     ! Position of first character in BJ.
      INTEGER          BJLST     ! Position of last character in BJ.
      INTEGER          CHR_LEN   ! Function giving used length of a
                                 ! string.
      INTEGER          ESTART    ! Position of start of equinox
                                 ! specifier.
      INTEGER          I         ! Loop count.
      INTEGER          ISTAT     ! Fortran IO status value.
      CHARACTER        NAME*(IRA__SZSCS) ! Full SCS name (without
                                          ! equinox specifier).
      CHARACTER        NEWBJ*1   ! BJ converted to upper case.
      DOUBLE PRECISION NEWEQU    ! EQU rounded to 4 decimal places.
      CHARACTER        OLDBJ*1   ! BJ in input SCS.
      DOUBLE PRECISION OLDEQU    ! EQU in input SCS.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the SCS argument is long enough.
      IF( LEN( SCS ) .LT. IRA__SZSCS ) THEN
         STATUS = IRA__TOOSH
         CALL ERR_REP( 'IRA_SETEQ_ERR1',
     :   'IRA_SETEQ: Declared size of argument SCS is too small',
     :                 STATUS )
         GO TO 999
      END IF

*  Identify and expand the input SCS name.
      CALL IRA1_CHSCS( SCS, NAME, OLDEQU, OLDBJ, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If the sky coordinate system is not refered to the equinox, or if
*  the input value of EQU is bad, just copy the full name to the output
*  SCS without any equinox specifier.
      IF( OLDBJ .EQ. ' ' .OR. EQU .EQ. VAL__BADD ) THEN
         SCS = NAME

*  Otherwise,
      ELSE

*  If BJ is blank, use "B".
         IF( BJ .EQ. ' ' ) THEN
            NEWBJ = 'B'

*  Otherwise, convert the BJ argument to upper case and remove leading blanks.
         ELSE
            CALL CHR_FANDL( BJ, BJFST, BJLST )
            NEWBJ = BJ( BJFST:BJFST )
            CALL CHR_UCASE( NEWBJ )

         END IF

*  Verify that BJ has a valid value (B or J).
         IF( NEWBJ .NE. 'B' .AND. NEWBJ .NE. 'J' ) THEN
            STATUS = IRA__BADBJ
            CALL MSG_SETC( 'BJ', BJ )
            CALL ERR_REP( 'IRA_SETEQ_ERR1',
     :        'IRA_SETEQ: Illegal epoch type identifier supplied: ^BJ',
     :                    STATUS )
            GO TO 999
         END IF

*  Round the given date to 4 decimal places.
         NEWEQU = 1.0D-4*NINT( EQU*1.0D4 )

*  Append the equinox identifier to the end of the SCS name. First
*  append the opening paranethesis and epoch type specifier to the end
*  of the SCS name.
         ESTART = CHR_LEN( NAME ) + 1
         SCS = NAME
         SCS( ESTART : ESTART + 1 ) = '('//NEWBJ

*  Now append the epoch value as an F11.4 number.
         WRITE( SCS( ESTART + 2 : ), '(F11.4)', IOSTAT=ISTAT ) NEWEQU
         IF( ISTAT .NE. 0 ) THEN
            STATUS = IRA__NOFRM
            CALL ERR_REP( 'IRA_SETEQ_ERR4',
     :              'IRA_SETEQ: Unable to format an equinox specifier',
     :                    STATUS )
            GO TO 999
         END IF

*  Remove any trailing zeros.
         ALLZER = .TRUE.

         DO I = ESTART + 12, ESTART + 9, -1

            IF( SCS( I:I ) .EQ. '0' .AND. ALLZER ) THEN
               SCS( I:I ) = ' '

            ELSE
               ALLZER = .FALSE.

            END IF

         END DO

*  If all decimal places were zero, also remove the decimal point.
         IF( ALLZER ) SCS( ESTART + 8 : ESTART + 8 ) = ' '

*  Remove any blanks infront of the epoch value, and add a closing
*  parenthesis.
         CALL CHR_LDBLK( SCS( ESTART + 2 : ) )
         SCS( CHR_LEN( SCS ) + 1 : ) = ')'

      END IF

 999  CONTINUE

      END
