      SUBROUTINE gns_1DECSP (SPEC,NAME,LNAME,PDEV,LPDEV)
*++
*   gns_1DECSP   Decompose a device specification into a device name and
*               a device type.
*
*   Description:
*      A device specification consists of a device name optionally
*      followed by a physical device name separated by a semi-colon.
*
*      Any leading or trailing spaces are removed from the strings.
*
*      If a component does not exist then a length of zero is returned as
*      the length and the string set to all blanks.
*
*   Input arguments:
*      SPEC    c*(*)    Device specification
*
*   Output arguments:
*      NAME    c*(*)    Name
*      LNAME   i        Length of name
*      PDEV    c*(*)    Physical device name
*      LPDEV   i        Length of physical device
*
*   Implicit inputs:
*      none
*
*   Implicit outputs:
*      none
*
*	 External references:
*      gns_1TRIM
*
*   D L Terrett   2-JUN-1988 
*++
      IMPLICIT NONE

      CHARACTER*(*) SPEC, NAME, PDEV
      INTEGER LNAME, LPDEV

      INTEGER I

      I = INDEX (SPEC,';')

      IF (I.EQ.0) THEN
         NAME = SPEC
         PDEV = ' '
      ELSE
         NAME = SPEC(:I-1)
         PDEV = SPEC(I+1:)
      END IF

      CALL gns_1TRIM (NAME,NAME,LNAME)
      CALL gns_1TRIM (PDEV,PDEV,LPDEV)

      END
