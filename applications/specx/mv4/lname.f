*  History:
*     13 Dec 1993 (hme):
*        Change file extension to lower case (.map).
*     15 Aug 1994 (hme):
*        Make map file extension "_map.sdf". The user has to handle none
*        of this, and the software only the _map part.
C-----------------------------------------------------------------------

      INTEGER*4 FUNCTION LNAME (MAP_NAME)

C  Routine to return length of map_name string, up to but not
C  including the "_map" filename extension. Exists chiefly to
C  avoid using INDEX function in map routines (which have INDEX array)

      IMPLICIT  NONE

      CHARACTER MAP_NAME*(*)
      INTEGER   INDEX

      INTEGER   GEN_ILEN

      LNAME = INDEX (MAP_NAME,'_map') - 1
      IF (LNAME.LE.0) LNAME = GEN_ILEN(MAP_NAME)

      RETURN
      END

C-----------------------------------------------------------------------
