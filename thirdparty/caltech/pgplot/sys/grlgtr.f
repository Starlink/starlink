C*GRLGTR -- translate logical name (dummy version)
C+
      SUBROUTINE GRLGTR (NAME)
      CHARACTER*(*) NAME
C
C Recursive translation of a logical name.
C This is used in the parsing of device specifications in the
C VMS implementation of PGPLOT. In other implementations, it may
C be replaced by a null routine.
C
C Argument:
C  NAME (input/output): initially contains the name to be
C       inspected.  If an equivalence is found it will be replaced
C       with the new name. If not, the old name will be left there.
C--
C 19-Dec-1994
C-----------------------------------------------------------------------
      END
