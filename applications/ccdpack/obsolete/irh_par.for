*+
*  Name:
*     IRH_PAR

*  Purpose:
*     Define global constants for the IRH_ system.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Global constants include file.

*  Description:
*     This file contains definitions of global constants which are used
*     by the IRH_ system and which may also be needed by software which
*     calls routines from this system.

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-JUN-1991 (DSB):
*        Original version.
*     28-FEB-1992 (PDRAPER):
*        Added machine dependent flag for case sensitivity.
*     13-AUG-1993 (PDRAPER):
*        Added depreciated substution element.
*     {enter_further_changes_here}

*-

*  Global Constants:



*  GENERAL
*  =======

*  String used to indicate that the IRH_ system has been initialised.
*  This value is assigned to common variable IRH_STATE when IRH_ is
*  initialised.
      CHARACTER IRH__GOING*9
      PARAMETER ( IRH__GOING = 'IRH GOING' )

*  String used to fill unused slots in the NAMES array of each group.
      CHARACTER IRH__BLANK*14
      PARAMETER ( IRH__BLANK = 'IRH BLANK NAME' )

*  The number of cells by which the array of GROUP structures is
*  extended, when the current array size is exhausted.
      INTEGER IRH__INCG
      PARAMETER ( IRH__INCG = 3 )

*  The number of cells by which NAMES arrays are extended, when the
*  current array size is exhausted.
      INTEGER IRH__INCN
      PARAMETER ( IRH__INCN = 5 )

*  The initial size of the array of GROUP structures.
      INTEGER IRH__INITG
      PARAMETER ( IRH__INITG = 10 )

*  The initial size of each NAMES array.
      INTEGER IRH__INITN
      PARAMETER ( IRH__INITN = 20 )

*  The maximum allowed depth of indirection.
      INTEGER IRH__MAXDI
      PARAMETER ( IRH__MAXDI = 7 )

*  The size of the common arrays. This is the limit on the number of
*  groups which can be used by an application at once.
      INTEGER IRH__MAXG
      PARAMETER ( IRH__MAXG = 20 )

*  An illegal IRH_ identifier value. This value can sometimes be
*  specified by an application in place of an IRH_ identifier in order
*  to supress some operation.
      INTEGER IRH__NOID
      PARAMETER ( IRH__NOID = 0 )



*  String lengths.
*  ==============

*  Maximum length of a group expression.
      INTEGER IRH__SZGEX
      PARAMETER ( IRH__SZGEX = 255 )

*  Length of a name within a group.
      INTEGER IRH__SZNAM
      PARAMETER ( IRH__SZNAM = 255 )

*  Length of the string given by parameter IRH__GOING.
      INTEGER IRH__SZSTAT
      PARAMETER ( IRH__SZSTAT = 9 )



*  IRH_ "control" characters.
*  =========================

*  Character used to indicate an indirection element.
      CHARACTER IRH__INDC*1
      PARAMETER ( IRH__INDC = '^' )

*  Character used to start a comment.
      CHARACTER IRH__COMC*1
      PARAMETER ( IRH__COMC = '#' )

*  Character used to delimit elements in a group expression.
      CHARACTER IRH__DELC*1
      PARAMETER ( IRH__DELC = ',' )

*  Character used to indicate the generalised input name within a 
*  modification element.
      CHARACTER IRH__MODNM*1
      PARAMETER ( IRH__MODNM = '*' )

*  Character used as a seperator in a modification element.
      CHARACTER IRH__MODSP*1
      PARAMETER ( IRH__MODSP = '|' )

*  Depreciated character used as a seperator in a modification element.
      CHARACTER IRH__MODSD*1
      PARAMETER ( IRH__MODSD = '/' )

*  IRH_ "machine dependents"
*  =========================

*  Flag indicating that upper case conversion should be performed.
      LOGICAL IRH__UCASE
      PARAMETER ( IRH__UCASE = .TRUE. ) ! VMS version


*.
* $Id$
