      SUBROUTINE SUBPAR_INIT( NAMECODE, STATE, STATUS )
*+
*  Name:
*     SUBPAR_INIT

*  Purpose:
*     Initialise variable to initial state.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL SUBPAR_INIT( NAMECODE, STATE, STATUS )

*  Arguments:
*     NAMECODE = INTEGER (Given)
*        SUBPAR namecode of parameter.
*     STATE = INTEGER (Given)
*        SUBPAR state into which the parameter must be initialised.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This is a proof-of-concept hack, pending AJC coming up with a
*     proper implementation within SUBPAR.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     11-MAY-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      
*  Global Variables:
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'

************************************************************************
*  The following is a literal include of the SUBPAR private include
*  file 'subpar_cmn':
************************************************************************

*+  SUBPAR_CMN - Internal storage for parameter system
*
*   This consists, firstly, of two character*15 arrays, one holding the 
*   names of program parameters (PARNAMES) and one the names of d-task 
*   actions (ACTNAMES). Each has a parallel integer array holding the 
*   lengths of the names (PARLEN and ACTLEN).
*   The remaining storage associated with parameters and actions 
*   diverges in the two cases.
*   The names of individual programs in a MONOLITH are stored in 
*   ACTNAMES.
*
*   PARAMETERS
*
*   Parallel to PARNAMES and PARLEN is :
*     (1) an integer array PARSTATE containing codes for the current 
*     state of each parameter, GROUND, ACTIVE, CANCEL, NULL, EOL.
*     (2) an integer array containing codes for the types of the
*     parameters PARTYPE. The values in this indicate the data type 
*     of the parameter, and whether the value itself is stored internally
*     (which is usually the case for scalars), or in an external temporary 
*     HDS structure (arrays) or in a permanent HDS structure.
*     (3) a CHARACTER*(SUBPAR__SZLEN) array PARVALS which holds the values of 
*     character strings OR the name of the HDS structure containing the 
*     parameter value. There are also INTEGER, REAL, DOUBLE PRECISION and 
*     LOGICAL arrays for values.
*     (4) a character*80 array PARPROM which holds prompt strings for 
*     the parameters.
*     (5) a logical array PARWRITE which has .TRUE. values for those 
*     parameters which can be written, .FALSE. for readonly.
*     (6) an integer array (PARLIMS) with three elements for each parameter, 
*     giving the data type of the constraints on the parameter and
*     pointing to the first and last elements of the array of relevant 
*     type which holds the constraint values.
*     (7) a logical array (PARCONT). .TRUE. implies that the constraint 
*     list for this parameter is a pair of values giving a range (ie 
*     continuous), otherwise the constraint list is a set of possible 
*     values.
*     (8) a character*DAT__SZLOC array PARLOC with two values for each 
*     parameter storing HDS locators to the container file and the component 
*     containing the value of the parameter (if relevant).
*     (9) a logical array PARVALID containing .TRUE. for parameters with 
*     active locators in PARLOC.
*     (10) an integer array PARDEF with three elements for each parameter, 
*     storing the data type of the static defaults for the parameter and
*     pointing to a list of static default values.
*     (11) an integer array PARDYN with three values for each parameter, 
*     storing the data type of the dynamic defaults for the parameter and
*     pointing to a list of dynamic default values. (PARDYN(1,-) .EQ. 0) is
*     used to flag that there is no dynamic default space allocated for this 
*     parameter. If space has been allocated, (PARDYN(3,-) .EQ. type) indicates
*     that dynamic defaults are set in the typeLIST array. 
*     (PARDYN(3,-) .EQ. -type) indicates that space is allocated in the 
*     typeLIST array but no dynamic default has been set.
*     (12) an integer array PARASSOC with two values for each parameter, 
*     containing pointers to CHARLIST for the names of HDS structures 
*     associated with the parameter, and a code for the access mode.
*     (13) a byte array PARVPATH with five elements for each parameter,
*     storing the search paths for obtaining values.
*     (14) a character*15 array PARKEY containing the keyword for each 
*     parameter, which is used when prompting.
*     (15) a character*132 array PARHELP containing the one-line help 
*     text for each parameter.
*     (16) a character*15 array PARPTY containing the PTYPE for physical 
*     devices.
*     (17) an integer array PARRPATH with two values for each parameter, 
*     containing pointers to CHARLIST for the names of HDS structures 
*     which are the upper-levels associated with a physical device.
*     (18) an integer array PARPOS. This is not strictly parallel to 
*     PARNAMES. It contains indices to the parameters following their 
*     defined command-line order. Eg. Suppose PARNAMES(3) is a parameter 
*     declared to have POSITION 5, then PARPOS(5) = 3.
*     (19) A logical array PARLIT. If an element of this is .TRUE. then 
*     when a value is given to the corresponding parameter, it is 
*     interpreted as a literal string NOT an HDS name, even if not 
*     surrounded by quotes.
*     (20) A 2-D integer array PARCOORDS giving the (ROW,COL) position 
*     of the parameter in a menu display.
*     (21) a character array PARMENU giving the name of the menu on 
*     which the parameter is to appear.
*     (22) a byte array PARPPATH with five elements for each parameter,
*     storing the search paths for obtaining prompt values.
*     (23) a character*(132) array PARHKEY containing the parameter's
*     `helpkey' specifier (which may be a combination of helplib and
*     helpkey specifiers).
*     (24) A 2-D integer array PARMIN giving the index into the approprate 
*     typeLIST array for the parameter's minimum value, and the type (or
*     -1 if no minimum value is set.
*     (24) A 2-D integer array PARMAX giving the index into the approprate 
*     typeLIST array for the parameter's maximum value, and the type (or
*     -1 if no maximum value is set.
*   ACTIONS
*
*   Parallel to ACTNAMES and ACTLEN is :
*     (1) a logical array (MAYOB) indicating whether OBEY is valid for 
*     the action.
*     (2) a logical array (MAYCAN) indicating whether CANCEL is valid 
*     for the action.
*     (3) an integer array (NEEDOB) with two elements for each action, 
*     pointing to the first and last elements in a list of required 
*     parameters for OBEYing the action.
*     (4) an integer array (NEEDCAN) with two elements for each action,
*     pointing to the first and last elements in a list of required 
*     parameters for cancelling the action.
*     (5) a character array (ACTHELP) giving the help line for the 
*     action.
*     (6) a character array (ACTKEY) giving the keyword for the action.
*     (7) a character array (ACTMENU) giving the name of the menu on 
*     which the action is to appear.
*     (8) a 2-D integer array (ACTCOORDS) giving the coordinates on 
*     which the action is to appear on the menu.
*
*   ACTION NEEDS
*
*   An integer array (NEEDPAR) contains a list of pointers to the 
*   parameter list. NEEDPAR is pointed at by NEEDOB and NEEDCAN, thereby
*   specifying the list of parameters required to have values by each 
*   action.
*   Parallel to NEEDPAR is :
*     (1) an integer array (NEEDLIMS) with two elements for each NEEDPAR 
*     entry, pointing to the start and end of the constraint list for 
*     the entry.
*     (2) a logical array (NEEDCONT) with .TRUE. values corresponding to 
*     elements of NEEDLIMS pointing to a range constraint, .FALSE. if 
*     NEEDLIMS is pointing to a list of possible values.
*
*   CONSTRAINTS
*
*   There is a list of constraints for each relevant data type -
*     integer          - INTLIST
*     real             - REALLIST
*     double precision - DOUBLELIST
*     character (*SUBPAR__STRLEN) - CHARLIST
*     logical          - LOGLIST
*   These are pointed at by
*     (1) PARLIMS, indicating general constraints on the value of a 
*     scalar parameter.
*     (2) NEEDLIMS, indicating constraints on the value of a parameter 
*     specifically associated with some action.
*   Note that constraints only apply to scalar parameter values.
*
*   The 'constraint lists' are also used to store lists of items for
*     (1) static default values, pointed at by PARDEF
*     (2) dynamic default values, pointed at by PARDYN
*     (3) associated HDS structures, pointed at by PARVPATH.
*
*   MONOLITHS
*     The names of the individual programs in a monolith are entered in 
*     ACTNAMES. Parallel to ACTNAMES is an integer array PROGADD with 
*     two elements for each program, containing pointers to PARNAMES 
*     indicating where the parameters for this program are to be found.
*
*   GENERAL STORED INFORMATION
*     In addition to the above complex data structure, the common blocks 
*     also store the following pieces of information:
*       The name of the interface module or monolith
*       A flag, .TRUE. => monolith
*       The program name
*       The search path for the .EXE file
*       A flag, .TRUE. => automatic checking for NEEDS lists
*     For A-tasks, the two following pieces of run-time information are 
*     held :
*       The path back to the task which ordered this task to RUN
*       The message number of the RUN message.
*       A flag indicating that prompts should go direct to the terminal
*    Authors :
*     B.D.Kelly (REVAD::BDK)
*    History :
*     13.09.1984:  Original (REVAD::BDK)
*     21.11.1984:  add RUNPATH (REVAD::BDK)
*     27.02.1085:  add PROGNAME and EXEPATH (REVAD::BDK)
*     16.04.1985:  add RUNID (REVAD::BDK)
*     23.08.1985:  handle monoliths - PROGADD, MONOLITH, EXTTOP, PROGNUM
*                      (REVAD::BDK)
*     05.09.1985:  store HDS locator to dynamic defaults (REVAD::BDK)
*     11.11.1985:  add PARLIT and RUNFACE (REVAD::BDK)
*     25.11.1985:  increase MAXPAR to 500 from 150 (REVAD::BDK)
*     06.03.1986:  add CHECKNEEDS flag (REVAD::BDK)
*     13.05.1986:  handle menus. Add PARCOORDS, PARMENU, ACTHELP, 
*                  ACTKEY, ACTMENU, ACTCOORDS (REVAD::BDK)
*     05.05.1987:  handle prompt search path. Add PARPPATH (REVAD::BDK)
*     05.05.1987:  add extra states RESET, ACCPR, RESACC, PROMPT, 
*                  RESPROM (REVAD::BDK)
*     05.05.1987:  increase SUBPAR__MAXPAR and SUBPAR__MAXACT 
*                  (REVAD::BDK)
*     05.05.1987:  make RUNFACE an integer (REVAD::BDK)
*     30.07.1987:  move constants for parameter states to SUBPAR.PAR 
*                  (REVAD::BDK)
*     30.04.1990:  Increase SUBPAR__MAXLIMS 300->350  (RLVAD::AJC)
*     15.05.1990:  Add parameter helpkey  (RLVAD::AJC)
*     03.07.1990:  Increase SUBPAR__MAXLIMS 350->500
*                  Add SUBPARTERM help paging control  (RLVAD::AJC)
*     15.10.1990:  Avoid mixing CHAR and numeric and
*                  remove equivalence PTRIFL to pointers etc.  (RLVAD::AJC)
*     14.07.1992:  Add PARMIN/PARMAX arrays (RLVAD::AJC)
*     31.07.1992:  Add SUBPARPSV block to save pointers after interface file 
*                  load so can restore to them at DEACT.
*                  Better comments on PARDYN. (RLVAD::AJC)
*      1.03.1993:  Use DAT__SZLOC, not 15, for portability (RLVAD::AJC)
*     20.08.1993:  Remove system parameters to SUBPAR_SYS 
*                  and include SUBPAR_SYS (RLVAD::AJC)
*      3.09.1993:  Correctly SAVE SUBPARPSV (not PSAV) (RLVAD::AJC)
*    endhistory
*-

*
*    Constants :
*
c     INCLUDE 'SUBPAR_SYS'

*+  SUBPAR_SYS - SUBPAR system parameters
*
*    Authors :
*     AJC: A J Chipperfield (STARLINK)
*    History :
*     20-AUG-1993 (AJC): 
*       Original version separated from SUBPAR_CMN 
*     21-DEC-1994 (AJC):
*       Add SUBPAR__STRLEN
*-

*
*    Constants :
*
      INTEGER SUBPAR__NAMELEN          ! maximum length of parameter and 
                                       ! action names
      PARAMETER ( SUBPAR__NAMELEN = 15 )

      INTEGER SUBPAR__MAXPAR           ! maximum number of parameters 
      PARAMETER ( SUBPAR__MAXPAR = 1500 )

      INTEGER SUBPAR__MAXACT           ! maximum number of actions
      PARAMETER ( SUBPAR__MAXACT = 300 )

      INTEGER SUBPAR__MAXNEEDS         ! size of NEEDS storage
      PARAMETER ( SUBPAR__MAXNEEDS = 300 )

      INTEGER SUBPAR__MAXLIMS          ! size of storage for constraints
      PARAMETER ( SUBPAR__MAXLIMS = 500 )

      INTEGER SUBPAR__STRLEN           ! size of Character values
      PARAMETER ( SUBPAR__STRLEN = 256 )

c End of SUBPAR_SYS

*
*   Codes for value search-path
*
      INTEGER SUBPAR__NOPATH, SUBPAR__CURRENT, SUBPAR__DEFAULT, 
     :  SUBPAR__DYNAMIC, SUBPAR__GLOBAL, SUBPAR__NOPROMPT, 
     :  SUBPAR__PROMPT, SUBPAR__INTERNAL

      PARAMETER ( SUBPAR__NOPATH    = 0 )
      PARAMETER ( SUBPAR__CURRENT   = 1 )
      PARAMETER ( SUBPAR__DEFAULT   = 2 )
      PARAMETER ( SUBPAR__DYNAMIC   = 3 )
      PARAMETER ( SUBPAR__GLOBAL    = 4 )
      PARAMETER ( SUBPAR__NOPROMPT  = 5 )
      PARAMETER ( SUBPAR__PROMPT    = 6 )
      PARAMETER ( SUBPAR__INTERNAL  = 7 )
*
*   Codes for access-type
*
      INTEGER SUBPAR__READ, SUBPAR__WRITE, SUBPAR__UPDATE

      PARAMETER ( SUBPAR__READ = 1 )
      PARAMETER ( SUBPAR__WRITE = 2 )
      PARAMETER ( SUBPAR__UPDATE = 3 )

*
*   Codes for connection to user interface
*
      INTEGER SUBPAR__TASK, SUBPAR__TERM, SUBPAR__UFACE

      PARAMETER ( SUBPAR__TASK = 1 )
      PARAMETER ( SUBPAR__TERM = 2 )
      PARAMETER ( SUBPAR__UFACE = 3 )

*
*    Program parameter definitions and storage
*
      CHARACTER*(SUBPAR__NAMELEN) PARNAMES(SUBPAR__MAXPAR) ! Names

      INTEGER PARLEN(SUBPAR__MAXPAR)   ! Actual lengths of parameter names

      INTEGER PARSTATE(SUBPAR__MAXPAR) ! Parameter states

      INTEGER PARTYPE(SUBPAR__MAXPAR)  ! parameter types

      CHARACTER*(SUBPAR__STRLEN) PARVALS(SUBPAR__MAXPAR) ! parameter values OR 
                                            ! storage location

      CHARACTER*80 PARPROM(SUBPAR__MAXPAR)  ! prompt strings

      INTEGER PARINT(SUBPAR__MAXPAR)        ! integer values

      REAL PARREAL(SUBPAR__MAXPAR)          ! real values

      DOUBLE PRECISION PARDOUBLE(SUBPAR__MAXPAR) ! r*8 values

      LOGICAL PARLOG(SUBPAR__MAXPAR)        ! logical values

      LOGICAL PARWRITE(SUBPAR__MAXPAR)      ! write access

      INTEGER PARLIMS(3,SUBPAR__MAXPAR)     ! data type of constraints
                                            ! and pointers to list of 
                                            ! values

      LOGICAL PARCONT(SUBPAR__MAXPAR)       ! .TRUE. => range constraint
                                            ! .FALSE. => set of values

      CHARACTER*(DAT__SZLOC) PARLOC(2,SUBPAR__MAXPAR) ! HDS locators
                                            ! (1,n) => container
                                            ! (2,n) => component

      LOGICAL PARVALID(SUBPAR__MAXPAR)      ! .TRUE. => valid locators

      INTEGER PARDEF(3,SUBPAR__MAXPAR)      ! data type of static 
                                            ! defaults and pointers to 
                                            ! list of values

      INTEGER PARDYN(3,SUBPAR__MAXPAR)      ! data type of dynamic
                                            ! defaults and pointers to 
                                            ! list of values

      INTEGER PARASSOC(2,SUBPAR__MAXPAR)    ! associated HDS structure
                                            ! (1,n)  points to name
                                            ! (2,n)  stores access code
                                            ! READ, WRITE or UPDATE

      BYTE PARVPATH(5,SUBPAR__MAXPAR)       ! value search-path

      CHARACTER*(SUBPAR__NAMELEN) PARKEY(SUBPAR__MAXPAR)   ! keywords

      INTEGER PARPOS(SUBPAR__MAXPAR)        ! command-line positions

      CHARACTER*132 PARHELP(SUBPAR__MAXPAR) ! one-line help

      CHARACTER*15 PARPTY(SUBPAR__MAXPAR)   ! PTYPE

      INTEGER PARRPATH(2,SUBPAR__MAXPAR)    ! pointers to RPATH strings

      LOGICAL PARLIT(SUBPAR__MAXPAR)        ! .TRUE. => literal string, 
                                            ! ie never interpret value 
                                            ! as HDS name

      CHARACTER*80 PARERRMESS               ! error message. This can be 
                                            ! set up by eg. SUBPAR_ASSOC 
                                            ! and is displayed when 
                                            ! prompting for values.

      CHARACTER*(SUBPAR__NAMELEN) PARMENU(SUBPAR__MAXPAR) ! menus

      INTEGER PARCOORDS(2,SUBPAR__MAXPAR)   ! (row,col) position in menu

      BYTE PARPPATH(5,SUBPAR__MAXPAR)       ! prompt value search-path

      CHARACTER*(132) PARHKEY(SUBPAR__MAXPAR)  ! helpkey spec

      INTEGER PARMIN(2,SUBPAR__MAXPAR)     ! MIN value type and pointer
      INTEGER PARMAX(2,SUBPAR__MAXPAR)     ! MAX value type and pointer

*
*   Common blocks holding basic definitions given by interface file.
*
      COMMON / SUBPARVALS / PARLEN, PARTYPE, PARWRITE, PARLIMS, PARCONT,
     :  PARDEF, PARDYN,  PARASSOC, PARPOS, PARRPATH, PARVPATH, PARLIT,
     :  PARCOORDS, PARPPATH
      COMMON / SUBPARVALC / PARHELP, PARNAMES, PARPROM, PARKEY, PARPTY, 
     :  PARMENU, PARHKEY

*
*   Common blocks holding data which is generated when the application is 
*   running. This block does not need to be read from disk on program 
*   start-up.
*
      COMMON / SUBPARDAT / PARSTATE, PARINT, PARREAL, 
     :  PARDOUBLE, PARLOG, PARVALID, PARMIN, PARMAX
      COMMON / SUBPARDATC / PARVALS, PARLOC, PARERRMESS 

*
*   Action definitions and storage.
*
      CHARACTER*(SUBPAR__NAMELEN) ACTNAMES(SUBPAR__MAXACT) ! names

      INTEGER ACTLEN(SUBPAR__MAXACT)        ! lengths of actnames

      LOGICAL MAYOB(SUBPAR__MAXACT)         ! .TRUE. => OBEY is valid 
                                            ! for action

      LOGICAL MAYCAN(SUBPAR__MAXACT)        ! .TRUE. => CANCEL is valid 
                                            ! for action

      INTEGER NEEDOB(2,SUBPAR__MAXACT)      ! pointers to needs list for 
                                            ! OBEY

      INTEGER NEEDCAN(2,SUBPAR__MAXACT)     ! pointers to needs list for 
                                            ! CANCEL

      INTEGER PROGADD(2,SUBPAR__MAXACT)     ! pointers to the parameter 
                                            ! list for individual 
                                            ! programs within a monolith

      CHARACTER*132 ACTHELP(SUBPAR__MAXACT) ! help line

      CHARACTER*(SUBPAR__NAMELEN) ACTKEY(SUBPAR__MAXACT)   ! keyword for action

      CHARACTER*(SUBPAR__NAMELEN) ACTMENU(SUBPAR__MAXACT)  ! menus

      INTEGER ACTCOORDS(2,SUBPAR__MAXACT)   ! (row,col) position in menu


      COMMON / SUBPARACTS / ACTLEN, MAYOB, MAYCAN,
     :  NEEDOB, NEEDCAN, PROGADD, ACTCOORDS
      COMMON / SUBPARACTC / ACTNAMES, ACTHELP, ACTKEY, ACTMENU


*
*   Needs list - ie specification of parameters required by specific 
*   OBEY/CANCEL actions.
*
      INTEGER NEEDPAR(SUBPAR__MAXNEEDS)     ! pointers to required 
                                            ! parameters

      INTEGER NEEDLIMS(2,SUBPAR__MAXNEEDS)  ! pointers to constraint 
                                            ! list for required 
                                            ! parameters

      LOGICAL NEEDCONT(SUBPAR__MAXNEEDS)    ! .TRUE. => range constraint

      COMMON/ SUBPARNEEDS / NEEDPAR, NEEDLIMS, NEEDCONT

*
*   Data lists. There is a list for each data type.
*   The lists may contain a pair of values specifying a range, 
*   or a set of possible valid values, or static and dynamic defaults,
*   or the names of associated HDS objects.
*   The interpretation of the contents of the list is determined by
*     parameter type +
*     PARLIMS,PARCONT or NEEDLIMS,NEEDCONT or PARDEF or PARDYN or 
*     PARVPATH.
*
      INTEGER INTLIST(SUBPAR__MAXLIMS)
      REAL REALLIST(SUBPAR__MAXLIMS)
      DOUBLE PRECISION DOUBLELIST(SUBPAR__MAXLIMS)
      CHARACTER*(SUBPAR__STRLEN) CHARLIST(SUBPAR__MAXLIMS)
      LOGICAL LOGLIST(SUBPAR__MAXLIMS)

      COMMON / SUBPARCONST / DOUBLELIST, INTLIST, REALLIST, LOGLIST
      COMMON / SUBPARCONSTC / CHARLIST


*
*   There is a set of pointers to the last elements of the various 
*   lists. These pointers are set and manipulated during the parsing of 
*   the interface file, but remain constant thereafter, except when 
*   storing dynamic defaults.
*   Also stored with these are the interface name, program name and 
*   EPATH string (search path for directory containing the .EXE), as 
*   well as the flag for whether this is a monolith.
*   The equivalence is to enable the block to be written to or read from
*   a disk-file.
*
      INTEGER PARPTR             ! last parameter on namelist
                                 ! = total number of parameters

      INTEGER ACTPTR             ! last action on namelist
                                 ! = total number of actions

      INTEGER NEEDPTR            ! last entry in needs list
                                 ! = total number of entries

      INTEGER INTPTR             ! last entry in INTLIST

      INTEGER REALPTR            ! last entry in REALLIST

      INTEGER DOUBLEPTR          ! last entry in DOUBLELIST

      INTEGER CHARPTR            ! last entry in CHARLIST

      INTEGER LOGPTR             ! last entry in LOGLIST

      CHARACTER*80 FACENAME      ! name of interface module

      CHARACTER*9 PROGNAME       ! program name

      CHARACTER*132 EXEPATH      ! search-path for execution module

      LOGICAL MONOLITH           ! .TRUE. => monolith


      COMMON / SUBPARPTR / PARPTR, ACTPTR, NEEDPTR, INTPTR, REALPTR, 
     :  DOUBLEPTR, CHARPTR, LOGPTR, MONOLITH
      COMMON / SUBPARPTRC / FACENAME, PROGNAME, EXEPATH
*
*   The values of INTPTR, REALPTR etc. after interface file loading are save
*   so that they can be restored at SUBPAR_DEACT in order to re-use space --
*   particularly important for monoliths.
*
      INTEGER INTPSV
      INTEGER REALPSV
      INTEGER DOUBLEPSV
      INTEGER CHARPSV
      INTEGER LOGPSV

      COMMON / SUBPARPSV / INTPSV, REALPSV, DOUBLEPSV, CHARPSV, LOGPSV
*
*   The common blocks also hold four locators to HDS structures which are 
*   given values at progam activation. These are the locators to the 
*   container files for global data, and for the external (ie not 
*   common-block) storage of parameter values.
*
      CHARACTER*(DAT__SZLOC) GLOBLOC    ! locator to global data storage

      CHARACTER*(DAT__SZLOC) EXTTOP     ! top-level locator to external 
                                        ! parameter storage

      CHARACTER*(DAT__SZLOC) EXTLOC     ! locator to external parameter storage
                                        ! for the currently active program in a 
                                        ! monolith. This is equal to EXTTOP for 
                                        ! non-monoliths

      CHARACTER*(DAT__SZLOC) DYNLOC     ! locator to store for dynamic defaults

      COMMON / SUBPARLOCS / GLOBLOC, EXTLOC, EXTTOP, DYNLOC


*
*   The path back to the task which ordered an ADAM A-task to RUN, and 
*   the associated message id.
*   These are used if it is necessary to prompt for a parameter. They 
*   are set-up before the application routine is called, and are used by 
*   SUBPAR_REQUEST.
*   RUNFACE can have values
*     SUBPAR__TASK => this is a stand-alone task.
*     SUBPAR__TERM => connected directly to the terminal. In this case, 
*                     output and prompting goes directly to the terminal 
*                     rather than to another task.
*     SUBPAR__UTASK => this is a UTASK. Call the UTASK library for 
*                      terminal output. Prompting should never occur.
*
      INTEGER RUNPATH
      INTEGER RUNID
      INTEGER RUNFACE

      COMMON / SUBPARRUN / RUNPATH, RUNID, RUNFACE

*
*   Number of program within a monolith
*
      INTEGER PROGNUM            ! current program number within a
                                 ! monolith.
                                 ! MONOLITH => PROGNUM = action number
                                 ! otherwise PROGNUM = 1

      COMMON / MONPOINTER / PROGNUM

*
*   Flag for controlling whether the task fixed part checks items on a 
*   NEEDS list.
*
      LOGICAL CHECKNEEDS         ! .TRUE. => NEEDS list checked before 
                                 ! ACT called

      COMMON / NEEDSFLAG / CHECKNEEDS

*
*   Terminal size for help paging control
*
      INTEGER SUBPARPGSZ           ! Page size of help screen

      INTEGER SUBPARLCNT           ! Help line down counter

      COMMON / SUBPARTERM / SUBPARPGSZ, SUBPARLCNT
      
      SAVE / SUBPARVALS /
      SAVE / SUBPARVALC /
      SAVE / SUBPARDAT /
      SAVE / SUBPARDATC /
      SAVE / SUBPARACTS /
      SAVE / SUBPARACTC /
      SAVE / SUBPARNEEDS /
      SAVE / SUBPARCONST /
      SAVE / SUBPARCONSTC /
      SAVE / SUBPARPTR /
      SAVE / SUBPARPTRC /
      SAVE / SUBPARPSV /
      SAVE / SUBPARLOCS /
      SAVE / SUBPARRUN /
      SAVE / MONPOINTER /
      SAVE / NEEDSFLAG /
      SAVE / SUBPARTERM /

************************************************************************
*  End of SUBPAR_CMN include.
************************************************************************


      
*  Arguments Given:
      INTEGER NAMECODE
      INTEGER STATE
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL VOOD               ! Is the current value out of date?
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      
*  See if it has a value which is out of date.  This should be the case
*  if the state into which we are about to restore it (its initial
*  state) is not one which will should have had a value when it was
*  set up.  I may not have the logic here correct for all circumstances.
      VOOD = STATE .NE. SUBPAR__ACTIVE

*  If the value is out of date, mark the locators as inactive.
      IF ( VOOD ) CALL SUBPAR_CANCL( NAMECODE, STATUS )

*  Set the parameter's state to the requested one.
      PARSTATE( NAMECODE ) = STATE

      END
* $Id$
