      SUBROUTINE AGPWRT (XPOS,YPOS,CHRS,NCHS,ISIZ,IORI,ICEN)
*+
*  Name:
*     AGPWRT

*  Purpose:
*     SNX version of NCAR routine of the same name.

*  Language:
*     Starlink Fortran 77

*  Description:
*     This routine is a substitute for the NCAR routine of the
*     same name but allowing the PWRITX character drawing routine
*     to be used instead of the usual PWRIT, giving access to
*     special fonts etc.
*
*     The character string accepted by the PWRITX routine includes
*     control codes to switch font etc.  This implementation of
*     AGPWRT accepts and passes PWRITX codes and also inserts the
*     appropriate codes where lowercase and certain other characters
*     are supplied.  Some special characters not directly available
*     from PWRITX are produced by means of control sequences selected
*     through additional codes which are peculiar to this
*     implementation of AGPWRT.
*
*     Other special values of the ICEN parameter are used to select
*     plotting via either the PWRITX routine or the PWRIT routine.
*     This feature allows an application to plot using GKS character
*     drawing facilities on some occasions while using PWRITX on other
*     occasions.  The PWRITX option gives high quality characters from
*     a rich set, but slowly;  the GKS option, on the other hand,
*     provides access to other GKS fonts and allows the use of lower
*     text precision on graphics devices where this will reduce the
*     plotting time.
*
*     The NCAR utilities use values of the justification argument ICEN
*     argument of +/-1 where left or right justification is required,
*     and in these cases the string is plotted artificially monospaced to
*     avoid irregular axis labelling, subject to the restriction that
*     the string must not contain PWRITX function codes (though lowercase
*     and common punctuation symbols are acceptable).  To enable left
*     and right justified strings to be proportionally spaced and to
*     contain PWRITX function codes, this implementation of AGPWRT also
*     supports the non-NCAR values ICEN = +/-2.  There is no provision
*     for centred monospaced strings.
*
*  Arguments:
*     XPOS,YPOS = REAL (Given)
*         String position in SPPS user coordinates
*     CHRS = CHAR (Given)
*         String, inclusive of codes etc.
*     NCHS = INTEGER (Given)
*         Length of CHRS.
*     ISIZ = INTEGER (Given)
*         Character size
*     IORI = INTEGER (Given)
*         Orientation
*     ICEN = INTEGER (Given)
*         Justification, or PWRIT/PWRITX selection.
*         Where the special values of ICEN are used to select plotting
*         via either PWRIT or PWRITX, the other arguments are ignored.
*
*  Plotter units:
*     Displacements and character sizes are specified in "plotter
*     units".  (n.b.  Do not confuse plotter units with device
*     coordinates.  Plotter units are not related to true device
*     resolution.)  Normally the longest dimension of the display
*     surface is 1024 plotter units.  Should the resolution
*     limitation that this implies be unacceptable, it can be
*     changed by means of the SPPS routine SETI.
*
*  PWRITX fonts:
*     Twelve fonts are provided, each containing 47 symbols and indexed
*     by the standard Fortran characters (A-Z 0-9 +-*/()$=,. and space).
*     A given font is specified by three letters which in broad terms
*     select which combination of size (Principal, Indexical,
*     Cartographic), alphabet (Roman, Greek) and case (upper, lower) is
*     required.
*
*  ISIZ argument:
*     ISIZ specifies an overall magnification factor for all
*     the characters drawn.
*
*     Each of the three size classifications in the set of twelve
*     fonts provided (Principal, Indexical and Cartographic) has a
*     nominal character width (in spite of proportional spacing),
*     height and total height.  The space character is exactly one
*     nominal width wide, and a carriage control control code causes
*     a downwards increment equal to the nominal total height, which
*     thus includes white space.
*
*     The nominal width (W), height (H) and total height (T) for
*     the Principal (P), Indexical (I) and Cartographic (K) fonts
*     are as follows (plotter units):
*
*                     W        H        T
*             P      16       21       32
*             I      12       13       20
*             K       8        9       14
*
*     ISIZ values of 3 or less select standard magnifications of the
*     above sizes.  Values of 4 or more are proportional to the
*     resulting magnification.  The overall magnification for
*     different ISIZ values is as follows:
*
*                  ISIZ        mag
*
*                   <1         8/21
*                    1        12/21
*                    2        18/21
*                    3        24/21
*                   >3       ISIZ/21
*
*     Thus, for ISIZ>3, Principal characters will be drawn nominally
*     ISIZ plotter units high, and ISIZ can simply be thought of
*     as the nominal character height in plotter units.

*  IORI argument:
*     IORI is the string orientation in degrees anticlockwise
*     from the normal left-to-right.

*  ICEN argument (normal use to specify positioning):
*          ICEN                   meaning
*
*           -2            (XPOS,YPOS) is the centre of the left
*                         edge of the first character.  The string
*                         is proportionally spaced.
*
*           -1            (XPOS,YPOS) is the centre of the left
*                         edge of the first character.  The string
*                         is monospaced.  Lowercase and punctuation
*                         characters are permitted, but not function
*                         codes.
*
*            0            (XPOS,YPOS) is the centre of the entire
*                         string.  The string is proportionally
*                         spaced.
*
*           +1            (XPOS,YPOS) is the centre of the right
*                         edge of the last character.  The string
*                         is monospaced.  Lowercase and punctuation
*                         characters are permitted, but not function
*                         codes.
*
*           +2            (XPOS,YPOS) is the centre of the right
*                         edge of the last character.  The string
*                         is proportionally spaced.

*  ICEN argument (special use to select string drawing routine):
*          ICEN                   meaning
*
*           -100          Selects PWRIT routine, which gives access
*                         to GKS fonts, precision etc.
*
*           +100          Selects PWRITX routine, which gives access
*                         to special fonts and other features.
*
*     Notes:
*       1) For ICEN = +/-100 the other arguments are ignored.
*       2) Initially, PWRITX is selected.
*       3) This inelegant use of the ICEN argument is a consequence
*          of having to work within the standard AGPWRT call, which
*          is made directly by the AUTOGRAPH utilities.

*  Characters available without using function codes:
*     1)  All uppercase and lowercase Roman characters, and most common
*         punctuation symbols are available by including them literally
*         in the string CHRS.
*     2)  Two consecutive apostrophes causes a single apostrophe to be
*         drawn - for example "Murphy''s Law".
*
*     Both of these are features of this implementation of AGPWRT and
*     are not available when directly calling PWRITX.

*  Function codes:
*     Function codes are sequences of characters, enclosed within
*     apostrophes, which may (except when ICEN=+/-1) be included in the
*     character string CHRS to change font, case, etc. within the plotted
*     string.  No punctuation is needed between functions except for a
*     comma between adjacent numbers;  however, commas may be used between
*     functions to improve readability.  The following are the only legal
*     function codes.  Any other characters in a function string will be
*     ignored except that an error report will be issued and, if more
*     than 10 errors occur within a string, control will be returned to
*     the main program without further plotting.  At the start of the
*     string, size, type and case are Principal, Roman and Upper.
*
*     FONT ALPHABET
*
*        R         Roman characters etc
*        G         Greek characters etc
*
*     FONT SIZE (see table, above, under ISIZ)
*
*        P         Principal fonts
*        I         Indexical fonts
*        K         Cartographic fonts
*
*     FONT CASE
*
*        U or Un   Upper case.  If U is followed by a number n (not
*                  separated by a comma) then n characters will be drawn
*                  in uppercase and subsequent characters will be in
*                  lowercase.  The U1 option is thus particularly useful
*                  for capitalizing sentences.
*        L or Ln   Lower case.  If L is followed by a number n, then n
*                  characters will be drawn in lower case and subsequent
*                  characters will be in upper case.
*
*     SUBSCRIPTS AND SUPERSCRIPTS
*
*        S or Sn   Superscript level.
*        B or Bn   Subscript level.
*        N or Nn   Normal level.
*
*        When switching from some "base" character size to super- or
*        subscripting, the character size will change depending on
*        the base character size.  Principal base characters will be
*        subscripted or superscripted with indexical characters, with
*        a 10 plotter unit shift (scaled in accordance with ISIZ) up
*        or down.  Indexical and cartographic base characters will be
*        sub- or superscripted with cartographic characters with a
*        7 plotter unit shift.
*
*        If multiple S or B functions are used the original base
*        character is forgotten, and the base is simply the one
*        preceding the latest S or B.
*
*        The case of the indexing characters will generally be the
*        same as that of the base character unless otherwise specified.
*        There is one exception: a lower case indexical base will be
*        super- or subscripted with upper case cartographic, as the
*        lowercase cartographic font is composed of special characters
*        rather than letters and numbers.
*
*        If S, B or N is followed by a number n, then n characters will
*        be drawn as specified above, after which character size, case
*        and position will be reset to that of the base characters.
*        If n is negative, its absolute value will be used instead;  n
*        cannot be zero.  Do not overlap level definitions given for a
*        specified number of characters.
*
*        The N option returns character, case and size to that of the
*        base but maintains the current character position.
*
*        For example:  'U1'T'S1'EST    will be drawn   e
*                                                     Tst
*
*                and   'U1'T'S'E'N'ST  will be drawn   e
*                                                     T st
*
*     DIRECT CONTROL OVER POSITION
*
*        Hn, HnQ   Increment horizontally in the frame.  Hn will
*                  shift the present X position by n plotter units.
*                  HnQ will shift the present X position by n nominal
*                  character widths.  Positive n shifts to the right,
*                  and negative to the left.  If n is omitted, a
*                  value of 1 is assumed.
*
*        Vn, VnQ   Increment vertically in the frame.  Vn will shift
*                  the present Y position by n plotter units.  VnQ
*                  will shift the present Y position by n nominal
*                  character total heights.  Positive n shifts
*                  upwards, and negative downwards.  If n is omitted,
*                  a value of 1 is assumed.
*
*        H:n       Increment along the direction of the string by
*                  n percent of the nominal character height for
*                  Principal characters.  If n is positive the
*                  shift is rightwards, and if negative the shift
*                  is leftwards.  If n is absent there is no shift.
*
*        V:n       Increment at right angles to the direction of the
*                  string by n percent of the nominal character
*                  height for Principal characters.  If n is positive
*                  the shift is upwards, and if negative the shift is
*                  downwards.  If n is absent there is no shift.
*
*        (Note: these two are nonstandard features peculiar to this
*         implementation of AGPWRT and not available when PWRITX is
*         called directly.)
*
*        X or Xn   Set X or Y.  If the X or Y appears without a number n,
*    and Y or Yn   the function will do nothing.  Otherwise, the character
*                  coordinate in the X or Y direction will be set to the
*                  plotter coordinate n, so that the next character drawn
*                  will be have this position in X or Y subsequent
*                  characters will be drawn from this position.
*
*        (Note: within PWRITX, interactions with the proportional font
*         and justification logic make these functions hard to use, and
*         they are not recommended.)
*         
*        C         Carriage return: a carriage return and line feed
*                  will be done before the next character is plotted.
*                  n.b. The justification applies to the final line.
*
*     DIRECTION
*
*        D or Dn   Write down, rather than across the frame.  If D appears
*                  without an n or if n=0, all characters will be written
*                  down, until an 'A' function is encountered.  If D is
*                  followed by a number n, n characters will be written
*                  down and subsequent characters will be written across
*                  the frame.  If n is negative, the absolute value of n
*                  is used instead.
*        A         Write across: escape from the D option.
*
*     SPECIAL CHARACTERS
*
*        .A        Angstrom unit
*
*        For example, to draw a string meaning "three Angstrom units per
*        millimetre" we might use:
*                  CALL AGPWRT (XPOS,YPOS,'3''.A''/mm', ...
*
*     DIRECT CHARACTER ACCESS
*
*        nnn       Numeric character: character number nnn (octal) will
*                  be drawn.

*  Externals:
*     AGGETI, AGSETI, SETER, PWRITX, PWRIT, GTNUM,
*     KUPX, KUPY, CPUX, CPUY,
*
*  Notes:
*     This routine uses characters outside the ANSI Fortran 77 character
*     set.

*  Authors:
*     PTW: Pat Wallace (Starlink)

*  History:
*     03-SEP-1990 (PTW):
*        Modified.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*-

      IMPLICIT NONE

*  Arguments
      REAL XPOS,YPOS
      CHARACTER CHRS*(*)
      INTEGER NCHS,ISIZ,IORI,ICEN

*  SPPS functions
      INTEGER KUPX,KUPY
      REAL CPUX,CPUY

*  String drawing routine flag:  1=PWRITX, 2=PWRIT
      INTEGER NRTNE
      SAVE NRTNE

*  PWRIT/PWRITX string justification
      INTEGER JCEN

*  Nominal character height (plotter units)
      REAL HEIGHT

*  String orientation (radians) and functions
      REAL THETA,STHETA,CTHETA

*  Degrees to radians
      REAL D2R
      PARAMETER (D2R=0.0174532922)

*  Reference x,y in plotter coordinates
      INTEGER IX0P,IY0P

*  Pitch of monospaced strings, fraction of height and in plotter units
      REAL PF,PP
      PARAMETER (PF=0.95)

*  Input character pointer
      INTEGER ICHRS

*  One and two characters from string
      CHARACTER C*1,C2*2

*  A single backslash, regardless of compiler behaviour.
      CHARACTER BS*1
      PARAMETER ( BS = '\\' )

*  The value '\KRLQ', using our single backslash (UNIX compilers
*  typically need '\\KRLQ', which is not as portable).
      CHARACTER KRLQ*(5)
      PARAMETER ( KRLQ = BS//'KRLQ' )

*  Distance along monospaced string in plotter units
      REAL ALONGP

*  Character position in plotter coordinates
      REAL X,Y

*  Auxiliary input string: size, string, pointers
      INTEGER NIP
      PARAMETER (NIP=100)
      CHARACTER IP*(NIP)
      INTEGER IPIN,IPOUT

*  Current and last font codes
      CHARACTER*3 FONT,OFONT

*  String for output: size, workspace, pointer
      INTEGER NOP
      PARAMETER (NOP=1000)
      CHARACTER OP*(NOP)
      INTEGER IOP

*  Flag: "in PWRITX function"
      LOGICAL INFN

*  For H: and V: functions - percent of character height
      INTEGER NPCENT
*  Shift in plotter units
      REAL RP
*  Horizontal and vertical components in plotter units
      INTEGER NHP,NVP

*  Strings for internal WRITEs
      CHARACTER HHHHH*5,VVVVV*5

*  Saved value of LINE/MAX parameter
      INTEGER LMAX

*
*  Translation table (n.b. uses characters outside the ANSI Fortran set)
*     character 1 = character to be plotted
*     characters 2-4 = required PWRITX font
*     character 5 = character in PWRITX font
*
      INTEGER NT,I
      PARAMETER (NT=92)
      CHARACTER*5 CHTAB(NT)
      DATA (CHTAB(I),I= 1,16)/'!PRL2','"PGL9','#PGL+','$PRU$',
     :                        '%PRL.','&PGU+','(PRU(',')PRU)',
     :                        '*PRU*','+PRU+',',PRU,','-PRU-',
     :                        '.PRU.','/PRU/','0PRU0','1PRU1'/
      DATA (CHTAB(I),I=17,32)/'2PRU2','3PRU3','4PRU4','5PRU5',
     :                        '6PRU6','7PRU7','8PRU8','9PRU9',
     :                        ':PRL0',';PRL1','<PRL8','=PRU=',
     :                        '>PRL9','?PRL3','@KRLA','APRUA'/
      DATA (CHTAB(I),I=33,48)/'BPRUB','CPRUC','DPRUD','EPRUE',
     :                        'FPRUF','GPRUG','HPRUH','IPRUI',
     :                        'JPRUJ','KPRUK','LPRUL','MPRUM',
     :                        'NPRUN','OPRUO','PPRUP','QPRUQ'/
      DATA (CHTAB(I),I=49,64)/'RPRUR','SPRUS','TPRUT','UPRUU',
     :                        'VPRUV','WPRUW','XPRUX','YPRUY',
     :                        'ZPRUZ','[PRL(', KRLQ,  ']PRL)',
     :                        '^KGL*','`PGL6','aPRLA','bPRLB'/
      DATA (CHTAB(I),I=65,80)/'cPRLC','dPRLD','ePRLE','fPRLF',
     :                        'gPRLG','hPRLH','iPRLI','jPRLJ',
     :                        'kPRLK','lPRLL','mPRLM','nPRLN',
     :                        'oPRLO','pPRLP','qPRLQ','rPRLR'/
      DATA (CHTAB(I),I=81,NT)/'sPRLS','tPRLT','uPRLU','vPRLV',
     :                        'wPRLW','xPRLX','yPRLY','zPRLZ',
     :                        '{KGL(','|KRLP','}KGL)','~PGU.'/

*  Default string drawing routine
      DATA NRTNE /1/




*  Type of call - normal, or selection of drawing routine?
      IF (ICEN.EQ.100) THEN

*     Select PWRITX routine
         NRTNE = 1

      ELSE IF (ICEN.EQ.-100) THEN

*     Select PWRIT routine
         NRTNE = 2

      ELSE

*     Normal call: establish PWRIT/PWRITX justification parameter
            IF (ICEN.EQ.-2) THEN
               JCEN = -1
            ELSE IF (ICEN.EQ.2) THEN
               JCEN = 1
            ELSE
               JCEN = ICEN
            END IF

*     Which character drawing routine is to be used?
         IF (NRTNE.EQ.2) THEN

*        PWRIT to be used: draw the string
            CALL PWRIT(XPOS,YPOS,CHRS,NCHS,ISIZ,IORI,JCEN)

         ELSE

*        Establish nominal character height
            IF (ISIZ.LT.1) THEN
               HEIGHT = 8.0
            ELSE IF (ISIZ.EQ.1) THEN
               HEIGHT = 12.0
            ELSE IF (ISIZ.EQ.2) THEN
               HEIGHT = 16.0
            ELSE IF (ISIZ.EQ.3) THEN
               HEIGHT = 24.0
            ELSE
               HEIGHT = REAL(ISIZ)
            END IF

*        Functions of string orientation
            THETA = REAL(IORI)*D2R
            STHETA = SIN(THETA)
            CTHETA = COS(THETA)

*        Is artificial monospacing required?
            IF (ICEN.EQ.-1.OR.ICEN.EQ.1) THEN

*           Yes: transform string position to plotter coordinates
               IX0P = KUPX(XPOS)
               IY0P = KUPY(YPOS)

*           Pitch in plotter units
               PP = PF*HEIGHT

*           Loop character by character
               DO 300 ICHRS=1,NCHS
                  C = CHRS(ICHRS:ICHRS)

*              Translate character into PWRITX sequence
                  DO 100 I=1,NT
                     IF (C.EQ.CHTAB(I)(1:1)) THEN
                        FONT = CHTAB(I)(2:4)
                        C = CHTAB(I)(5:5)
                        GO TO 200
                     END IF
 100                 CONTINUE

*              Not found: use space
                  FONT = 'PRU'
                  C = ' '

*              Distance along string in plotter units
 200              CONTINUE
                  IF (ICEN.EQ.-1) THEN

*                 Left justified
                     ALONGP = (REAL(ICHRS)-0.5)*PP

                  ELSE

*                 Right justified
                     ALONGP = (REAL(ICHRS-NCHS)-0.5)*PP

                  END IF

*              Character coordinates
                  X = CPUX(IX0P+NINT(ALONGP*CTHETA))
                  Y = CPUY(IY0P+NINT(ALONGP*STHETA))

*              Draw the character
                  CALL PWRITX(X,Y,''''//FONT//''''//C,6,
     :                        ISIZ,IORI,0)

*              Next character
 300              CONTINUE

            ELSE IF (ICEN.EQ.-2 .OR.
     :               ICEN.EQ.0 .OR.
     :               ICEN.EQ.2) THEN

*           Not artificially monospacing:  reset pointers
               ICHRS = 1
               IPIN = 1
               IPOUT = 1
               IOP = 1

*           Reset "in PWRITX function" flag
               INFN = .FALSE.

*           PWRITX to be used: reset "last font"
               OFONT = ' '

*           Main loop: next input character
 400           CONTINUE

*           Anything in auxiliary input string?
               IF (IPIN.LT.IPOUT) THEN

*              Yes: get next 2 characters and increment pointer by 1
                  C2 = IP(IPIN:MIN(IPIN+1,IPOUT-1))
                  IPIN = IPIN+1

*           Any primary input left?
               ELSE IF (ICHRS.LE.NCHS) THEN

*              Yes: get next 2 characters and increment pointer by 1
                  C2 = CHRS(ICHRS:MIN(ICHRS+1,NCHS))
                  ICHRS = ICHRS+1

               ELSE

*              Both inputs exhausted: exit from main loop
                  GO TO 1000

               END IF

*           Consecutive apostrophes?
               IF (C2.EQ.'''''') THEN

*              Yes: skip additional character of input
                  IF (IPIN.LT.IPOUT) THEN
                     IPIN = IPIN+1
                  ELSE
                     ICHRS = ICHRS+1
                  END IF

*              Inside function code?
                  IF (.NOT.INFN) THEN

*                 No: append apostrophe sequence to auxiliary input
                     IF (IPOUT+10.GT.NIP) GO TO 9090
                     IP(IPOUT:IPOUT+10) = '''PGL''8''PRU'''
                     IPOUT = IPOUT+11

                  END IF

               ELSE

*              Not consecutive apostrophes: get current character
                  C = C2(1:1)

*              Function code delimiter?
                  IF (C.EQ.'''') THEN

*                 Either append to output or eliminate redundant pair
                     IF (IOP.GT.1) THEN
                        IF (OP(IOP-1:IOP-1).EQ.'''') THEN
                           IOP = IOP-1
                        ELSE
                           IF (IOP.GT.NOP) GO TO 9090
                           OP(IOP:IOP) = ''''
                           IOP = IOP+1
                        END IF
                     ELSE
                        IF (IOP.GT.NOP) GO TO 9090
                        OP(IOP:IOP) = ''''
                        IOP = IOP+1
                     END IF

*                 Flip "in function code" toggle
                     INFN = .NOT.INFN

*                 New function, and last char was lowercase etc?
                     IF (INFN.AND.OFONT.NE.'PRU') THEN

*                    Yes: reselect default font
                        IF (IOP+2.GT.NOP) GO TO 9090
                        OP(IOP:IOP+2) = 'PRU'
                        IOP = IOP+3
                        OFONT = 'PRU'

                     END IF

                  ELSE

*                 Within function?
                     IF (INFN) THEN

*                    Yes: look for H:,V: and special character codes
                        IF (C2.EQ.'V:' .OR.
     :                      C2.EQ.'H:' .OR.
     :                      C2.EQ.'.A' ) THEN

*                       Found: skip additional character of input
                           IF (IPIN.LT.IPOUT) THEN
                              IPIN = IPIN+1
                           ELSE
                              ICHRS = ICHRS+1
                           END IF

*                       What was it - H: or V:?
                           IF (C2.EQ.'H:'.OR.C2.EQ.'V:') THEN

*                          H: or V: - decode number
                              IF (IPIN.LT.IPOUT) THEN
                                 IPIN = IPIN-1
                                 CALL GTNUM(IP,IPOUT-1,IPIN,NPCENT)
                                 IPIN = IPIN+1
                              ELSE
                                 ICHRS = ICHRS-1
                                 CALL GTNUM(CHRS,NCHS,ICHRS,NPCENT)
                                 ICHRS = ICHRS+1
                              END IF

*                          Shift in plotter units
                              RP = MIN(HEIGHT*REAL(NPCENT)/100.0,
     :                                 9999.0)

*                          Horizontal and vertical components
                              IF (C2.EQ.'H:') THEN
                                 NHP = NINT(RP*CTHETA)
                                 NVP = NINT(RP*STHETA)
                              ELSE
                                 NHP = -NINT(RP*STHETA)
                                 NVP = NINT(RP*CTHETA)
                              END IF

*                          Append PWRITX control sequence to output
                              IF (NHP.NE.0) THEN
                                 WRITE (HHHHH,'(SP,I5.4)') NHP
                                 IF (IOP+5.GT.NOP) GO TO 9090
                                 OP(IOP:IOP+5) = 'H'//HHHHH
                                 IOP = IOP+6
                              END IF
                              IF (NVP.NE.0) THEN
                                 WRITE (VVVVV,'(SP,I5.4)') NVP
                                 IF (IOP+5.GT.NOP) GO TO 9090
                                 OP(IOP:IOP+5) = 'V'//VVVVV
                                 IOP = IOP+6
                              END IF

                           ELSE IF (C2.EQ.'.A') THEN

*                          Angstrom: append sequence to auxiliary input
                              IF (IPOUT+28.GT.NIP) GO TO 9090
                              IP(IPOUT:IPOUT+28) =
     :                           'H:36V:75PGL''Z''RUH:-56V:-75''A'''
                              IPOUT = IPOUT+29

                           END IF

                        ELSE

*                       Part of normal function code: append to output
                           IF (IOP.GT.NOP) GO TO 9090
                           OP(IOP:IOP) = C
                           IOP = IOP+1

                        END IF

                     ELSE

*                    Not within function code: underscore character?
                        IF (C.EQ.'_') THEN

*                       Underscore: append sequence to auxiliary input
                           IF (IPOUT.GT.NIP) GO TO 9090
                           IP(IPOUT:IPOUT) = ''''
                           IPOUT = IPOUT+1
                           FONT = 'PRU'
                           IF (FONT.NE.OFONT) THEN
                              IF (IPOUT+2.GT.NIP) GO TO 9090
                              IP(IPOUT:IPOUT+2) = FONT
                              IPOUT = IPOUT+3
                           END IF
                           IF (IPOUT+12.GT.NIP) GO TO 9090
                           IP(IPOUT:IPOUT+12) = 'V:-50''-''V:50'''
                           IPOUT = IPOUT+13

                        ELSE

*                       Normal character: translate into PWRITX sequence
                           DO 500 I=1,NT
                              IF (C.EQ.CHTAB(I)(1:1)) THEN
                                 FONT = CHTAB(I)(2:4)
                                 C = CHTAB(I)(5:5)
                                 GO TO 600
                              END IF
 500                          CONTINUE

*                       Not found: use space
                           FONT = OFONT
                           C = ' '

*                       Append to output, avoiding redundancies
 600                       CONTINUE
                           IF (FONT.NE.OFONT) THEN
                              OFONT = FONT
                              IF (IOP.GT.1) THEN
                                 IF (OP(IOP-1:IOP-1).EQ.'''') THEN
                                    IOP = IOP-1
                                 ELSE
                                    IF (IOP.GT.NOP) GO TO 9090
                                    OP(IOP:IOP) = ''''
                                    IOP = IOP+1
                                 END IF
                              ELSE
                                 IF (IOP.GT.NOP) GO TO 9090
                                 OP(IOP:IOP) = ''''
                                 IOP = IOP+1
                              END IF
                              IF (IOP+3.GT.NOP) GO TO 9090
                              OP(IOP:IOP+3) = FONT // ''''
                              IOP = IOP+4
                           END IF
                           IF (IOP.GT.NOP) GO TO 9090
                           OP(IOP:IOP) = C
                           IOP = IOP+1

                        END IF

                     END IF

                  END IF

               END IF

*           Next input character
               GO TO 400

*           No more input characters
 1000          CONTINUE

*           Save current value of maximum line length parameter
               CALL AGGETI('LINE/MAXIMUM.',LMAX)

*           Set line length parameter to accommodate current line
               CALL AGSETI('LINE/MAXIMUM.',IOP-1)

*           Draw the characters
               CALL PWRITX(XPOS,YPOS,OP,IOP-1,ISIZ,IORI,JCEN)

*           Restore original line length parameter
               CALL AGSETI('LINE/MAXIMUM.',LMAX)

            ELSE

*           Unrecognised ICEN value
               GO TO 9010

            END IF

         END IF

      END IF

*  Finished
      GO TO 9999

*  Fatal errors
 9010 CONTINUE
      CALL SETER('AGPWRT: ILLEGAL ICEN VALUE',102,2)
      GO TO 9999

 9090 CONTINUE
      CALL SETER('AGPWRT: WORKSPACE OVERFLOW',
     :           103,2)

*  Exit
 9999 CONTINUE

      END
