proc HelpText helpItem {

#+ HelpText
#
#  Display help text.  A scrolling window of help text is displayed.
#  The input argument defines which item of text is listed.
#
#  Given
#    helpItem  Name of the item of text to be listed.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)
#
#  History:
#   18/10/94 (ACD): Original version.
#   21/3/95  (ACD): First stable version.
#   28/2/96  (ACD): Modified text to reflect additional options.
#   5/12/96  (ACD): Added entries for windows for computing statistics.
#   19/12/96 (ACD): Completed text for computing statistics and
#     modified for CURSA version 2.1.
#   31/3/97  (ACD): Added hooks for the windows for accessing remote
#     catalogues.
#   23/11/98 (ACD): Added entries for the scatter-plot and scatter-plot
#     overlay windows.  Also corrected a couple of minor mistakes and
#     brought some of the general help information up to date.
#   15/5/01  (ACD): Modified for CURSA version 6.3.  The main changes were
#     to revise the description of remote catalogue access and to document
#     the 'Quiet Mode' configuration option.
#-

#
#  Create the toplevel window to hold the messages.

    toplevel     .helptext -class Dialog   -bd 10
    wm title     .helptext "Help on choosing columns"
    wm iconname  .helptext "C H"
    wm transient .helptext .
    wm minsize   .helptext 1 1

#
#  Define the output region for the messages window.

    frame  .helptext.text  -relief sunken  -bd 2

    scrollbar .helptext.text.yscroll -orient vertical \
       -command {.helptext.text.output yview} \
       -relief sunken  -bd 2
    text .helptext.text.output -wrap none \
       -yscroll {.helptext.text.yscroll set}

    pack .helptext.text.yscroll -side right -fill y
    pack .helptext.text.output -expand y

    pack .helptext.text -side top

#  Define the "OK" button.

    button .helptext.ok -text OK -width 6 -command {set HTButton "ok"}

    frame .helptext.default -relief sunken -bd 1
    raise .helptext.ok    .helptext.default
    pack .helptext.default -side top -expand 1 -padx 3m -pady 2m
    pack .helptext.ok -in .helptext.default -padx 1m -pady 1m -ipadx 1m

#
#  Use a case statement to select the item of text to be displayed.

    case $helpItem in {

    "introduction_help" {

    .helptext.text.output insert end "
xcatview is a Starlink application for browsing or viewing the contents
of an astronomical catalogue or similar tabular dataset.  It can access
catalogues and tables held as either ASCII or binary FITS tables and
several other formats.  xcatview provides functions to:

*  list the contents of a catalogue,

*  list auxiliary information about a catalogue,

*  select subsets from a catalogue,

*  save part or all of a catalogue as a text file suitable for printing,

*  save a selection as a catalogue.

You can use xcatview to view either the entire catalogue, or subsets
created from it according to some criteria which you supply (these
subsets are called 'selections').  Usually a catalogue will contain more
rows than the twenty-odd which can be fitted into xcatview's main
display region.  Thus, functions are provided to 'move' or 'navigate'
through the catalogue or selection, displaying a strip of contiguous
rows.  Similarly, often a catalogue will have more columns than can fit
into the main display region, and again options are provided to choose
the columns to be displayed.

Using xcatview you can create an arbitrary number of selections from
the catalogue, each defined by its own criteria.  Any of these
selections can be displayed or saved as a text file or catalogue.
xcatview has the notion of a 'current selection', which is the
selection that it is currently working on.  It is the current selection
which is displayed or saved as a text file or catalogue.  Similarly when
a new selection is created it is generated from the current selection.
By default the most recent selection is the current one, though you may
choose to make any of the selections the current one.  If no selections
have been made, the current selection is the entire catalogue.

xcatview is part of the CURSA package for manipulating astronomical
catalogues and tables.  CURSA is documented in SUN/190.  There is also
a 'home page' giving additional information about CURSA (and xcatview)
available via the World Wide Web.  Its URL is:

   http://www.starlink.ac.uk/cursa/

The various catalogue formats which xcatview can access are fully
described in these documents.

"
    }

    "mainwindow_help" {

    .helptext.text.output insert end "
Currently you are accessing the primary window for navigating through
a catalogue and displaying information from it.  It is divided
horizontally into four components:


*  along the top is a list of options for controlling xcatview,

*  beneath this is the main display region, which is used to view the
   rows and columns of the current selection, and also to display
   various auxiliary information about the catalogue,

*  next is a row of buttons for navigating through the current
   selection,

*  Finally, at the bottom, is a smaller window where any error,
   warning or information messages are displayed.


The options along the top strip provide access to related facilities.
Briefly, they are:


File:  opening and saving catalogues, also exiting from xcatview,

Listing:  display information about the catalogue,

Selection:  create and manipulate selections,

Plotting:  plot scatter-plots and histograms from selections,

Configuration:  configure xcatview,

Help:  general help about xcatview (this text).


The row of buttons below the main display area is used to navigate
through the current selection.  The purpose of each button is:


Next:  display the next page of the current selection.  A series of
   clicks on this button will progress sequentially through the current
   selection,

Previous:  display the previous page in the current selection, that is,
   the set of rows immediately preceding those currently displayed,

Position:  display a page of rows in the current selection, starting at
   an a arbitrary position, which you will specify,

Top:  display the first few rows in the current selection,

Bottom:  display the last few rows in the current selection.

"
    }

    "assistance_help" {

    .helptext.text.output insert end "
If you are experiencing difficulties using xcatview then in the first
instance you should probably seek advice and assistance from your local
site manager.  Bug reports should be sent to username:

   starlink@jiscmail.ac.uk

In case of difficulty you should contact the Starlink Software Librarian,
as follows.

e-mail: starlink@jiscmail.ac.uk

Bug reports should always be sent to username starlink@jiscmail.ac.uk.
However, you are welcome to contact me directly for advice and assistance.
Suggestions for enhancements and improvements to xcatview are also
welcome.  Details of how to contact me are given below.

Clive Davenhall.

e-mail: acd@roe.ac.uk

post:   Institute for Astronomy,
        Royal Observatory,
        Blackford Hill,
        Edinburgh,
        EH9 3HJ,
        United Kingdom.

fax:    from within the United Kingdom:     0131 668 8416
        from overseas:                   +44 131 668 8416

Original version: 17th March 1995,
Most recently modified: 15th May 2001.
"
    }

    "GetCatalogue_help" {

    .helptext.text.output insert end "
The purpose of this window is to allow you to choose a catalogue to be
viewed with xcatview.  It provides two basic functions:


*  navigation through your directory tree to locate the catalogue of
   interest,

*  selection of a catalogue from your current directory.


There are two display boxes in the top half of the window:


*  the left hand box:  lists all the subdirectories in the current
   directory,

*  the right hand box:  lists all the catalogues in the current
   directory.


In the lower portion of the window, above the row of buttons, are two
horizontal boxes, labelled 'Current directory:' and 'Catalogue:':


*  the 'Current directory:' box:  shows your current directory,

*  the 'Catalogue:' box:  shows any catalogue which you have chosen.


To choose a catalogue from the current directory:


*  click on one of the catalogues in the list of catalogues, and it will
   appear in the 'Catalogue:' box,

*  alternatively, you can type the name of the catalogue into the
   'Catalogue:' box (though beware that there is no check that the name
   you have typed corresponds to a catalogue),

*  Finally, when you are satisfied with the name of the catalogue
   appearing in the 'Catalogue:' box then either click the \"OK\"
   button or hit return in the 'Catalogue:' box.


To navigate through your directory tree:


*  to move to a subdirectory of the current directory:  click on the
   appropriate entry in the list of subdirectories,

*  to move up a level in the directory tree:  click on the \"Up\"
   button,

*  to move to your 'home' directory:  click on the \"Home\" button,

*  alternatively, to move to any named directory you can click on the
   'Current directory:' box, type in the name of the required directory
   (or edit the existing directory name) and hit return.


In any event, when you move to a new directory the lists of
subdirectories and catalogues will be updated automatically to
correspond to the contents of this new directory.
"
    }

    "GetRow_help" {

    .helptext.text.output insert end "
Select the required row by either clicking on the slider and moving
it or typing the required row number into the entry box.

Finally, to accept the value given either click on the \"OK\" button or
hit return.

"

    }

    "GetCat_help" {

    .helptext.text.output insert end "
This window allows you to save the current selection as a catalogue.

You should enter the file name required for the new catalogue in the box
labelled 'Catalogue:' towards the top of the window.  If you simply
enter a file name the catalogue will be created in your current
directory.  Alternatively, you can create the catalogue in another
directory by prefixing the file name with the appropriate Unix directory
specification.

If you want to annotate the catalogue with descriptive comments then
type the required comments into the box labelled 'Comments:'.  These
comments are optional, so you can omit them if you wish.

The new catalogue may contain either all the columns in the original
catalogue or just the currently chosen columns.  Similarly, it may
optionally contain a copy of any header information associated with the
original catalogue.  These items are specified by the two buttons on the
left of the lower portion of the window.  The text to the right of each
button shows the option which is currently in effect.  To change the
option for either of the items: click on the button corresponding to the
item.  A list of the options for the item will appear; click on the one
that you require.  The current option shown to the right of the button
will be updated to reflect your choice.  Details of the individual items
are given below.

When you are happy with the file name for the catalogue and the items
to be included simply click on the \"OK\" button or hit return.


Details of the catalogue items
------------------------------

Columns:  specifies which columns will be saved in the new catalogue.
   There are two options: either all the columns in the original
   catalogue or just the currently chosen columns (as specified with
   the 'Choose columns to be listed' option under the 'Listing' option
   on the main window).  Note that if you choose the 'currently chosen
   columns' option then any expressions in the list will be converted to
   columns in the new catalogue.  (Options: all or current list).

Text:  specifies whether any header text associated with the original
   catalogue will be copied to the new catalogue.  Often such text will
   contain information intended to be read by users of the original
   catalogue, for example, perhaps an explanation of its contents or
   details of its provenance.  (Options: yes or no).
"

    }

    "GetFile_help" {

    .helptext.text.output insert end "
This window allows you to save the current selection and various other
items from the catalogue as a text file suitable for printing.

You should enter the name required for the output file in the box
labelled 'File name:' towards the top of the window.  If you simply
enter a file name the file will be created in your current directory.
Alternatively, you can create the file in another directory by prefixing
the name with the appropriate Unix directory specification.

Various optional items can be included in the file in addition to, or
instead of, the table of values in the current selection.  Examples
include a summary of the catalogue and details of all the columns that
it contains.  For some of these items various levels of detail may be
specified, for example, details of the columns may be absent, a summary
may be included or full details may be included.

The items to be included in the file are controlled by the vertical row
of buttons on the left of the middle portion of the window.  The text to
the right of each button shows the option which is currently in effect.
To change the option for any of the items: click on the button
corresponding to the item.  A list of the options for the item will
appear; click on the one that you require.  The current option shown
to the right of the button will be updated to reflect your choice.
Details of the individual items are given below.

You may also specify the range of rows in the current selection to be
listed.  Simply enter the numbers of the first and last rows required in
the boxes towards the bottom of the window.  If you want the last row
listed to be the last row in the current selection then, as an alternative
to entering the actual row number, simply type 'last' into the 'Last row:'
box, or alternatively (and even simpler) click on the 'Last row:' button
and the string 'last' will automatically be inserted into the 'Last
row:' box.

If you have set the 'Table' option to 'None' then obviously any range
of rows specified is irrelevant and will be ignored.

When you are happy with the file name and the items to be included
simply click on the \"OK\" button or hit return.


Details of the text file items
------------------------------

Summary:  a brief summary of the catalogue.  (Options: none or present).

Columns:  details of all the columns in the catalogue.  For every column
   the name, data type etc. is listed.  (Options: none, summary or
   full).

Parameters:  details of all the parameters in the catalogue.  For every
   parameter the name, data type etc. is listed.  (Options: none,
   summary or full).

Text:  a listing of any header text associated with the catalogue.
   Often such text will contain information intended to be read by
   users of the catalogue, for example, perhaps an explanation of its
   contents or details of its provenance.  (Options: none or present).

Table:  the table of values for the current selection.  Only the chosen
   columns will be listed.  The table may be listed either with or
   without column headings.  If column headings are chosen then the name
   and units of each listed column will be included at the top of each
   page in the file.  If column headings are not chosen then the table
   will be listed continuously, without any headings.  (Options: none,
   no column headings, with column headings).


Generating input for other programs
-----------------------------------

Sometimes you may wish to generate a file suitable for use as input to
another program.  Often programs will require an input file containing
the columns that they are to read without any additional annotation.
This effect can be achieved as follows.


*  Choose the columns to be listed to be just those which are to go into
   the file (use the 'Choose columns to be listed' option under the
   'Listing' option on the main window).

*  Set the text file options as follows:


  Summary     None

  Columns     None

  Parameters  None

  Text        None

  Table       No headings

*  Enter the first and last rows required.

*  Then enter the file name and save the file.
"
    }

    "GetRemoteQuery_help" {

    .helptext.text.output insert end "
This window allows you to access a catalogue held on-line on a remote
computer and to select from it objects which lie within a given angular
distance from a given position on the celestial sphere.  That is, the
selection finds all the objects in the catalogue inside a circular area
of sky.  This type of selection is sometimes called a 'cone search'.
The selected objects are saved as a catalogue in your current directory
and xcatview automatically opens this catalogue for viewing.  The details
of accessing remote catalogues, including various peculiarities, are
described in full in SUN/190.

On the left hand side is a list of all the remote catalogues which are
currently accessible.  This list might also contain pointers to additional
lists of catalogues.  The latter are identified by having entries which
end in '(directory)'.  It is possible to configure the list of catalogues
which are accessible.  However, this must be done prior to starting
xcatview.  See SUN/190 for details.

To make one of the additional lists of catalogues the current list simply
click on it, and after a few moments the new list will replace the
existing one.  To revert to the previous list click on the \"Previous\"
button, which is towards the bottom of the window.  If you have worked
through a series of lists then you can backtrack by making multiple clicks
on the  \"Previous\" button.

If the catalogue that you wish to search is in the current list then click
on its entry.  Its description and an abbreviation identifying it will be
displayed towards the top of the window.  Note that the abbreviation,
together with the central coordinates of the search, are used to create
the name given to the local catalogue containing the selected objects.

Next you need to specify the region of sky to be searched, which can
be done either by giving the name of an object on which the region is
centred or by giving the central Right Ascension and Declination.  If
you know the name of the central object then simply type it into the
'Object name:' box.  The case (upper or lower) of the supplied name
and any embedded spaces are ignored.  xcatview will attempt to use the
SIMBAD name resolver provided by the Centre de Donnees astronomiques
de Strasbourg (CDS) to look up coordinates for the given name.  SIMBAD
has a very extensive dictionary of names of astronomical objects, but
occasionally you may enter a name which it does not recognise.

Alternatively, you can enter the central Right Ascension and Declination
into the appropriate boxes.  These coordinates should be for equinox J2000.
If your coordinates are for some other equinox then you can use the
Starlink utility COCO (see SUN/56) to convert them to the required equinox.
Right Ascension should be in the format 'hh:mm:ss', for example '10:30:00'
corresponding to 10 hours 30 minutes.  The Declination should be in the
format 'sdd:mm:ss', for example '-30:45:00' corresponding to -30 degrees
45 minutes.  Note that for positive Declinations the sign is optional here.

Finally enter the radius from the central position within which objects are
to be selected, in minutes of arc, in the appropriate box.

Once you are happy with the query that you have specified click on
the \"OK\" button or hit return.  There will probably be a slight delay
while xcatview searches the remote catalogue.  Then it will open the
catalogue of selected objects that it has created, which will be available
for viewing in the normal manner.
"
    }

    "GetColumns_help" {

    .helptext.text.output insert end "
The purpose of this window is to choose the columns from the catalogue
which will be listed to the screen or to a text file.

On the left hand side of the window there is a list of all the columns
in the catalogue.  To choose the columns to be listed you should
proceed as follows.  Click on the required column or columns, then
click on the \"Copy\" button.  The chosen columns will appear in the list
on the right hand side of the window.  Repeat this procedure until you
have chosen all the columns that you require.

Alternatively, to choose all the columns in the catalogue click on the
\"All\" button and all the columns will appear in the list on the right
hand side of the window.

If you make a mistake and choose the wrong column you can remove it
from the list on the right hand side.  Simply click on the column or
columns to be removed (in the right hand list) and then click on
\"Clear\".  If you click on \"Clear\" without first choosing any columns
in the right hand list all the columns will be cleared.

In addition to listing columns from the catalogue, expressions may also
be listed.  To enter an expression click on the \"Exprn.\" button.  A
window will appear.  Enter the required expression and then click
\"OK\".  The expression will appear in the right hand list, along with
ordinary columns.  It may be cleared in the same way as ordinary
columns.  You may enter an arbitrary number of expressions.

Also you can control whether a sequence number is displayed for each
row listed, how angles (such as celestial coordinates) are displayed and
the width of the main display window.  To access these configuration
options click on the \"Config.\" button.

Finally, when you are satisfied with the list of columns, click on the
\"OK\" button.

"

    }


    "GetExprn_help" {

    .helptext.text.output insert end "
The purpose of this window is to define new columns to appear in
listings or in new catalogues saved with xcatview.

Enter the name required for the new column in the 'Name:' box.  The
rules for column names are as follows.


*  The name must be unique within the totality of parameters and
   columns for the catalogue.

*  A name can comprise up to fifteen characters.

*  A name can contain only: upper or lower case alphabetic characters
   (a-z, A-Z), numeric characters (0-9) and the underscore character
   ('_'). Note that corresponding upper and lower case characters are
   considered to be equivalent; that is, names are case insensitive.
   Thus, for example, the names: 'B_V', 'B_v' and 'b_v' would all refer to
   the same column.

*  The first character must be a letter.


Enter the expression defining the column in the 'Expression:' box.  The
expression defines the new column in terms of existing columns and
parameters and constants, linked by arithmetic operators.  See the help
entry 'Expression syntax' for details of the syntax of expressions.

Both the name and the expression are mandatory.

Optionally you may also enter units for the expression in the 'Units:'
box.

The data type of new columns defined as expressions is always DOUBLE
PRECISION.

Once you are happy with the values entered for the new column either
click on the \"OK\" button or hit return.

"

    }

    "expressions_help" {

    .helptext.text.output insert end "
Expressions
===========

Expressions in xcatview are used for two purposes:

*  computing a new column to appear in a listing,

*  defining a new selection.

The rules for expressions are similar in both cases and both usages are
described here.


Creating a new column
---------------------

Expressions for creating a new column have an algebraic format, and comprise
columns, parameters and constants linked by arithmetic operators and
mathematical functions.  For example, suppose that a catalogue contained
columns called x, y and z and parameters called p and q.  Some valid
expressions are:

x
p
x + p
(x + y + 2) / (p + q)
(2.0*x + y + 3.75*p) + 13.0) / (z + 1.8q)

Column and parameter names are not case sensitive; corresponding upper
and lower case letters are considered equivalent.  Thus, the following
names would all refer to the same column or parameter:

HD_NUMBER
HD_Number
hd_number


Defining a new selection
------------------------

Expressions for defining a selection have a similar algebraic format to
those for creating a new column.  However, they must include a
relational operator to define the selection criterion.  All the other
rules are exactly as for defining a new column.  Following the above
example some valid expressions for defining a selection are:

x > 3.0
x > y + p + 2.36
y = 3

Angles can be included in expressions using sexagesimal notation.  For
example:

dec > +190:30:00 .and. dec < +191:30:00

(remember that if a sexagesimal number is unsigned it is interpreted as
hours; to be interpreted as degrees it must be signed).


Details of expressions
======================

The arithmetic operators are:

+  addition,
-  subtraction,
*  multiplication,
/  division.

brackets (\"(\" and \")\") may be used as required.


Mathematical functions provided
-------------------------------

The Following mathematical functions are provided.  In the following list the
letters denote data types permitted: B = byte, H = halfint, I = integer,
R = real, D = double-precision, C = character, L = logical.
The appearance of N as an argument means that any numeric type (BHIRD)
is permitted, as a result it means that the type is the widest type of
any of the arguments.  R/D means that the result is REAL unless one or
more arguments is of double-precision type in which case D is the
result.

B = BYTE(N)       convert to BYTE             data type
H = HALF(N)          \"    \"  INTEGER*2         \"    \"
I = INT(N)           \"    \"  INTEGER           \"    \"
R = REAL(N)          \"    \"  REAL              \"    \"
D = DBLE(N)          \"    \"  DOUBLE PRECISION  \"    \"
I = NINT(N)       convert to nearest INTEGER
N = MIN(N,N)      the function must have precisely two arguments
N = MAX(N,N)      the function must have precisely two arguments
N = MOD(N,N)      remainder
N = ABS(N)        absolute value
R/D = SQRT(N)     square root
R/D = LOG(N)      natural logarithm
R/D = LOG10(N)    logarithm to the base 10
R/D = EXP(N)      exponential
R/D = SIN(N)      argument in radians
R/D = COS(N)      argument in radians
R/D = TAN(N)      argument in radians
R/D = ASIN(N)     result in radians
R/D = ACOS(N)     result in radians
R/D = ATAN(N)     result in radians
R/D = ATAN2(N,N)  result in radians
I = IAND(I,I)     logical AND
I = IOR(I,I)      logical OR
I = XOR(I,I)      logical exclusive OR
R/D = DTOR(N)     degrees to radians conversion
R/D = RTOD(N)     degrees to radians conversion
C = UPCASE(C)     convert character string to upper case
C = STRIP(C)      leading and trailing spaces are removed
C = SUBSTR(C,N,N) returns chars from positions arg2 to arg3 inclusive,
                  with the positions starting at 1.
L = NULL(*)       .TRUE. if argument is NULL.
D = HMSRAD(N,N,N) converts 3 args hours,mins,secs to radians
D = DMSRAD(C,N,N,N) first arg is the sign ('+' default), converts
                  degs,mins,secs to radians.
D = GREAT(N,N,N,N) great circle distance between two spherical
                  coordinates.  All the input arguments and the return
                  argument are in radians. The input arguments are in the
                  order: alpha(1), delta(1), alpha(2), delta(2).
D = PANGLE(N,N,N,N) position angle of point alpha(2), delta(2) from point
                  alpha(1), delta(1).  All the input arguments and the
                  return argument are in radians. The input arguments are
                  in the order: alpha_(1), delta_(1), alpha(2), delta_(2).


Rules for expressions
---------------------

The expression string can contain constants, variable names, operators,
functions, and parentheses.  In general the usual rules of algebra and
Fortran should be followed, with some minor exceptions as follows.

* Spaces are permitted between items, except that a function-name must
be followed immediately by a left parenthesis.  Spaces are not permitted
within items such as names and numerical constants, but can be used
within character strings and date/time values in curly braces.

* Lower-case letters are treated everywhere as identical to the
corresponding upper-case letter.

* Column and parameter names can be up to fifteen characters long, and
may consist of letters, digits, and underscores, except that the first
character must not be a digit.

* Array elements are supported but with a restricted syntax: they may
consist of a name followed by an unsigned integer constant subscript
enclosed in square brackets, for example MAGNITUDE\[13\].

* Character constants may be enclosed in a pair of single or double
quotes; embedded quotes of the same type may be denoted by doubling up
on the quote character within the string, for example 'DON''T or \"DON'T\".

* Logical constants may be .TRUE. or .FALSE. but abbreviations of the
word are allowed down to .T. and .F.

* Numerical constants may appear in any valid form for Fortran77 (except
that embedded spaces are not allowed).  Some additional forms are also
permitted, as shown below.

* %Xstring %Ostring %Bstring for hex/octal/binary integer constants.

* angles in sexagesimal notation: colons must to separate items, for
example hours:mins:secs (or degs:mins:secs).  If there is a leading sign
then the value will be taken as degs:mins:secs, otherwise hours:mins:secs.
In either case the value is internally converted to RADIANS.

* A date/time value may be given as a string enclosed in curly braces;
A range of common formats are permitted, with order year-month-day or
day-month-year, and the month as a number or three-character abbreviation.
The time may follow with colons separating hour:min:second.  Examples of
valid dates: {1992-JUL-26 12:34:56}  {92.7.26} {26/7/92T3:45}

* Relational operators are supported in both Fortran77 form (for example
.GE. .NE.) as well as in the Fortran90 forms (for example >= /= ).

* Single-symbol forms for .AND. .OR. and .NOT. are provided as an
alternative: & | # respectively.

* The dots may be left off the Fortran77 forms of the relational
operators and the logical operators .AND. and .OR. where spaces or
parentheses separate them from names or constants, but the logical
constants and the .NOT. operator need the enclosing dots to distinguish
them from other lexical items in all cases.

* Integer division does not result in truncation (as in Fortran) but
produces a floating-point result.  The NINT or INT function should be
used (as appropriate) if an integer result is required.

* The functions MAX and MIN may only have exactly two arguments.

* All arithmetic is carried out internally in DOUBLE PRECISION (but the
compiler works out the effective data type of the result using the
normal expression rules).

* Exponentiation is performed by log/exp functions, with use of ABS to
avoid taking logs of negative args, thus -2**3 will come out as +8 not
-8.


Operator precedence
-------------------

The operator precedence rules of Fortran90 are used as far as possible.
In the following table the larger numbers denote higher precedence
(tighter binding).

   precedence   function/operator
   ----------   -----------------
       2	start/end of expression
       4	(  )
       6	,
       8	.EQV.  .NEQV.
      10	.OR.   |
      12	.AND.  &
      14	.NOT.  #
      16	.EQ. .GE. .GT. .LE. .LT. .NE. == >= > <= < /=
      18	FROM  TO
      20	//
      22	+     -  (binary operators)
      24	+     -  (unary operators)
      26	*     /
      28	**
      30 	all functions

Note that all operators except ** associate from left to right, but **
and functions associate from right to left.

"

     }

    "GetDispConfig_help" {

    .helptext.text.output insert end "
This window allows you to set a few options to configure how xcatview
displays columns.

The items which may be controlled are displayed in the three buttons on
the left hand side of the window.  The text to the right of each button
shows the option which is currently in effect.  To change the option for
any of the items: click on the button corresponding to the item.  A list
of the options for the item will appear; click on the one that you
require.  The current option shown to the right of the button will be
updated to reflect your choice.  Details of the individual items are
given below.

When you are happy with the settings of the items then simply click on
the \"OK\" button or hit return.


Details of the configuration items
----------------------------------

Sequence Number:  specifies whether or not a sequence number is
   displayed at the start of every row listed in the current selection.

Angle Format:  specifies how columns of angles are displayed.  The
   options are either to display angles in hours or degrees formatted as
   sexagesimal values or to display them as floating point numbers
   in radians.  This option only affects columns which CAT understands
   to contain angles.  Briefly, such columns must:


   *  have data type DOUBLE PRECISION or REAL,

   *  have a UNITS attribute of the form RADIANS{sexagesimal format
      specifier} such as RADIANS{HOURS} or RADIANS{DEGREES},

   *  have values stored in the columns in radians.


Screen Width:  specifies the width of the main display window.  Obviously,
a wide window can accommodate more columns than a narrow one.  The values
correspond to the width of the window in characters.
"

     }

    "GetFormat_help" {

    .helptext.text.output insert end "
This window allows you to change the display format or units for a column
or expression.  You might want to change the display format in order to
produce tidier listings or to show more decimal places.  For columns of
angles changing the units similarly controls the way that the columns are
formatted for display.  For other columns you are less likely to need to
change the units (though perhaps you might want to shorten a long units
string in order to prevent it occupying too much space in the display).

On the left hand side is a list of columns.  Click on the column that you
require to change and its name, data type, current display format and
current units will be displayed in the column of entries down the centre
of the window.  By default the list on the left hand side of the window
shows all the columns in the catalogue.  You can use the button labelled
\"Columns\" in the lower middle portion of the window to control whether
it contains all the columns in the catalogue or just the columns and
expressions chosen for display.  The latter option allows you to change
the display format and units of an expression.

Once you have chosen a column (or expression), and its name, data type,
current display format and current units are displayed in the column of
entries down the centre of the window, you can enter a new display format
and/or units string into the relevant boxes.  You should note the following
points.


*  The name and data type of the column cannot be changed; they are
   displayed for information.

*  Any new display format must be a valid Fortran 77 format specifier
   for the data type of the column (or expression).

*  The way that the units string is used to control the display of columns
   of angles is described in the CURSA User Manual (SUN/190).


Finally, once you are happy with the new display format and/or units string
click on the \"OK\" button or hit return.

"

     }

    "GetStatsCols_help" {

    .helptext.text.output insert end "
This window allows you to compute statistics for a chosen set of
columns.  This set of columns is unrelated to the set chosen for
display.  The statistics are computed from the rows in the current
selection, though obviously only non-null rows are used in the
calculations.  Statistics can be displayed for columns of any data type,
though for CHARACTER and LOGICAL columns the only quantity which can be
determined is the number of non-null rows in the selection.  The
statistics are displayed in the main display region and optionally may
also be written to a text file.

On the left hand side of the window there is a list of all the columns
in the catalogue.  To choose the columns for which statistics are to be
computed you should proceed as follows.  Click on the required column or
columns, then click on the \"Copy\" button.  The chosen columns will
appear in the list on the right hand side of the window.  Repeat this
procedure until you have chosen all the columns that you require.

Alternatively, to choose all the columns in the catalogue click on the
\"All\" button and all the columns will appear in the list on the right
hand side of the window.

If you make a mistake and choose the wrong column you can remove it
from the list on the right hand side.  Simply click on the column or
columns to be removed (in the right hand list) and then click on
\"Clear\".  If you click on \"Clear\" without first choosing any columns
in the right hand list all the columns will be cleared.

The buttons in the bottom left hand corner of the screen control two
options.  The \"File\" button determines whether the statistics are
written to a text file (suitable, for example, for printing) as well as
being displayed in the main display region.  By default no output file
of statistics is written.  To specify an output file: click on the
\"File\" button and select \"Yes\" from the options.  A separate window
will then appear in which you specify the required file name.  Once this
name has been given it will appear to the right of the \"File\" button.

You can also specify the number of decimal places to which the
statistics are displayed.  Either click on the \"Decimal places\" button
and select the required number from the values shown or simply type the
required number directly into the box to the right of the \"Decimal
places\" button.  Obviously the value supplied cannot be negative and
the maximum allowed is ten.  Note that this option merely controls the
number of decimal places to which the statistics are displayed; they are
always computed and stored internally as DOUBLE PRECISION numbers in
order to maintain the maximum possible accuracy.

Finally, once you are happy with the columns and options chosen then
click on the \"OK\" button or hit return.


Statistics computed
-------------------

The statistics are computed from the rows in the current selection.  Any
null values are ignored.  For each column its name, data type and the
number of non-null rows (in the current selection) are displayed and the
following statistics computed:

   Minimum
   Maximum
   Total range

   First quartile
   Third quartile
   Interquartile range

   Median
   Mean
   Mode (approximate)

   Standard deviation
   Skewness
   Kurtosis


Definitions of the statistics
-----------------------------

Though all the quantities computed are standard statistics there is a
remarkable amount of muddle and confusion over their definitions, with
textbooks giving divers differing formulae.  For completeness, and to
avoid any possible ambiguity, the definitions used in xcatview are given
below.  These formulae follow 'CRC Standard Mathematical Tables',
twenty-fourth edition, edited by W.H. Beyer (1974, CRC Press: Cleveland,
Ohio), except for the definition of skewness which is taken from
'Practical Statistics for Astronomers' by J.V. Wall (1979, Quart. J.
Royal. Astron. Soc. v20, pp138-152).

In the following:

   x(i) is the value of the column for the ith non-null row in the
   current selection,

   the column has n non-null rows in the current selection,

   summations should be understood to range over i = 1 to n.


*  The minimum and maximum are (obviously) simply the smallest and
   largest values in the current selection and the total range is simply
   the positive difference between these two values.

*  If the column is sorted into ascending order then the jth quartile,
   Q(j), is the value of element j(n + 1)/4, where j = 1, 2 or 3.
   Depending on the value n, there may not be an element which
   corresponds exactly to a given quartile.  In this case the value is
   computed by averaging the two nearest elements.

   The interquartile range is simply the positive difference between
   Q(1) and Q(3).

*  The median is simply the second quartile (j = 2).  The mean has its
   usual definition: the sum of all the values divided by the number of
   values.

   The value computed for the mode is not exact.  Indeed it is not
   obvious that the mode is defined for ungrouped data.  Rather, the value
   given is computed from the empirical relation:

                     mode = mean - 3(mean - median)

*  The standard deviation, s, is defined as:

                           -----------------------------
                          /   1      --                 \\
                 s =     / ------- . \\ (x(i) - mean)**2
                        /  (n - 1)   /
                      \\/             --

*  The skewness and kurtosis are defined in terms of moments.  The
   kth moment, u(k), is defined as

                                  --
                      u(k) =  1 . \\ (x(i) - mean)**k
                              -   /
                              n   --


   then               skewness = u(3)**2 / u(2)**3

   and                kurtosis = u(4) / u(2)**2


   The expected values for the skewness and kurtosis are:

   -  skewness = 0 for a symmetrical distribution,

   -  kurtosis = 3 for a normal distribution.

"

     }

    "statistics_file_help" {

    .helptext.text.output insert end "
This window allows you to specify the name of the file to which the
computed statistics will be written.  Simply enter the name of the
required file.  The file name may optionally be preceded by a directory
specification.  If no directory specification is given the file will be
written in the current directory.  The name should correspond to a new
file (that is, a file with this name should not already exist).

The file created is a simple text file, suitable for, for example,
displaying to the terminal screen or printing.
"

     }

    "GetSelection_help" {

    .helptext.text.output insert end "
The purpose of this window is twofold:


*  to display a summary of all the selections which currently exist,

*  to choose a new current selection.


On the left hand side of the window is a box listing all the selections
which currently exist.  The list starts with two lines of titles and
then shows a summary line for each selection.  The line corresponding to
the current selection starts with an asterisk ('*').

If you do not want to change the current selection simply click on the
\"Cancel\" button.

To choose a new selection proceed as follows:


*  click on the required selection in the list of selections (any
   attempts to click on either of the two title lines will be ignored).
   The number of the chosen selection appears in the box immediately
   above the row of buttons on the right hand side of the window,

*  alternatively, you can type the number of the required selection
   directly into this box,

*  in either case, once you are satisfied with the number of the
   selection appearing in the 'Selection:' box either click on the
   \"OK\" button or hit return.
"

     }

    "GetRange_help" {

    .helptext.text.output insert end "
The purpose of this window is to perform a range selection (that is,
select all the rows within a given range for a sorted column).  On the
left hand side of the window is a list of columns on which range
selections may be performed.  Click on the column that you require and
it will appear in the box labelled 'Column:' in the middle of the window.
If you make a mistake then simply try again, clicking on the column that
you require.  Note that you cannot type into the 'Column:' box.

Next type the minimum and maximum values of the required range into the
boxes labelled 'Minimum value:' and 'Maximum value:' respectively.

If the chosen column is recognised by CAT as containing an angle then
the range can be expressed as a sexagesimal number.  For example:

  Minimum value:  +130:00:00

  Maximum value:  +137:30:00

(remember that an unsigned sexagesimal number is interpreted as having
units of hours; a sign must always be included in a number which is to
be interpreted as having units of degrees).

Once you are happy with all the specified values simply click the \"OK\"
button or hit return.
"

     }

    "selections_help" {

    .helptext.text.output insert end "
The entries in this menu are all concerned with creating and examining
'selections'.  In xcatview selections are subsets of the catalogue
being examined which satisfy some criteria.  You can create an arbitrary
number of selections using xcatview.

At any one time xcatview operates on a single selection, called the
'current selection'.  Tables of columns and rows displayed on the screen
or saved as files all correspond to the current selection.  By default
the current selection is the most recent one created.  If no selections
have been created then the current selection is the entire catalogue.

To make any of the existing selections the current selection use the
'Choose an existing selection' menu option.

To examine a summary of the all the existing selections you should also
use this menu option.


Creating a new selection
------------------------

New selections are always generated from the current selection.

Two menu options are available for creating selections:


*  'Create a new selection'

*  'Create a new range selection'


They are used under different circumstances.


*  'Create a new selection' is a flexible way to generate selections
   which satisfy a wide range of different sorts of criteria.  You
   supply an algebraic expression, involving the names of columns,
   parameters, numeric constants etc. which defines the selection.  The
   rows of the current selection which satisfy this criterion become
   the selection.  This sort of selection is always available for all
   catalogues.  It may be slow for large catalogues (say more than
   15,000 rows) because every row in the catalogue has to be examined.

*  'Create a new range selection' is a rather more limited sort of
   selection.  It allows you to select the rows for which the values of
   a given column lie within a specified range.  You supply the name of
   the column and the minimum and maximum values of the range.  The
   advantage of range selections is that they are fast; the required
   rows are selected essentially immediately, irrespective of the size
   of the catalogue.  The disadvantage is that they are only available
   on columns which are sorted into ascending or descending order (or,
   more precisely, columns which are known to xcatview to be so
   sorted).

   If you click on the 'Create a new range selection' menu option the
   ensuing window only allows you to make range selections on columns
   which are sorted; it is not possible to confuse xcatview by making a
   range selection on an unsorted column.  Not all catalogues have
   columns suitable for use in a range selection (in fact, probably most
   do not).  If you try to make a range selection on a catalogue with no
   suitable columns, no window will appear, a warning message will be
   displayed in the error and warning region and you will be left at the
   main display window.


If you are intending to perform a series of selections on a large
catalogue with some columns suitable for range selection then it is
usually advisable to perform any range selections first, if possible,
in order to reduce the size of the current selection and hence speed up
any subsequent selections.
"

     }

    "GetScatter_help" {

    .helptext.text.output insert end "
This window allows you to plot a simple scatter-plot from two columns
in the current selection.

You define the plot by specifying values for the column of items down
the middle of the window.

The first item at the top of the column is an optional title for the
plot; simply enter any title required into the appropriate box.  If
you omit the title then the criterion used to specify the current
selection will be used as the title.

The next two items are the names of the columns to be plotted as the
X and Y axes of the plot.  On the left hand side of the window is a
list of columns.  To specify the X axis click on the required column
and then click on the \"X axis:\" button.  The name of the selected
column will appear in the box to the right of the button.
Alternatively, you can simply type the column name into the box.
Repeat the procedure for the Y axis.

The next three buttons control aspects of the appearance of the plot.
The purpose of each button is more-or-less self-explanatory, but briefly:

Plotting Symbol:  the symbol to be used to plot each data point,

Symbol Colour:  the colour in which each symbol is drawn,

Auto-scale:  is the plot to be auto-scaled?  If the option is set to
   \"Yes\" then the range of both axes will be determined automatically
   from the data values.  If it is set to \"No\" then you will supply
   your own values for the ranges.  If in doubt you should set
   Auto-scale to \"Yes\".

The text to the right of each button shows the option which is currently
in effect.  To change any of the items click on the appropriate button.
A list of the options for the item will appear; click on the one that
you require.  The current option shown to the right of the button will
be updated to reflect your choice.

The final four items are the minimum and maximum  X and Y extents of the
plot.  These values are only required if Auto-scale is set to \"No\".
You should simply enter the required values into the boxes.  Note that
if Auto-scale is set to \"Yes\" you cannot enter values in the boxes.

When you are happy with the settings of the items then simply click on
the \"OK\" button or hit return.
"

     }

    "GetScatterOver_help" {

    .helptext.text.output insert end "
This window allows you to overlay a scatter-plot on top of an existing
scatter-plot.  Clearly a scatter-plot must already have been drawn,
in order that the new plot can overlay it.  The overlay will have the
same axes and ranges as the original plot, but will show a different
set of points.  Typically, the original plot and overlay will show
different selections from the same catalogue.

On the left hand side of the window is a box listing all the selections
which currently exist.  The list starts with two lines of titles and
then shows a summary line for each selection.  The line corresponding to
the current selection starts with an asterisk ('*').

You need to specify the selection which is to be plotted as an overlay.
Click on the required selection in the list of selections (any attempts
to click on either of the two title lines will be ignored).  The number
of the chosen selection appears in the box labelled \"Selection:\",
which is at the top of the column of three items to the right of the list
of selections.  Alternatively, you can simply type the number of the
required selection into this box.

The two buttons beneath the chosen selection control the appearance
of the plotted symbols.  The purpose of each button is more-or-less
self-explanatory, but briefly:

Plotting Symbol:  the symbol to be used to plot each data point,

Symbol Colour:  the colour in which each symbol is drawn.

The text to the right of each button shows the option which is currently
in effect.  To change either of the items click on the appropriate button.
A list of the options for the item will appear; click on the one that
you require.  The current option shown to the right of the button will
be updated to reflect your choice.

When you are happy with the settings of the items then simply click on
the \"OK\" button or hit return.
"

     }

    "GetHist_help" {

    .helptext.text.output insert end "
This window allows you to plot a simple histogram from a column in
the current selection.

You define the histogram by specifying values for the column of items
down the middle of the window.

The first item at the top of the column is an optional title for the
histogram; simply enter any title required into the appropriate box.
If you omit the title then the criterion used to specify the current
selection will be used as the title.

The next item is the name of the column to be histogrammed.  On the
left hand side of the window is a list of columns.  Click on the
required column and it will appear in the box to the right of the
label \"X axis:\".

The next four buttons control aspects of the histogram.  The purpose of
each button is more-or-less self-explanatory, but briefly:

Bin details:  details of how individual bins in the histogram are to be
   defined.  The options are:

   Bin width:  you specify the width of each bin in the histogram,

   Number of bins: you specify the total number of bins in the histogram.

   In both cases, when you choose the option an additional window
   will appear, into which you enter either the bin width or number of
   bins required, as appropriate.

Normalise:  is the histogram to be normalised?  If \"No\" is selected
   then the histogram simply shows the number of data points in each
   bin.  If \"Yes\" is specified then the number of data points in each
   bin is divided by the total number of points.

Line Colour:  the colour in which the histogram is drawn,

Auto-scale:  is the histogram to be auto-scaled?  If the option is set to
   \"Yes\" then the range of the X axis will be determined automatically
   from the data values.  If it is set to \"No\" then you will supply
   your own values for the range.  If in doubt you should set
   Auto-scale to \"Yes\".

The text to the right of each button shows the option which is currently
in effect.  To change any of the items click on the appropriate button.
A list of the options for the item will appear; click on the one that
you require.  The current option shown to the right of the button will
be updated to reflect your choice.

The final two items are the minimum and maximum X extent of the
histogram.  These values are only required if Auto-scale is set to \"No\".
You should simply enter the required values into the boxes.  Note that
if Auto-scale is set to \"Yes\" you cannot enter values in the boxes.

When you are happy with the settings of the items then simply click on
the \"OK\" button or hit return.
"

     }

    "GetHistOver_help" {

    .helptext.text.output insert end "
This window allows you to overlay a histogram on top of an existing
histogram.  Clearly a histogram must already have been drawn, in order
that the new one can overlay it.  The overlay will have the same axes
and range as the original plot, but will calculated from a different set
of points.  Typically, the original histogram and overlay will show
different selections from the same catalogue.

On the left hand side of the window is a box listing all the selections
which currently exist.  The list starts with two lines of titles and
then shows a summary line for each selection.  The line corresponding to
the current selection starts with an asterisk ('*').

You need to specify the selection which is to be plotted as an overlay.
Click on the required selection in the list of selections (any attempts
to click on either of the two title lines will be ignored).  The number
of the chosen selection appears in the box labelled \"Selection:\",
which is at the top of the column of two items to the right of the list
of selections.  Alternatively, you can simply type the number of the
required selection into this box.

The button beneath the chosen selection controls the colour used to
draw the overlay histogram.  The text to the right of the button shows
the option which is currently in effect.  To select the colour click on
the button.  A list of colours will appear; click on the one that you
require.  The current colour shown to the right of the button will be
updated to reflect your choice.

When you are happy with the settings of the items then simply click on
the \"OK\" button or hit return.

Note: if you want the bins in an overlay histogram to correspond exactly
to those in the histogram which which it is to overlay then Auto-scale
should be set to \"No\" when the original histogram is plotted.
"

     }
    "GetHistBinWidth_help" {

    .helptext.text.output insert end "
Enter the width required for each histogram bin.
"

     }
    "GetHistBinNumber_help" {

    .helptext.text.output insert end "
Enter the total number of bins that the histogram is to contain.
"

     }

    "GetConfig_help" {

    .helptext.text.output insert end "
This window allows you to control the 'Echo Command', 'Reformat Angle
Units' and 'Quiet Mode' configuration options for xcatview.  The first two
options, at least, are fairly obscure and you are unlikely to need to
change them; if in doubt stick with the defaults.

The 'Echo Command' option specifies whether or not commands sent to the
catview ADAM A-task are to be echoed in the error messages and information
region.  Seeing the commands echoed in this way is more likely to be
useful to a programmer working on xcatview than to a user.  If you do
not understand this explanation then you probably want to leave this item
set to 'No'; turning it on will just cause lines of apparent gibberish to
appear in the error messages and information region.

The 'Reformat Angle Units' option controls the appearance of the units
listed for each column at the top of the main display region.  The
option only affects the units, not the values tabulated, and only has
any effect for columns recognised as angles.  If the option is in effect
then units are shown as either the sexagesimal format specifier for the
column or 'Radians', depending on whether or not angles are currently
being output as sexagesimal values or radians.  If the option is not in
effect then the full units attribute of the column is shown unaltered.
Usually you would only need to set the option to 'No' if you were
investigating some oddity in the units attribute of a column of angles.

The 'Quiet Mode' controls the extent to which informational and warning
messages are issued.  In the normal 'verbose' mode all these messages
are issued, but in the 'quiet' mode some of them are suppressed.  The
quiet mode affects only informational and warning messages, not error
messages.  You should use the quiet mode with caution; it is usually better
to see the informational and warning messages.  Finally, if you change the
quiet mode the new setting will remain in effect not just for the current
session using xcatview but also for any other CURSA applications which you
run subsequently, until you explicitly reset it.

The options are controlled by the three buttons on the left hand side of
the window.  The text to the right of each button shows the option which
is currently in effect.  To change an option click on the appropriate
button.  A list of alternatives will appear; click on the one that you
require.  The current option shown to the right of the button will be
updated to reflect your choice.

When you are happy with the setting of the options simply click on the
\"OK\" button or hit return.

"

     }
     }

#
#  Withdraw the window, then update all the geometry information
#  so we know how big it wants to be, then centre the window in
#  parent and de-iconify it.

     wm withdraw .helptext
     update idletasks
     set x [expr [winfo width .]/2 - [winfo reqwidth .helptext]/2 \
       + [winfo x .]]
     set y [expr [winfo height .] - [winfo reqheight .helptext]/2 \
       + [winfo y .]]
     wm geom .helptext +$x+$y
     wm deiconify .helptext

#
#   Set a grab and claim the focus.

     set oldFocus [focus]
     grab .helptext
     focus .helptext

#
#  Wait for the "OK" button to be pressed.

     global HTButton
     tkwait variable HTButton

#
#   Destroy the dialogue box and restore the focus.
     destroy  .helptext
     focus    $oldFocus

}
