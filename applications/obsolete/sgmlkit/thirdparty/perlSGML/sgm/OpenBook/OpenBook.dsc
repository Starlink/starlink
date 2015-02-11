<!-- ############### Global Element Definitions ############### -->
<!>
<?DTD2HTML d3,d4,d5,d6,d7,d8,d9,d10 >
<strong>d3-d10</strong>
<p>
Start hierarchical level subsections within a section
(<a href="d2.html">D2</a>).
Each subsection:
<ul>
<li>becomes an entry in the table of contents</li>
<li>in online documents, starts a new topic</li>
</ul>
<p>
Subsections may be nested to eight levels.  D3 subsections must
occur after a D2 element, and each subsection must occur within a
successively higher level section. For example, if you use a D4, it
must be preceded by at least one D3 and D2.
<p>
A
<a href="head.html">HEAD</a>
element is required for each subsection, but text is optional in
all subsections.
<p>
<!>
<!-- ############### Global Attribute Definitions ############### -->
<!>
<?DTD2HTML *cdrom >
Include, or exclude, the element in a CD-ROM volume.
<p>
<?DTD2HTML *hardcopy >
Include, or exclude, the element in a printed volume.
<p>
<?DTD2HTML *help >
Include, or exclude, the element in an on-line help volume.
<p>
<?DTD2HTML *id >
Optional parameter whose value is used elsewhere in the document to
cross-reference the element (see
<a href="xref.html">XREF</a> and
<a href="link.html">LINK</a>).
<p>
<?DTD2HTML *locale >
Specifies the location for which the element is printed. This 
conditional can be used to combine two localized versions of a document 
(for example, an English and French version) in one document.
<p>
<?DTD2HTML *memo >
Identifies if the element is an author's comment or question.
<p>
<?DTD2HTML *revlevel >
Specifies in which revision of the document an element is
printed or displayed.
<p>
<!>
<!-- ############### Home Page Text ############### -->
<!>
<?DTD2HTML -home- >
<address>
<a href="Copy.html">Copyright information</a>
</address>
<p>
The element and attribute descriptions have been extracted/adapted 
to HTML from the <em>FrameBuilder</em> online
<em>OpenBook Reference Manual</em>.
<p>
<address>Earl Hood, ehood@convex.com</address>
<P>
<!>
<!-- ###############  Element/Attribute Descriptions ############### -->
<!>
<?DTD2HTML a >
<P>
<?DTD2HTML a* >
<P>
<?DTD2HTML a*valign >
<P>
<?DTD2HTML abbrev >
Specifies an abbreviated version of a division
<a href="head.html">HEAD</a>, or appendix HEAD.
<p>
When used with a chapter
(<a href="d1.html">D1</a>) or
<a href="appendix.html">APPENDIX</a>
head, the abbreviation is used in the page footer.  When used with a
section head (<a href="d2.html">D2</a>) or section subhead (D3-D10),
the abbreviation is used in cross references.
<P>
<?DTD2HTML abbrev* >
<P>
<?DTD2HTML abbrev*cdrom >
<P>
<?DTD2HTML abbrev*hardcopy >
<P>
<?DTD2HTML abbrev*help >
<P>
<?DTD2HTML abbrev*id >
<P>
<?DTD2HTML abbrev*locale >
<P>
<?DTD2HTML abbrev*memo >
<P>
<?DTD2HTML abbrev*revlevel >
<P>
<?DTD2HTML abstract >
Provides a brief description of the document contents.  The abstract element
is an optional element in the
<a href="metainfo.html">METAINFO</a> element. It follows the optional
<a href="map.html">MAP</a>
element and precedes the optional
<a href="preface.html">PREFACE</a>
element.
<P>
<?DTD2HTML abstract* >
<P>
<?DTD2HTML abstract*cdrom >
<P>
<?DTD2HTML abstract*hardcopy >
<P>
<?DTD2HTML abstract*help >
<P>
<?DTD2HTML abstract*id >
<P>
<?DTD2HTML abstract*locale >
<P>
<?DTD2HTML abstract*memo >
<P>
<?DTD2HTML abstract*revlevel >
<P>
<?DTD2HTML ac >
Identifies acronyms, mnemonics, or product names.
<P>
<?DTD2HTML ach >
<P>
<?DTD2HTML ach* >
<P>
<?DTD2HTML ach*atom >
<P>
<?DTD2HTML acro >
<P>
<?DTD2HTML annotation >
Provides an explanatory note or comment to an
<a href="ex.html">EX</a>
element.
You may specify the width of annotations using the
<a href="ex.attr.html">width attribute</a>
of the
<a href="ex.html">EX</a>
element.
<P>
<?DTD2HTML annotext >
Contains text paragraphs for
<a href="note.html">NOTE</a>s,
<a href="warning.html">WARNING</a>s, and
<a href="caution.html">CAUTION</a>s.
<P>
<?DTD2HTML annotext* >
<P>
<?DTD2HTML annotext*cdrom >
<P>
<?DTD2HTML annotext*hardcopy >
<P>
<?DTD2HTML annotext*help >
<P>
<?DTD2HTML annotext*id >
<P>
<?DTD2HTML annotext*locale >
<P>
<?DTD2HTML annotext*memo >
<P>
<?DTD2HTML annotext*revlevel >
<P>
<?DTD2HTML appendix >
Starts an appendix.
<P>
<?DTD2HTML appendix* >
<P>
<?DTD2HTML appendix*cdrom >
<P>
<?DTD2HTML appendix*hardcopy >
<P>
<?DTD2HTML appendix*help >
<P>
<?DTD2HTML appendix*id >
<P>
<?DTD2HTML appendix*letter >
OpenBook applications label appendixes sequentially with letters. Specify
a letter to override the one assigned. You may follow the letter with
a number; for example, "A1".
<P>
<?DTD2HTML appendix*locale >
<P>
<?DTD2HTML appendix*memo >
<P>
<?DTD2HTML appendix*revlevel >
<P>
<?DTD2HTML ar >
<P>
<?DTD2HTML ar* >
<P>
<?DTD2HTML ar*ca >
<P>
<?DTD2HTML ar*cs >
<P>
<?DTD2HTML ar*rs >
<P>
<?DTD2HTML arc >
<P>
<?DTD2HTML arc* >
<P>
<?DTD2HTML arc*align >
<P>
<?DTD2HTML arr >
<P>
<?DTD2HTML art >
Reserves space for paste-in artwork and identifies production notes.
A height measurement is required.
<p>
The art element must be used inside the
<a href="frame.html">FRAME</a>
element.
<P>
<?DTD2HTML art* >
<P>
<?DTD2HTML art*bottommargin >
Specifies the amount of space reserved below the artwork.
<P>
<?DTD2HTML art*cdrom >
<P>
<?DTD2HTML art*hardcopy >
<P>
<?DTD2HTML art*height >
Specifies the height of the space reserved.
<P>
<?DTD2HTML art*help >
<P>
<?DTD2HTML art*id >
<P>
<?DTD2HTML art*leftmargin >
Specifies the amount of space reserved to the left of the artwork.
<P>
<?DTD2HTML art*locale >
<P>
<?DTD2HTML art*memo >
<P>
<?DTD2HTML art*placement >
Specifies the placement of the reserved space.
<P>
<?DTD2HTML art*revlevel >
<P>
<?DTD2HTML art*rightmargin >
Specifies the amount of space reserved to the right of the artwork.
<P>
<?DTD2HTML art*topmargin >
Specifies the amount of space reserved above the artwork.
<P>
<?DTD2HTML art*width >
Specifies the width of the space reserved.
<P>
<?DTD2HTML b >
<P>
<?DTD2HTML bg >
<P>
<?DTD2HTML blkbd >
<P>
<?DTD2HTML book >
Identifies the title of a book or manual within the document.
<P>
<?DTD2HTML book* >
<P>
<?DTD2HTML book*id >
<P>
<?DTD2HTML bridgehead >
The bridgehead element is like the
<a href="head.html">HEAD</a>
element, with two exceptions:
<ul>
<li>It is not included in the table of contents.</li>
<li>In online documents, it does not start a new topic; it appears in
the same window or "page" as the preceding text.
</ul>
<P>
<?DTD2HTML bridgehead* >
<P>
<?DTD2HTML bridgehead*cdrom >
<P>
<?DTD2HTML bridgehead*hardcopy >
<P>
<?DTD2HTML bridgehead*help >
<P>
<?DTD2HTML bridgehead*id >
<P>
<?DTD2HTML bridgehead*locale >
<P>
<?DTD2HTML bridgehead*memo >
<P>
<?DTD2HTML bridgehead*revlevel >
<P>
<?DTD2HTML caption >
Identifies caption text for
<a href="frame.html">FRAME</a>
elements.
<P>
<?DTD2HTML caption* >
<P>
<?DTD2HTML caption*cdrom >
<P>
<?DTD2HTML caption*hardcopy >
<P>
<?DTD2HTML caption*help >
<P>
<?DTD2HTML caption*id >
<P>
<?DTD2HTML caption*locale >
<P>
<?DTD2HTML caption*memo >
<P>
<?DTD2HTML caption*revlevel >
<P>
<?DTD2HTML cases >
<P>
<?DTD2HTML caution >
Alerts the reader to a situation that could cause damage or destruction to
the product.
<p>
You can specify a different head with the
<a href="head.html">HEAD</a>
element. If you do not,
"Caution" is printed as the head. To print a hand icon, specify the
"hands" attribute of the
<a href="openbook.html">OPENBOOK</a>
element.
<P>
<?DTD2HTML colspec >
<P>
<?DTD2HTML colspec* >
<P>
<?DTD2HTML colspec*align >
<P>
<?DTD2HTML colspec*char >
<P>
<?DTD2HTML colspec*charoff >
<P>
<?DTD2HTML colspec*colname >
<P>
<?DTD2HTML colspec*colnum >
<P>
<?DTD2HTML colspec*colsep >
<P>
<?DTD2HTML colspec*colwidth >
<P>
<?DTD2HTML colspec*rowsep >
<P>
<?DTD2HTML computer >
Simulates computer screen text.
<P>
<?DTD2HTML conventions >
Starts the section that describes typographical conventions used in a
document. An optional element in
<a href="metainfo.html">METAINFO</a>,
conventions follows the optional
<a href="preface.html">PREFACE</a>
element and precedes the optional
<a href="miscmeta.html">MISCMETA</a>
element.
<P>
<?DTD2HTML conventions* >
<P>
<?DTD2HTML conventions*cdrom >
<P>
<?DTD2HTML conventions*hardcopy >
<P>
<?DTD2HTML conventions*help >
<P>
<?DTD2HTML conventions*id >
<P>
<?DTD2HTML conventions*locale >
<P>
<?DTD2HTML conventions*memo >
<P>
<?DTD2HTML conventions*revlevel >
<P>
<?DTD2HTML copyright >
Identifies text for the copyright notice. 
<p>
The copyright notice is a required element of the 
<a href="idsection.html">IDSECTION</a> 
within the
<a href="metainfo.html">METAINFO</a>.
The copyright element follows the optional
<a href="edition.html">EDITION</a>
element and precedes the
<a href="source.html">SOURCE</a>
element.
<P>
<?DTD2HTML copyright* >
<P>
<?DTD2HTML copyright*id >
<P>
<?DTD2HTML country >
Specifies the name of the country where a manual is printed. An
optional element in the 
<a href="idsection.html">IDSECTION</a>, 
country follows the optional 
<a href="ficheno.html">FICHENO</a>
element and precedes the optional 
<a href="printdate.html">PRINTDATE</a> 
element.
<P>
<?DTD2HTML country* >
<P>
<?DTD2HTML country*id >
<P>
<?DTD2HTML cp >
<P>
<?DTD2HTML cp* >
<P>
<?DTD2HTML cp*post >
<P>
<?DTD2HTML cp*style >
<P>
<?DTD2HTML cursor >
Indicates the position of the cursor in examples. 
<p>
The cursor element is used with the
<a href="user.html">USER</a> 
element in an 
<a href="ex.html">EX</a> 
element to indicate the position of the cursor on the screen.
<P>
<?DTD2HTML d1 >
Starts a chapter (also referred to as a division).
<P>
<?DTD2HTML d1* >
<P>
<?DTD2HTML d1*cdrom >
<P>
<?DTD2HTML d1*hardcopy >
<P>
<?DTD2HTML d1*help >
<P>
<?DTD2HTML d1*id >
Optional attribute whose value is used elsewhere in the document 
to cross-reference the chapter (see the
<a href="xref.html">XREF</a>
element).
<P>
<?DTD2HTML d1*insertnum >
Optional attribute used to override the number assigned by Open
Book application. Normally, chapters are numbered sequentially. The value 
must begin with a digit, but can contain alphabetic characters (as in 5a).
<P>
<?DTD2HTML d1*locale >
<P>
<?DTD2HTML d1*memo >
<P>
<?DTD2HTML d1*resetnum >
Resets the counter. All subsequent numbers used after this point are 
incremented by 1 from the value of "resetnum".
<P>
<?DTD2HTML d1*revlevel >
<P>
<?DTD2HTML d10 >
<P>
<?DTD2HTML d10* >
<P>
<?DTD2HTML d10*cdrom >
<P>
<?DTD2HTML d10*hardcopy >
<P>
<?DTD2HTML d10*help >
<P>
<?DTD2HTML d10*id >
<P>
<?DTD2HTML d10*locale >
<P>
<?DTD2HTML d10*memo >
<P>
<?DTD2HTML d10*revlevel >
<P>
<?DTD2HTML d2 >
Starts a section within a chapter
(<a href="d1.html">D1</a>)
or an
(<a href="appendix.html">APPENDIX</a>).
Each section:
<ul>
<li>becomes an entry in the table of contents</li>
<li>in online documents, starts a new topic</li>
</ul>
<p>
A
<a href="head.html">HEAD</a>
element is required for each section, but text is optional in
all sections.
<P>
<?DTD2HTML d2* >
<P>
<?DTD2HTML d2*cdrom >
<P>
<?DTD2HTML d2*hardcopy >
<P>
<?DTD2HTML d2*help >
<P>
<?DTD2HTML d2*id >
<P>
<?DTD2HTML d2*locale >
<P>
<?DTD2HTML d2*memo >
<P>
<?DTD2HTML d2*revlevel >
<P>
<?DTD2HTML d3 >
<P>
<?DTD2HTML d3* >
<P>
<?DTD2HTML d3*cdrom >
<P>
<?DTD2HTML d3*hardcopy >
<P>
<?DTD2HTML d3*help >
<P>
<?DTD2HTML d3*id >
<P>
<?DTD2HTML d3*locale >
<P>
<?DTD2HTML d3*memo >
<P>
<?DTD2HTML d3*revlevel >
<P>
<?DTD2HTML d4 >
<P>
<?DTD2HTML d4* >
<P>
<?DTD2HTML d4*cdrom >
<P>
<?DTD2HTML d4*hardcopy >
<P>
<?DTD2HTML d4*help >
<P>
<?DTD2HTML d4*id >
<P>
<?DTD2HTML d4*locale >
<P>
<?DTD2HTML d4*memo >
<P>
<?DTD2HTML d4*revlevel >
<P>
<?DTD2HTML d5 >
<P>
<?DTD2HTML d5* >
<P>
<?DTD2HTML d5*cdrom >
<P>
<?DTD2HTML d5*hardcopy >
<P>
<?DTD2HTML d5*help >
<P>
<?DTD2HTML d5*id >
<P>
<?DTD2HTML d5*locale >
<P>
<?DTD2HTML d5*memo >
<P>
<?DTD2HTML d5*revlevel >
<P>
<?DTD2HTML d6 >
<P>
<?DTD2HTML d6* >
<P>
<?DTD2HTML d6*cdrom >
<P>
<?DTD2HTML d6*hardcopy >
<P>
<?DTD2HTML d6*help >
<P>
<?DTD2HTML d6*id >
<P>
<?DTD2HTML d6*locale >
<P>
<?DTD2HTML d6*memo >
<P>
<?DTD2HTML d6*revlevel >
<P>
<?DTD2HTML d7 >
<P>
<?DTD2HTML d7* >
<P>
<?DTD2HTML d7*cdrom >
<P>
<?DTD2HTML d7*hardcopy >
<P>
<?DTD2HTML d7*help >
<P>
<?DTD2HTML d7*id >
<P>
<?DTD2HTML d7*locale >
<P>
<?DTD2HTML d7*memo >
<P>
<?DTD2HTML d7*revlevel >
<P>
<?DTD2HTML d8 >
<P>
<?DTD2HTML d8* >
<P>
<?DTD2HTML d8*cdrom >
<P>
<?DTD2HTML d8*hardcopy >
<P>
<?DTD2HTML d8*help >
<P>
<?DTD2HTML d8*id >
<P>
<?DTD2HTML d8*locale >
<P>
<?DTD2HTML d8*memo >
<P>
<?DTD2HTML d8*revlevel >
<P>
<?DTD2HTML d9 >
<P>
<?DTD2HTML d9* >
<P>
<?DTD2HTML d9*cdrom >
<P>
<?DTD2HTML d9*hardcopy >
<P>
<?DTD2HTML d9*help >
<P>
<?DTD2HTML d9*id >
<P>
<?DTD2HTML d9*locale >
<P>
<?DTD2HTML d9*memo >
<P>
<?DTD2HTML d9*revlevel >
<P>
<?DTD2HTML de >
<P>
<?DTD2HTML definition >
Specifies the definition text for a term in the
<a href="glossary.html">GLOSSARY</a>.
<P>
<?DTD2HTML definition* >
<P>
<?DTD2HTML definition*cdrom >
<P>
<?DTD2HTML definition*hardcopy >
<P>
<?DTD2HTML definition*help >
<P>
<?DTD2HTML definition*id >
<P>
<?DTD2HTML definition*locale >
<P>
<?DTD2HTML definition*memo >
<P>
<?DTD2HTML definition*revlevel >
<P>
<?DTD2HTML display >
Simulates readouts on instrument panels.
<p>
Display text is similar to
<a href="computer.html">COMPUTER</a>
text. Use it to produce monospaced, bit-mapped text within paragraph text.
<P>
<?DTD2HTML dterm >
Identifies a term being defined in the
<a href="glossary.html">GLOSSARY</a>.
<P>
<?DTD2HTML dterm* >
<P>
<?DTD2HTML dterm*id >
<P>
<?DTD2HTML dy >
<P>
<?DTD2HTML edition >
Specifies the edition number of the manual. This element is part of the
<a href="idsection.html">IDSECTION</a>.
It follows the optional
<a href="mfgno.html">MFGNO</a>
and precedes the required
<a href="copyright.html">COPYRIGHT</a>.
<P>
<?DTD2HTML edition* >
<P>
<?DTD2HTML edition*id >
<P>
<?DTD2HTML emph >
Specifies emphasized text.
<P>
<?DTD2HTML emph* >
<P>
<?DTD2HTML emph*id >
<P>
<?DTD2HTML entry >
Identifies the contents text in a <a href="table.html">TABLE</a> cell.
<P>
<?DTD2HTML entry* >
<P>
<?DTD2HTML entry*align >
<P>
<?DTD2HTML entry*char >
<P>
<?DTD2HTML entry*charoff >
<P>
<?DTD2HTML entry*colname >
<P>
<?DTD2HTML entry*colsep >
<P>
<?DTD2HTML entry*morerows >
<P>
<?DTD2HTML entry*nameend >
<P>
<?DTD2HTML entry*namest >
<P>
<?DTD2HTML entry*rotate >
<P>
<?DTD2HTML entry*rowsep >
<P>
<?DTD2HTML entry*spanname >
<P>
<?DTD2HTML entry*valign >
<P>
<?DTD2HTML eqaln >
<P>
<?DTD2HTML eqline >
<P>
<?DTD2HTML eqn >
Identifies an equation.
<P>
<?DTD2HTML eqn* >
<P>
<?DTD2HTML eqn*bitmap >
Renders the equation in bitmap form.
<P>
<?DTD2HTML eqn*id >
The identifying name you give to an equation you want to cross-
reference (see the <a href="xref.html">XREF</a> element).
<P>
<?DTD2HTML eqn*lindent >
<P>
<?DTD2HTML eqn*postsp >
<P>
<?DTD2HTML eqn*presp >
<P>
<?DTD2HTML eqn*quadding >
<P>
<?DTD2HTML eqn*rindent >
<P>
<?DTD2HTML ex >
Presents text with spacing and line breaks preserved. Each line in an
example is contained by the
<a href="exampleseg.html">EXAMPLESEG</a>
element.
<p>
Add comments to examples with the
<a href="annotation.html">ANNOTATION</a> element.
<P>
<?DTD2HTML ex* >
<P>
<?DTD2HTML ex*lines >
Number, or not number, the lines in an example.
<P>
<?DTD2HTML ex*notes >
Place the
<a href="annotation.html">ANNOTATION</a>
to the right (SIDE) of, or under (STACK), the
example text, on the same line as the first line of the example.
<P>
<?DTD2HTML ex*textsize >
Specifies the size of the computer font used.
<P>
<?DTD2HTML ex*type >
Print the text in monospaced computer type (COMPUTER), or in bit-mapped
fonts (DISPLAY).
<P>
<?DTD2HTML ex*width >
Specifies the width of the
<a href="annotation.html">ANNOTATION</a>.
<P>
<?DTD2HTML exampleseg >
Contains the text in an
<a href="ex.html">EX</a>
or
<a href="vex.html">VEX</a>
element.
<P>
<?DTD2HTML exampleseg* >
<P>
<?DTD2HTML exampleseg*id >
<P>
<?DTD2HTML explain >
Identifies the explanation text that optionally accompanies a
<a href="msg.html">MSG</a>
element.
<P>
<?DTD2HTML explain* >
<P>
<?DTD2HTML explain*cdrom >
<P>
<?DTD2HTML explain*hardcopy >
<P>
<?DTD2HTML explain*help >
<P>
<?DTD2HTML explain*id >
<P>
<?DTD2HTML explain*locale >
<P>
<?DTD2HTML explain*memo >
<P>
<?DTD2HTML explain*revlevel >
<P>
<?DTD2HTML externalobjs >
Provides a place within a document to describe all external information
objects that are referenced from the document.
<p>
This subsection contains an idsection-like element that identifies the
external document and elements for locating anchor points within the
target document. This arrangement allows for easy creation of a
cross-book data structure, as well as easy algorithm devel opment that
allows publishing and delivery systems to resolve the references.
<p>
The externalobjs section is not for objects that are part of the
current document, either directly or by entity reference. Note that one
primary difference between external objects and entities is their
transport model:
<p>
<ul>
<li>Entities are included with the document when the document is interchanged.
</li>
<li>External objects are not included when the document is interchanged.
</li>
<li>External objects are only intended for use by an on-line delivery
system; they are not intended for use in printed documents.
</li>
</ul>
<p>
External objects are identified by the
<a href="xobjinfo.html">XOBJINFO</a>
element. This group contains the same elements as a document's
<a href="idsection.html">IDSECTION</a>.
<a href="objid.html">OBJID</a>
is a new application-specific element used to specify a specific information
object in the format required by an application. The content of this
element is intended to be supplied by either an editor or publishing
application.
<P>
<?DTD2HTML f >
<P>
<?DTD2HTML fd >
<P>
<?DTD2HTML fen >
<P>
<?DTD2HTML fen* >
<P>
<?DTD2HTML fen*lp >
<P>
<?DTD2HTML fen*style >
<P>
<?DTD2HTML fi >
<P>
<?DTD2HTML ficheno >
Identifies the microfiche number of the documentation. FICHENO is an
optional element in the
<a href="idsection.html">IDSECTION</a>;
it follows the optional
<a href="partno.html">PARTNO</a>
element and precedes the optional
<a href="country.html">COUNTRY</a>
element.
<P>
<?DTD2HTML ficheno* >
<P>
<?DTD2HTML ficheno*id >
<P>
<?DTD2HTML figtext >
Identifies text entered in the source file that is to be printed within a
<a href="frame.html">FRAME</a>.
<P>
<?DTD2HTML figtext* >
<P>
<?DTD2HTML figtext*bottommargin >
Specifies the amount of space reserved below the figtext.
<P>
<?DTD2HTML figtext*cdrom >
<P>
<?DTD2HTML figtext*hardcopy >
<P>
<?DTD2HTML figtext*help >
<P>
<?DTD2HTML figtext*id >
<P>
<?DTD2HTML figtext*leftmargin >
Specifies the amount of space reserved to the left of the figtext.
<P>
<?DTD2HTML figtext*locale >
<P>
<?DTD2HTML figtext*memo >
<P>
<?DTD2HTML figtext*placement >
Specifies the placement of the figtext.
<P>
<?DTD2HTML figtext*revlevel >
<P>
<?DTD2HTML figtext*rightmargin >
Specifies the amount of space reserved to the right of the figtext.
<P>
<?DTD2HTML figtext*textsize >
Specifies the size of the computer font used.
<P>
<?DTD2HTML figtext*topmargin >
Specifies the amount of space reserved above the figtext.
<P>
<?DTD2HTML figtoc >
Lists all the figures within a document.
<P>
<?DTD2HTML figtocentry >
<P>
<?DTD2HTML fillin >
Draws a line under a blank space where you want the user to fill in the
blank. Can be used in
<a href="p.html">P</a>
elements and in
<a href="ex.html">EX</a>
elements.
<P>
<?DTD2HTML fillin* >
<P>
<?DTD2HTML fillin*length >
Defines a specific length for the underline.
<P>
<?DTD2HTML fillin*position >
Specifies that the underline should extend to the end of the line. 
<P>
<?DTD2HTML fl >
<P>
<?DTD2HTML footer >
Places a footer (text) at the bottom of each page in a printed document.
<P>
<?DTD2HTML footer* >
<P>
<?DTD2HTML footer*cdrom >
<P>
<?DTD2HTML footer*hardcopy >
<P>
<?DTD2HTML footer*help >
<P>
<?DTD2HTML footer*id >
<P>
<?DTD2HTML footer*locale >
<P>
<?DTD2HTML footer*memo >
<P>
<?DTD2HTML footer*revlevel >
<P>
<?DTD2HTML footnote >
<P>
<?DTD2HTML footnote* >
Places a footnote at the bottom of the current page.
<P>
<?DTD2HTML footnote*cdrom >
<P>
<?DTD2HTML footnote*hardcopy >
<P>
<?DTD2HTML footnote*help >
<P>
<?DTD2HTML footnote*id >
<P>
<?DTD2HTML footnote*locale >
<P>
<?DTD2HTML footnote*memo >
<P>
<?DTD2HTML footnote*revlevel >
<P>
<?DTD2HTML fr >
<P>
<?DTD2HTML fr* >
<P>
<?DTD2HTML fr*align >
<P>
<?DTD2HTML fr*shape >
<P>
<?DTD2HTML fr*style >
<P>
<?DTD2HTML frame >
Integrates scanned images, captured screens, drawings, computer
listings, or source text into a document.
<P>
<?DTD2HTML frame* >
<P>
<?DTD2HTML frame*border >
Type of border around the frame.
<P>
<?DTD2HTML frame*bottommargin >
Specifies the amount of space reserved below the art, figtext
graphic, or listing.
<P>
<?DTD2HTML frame*cdrom >
<P>
<?DTD2HTML frame*hardcopy >
<P>
<?DTD2HTML frame*help >
<P>
<?DTD2HTML frame*id >
<P>
<?DTD2HTML frame*leftmargin >
Specifies the amount of space reserved to the left of the art, figtext,
graphic, or listing.
<P>
<?DTD2HTML frame*locale >
<P>
<?DTD2HTML frame*memo >
<P>
<?DTD2HTML frame*number >
Number of frame.
<P>
<?DTD2HTML frame*revlevel >
<P>
<?DTD2HTML frame*rightmargin >
Specifies the amount of space reserved to the right of the art, figtext,
graphic, or listing.
<P>
<?DTD2HTML frame*tonumber >
Specifies if frame is numbered.
<P>
<?DTD2HTML frame*topmargin >
Specifies the amount of space reserved above the art, figtext,
graphic, or listing.
<P>
<?DTD2HTML g >
<P>
<?DTD2HTML ge >
<P>
<?DTD2HTML glossary >
Starts a glossary.
<P>
<?DTD2HTML glossary* >
<P>
<?DTD2HTML glossary*cdrom >
<P>
<?DTD2HTML glossary*hardcopy >
<P>
<?DTD2HTML glossary*help >
<P>
<?DTD2HTML glossary*id >
<P>
<?DTD2HTML glossary*locale >
<P>
<?DTD2HTML glossary*memo >
<P>
<?DTD2HTML glossary*revlevel >
<P>
<?DTD2HTML graphic >
Integrates scanned images, captured screens, and on-line drawings.
<P>
<?DTD2HTML graphic* >
<P>
<?DTD2HTML graphic*bottommargin >
<P>
<?DTD2HTML graphic*button >
<P>
<?DTD2HTML graphic*cappos >
Specifies whether the caption is left-justified, centered, or right-justified.
<P>
<?DTD2HTML graphic*cdrom >
<P>
<?DTD2HTML graphic*description >
<P>
<?DTD2HTML graphic*entity >
<P>
<?DTD2HTML graphic*figpos >
<P>
<?DTD2HTML graphic*hardcopy >
<P>
<?DTD2HTML graphic*help >
<P>
<?DTD2HTML graphic*id >
<P>
<?DTD2HTML graphic*leftmargin >
<P>
<?DTD2HTML graphic*linkinfo >
<P>
<?DTD2HTML graphic*locale >
<P>
<?DTD2HTML graphic*magnification >
Specifies the amount of magnification. For example, a value of 2
would double the size of the graphic.
<P>
<?DTD2HTML graphic*memo >
<P>
<?DTD2HTML graphic*placement >
Specifies whether the graphic is centered, left-justified, or right-justified.
<P>
<?DTD2HTML graphic*reprodep >
<P>
<?DTD2HTML graphic*reprowid >
<P>
<?DTD2HTML graphic*resolution >
Specifies the resolution in number of dots per inch.
<P>
<?DTD2HTML graphic*revlevel >
<P>
<?DTD2HTML graphic*rid >
<P>
<?DTD2HTML graphic*rightmargin >
<P>
<?DTD2HTML graphic*scalefit >
Specifies a value for scaling a graphic so it fits inside a 
currently defined space (such as a frame). 
<P>
<?DTD2HTML graphic*topmargin >
<P>
<?DTD2HTML graphic*traversal >
<P>
<?DTD2HTML graphic*window >
<P>
<?DTD2HTML hardware >
Specifies the hardware on which a product runs. An optional element in
the <a href="idsection.html">IDSECTION</a>;
hardware follows the optional <a href="volume.html">VOLUME</a>
and precedes the optional <a href="version.html">VERSION</a>,
<P>
<?DTD2HTML hardware* >
<P>
<?DTD2HTML hardware*id >
<P>
<?DTD2HTML head >
Identifies the title for elements such as
<a href="d1.html">D1</a>,
<a href="d2.html">D2</a>,
D3-D10,
<a href="annotext.html">ANNOTEXT</a>,
and
<a href="list.html">LIST</a>.
The head element is also used with idsection elements that do
not have a default head, such as
<a href="conventions.html">CONVENTIONS</a>,
<a href="history.html">HISTORY</a>,
and
<a href="legal.html">LEGAL</a>.
<p>
Certain elements (such as
<a href="note.html">NOTE</a>s,
<a href="caution.html">CAUTION</a>s,
and
<a href="warning.html">WARNING</a>s)
have a default
head where the head element is optional. In these cases, you can use
the head element to override the default head.
<P>
<?DTD2HTML head* >
<P>
<?DTD2HTML head*cdrom >
<P>
<?DTD2HTML head*hardcopy >
<P>
<?DTD2HTML head*help >
<P>
<?DTD2HTML head*id >
<P>
<?DTD2HTML head*locale >
<P>
<?DTD2HTML head*memo >
<P>
<?DTD2HTML head*revlevel >
<P>
<?DTD2HTML header >
Places text in the top margin of each page of a printed document.
<P>
<?DTD2HTML header* >
<P>
<?DTD2HTML header*cdrom >
<P>
<?DTD2HTML header*hardcopy >
<P>
<?DTD2HTML header*help >
<P>
<?DTD2HTML header*id >
<P>
<?DTD2HTML header*locale >
<P>
<?DTD2HTML header*memo >
<P>
<?DTD2HTML header*revlevel >
<P>
<?DTD2HTML history >
Provides the printing history of the document. History is an optional
element in
<a href="metainfo.html">METAINFO</a>.
It follows the optional
<a href="legal.html">LEGAL</a>
element and precedes the optional
<a href="safety.html">SAFETY</a>
element.
<P>
<?DTD2HTML history* >
<P>
<?DTD2HTML history*cdrom >
<P>
<?DTD2HTML history*hardcopy >
<P>
<?DTD2HTML history*help >
<P>
<?DTD2HTML history*id >
<P>
<?DTD2HTML history*locale >
<P>
<?DTD2HTML history*memo >
<P>
<?DTD2HTML history*revlevel >
<P>
<?DTD2HTML hometopic >
Identifies the top-level topic in an on-line document. Conceptually,
the hometopic is the root of the hierarchy that contains all other
topics.
<P>
<?DTD2HTML hometopic* >
<P>
<?DTD2HTML hometopic*cdrom >
<P>
<?DTD2HTML hometopic*hardcopy >
<P>
<?DTD2HTML hometopic*help >
<P>
<?DTD2HTML hometopic*id >
<P>
<?DTD2HTML hometopic*locale >
<P>
<?DTD2HTML hometopic*memo >
<P>
<?DTD2HTML hometopic*revlevel >
<P>
<?DTD2HTML hsp >
<P>
<?DTD2HTML hsp* >
<P>
<?DTD2HTML hsp*sp >
<P>
<?DTD2HTML hyphenationset >
<P>
<?DTD2HTML hyphentry >
<P>
<?DTD2HTML idsection >
Begins the identification section of a document. IDSECTION is a
equired element in the optional
<a href="metainfo.html">METAINFO</a>
element.
<P>
<?DTD2HTML idx >
Designates entries for an automatically generated index.
<P>
<?DTD2HTML idx* >
<P>
<?DTD2HTML idx*main >
REF identifies a normal index entry. MAIN identifies an index entry as
the main one among all entries listed for hat item.
<P>
<?DTD2HTML idx*range >
Marks the beginning, or the end, of a segment of indexed text.
<P>
<?DTD2HTML idxkey1 >
<P>
<?DTD2HTML idxkey2 >
<P>
<?DTD2HTML idxsyn >
Identifies an index synonym.
<P>
<?DTD2HTML ig >
<P>
<?DTD2HTML image >
Maintains the author's line breaks in a section of text.
Special characters retain their meaning.
<P>
<?DTD2HTML image* >
<P>
<?DTD2HTML image*cdrom >
<P>
<?DTD2HTML image*hardcopy >
<P>
<?DTD2HTML image*help >
<P>
<?DTD2HTML image*id >
<P>
<?DTD2HTML image*indent >
<P>
<?DTD2HTML image*locale >
<P>
<?DTD2HTML image*memo >
<P>
<?DTD2HTML image*revlevel >
<P>
<?DTD2HTML in >
<P>
<?DTD2HTML in* >
<P>
<?DTD2HTML in*align >
<P>
<?DTD2HTML inc >
<P>
<?DTD2HTML index >
Starts the index.
<P>
<?DTD2HTML indexchapref >
<P>
<?DTD2HTML indexchaprefmain >
<P>
<?DTD2HTML indexentry >
<P>
<?DTD2HTML indexkey1 >
<P>
<?DTD2HTML indexkey1andref >
<P>
<?DTD2HTML indexkey2 >
<P>
<?DTD2HTML indexkey2andref >
<P>
<?DTD2HTML indexletter >
<P>
<?DTD2HTML indexsect >
<P>
<?DTD2HTML indexsyn >
<P>
<?DTD2HTML ineqn >
Creates in-line equations.
<p>
An ineqn element is treated like a single character in the text and
obeys the word-wrap rules of the current language.
<P>
<?DTD2HTML ineqn* >
<P>
<?DTD2HTML ineqn*bitmap >
<P>
<?DTD2HTML ineqn*id >
<P>
<?DTD2HTML inf >
<P>
<?DTD2HTML inf* >
<P>
<?DTD2HTML inf*loc >
<P>
<?DTD2HTML ingraphic >
Identifies an in-line graphic or spot graphic. Inline graphics are
small graphics, such as icons, that become part of a stream of text.
Spot graphics are placed in the upper left or upper right corner of the
paragraph.
<P>
<?DTD2HTML ingraphic* >
<P>
<?DTD2HTML ingraphic*button >
<P>
<?DTD2HTML ingraphic*cdrom >
<P>
<?DTD2HTML ingraphic*description >
<P>
<?DTD2HTML ingraphic*entity >
Entity name of the graphic.
<P>
<?DTD2HTML ingraphic*hardcopy >
<P>
<?DTD2HTML ingraphic*help >
<P>
<?DTD2HTML ingraphic*id >
<P>
<?DTD2HTML ingraphic*linkinfo >
<P>
<?DTD2HTML ingraphic*locale >
<P>
<?DTD2HTML ingraphic*magnification >
Specifies the amount of magnification. For example, a value of 2 
would double the size of the graphic.
<P>
<?DTD2HTML ingraphic*memo >
<P>
<?DTD2HTML ingraphic*resolution >
Specifies the resolution in number of dots per inch.
<P>
<?DTD2HTML ingraphic*revlevel >
<P>
<?DTD2HTML ingraphic*rid >
<P>
<?DTD2HTML ingraphic*traversal >
<P>
<?DTD2HTML ingraphic*type >
Type of graphic.
<P>
<?DTD2HTML ingraphic*window >
<P>
<?DTD2HTML inside >
<P>
<?DTD2HTML inside* >
<P>
<?DTD2HTML inside*id >
<P>
<?DTD2HTML intro >
Specifies the introductory text.
<P>
<?DTD2HTML intro* >
<P>
<?DTD2HTML intro*cdrom >
<P>
<?DTD2HTML intro*hardcopy >
<P>
<?DTD2HTML intro*help >
<P>
<?DTD2HTML intro*id >
<P>
<?DTD2HTML intro*locale >
<P>
<?DTD2HTML intro*memo >
<P>
<?DTD2HTML intro*revlevel >
<P>
<?DTD2HTML it >
<P>
<?DTD2HTML item >
Identifies items in a list.
<P>
<?DTD2HTML item* >
<P>
<?DTD2HTML item*cdrom >
<P>
<?DTD2HTML item*hardcopy >
<P>
<?DTD2HTML item*help >
<P>
<?DTD2HTML item*id >
The identifying name that you give to an item you want to cross-
reference (see the
<a href="xref.html">XREF</a>
element).
<P>
<?DTD2HTML item*locale >
<P>
<?DTD2HTML item*memo >
<P>
<?DTD2HTML item*revlevel >
<P>
<?DTD2HTML keycap >
Graphically represents the key caps on a keyboard.
<P>
<?DTD2HTML label >
Identifies the labels in a
<a href="lablist.html">LABLIST</a>.
<P>
<?DTD2HTML label* >
<P>
<?DTD2HTML label*cdrom >
<P>
<?DTD2HTML label*hardcopy >
<P>
<?DTD2HTML label*help >
<P>
<?DTD2HTML label*id >
<P>
<?DTD2HTML label*locale >
<P>
<?DTD2HTML label*memo >
<P>
<?DTD2HTML label*revlevel >
<P>
<?DTD2HTML labeltext >
Identifies the text that accompanies the
<a href="label.html">LABEL</a>
element in a
<a href="lablist.html">LABLIST</a>.
<P>
<?DTD2HTML labeltext* >
<P>
<?DTD2HTML labeltext*cdrom >
<P>
<?DTD2HTML labeltext*hardcopy >
<P>
<?DTD2HTML labeltext*help >
<P>
<?DTD2HTML labeltext*id >
<P>
<?DTD2HTML labeltext*locale >
<P>
<?DTD2HTML labeltext*memo >
<P>
<?DTD2HTML labeltext*revlevel >
<P>
<?DTD2HTML labheads >
Identifies the label heads in a
<a href="lablist.html">LABLIST</a>.
<P>
<?DTD2HTML labheads* >
<P>
<?DTD2HTML labheads*cdrom >
<P>
<?DTD2HTML labheads*hardcopy >
<P>
<?DTD2HTML labheads*help >
<P>
<?DTD2HTML labheads*id >
<P>
<?DTD2HTML labheads*locale >
<P>
<?DTD2HTML labheads*memo >
<P>
<?DTD2HTML labheads*revlevel >
<P>
<?DTD2HTML lablist >
Identifies a labeled list.
<p>
Output prints in two columns: one for labels, the other for
corresponding text. Heads for each column are optional; use
<a href="labheads.html">LABHEADS</a>
to identify the label head. Labels that are wider than the label area are
automatically wrapped onto successive lines.
<P>
<?DTD2HTML lablist* >
<P>
<?DTD2HTML lablist*spacing >
Specify line spacing between elements.
<P>
<?DTD2HTML lablist*width >
Numeric value of the width of the first column.
<P>
<?DTD2HTML leader >
<P>
<?DTD2HTML leader* >
<P>
<?DTD2HTML leader*type >
<P>
<?DTD2HTML legal >
Identifies any legal material other than the copyright. Legal is the
first optional part of the
<a href="metainfo.html">METAINFO</a>
element; it precedes the optional
<a href="history.html">HISTORY</a>
element.
<P>
<?DTD2HTML legal* >
<P>
<?DTD2HTML legal*cdrom >
<P>
<?DTD2HTML legal*hardcopy >
<P>
<?DTD2HTML legal*help >
<P>
<?DTD2HTML legal*id >
<P>
<?DTD2HTML legal*locale >
<P>
<?DTD2HTML legal*memo >
<P>
<?DTD2HTML legal*revlevel >
<P>
<?DTD2HTML lim >
<P>
<?DTD2HTML lim* >
<P>
<?DTD2HTML lim*align >
<P>
<?DTD2HTML lineno >
Allows you to cross-reference (see the
<a href="xref.html">XREF</a>
element) to specified line
numbers in an
<a href="ex.html">EX</a>
(example) element.
<P>
<?DTD2HTML lineno* >
<P>
<?DTD2HTML lineno*id >
<P>
<?DTD2HTML link >
Defines a relationship between two topics. The relationship is usually
indicated by an icon or highlighted phrase in one topic that, when
selected, causes the other topic to appear.
<p>
A segment of text, a
<a href="graphic.html">GRAPHIC</a>,
or an
<a href="ingraphic.html">INGRAPHIC</a>
may be used as a link.
Links may identify any element in the current document that has an
value assigned to its ID attribute. Links can also identify external
objects described in the
<a href="externalobjs.html">EXTERNALOBJS</a>
element. External objects may include:
<p>
<ul>
<li>documents via the element
<a href="xdocloc.html">XDOCLOC</a>
</li>
<li>"man" pages via the element
<a href="xmanloc.html">XMANLOC</a>
</li>
<li>system files via the element
<a href="xfileloc.html">XFILELOC</a>
</li>
<li>applications via the element
<a href="xapploc.html">XAPPLOC</a>
</li>
<li>and system commands via the element
<a href="xsysloc.html">XSYSLOC</a>.
</li>
</ul>
<P>
<?DTD2HTML link* >
<P>
<?DTD2HTML link*button >
Allows you to identify an icon as the clickable item, in addition to the 
hypertext.
<P>
<?DTD2HTML link*cdrom >
<P>
<?DTD2HTML link*description >
Provides an abstract of the link anchor (the target of the link). This 
information is intended to be used either as a:
<ul>
<li>preview of a link traversal </li>
<li>alternate to link traversal when time prevents full traversal </li>
<li>substitute for the link traversal when the anchor is not reachable </li>
</ul>
<P>
<?DTD2HTML link*hardcopy >
<P>
<?DTD2HTML link*help >
<P>
<?DTD2HTML link*id >
<P>
<?DTD2HTML link*linkinfo >
Provides a brief description of the location linked to. It serves as an
alias for the link in lists and other situations that require a brief
heading-like textual representation of the link.
<P>
<?DTD2HTML link*locale >
<P>
<?DTD2HTML link*memo >
<P>
<?DTD2HTML link*revlevel >
<P>
<?DTD2HTML link*rid >
Specifies the anchor of the link. If the anchor is within the current
document (internal), then the RID simply contains the ID of the anchor
element. If the anchor is in another information object (external),
then the RID identifies the external object element that describes that
target object.
<P>
<?DTD2HTML link*traversal >
Specifies if the relationship between the 
two points is symmetric. This attribute allows an author to differentiate 
between different relationships such as progressions and mutual dependen
cies.
<P>
<?DTD2HTML link*window >
Specifies the nature of the window used to render the anchor.
<P>
<?DTD2HTML list >
Produces a list consisting of one or more bulleted, unmarked or
automatically numbered items.
<P>
<?DTD2HTML list* >
<P>
<?DTD2HTML list*continue >
<P>
<?DTD2HTML list*spacing >
<P>
<?DTD2HTML list*type >
<P>
<?DTD2HTML listing >
Specifies that a text file is to be brought in as
<a href="figtext.html">FIGTEXT</a>.
<P>
<?DTD2HTML listing* >
<P>
<?DTD2HTML listing*cdrom >
<P>
<?DTD2HTML listing*entity >
<P>
<?DTD2HTML listing*hardcopy >
<P>
<?DTD2HTML listing*help >
<P>
<?DTD2HTML listing*id >
<P>
<?DTD2HTML listing*locale >
<P>
<?DTD2HTML listing*memo >
<P>
<?DTD2HTML listing*placement >
Specifies the placement of the listing.
<P>
<?DTD2HTML listing*revlevel >
<P>
<?DTD2HTML listing*textsize >
Specifies the size of the computer font used.
<P>
<?DTD2HTML ll >
<P>
<?DTD2HTML location >
Identifies a point or sub-paragraph phrase in a document that may be
used as an anchor (the target of a link). Use location when the point
or phrase is not a part of the document structure. You can assign an ID
value to the point or phrase.
<P>
<?DTD2HTML location* >
<P>
<?DTD2HTML location*id >
<P>
<?DTD2HTML lyr >
<P>
<?DTD2HTML lyr* >
<P>
<?DTD2HTML lyr*align >
<P>
<?DTD2HTML map >
Describes related documents. Map is an optional element in the
<a href="metainfo.html">METAINFO</a>;
it follows the optional
<a href="safety.html">SAFETY</a>
element and precedes the optional
<a href="abstract.html">ABSTRACT</a>
element.
<P>
<?DTD2HTML map* >
<P>
<?DTD2HTML map*cdrom >
<P>
<?DTD2HTML map*hardcopy >
<P>
<?DTD2HTML map*help >
<P>
<?DTD2HTML map*id >
<P>
<?DTD2HTML map*locale >
<P>
<?DTD2HTML map*memo >
<P>
<?DTD2HTML map*revlevel >
<P>
<?DTD2HTML message >
Begins the message section of a document, which provides system error
messages, their probable cause and some actions the user can take to
correct the error.
<p>
You can have only one message section in a manual. This section must be
placed after the appendixes, and before the glossary and index.
<P>
<?DTD2HTML message* >
<P>
<?DTD2HTML message*cdrom >
<P>
<?DTD2HTML message*hardcopy >
<P>
<?DTD2HTML message*help >
<P>
<?DTD2HTML message*id >
<P>
<?DTD2HTML message*locale >
<P>
<?DTD2HTML message*memo >
<P>
<?DTD2HTML message*revlevel >
<P>
<?DTD2HTML metainfo >
Identifies the front matter of a document.
<P>
<?DTD2HTML mfgno >
Identifies the document's manufacturing number. MFGNO is an optional
element in the
<a href="idsection.html">IDSECTION</a>;
it follows the optional 
<a href="printdate.html">PRINTDATE</a>
element and precedes the optional
<a href="edition.html">EDITION</a>
element.
<P>
<?DTD2HTML mfgno* >
<P>
<?DTD2HTML mfgno*id >
<P>
<?DTD2HTML middle >
<P>
<?DTD2HTML middle* >
<P>
<?DTD2HTML middle*id >
<P>
<?DTD2HTML miscmeta >
Identifies front matter that does not fit into the other
<a href="metainfo.html">METAINFO</a>
elements.
<P>
<?DTD2HTML miscmeta* >
<P>
<?DTD2HTML miscmeta*cdrom >
<P>
<?DTD2HTML miscmeta*hardcopy >
<P>
<?DTD2HTML miscmeta*help >
<P>
<?DTD2HTML miscmeta*id >
<P>
<?DTD2HTML miscmeta*locale >
<P>
<?DTD2HTML miscmeta*memo >
<P>
<?DTD2HTML miscmeta*revlevel >
<P>
<?DTD2HTML mit >
<P>
<?DTD2HTML msg >
Identifies a message returned by the documented product, such as an
error message.  Used in the
<a href="message.html">MESSAGE</a>
section.
<P>
<?DTD2HTML msg* >
<P>
<?DTD2HTML msg*cdrom >
<P>
<?DTD2HTML msg*hardcopy >
<P>
<?DTD2HTML msg*help >
<P>
<?DTD2HTML msg*id >
<P>
<?DTD2HTML msg*locale >
<P>
<?DTD2HTML msg*memo >
<P>
<?DTD2HTML msg*revlevel >
<P>
<?DTD2HTML msgnum >
Assigns the message number for an individual
<a href="msg.html">MSG</a>
element in the
<a href="message.html">MESSAGE</a>
section. 
<P>
<?DTD2HTML msgnum* >
<P>
<?DTD2HTML msgnum*cdrom >
<P>
<?DTD2HTML msgnum*hardcopy >
<P>
<?DTD2HTML msgnum*help >
<P>
<?DTD2HTML msgnum*id >
<P>
<?DTD2HTML msgnum*locale >
<P>
<?DTD2HTML msgnum*memo >
<P>
<?DTD2HTML msgnum*revlevel >
<P>
<?DTD2HTML msgsub >
Creates subsections within the message section. For example, you can
include diagnos tic, error and warning message sections.
<P>
<?DTD2HTML msgsub* >
<P>
<?DTD2HTML msgsub*cdrom >
<P>
<?DTD2HTML msgsub*hardcopy >
<P>
<?DTD2HTML msgsub*help >
<P>
<?DTD2HTML msgsub*id >
<P>
<?DTD2HTML msgsub*locale >
<P>
<?DTD2HTML msgsub*memo >
<P>
<?DTD2HTML msgsub*revlevel >
<P>
<?DTD2HTML msgtext >
Identifies the text of a
<a href="msg.html">MSG</a>
element.
<P>
<?DTD2HTML msgtext* >
<P>
<?DTD2HTML msgtext*cdrom >
<P>
<?DTD2HTML msgtext*hardcopy >
<P>
<?DTD2HTML msgtext*help >
<P>
<?DTD2HTML msgtext*id >
<P>
<?DTD2HTML msgtext*locale >
<P>
<?DTD2HTML msgtext*memo >
<P>
<?DTD2HTML msgtext*revlevel >
<P>
<?DTD2HTML needbegin >
Keeps information together so that it is grouped appropriately (as in a
list) or so it remains on a single page. Use NEEDBEGIN to start the
section; use
<a href="needend.html">NEEDEND</a>
to identify the end of the section.
<P>
<?DTD2HTML needbegin* >
<P>
<?DTD2HTML needbegin*cdrom >
<P>
<?DTD2HTML needbegin*hardcopy >
<P>
<?DTD2HTML needbegin*help >
<P>
<?DTD2HTML needbegin*id >
<P>
<?DTD2HTML needbegin*locale >
<P>
<?DTD2HTML needbegin*memo >
<P>
<?DTD2HTML needbegin*revlevel >
<P>
<?DTD2HTML needend >
Keeps information together so that it is grouped appropriately (as in a
list) or so it remains on a single page. Use
<a href="needbegin.html">NEEDBEGIN</a>
to start the section; use NEEDEND to identify the end of the section.
<P>
<?DTD2HTML newline >
Forces a line break.
<P>
<?DTD2HTML newpage >
Forces a page break.
<P>
<?DTD2HTML nobreak >
Keeps together text that is not to be split across successive lines when
printed.
<P>
<?DTD2HTML nobreak* >
<P>
<?DTD2HTML nobreak*id >
<P>
<?DTD2HTML not >
Indicates the inverse of a logical state, formatted as a horizontal bar
over the state's name.
<P>
<?DTD2HTML note >
Alerts the reader to an important point in the text.
<P>
<?DTD2HTML nu >
<P>
<?DTD2HTML objid >
Uniquely identifies an external information object in the format
required by an applica tion. The content of this element, typically a
numerical value, is intended to be supplied to the author of the
document.
<P>
<?DTD2HTML objid* >
<P>
<?DTD2HTML objid*id >
<P>
<?DTD2HTML op >
<P>
<?DTD2HTML opd >
<P>
<?DTD2HTML openbook >
The master container for a document.
<P>
<?DTD2HTML openbook* >
<P>
<?DTD2HTML openbook*author >
Identifies the author of the document.
<P>
<?DTD2HTML openbook*charset >
Specifies which character set was used to author the text files.
<P>
<?DTD2HTML openbook*cropmarks >
Specifies whether crop marks are printed.
<P>
<?DTD2HTML openbook*docid >
Identifies the document.
<P>
<?DTD2HTML openbook*handstyle >
Specifies if hand icons should be printed in
<a href="caution.html">CAUTION</a>,
<a href="note.html">NOTE</a>,
and
<a href="warning.html">WARNING</a>
elements.
<P>
<?DTD2HTML openbook*language >
Specifies the language in which the document is created.
<P>
<?DTD2HTML openbook*layout >
Specifies the size of the document.
<P>
<?DTD2HTML openbook*margin >
Specifies the page layout format.
<P>
<?DTD2HTML openbook*memos >
Specifies whether memo text is printed.
<P>
<?DTD2HTML openbook*revision >
<P>
<?DTD2HTML openbook*revmarks >
Specify if revised text is highlighted.
<P>
<?DTD2HTML openbook*status >
Specifies the status of the document.
<P>
<?DTD2HTML openbook*tabstyle >
Should bleeder tabs be printed.
<P>
<?DTD2HTML openbook*wrapping >
<P>
<?DTD2HTML optblock >
Indicates optional parameters in a syntax diagram.
<P>
<?DTD2HTML outside >
<P>
<?DTD2HTML outside* >
<P>
<?DTD2HTML outside*id >
<P>
<?DTD2HTML ovb >
<P>
<?DTD2HTML ovl >
<P>
<?DTD2HTML ovl* >
<P>
<?DTD2HTML ovl*style >
<P>
<?DTD2HTML p >
Starts a new paragraph.
<P>
<?DTD2HTML p* >
<P>
<?DTD2HTML p*cdrom >
<P>
<?DTD2HTML p*hardcopy >
<P>
<?DTD2HTML p*help >
<P>
<?DTD2HTML p*id >
<P>
<?DTD2HTML p*indent >
Specifies whether paragraphs are indented from the 
current left margin.
<P>
<?DTD2HTML p*locale >
<P>
<?DTD2HTML p*memo >
<P>
<?DTD2HTML p*revlevel >
<P>
<?DTD2HTML parm >
Identifies a parameter in either a syntax statement or in body text.
<P>
<?DTD2HTML parm* >
<P>
<?DTD2HTML parm*id >
<P>
<?DTD2HTML part >
Starts a part. This element is typically used to bind two or more books
in one binding. It allows the books to be treated as individual
documents-each has its own document elements, such as a title, table of
contents, page numbering, and index.
<P>
<?DTD2HTML part* >
<P>
<?DTD2HTML part*cdrom >
<P>
<?DTD2HTML part*hardcopy >
<P>
<?DTD2HTML part*help >
<P>
<?DTD2HTML part*id >
<P>
<?DTD2HTML part*insertnum >
Optional attribute used to override automatic numbering.
<P>
<?DTD2HTML part*locale >
<P>
<?DTD2HTML part*memo >
<P>
<?DTD2HTML part*resetnum >
Resets the counter. All subsequent numbers used after this point are 
incremented by 1 from the value of "resetnum".
<P>
<?DTD2HTML part*revlevel >
<P>
<?DTD2HTML partno >
Identifies the manual part number. PARTNO is an optional element in the
<a href="idsection.html">IDSECTION</a>;
it follows the optional
<a href="serialno.html">SERIALNO</a>
element and precedes the optional
<a href="ficheno.html">FICHENO</a>
element.
<P>
<?DTD2HTML partno* >
<P>
<?DTD2HTML partno*id >
<P>
<?DTD2HTML phr >
<P>
<?DTD2HTML pr >
<P>
<?DTD2HTML pr* >
<P>
<?DTD2HTML pr*align >
<P>
<?DTD2HTML preface >
Identifies preface text in a document.
<P>
<?DTD2HTML preface* >
<P>
<?DTD2HTML preface*cdrom >
<P>
<?DTD2HTML preface*hardcopy >
<P>
<?DTD2HTML preface*help >
<P>
<?DTD2HTML preface*id >
<P>
<?DTD2HTML preface*locale >
<P>
<?DTD2HTML preface*memo >
<P>
<?DTD2HTML preface*revlevel >
<P>
<?DTD2HTML printdate >
Specifies the date the manual is printed.
<P>
<?DTD2HTML printdate* >
<P>
<?DTD2HTML printdate*id >
<P>
<?DTD2HTML product >
Identifies the name of the documented product.
<P>
<?DTD2HTML product* >
<P>
<?DTD2HTML product*id >
<P>
<?DTD2HTML rad >
<P>
<?DTD2HTML rcd >
<P>
<?DTD2HTML rdx >
<P>
<?DTD2HTML reqblock >
Indicates required parameters in a syntax diagram.
<P>
<?DTD2HTML reviewer >
Identifies the reviewer of the document.
<P>
<?DTD2HTML reviewer* >
<P>
<?DTD2HTML reviewer*id >
<P>
<?DTD2HTML reviewerset >
Lists the reviewers of the document.
<P>
<?DTD2HTML rf >
<P>
<?DTD2HTML rm >
<P>
<?DTD2HTML row >
<P>
<?DTD2HTML row* >
<P>
<?DTD2HTML row*rowsep >
<P>
<?DTD2HTML rp >
<P>
<?DTD2HTML rp* >
<P>
<?DTD2HTML rp*post >
<P>
<?DTD2HTML rp*style >
<P>
<?DTD2HTML rsect >
Begins a reference section.
<P>
<?DTD2HTML rsect* >
<P>
<?DTD2HTML rsect*cdrom >
<P>
<?DTD2HTML rsect*hardcopy >
<P>
<?DTD2HTML rsect*help >
<P>
<?DTD2HTML rsect*id >
<P>
<?DTD2HTML rsect*locale >
<P>
<?DTD2HTML rsect*memo >
<P>
<?DTD2HTML rsect*pagebreak >
Set the page break.
<P>
<?DTD2HTML rsect*revlevel >
<P>
<?DTD2HTML rsub >
Begins a reference subsection.
<P>
<?DTD2HTML rsub* >
<P>
<?DTD2HTML rsub*cdrom >
<P>
<?DTD2HTML rsub*hardcopy >
<P>
<?DTD2HTML rsub*help >
<P>
<?DTD2HTML rsub*id >
<P>
<?DTD2HTML rsub*locale >
<P>
<?DTD2HTML rsub*memo >
<P>
<?DTD2HTML rsub*revlevel >
<P>
<?DTD2HTML safety >
Identifies safety information for the documented product.
<P>
<?DTD2HTML safety* >
<P>
<?DTD2HTML safety*cdrom >
<P>
<?DTD2HTML safety*hardcopy >
<P>
<?DTD2HTML safety*help >
<P>
<?DTD2HTML safety*id >
<P>
<?DTD2HTML safety*locale >
<P>
<?DTD2HTML safety*memo >
<P>
<?DTD2HTML safety*revlevel >
<P>
<?DTD2HTML sc >
<P>
<?DTD2HTML seefromkey1 >
<P>
<?DTD2HTML seefromkey2 >
<P>
<?DTD2HTML seetokey1 >
<P>
<?DTD2HTML seetokey2 >
<P>
<?DTD2HTML serialno >
Lists the serial numbers of the documented product.
<P>
<?DTD2HTML serialno* >
<P>
<?DTD2HTML serialno*id >
<P>
<?DTD2HTML softkey >
Creates a graphic representation of the text within the label field of a
function key.
<P>
<?DTD2HTML sort >
<P>
<?DTD2HTML source >
Gives copyright credit to vendors or third parties if part of the product
was developed outside of the company.
<P>
<?DTD2HTML source* >
<P>
<?DTD2HTML source*id >
<P>
<?DTD2HTML spanspec >
<P>
<?DTD2HTML spanspec* >
<P>
<?DTD2HTML spanspec*align >
<P>
<?DTD2HTML spanspec*char >
<P>
<?DTD2HTML spanspec*charoff >
<P>
<?DTD2HTML spanspec*colsep >
<P>
<?DTD2HTML spanspec*nameend >
<P>
<?DTD2HTML spanspec*namest >
<P>
<?DTD2HTML spanspec*rowsep >
<P>
<?DTD2HTML spanspec*spanname >
<P>
<?DTD2HTML stk >
<P>
<?DTD2HTML sub >
Identifies subscript data characters.
<P>
<?DTD2HTML subcategory >
Indicates a subgroup within the
<a href="hardware.html">HARDWARE</a>
element in the
<a href="idsection.html">IDSECTION</a>.
<P>
<?DTD2HTML subcategory* >
<P>
<?DTD2HTML subcategory*id >
<P>
<?DTD2HTML sum >
<P>
<?DTD2HTML sum* >
<P>
<?DTD2HTML sum*align >
<P>
<?DTD2HTML sup >
<P>
<?DTD2HTML sup* >
<P>
<?DTD2HTML sup*loc >
<P>
<?DTD2HTML super >
Identifies superscript data characters.
<P>
<?DTD2HTML supplement >
Contains all the back matter in a document.
<P>
<?DTD2HTML synonym >
<P>
<?DTD2HTML syntax >
Defines syntax diagrams.
<P>
<?DTD2HTML table >
Defines the structure of tables.  The industry standard CALS tables
have been implemented.
<P>
<?DTD2HTML table* >
<P>
<?DTD2HTML table*cdrom >
<P>
<?DTD2HTML table*frame >
Frame type that surrounds the table.
<P>
<?DTD2HTML table*hardcopy >
<P>
<?DTD2HTML table*help >
<P>
<?DTD2HTML table*id >
<P>
<?DTD2HTML table*locale >
<P>
<?DTD2HTML table*memo >
<P>
<?DTD2HTML table*number >
Assigns caption number.
<P>
<?DTD2HTML table*revlevel >
<P>
<?DTD2HTML table*tonumber >
Should tables be numbered.
<P>
<?DTD2HTML tabletoc >
Lists all the tables within a document.
<P>
<?DTD2HTML tabletocentry >
<P>
<?DTD2HTML tbody >
<P>
<?DTD2HTML tbody* >
<P>
<?DTD2HTML tbody*valign >
<P>
<?DTD2HTML term >
Identifies newly introduced terms within the body of the document.
<P>
<?DTD2HTML term* >
<P>
<?DTD2HTML term*base >
<P>
<?DTD2HTML term*gloss >
<P>
<?DTD2HTML term*id >
<P>
<?DTD2HTML test >
Provides a heading one level above
<a href="d2.html">D2</a>
for text presented in service
documentation.
<P>
<?DTD2HTML test* >
<P>
<?DTD2HTML test*cdrom >
<P>
<?DTD2HTML test*hardcopy >
<P>
<?DTD2HTML test*help >
<P>
<?DTD2HTML test*id >
<P>
<?DTD2HTML test*locale >
<P>
<?DTD2HTML test*memo >
<P>
<?DTD2HTML test*revlevel >
<P>
<?DTD2HTML tfoot >
<P>
<?DTD2HTML tfoot* >
<P>
<?DTD2HTML tfoot*valign >
<P>
<?DTD2HTML tgroup >
<P>
<?DTD2HTML tgroup* >
<P>
<?DTD2HTML tgroup*align >
<P>
<?DTD2HTML tgroup*char >
<P>
<?DTD2HTML tgroup*charoff >
<P>
<?DTD2HTML tgroup*cols >
<P>
<?DTD2HTML tgroup*colsep >
<P>
<?DTD2HTML tgroup*rowsep >
<P>
<?DTD2HTML thead >
<P>
<?DTD2HTML thead* >
<P>
<?DTD2HTML thead*valign >
<P>
<?DTD2HTML title >
Specifies the title of a manual.
<P>
<?DTD2HTML title* >
<P>
<?DTD2HTML title*id >
<P>
<?DTD2HTML tnote >
Identifies a table footnote.
<P>
<?DTD2HTML tnote* >
<P>
<?DTD2HTML tnote*cdrom >
<P>
<?DTD2HTML tnote*hardcopy >
<P>
<?DTD2HTML tnote*help >
<P>
<?DTD2HTML tnote*id >
<P>
<?DTD2HTML tnote*locale >
<P>
<?DTD2HTML tnote*memo >
<P>
<?DTD2HTML tnote*revlevel >
<P>
<?DTD2HTML tnoteref >
Allows you to cross-reference footnotes to tables.
<P>
<?DTD2HTML tnoteref* >
<P>
<?DTD2HTML tnoteref*rid >
Specifies the reference ID of the footnote being cross-referenced.
<P>
<?DTD2HTML toc >
Specifies a document's table of contents.
<P>
<?DTD2HTML tocd1name >
<P>
<?DTD2HTML tocd1no >
<P>
<?DTD2HTML tocentryd1 >
<P>
<?DTD2HTML tocentryd2 >
<P>
<?DTD2HTML tocentryd3 >
<P>
<?DTD2HTML tocentryd4 >
<P>
<?DTD2HTML tocentryd5 >
<P>
<?DTD2HTML tocentrypart >
<P>
<?DTD2HTML tocname >
<P>
<?DTD2HTML tocpage >
<P>
<?DTD2HTML tocpartname >
<P>
<?DTD2HTML tocpartno >
<P>
<?DTD2HTML toftcaption >
<P>
<?DTD2HTML toftentryno >
<P>
<?DTD2HTML toftpage >
<P>
<?DTD2HTML tu >
<P>
<?DTD2HTML ty >
<P>
<?DTD2HTML ul >
<P>
<?DTD2HTML unb >
<P>
<?DTD2HTML unl >
<P>
<?DTD2HTML unl* >
<P>
<?DTD2HTML unl*style >
<P>
<?DTD2HTML user >
Indicates text the user enters or the user's response to a computer prompt.
<P>
<?DTD2HTML v >
<P>
<?DTD2HTML var >
Indicates a variable (a user-supplied field) in a command.
<P>
<?DTD2HTML version >
Indicates the version of a document.
<P>
<?DTD2HTML version* >
<P>
<?DTD2HTML version*id >
<P>
<?DTD2HTML vex >
Creates a verbatim example.
<p>
No element markup is recognized within vex.
<P>
<?DTD2HTML vex* >
<P>
<?DTD2HTML vex*textsize >
Specify text size of computer font.
<P>
<?DTD2HTML vex*type >
Specify type of text.
<P>
<?DTD2HTML vmk >
<P>
<?DTD2HTML vmk* >
<P>
<?DTD2HTML vmk*id >
<P>
<?DTD2HTML vmkr >
<P>
<?DTD2HTML vmkr* >
<P>
<?DTD2HTML vmkr*rid >
<P>
<?DTD2HTML volume >
Indicates when a manual is a volume within a series.
<P>
<?DTD2HTML volume* >
<P>
<?DTD2HTML volume*id >
<P>
<?DTD2HTML vsp >
<P>
<?DTD2HTML vsp* >
<P>
<?DTD2HTML vsp*sp >
<P>
<?DTD2HTML warning >
Alerts the reader to a situation that could be dangerous to the user.
<P>
<?DTD2HTML xapploc >
Specifies that the external object is in the information domain of
he parent application of the viewer application.
<P>
<?DTD2HTML xapploc* >
<P>
<?DTD2HTML xapploc*id >
<P>
<?DTD2HTML xapploc*xcbdata >
The application callback is provided the data from the XCBDATA attribute.
<P>
<?DTD2HTML xapploc*xcbtype >
The XCBTYPE attribute contains data to select a particular application
callback if the application supports multiple callbacks.
<P>
<?DTD2HTML xdocloc >
Specifies that external information is in an online document.
<P>
<?DTD2HTML xdocloc* >
<P>
<?DTD2HTML xdocloc*id >
Specifies the ID used to link to the XDOCLOC element.
<P>
<?DTD2HTML xdocloc*xquery >
Used for documents that are to be located by query.
<P>
<?DTD2HTML xdocloc*xrid >
Specifies the point within the specified document whose element ID 
attribute has the same value.  XRID overrides XQUERY if both are defined.
<P>
<?DTD2HTML xfileloc >
Specifies that external information is in the information domain of the
file system.
<P>
<?DTD2HTML xfileloc* >
<P>
<?DTD2HTML xfileloc*id >
Specifies the ID used to link to the XFILELOC element.
<P>
<?DTD2HTML xfileloc*xfsspec >
Identifies the file within the file system.
<P>
<?DTD2HTML xfileloc*xoffset >
Specifies the byte (not character) offset into the file.
<P>
<?DTD2HTML xmanloc >
Specifies that external information is located in a man page.
<P>
<?DTD2HTML xmanloc* >
<P>
<?DTD2HTML xmanloc*id >
Specifies the ID used to link to the XMANLOC element.
<P>
<?DTD2HTML xmanloc*xmancmd >
Specifies the command used to access the information.
<P>
<?DTD2HTML xobjinfo >

Identifies external objects. This element contains the same elements as
a document's
<a href="idsection.html">IDSECTION</a>.
The XOBJINFO element identifies a wide variety
of information that allows the evolution of cross-document's object
authoring and delivery tools and practices.
<p>
Cross-object references made directly to this element imply that the
link is to the begin ning of that object.
<P>
<?DTD2HTML xobjinfo* >
<P>
<?DTD2HTML xobjinfo*id >
<P>
<?DTD2HTML xquery >
Is contained within the
<a href="xobjinfo.html">XOBJINFO</a>
element and may be used to locate a
different document that the current document should link to.
<P>
<?DTD2HTML xquery* >
<P>
<?DTD2HTML xquery*id >
Specifies the ID used to link to the XQUERY element.
<P>
<?DTD2HTML xref >
<P>
<?DTD2HTML xref* >
Creates a cross reference to another section of the document.
<P>
<?DTD2HTML xref*rid >
Specifies the reference ID of the element being cross-referenced.
<P>
<?DTD2HTML xsysloc >
Identifies the external object in the information domain of the
operating system.
<P>
<?DTD2HTML xsysloc* >
<P>
<?DTD2HTML xsysloc*id >
<P>
<?DTD2HTML xsysloc*xsyscmd >
The operating system supplying the data.
<P>
