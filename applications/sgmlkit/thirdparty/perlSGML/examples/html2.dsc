<!-- Element Description File   				          -->
<!-- Source File:  html2.0.desc                                           -->
<!-- Source DTD:  html.dtd                                                -->
<!-- Date:  Thu Aug 24 14:51:46 CDT 1995                                  -->
<!-- #################################################################### -->
<!-- ##                      Home Page Description                     ## -->
<!-- #################################################################### -->
<?DTD2HTML #include htmlents.dsc >
<?DTD2HTML -home- >
<p>
This document allows you to navigate through the structure of the HTML
DTD designated by the RCS Id:
</p>
<pre>
    $Id: html.dtd,v 1.30 1995/09/21 23:30:19 connolly Exp $
</pre>
<p>
Element and attribute descriptions are extracted from
<a href="http://www.ics.uci.edu/pub/ietf/html/rfc1866.txt">RFC 1866</a>.
</p>
<dl>
<dt>NOTE</dt>
<dd>
HTML 2.0 contains SGML Document Access (SDA) fixed attributes
in support of easy transformation to the International Committee
for Accessible Document Design (ICADD) DTD
         "-//EC-USA-CDA/ICADD//DTD ICADD22//EN".
<p>
ICADD applications are designed to support usable access to
structured information by print-impaired individuals through
Braille, large print and voice synthesis.  For more information on
SDA & ICADD:
</p>
<ul>
<li>ISO 12083:1993, Annex A.8, Facilities for Braille,
  large print and computer voice
<li>ICADD ListServ
  &lt;ICADD%ASUACAD.BITNET@ARIZVM1.ccit.arizona.edu&gt;
<li>Usenet news group bit.listserv.easi
<li>Recording for the Blind, +1 800 221 4792
</ul>
</dl>
<hr>
<p>
For the latest information about HTML, see
&lt;URL:<a href="http://www.w3.org/hypertext/WWW/MarkUp/MarkUp.html">
http://www.w3.org/hypertext/WWW/MarkUp/MarkUp.html</a>&gt;
</p>
<!-- #################################################################### -->
<!-- ##                       Shared Descriptions                      ## -->
<!-- #################################################################### -->
<?DTD2HTML *sdaform >
<p>
SDA/ICADD: One to one mapping.
</p>
<?DTD2HTML *sdarule >
<p>
SDA/ICADD: Context-sensitive mapping.
</p>
<?DTD2HTML *sdapref >
<p>
SDA/ICADD: Generated text prefix.
</p>
<?DTD2HTML *sdasuff >
<p>
SDA/ICADD: Generated text suffix.
</p>
<?DTD2HTML *sdasusp >
<p>
SDA/ICADD: Suspend transform process.
</p>
<?DTD2HTML h1,h2,h3,h4,h5,h6 >
<p>
The six heading elements,
&lt;H1&gt; through &lt;H6&gt;, denote section headings.
Although the order and occurrence of headings is not constrained by
the HTML DTD, documents should not skip levels (for example, from H1
to H3), as converting such documents to other representations is
often problematic.
</p>
<p>
Example of use:
</p>
<pre>
    &lt;H1&gt;This is a heading&lt;/H1&gt;
    Here is some text
    &lt;H2&gt;Second level heading&lt;/H2&gt;
    Here is some more text.
</pre>
<!-- #################################################################### -->
<!-- ##                       Short Descriptions                       ## -->
<!-- #################################################################### -->
<?DTD2HTML a+ >
Anchor; source and/or destination of a link
<?DTD2HTML address+ >
Address, signature, or byline for document or passage
<?DTD2HTML b+ >
Bold text
<?DTD2HTML base+ >
Base context document
<?DTD2HTML blockquote+ >
Quoted passage
<?DTD2HTML body+ >
Document body
<?DTD2HTML br+ >
Line break
<?DTD2HTML cite+ >
Name or title of cited work
<?DTD2HTML code+ >
Source code phrase
<?DTD2HTML dd+ >
Definition of term
<?DTD2HTML dfn+ >
<?DTD2HTML dir+ >
Directory list
<?DTD2HTML dl+ >
Definition list, or glossary
<?DTD2HTML dt+ >
Term in definition list
<?DTD2HTML em+ >
Emphasized phrase
<?DTD2HTML expires+ >
<?DTD2HTML form+ >
Fill-out or data-entry form
<?DTD2HTML h1+ >
Heading, level 1
<?DTD2HTML h2+ >
Heading, level 2
<?DTD2HTML h3+ >
Heading, level 3
<?DTD2HTML h4+ >
Heading, level 4
<?DTD2HTML h5+ >
Heading, level 5
<?DTD2HTML h6+ >
Heading, level 6
<?DTD2HTML head+ >
Document head
<?DTD2HTML hr+ >
Horizontal rule
<?DTD2HTML html+ >
HyperText Markup Language Document
<?DTD2HTML i+ >
Italic text
<?DTD2HTML img+ >
Image; icon, glyph or illustration
<?DTD2HTML input+ >
Form input datum
<?DTD2HTML isindex+ >
Document is a searchable index
<?DTD2HTML kbd+ >
Keyboard phrase, e.g. user input
<?DTD2HTML key+ >
<?DTD2HTML li+ >
List item
<?DTD2HTML link+ >
Link from this document
<?DTD2HTML listing+ >
Computer listing
<?DTD2HTML menu+ >
Menu list
<?DTD2HTML meta+ >
Generic meta-information
<?DTD2HTML nextid+ >
Next ID to use for link name
<?DTD2HTML ol+ >
Ordered, or numbered list
<?DTD2HTML option+ >
A selection option
<?DTD2HTML p+ >
Paragraph
<?DTD2HTML plaintext+ >
Plain text passage
<?DTD2HTML pre+ >
Preformatted text
<?DTD2HTML samp+ >
Sample text or characters
<?DTD2HTML select+ >
Selection of option(s)
<?DTD2HTML strike+ >
<?DTD2HTML strong+ >
Strong emphasis
<?DTD2HTML textarea+ >
An area for text input
<?DTD2HTML title+ >
Title of document
<?DTD2HTML tt+ >
Typewriter text
<?DTD2HTML u+ >
<?DTD2HTML ul+ >
Unordered list
<?DTD2HTML var+ >
Variable phrase or substitutable
<?DTD2HTML xmp+ >
Example section
<!-- #################################################################### -->
<!-- ##                          Descriptions                          ## -->
<!-- #################################################################### -->
<?DTD2HTML a >
<p>
The &lt;A&gt; element indicates a hyperlink anchor.
At least one of the NAME and HREF attributes should be present.
</p>
<?DTD2HTML a* >
<?DTD2HTML a*href >
<p>
Gives the URI of the head anchor of a hyperlink.
</p>
<?DTD2HTML a*methods >
<p>
Specifies methods to be used in accessing the
destination, as a whitespace-separated list of names.
The set of applicable names is a function of the scheme
of the URI in the HREF attribute. For similar reasons as
for the <a href="title.html">TITLE</a>
attribute, it may be useful to include the
information in advance in the link. For example, the
HTML user agent may chose a different rendering as a
function of the methods allowed; for example, something
that is searchable may get a different icon.
</p>
<?DTD2HTML a*name >
<p>
Gives the name of the anchor, and makes it available as
a head of a hyperlink.
</p>
<?DTD2HTML a*rel >
<p>
The REL attribute gives the relationship(s) described by
the hyperlink. The value is a whitespace separated list
of relationship names. The semantics of link
relationships are not specified in this document.
</p>
<?DTD2HTML a*rev >
<p>
Same as the REL attribute, but the semantics of the
relationship are in the reverse direction. A link from A
to B with REL="X" expresses the same relationship as a
link from B to A with REV="X". An anchor may have both
REL and REV attributes.
</p>
<?DTD2HTML a*sdapref >
<?DTD2HTML a*title >
<p>
Suggests a title for the destination resource --
advisory only. The TITLE attribute may be used:
</p>
<ul>
<li>for display prior to accessing the destination
resource, for example, as a margin note or on a
small box while the mouse is over the anchor, or
while the document is being loaded;</li>
<li>for resources that do not include a title, such as
graphics, plain text and Gopher menus, for use as a
window title.</li>
</ul>
<?DTD2HTML a*urn >
<p>
Specifies a preferred, more persistent identifier for
the head anchor of the hyperlink. The syntax and
semantics of the URN attribute are not yet specified.
</P>

<?DTD2HTML address >
<p>
The &lt;ADDRESS&gt; element contains such information as address, signature
and authorship, often at the beginning or end of the body of a
document.
</p>
<p>
Example of use:
</p>
<pre>
    &lt;ADDRESS&gt;
    Newsletter editor&lt;BR&gt;
    J.R. Brown&lt;BR&gt;
    JimquickPost News, Jimquick, CT 01234&lt;BR&gt;
    Tel (123) 456 7890
    &lt;/ADDRESS&gt;
</pre>
<?DTD2HTML address* >
<?DTD2HTML address*sdaform >
<?DTD2HTML address*sdapref >

<?DTD2HTML b >
<p>
The &lt;B&gt; element indicates bold text. Where bold typography is
unavailable, an alternative representation may be used.
</p>
<?DTD2HTML b* >
<?DTD2HTML b*sdaform >

<?DTD2HTML base >
<p>
The optional &lt;BASE&gt; element provides a base address for interpreting
relative URLs when the document is read out of context
The value of the HREF attribute must be an absolute URI.
</p>
<?DTD2HTML base* >
<P>
<?DTD2HTML base*href >
The URL of the document.
<P>
<?DTD2HTML base*sdapref >
<P>
<?DTD2HTML blockquote >
<p>
The &lt;BLOCKQUOTE&gt; element contains text quoted from another source.
</p>
<p>
Example of use:
</p>
<pre>
    I think the play ends
    &lt;BLOCKQUOTE&gt;
    &lt;P&gt;Soft you now, the fair Ophelia. Nymph, in thy orisons, be all
    my sins remembered.
    &lt;/BLOCKQUOTE&gt;
    but I am not sure.
</pre>
<?DTD2HTML blockquote* >
<?DTD2HTML blockquote*sdaform >

<?DTD2HTML body >
<P>
The &lt;BODY&gt; element contains the text flow of the document, including
headings, paragraphs, lists, etc.
</p>
<p>
For example:
</p>
<pre>
    &lt;BODY&gt;
    &lt;h1&gt;Important Stuff&lt;/h1&gt;
    &lt;p&gt;Explanation about important stuff...
    &lt;/BODY&gt;
</pre>
<?DTD2HTML body* >
<P>
<?DTD2HTML body*sdapref >
<P>
<?DTD2HTML br >
<p>
The &lt;BR&gt; element specifies a line break between words. For example:
</p>
<pre>
    &lt;P&gt; Pease porridge hot&lt;BR&gt;
    Pease porridge cold&lt;BR&gt;
    Pease porridge in the pot&lt;BR&gt;
    Nine days old.
</pre>
<?DTD2HTML br* >
<?DTD2HTML br*sdapref >

<?DTD2HTML cite >
<P>
The &lt;CITE&gt; element is used to indicate the title of a book or
other citation.
</p>
<?DTD2HTML cite* >
<?DTD2HTML cite*sdaform >

<?DTD2HTML code >
<p>
The &lt;CODE&gt; element indicates an example of code, typically
rendered in a mono-spaced font. The &lt;CODE&gt; element is intended for
short words or phrases of code; the
<a href="pre.html">&lt;PRE&gt;</a> block structuring
element is more appropriate
for multiple-line listings. For example:
</p>
<pre>
    &lt;p&gt;The expression &lt;code&gt;x += 1&lt;/code&gt;
    is short for &lt;code&gt;x = x + 1&lt;/code&gt;.
</pre>
<?DTD2HTML code* >
<P>
<?DTD2HTML code*sdaform >
<P>
<?DTD2HTML dd >
<P>
Definition text of a term in a <A HREF="dl.html">definition list</a>.
</P>
<?DTD2HTML dd* >
<?DTD2HTML dd*sdaform >

<?DTD2HTML dfn >
<P>
The defining instance of a term.
</P>
<?DTD2HTML dir >
<p>
The &lt;DIR&gt; element is similar to the
<a href="ul.html">&lt;UL&gt;</a> element. It represents a
list of short items, typically up to 20 characters each. Items in a
directory list may be arranged in columns, typically 24 characters
wide.
</p>
<p>
The content of a &lt;DIR&gt; element is a sequence of &lt;LI&gt; elements.
Nested block elements are not allowed in the content of &lt;DIR&gt;
elements. For example:
</p>
<pre>
    &lt;DIR&gt;
    &lt;LI&gt;A-H&lt;LI&gt;I-M
    &lt;LI&gt;M-R&lt;LI&gt;S-Z
    &lt;/DIR&gt;
</pre>
<?DTD2HTML dir* >
<P>
<?DTD2HTML dir*compact >
Suggests that a compact rendering be used.
<P>
<?DTD2HTML dir*sdaform >
<P>
<?DTD2HTML dir*sdapref >
<P>
<?DTD2HTML dl >
<p>
A definition list is a list of terms and corresponding definitions.
</p>
<p>
The content of a &lt;DL&gt; element is a sequence of &lt;DT&gt;
elements and/or &lt;DD&gt; elements, usually in pairs. Multiple
&lt;DT&gt; may be paired with a single &lt;DD&gt; element. Documents
should not contain multiple consecutive &lt;DD&gt; elements.
</p>
<p>
Example of use:
</p>
<pre>
    &lt;DL&gt;
    &lt;DT&gt;Term&lt;DD&gt;This is the definition of the first term.
    &lt;DT&gt;Term&lt;DD&gt;This is the definition of the second term.
    &lt;/DL&gt;
</pre>
<?DTD2HTML dl* >
<?DTD2HTML dl*compact >
<P>
The optional COMPACT attribute suggests that a compact rendering be
used, because the list items are small and/or the entire list is
large.
</P>
<?DTD2HTML dl*sdaform >
<?DTD2HTML dl*sdapref >

<?DTD2HTML dt >
<P>
Definition term in a <A HREF="dl.html">definition list</a>.
</P>
<?DTD2HTML dt* >
<?DTD2HTML dt*sdaform >

<?DTD2HTML em >
<p>
The &lt;EM&gt; element indicates an emphasized phrase.
</p>
<?DTD2HTML em* >
<?DTD2HTML em*sdaform >

<?DTD2HTML expires >
The date after which the document should be
considered invalid. Semantics as in the HTTP
specification.
<P>
<?DTD2HTML form >
<p>
A form is a template for a form data set and an associated
method and action URI. A form data set is a sequence of
name/value pair fields. The names are specified on the NAME
attributes of form input elements, and the values are given
initial values by various forms of markup and edited by the
user. The resulting form data set is used to access an
information service as a function of the action and method.
</p>
<p>
Forms elements can be mixed in with document structuring
elements. For example, a
<a href="pre.html">&lt;PRE&gt;</a> element may contain a &lt;FORM&gt;
element, or a &lt;FORM&gt; element may contain lists which contain
<a href="input.html">&lt;INPUT&gt;</a> elements.
This gives considerable flexibility in
designing the layout of forms.
</p>
<?DTD2HTML form* >
<P>
<?DTD2HTML form*action >
<P>
Specifies the action URI for the form. The action URI of
a form defaults to the base URI of the document.
</P>
<?DTD2HTML form*enctype >
<p>
Specifies the media type used to encode the name/value
pairs for transport, in case the protocol does not
itself impose a format.
</p>
<p>
The default encoding for all forms is 'application/x-www-form-urlencoded'.
A form data set is represented in this media type as follows:
<ol>
<li>
The form field names and values are escaped: space
characters are replaced by `+', and then reserved characters
are escaped as per URL; that is, non-alphanumeric
characters are replaced by `%HH', a percent sign and two
hexadecimal digits representing the ASCII code of the
character. Line breaks, as in multi-line text field values,
are represented as CR LF pairs, i.e. `%0D%0A'.</li>
<li>
The fields are listed in the order they appear in the
document with the name separated from the value by `=' and
the pairs separated from each other by `&amp;'. Fields with null
values may be omitted. In particular, unselected radio
buttons and checkboxes should not appear in the encoded
data, but hidden fields with VALUE attributes present
should.
<dl>
<dt>NOTE</dt>
<dd>
The URI from a query form submission can be
used in a normal anchor style hyperlink.
Unfortunately, the use of the `&amp;' character to
separate form fields interacts with its use in SGML
attribute values as an entity reference delimiter.
For example, the URI `http://host/?x=1&amp;y=2' must be
written `&lt;a href="http://host/?x=1&amp;#38;y=2"&gt;' or `&lt;a
href="http://host/?x=1&amp;amp;y=2"&gt;'.
<p>
HTTP server implementors, and in particular, CGI
implementors are encouraged to support the use of
`;' in place of `&' to save users the trouble of
escaping `&' characters this way.
</p>
</dd>
</dl></li>
</ol>
</p>
<?DTD2HTML form*method >
<p>
Selects a method of accessing the action URI. The set of
applicable methods is a function of the scheme of the
action URI of the form.
</p>
<?DTD2HTML form*sdapref >
<?DTD2HTML form*sdasuff >

<?DTD2HTML h1 >
<?DTD2HTML h1* >
<?DTD2HTML h1*sdaform >
<?DTD2HTML h2 >
<?DTD2HTML h2* >
<?DTD2HTML h2*sdaform >
<?DTD2HTML h3 >
<?DTD2HTML h3* >
<?DTD2HTML h3*sdaform >
<?DTD2HTML h4 >
<?DTD2HTML h4* >
<?DTD2HTML h4*sdaform >
<?DTD2HTML h5 >
<?DTD2HTML h5* >
<?DTD2HTML h5*sdaform >
<?DTD2HTML h6 >
<?DTD2HTML h6* >
<?DTD2HTML h6*sdaform >

<?DTD2HTML head >
<p>
The head of an HTML document is an unordered collection of
information about the document. For example:
</p>
<pre>
    &lt;!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN"&gt;
    &lt;HEAD&gt;
    &lt;TITLE&gt;Introduction to HTML&lt;/TITLE&gt;
    &lt;/HEAD&gt;
    ...
</pre>
<?DTD2HTML head* >
<?DTD2HTML head*sdapref >

<?DTD2HTML hr >
<p>
The &lt;HR&gt; element is a divider between sections of text; typically a
full width horizontal rule or equivalent graphic.  For example:
</p>
<pre>
    &lt;HR&gt;
    &lt;ADDRESS&gt;February 8, 1995, CERN&lt;ADDRESS&gt;
    &lt;BODY&gt;
</pre>
<?DTD2HTML hr* >
<?DTD2HTML hr*sdapref >

<?DTD2HTML html >
<p>
The HTML document element consists of a 
<a href="head.html">head</a> and a
<a href="body.html">body</a>,
much like a
memo or a mail message. The head contains the title and optional
elements. The body is a text flow consisting of paragraphs, lists,
and other elements.
</p>
<?DTD2HTML html* >
<?DTD2HTML html*sdaform >
<?DTD2HTML html*version >
<?DTD2HTML html*sdapref >

<?DTD2HTML i >
<P>
The &lt;I&gt; element indicates italic text. Where italic typography is
unavailable, an alternative representation may be used.
</P>
<?DTD2HTML i* >
<?DTD2HTML i*sdaform >

<?DTD2HTML img >
<p>
The &lt;IMG&gt; element refers to an image or icon via a hyperlink.
</p>
<p>
HTML user agents may process the value of the ALT attribute as an
alternative to processing the image resource indicated by the SRC
attribute.
</p>
<dl>
<dt>NOTE</dt>
<dd>Some HTML user agents can process graphics linked via
anchors, but not &lt;IMG&gt; graphics. If a graphic is essential, it
should be referenced from an
<a href="a.html">&lt;A&gt;</a> element rather than an &lt;IMG&gt;
element. If the graphic is not essential, then the &lt;IMG&gt; element
is appropriate.</dd>
</dl>
<p>
Examples of use:
<pre>
    &lt;IMG SRC="triangle.xbm" ALT="Warning:"&gt; Be sure
    to read these instructions.

    &lt;a href="http://machine/htbin/imagemap/sample"&gt;
    &lt;IMG SRC="sample.xbm" ISMAP&gt;
    &lt;/a&gt;
</pre>
<?DTD2HTML img* >
<?DTD2HTML img*align >
<P>
Alignment of the image with respect to the text
baseline.
</p>
<ul>
<li>`TOP' specifies that the top of the image aligns
with the tallest item on the line containing the
image.</li>
<li>`MIDDLE' specifies that the center of the image
aligns with the baseline of the line containing the
image.</li>
<li>`BOTTOM' specifies that the bottom of the image
aligns with the baseline of the line containing the
image.</li>
</ul>
<?DTD2HTML img*alt >
<p>
Text to use in place of the referenced image resource,
for example due to processing constraints or user
preference.
</p>
<?DTD2HTML img*ismap >
<P>
Indicates an image map.
If the ISMAP attribute is present on an &lt;IMG&gt; element, the &lt;IMG&gt;
element must be contained in an
<a href="a.html">&lt;A&gt;</a> element with an HREF present.
This construct represents a set of hyperlinks. The user can choose
from the set by choosing a pixel of the image. The user agent
computes the head URI by appending `?' and the x and y coordinates of
the pixel to the URI given in the &lt;A&gt; element.  For example, if a
document contains:
</p>
<pre>
   &lt;!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN"&gt;
   &lt;head&gt;&lt;title&gt;ImageMap Example&lt;/title&gt;
   &lt;BASE HREF="http://host/index"&gt;&lt;/head&gt;
   &lt;body&gt;
   &lt;p&gt; Choose any of these icons:&lt;br&gt;
   &lt;a href="/cgi-bin/imagemap"&gt;&lt;img ismap src="icons.gif"&gt;&lt;/a&gt;
</pre>
<p>
and the user chooses the upper-leftmost pixel, the chosen
hyperlink is the one with the URI `http://host/cgi-bin/imagemap?0,0'.
</P>
<?DTD2HTML img*sdapref >
<?DTD2HTML img*src >
<p>
Specifies the URI of the image resource.
</p>

<?DTD2HTML input >
<p>
The &lt;INPUT&gt; element represents a field for user input. The TYPE
attribute discriminates between several variations of fields.
</p>
<?DTD2HTML input* >
<?DTD2HTML input*align >
<p>
Valid for `TYPE=IMAGE'.  Behaves the same as the
<a href="img.attr.html#align">ALIGN</a> attribute for the
<a href="img.html">&lt;IMG&gt;</a> element.
</p>
<?DTD2HTML input*checked >
<P>
Indicates that the initial state is on for CHECKBOX or RADIO input
types.
</p>
<?DTD2HTML input*maxlength >
<p>
Constrains the number of characters that can be entered
into a text input field. If the value of MAXLENGTH is
greater the the value of the SIZE attribute, the field
should scroll appropriately. The default number of
characters is unlimited.
</p>
<?DTD2HTML input*name >
<p>
Name for the form field corresponding to this element.
</p>
<?DTD2HTML input*sdapref >
<?DTD2HTML input*size >
<p>
Specifies the amount of display space allocated to this
input field according to its type. The default depends
on the user agent.
</p>
<?DTD2HTML input*src >
<p>
Valid for `TYPE=IMAGE'.  Attribute the same as the
<a href="img.attr.html#src">SRC</a> attribute for the
<a href="img.html">&lt;IMG&gt;</a> element.
</p>
<?DTD2HTML input*type >
Defines the type of data the field accepts.  Defaults to free text. Several
types of fields can be defined with the type attribute:
<dl>
<dt>CHECKBOX
<dd>
Represents a boolean choice.
A set of such elements with the same name represents an n-of-many
choice field.
<p>Example:
</p>
<pre>
  &lt;p&gt;What flavors do you like?
  &lt;input type=checkbox name=flavor value=vanilla&gt;Vanilla&lt;br&gt;
  &lt;input type=checkbox name=flavor value=strawberry&gt;Strawberry&lt;br&gt;
  &lt;input type=checkbox name=flavor value=chocolate checked&gt;Chocolate&lt;br&gt;
</pre>

<dt>HIDDEN
<dd>
Represents a hidden field. The
user does not interact with this field; instead, the VALUE attribute
specifies the value of the field. The NAME and VALUE attributes are
required.
<p>
Example:
</p>
<pre>
    &lt;input type=hidden name=context value="l2k3j4l2k3j4l2k3j4lk23"&gt;
</pre>

<dt>IMAGE
<dd>
Specifies an image resource to
display, and allows input of two form fields: the x and y coordinate
of a pixel chosen from the image. The names of the fields are the
name of the field with `.x' and `.y' appended.  `TYPE=IMAGE' implies
`TYPE=SUBMIT' processing; that is, when a pixel is chosen, the form
as a whole is submitted.
<p>Example</p>
<pre>
    &lt;p&gt;Choose a point on the map:
    &lt;input type=image name=point src="map.gif"&gt;
</pre>

<dt>PASSWORD
<dd>
A text field where the value is obscured as it is entered.
<p>Example:</p>
<pre>
    &lt;p&gt;Name: &lt;input name=login&gt; Password: &lt;input type=password name=passwd&gt;
</pre>

<dt>RADIO
<dd>
Represents a boolean choice. A
set of such elements with the same name represents a 1-of-many choice
field.
<p>Example:</p>
<pre>
    &lt;p&gt;Which is your favorite?
    &lt;input type=radio name=flavor value=vanilla&gt;Vanilla&lt;br&gt;
    &lt;input type=radio name=flavor value=strawberry&gt;Strawberry&lt;br&gt;
    &lt;input type=radio name=flavor value=chocolate&gt;Chocolate&lt;br&gt;
</pre>

<dt>RESET
<dd>
Represents an input option,
typically a button, that instructs the user agent to reset the form's
fields to their initial states. The VALUE attribute, if present,
indicates a label for the input (button).

<dt>SUBMIT
<dd>
Represents an input option,
typically a button, that instructs the user agent to submit the form.

<dt>TEXT
<dd>
Indicates a single line text entry field.
<p>Example:</p>
<pre>
&lt;p&gt;Street Address: &lt;input name=street&gt;&lt;br&gt;
Postal City code: &lt;input name=city size=16 maxlength=16&gt;&lt;br&gt;
Zip Code: &lt;input name=zip size=10 maxlength=10 value="99999-9999"&gt;&lt;br&gt;
</pre>
</dl>

<?DTD2HTML input*value >
<P>
The initial value of the field, or the value
when checked for checkboxes and radio
buttons.
</P>
<?DTD2HTML input*disabled >
<P>
When present indicates that this field is
temporarily disabled. Browsers should show
this by "greying it" out in some manner.
</P>
<?DTD2HTML input*error >
<P>
When present indicates that the field's
initial value is in error in some way, e.g.
because it is inconsistent with the values of
other fields. Servers should include an
explanatory error message with the form's
text.
</P>

<?DTD2HTML isindex >
<p>
The &lt;ISINDEX&gt; element indicates that the user agent should allow the
user to search an index by giving keywords.
</p>
<?DTD2HTML isindex* >
<?DTD2HTML isindex*sdapref >

<?DTD2HTML kbd >
<p>
The &lt;KBD&gt; element indicates text typed by a user, typically
rendered in a mono-spaced font. This is commonly used in
instruction manuals. For example:
</p>
<p>
Enter <kbd>FIND IT</kbd> to search the database.
</p>
<?DTD2HTML kbd* >
<?DTD2HTML kbd*sdaform >

<?DTD2HTML key >

<?DTD2HTML li >
<P>
A list item.
</P>
<?DTD2HTML li* >
<?DTD2HTML li*sdaform >

<?DTD2HTML link >
<p>
The &lt;LINK&gt; element represents a hyperlink.  Any
number of LINK elements may occur in the
<a href="head.html">&lt;HEAD&gt;</a>
element of an HTML document. It has the same attributes as the
<a href="a.html">&lt;A&gt;</a>
 element.
</p>
<p>
The &lt;LINK&gt; element is typically used to indicate authorship, related
indexes and glossaries, older or more recent versions, document
hierarchy, associated resources such as style sheets, etc.
</p>
<?DTD2HTML link* >
<P>
See <a href="a.attr.html">anchors attributes</a> for a description
</P>of link's attributes.
<?DTD2HTML link*href >
<?DTD2HTML link*methods >
<?DTD2HTML link*rel >
<?DTD2HTML link*rev >
<?DTD2HTML link*sdapref >
<?DTD2HTML link*title >
<?DTD2HTML link*urn >
<?DTD2HTML link*name >

<?DTD2HTML listing >
<p>
See <a href="xmp.html">&lt;XMP&gt;</a>.
</P>
<?DTD2HTML listing* >
<?DTD2HTML listing*sdaform >
<?DTD2HTML listing*sdapref >

<?DTD2HTML menu >
<p>
The &lt;MENU&gt; element is a list of items with typically one line per
item. The menu list style is typically more compact than the style of
an <a href="ul.html">unordered list</a>.
</p>
<p>
The content of a &lt;MENU&gt; element is a sequence of &lt;LI&gt; elements.
Nested block elements are not allowed in the content of &lt;MENU&gt;
elements. For example:
</p>
<pre>
    &lt;MENU&gt;
    &lt;LI&gt;First item in the list.
    &lt;LI&gt;Second item in the list.
    &lt;LI&gt;Third item in the list.
    &lt;/MENU&gt;
</pre>
<?DTD2HTML menu* >
<?DTD2HTML menu*compact >
<P>
Suggests that a compact rendering be used.
</P>
<?DTD2HTML menu*sdaform >
<?DTD2HTML menu*sdapref >

<?DTD2HTML meta >
<p>
The &lt;META&gt; element is an extensible container for use in identifying
specialized document meta-information.  Meta-information has two main
functions:
</p>
<ul>
<li>to provide a means to discover that the data set exists
and how it might be obtained or accessed; and</li>
<li>to document the content, quality, and features of a data
set, indicating its fitness for use.</li>
</ul>
<p>
Each &lt;META&gt; element specifies a name/value pair. If multiple META
elements are provided with the same name, their combined contents--
concatenated as a comma-separated list--is the value associated with
that name.
</p>
<dl>
<dt>NOTE</dt>
<dd>The &lt;META&gt; element should not be used where a
specific element, such as
<a href="title.html">&lt;TITLE&gt;</a>, would be more
appropriate. Rather than a &lt;META&gt; element with a URI as
the value of the CONTENT attribute, use a
<a href="link.html">&lt;LINK&gt;</a>
element.
</dd>
</dl>
<p>
HTTP servers may read the content of the document
<a href="head.html">&lt;HEAD&gt;</a> to generate
header fields corresponding to any elements defining a value for the
attribute HTTP-EQUIV.
</p>
<dl>
<dt>NOTE</dt>
<dd>
The method by which the server extracts document
meta-information is unspecified and not mandatory. The
&lt;META&gt; element only provides an extensible mechanism for
identifying and embedding document meta-information --
how it may be used is up to the individual server
implementation and the HTML user agent.
</dd>
</dl>
<p><strong>Examples</strong></p>
<p>
If the document contains:
</p>
<pre>
    &lt;META HTTP-EQUIV="Expires"
          CONTENT="Tue, 04 Dec 1993 21:29:02 GMT"&gt;
    &lt;meta http-equiv="Keywords" CONTENT="Fred"&gt;
    &lt;META HTTP-EQUIV="Reply-to"
          content="fielding@ics.uci.edu (Roy Fielding)"&gt;
    &lt;Meta Http-equiv="Keywords" CONTENT="Barney"&gt;
</pre>
<p>
then the server may include the following header fields:
</p>
<pre>
    Expires: Tue, 04 Dec 1993 21:29:02 GMT
    Keywords: Fred, Barney
    Reply-to: fielding@ics.uci.edu (Roy Fielding)
</pre>
<p>
as part of the HTTP response to a `GET' or `HEAD' request for
that document.
</p>
<p>
An HTTP server must not use the &lt;META&gt; element to form an HTTP
response header unless the HTTP-EQUIV attribute is present.
</p>
<p>
An HTTP server may disregard any &lt;META&gt; elements that specify
information controlled by the HTTP server, for example `Server',
`Date', and `Last-modified'.
</p>
<?DTD2HTML meta* >
<?DTD2HTML meta*content >
<P>
Specifies the name of the name/value pair. If not
present, HTTP-EQUIV gives the name.
</p>
<?DTD2HTML meta*http-equiv >
<P>
Binds the element to an HTTP header field. An HTTP
server may use this information to process the document.
In particular, it may include a header field in the
responses to requests for this document: the header name
is taken from the HTTP-EQUIV attribute value, and the
header value is taken from the value of the CONTENT
attribute. HTTP header names are not case sensitive.
</p>
<?DTD2HTML meta*name >
<P>
Specifies the name of the name/value pair. If not
present, HTTP-EQUIV gives the name.
</p>
<?DTD2HTML meta*sdapref >

<?DTD2HTML nextid >
<p>
The &lt;NEXTID&gt; element is included for historical reasons only.  HTML
documents should not contain &lt;NEXTID&gt; elements.
</p>
<p>
The &lt;NEXTID&gt; element gives a hint for the name to use for a new
<a href="a.html">&lt;A&gt;</a>
element when editing an HTML document. It should be distinct from all
NAME attribute values on &lt;A&gt; elements. For example:
</p>
<pre>
   &lt;NEXTID N=Z27&gt;
</pre>
<?DTD2HTML nextid* >
<?DTD2HTML nextid*n >
<?DTD2HTML nextid*sdapref >

<?DTD2HTML ol >
<P>
The &lt;OL&gt; element represents an ordered list of items, sorted by
sequence or order of importance. It is typically rendered as a
numbered list.
</p>
<p>
The content of a &lt;OL&gt; element is a sequence of &lt;LI&gt; elements.  For
example:
</p>
<pre>
    &lt;OL&gt;
    &lt;LI&gt;Click the Web button to open URI window.
    &lt;LI&gt;Enter the URI number in the text field of the Open URI
    window. The Web document you specified is displayed.
      &lt;ol&gt;
       &lt;li&gt;substep 1
       &lt;li&gt;substep 2
      &lt;/ol&gt;
    &lt;LI&gt;Click highlighted text to move from one link to another.
    &lt;/OL&gt;
</pre>
<?DTD2HTML ol* >
<?DTD2HTML ol*compact >
<P>
Suggests that a compact rendering be used.
</p>
<?DTD2HTML ol*sdaform >
<?DTD2HTML ol*sdapref >

<?DTD2HTML option >
<p>
The Option element can only occur within a
<a href="select.html">Select</a> element. It
represents one choice.
</P>
<?DTD2HTML option* >
<?DTD2HTML option*sdaform >
<?DTD2HTML option*sdapref >
<?DTD2HTML option*selected >
<P>
Indicates that this option is initially selected.
</P>
<?DTD2HTML option*value >
<P>
Indicates the value to be returned if this option is
chosen. The field value defaults to the content of the
&lt;OPTION&gt; element.
</P>
<?DTD2HTML option*disabled >
<P>
When present indicates that this option is
temporarily disabled.
</P>

<?DTD2HTML p >
<p>
The &lt;P&gt; element indicates a paragraph.
</p>
<p>
Example of use:
</p>
<pre>
    &lt;H1&gt;This Heading Precedes the Paragraph&lt;/H1&gt;
    &lt;P&gt;This is the text of the first paragraph.
    &lt;P&gt;This is the text of the second paragraph. Although you do not
    need to start paragraphs on new lines, maintaining this
    convention facilitates document maintenance.&lt;/P&gt;
    &lt;P&gt;This is the text of a third paragraph.&lt;/P&gt;
</pre>
<?DTD2HTML p* >
<?DTD2HTML p*sdaform >

<?DTD2HTML plaintext >
<P>
&lt;PLAINTEXT&gt; is deprecated, and its use is strongly discouraged.
All characters after the &lt;PLAINTEXT&gt; start-tag are data.  Recognition
of any HTML markup is disabled.
</P>
<?DTD2HTML plaintext* >
<?DTD2HTML plaintext*sdaform >

<?DTD2HTML pre >
<P>
The &lt;PRE&gt; element represents a character cell block of text and is
suitable for text that has been formatted for a monospaced font.
</p>
<p>
The &lt;PRE&gt; tag may be used with the optional WIDTH attribute. The
WIDTH attribute specifies the maximum number of characters for a line
and allows the HTML user agent to select a suitable font and
indentation.
</p>
<p>
Within preformatted text:
</p>
<ul>
<li>Line breaks within the text are rendered as a move to the
beginning of the next line.
<dl>
    <dt>NOTE</dt>
    <dd>References to the "beginning of a new line"
    do not imply that the renderer is forbidden from
    using a constant left indent for rendering
    preformatted text. The left indent may be
    constrained by the width required.</dd>
</dl>
</li>
<li>Anchor elements and phrase markup may be used.
<dl>
    <dt>NOTE</dt>
    <dd>Constraints on the processing of &lt;PRE&gt;
    content may limit or prevent the ability of the HTML
    user agent to faithfully render phrase markup.</dd>
</dl>
</li>
<li>Elements that define paragraph formatting (headings,
address, etc.) must not be used.
<dl>
    <dt>NOTE</dt>
    <dd>Some historical documents contain &lt;P&gt; tags in
    &lt;PRE&gt; elements. User agents are encouraged to treat
    this as a line break. A &lt;P&gt; tag followed by a
    newline character should produce only one line
    break, not a line break plus a blank line.</dd>
</dl>
</li>
<li>The horizontal tab character (code position 9 in the HTML
document character set) must be interpreted as the smallest
positive nonzero number of spaces which will leave the
number of characters so far on the line as a multiple of 8.
Documents should not contain tab characters, as they are not
supported consistently.
</li>
</ul>
<p>
Example of use:
</p>
<pre>
&lt;PRE&gt;
Line 1.
       Line 2 is to the right of line 1.     &lt;a href="abc"&gt;abc&lt;/a&gt;
       Line 3 aligns with line 2.            &lt;a href="def"&gt;def&lt;/a&gt;
&lt;/PRE&gt;
</pre>
<?DTD2HTML pre* >
<?DTD2HTML pre*sdaform >
<?DTD2HTML pre*width >
<P>
This attribute gives the maximum number of characters
which will occur on a line. It allows the presentation system
to select a suitable font and indentation. Where the WIDTH
attribute is not recognized, it is recommended that a width of
80 be assumed. Where WIDTH is supported, it is
recommended that at least widths of 40, 80 and 132
characters be presented optimally, with other widths being
rounded up.
</P>
<?DTD2HTML pre*sdapref >

<?DTD2HTML samp >
<p>
The &lt;SAMP&gt; element indicates a sequence of literal characters,
typically rendered in a mono-spaced font. For example:
</p>
<p>
The only word containing the letters <samp>mt</samp> is dreamt.
</p>
<?DTD2HTML samp* >
<?DTD2HTML samp*sdaform >

<?DTD2HTML select >
<p>
The &lt;SELECT&gt; element constrains the form field to an enumerated list
of values. The values are given in &lt;OPTION&gt; elements.
</p>
<p>Example:</p>
<pre>
    &lt;SELECT NAME="flavor"&gt;
    &lt;OPTION&gt;Vanilla
    &lt;OPTION&gt;Strawberry
    &lt;OPTION value="RumRasin"&gt;Rum and Raisin
    &lt;OPTION selected&gt;Peach and Orange
    &lt;/SELECT&gt;
</pre>
<p>
The initial state has the first option selected, unless a SELECTED
attribute is present on any of the &lt;OPTION&gt; elements.
</p>
<?DTD2HTML select* >
<?DTD2HTML select*multiple >
<P>
Indicates that more than one option may be included in the value.
</p>
<?DTD2HTML select*name >
<P>
Specifies the name of the form field.
</p>
<?DTD2HTML select*sdaform >
<?DTD2HTML select*sdapref >
<?DTD2HTML select*size >
<P>
Specifies the number of visible items. Select fields of
size one are typically pop-down menus, whereas select
fields with size greater than one are typically lists.
</p>
<?DTD2HTML select*error >
<P>
The ERROR attribute can be used to indicate
that the initial selection is in error in
some way, e.g. because it is inconsistent
with the values of other fields.
</P>

<?DTD2HTML strike >
<P>
The STRIKE element indicates "strike out" text, as in a legal document.
This tag is not widely supported.
</P>

<?DTD2HTML strong >
<P>
The &lt;STRONG&gt; element indicates strong emphasis, typically rendered
in bold. For example:
</p>
<pre>
&lt;strong&gt;STOP&lt;/strong&gt;, or I'll say "&lt;strong&gt;STOP&lt;/strong&gt;" again!
</pre>
<?DTD2HTML strong* >
<?DTD2HTML strong*sdaform >

<?DTD2HTML textarea >
<p>
The &lt;TEXTAREA&gt; element represents a multi-line text field in a
<a href="form.html">Form</a>.
</p>
<p>Example:</p>
<pre>
    &lt;TEXTAREA NAME="address" ROWS=6 COLS=64&gt;
    HaL Computer Systems
    1315 Dell Avenue
    Campbell, California 95008
    &lt;/TEXTAREA&gt;
</pre>
<p>
The content of the &lt;TEXTAREA&gt; element is the field's initial value.
</p>
<?DTD2HTML textarea* >
<?DTD2HTML textarea*cols >
<P>
The number of visible columns to display for the text area, in characters.
</P>
<?DTD2HTML textarea*name >
<P>
Specifies the name of the form field.
</P>
<?DTD2HTML textarea*rows >
<P>
The number of visible rows to display for the text area, in characters.
</P>
<?DTD2HTML textarea*sdaform >
<?DTD2HTML textarea*sdapref >

<?DTD2HTML title >
<P>
Every HTML document must contain a &lt;TITLE&gt; element.
</p>
<p>
The title should identify the contents of the document in a global
context. A short title, such as "Introduction" may be meaningless out
of context. A title such as "Introduction to HTML Elements" is more
appropriate.
</p>
<dl>
<dt>NOTE</dt>
<dd>
The length of a title is not limited; however, long titles
may be truncated in some applications. To minimize this
possibility, titles should be fewer than 64 characters.
<p>
A user agent may display the title of a document in a history list or
as a label for the window displaying the document. This differs from
headings, which are typically displayed within the body text flow.
</p>
<?DTD2HTML title* >
<?DTD2HTML title*sdaform >

<?DTD2HTML tt >
<p>
The &lt;TT&gt; element indicates teletype (monospaced) text. Where a
teletype font is unavailable, an alternative representation may be
used.
</p>
<?DTD2HTML tt* >
<?DTD2HTML tt*sdaform >

<?DTD2HTML ul >
<P>
The &lt;UL&gt; represents a list of items -- typically rendered as a
bulleted list.
</p>
<p>
The content of a &lt;UL&gt; element is a sequence of &lt;LI&gt; elements.  For
example:
</p>
<pre>
    &lt;UL&gt;
    &lt;LI&gt;First list item
    &lt;LI&gt;Second list item
     &lt;p&gt;second paragraph of second item
    &lt;LI&gt;Third list item
    &lt;/UL&gt;
</pre>
<?DTD2HTML ul* >
<?DTD2HTML ul*compact >
<P>
Suggests that a compact rendering be used.
</p>
<?DTD2HTML ul*sdaform >
<?DTD2HTML ul*sdapref >

<?DTD2HTML var >
<p>
The &lt;VAR&gt; element indicates a placeholder variable, typically
rendered as italic. For example:
</p>
<pre>
  Type &lt;SAMP&gt;html-check &lt;VAR&gt;file&lt;/VAR&gt; | more&lt;/SAMP&gt;
  to check &lt;VAR&gt;file&lt;/VAR&gt; for markup errors.
</pre>
<?DTD2HTML var* >
<?DTD2HTML var*sdaform >

<?DTD2HTML xmp >
<p>
The &lt;XMP&gt; and &lt;LISTING&gt; elements are similar to the
<a href="pre.html">&lt;PRE&gt;</a> element,
but they have a different syntax. Their content is declared as CDATA,
which means that no markup except the end-tag open delimiter-in-context
is recognized.
</p>
<dl>
<dt>NOTE</dt>
<dd>In a previous draft of the HTML specification, the syntax
of &lt;XMP&gt; and &lt;LISTING&gt; elements allowed closing tags to be treated
as data characters, as long as the tag name was not &lt;XMP&gt; or
&lt;LISTING&gt;, respectively.</dd>
</dl>
<p>
Since CDATA declared content has a number of unfortunate interactions
with processing techniques and tends to be used and implemented
inconsistently, HTML documents should not contain &lt;XMP&gt;
nor &lt;LISTING&gt;
elements -- the 
<a href="pre.html">&lt;PRE&gt;</a>
tag is more expressive and more consistently
supported.
</p>
<p>
The &lt;LISTING&gt; element should be rendered so that at least 132
characters fit on a line. The &lt;XMP&gt; element should be rendered so
that at least 80 characters fit on a line but is otherwise identical
to the &lt;LISTING&gt; element.
</p>
<dl>
<dt>NOTE</dt>
<dd>In a previous draft, HTML included a &lt;PLAINTEXT&gt; element
that is similar to the &lt;LISTING&gt; element, except that there is no
closing tag: all characters after the &lt;PLAINTEXT&gt; start-tag are
data.</dd>
</dl>
<?DTD2HTML xmp* >
<?DTD2HTML xmp*sdaform >
<?DTD2HTML xmp*sdapref >
