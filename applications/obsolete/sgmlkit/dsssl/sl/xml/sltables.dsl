<!-- $Id$ -->

<!--
<docblock>
<title>XML Tables
<description>The Starlink General DTD uses the OASIS Exchange Table
Model subset of the CALS table model (see
<url>http://www.oasis-open.org/html/a503.htm</url> for discussion and
<url>http://www.oasis-open.org/html/publtext.htm</url> for public
texts).

<p>The Exchange Table Model can be customised.  The only such
customisations at present are: replace the optional TITLE element with
a required CAPTION; add implied ID and EXPORT attributes to the TABLE;
extend the table entry model to include phrase markup.


<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council

<codegroup id=code.tables>
<title>Tables
<description>I've aimed to support all of the <em/structure/ of this table
model below, but not necessarily to support all of the attributes at
first.
-->

<routine>
<description>All the flow-object constructors
<codebody>
(element table
  (with-mode copy-elements
    (process-children)))

(mode copy-elements
  (default
      (make element gi: (gi)
	    (process-children))))
