<?xml version="1.0"?>
<!DOCTYPE xsl:stylesheet PUBLIC "-//W3C//DTD XSLT 1.0//EN"
"/home/norman/s/src/sgml/w/sgml/dtd/xslt.dtd">

<!-- $Id$ -->

<xsl:stylesheet version="1.0">

  <xsl:param name="listprefix">unknown</xsl:param>
  <xsl:param name="listaddress">@astro.gla.ac.uk</xsl:param>

  <xsl:template match="/">
    <html>
      <head>
	<title>DTD Description</title>
      </head>
      <xsl:apply-templates select="/dtddescription/dtdsummary"/>
    </html>
  </xsl:template>

  <xsl:template match="dtdsummary">
    <body>
      <h1>DTD summary for DTD <xsl:value-of select="@top"/></h1>
      <p>Generated from DTD <code><xsl:value-of	select="@sysid"/></code></p>
      <p>This list includes all of the elements currently present in
	the DTD, plus a few which have not even made it to the DTD.  Not
	all the elements in the DTD have been implemented in
	down-converters.</p>
      <p>Each element has one of five statuses (asterisks indicate
	that there is some discussion on this element):</p>
      <!-- Note that the element lists here are formed by processing
      `../element' elements, and the `all-element' list is formed by
      processing `dtdelement' children -->
      <table border="1">
	<tr><th>Status</th><th>Meaning</th></tr>
	<tr><td>experimental</td><td>Proposed element, under
	    discussion.  May or may not be included in the DTD.
	    <p><xsl:apply-templates
	      select="../element[@status='experimental']" mode="elementtoc"/>
	    </p></td></tr>
	<tr><td>alpha</td><td>Proposed element.  This will at least
	    be in the DTD, but it may not be implemented in the
	    down-converters.
	    <p><xsl:apply-templates 
	      select="../element[@status='alpha']" mode="elementtoc"/>
	    </p></td></tr>
	<tr><td>beta</td><td>Candidate for stable status.  This will
	    certainly be in the DTD, and 
	    have at least some implementation in the
	    down-converters, but the DTD declaration, and the
	    element's semantics, may yet be subject to change.
	    <p><xsl:apply-templates
	      select="../element[@status='beta']" mode="elementtoc"/>
	    </p></td></tr>
	<tr><td>stable</td><td>Fully supported element.  Changes to
	    the DTD, at least up to the next major revision, will
	    retain this element.
	    <p><xsl:apply-templates
	      select="../element[@status='stable']" mode="elementtoc"/>
	    </p></td></tr>
	<tr><td>deprecated</td><td>Do not use.  This is still
	    supported in the DTD, but it will probably disappear
	    come the next major revision.
	    <p><xsl:apply-templates
	      select="../element[@status='deprecated']" mode="elementtoc"/>
	    </p></td></tr>
      </table>
      <p>All elements in DTD:
	<xsl:apply-templates select="dtdelement" mode="elementtoc"/>.
      </p>
      <xsl:apply-templates select="dtdelement"/>
    </body>
  </xsl:template>

  <xsl:template match="dtdelement">
    <hr />
    <h2><a><xsl:attribute name="name">
	  <xsl:value-of select="@gi"/>
	</xsl:attribute>Element &lt;<xsl:value-of select="@gi"/>&gt;</a></h2>
    <xsl:apply-templates select="id(@gi)"/>
    <xsl:apply-templates select="dtdparents"/>
    <xsl:apply-templates select="dtdcontent"/>
    <xsl:if test="dtdattribute">
      <h3>Attribute summary</h3>
      <table border="1">
	<tr><th>Name</th><th>type</th><th>default</th></tr>
	<xsl:apply-templates select="dtdattribute"/>
      </table>
    </xsl:if>
    <xsl:if test="id(@gi)/commentary">
      <xsl:apply-templates select="id(@gi)/commentary"/>
    </xsl:if>
  </xsl:template>

  <xsl:template match="dtdelement" mode="elementtoc">
    <a><xsl:attribute name="href">#<xsl:value-of select="@gi"/></xsl:attribute>
      <xsl:value-of select="@gi"/>
    </a>
    <xsl:if test="id(@gi)/commentary">
      <a><xsl:attribute name="href">#comm.<xsl:value-of select="@gi"/></xsl:attribute>*</a></xsl:if>
    <xsl:if test="position()!=last()">,      
    </xsl:if>
  </xsl:template>

  <xsl:template match="element" mode="elementtoc">
    <a><xsl:attribute name="href">#<xsl:value-of select="@gi"/></xsl:attribute>
      <xsl:value-of select="@gi"/>
    </a>
    <xsl:if test="commentary">
      <a><xsl:attribute name="href">#comm.<xsl:value-of select="@gi"/></xsl:attribute>*</a></xsl:if>
    <xsl:if test="position()!=last()">,      
    </xsl:if>
  </xsl:template>

  <xsl:template match="dtdparents">
    <h3>Parents</h3>
    <xsl:if test="@orphan">
      <p>No parents (<xsl:value-of select="@orphan"/>)</p>
    </xsl:if>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="dtdcontent">
    <h3>Content</h3>
    <xsl:apply-templates select="dtdcontentmodel"/>
    <!-- only display the content model -->
  </xsl:template>

  <xsl:template match="dtdcontentmodel">
    <p><xsl:apply-templates/></p>
  </xsl:template>

  <xsl:template match="dtdcontenttree">
    <p><pre><xsl:apply-templates/></pre></p>
  </xsl:template>

  <xsl:template match="dtdattribute">
    <tr>
      <td><xsl:value-of select="@name"/></td>
      <td><xsl:value-of select="@type"/></td>
      <td><xsl:value-of select="@default"/></td>
    </tr>
  </xsl:template>

  <xsl:template match="dtdelemref">
    <a><xsl:attribute name="href">#<xsl:value-of
    select="id(@id)/@gi"/></xsl:attribute><xsl:value-of select="id(@id)/@gi"/></a></xsl:template>

  <xsl:template match="element">
    <xsl:if test="@status">
      <p><em>Status: <xsl:value-of select="@status"/></em></p>
    </xsl:if>
    <xsl:apply-templates select="p"/>
    <xsl:apply-templates select="attribute"/>
  </xsl:template>

  <xsl:template match="p">
    <xsl:copy>
      <xsl:apply-templates select="node()"/>
    </xsl:copy>
  </xsl:template>

  <xsl:template match="attribute">
    <h3>Attribute: <xsl:value-of select="@name"/></h3>
    <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="commentary">
    <!-- Test if this is the first commentary sibling -->
    <xsl:if test="count(preceding-sibling::commentary)=0">
      <h3><a><xsl:attribute name="name">comm.<xsl:value-of select="../@gi"/></xsl:attribute>Commentary on <xsl:value-of select="../@gi"/></a></h3>
    </xsl:if>
    <table>
      <tr><td align="right"><strong>From:</strong></td>
	<td>
	  <xsl:apply-templates select="from"/>
	  <xsl:apply-templates select="email"/>
	</td>
      </tr>
      <tr><td align="right"><strong>Date:</strong></td>
	<td>
	  <xsl:apply-templates select="date"/>
	</td>
      </tr>
      <tr><td align="right"><strong>Subject:</strong></td>
	<td>
	  <xsl:apply-templates select="subject"/>
	</td>
      </tr>
    </table>
    <pre>
      <xsl:apply-templates select="body"/>
    </pre>
    <xsl:if test="count(following-sibling::commentary)=0 and
	    $listprefix != 'unknown'">
      <p><a><xsl:attribute name="href">mailto:<xsl:value-of
	select="$listprefix"/>+<xsl:value-of
        select="../@gi"/><xsl:value-of select="$listaddress"/></xsl:attribute>
	Add response</a></p>  
    </xsl:if>
  </xsl:template>

  <xsl:template match="elemref">
    <code>&lt;<a><xsl:attribute name="href">#<xsl:value-of
    select="@gi"/></xsl:attribute><xsl:value-of select="@gi"/></a>&gt;</code>
  </xsl:template>

  <xsl:template match="example">
    <p>Example:
      <blockquote><pre><xsl:apply-templates/></pre></blockquote>
    </p>
  </xsl:template>

  <xsl:template match="code">
    <xsl:copy><xsl:apply-templates/></xsl:copy>
  </xsl:template>

  <xsl:template match="em">
    <xsl:copy><xsl:apply-templates/></xsl:copy>
  </xsl:template>

  <xsl:template match="url">
    <a><xsl:attribute name="href"><xsl:apply-templates/></xsl:attribute>
      <xsl:apply-templates/></a>
  </xsl:template>

  <xsl:template match="webref">
    <a><xsl:attribute name="href"><xsl:value-of select="@url"/></xsl:attribute>
      <xsl:apply-templates/></a>
  </xsl:template>

  <xsl:template match="email">
    &lt;<xsl:apply-templates/>&gt;
  </xsl:template>

</xsl:stylesheet>
