<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:exslt="http://exslt.org/common"
		xmlns:out1="http://out1namespace"
		xmlns:tns="http://tnsnamespace"
		exclude-result-prefixes="out1 exslt">
  <xsl:output omit-xml-declaration="yes" indent="yes"/>


  <xsl:template match="/">
    <html>
      <head>
	<title>Starlink Software Collection Summary</title>
	<link href="starlinksummary.css" title="compact" rel="stylesheet" type="text/css"/>
      </head>

      <body>
	<h1> Starlink Software Collection Summary</h1>

	<p> The <a
	href="http://starlink.eao.hawaii.edu/starlink">Starlink
	Software Collection</a> provides access to a large number of
	Astronomical data reduction, analysis, and visualisation
	tools. Its history is in the UK Starlink Project, which
	provided software and computing support for UK astronomy
	departments. In 2005 the project was shut down, but the
	software package continued to be developed by the Join
	Astronomy Centre. From March 2015, it has been maintained by
	by the <a href="http://www.eaobservatory.org">East Asian
	Observatory</a>, in support of the <a
	href="http://www.eaobservatory.org/jcmt">JCMT</a> and <a
	href="http://www.ukirt.hawaii.edu">UKIRT</a> telescopes.</p>


        <p>The open source code repository is hosted on <a
        href="http://github.com/Starlink/starlink">Github</a>, and
        contributions from the community are welcome. There is also a
        <a
        href="http://www.jiscmail.ac.uk/archives/starlink.html">user
        support mailing list</a>, and a <a
        href="http://www.jiscmail.ac.uk/archives/stardev.html">developer
        mailing list</a>. Efforts in support of Starlink software by
        EAO personnel are predominantly focused on the packages relevant for
        operation and use of the JCMT and UKIRT telescopes, but
        community support and development is welcomed for all parts of
        the collection. The tables below have a column 'Supported'
        indicating whether or not they are considered part of the
        'core' set supported by EAO (Y='yes', and N='no').</p>

        <p>This page provides a summary description of all
	the <a href="#apps">applications</a>, <a href="#docs">extra
	documents</a>, and <a href="#libs">libraries</a> provided in
	the main Starlink Software Collection. It does not, however,
	list any of the Perl packages shipped with the software
	(including those with command line interfaces), the ORAC-DR
	and Picard pipelines or the Starjava applications.</p>

	<h2 id="apps"> Starlink Applications </h2>

	<p> This is the full set of applications in this Starlink
	build, along with links to their documentation and a brief
	summary.</p>

	<xsl:call-template name="create-table">
	  <xsl:with-param name="typeofpackage">
	    <xsl:value-of select="'applications'"/>
	  </xsl:with-param>
	  <xsl:with-param name="withabs">
	    <xsl:value-of select="'true'"/>
	  </xsl:with-param>
	</xsl:call-template>

	<h2 id="docs"> Additional Documents </h2>

	<p>Starlink ships several additional documents that are not
	part of a package; these include cookbooks for various
	specialised task.</p>

	<xsl:call-template name="create-table">
	  <xsl:with-param name="typeofpackage">
	    <xsl:value-of select="'docs'"/>
	  </xsl:with-param>
	  <xsl:with-param name="withabs">
	    <xsl:value-of select="'true'"/>
	  </xsl:with-param>

	</xsl:call-template>

	<h2 id="libs"> Starlink Libraries </h2>

	<p> This lists the various libraries that are part of this
	Starlink build.</p>


	<xsl:call-template name="create-table">
	  <xsl:with-param name="typeofpackage">
	    <xsl:value-of select="'libraries'"/>
	  </xsl:with-param>
	  <xsl:with-param name="withabs">
	    <xsl:value-of select="'true'"/>
	  </xsl:with-param>

	</xsl:call-template>

        <h2 id="extlibs"> External libraries (from /libext)</h2>
        <p> This lists the various libraries from /libext
        in this Starlink build.</p>
        <xsl:call-template name="create-table">
	  <xsl:with-param name="typeofpackage">
	    <xsl:value-of select="'libext'"/>
	  </xsl:with-param>
	  <xsl:with-param name="withabs">
	    <xsl:value-of select="'true'"/>
	  </xsl:with-param>
        </xsl:call-template>

        <h2 id="thirdparty"> External programs libraries (from /thirdparty)</h2>
        <p> List of installed thirdparty libraries from /thirdparty in
        this Starlink build.</p>
        <xsl:call-template name="create-table">
	  <xsl:with-param name="typeofpackage">
	    <xsl:value-of select="'thirdparty'"/>
	  </xsl:with-param>
	  <xsl:with-param name="withabs">
	    <xsl:value-of select="'true'"/>
	  </xsl:with-param>
        </xsl:call-template>

      </body>
    </html>
  </xsl:template>

  <xsl:template name="output-tokens">
    <xsl:param name="list"/>
    <xsl:param name="delimiter"/>
    <xsl:variable name="newlist">
      <xsl:choose>
	<xsl:when test="contains($list, $delimiter)">
	  <xsl:value-of select="$list"/>
	</xsl:when>
	<xsl:otherwise>
	  <xsl:value-of select="concat($list, $delimiter)"/>
	</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="first" select="substring-before($newlist, $delimiter)"/>
    <xsl:variable name="remaining"
		  select="substring-after($newlist, $delimiter)"/>
    <xsl:variable name="count" select="position()"/>
    <num>
	<xsl:element name="a">
	  <xsl:attribute name="href">
	    <xsl:value-of select="concat('',$first,'.htx/',$first, '.html')"/>
	  </xsl:attribute>
	  <xsl:value-of select="translate(translate($first, translate($first, 'suncgp', ''), ''), 'suncgp', 'SUNCGP')"/>
	  <xsl:value-of select="'/'"/>
	  <xsl:value-of select="translate($first, translate($first, '0123456789', ''), '')" />
	</xsl:element>
	<xsl:element name="br"/>
    </num>
    <xsl:if test="$remaining">
      <xsl:call-template name="output-tokens">
	<xsl:with-param name="list" select="$remaining"/>
	<xsl:with-param name="delimiter">
	  <xsl:value-of select="$delimiter"/>
	</xsl:with-param>
      </xsl:call-template>
    </xsl:if>
  </xsl:template>

  <xsl:template name="create-table">
    <xsl:param name="typeofpackage"/>
    <xsl:param name="withabs"/>
    <xsl:element name="table">
      <xsl:element name="tr">
	<xsl:element name="th">
	  <xsl:value-of select="'Name'"/>
	</xsl:element>
	<xsl:element name="th">
	  <xsl:value-of select="'Description'"/>
	</xsl:element>
	<xsl:element name="th">
	  <xsl:value-of select="'Documents'"/>
	</xsl:element>
	<xsl:if test="$withabs='true'">
	  <xsl:element name="th">
	    <xsl:value-of select="'Abstract'"/>
	  </xsl:element>
	</xsl:if>
	<xsl:element name="th">
	  <xsl:value-of select="'Supported'"/>
	</xsl:element>
      </xsl:element>

      <xsl:for-each select="componentset/component[not(@status='obsolete')]">
	<xsl:choose>
	  <xsl:when test="starts-with(path, concat($typeofpackage, '/'))">
	    <xsl:element name="tr">

	      <xsl:element name="td">
		<xsl:value-of select="@id"/>
	      </xsl:element>

	      <xsl:element name="td">
		<xsl:value-of select="description"/>
	      </xsl:element>

	      <xsl:element name="td">
		<xsl:if test="normalize-space(documentation)!=''">
		  <xsl:call-template name="output-tokens">
		    <xsl:with-param name="list" select="normalize-space(documentation)"/>
		    <xsl:with-param name="delimiter" select="' '"/>
		  </xsl:call-template>
		</xsl:if>
	      </xsl:element>
	      <xsl:if test="$withabs='true'">
		<xsl:element name="td">
		  <xsl:copy-of select="abstract"/>
		</xsl:element>
	      </xsl:if>
	      <xsl:element name="td">
                <xsl:choose>
                  <xsl:when test="@support='S'">
                  <xsl:text>Yes</xsl:text>
                </xsl:when>
                <xsl:otherwise>
                  <xsl:text>No</xsl:text>
                </xsl:otherwise>
              </xsl:choose>
	    </xsl:element>

	    </xsl:element>
	  </xsl:when>
	</xsl:choose>
      </xsl:for-each>
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>

