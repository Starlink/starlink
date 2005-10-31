<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>
 
<xsl:template match="/package">
$prefix=<xsl:value-of select="prefix"/>
$starprefix=<xsl:value-of select="prefix"/>
$starlink=<xsl:value-of select="prefix"/>
%product <xsl:value-of select="@component"/> - <xsl:value-of select="description"/>
%version <xsl:value-of select="version"/>
%release <xsl:value-of select="release"/><xsl:text>&#10;</xsl:text>
%copyright <xsl:value-of select="copyright"/>
%vendor <xsl:value-of select="vendor"/>
%packager <xsl:value-of select="packager"/>
%license ./license
%readme ./readme
<xsl:variable name="description">
 <xsl:call-template name="replace-string"> <!-- imported template -->
   <xsl:with-param name="text" select="abstract"/>
   <xsl:with-param name="replace" select="'&#10;'"/>
   <xsl:with-param name="with" select="'&#10;%description '"/>
   </xsl:call-template>
 </xsl:variable>
<xsl:value-of select="$description"/>
<xsl:text>&#10;</xsl:text>
<xsl:for-each select="run-dependencies/run">
   <xsl:variable name="newversion">
      <xsl:call-template name="replace-string"> <!-- imported template -->
        <xsl:with-param name="text" select="."/>
        <xsl:with-param name="replace" select="'-'"/>
        <xsl:with-param name="with" select="'.'"/>
      </xsl:call-template>
    </xsl:variable>
%requires <xsl:value-of select="@name"/> >= <xsl:value-of select="$newversion"/>
    </xsl:for-each>
<xsl:choose>
<xsl:when test="@component = 'htx'">
%requires init
</xsl:when>
<xsl:otherwise>
%requires htx
%requires init
</xsl:otherwise>
</xsl:choose>

%postinstall <xsl:text>&lt;&lt;EOF</xsl:text>
if test -f ${starlink}/bin/hlink; then
 ${starlink}/bin/hlink ${starprefix}/docs

else if test -f ${starprefix}/bin/hlink; then
 ${starprefix}/bin/hlink ${starprefix}/docs
else 
 echo "No hlink command found, cannot re-link documents."
fi
fi
EOF

%postremove <xsl:text>&lt;&lt;EOF</xsl:text>
if test -f ${starlink}/bin/hlink; then
 ${starlink}/bin/hlink ${starprefix}/docs

else if test -f ${starprefix}/bin/hlink; then
 ${starprefix}/bin/hlink ${starprefix}/docs
else 
 echo "No hlink command found, cannot re-link documents."
fi
fi
EOF

<xsl:for-each select="files/file">
<xsl:choose>
<xsl:when test="@type != 'd' and @type != 'l'">
<xsl:value-of select="@type"/><xsl:text> </xsl:text><xsl:value-of select="@permissions-octal"/> - - ${prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/> ${prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>  nostrip()
</xsl:when>
<xsl:when test="@type = 'd'">
<xsl:value-of select="@type"/><xsl:text> </xsl:text><xsl:value-of select="@permissions-octal"/> - - ${prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/> -
</xsl:when>
<xsl:when test="@type = 'l'">
<xsl:value-of select="@type"/><xsl:text> </xsl:text><xsl:value-of select="@permissions-octal"/> - - ${prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/> ${prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="@link"/><xsl:text>&#10;</xsl:text>
</xsl:when>
<xsl:otherwise>
<xsl:value-of select="@type"/><xsl:text> </xsl:text><xsl:value-of select="@permissions-octal"/> - - ${prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/> ${prefix}/<xsl:value-of select="@dir"/>/<xsl:value-of select="."/>  nostrip()
</xsl:otherwise>
</xsl:choose>
</xsl:for-each>
</xsl:template>
 
 <xsl:template name="replace-string">
    <xsl:param name="text"/>
    <xsl:param name="replace"/>
    <xsl:param name="with"/>
    <xsl:choose>
      <xsl:when test="contains($text,$replace)">
        <xsl:value-of select="substring-before($text,$replace)"/>
        <xsl:value-of select="$with"/>
        <xsl:call-template name="replace-string">
          <xsl:with-param name="text"
select="substring-after($text,$replace)"/>
          <xsl:with-param name="replace" select="$replace"/>
          <xsl:with-param name="with" select="$with"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>


</xsl:stylesheet>
