<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>

<xsl:template match="/">
 <xsl:for-each select="componentset/component">
    <xsl:if test="@id != 'starconf' and @id != 'automake'
                  and @id != 'autoconf' and @id != 'm4'
                  and @id != 'libtool' and @id != 'ssn78'
                  and @status != 'obsolete'">
    <xsl:value-of select="@id"/><xsl:text>&#10;       </xsl:text>
    </xsl:if>
 </xsl:for-each>
</xsl:template>


</xsl:stylesheet>
