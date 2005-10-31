<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text"/>
  <xsl:param name="compname">ast</xsl:param>

<xsl:template match="/">

    <xsl:for-each select="componentset/component">
      <xsl:if test="@id = $compname">
       <xsl:value-of select="path"/>
      </xsl:if>
    </xsl:for-each>
    
</xsl:template>

</xsl:stylesheet>
