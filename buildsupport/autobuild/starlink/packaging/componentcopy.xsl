<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="xml" indent="yes" omit-xml-declaration="yes"/>
  <xsl:param name="flatdeps">flatdeps.xml</xsl:param>
  <xsl:param name="cpt">gaia</xsl:param>
  <xsl:param name="componentset">componentset.xml</xsl:param>
<xsl:template match="component">
  <xsl:apply-templates select="version"/>
  <xsl:apply-templates select="path"/>
  <xsl:apply-templates select="description"/>
  <xsl:apply-templates select="nl"/>
  <xsl:apply-templates select="abstract"/>
  <xsl:apply-templates select="nl"/>
  <xsl:apply-templates select="developers"/>
  <xsl:apply-templates select="nl"/>
  <xsl:apply-templates select="documentation"/>
  <xsl:apply-templates select="nl"/>
  <xsl:apply-templates select="bugreports"/>
  <xsl:apply-templates select="nl"/>
  <xsl:apply-templates select="notes"/>
  <xsl:apply-templates select="nl"/>
  <xsl:choose>
    <xsl:when test="starts-with(path,'applications')">
     <xsl:apply-templates select="dependencies" mode="run_applications"/>
     <xsl:apply-templates select="nl"/>
     <xsl:apply-templates select="dependencies" mode="build_applications"/>
    </xsl:when>
    <xsl:when test="starts-with(path,'etc')">
     <xsl:apply-templates select="dependencies" mode="run_applications"/>
     <xsl:apply-templates select="nl"/>
     <xsl:apply-templates select="dependencies" mode="build_applications"/>
    </xsl:when>
    <xsl:when test="starts-with(path,'libraries')">
     <xsl:apply-templates select="dependencies" mode="run_libraries"/>
     <xsl:apply-templates select="nl"/>
     <xsl:apply-templates select="dependencies" mode="build_libraries"/>
     </xsl:when>
    <xsl:when test="starts-with(path,'thirdparty')">
     <xsl:apply-templates select="dependencies" mode="run_libraries"/>
     <xsl:apply-templates select="nl"/>
     <xsl:apply-templates select="dependencies" mode="build_libraries"/>
    </xsl:when>
    </xsl:choose> 
</xsl:template>

<xsl:template match="version">
</xsl:template>

<xsl:template match="path">
</xsl:template>

<xsl:template match="description">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
</xsl:template>

<xsl:template match="abstract">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
</xsl:template>

<xsl:template match="p">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
</xsl:template>

<xsl:template match="dependencies" mode="build_applications">
 <build-dependencies>
   <xsl:apply-templates select="sourceset"/>
   <xsl:apply-templates select="build" mode="build"/>
   <xsl:apply-templates select="link" mode="build"/>
   <xsl:apply-templates select="configure"/>
 </build-dependencies>
</xsl:template>

<xsl:template match="dependencies" mode="build_libraries">
 <build-dependencies>
   <xsl:apply-templates select="sourceset"/>
   <xsl:apply-templates select="build" mode="build"/>
   <xsl:apply-templates select="configure"/>
 </build-dependencies>
</xsl:template>

<xsl:template match="sourceset">
 <build name="{.}"><xsl:variable name="package" select="."/>
   <xsl:value-of select="document($componentset)//component[@id=$package]/version"/>
 </build>
</xsl:template>

<xsl:template match="build" mode="build">
 <build name="{.}"><xsl:variable name="package" select="."/>
   <xsl:value-of select="document($componentset)//component[@id=$package]/version"/>
 </build>
</xsl:template>

<xsl:template match="build[@option='link']" mode="run">
 <run name="{.}"><xsl:variable name="package" select="."/>
   <xsl:value-of select="document($componentset)//component[@id=$package]/version"/>
 </run>
</xsl:template>

<xsl:template match="link" mode="build">
 <build name="{.}"><xsl:variable name="package" select="."/>
   <xsl:value-of select="document($componentset)//component[@id=$package]/version"/>
 </build>
</xsl:template>

<xsl:template match="link" mode="run">
 <run name="{.}"><xsl:variable name="package" select="."/>
   <xsl:value-of select="document($componentset)//component[@id=$package]/version"/>
 </run>
</xsl:template>


<xsl:template match="use">
 <run name="{.}"><xsl:variable name="package" select="."/>
   <xsl:value-of select="document($componentset)//component[@id=$package]/version"/>
 </run>
</xsl:template>

<xsl:template match="test">
 <run name="{.}"><xsl:variable name="package" select="."/>
   <xsl:value-of select="document($componentset)//component[@id=$package]/version"/>
 </run>
</xsl:template>

<xsl:template match="configure">
 <build name="{.}"><xsl:variable name="package" select="."/>
   <xsl:value-of select="document($componentset)//component[@id=$package]/version"/>
 </build>
</xsl:template>

<xsl:template match="dependencies" mode="run_applications">
<run-dependencies>  
   <xsl:apply-templates select="link" mode="run"/>
   <xsl:apply-templates select="use"/>
   <xsl:apply-templates select="build[@option='link']" mode="run"/>
</run-dependencies>
</xsl:template>

<xsl:template match="dependencies" mode="run_libraries">
<run-dependencies>  
   <xsl:apply-templates select="link" mode="run"/>
</run-dependencies>
</xsl:template>

<xsl:template match="developers">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
</xsl:template>

<xsl:template match="person">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
</xsl:template>

<xsl:template match="name">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
</xsl:template>

<xsl:template match="uname">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
</xsl:template>

<xsl:template match="email">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
</xsl:template>

<xsl:template match="role">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
</xsl:template>

<xsl:template match="documentation">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
</xsl:template>

<xsl:template match="bugreports">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
</xsl:template>

<xsl:template match="notes">
 <xsl:copy>
   <xsl:apply-templates/>
 </xsl:copy>
 </xsl:template>

<xsl:template name="nl">
  <xsl:text>&#10;</xsl:text>
</xsl:template>

</xsl:stylesheet>
