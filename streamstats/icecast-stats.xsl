<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
		xmlns="http://www.w3.org/1999/xhtml"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		exclude-result-prefixes="xsl">

<xsl:output method="text"/>

<xsl:template match="/">
  <xsl:apply-templates select="//div[@class='newscontent']">
  </xsl:apply-templates>
</xsl:template>

<xsl:template match="div[@class='newscontent']">
  <xsl:variable name="title">
    <xsl:value-of select="substring-after(.//h3, '/')"/>
  </xsl:variable>
  <xsl:variable name="listeners">
    <xsl:value-of select=".//tr[contains(td[1], 'Current Listeners')]/td[@class='streamdata']"/>
  </xsl:variable>

  <xsl:if test="not($title = '')">
    <xsl:value-of select="$title"/>
    <xsl:text>.value </xsl:text>
    <xsl:value-of select="$listeners"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>


