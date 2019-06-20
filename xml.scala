

val playerEventXML =
<report xmlns="http://schemas.adfotain.org/adapi-1.0">
  <date>123349999</date>
  <version>1.0</version>
  <player id="playerId">
    <playerEventLog>
      <event>
        <eventType>informational</eventType>
        <eventTime>eventTime</eventTime>
        <eventSource>eventSource</eventSource>
        <eventName>eventName</eventName>
        <metadata>
          <meta name="meta1-name" content="meta1-content"/>
          <meta name="meta2-name" content="meta2-content"/>
        </metadata>
      </event>
    </playerEventLog>
  </player>
</report>


(playerEventXML \ "date").text
(playerEventXML \ "eee" \ "zzz").text

//val malformedPlayerEventXML = playerEventXML.child(0).head