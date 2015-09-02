[Installation](Installation.md) [Examples](SimpleCpp.md) [BuildInstructions](BuildInstructions.md)

---

A sample configuration file that defines one Domain (FooDomain) and one Topic (FooTopic) in that Domain.
```
<root>
    <ops_config type = "DefaultOPSConfigImpl">
        <domains>
            <!-- Define a domain called FooDomain.-->
            <element type = "MulticastDomain">
                <domainID>FooDomain</domainID>
                <domainAddress>234.5.6.8</domainAddress>
                <topics>
                    <!-- Define a Topic called FooTopic. -->
                    <element type = "Topic">
                        <name>FooTopic</name>
                        <port>8888</port>
                        <dataType>foo.FooData</dataType>
                    </element>
                </topics>
            </element>
        </domains>
    </ops_config>
</root>

```