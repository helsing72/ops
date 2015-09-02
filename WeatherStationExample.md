
```

package weatherstation;

class BasicWeatherData
{
    double temprature;
}

```

```
package weatherstation;

class ExtendedWeatherData extends BasicWeatherData
{
    virtual WindData windData;
    double humidity;

}
```

```
package weatherstation;

class BasicWindData
{
    double speed;

}
```

```
package weatherstation;

class ExtendedWindData extends BasicWindData
{
    double direction;
}

```

```
<?xml version="1.0" encoding="UTF-8"?>
<!--
 Description:
 A template ops_config.xml file, this file shall be put on run directory of all applications that wants to use these topics.
-->
<root>
    <ops_config type = "DefaultOPSConfigImpl">
        <domains>

            
            <element type = "MulticastDomain">
                <domainID>WeatherDomain</domainID>
                <domainAddress>234.5.6.8</domainAddress>
                <topics>
                    
                    <element type = "Topic">
                        <name>WeatherTopic</name>
                        <port>6686</port>
                        <dataType>weatherstation.BasicWeatherData</dataType>
                    </element>
                    <!-- TODO: Add more topics here... -->
                </topics>
            </element>

        </domains>
    </ops_config>
</root>


```