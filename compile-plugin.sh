mvn package
zip target/objectformatter-1.0-SNAPSHOT.jar scalac-plugin.xml
/home/johannes/download/scala-2.7.3.final/bin/scala -Xplugin:target/objectformatter-1.0-SNAPSHOT.jar 
