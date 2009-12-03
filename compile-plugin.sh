mvn package &&
zip target/objectformatter-1.0-SNAPSHOT.jar scalac-plugin.xml &&
./with-plugin.sh
