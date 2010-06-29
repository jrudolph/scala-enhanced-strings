SXR=/home/johannes/git/opensource/sxr/target/scala_2.8.0.RC6/sxr_2.8.0.RC6-0.2.5-SNAPSHOT.jar
ES=../../target/scala_2.8.0.RC6/scala-enhanced-strings_2.8.0.RC6-1.0.jar
SBT=../../project/boot/scala-2.7.7/org.scala-tools.sbt/sbt/0.7.4/sbt_2.7.7-0.7.4.jar

scalac -cp $SBT \
  -Xplugin:$ES \
  -Xplugin:$SXR \
  -P:sxr:base-directory:scala-src \
  -Xprint-pos \
  -Xprint:enhanced-strings \
  -Ylog:sxr \
  -Ystop:pickler \
  scala-src/*.scala &&
  #-Yrangepos \
#  -Xprint:enhanced-strings \
#  -Yshow-trees \

cp style.css ..sxr &&

gnome-open ..sxr/Overview.scala.html
