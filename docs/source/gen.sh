SXR=/home/johannes/git/opensource/sxr/target/scala_2.9.1/sxr_2.9.1-0.2.5-SNAPSHOT.jar
ES=../../target/scala_2.9.1/scala-enhanced-strings_2.9.1-0.5.2.jar
#SBT=../../project/boot/scala-2.7.7/org.scala-tools.sbt/sbt/0.7.4/sbt_2.7.7-0.7.4.jar

scalac \
  -Xplugin:$ES \
  -Xplugin:$SXR \
  -P:sxr:base-directory:scala-src \
  -Xprint-pos \
  -Xprint:enhanced-strings \
  -Ylog:sxr \
  scala-src/*.scala &&
#  -Ystop:pickler \
  #-Yrangepos \
#  -Xprint:enhanced-strings \
#  -Yshow-trees \

cp style.css ..sxr &&

gnome-open ..sxr/Overview.scala.html
