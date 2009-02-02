package net.virtualvoid.string

object TypeHelper {
  import java.lang.reflect._

  def supertype(cl:Class[_]):Option[Type] = {
    val res = cl.getGenericSuperclass
    if (res == null) None
    else Some(res)
  }
  def genericInstanceType(cl:Type,Candidate:Class[_],tp:RandomAccessSeq[Type]):Option[Type] = {
    def resolved(ts:RandomAccessSeq[Type]):RandomAccessSeq[Type] =
      ts.map {
        case cl:Class[_] => cl
        case v:TypeVariable[_] => tp(v.getGenericDeclaration.asInstanceOf[Class[_]].getTypeParameters.indexOf(v))
      }.toArray

    cl match {
      case Candidate => Some(tp(0))
      case p:ParameterizedType => 
        genericInstanceType(p.getRawType,Candidate,resolved(p.getActualTypeArguments))
      case cl:Class[_] => 
        (supertype(cl).toList ++ cl.getGenericInterfaces)
          .flatMap(t => genericInstanceType(t,Candidate,tp).toList)
          .firstOption
    }
  }
  def main(args:scala.Array[String]):Unit = {
    System.out.println(genericInstanceType(classOf[ProcessBuilder].getMethod("command").getGenericReturnType
      ,classOf[java.lang.Iterable[_]],scala.Array()))
  }
}
