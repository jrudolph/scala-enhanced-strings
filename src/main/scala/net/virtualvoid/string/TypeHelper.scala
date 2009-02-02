package net.virtualvoid.string

object TypeHelper {
  import java.lang.reflect._

  def nullSafe[T](x:T):Option[T] =
    if (x != null) Some(x) else None
  
  def supertype(cl:Class[_]):Option[Type] =
    nullSafe(cl.getGenericSuperclass)

  /**
   * Search recursively through the type hierarchy of cl (superclasses and interfaces) 
   * to find the Candidate class. If found return the first type parameter's type value.
   * 
   * In other words: Given a method with a generic result type, 
   * this function can be used to find out how a superclass or implemented interface is
   * parameterized.
   * 
   * For example, a method getList() returns a List[String]. We are not interested 
   * which type parameter List is applied to but which type parameter 
   * Iterable, which is a supertype of List, is applied to.
   * 
   * tp is the list of type values applied to type cl. While traversing the type
   * hierarchy, the type parameters have to be resolved in the context of the current
   * class.
   * 
   * Example: Candidate = Iterable
   * cl				                      tp
   * ArrayList[String]                    []  => getGenericInterface	
   * ParameterizedType(List,Class String) []  => resolved:String
   * List				              [String]=> getGenericInterface
   * ParameterizedType(Collection,E)  [String]=> resolved: TypeVariable(E,List) => String
   * Collection                       [String]=> getGenericInterface
   * ParameterizedType(Iterable,E)    [String]=> resolved: TypeVariable(E,Collection) => String
   * Iterable                         [String]=> String
   */
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
