package edu.cmu.lti.nlp.amr.standford_parser

import java.lang.reflect.Field

import scala.annotation.tailrec

object ReflectionUtils {

  implicit class ThugLife(val target: Any) {
    /** Allows to access private fields with simple usage:
      * someObject.getField(fieldName) */
    def getField[T](name: String): T = {
      @tailrec
      def collectAllSuperclasses(currClass: Class[_], acc: Seq[Class[_]] = Seq()): Seq[Class[_]] = {
        if (currClass == classOf[Object]) acc
        else collectAllSuperclasses(currClass.getSuperclass, acc :+ currClass)
      }

      val superclasses = collectAllSuperclasses(target.getClass)
      val fields: Seq[Field] = superclasses.flatMap(_.getDeclaredFields)
      fields.find(_.getName == name) match {
        case Some(f) =>
          f.setAccessible(true)
          f.get(target).asInstanceOf[T]
        case None =>
          throw new RuntimeException("Thug life is over, man!")
      }
    }

    def callVoidMethod(name: String) = {
      val method = target.getClass.getDeclaredMethod(name)
      method.setAccessible(true)
      method.invoke(target)
    }
  }
}
