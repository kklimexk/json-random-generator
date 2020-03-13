package generator

import java.util.Date
import java.{lang, util}

import annotations.GeneratorAnnotation.{ValueHintDecimal, ValueHintIterator, ValueHintPrefix}
import annotations.{GeneratorAnnotation, ValueHintOptions}
import generators.field.{IteratorField, IteratorFieldGenerator}
import org.scalacheck.Gen
import utils.ReflectionUtils
import utils.ReflectionUtils._

import scala.collection.JavaConverters._
import scala.reflect.runtime.universe._

class JsonRandomGenerator(strGen: Gen[String],
                          longGen: Gen[java.lang.Long],
                          bigDecimalGen: (Int, Int) => Gen[java.math.BigDecimal],
                          booleanGen: Gen[java.lang.Boolean],
                          enumGen: Array[Any] => Gen[Any],
                          mapGen: Gen[Map[String, String]],
                          enumListGen: (Int, Array[Any]) => Gen[util.List[Any]],
                          strListGen: Gen[util.List[String]],
                          longListGen: Gen[util.List[java.lang.Long]],
                          bigDecimalListGen: (Int, Int) => Gen[util.List[java.math.BigDecimal]],
                          booleanListGen: Gen[util.List[java.lang.Boolean]]) {
  def generate[A](topLevelObj: A)(implicit c: TypeTag[A]): A = {

    val runtimeM = runtimeMirror(getClass.getClassLoader)

    def loop(obj: Any, tp: Type): A = {
      val members = tp.decls.filter(_.isPrivate)
      members.foreach { m =>
        val field = m.asTerm
        val methodFullName = field.fullName
        val methodName = field.name
        val methodReturnType = field.typeSignature
        val methodTypeArgs = methodReturnType.typeArgs
        val fieldAnnotations = field.annotations
        val fieldAnnotationsProperties = fieldAnnotations.map(a => ReflectionUtils.getAnnotationProperties(a))

        def valueHintDecimal = resolvePropertiesForAnnotation(ValueHintDecimal, fieldAnnotationsProperties)
          .getOrElse(Map("precision" -> "10", "scale" -> "0"))
        def valueHintOptionsAnnotation =
          fieldAnnotations.find(a => a.tree.tpe.typeSymbol.name.toString == GeneratorAnnotation.ValueHintOptions.value)
            .map { a =>
              val annotation = ReflectionUtils.getAnnotationPropertiesJava(methodName.toString, obj, a.tree.tpe.typeSymbol.fullName)
              annotation.asInstanceOf[ValueHintOptions].options().toList
            }
        def valueHintIteratorAnnotation = resolvePropertiesForAnnotation(ValueHintIterator, fieldAnnotationsProperties)
          .map(p => IteratorFieldGenerator.next(IteratorField(methodFullName, p("start").toInt), p("step").toInt))
          .map(_.state)
        def valueHintPrefixAnnotation = resolvePropertiesForAnnotation(ValueHintPrefix, fieldAnnotationsProperties)
          .getOrElse(Map("prefix" -> ""))

        if (methodFullName.startsWith("output")) {
          //println(s"$methodName, $methodReturnType")

          val returnTypeSymbolFullName = resolveTypeSymbolFullName(methodReturnType)

          try {
            Class.forName(returnTypeSymbolFullName) match {
              case t if t == classOf[Object] =>
              //Do nothing
              case t if t == classOf[String] =>
                def valueHintIterator = valueHintIteratorAnnotation
                  .map(_.toString)

                val resGen = valueHintOptionsAnnotation
                  .map(l => Gen.oneOf(l).sample.get)
                  .orElse(valueHintIterator)
                  .getOrElse(strGen.sample.get)

                invokeMethod(obj,
                  Seq(s"${valueHintPrefixAnnotation("prefix").replaceAll("^\"|\"$", "")}$resGen"),
                  s"set${methodName.toString.capitalize}", Seq(t))
              case t if t == classOf[java.lang.Long] =>
                def valueHintIterator = valueHintIteratorAnnotation
                  .map(el => new lang.Long(el))

                val resGen = valueHintOptionsAnnotation.map(_.map(el => new lang.Long(el)))
                  .map(l => Gen.oneOf(l).sample.get)
                  .orElse(valueHintIterator)
                  .getOrElse(longGen.sample.get)

                invokeMethod(obj, Seq(resGen),
                  s"set${methodName.toString.capitalize}", Seq(t))
              case t if t == classOf[java.math.BigDecimal] =>
                def valueHintIterator = valueHintIteratorAnnotation
                  .map(el => new java.math.BigDecimal(el))

                val resGen = valueHintOptionsAnnotation.map(_.map(el => new java.math.BigDecimal(el)))
                  .map(l => Gen.oneOf(l).sample.get)
                  .orElse(valueHintIterator)
                  .getOrElse(bigDecimalGen(valueHintDecimal("precision").toInt, valueHintDecimal("scale").toInt).sample.get)

                invokeMethod(obj, Seq(resGen),
                  s"set${methodName.toString.capitalize}", Seq(t))
              case t if t == classOf[java.lang.Boolean] =>
                val resGen = valueHintOptionsAnnotation.map(_.map(el => new lang.Boolean(el)))
                    .map(l => Gen.oneOf(l))
                    .getOrElse(booleanGen)

                invokeMethod(obj, Seq(resGen.sample.get),
                  s"set${methodName.toString.capitalize}", Seq(t))
              case t if t == classOf[java.util.Date] =>
                def valueHintIterator = valueHintIteratorAnnotation
                  .map(el => new lang.Long(el))
                  .map(el => new Date(el))

                val resGen = valueHintOptionsAnnotation.map(_.map(el => new Date(el.toLong)))
                    .map(l => Gen.oneOf(l).sample.get)
                    .orElse(valueHintIterator)
                    .getOrElse(new Date())

                invokeMethod(obj, Seq(resGen),
                  s"set${methodName.toString.capitalize}", Seq(t))
              case t if t.isEnum =>
                val enumValues = getEnumValues(t.asInstanceOf[Class[Any]])
                val resGen = valueHintOptionsAnnotation
                  .map(_.flatMap(eV => enumValues.filter(_.toString == eV)))
                  .map(l => Gen.oneOf(l))
                  .getOrElse(enumGen(enumValues))

                invokeMethod(obj, Seq(resGen.sample.get.asInstanceOf[AnyRef]),
                  s"set${methodName.toString.capitalize}", Seq(t))
              case t if t == classOf[java.util.List[_]] =>
                generateLists(methodTypeArgs, runtimeM, obj, methodName.toString, fieldAnnotations, loop, isNested = false)
              case t if t == classOf[java.util.Map[_, _]] && methodTypeArgs.map(_.typeSymbol).map(_.asClass).map(runtimeM.runtimeClass) == List(classOf[String], classOf[String]) =>
                val generatedMap = mapGen.sample.get
                generatedMap.foreach {
                  case (k, v) => invokeMethod(obj, Seq(k, v),
                    "setAdditionalProperty", Seq(classOf[String], classOf[String]))
                }
              case t if t == classOf[java.util.Map[_, _]] && methodTypeArgs.map(_.typeSymbol).map(_.asClass).map(runtimeM.runtimeClass) == List(classOf[String], classOf[Object]) =>
              //Do nothing
              case t =>
                val newInstance = Class.forName(returnTypeSymbolFullName).newInstance()
                invokeMethod(obj, Seq(newInstance.asInstanceOf[AnyRef]),
                  s"set${methodName.toString.capitalize}", Seq(t))
                loop(newInstance, methodReturnType)
              //loop(mirror.reflect(methodReturnType).instance, methodReturnType)
            }
          } catch {
            case e: ClassNotFoundException => e.printStackTrace()
          }
        }
      }
      topLevelObj
    }

    loop(topLevelObj, c.tpe)
  }

  private def generateLists[A](methodTypeArgs: List[Type], runtimeM: Mirror, obj: Any,
                               methodName: String, fieldAnnotations: List[Annotation], loop: (Any, Type) => A, isNested: Boolean): Unit = {

    val listTypeArg = methodTypeArgs.head
    val listTypeSymbolFullName = resolveTypeSymbolFullName(listTypeArg.typeSymbol.typeSignature)
    val listTypeParam = runtimeM.runtimeClass(listTypeArg.typeSymbol.asClass)
    val fieldAnnotationsProperties = fieldAnnotations.map(a => ReflectionUtils.getAnnotationProperties(a))

    def valueHintDecimal = resolvePropertiesForAnnotation(ValueHintDecimal, fieldAnnotationsProperties)
      .getOrElse(Map("precision" -> "10", "scale" -> "0"))
    def valueHintOptionsAnnotation =
      fieldAnnotations.find(a => a.tree.tpe.typeSymbol.name.toString == GeneratorAnnotation.ValueHintOptions.value)
        .map { a =>
          val annotation = ReflectionUtils.getAnnotationPropertiesJava(methodName.toString, obj, a.tree.tpe.typeSymbol.fullName)
          annotation.asInstanceOf[ValueHintOptions].options().toList
        }

    if (isNested) {
      listTypeParam match {
        case ltp if ltp == classOf[Object] =>
        //Do nothing
        case ltp if ltp == classOf[String] =>
          val resGen = valueHintOptionsAnnotation.map(_.asJava).getOrElse(strListGen.sample.get)

          obj.asInstanceOf[java.util.List[String]].addAll(resGen)
        case ltp if ltp == classOf[java.lang.Long] =>
          val resGen = valueHintOptionsAnnotation.map(_.map(el => new lang.Long(el)))
            .map(_.asJava)
            .getOrElse(longListGen.sample.get)

          obj.asInstanceOf[java.util.List[java.lang.Long]].addAll(resGen)
        case ltp if ltp == classOf[java.math.BigDecimal] =>
          val resGen = valueHintOptionsAnnotation.map(_.map(el => new java.math.BigDecimal(el)))
            .map(_.asJava)
            .getOrElse(bigDecimalListGen(valueHintDecimal("precision").toInt, valueHintDecimal("scale").toInt).sample.get)

          obj.asInstanceOf[java.util.List[java.math.BigDecimal]].addAll(resGen)
        case ltp if ltp == classOf[java.lang.Boolean] =>
          val resGen = valueHintOptionsAnnotation.map(_.map(el => new lang.Boolean(el)))
            .map(_.asJava)
            .getOrElse(booleanListGen.sample.get)

          obj.asInstanceOf[java.util.List[java.lang.Boolean]].addAll(resGen)
        case ltp if ltp == classOf[java.util.Date] =>
          val resGen = valueHintOptionsAnnotation.map(_.map(el => new Date(el.toLong)))
            .map(_.asJava)
            .getOrElse(Seq(new Date(), new Date()).asJava)

          obj.asInstanceOf[java.util.List[java.util.Date]].addAll(resGen)
        case ltp if ltp.isEnum =>
          val enumValues = getEnumValues(ltp.asInstanceOf[Class[Any]])

          val resGen = valueHintOptionsAnnotation
            .map(_.flatMap(eV => enumValues.filter(_.toString == eV)))
            .map(_.asJava)
            .getOrElse(enumListGen(2, enumValues).sample.get)

          obj.asInstanceOf[java.util.List[Any]].addAll(resGen)
        case ltp if ltp == classOf[java.util.List[_]] =>
          val numOfElemInOuterArr = 2
          val outerArr = new util.ArrayList[java.util.List[Any]]()

          val innerArrays = (1 to numOfElemInOuterArr).map(_ => new util.ArrayList[Any]())
          innerArrays.foreach(outerArr.add)

          obj.asInstanceOf[java.util.List[java.util.List[Any]]].addAll(outerArr)

          innerArrays.foreach(nestedArray => generateLists(listTypeArg.typeArgs, runtimeM, nestedArray, methodName, fieldAnnotations, loop, isNested = true))
        case _ =>
          val numOfInstancesToGenerate = 2
          val instanceList = (1 to numOfInstancesToGenerate).map(_ => Class.forName(listTypeSymbolFullName).newInstance())

          obj.asInstanceOf[java.util.List[Any]].addAll(instanceList.asJava)

          instanceList.foreach(ins => loop(ins, listTypeArg))
      }
    } else {
      listTypeParam match {
        case ltp if ltp == classOf[Object] =>
        //Do nothing
        case ltp if ltp == classOf[String] =>
          val resGen = valueHintOptionsAnnotation.map(_.asJava)
            .getOrElse(strListGen.sample.get)

          invokeMethod(obj, Seq(resGen),
            s"set${methodName.capitalize}", Seq(classOf[java.util.List[_]]))
        case ltp if ltp == classOf[java.lang.Long] =>
          val resGen = valueHintOptionsAnnotation.map(_.map(el => new lang.Long(el)))
            .map(_.asJava)
            .getOrElse(longListGen.sample.get)

          invokeMethod(obj, Seq(resGen),
            s"set${methodName.capitalize}", Seq(classOf[java.util.List[_]]))
        case ltp if ltp == classOf[java.math.BigDecimal] =>
          val resGen = valueHintOptionsAnnotation.map(_.map(el => new java.math.BigDecimal(el)))
            .map(_.asJava)
            .getOrElse(bigDecimalListGen(valueHintDecimal("precision").toInt, valueHintDecimal("scale").toInt).sample.get)

          invokeMethod(obj, Seq(resGen),
            s"set${methodName.capitalize}", Seq(classOf[java.util.List[_]]))
        case ltp if ltp == classOf[java.lang.Boolean] =>
          val resGen = valueHintOptionsAnnotation.map(_.map(el => new lang.Boolean(el)))
            .map(_.asJava)
            .getOrElse(booleanListGen.sample.get)

          invokeMethod(obj, Seq(resGen),
            s"set${methodName.capitalize}", Seq(classOf[java.util.List[_]]))
        case ltp if ltp == classOf[java.util.Date] =>
          val resGen = valueHintOptionsAnnotation.map(_.map(el => new Date(el.toLong)))
            .map(_.asJava)
            .getOrElse(Seq(new Date(), new Date()).asJava)

          invokeMethod(obj, Seq(resGen),
            s"set${methodName.capitalize}", Seq(classOf[java.util.List[_]]))
        case ltp if ltp.isEnum =>
          val enumValues = getEnumValues(ltp.asInstanceOf[Class[Any]])

          val resGen = valueHintOptionsAnnotation
            .map(_.flatMap(eV => enumValues.filter(_.toString == eV)))
            .map(_.asJava)
            .getOrElse(enumListGen(2, enumValues).sample.get)

          invokeMethod(obj, Seq(resGen),
            s"set${methodName.capitalize}", Seq(classOf[java.util.List[_]]))
        case ltp if ltp == classOf[java.util.List[_]] =>
          val numOfElemInOuterArr = 2
          val outerArr = new util.ArrayList[java.util.List[Any]]()

          val innerArrays = (1 to numOfElemInOuterArr).map(_ => new util.ArrayList[Any]())
          innerArrays.foreach(outerArr.add)

          invokeMethod(obj, Seq(outerArr),
            s"set${methodName.capitalize}", Seq(classOf[java.util.List[java.util.List[_]]]))
          innerArrays.foreach(nestedArray => generateLists(listTypeArg.typeArgs, runtimeM, nestedArray, methodName, fieldAnnotations, loop, isNested = true))
        case _ =>
          val numOfInstancesToGenerate = 2
          val instanceList = (1 to numOfInstancesToGenerate).map(_ => Class.forName(listTypeSymbolFullName).newInstance())

          invokeMethod(obj, Seq(instanceList.asJava),
            s"set${methodName.capitalize}", Seq(classOf[java.util.List[_]]))

          instanceList.foreach(ins => loop(ins, listTypeArg))
      }
    }
  }

  private def resolvePropertiesForAnnotation(generatorAnnotation: GeneratorAnnotation,
                                             annotationProps: Seq[(String, Map[String, String])]): Option[Map[String, String]] =
    annotationProps.find { case (annName, _) => annName == generatorAnnotation.value }.map(_._2)

  private def getEnumValues[E](enumClass: Class[E]): Array[E] = {
    val f = enumClass.getDeclaredField("$VALUES")
    f.setAccessible(true)
    val o = f.get(null)

    o.asInstanceOf[Array[E]]
  }

  private def resolveTypeSymbolFullName(methodReturnType: Type): String = {
    val typeS = methodReturnType.typeSymbol.asType
    val typeSFullName = typeS.fullName

    val typeSArr = typeSFullName.split("\\.").toList
    val isInnerEnum = typeSArr.lift(typeSArr.size - 2).exists(_.head.isUpper)

    if (typeS.isJavaEnum && isInnerEnum)
      typeS.fullName.patch(typeS.fullName.lastIndexOf('.'), "$", 1)
    else typeS.fullName
  }
}
