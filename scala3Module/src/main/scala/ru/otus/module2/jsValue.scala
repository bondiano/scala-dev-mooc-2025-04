package ru.otus.module2

enum JsValue:
  case JsObject(get: Map[String, JsValue])
  case JsString(get: String)
  case JsNumber(get: Double)
  case JsNull

trait JsonWriter[T]:
  def write(v: T): JsValue

object JsonWriter:
  def apply[T](using ev: JsonWriter[T]): JsonWriter[T] = ev

  def from[T](f: T => JsValue): JsonWriter[T] = new JsonWriter[T]:
    def write(v: T): JsValue = f(v)

  given JsonWriter[Int] = from(JsValue.JsNumber(_))
  given JsonWriter[String] = from(JsValue.JsString(_))

  given [T](using ev: JsonWriter[T]): JsonWriter[Option[T]] =
    from[Option[T]]:
      case Some(value) => ev.write(value)
      case None => JsValue.JsNull

extension [T](v: T)
  def toJson(using ev: JsonWriter[T]): JsValue =
    ev.write(v)

@main def testJson(): Unit =
  println(10.toJson)
  println("hello".toJson)
  println(Option(10).toJson)
  println(Option("world").toJson)

  println("test".toJson)
  println(Option(42).toJson)