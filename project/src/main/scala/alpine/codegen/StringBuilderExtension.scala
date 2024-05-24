package alpine.codegen

extension (self: StringBuilder) def appendCommaSeparated[T](ls: Seq[T])(
    reduce: (StringBuilder, T) => Unit
): Unit =
    var f = true
    for l <- ls do
      if f then f = false else self ++= ", "
      reduce(self, l)