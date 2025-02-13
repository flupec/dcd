package parse

import java.nio.file.Path

object ParseTestUtils:
  def resourcePathAt(path: String): Path =
    val classLoader = Thread.currentThread().getContextClassLoader()
    Path.of(classLoader.getResource(path).toURI())

  def parseInputAt(path: String): ParseInput =
    val resourcePath = resourcePathAt(path)
    ParseInput.fromFile(resourcePath).right.get
