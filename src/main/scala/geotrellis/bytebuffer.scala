package geotrellis.shapefile

import java.nio.{ByteBuffer,ByteOrder}

object Implicits {

  class ExtendedByteBuffer(val b: ByteBuffer) extends AnyVal {
    def little = b.order(ByteOrder.LITTLE_ENDIAN)
    def big = b.order(ByteOrder.BIG_ENDIAN)
    def skip(nBytes: Int) = b.position(b.position + nBytes)
    def skipBbox = b.skip(32) // skip 4 doubles (32 bytes)
  }

  // perhaps this shouldn't live in a package object
  implicit def extendByteBuffer(b: ByteBuffer):ExtendedByteBuffer = new ExtendedByteBuffer(b)

}
