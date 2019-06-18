import org.scalatest.FunSpec
import ringbuffer.RingBuffer

class RingBufferSpec extends FunSpec {
  describe("RingBuffer") {
    it("writes") {
      val b1 = RingBuffer()
      val b2 = b1 write 1
      val b3 = b2 write 2

      println(b3)
    }
  }
}