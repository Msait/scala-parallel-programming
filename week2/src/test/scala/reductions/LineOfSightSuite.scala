package reductions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    lineOfSight(Array[Float](0f, 1f, 8f, 9f), output)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 9f), 1, 4)
    assert(res == 4f)
  }


  test("upsweep should correctly handle the chunk 1 until 4 of an array of 4 elements with threshold = 2") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f), 1, 4, threshold = 2)
    assert(res == Node(Leaf(1,2, 1f), Leaf(2, 4, 4f)) )
  }

  test("upsweep should correctly handle the chunk 1 until 5 of an array of 5 elements with threshold = 1") {
    val res = upsweep(Array[Float](0f, 1f, 8f, 9f, 3f), 1, 5, threshold = 1)
    assert(res == Node(Node(Leaf(1,2,1.0f),Leaf(2,3,4.0f)),Node(Leaf(3,4,4.0f),Leaf(4,5,4.0f))))
  }


  test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 9f), output, 0f, 1, 4)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("parLineOfSight should correctly handle an array of size 4 with threshold = 2") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, threshold = 2)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

  test("parLineOfSight should correctly handle an array of size 4 with threshold = 5") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output, threshold = 5)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }

}

