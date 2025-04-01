import java.awt.Color
import java.awt.image.BufferedImage
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

object ParEdgeDetector {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(4))

  // function to parallelize the contour tracing process
  def findBoundaryPolygons(image: BufferedImage): List[List[(Int, Int)]] = {
    val width = image.getWidth
    val height = image.getHeight

    // split the image into four subregions
    val subregions = List(
      (0, 0, width / 2, height / 2),
      (width / 2, 0, width, height / 2),
      (0, height / 2, width / 2, height),
      (width / 2, height / 2, width, height)
    )

    // process each subregion in parallel
    val futures = subregions.map { case (xStart, yStart, xEnd, yEnd) =>
      Future {
        traceBoundariesInRegion(image, xStart, yStart, xEnd, yEnd)
      }
    }

    val boundaryResults = scala.concurrent.Await.result(Future.sequence(futures), 10.seconds)

    // remove duplicates
    boundaryResults.flatten.distinct
  }

  // trace boundaries in a specific region of the image
  def traceBoundariesInRegion(image: BufferedImage, xStart: Int, yStart: Int, xEnd: Int, yEnd: Int): List[List[(Int, Int)]] = {
    var visited = Set.empty[(Int, Int)]
    var boundaries = List.empty[List[(Int, Int)]]

    for (x <- xStart until xEnd; y <- yStart until yEnd if !visited((x, y)) && isBlack(image, x, y)) {
      val boundary = traceBoundary(image, x, y)
      if (boundary.nonEmpty) {
        boundaries ::= boundary
        visited ++= boundary
      }
    }
    boundaries
  }

  // trace the boundary starting from a black pixel
  def traceBoundary(image: BufferedImage, startX: Int, startY: Int): List[(Int, Int)] = {
    var boundary = List.empty[(Int, Int)]
    var (x, y) = (startX, startY)

    do {
      boundary ::= (x, y)
      getNextBoundaryPixel(image, x, y) match {
        case Some((nx, ny)) => x = nx; y = ny
        case None => return boundary
      }
    } while ((x, y) != (startX, startY))

    boundary
  }

  // get the next black pixel in one of the four directions
  def getNextBoundaryPixel(image: BufferedImage, x: Int, y: Int): Option[(Int, Int)] = {
    val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))

    directions.collectFirst {
      case (dx, dy) if isBlack(image, x + dx, y + dy) => (x + dx, y + dy)
    }
  }

  // check if a pixel is black
  def isBlack(image: BufferedImage, x: Int, y: Int): Boolean = {
    x >= 0 && y >= 0 && x < image.getWidth && y < image.getHeight &&
      new Color(image.getRGB(x, y)).getRed < 128
  }
}

