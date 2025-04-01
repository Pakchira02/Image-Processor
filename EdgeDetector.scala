import java.awt.Color
import java.awt.image.BufferedImage

// simplest edge detector using contour tracing
// detect and trace boundaries of black regions in a binary image
// aka grayscale image where black pixels represent objects and white pixels represent background

object EdgeDetector {
  def findBoundaryPolygons(image: BufferedImage): List[List[(Int, Int)]] = {
    var visited = Set.empty[(Int, Int)] // set to keep track of pixels that already part of a detected boundary
    var boundaries = List.empty[List[(Int, Int)]] // stores detected boundary polygons

    // if image is black and has not been visited
    for (x <- 0 until image.getWidth; y <- 0 until image.getHeight
         if !visited((x, y)) && isBlack(image, x, y)) {
      val boundary = traceBoundary(image, x, y)
      if (boundary.nonEmpty) {
        boundaries ::= boundary // store detected boundary
        visited ++= boundary // mark visited boundary
      }
    }
    boundaries // return all detected boundary
  }

  def traceBoundary(image: BufferedImage, startX: Int, startY: Int): List[(Int, Int)] = {
    var boundary = List.empty[(Int, Int)]
    var (x, y) = (startX, startY)

    do {
      boundary ::= (x, y) // store detected boundary
      getNextBoundaryPixel(image, x, y) match {
        case Some((nx, ny)) => x = nx; y = ny // -> move to the next boundary pixel
        case None => return boundary // case no pixel is found
      }
    } while ((x, y) != (startX, startY)) // stop when return to the start boundary

    boundary // return the detected boundary
  }

  // function to find the next black pixel
  def getNextBoundaryPixel(image: BufferedImage, x: Int, y: Int): Option[(Int, Int)] = {
    val directions = List((0,1), (1,0), (0,-1), (-1,0)) // directions: right, down, left, up

    // collectFirst returns an Option -> returns a single value or nothing
    directions.collectFirst {
      case (dx, dy) if isBlack(image, x + dx, y + dy) => (x + dx, y + dy)
    }
  }

  def isBlack(image: BufferedImage, x: Int, y: Int): Boolean = {
    x >= 0 && y >= 0 && x < image.getWidth && y < image.getHeight && new Color(image.getRGB(x, y)).getRed < 128
    // defines black pixel as those with red component below 128
  }
}
