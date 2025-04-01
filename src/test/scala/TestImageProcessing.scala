import java.awt.Color
import java.awt.image.BufferedImage

object TestImageProcessing {
  def main(args: Array[String]): Unit = {
    val mario = "/Users/pakachira/Downloads/mario.png"
    val rein = "/Users/pakachira/Downloads/rein.webp"
    val kinoko = "/Users/pakachira/Downloads/kinoko.jpeg"

    /* ImageProcessor.processAndSave(mario, "mario_mirrored.jpg", MirrorImage.apply)
    println("mario mirrored")
    ImageProcessor.processAndSave(rein, "rein_mirrored.webp", MirrorImage.apply)
    println("rein mirrored")
    ImageProcessor.processAndSave(kinoko, "kinoko_mirrored.jpeg", MirrorImage.apply)
    println("kinoko mirrored") */

    // wrapper function for deblurImage that passes a default noisePower value
    /* val deblurredImageFunction = (image: BufferedImage) => DeblurImage.deblurImage(image, 0.01)
    ImageProcessor.processAndSave(mario, "mario_deblurred.jpg", deblurredImageFunction)
    println("mario deblurred")
    ImageProcessor.processAndSave(rein, "rand_deblurred", deblurredImageFunction)
    println("rein deblurred")
    ImageProcessor.processAndSave(kinoko, "kinoko_deblurred", deblurredImageFunction)
    println("kinoko deblurred") */

    val parDeblurred = (image: BufferedImage) => ParDeblurImage.deblurImage(image, 0.01)

    ImageProcessor.processAndSave(mario, "mario_deblurred.jpg", parDeblurred)
    println("mario deblurred")

    ImageProcessor.processAndSave(rein, "rand_deblurred.jpg", parDeblurred)
    println("rein deblurred")

    ImageProcessor.processAndSave(kinoko, "kinoko_deblurred.jpg", parDeblurred)
    println("kinoko deblurred")

    /* ImageProcessor.processAndSave(mario, "edges_parallel.jpg", image => {
      val polygons = EdgeDetector.findBoundaryPolygons(image)

      // Create a new image to draw the boundaries
      val resultImage = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_INT_RGB)

      // Draw boundaries on the result image
      for (polygon <- polygons) {
        polygon.foreach {
          case (x, y) =>
            resultImage.setRGB(x, y, Color.RED.getRGB) // Highlight boundaries with red color
        }
      }

      resultImage // Return the processed image
    })

    println("Edge Detection (Parallelized) processing completed.") */
  }
  }