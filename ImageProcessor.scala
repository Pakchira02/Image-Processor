// https://otfried.org/scala/image.html

import java.awt.image.{BufferedImage, ConvolveOp, Kernel}
import java.io.File
import javax.imageio.ImageIO
import scala.math._

object ImageProcessor {

  def loadImage(path: String): Option[BufferedImage] = {
    try Some(ImageIO.read(new File(path)))
    catch { case _: Throwable => println(s"error: cannot load image at $path"); None }
  }

  def processAndSave(imagePath: String, outputPath: String, processFunction: BufferedImage => BufferedImage): Unit = {
    val image = ImageIO.read(new File(imagePath))
    val processedImage = processFunction(image)
    ImageIO.write(processedImage, "jpg", new File(outputPath))
  }

  def saveImagejpg(image: BufferedImage, path: String): Unit = {
    ImageIO.write(image, "jpg", new File(path))
  }

  def cropImage(image: BufferedImage): BufferedImage = {
    val w = min(image.getWidth, 256)
    val h = min(image.getHeight, 256)
    image.getSubimage(0, 0, w, h)
  }

  def toGrayscale(image: BufferedImage): BufferedImage = {
    val grayImage = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    val g = grayImage.getGraphics
    g.drawImage(image, 0, 0, null)
    g.dispose()
    grayImage
  }

  // sharpen image using 3x3 kernel
  def sharpenImage(image: BufferedImage): BufferedImage = {
    val sharpenKernel = Array(
      0f, -1f,  0f,
      -1f,  5f, -1f,
      0f, -1f,  0f
    )

    // the convolution may produce artifacts at the edges
    val op = new ConvolveOp(new Kernel(3, 3, sharpenKernel), ConvolveOp.EDGE_NO_OP, null)

    val output = new BufferedImage(image.getWidth, image.getHeight, image.getType)
    op.filter(image, output)
    output
  }
}
