import java.awt.image.BufferedImage
import java.util.stream.IntStream

import scala.collection.parallel.CollectionConverters._

object MirrorImage {
  def apply(image: BufferedImage): BufferedImage = {
    val width = image.getWidth
    val height = image.getHeight
    val newImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    // Parallelize the processing
    (0 until width).par.foreach { x =>
      (0 until height).foreach { y =>
        newImage.setRGB(x, y, image.getRGB(width - x - 1, y))
      }
    }

    newImage
  }
}

/* object MirrorImage {
  def apply(image: BufferedImage): BufferedImage = {
    val width = image.getWidth
    val height = image.getHeight
    val newImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    // Using Java parallel stream
    IntStream.range(0, width).parallel().forEach { x =>
      (0 until height).foreach { y =>
        newImage.setRGB(x, y, image.getRGB(width - x - 1, y))
      }
    }

    newImage
  }
} */
