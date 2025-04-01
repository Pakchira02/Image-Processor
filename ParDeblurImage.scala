import java.awt.image.BufferedImage
import java.awt.Color
import org.apache.commons.math3.complex.Complex
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

object ParDeblurImage {

  def conjugate(c: Complex): Complex = new Complex(c.getReal, -c.getImaginary)

  def multiply(c1: Complex, c2: Complex): Complex =
    new Complex(
      c1.getReal * c2.getReal - c1.getImaginary * c2.getImaginary,
      c1.getReal * c2.getImaginary + c1.getImaginary * c2.getReal
    )

  def absSq(c: Complex): Double = c.getReal * c.getReal + c.getImaginary * c.getImaginary

  // problem: FFT is not centered correctly -> high-frequency components can wrap around creating patterns
  // fix: apply a frequency shift before and after FFT
  def fftShift(input: Array[Array[Complex]]): Array[Array[Complex]] = {
    val n = input.length
    val m = input(0).length
    Array.tabulate(n, m) { (x, y) =>
      input(x)(y).multiply(if ((x + y) % 2 == 0) 1 else -1) // Alternate sign
    }
  }

  def deblurImage(image: BufferedImage, noisePower: Double = 0.01): BufferedImage = {
    if (image == null) {
      System.err.println("Error: Image is null")
      return null
    }

    val (width, height) = (image.getWidth, image.getHeight)
    val newSize = nextPowerOf2(math.max(width, height))

    val complexImage = imageToComplexArray(image)
    val paddedImage = padImage(imageToComplexArray(image), newSize, newSize)
    val shiftedImage = fftShift(paddedImage) // apply shift before FFT

    val windowedImage = applyHannWindow(paddedImage)  // apply window

    val blurKernel = createGaussianKernel(newSize, newSize, sigma = 5.0)

    // parallel FFT
    val fftImageFuture = Future { FFT2D(paddedImage) }
    val fftKernelFuture = Future { FFT2D(blurKernel) }

    // problem: if absSq(kernel) is too small, division leads to instability
    // fix: threshold small values to prevent division by near-zero

    // fixL: cap high frequencies (reduce noise amplification)
    val deblurredFFT = for {
      fftImage <- fftImageFuture
      fftKernel <- fftKernelFuture
    } yield fftImage.indices.map { i =>
      fftImage(i).indices.map { j =>
        val img = fftImage(i)(j)
        val kernel = fftKernel(i)(j)
        val denom = absSq(kernel) + noisePower

        if (denom > 1e-5) multiply(img, conjugate(kernel)).divide(denom)
        else img.multiply(0) // Avoid artifacts
      }.toArray
    }.toArray



    // wait
    val finalFFT = Await.result(deblurredFFT, scala.concurrent.duration.Duration.Inf)

    // val deblurredImage = IFFT2D(finalFFT)
    // fix:
    val deblurredImage = fftShift(IFFT2D(finalFFT)) // centers the FFT


    complexArrayToImage(deblurredImage, width, height)
  }

  def applyHannWindow(image: Array[Array[Complex]]): Array[Array[Complex]] = {
    val (width, height) = (image.length, image(0).length)
    val windowed = Array.tabulate(width, height) { (x, y) =>
      val hannX = 0.5 * (1 - Math.cos(2 * Math.PI * x / (width - 1)))
      val hannY = 0.5 * (1 - Math.cos(2 * Math.PI * y / (height - 1)))
      val weight = hannX * hannY
      image(x)(y).multiply(weight)
    }
    windowed
  }

  // problem: checkerboard
  // help: reduce artifacts from abrupt edges
  def applyGaussianBlur(image: BufferedImage): BufferedImage = {
    val kernelSize = 3
    val kernel = Array(
      Array(1.0, 2.0, 1.0),
      Array(2.0, 4.0, 2.0),
      Array(1.0, 2.0, 1.0)
    ).map(_.map(_ / 16))

    val output = new BufferedImage(image.getWidth, image.getHeight, BufferedImage.TYPE_BYTE_GRAY)

    for (x <- 1 until image.getWidth - 1; y <- 1 until image.getHeight - 1) {
      var sum = 0.0
      for (i <- -1 to 1; j <- -1 to 1) {
        val intensity = new Color(image.getRGB(x + i, y + j)).getRed
        sum += intensity * kernel(i + 1)(j + 1)
      }
      val newIntensity = Math.min(255, Math.max(0, sum))
      output.setRGB(x, y, new Color(newIntensity.toInt, newIntensity.toInt, newIntensity.toInt).getRGB)
    }

    output
  }

  // problem: checkerboard
  // possible cause: .par.map(fft1D) might be introducing race conditions due to parallelism
  def FFT2D(input: Array[Array[Complex]]): Array[Array[Complex]] = {
    val rowTransformed = input.map(fft1D) // row-wise FFT
    val transposed = rowTransformed.transpose
    val colTransformed = transposed.map(fft1D) // column-wise FFT
    colTransformed.transpose
  }

  def IFFT2D(input: Array[Array[Complex]]): Array[Array[Complex]] = {
    val rowTransformed = input.map(iFFT1D) // row-wise IFFT
    val transposed = rowTransformed.transpose
    val colTransformed = transposed.map(iFFT1D) // column-wise IFFT
    colTransformed.transpose
  }


  def fft1D(input: Array[Complex]): Array[Complex] = {
    val n = input.length
    if (n == 1) return input

    val (evens, odds) = input.zipWithIndex.partition(_._2 % 2 == 0)
    val evenFFT = fft1D(evens.map(_._1))
    val oddFFT = fft1D(odds.map(_._1))

    Array.tabulate(n) { k =>
      val twiddle = new Complex(Math.cos(-2 * Math.PI * k / n), Math.sin(-2 * Math.PI * k / n)).multiply(oddFFT(k % (n / 2)))
      if (k < n / 2) evenFFT(k).add(twiddle)
      else evenFFT(k - n / 2).subtract(twiddle)
    }
  }

  def iFFT1D(input: Array[Complex]): Array[Complex] = {
    val n = input.length
    if (n == 1) return input

    val (evens, odds) = input.zipWithIndex.partition(_._2 % 2 == 0)
    val evenIFFT = iFFT1D(evens.map(_._1))
    val oddIFFT = iFFT1D(odds.map(_._1))

    val scaledIFFT = Array.tabulate(n) { k =>
      val twiddle = new Complex(Math.cos(2 * Math.PI * k / n), Math.sin(2 * Math.PI * k / n)).multiply(oddIFFT(k % (n / 2)))
      if (k < n / 2) evenIFFT(k).add(twiddle)
      else evenIFFT(k - n / 2).subtract(twiddle)
    }

    // normalize in each step
    scaledIFFT.map(_.divide(n))
  }

  def imageToComplexArray(image: BufferedImage): Array[Array[Complex]] =
    Array.tabulate(image.getHeight, image.getWidth)((y, x) => new Complex(new Color(image.getRGB(x, y)).getRed, 0))

  def complexArrayToImage(array: Array[Array[Complex]], width: Int, height: Int): BufferedImage = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)
    val realValues = array.flatten.map(_.getReal)
    val minReal = realValues.min
    val maxReal = realValues.max

    for (x <- 0 until width; y <- 0 until height) {
      val intensity = ((array(y)(x).getReal - minReal) / (maxReal - minReal)) * 255
      val gammaCorrected = Math.pow(intensity / 255, 1 / 2.2) * 255 // apply gamma correction (for correct FFT)
      // gamma correction helps prevent loss of contrast
      val validIntensity = math.min(255, math.max(0, gammaCorrected))
      image.setRGB(x, y, new Color(validIntensity.toInt, validIntensity.toInt, validIntensity.toInt).getRGB)
    }

    image
  }


  def createGaussianKernel(width: Int, height: Int, sigma: Double): Array[Array[Complex]] = {
    val centerX = width / 2
    val centerY = height / 2
    val sigmaSq = sigma * sigma
    Array.tabulate(width, height) { (x, y) =>
      val dx = x - centerX
      val dy = y - centerY
      new Complex(math.exp(-(dx * dx + dy * dy) / (2 * sigmaSq)), 0)
    }
  }

  // problem: checkerboard
  // possible cause: padding discontinuities
  // fix: change from zero-padding to mirror-padding
  // ^^ mirror padding preserves edge continuity (reduce artifacts)
  def padImage(image: Array[Array[Complex]], newWidth: Int, newHeight: Int): Array[Array[Complex]] = {
    val oldWidth = image.length
    val oldHeight = image(0).length
    val padded = Array.fill(newWidth, newHeight)(new Complex(0, 0))

    // copy original image
    for (x <- 0 until oldWidth; y <- 0 until oldHeight) {
      padded(x)(y) = image(x)(y)
    }

    // mirror-padding on right and bottom (clamping correctly)
    // preserves symmetry
    for (x <- oldWidth until newWidth; y <- 0 until oldHeight) {
      val mirroredX = Math.max(0, Math.min(2 * oldWidth - x - 2, oldWidth - 1))
      padded(x)(y) = padded(mirroredX)(y)
    }

    for (y <- oldHeight until newHeight; x <- 0 until newWidth) {
      val mirroredY = Math.max(0, Math.min(2 * oldHeight - y - 2, oldHeight - 1))
      padded(x)(y) = padded(x)(mirroredY)
    }

    padded
  }

  def nextPowerOf2(n: Int): Int = {
    var power = 1
    while (power < n) power *= 2
    power
  }

}
