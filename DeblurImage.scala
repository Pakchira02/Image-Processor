import org.apache.commons.math3.complex.Complex

import java.awt.Color
import java.awt.image.BufferedImage

object DeblurImage {

  def conjugate(c: Complex): Complex =
    new Complex(c.getReal, -c.getImaginary)

  def multiply(c1: Complex, c2: Complex): Complex =
    new Complex(
      c1.getReal * c2.getReal - c1.getImaginary * c2.getImaginary,
      c1.getReal * c2.getImaginary + c1.getImaginary * c2.getReal
    )

  def absSq(c: Complex): Double =
    c.getReal * c.getReal + c.getImaginary * c.getImaginary

  def deblurImage(image: BufferedImage, noisePower: Double = 0.01): BufferedImage = {
    // error handling
    if (image == null) {
      System.err.println("Error: Image is null")
      return null
    }

    val (width, height) = (image.getWidth, image.getHeight)

    // find the next power of 2 for FFT size (for performance efficiency)
    val newSize = nextPowerOf2(math.max(width, height))

    // convert image to complex array for FFT processing
    val complexImage = imageToComplexArray(image)

    // pad the image to the next power of 2 (avoids wraparound artifacts)
    // padded image is used to prevent information loss during operations where output size might be smaller than the input
    // prevents output from shrinking too much
    val paddedImage = padImage(complexImage, newSize, newSize)

    // create a gaussian kernel -> use to approximates blur
    val blurKernel = createGaussianKernel(newSize, newSize, sigma = 5.0)

    // perform FFT on both the image and the blur kernel
    val fftImage = FFT2D(paddedImage)
    val fftKernel = FFT2D(blurKernel)

    // deconvolution: divide FFT(image) by FFT(kernel) to reverse the blurring
    val deblurredFFT = fftImage.indices.map { i =>
      fftImage(i).indices.map { j =>
        val img = fftImage(i)(j)
        val kernel = fftKernel(i)(j)

        if (absSq(kernel) > 1e-8)
          multiply(img, conjugate(kernel)).divide(absSq(kernel) + noisePower)
        else
          img
      }.toArray
    }.toArray

    // perform Inverse FFT to get the deblurred image back in spatial domain
    val deblurredImage = IFFT2D(deblurredFFT)

    // convert back to BufferedImage and crop to original size
    complexArrayToImage(deblurredImage, width, height)
  }


  def fft1D(input: Array[Complex]): Array[Complex] = {
    val n = input.length
    if (n == 1) return input

    val even = fft1D(input.indices.collect { case i if i % 2 == 0 => input(i) }.toArray)
    val odd = fft1D(input.indices.collect { case i if i % 2 == 1 => input(i) }.toArray)

    Array.tabulate(n) { k =>

      val twiddle = new Complex(Math.cos(-2 * Math.PI * k / n), Math.sin(-2 * Math.PI * k / n))
        .multiply(odd(k % (n / 2)))

      if (k < n / 2) even(k).add(twiddle)
      else even(k - n / 2).subtract(twiddle)
    }
  }

  def FFT2D(input: Array[Array[Complex]]): Array[Array[Complex]] = {
    val rows = input.length
    val cols = input(0).length

    // apply 1D FFT to each row
    val rowTransformed = input.map(fft1D)

    // apply 1D FFT to each column
    val transposed = rowTransformed.transpose
    val colTransformed = transposed.map(fft1D)

    colTransformed.transpose
  }

  def IFFT2D(input: Array[Array[Complex]]): Array[Array[Complex]] = {
    val rows = input.length
    val cols = input(0).length
    val total = rows * cols

    val rowTransformed = input.map(iFFT1D)
    val transposed = rowTransformed.transpose
    val colTransformed = transposed.map(iFFT1D).transpose

    colTransformed.map(_.map(_.divide(total)))
  }

  def iFFT1D(input: Array[Complex]): Array[Complex] = {
    val n = input.length
    if (n == 1) return input

    val even = iFFT1D(input.indices.collect { case i if i % 2 == 0 => input(i) }.toArray)
    val odd = iFFT1D(input.indices.collect { case i if i % 2 == 1 => input(i) }.toArray)

    Array.tabulate(n) { k =>
      val twiddle = new Complex(Math.cos(2 * Math.PI * k / n), Math.sin(2 * Math.PI * k / n))
        .multiply(odd(k % (n / 2)))

      if (k < n / 2) even(k).add(twiddle)
      else even(k - n / 2).subtract(twiddle)
    }.map(_.divide(n)) // normalize by dividing by n
  }

  // convert BufferedImage to complex array
  // reads pixel intensities from grayscale image
  def imageToComplexArray(image: BufferedImage): Array[Array[Complex]] = {
    Array.tabulate(image.getHeight, image.getWidth) { (y, x) =>
      new Complex(new Color(image.getRGB(x, y)).getRed, 0)
    }
  }

  // convert complex array (output from fft) back to BufferedImage
  def complexArrayToImage(array: Array[Array[Complex]], width: Int, height: Int): BufferedImage = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_GRAY)

    // find the minimum and maximum real values in the array
    val minReal = array.flatten.map(_.getReal).min
    val maxReal = array.flatten.map(_.getReal).max

    for (x <- 0 until width; y <- 0 until height) {
      // scale the real value to the range of 0 to 255
      val intensity = ((array(y)(x).getReal - minReal) / (maxReal - minReal)) * 255

      // ensure the intensity is within the valid range
      val validIntensity = math.min(255, math.max(0, intensity))

      val rgb = new Color(validIntensity.toInt, validIntensity.toInt, validIntensity.toInt).getRGB
      image.setRGB(x, y, rgb)
    }

    image
  }

  def createGaussianKernel(width: Int, height: Int, sigma: Double): Array[Array[Complex]] = {
    val kernel = Array.ofDim[Complex](width, height)
    val centerX = width / 2
    val centerY = height / 2
    val sigmaSq = sigma * sigma

    for (x <- 0 until width; y <- 0 until height) {
      val dx = x - centerX
      val dy = y - centerY
      val value = math.exp(-(dx * dx + dy * dy) / (2 * sigmaSq))
      kernel(x)(y) = new Complex(value, 0)
    }

    kernel
  }

  // pads image to the next power of 2 for efficient fft
  def padImage(image: Array[Array[Complex]], newWidth: Int, newHeight: Int): Array[Array[Complex]] = {
    val padded = Array.fill(newWidth, newHeight)(new Complex(0, 0))

    for (x <- image.indices; y <- image(0).indices) {
      padded(x)(y) = image(x)(y)
    }

    padded
  }

  def nextPowerOf2(n: Int): Int = {
    var power = 1
    while (power < n) power *= 2
    power
  }
}
