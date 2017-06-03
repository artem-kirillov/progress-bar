package p1bar

import java.io.PrintStream
import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.concurrent.TimeUnit

import scala.collection.immutable.Stream.cons
import scala.concurrent.duration.{Duration, FiniteDuration}

trait BarFormat {
  def leftBoundary: String
  def bar: String
  def empty: String
  def rightBoundary: String
}

object AsciiBarFormat extends BarFormat {
  override def leftBoundary: String = "|"
  override def bar: String = "#"
  override def empty: String = "-"
  override def rightBoundary: String = "|"
}

object UnicodeBarFormat extends BarFormat {
  override def leftBoundary: String = "|"
  override def bar: String = "\u2588"
  override def empty: String = " "
  override def rightBoundary: String = "|"
}

object Scale extends ((Double, String, Double) => String) {
  private val units = Seq("", "K", "M", "G", "T", "P", "E", "Z", "Y")

  override def apply(num: Double, suffix: String = "", divisor: Double = 1000): String = {
    require(num >= 0 && divisor > 0)
    val (unit: String, value: Double) = units.zip(scale(num, divisor)).reverse.find(_._2 > 1d).getOrElse(("", num))
    s"${formatValue(value)}$unit$suffix"
  }

  private def formatValue(value: Double): String =
    if (value > 100) {
      f"${value.toLong}%d"
    } else if (value > 10) {
      f"$value%.1f"
    } else {
      f"$value%.2f"
    }

  private def scale(num: Double, divisor: Double): Stream[Double] =
    cons(num, scale(num / divisor, divisor))
}

class BarFormatter(barFormat: BarFormat = AsciiBarFormat, unit: String = "it",
                   unitScale: Boolean = false, unitDivisor: Int = 1000, ncols: Int = 10) {
  private val longFmt = DateTimeFormatter.ofPattern("HH:mm:ss")
  private val shortFmt = DateTimeFormatter.ofPattern("mm:ss")

  def format(n: Int, total: Int, elapsed: Duration): String = {
    require(n <= total && total > 0, s"Current n is $n, total is $total")
    require(n >= 0)

    val leftBarStr = leftBar(n, total)
    val rightBarStr = rightBar(n, total, elapsed)

    val nBars = Math.max(1, ncols - leftBarStr.length - rightBarStr.length - 2)
    val bar = if (nBars > 6) " " + progressBar(n, total, nBars) + " " else "|"

    s"$leftBarStr$bar$rightBarStr"
  }

  def format(n: Int, elapsed: Duration): String = rightBar(n, elapsed)

  private def formatInterval(int: Duration): String = {
    val inst = Instant.ofEpochMilli(int.toMillis).atZone(ZoneId.systemDefault()).toLocalDateTime
    if (int.toHours >= 1) longFmt.format(inst) else shortFmt.format(inst)
  }

  private def leftBar(n: Int, total: Int): String = {
    val v: Double = 100d * n / total
    f"$v%.1f%%"
  }

  private def progressBar(n: Int, total: Int, nBars: Int): String = {
    val bodyLength = nBars - barFormat.leftBoundary.length - barFormat.rightBoundary.length
    val frac = n.toDouble / total
    val done = (frac * bodyLength).toInt
    val remaining = bodyLength - done

    s"${barFormat.leftBoundary}${barFormat.bar * done}${barFormat.empty * remaining}${barFormat.rightBoundary}"
  }

  private def rightBar(n: Int, total: Int, elapsed: Duration): String = {
    val elapsedFmt = formatInterval(elapsed)

    val rate = n.toDouble / elapsed.toSeconds
    val rateFmt = formatRate(rate)

    val remainingFmt = formatInterval(FiniteDuration(((total - n) / rate).toLong, TimeUnit.SECONDS))
    val nFmt = if (unitScale) Scale(n, divisor = unitDivisor) else n.toString
    val totalFmt = if (unitScale) Scale(total, divisor = unitDivisor) else total.toString

    s"$nFmt/$totalFmt [$elapsedFmt<$remainingFmt, $rateFmt]"
  }

  private def rightBar(n: Int, elapsed: Duration): String = {
    val elapsedFmt = formatInterval(elapsed)

    val rate = n.toDouble / elapsed.toSeconds
    val rateFmt = formatRate(rate)
    val nFmt = if (unitScale) Scale(n, divisor = unitDivisor) else n.toString

    s"$nFmt [$elapsedFmt, $rateFmt]"
  }

  private def formatRate(rate: Double): String = {
    val rateFmt = if (unitScale) {
      Scale(rate, divisor = unitDivisor)
    } else {
      f"$rate%.2f"
    }
    s"$rateFmt$unit/s"
  }
}

trait Updater {
  def update(incr: Int): Unit
}

class ProgressBar private(total: Int, barFormatter: BarFormatter) {
  private lazy val console = new PrintStream(System.err, true, "UTF-8")
  private val renderInterval: Long = TimeUnit.MILLISECONDS.toNanos(100)

  private var startTime: Long = _
  private var n = 0
  private var lastLen = 0
  private var lastRenderTime: Long = 0

  private def now(): Long = System.nanoTime

  private def update(incr: Int): Unit = {
    require(incr >= 0)
    n += incr

    if (now() - lastRenderTime > renderInterval || n == total) {
      render()
      lastRenderTime = now()
    }
  }

  private def render(): Unit = {
    val elapsed = FiniteDuration(now() - startTime, TimeUnit.NANOSECONDS)

    val barLine: String = if (total == ProgressBar.UnknownTotal) {
      barFormatter.format(n, elapsed)
    } else {
      barFormatter.format(n, total, elapsed)
    }
    val padding: String = " " * Math.max(lastLen - barLine.length, 0)

    console.print(s"\r$barLine$padding")

    lastLen = barLine.length
  }

  def meter[A](block: Updater => A): Unit = {
    start()
    block(update)
    stop()
  }

  private def start(): Unit = {
    startTime = now()
    n = 0
    lastLen = 0
  }

  private def stop(): Unit = {
    console.println(" Done.")
  }
}

object ProgressBar {
  private val UnknownTotal: Int = -1

  def apply(total: Int, barFormatter: BarFormatter): ProgressBar = new ProgressBar(total, barFormatter)
  def apply(total: Int): ProgressBar = new ProgressBar(total, new BarFormatter())
  def apply(barFormatter: BarFormatter): ProgressBar = new ProgressBar(UnknownTotal, barFormatter)
  def apply(): ProgressBar = new ProgressBar(UnknownTotal, new BarFormatter())
}

object Main extends App {
  val its = 600000

  val progress = ProgressBar(its, new BarFormatter(ncols = 60, barFormat = UnicodeBarFormat, unit = "bit", unitScale = true))
  progress meter { updater =>
    (1 to its).foreach { i =>
      Thread.sleep(1)
      updater.update(100)
    }
  }
}
