package p1bar

import java.io.PrintStream
import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.concurrent.TimeUnit

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
  override def empty: String = "\u2591"
  override def rightBoundary: String = "|"
}

class BarFormatter(barFormat: BarFormat = AsciiBarFormat, unit: String = "it", unitScale: Boolean = false,
                   ncols: Int = 10) {
  private val longFmt = DateTimeFormatter.ofPattern("HH:mm:ss")
  private val shortFmt = DateTimeFormatter.ofPattern("mm:ss")

  def format(n: Int, total: Int, elapsed: Duration): String = {
    require(n <= total)
    require(n >= 0 && total >= 0)

    val leftBar = percentage(n, total)
    val nBars = Math.max(1, ncols - leftBar.length - 1)

    val elapsedFmt = formatInterval(elapsed)

    val rate = 1.0 * n / elapsed.toSeconds
    val rateFmt = f"$rate%5.2f$unit%s/s"

    val remainingFmt = formatInterval(FiniteDuration(((total - n) / rate).toLong, TimeUnit.SECONDS))

    val rightBar = s"$n/$total [$elapsedFmt<$remainingFmt, $rateFmt]"
    s"$leftBar ${progressBar(n, total, nBars)} $rightBar"
  }

  private def formatInterval(int: Duration): String = {
    val inst = Instant.ofEpochMilli(int.toMillis).atZone(ZoneId.systemDefault()).toLocalDateTime
    if (int.toHours >= 1) longFmt.format(inst) else shortFmt.format(inst)
  }

  private def percentage(n: Int, total: Int): String = {
    val v: Double = 100.0 * n / total
    f"$v%.1f%%"
  }

  private def progressBar(n: Int, total: Int, nBars: Int): String = {
    val totalLength = nBars - barFormat.leftBoundary.length - barFormat.rightBoundary.length
    val frac = 1.0 * n / total
    val done = (frac * totalLength).toInt
    val remaining = totalLength - done

    s"${barFormat.leftBoundary}${barFormat.bar * done}${barFormat.empty * remaining}${barFormat.rightBoundary}"
  }
}

trait Updater {
  def update(incr: Int): Unit
}

class ProgressBar(total: Int, barFormatter: BarFormatter) {
  def this() = this(ProgressBar.UnknownTotal, new BarFormatter())

  def this(total: Int) = this(total, new BarFormatter())

  private lazy val console = new PrintStream(System.err, true, "UTF-8")
  private var n = 0
  private var lastLen = 0
  private var startTime: Long = _

  private def now(): Long = System.nanoTime

  private def update(incr: Int): Unit = {
    val curTime  = now()

    require(incr >= 0)
    n += incr

    val elapsed = FiniteDuration(curTime - startTime, TimeUnit.NANOSECONDS)

    val barLine: String = barFormatter.format(n, total, elapsed)
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
}

object Main extends App {
  val its = 60

  val progress = new ProgressBar(its, new BarFormatter(ncols = 30))
  progress meter { updater =>
    (1 to its).foreach { i =>
      Thread.sleep(500)
      updater.update(1)
    }
  }
}
