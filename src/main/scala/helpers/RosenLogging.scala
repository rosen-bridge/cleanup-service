package helpers
import org.slf4j.Logger
import org.slf4j.LoggerFactory

trait RosenLogging {
  protected def log: Logger = LoggerFactory.getLogger(this.getClass)
}
