package hrf

/** Запуск симуляции Vast ботами: `HRF_VAST_SIM=1 sbt run` / `fastOptJS` (нужен Node.js в PATH). См. [[hrf.HRF]], build.sbt. */
object VastSimEntry {
    def main(args : Array[String]) : Unit = vast.Host.main(args)
}
