package hrf

/** Запуск симуляции Vast ботами: `HRF_MAIN=hrf.VastSimEntry sbt run` (нужен Node.js в PATH). См. безопасные hash/search в [[hrf.HRF]]. */
object VastSimEntry {
    def main(args : Array[String]) : Unit = vast.Host.main(args)
}
