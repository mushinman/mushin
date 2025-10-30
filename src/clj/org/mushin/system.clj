(ns org.mushin.system
  (:require [java-time.api :as time])
  (:import [java.lang.management ManagementFactory]))

(defn memory-usage
  "Get process memory usage in bytes."
  []
  (let [memory-bean (ManagementFactory/getMemoryMXBean)]
    (+ (.getUsed (.getHeapMemoryUsage memory-bean))
       (.getUsed (.getNonHeapMemoryUsage memory-bean)))))

(defn thread-count
  "Get process thread count."
  []
  (.getThreadCount (ManagementFactory/getThreadMXBean)))

(defn process-start-time
  "Get process start time as a ZonedDateTime."
  []
  (time/zoned-date-time
   (time/instant (.getStartTime (ManagementFactory/getRuntimeMXBean))) "UTC"))
