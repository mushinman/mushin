(ns org.mushin.multimedia.buffered-image-transcoder
  (:require [org.mushin.files :as files])
  (:import [io.sf.carte.echosvg.transcoder TranscoderOutput]
           [java.awt.image BufferedImage]
           [io.sf.carte.echosvg.util ParsedURL]
           [io.sf.carte.echosvg.bridge UserAgentAdapter ExternalResourceSecurity ScriptSecurity]
           [io.sf.carte.echosvg.transcoder.image ImageTranscoder]))

;; TODO proxy createUserAgent
(defn file-root-ua [^String root-path]
  (let [root (-> (files/path root-path)
                 (files/to-absolute-path)
                 (files/normalize))]
    (proxy [UserAgentAdapter] []
      (getExternalResourceSecurity
        [^ParsedURL res ^ParsedURL doc]
        (reify ExternalResourceSecurity
          (checkLoadExternalResource [_]
            (println "file-root-ua")
            (when (not= "file" (.getProtocol res))
              (throw (SecurityException. "Only file protocol is allowed")))
            (let [p (-> (files/path (.getPath res))
                        (files/to-absolute-path)
                        (files/normalize))]
              (when-not (.startsWith p root)
                (throw (SecurityException. (str "Blocked outside the root path " root))))))))
      (getScriptSecurity
        [^String t ^ParsedURL s ^ParsedURL d]
        (reify ScriptSecurity
          (checkLoadScript [_]
            (throw (SecurityException. "Scripts are disabled"))))))))

(defmacro transcoder-with-file-root
  [root transcoder-type]
  `(proxy [~transcoder-type] []
      (createUserAgent [] (file-root-ua ~root))))

(defn buffered-image-transcoder
  [root]
  (let [!img (atom nil)]
    (proxy [ImageTranscoder clojure.lang.IDeref] []
        (createImage
          (^BufferedImage [^Integer w ^Integer h]
           (let [img (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)]
             (reset! !img img)
             img)))
        (writeImage
          ([^BufferedImage img ^TranscoderOutput _]
           (reset! !img img)
           nil))
        (createUserAgent
          [] (file-root-ua root))
        (deref
          (^BufferedImage [] @!img)))))
