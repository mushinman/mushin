(ns org.mushin.geom
  (:import [java.awt.geom AffineTransform]))

(defn affine-transform
  ^AffineTransform
  ([]
   (AffineTransform.))
  ([tx]
   (AffineTransform. tx))
  ([m00 m10 m01 m11 m02 m12]
   (AffineTransform. m00 m10 m01 m11 m02 m12)))

(defn get-translate-transform
  ^AffineTransform
  [^double tx ^double ty]
  (AffineTransform/getTranslateInstance tx ty))

(defn concatenate-transforms
  ^AffineTransform
  [^AffineTransform tx1 ^AffineTransform tx2]
  (doto (affine-transform tx1)
    (.concatenate tx2)))

(defn translate-transform
  ^AffineTransform
  [^AffineTransform tx ^double dx ^double dy]
  (concatenate-transforms tx (get-translate-transform dx dy)))

(defn get-translate-x
  ^double
  [^AffineTransform tx]
  (.getTranslateX tx))

(defn get-translate-y
  ^double
  [^AffineTransform tx]
  (.getTranslateY tx))


(defn get-scale-x
  ^double
  [^AffineTransform tx]
  (.getScaleX tx))

(defn get-scale-y
  ^double
  [^AffineTransform tx]
  (.getScaleY tx))
