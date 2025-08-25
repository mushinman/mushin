(ns org.mushin.multimedia.colorspace
  (:require [clojure.math :as math]))

(defonce epsilon 0.008856)
(defonce kappa 903.3)

(defonce d65-xn 0.95047)
(defonce d65-yn 1.0)
(defonce d65-zn 1.08883)

(defn- inverse-srgb-companding
  [^Double v]
  (if (< v 0.04045)
    (/ v 12.92)
    (math/pow (/ (+ v 0.055) 1.055) 2.4)))

(defn- f-of-v
  [v]
  (if (> v epsilon)
    (math/cbrt v)
    (/ (+ (* kappa v) 16.0) 116.0)))

(defn- srgb-companding
  [^Double v]
  (if (<= v 0.0031308)
    (* 12.92 v)
    (- (* (math/pow v (/ 1.0 2.4)) 1.055) 0.055)))


(defn- lab->srgb
  [^Double v]
  (let [cubed (math/pow v 3.0)]
    (if (> cubed epsilon)
      cubed
      (/ (- (* 116.0 v) 16.0) kappa))))


(defn srgb->ciexyz
  [^Double r ^Double g  ^Double b]
  (let [r (inverse-srgb-companding r)
        g (inverse-srgb-companding g)
        b (inverse-srgb-companding b)]
    ;; Apply the matrix transformations for luminant D65.
    [(+ (* r 0.4124564) (* g 0.3575761) (* b 0.1804375))
     (+ (* r 0.2126729) (* g 0.7151522) (* b 0.0721750))
     (+ (* r 0.0193339) (* g 0.1191920) (* b 0.9503041))]))

(defn ciexyz->cielab
  [^Double x ^Double y ^Double z]
  (let [fx (f-of-v (/ x d65-xn))
        fy (f-of-v (/ y d65-yn))
        fz (f-of-v (/ z d65-zn))]
    [(- (* 116.0 fy) 16.0)
     (* 500.0 (- fx fy))
     (* (- fy fz) 200.0)]))

(defn srgb->cielab
  [^Double r ^Double g ^Double b]
  (let [[x y z] (srgb->ciexyz r g b)]
    (ciexyz->cielab x y z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn cielab->ciexyz
  [^Double l ^Double a ^Double b]
  (let [fy (/ (+ l 16.0) 116.0)
        fx (+ (/ a 500.0) fy)
        fz (- fy (/ b 200.0))
        xr (lab->srgb fx)
        yr (if (> l (* kappa epsilon))
             (math/pow (/ (+ l 16.0) 116.0) 3.0)
             (/ l kappa))
        zr (lab->srgb fz)]
    [(* d65-xn xr)
     (* d65-yn yr)
     (* d65-zn zr)]))

(defn ciexyz->srgb
  [^Double x ^Double y ^Double z]
  ;; Apply the XYZ->SRGB transformation matrix.
  (let [r (+ (* x 3.2404542) (* y -1.5371385) (* z -0.4985314))
        g (+ (* x -0.9692660) (* y 1.8760108) (* z 0.0415560))
        b (+ (* x 0.0556434) (* y -0.2040259) (* z 1.0572252))]
    ;; Compand everything to get result.
    [(srgb-companding r) (srgb-companding g) (srgb-companding b)]))

(defn cielab->srgb
  [^Double l ^Double a ^Double b]
  (let [[x y z] (cielab->ciexyz l a b)]
    (ciexyz->srgb x y z)))

(defn normalized->byte
  [^Double v]
  (unchecked-byte (math/round (* v 255.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
