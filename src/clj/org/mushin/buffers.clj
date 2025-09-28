(ns org.mushin.buffers
  (:import [java.nio ByteBuffer ByteOrder]))

(defn wrap-bytes
  ^ByteBuffer
  ([^bytes byte-buffer ^long offset ^long length]
   (ByteBuffer/wrap byte-buffer offset length))
  ([^bytes byte-buffer]
   (ByteBuffer/wrap byte-buffer)))

(defn set-byte-order
  ^ByteBuffer
  [^ByteBuffer bb ^ByteOrder byte-order]
  (.order bb byte-order)
  bb)

(defn reset-byte-buffer
  ^ByteBuffer
  [^ByteBuffer bb]
  (.reset bb)
  bb)

(defn clear-byte-buffer!
  ^ByteBuffer
  [^ByteBuffer bb]
  (.clear bb)
  bb)

(defn copy-ints-to-byte-buffer!
  ^ByteBuffer
  [^ints int-buffer ^ByteBuffer bb]
  (dotimes [n (alength int-buffer)]
    (.putInt bb (aget int-buffer n)))
  bb)

(defn copy-ints-to-bytes!
  ^bytes
  [^ints int-buffer ^bytes byte-buffer]
  (let [^ByteBuffer bb (set-byte-order (wrap-bytes byte-buffer 0 (* (alength int-buffer) 4)) (ByteOrder/nativeOrder))]
    (copy-ints-to-byte-buffer! int-buffer bb))
  byte-buffer)
